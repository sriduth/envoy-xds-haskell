{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module GrpcServices.AccessLogStream where

import Prelude
import Network.GRPC.Server

import Control.Lens
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan(..))

import Data.Maybe
import Data.ProtoLens
import Data.IORef
import Data.Tuple (fst)
import qualified Data.Map.Strict as Map

import qualified Data.Text as T

import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter

import Proto.Service.Accesslog.V2.Als as ALS
import Proto.Service.Accesslog.V2.Als_Fields as ALSF
import Proto.Data.Accesslog.V2.Accesslog_Fields as ALF

import Proto.Api.V2.Core.Base as B
import Proto.Api.V2.Core.Base_Fields as BF

import Proto.Api.V2.Core.Address
import Proto.Api.V2.Core.Address_Fields as AdrsF

import qualified Proto.Google.Protobuf.Wrappers_Fields as W

import Utils (getEnvConf)
import GHC.Conc (ThreadId)


type NodeIdentifier = StreamAccessLogsMessage'Identifier
type LoggersRef = IORef (Map.Map String Int)

accessLogStreamHandler loggers initReq = do
  debugM "XDS" ("Client has opened a log stream :: " <> show initReq)

  chan <- newChan
  
  threadIdRef <- newIORef Nothing
  
  return $ (Nothing, ClientStream
             (\n logLine -> do                   
                 let nodeInformation = logLine ^. ALSF.maybe'identifier
                 case nodeInformation of
                   Just identifier -> do
                     debugM "XDS" ("Node connected" <> show identifier)
                     prepareLogger identifier loggers
                     prepareLogWriter chan identifier threadIdRef
                     
                   Nothing ->
                     return ()

                 writeChan chan logLine
                 return n)
             (\n -> do
                 threadId <- readIORef threadIdRef
                 case threadId of
                   Just (tid, nodeId) -> do
                     _ <- killThread tid
                     stopLogWriter nodeId loggers
                   _ -> return ()
                 return defMessage))
  where
    stopLogWriter :: String -> LoggersRef -> IO ()
    stopLogWriter nodeId loggers = do
      loggers' <- readIORef loggers
      updatedLoggers <- return $ case ((+ (-1)) <$> (Map.lookup nodeId loggers')) of
                       Nothing ->
                         Map.delete nodeId loggers'
                       Just 0 ->
                         Map.delete nodeId loggers'
                       Just val ->
                         Map.insert nodeId val loggers'
      
      atomicWriteIORef loggers updatedLoggers

    prepareLogger :: NodeIdentifier -> LoggersRef -> IO ()
    prepareLogger identifier loggers = do      
      let nodeId = nodeIdFromIdentifier identifier
      loggers' <- readIORef loggers
      
      updatedLoggers <- case Map.lookup nodeId loggers' of
        Just count ->
          return $ Map.insert nodeId (1 + count) loggers'
        Nothing -> do
          debugM "XDS" "Configuring logger for instance"
          configureLoggerForNode (nodeIdFromIdentifier identifier)
          return $ Map.insert nodeId 1 loggers'

      atomicWriteIORef loggers updatedLoggers

    prepareLogWriter :: Chan StreamAccessLogsMessage
                        -> NodeIdentifier
                        -> IORef (Maybe (ThreadId, String))
                        -> IO ()
    prepareLogWriter chan identifier threadIdRef = do
      debugM "XDS" ("Starting log writer for" <> (show identifier))
      threadId <- forkIO $ accessLogStreamWriter chan identifier
      writeIORef threadIdRef (Just (threadId, nodeIdFromIdentifier identifier))
      return ()      

nodeIdFromIdentifier :: NodeIdentifier -> String
nodeIdFromIdentifier identifier = T.unpack $ identifier ^. ALSF.node ^. BF.id

-- Given a node ID configure hslogger to create a new file with
-- the node id as the name at a path specified by the option
configureLoggerForNode :: String -> IO ()
configureLoggerForNode nodeId = do
  basePath <- getEnvConf "ACCESS_LOGS_PATH" "/var/log/envoy/"
  logger <- (fileHandler (basePath <> loggerName) DEBUG) >>=
    (\handler ->
       return $ setFormatter handler (simpleLogFormatter "[$time] $msg"))
  updateGlobalLogger nodeId (addHandler logger)
  updateGlobalLogger nodeId (setLevel DEBUG)
  where
    loggerName :: String
    loggerName = nodeId <> "_access.log"
  

accessLogStreamWriter :: (Chan StreamAccessLogsMessage)
                      -> NodeIdentifier
                      -> IO ()
accessLogStreamWriter logStream identifier = forever $ do
  val <- readChan logStream
  debugM loggerName (makeLogLine val identifier)
  return ()

  where
    loggerName :: String
    loggerName = nodeIdFromIdentifier identifier
  
makeLogLine :: StreamAccessLogsMessage -> NodeIdentifier -> String
makeLogLine logLine identifier =
  nodeId <>  " " <> nodeCluster <> " " <> (show logLines)

  where    
    nodeInformation = identifier ^. ALSF.node
    
    nodeId :: String
    nodeId = T.unpack $ nodeInformation ^. BF.id
  
    nodeCluster :: String
    nodeCluster = T.unpack $ nodeInformation ^. BF.cluster
  
    logLines = (strLogLine
      <$> (logLine ^. ALSF.httpLogs ^. ALSF.logEntry))

    strLogLine line =
      let commonProperties = line ^. ALF.commonProperties
          httpVersion = line ^. protocolVersion
          req = line ^. request
          res = line ^. response
          reqHeaders = req ^. requestHeaders
      in
        (formatCommonProperties commonProperties)
        <> " " <> (showEnum $ httpVersion)
        <> " " <> (formatRequest req)
--        <> " " <> (formatRequestHeaders reqHeaders)
        <> " " <> (formatResponse res)

    formatResponse response =
      (show $ response ^. responseCode ^. W.value)
      
    formatRequest request =
      (showEnum $ request ^. requestMethod)
      <> " " <> (T.unpack $ request ^. ALF.path)
      <> " " <> (T.unpack $ request ^. requestId)
      <> " " <> (T.unpack $ request ^. originalPath)
      <> " " <> (T.unpack $ request ^. userAgent)

    formatRequestHeaders headers =
      let xMid = mconcat $ [ Map.lookup "x-jp-merchant-id" headers
                           , Map.lookup "x-merchantid" headers]
      in
        show $ T.pack $ fromMaybe "" xMid
      
    formatCommonProperties commonProps = T.unpack $
      (formatAddress $ commonProps ^. upstreamRemoteAddress)
      <> " " <> (commonProps ^. ALF.upstreamCluster)
      <> " " <> (T.pack $ show $ commonProps ^. ALF.responseFlags)
      
    formatAddress address =
      let socketAddress = address ^. AdrsF.socketAddress
          remoteIp = socketAddress ^. AdrsF.address
          port = T.pack $ show $ socketAddress ^. AdrsF.portValue
      in
        remoteIp <> ":" <> port
    
