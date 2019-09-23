{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Network.GRPC.Server

import Prelude

import Data.IORef
import Data.List.Split
import Data.Binary.Builder
import Data.ByteString.Lazy.Char8 hiding (foldl)
import qualified Data.Text.Lazy as T

import qualified Data.Map.Strict as Map

import System.Log.Logger

import Network.Wai.Handler.WarpTLS (tlsSettings, TLSSettings)
import Network.Wai.Handler.Warp (defaultSettings, setLogger, setOnException)
import Network.Wai.Handler.Warp.Internal (Settings)
import Network.GRPC.HTTP2.Types (RPC(..))
import Network.GRPC.HTTP2.Encoding (gzip)

import Proto.Service.Accesslog.V2.Als
import Proto.Api.V2.Cds
import Proto.Api.V2.Rds

import GrpcServices.ClusterDiscovery
import GrpcServices.RouteDiscovery
import GrpcServices.AccessLogStream (accessLogStreamHandler, LoggersRef)
import Utils (getEnvConf)


appSettings :: Settings
appSettings = (setLogger requestLogger)
  . (setOnException exceptionLogger) $ defaultSettings
  
  where
    requestLogger req status int = do
      debugM "XDS" ((show req) <> (show status) <> (show int))

    exceptionLogger req ex = do
      logM "XDS" ERROR ((show req) <> (show ex))
    
makeTlsSettings :: IO (TLSSettings)
makeTlsSettings = do
  tlsKeyPath <- getEnvConf "TLS_KEY_PATH" "./certs/xds-server.key"
  tlsCertPath <- getEnvConf "TLS_CERT_PATH" "./certs/xds-server.crt"

  return $ tlsSettings tlsCertPath tlsKeyPath

startGrpcLogger :: IO ()
startGrpcLogger = do
  loggers <- newIORef $ Map.empty
  xdsTlsSettings <- makeTlsSettings
  runGrpc xdsTlsSettings appSettings (handlers loggers) [gzip]
  
  where
    handlers loggers = [clientStream (RPC :: RPC AccessLogService "streamAccessLogs") (accessLogStreamHandler loggers)]
  
startFullXDS :: IO ()
startFullXDS = do
  -- Map a node to a logger
  -- this will be used to write the access logs
  loggers <- newIORef $ Map.empty
  xdsTlsSettings <- makeTlsSettings 
  
  runGrpc xdsTlsSettings appSettings (handlers loggers) [gzip]
  
  
-- register handler for access logs here
handlers :: LoggersRef -> [ServiceHandler]
handlers loggers =
  [ clientStream (RPC :: RPC AccessLogService "streamAccessLogs") (accessLogStreamHandler loggers)
  , bidiStream (RPC :: RPC ClusterDiscoveryService "streamClusters") clusterDiscoveryHandler
  , bidiStream (RPC :: RPC RouteDiscoveryService "streamRoutes") routeDiscoveryHandler]



