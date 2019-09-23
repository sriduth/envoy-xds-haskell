{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Common where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import System.IO
import System.Log.Logger
import Control.Concurrent (threadDelay)

import Control.Concurrent.MVar

import Proto.Google.Protobuf.Any
import qualified Proto.Google.Protobuf.Any_Fields as AF

-- Import EDS to use load assignment
import Proto.Api.V2.Discovery
import qualified Proto.Api.V2.Discovery_Fields as DF
import qualified Proto.Api.V2.Core.Base_Fields as BF

import Network.GRPC.Server

import Data.ProtoLens
import Control.Lens

import Data.ProtoLens.Service.Types


data CfgType = ROUTE | CLUSTER


data EnvoyNodeConfigState
  = Init
  | NotConfigured
  | WithVersion T.Text T.Text
  deriving (Show)

  
makeDiscoveryResponse :: (Message a) => T.Text -> T.Text -> [a] -> DiscoveryResponse
makeDiscoveryResponse typeUrl versionInfo protoRes =
  defMessage & DF.versionInfo .~ versionInfo
             & DF.typeUrl .~ typeUrl
             & DF.resources
                  .~ (toResource <$> protoRes)
  where
    toResource :: (Message a) => a -> Any
    toResource proto =
      defMessage & AF.typeUrl .~ typeUrl
                 & AF.value .~ encodeMessage proto

-- | TODO: The update handler should take the
-- current node configuration state and return the next one
onUpdateHandler :: CfgType
                -> EnvoyNodeConfigState
                -> DiscoveryRequest
                -> IO (T.Text)
onUpdateHandler configType state req = do
  let (nodeId, zone) = getNodeLocation req

  version <- return $ case state of
                        WithVersion v _ -> v
                        _ -> T.pack $ show state

  return zone


-- | Get the nodeId, zone of the given node
getNodeLocation :: DiscoveryRequest -> (T.Text, T.Text)
getNodeLocation req =
  let node = req ^. DF.node
  in (node ^. BF.id, node ^. BF.cluster)
      
    
-- | Enable the TypeFamilies extenstion else `s` and `m` will not be unified
-- TODO: the implementation of the WaitInput handler need not be placed outside -
--       currently we _have_ to keep it outside as the s and m variables are not
--       getting instantiated in time such that the (MethodInput s m) type family
--       can be instantiated as DiscoveryRequest
genericDiscoveryHandler :: (Message r) =>
                           (T.Text -> [r] -> (MethodOutput s m)) ->
                           (EnvoyNodeConfigState -> (MethodInput s m) -> (IO (T.Text))) ->
                           CfgType ->
                           MVar (MethodInput s m) ->
                           (EnvoyNodeConfigState -> IO (BiDiStep s m EnvoyNodeConfigState))
genericDiscoveryHandler makeResponse onUpdate configType reqRef = handler
  where
    -- handle the initial discovery request
    -- save the discovery request for later use
    initialReqHandler ref _ message = do
      putMVar ref message
      onUpdate Init message
      return $ NotConfigured
      
    handler st = do
      case st of
        Init -> return $  WaitInput (initialReqHandler reqRef) (\s -> return s)
        NotConfigured -> makeVersionedResponse st reqRef
        WithVersion _ _-> do
          threadDelay 5000000
          makeVersionedResponse st reqRef

    makeVersionedResponse st drRef = do
      req <- readMVar drRef
      zone <- onUpdate st req

      let cfgVersion = "latest"
      return $ WriteOutput (WithVersion cfgVersion zone) (makeResponse cfgVersion [defMessage])
        
