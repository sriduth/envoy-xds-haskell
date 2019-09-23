{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module GrpcServices.ClusterDiscovery where

import Prelude
import Control.Lens

import Network.GRPC.Server
import Network.HTTP.Simple

-- Import EDS to use load assignment
import Proto.Api.V2.Cds
import Proto.Api.V2.Cds_Fields as CF
import Proto.Api.V2.Eds
import Proto.Api.V2.Eds_Fields as EF

import Proto.Api.V2.Discovery
import Proto.Api.V2.Discovery_Fields as DF

import Proto.Api.V2.Endpoint.Endpoint
import Proto.Api.V2.Endpoint.Endpoint_Fields as EDF

import Proto.Api.V2.Core.Address
import Proto.Api.V2.Core.Address_Fields as AdrsF

import Proto.Google.Protobuf.Any
import Proto.Google.Protobuf.Any_Fields as AF

import Proto.Google.Protobuf.Duration_Fields as PDF

import Data.ProtoLens.Message (defMessage)
import Data.ProtoLens.Encoding

import Data.Int
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Common
import Utils (getEnvConf)

import Data.ProtoLens
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import System.Log.Logger

import qualified Data.Map.Strict as Map
import Data.IORef


mkSocketAddress :: T.Text -> Word32 -> SocketAddress
mkSocketAddress ip port=
  defMessage
  & AdrsF.address .~ ip
  & AdrsF.maybe'portSpecifier .~ (Just (SocketAddress'PortValue port))

mkCluster :: T.Text -> [(T.Text, Word32)] -> Int32 -> Cluster
mkCluster name upstreamHosts connectTimeoutNanos =
  defMessage & CF.name .~ name
             & CF.connectTimeout .~ (defMessage & PDF.nanos .~ connectTimeoutNanos)
             & CF.lbPolicy .~ Cluster'ROUND_ROBIN
             & CF.loadAssignment .~ loadAssignment
  where
    loadAssignment :: ClusterLoadAssignment
    loadAssignment =
      defMessage
        & EF.clusterName .~ name
        -- locality lb endpoints
        & EF.endpoints .~ [
          defMessage & EDF.lbEndpoints .~ endpoints]

    endpoints =
      (\(host, port) ->
         defMessage
         & EDF.maybe'hostIdentifier .~ (Just (LbEndpoint'Endpoint( 
            defMessage
            & EDF.address .~
               (defMessage
                 & AdrsF.socketAddress .~ (mkSocketAddress host port)))))
         ) <$> upstreamHosts
      
sampleCdsResponse :: DiscoveryResponse
sampleCdsResponse = makeDiscoveryResponse "type.googleapis.com/envoy.api.v2.Cluster " "sriduth" [sampleCluster]
  where
    sampleCluster :: Cluster
    sampleCluster = mkCluster "myCluster" [("127.0.0.1", 8888)] 250000000
    
                                    
clusterDiscoveryHandler :: BiDiStreamHandler ClusterDiscoveryService "streamClusters" EnvoyNodeConfigState
clusterDiscoveryHandler conn = do
    debugM "ClusterDiscovery" ("Got CDS stream :: " <> show conn)
    reqRef <- newEmptyMVar
    
    return (Init, BiDiStream (handler reqRef))
  where
    handler = genericDiscoveryHandler (protoToDR) (onUpdateHandler  CLUSTER) CLUSTER


    typeUrl :: T.Text
    typeUrl = "type.googleapis.com/envoy.api.v2.Cluster"
    
    protoToDR :: T.Text -> [Cluster] -> DiscoveryResponse
    protoToDR = makeDiscoveryResponse typeUrl

