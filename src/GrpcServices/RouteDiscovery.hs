{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module GrpcServices.RouteDiscovery where

import Prelude
import Control.Lens

import Network.GRPC.Server

import Proto.Google.Protobuf.Any
import Proto.Google.Protobuf.Any_Fields as AF

import Proto.Api.V2.Rds
import Proto.Api.V2.Rds_Fields

-- Import EDS to use load assignment
import Proto.Api.V2.Discovery
import Proto.Api.V2.Discovery_Fields as DF

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Common

import Data.ProtoLens
import Data.IORef

import qualified Data.Map.Strict as Map

import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import System.Log.Logger
import Proto.Admin.V2alpha.ConfigDump
import Proto.Google.Protobuf.Timestamp
import Proto.Api.V2.Route.Route
import Proto.Api.V2.Core.Base
import Proto.Google.Protobuf.Duration
import Data.Char (toLower)
import Network.Wai
  

routeDiscoveryHandler :: BiDiStreamHandler RouteDiscoveryService "streamRoutes" EnvoyNodeConfigState
routeDiscoveryHandler initReq = do
    debugM "XDS" ("Got CDS stream :: " <> show initReq)
    reqRef <- newEmptyMVar
    
    return (Init, BiDiStream (handler reqRef))
  where
    handler = (genericDiscoveryHandler (protoToDR) (onUpdateHandler ROUTE) ROUTE )

    typeUrl :: T.Text
    typeUrl = "type.googleapis.com/envoy.api.v2.RouteConfiguration"

    protoToDR :: T.Text -> [RouteConfiguration] -> DiscoveryResponse
    protoToDR = makeDiscoveryResponse typeUrl

   
  
