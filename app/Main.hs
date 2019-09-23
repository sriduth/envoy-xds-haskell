module Main where

import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter

import Lib (startFullXDS, startGrpcLogger)
import Utils

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger "LogProcessor.Source" (setLevel DEBUG)
  updateGlobalLogger "LogProcessor.Sink" (setLevel DEBUG)

  xdsAppLogsPath <- getEnvConf "XDS_LOGS_PATH" "/var/log/envoy/xds_app.log"
  xdsFileLogger <- (fileHandler xdsAppLogsPath DEBUG) >>=
    (\handler ->
        return $ setFormatter handler (simpleLogFormatter "[$time] $msg"))

  updateGlobalLogger "XDS" (addHandler xdsFileLogger)
  updateGlobalLogger "XDS" (setLevel DEBUG)

  -- configure file logger sink
  grpcLogStreamPath <- getEnvConf "GRPC_LOGS_PATH" "/var/log/envoy/xds_access.log"
  fileLogger <- (fileHandler grpcLogStreamPath DEBUG) >>=
    (\handler ->
        return $ setFormatter handler (simpleLogFormatter "[$time] $msg"))
  
  updateGlobalLogger "LogProcessor.FileSink" (addHandler fileLogger)
  updateGlobalLogger "LogProcessor.FileSink" (setLevel DEBUG)


  appMode <- getEnvConf "APP_MODE" "FULL_XDS"

  case appMode of
    "ACCESS_LOG" -> startGrpcLogger
    _ -> startFullXDS
