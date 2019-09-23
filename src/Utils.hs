module Utils where

import Prelude

import System.Environment (lookupEnv)
import Data.Maybe (Maybe(..), fromMaybe)

  
getEnvConf :: String -> String -> IO (String)
getEnvConf key def = (fromMaybe def) <$> (lookupEnv key)
