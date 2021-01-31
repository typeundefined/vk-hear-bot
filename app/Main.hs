{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Vk.Types as V
import Vk.Networking
import Control.Monad.State.Strict
import Data.Configurator

showState :: V.Handle -> IO ()
showState = putStrLn . show

main :: IO ()
main = do
  config <- load [Required ".vk-hear-bot.conf"]
  key    <- require config "vk.apiKey" :: IO String
  let handle = V.createHandle key
  s <- execStateT (listenUpdates showState) handle
  putStrLn $ show s
