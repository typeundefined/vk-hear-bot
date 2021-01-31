{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    , ResponseDTO (..)
    ) where

import Data.Aeson
import GHC.Generics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ResponseDTO = ResponseDTO {
  _msg :: String,
  _msg_id :: Int
} deriving (Generic)

instance ToJSON ResponseDTO where
  toJSON = genericToJSON defaultOptions {
            fieldLabelModifier = drop 1 <$> dropWhile (/= '_')
  }


