{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Vk.Networking
  ( getServer
  , getServerRaw
  , listenUpdates
  , getNextEventsRaw
  ) where

import qualified Data.Text as T
import qualified Vk.Types as VK
import Data.Maybe (fromJust)
import Network.HTTP.Req

import Control.Monad.State.Strict
import Data.Aeson (parseJSON, decodeStrict, Array, (.:))
import Data.Aeson.Types
  (FromJSON, parseMaybe, withArray, withObject, Parser, Value(..))
import Data.ByteString (ByteString)
import Data.Scientific (toBoundedInteger)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector.Generic ((!), basicLength)
import Data.Vector (Vector)
import Lens.Micro ((.~), (^.), (&), _Just, set)
import Lens.Micro.TH

vkApiVersion = "5.92" :: String
vkApiUrl = "api.vk.com"

type ApiResponse a = Either VK.ErrorInfo a

makeLenses ''VK.Handle
makeLenses ''VK.Event
makeLenses ''VK.LongPollServerInfo

parseApiResponse
  :: forall a . FromJSON a => ByteString -> Either VK.ErrorInfo a
parseApiResponse input =
  let
    goodObject  = decodeStrict input :: Maybe a
    errorObject = decodeStrict input :: Maybe VK.ErrorInfo
  in case goodObject of
    Just etw -> Right etw
    Nothing  -> extractErr errorObject
 where
  extractErr (Just e) = Left e
  extractErr Nothing  = Left VK.ErrorInfo
    { VK.errorCode = 0
    , VK.errorMsg  = "Unsupported response"
    }

getServerRaw :: VK.Handle -> IO ByteString
getServerRaw vk = runReq defaultHttpConfig $ do
  r <- req
    POST
    (https "api.vk.com" /: "method" /: "messages.getLongPollServer")
    (  ReqBodyUrlEnc
       $  "access_token" =: vk ^. key
       <> "need_pts" =: needPts
       <> "v" =: vkApiVersion
       <> "lp_version" =: lpVersion
    )
    bsResponse
    mempty

  return (responseBody r :: ByteString)
 where
  needPts   = 1 :: Int
  lpVersion = "3" :: String

getServer :: VK.Handle -> IO (ApiResponse VK.LongPollServerInfo)
getServer vk = do
  response <- getServerRaw vk
  return $ parseApiResponse response


updateServer :: VK.Handle -> IO VK.Handle
updateServer h = do
  resp <- getServer h
  case resp of
    Right etw -> return $ setServerInfo h etw
    Left  err -> fail $ show err

 where
  setServerInfo :: VK.Handle -> VK.LongPollServerInfo -> VK.Handle
  setServerInfo oldHandle info = set serverInfo (Just info) oldHandle

buildServerAddr :: (T.Text -> Url a) -> [T.Text] -> Url a
buildServerAddr fn (x : xs) =
  let
    add cmp url = url /: cmp
    init = fn x
  in foldr add init xs

-- TODO: add retries, modify ts when the method succeeds
getNextEventsRaw :: VK.Handle -> IO ByteString
getNextEventsRaw vk = do
  r <- runReq defaultHttpConfig $ req
    GET
    (buildServerAddr https serverParts)
    NoReqBody
    bsResponse
    (  "act" =: act
    <> "key" =: key
    <> "ts"  =: ts
    <> "version" =: lpVersion
    <> "wait" =: wait
    <> "mode" =: mode
    )

  return (responseBody r :: ByteString)
 where
  act = "a_check" :: String

  serverParts =
    let info = fromJust $ vk ^. serverInfo
    in T.split (== '/') $ T.pack $ info ^. lpServer
  key = let info = fromJust $ vk ^. serverInfo
        in info ^. lpKey
  ts =
    let info = fromJust $ vk ^. serverInfo in (T.pack . show . VK._lpTs) info
  wait      = 25 :: Int
  lpVersion = "3" :: String
  mode      = 234 :: Int


asText :: Value -> String
asText (String s) = T.unpack s

asInt :: Value -> Int
asInt (Number sc) = fromJust $ toBoundedInteger sc

asVector :: Value -> Array
asVector (Array a) = a

code :: Array -> Int
code t = asInt $ (asVector (t ! 0)) ! 0

extractMessage :: Array -> VK.Updates
extractMessage arr =
  let
    firstArr       = asVector (arr ! 0)
    getUserId      = asInt $ firstArr ! 3
    getMessageText = asText $ firstArr ! 5
  in VK.Message { VK.userId = getUserId, VK.messageText = getMessageText }

parseEvent :: Value -> Parser VK.Event
parseEvent = withObject "updates" $ \obj -> do
  ts       <- obj .: "ts"
  pts      <- obj .: "pts"
  rawArray <- obj .: "updates"
  updates  <- parseUpdates rawArray
  return $ VK.Event updates ts pts

parseUpdates :: Value -> Parser VK.Updates
parseUpdates = withArray "events" fn
 where
  fn :: Array -> Parser VK.Updates
  fn arr =
    let isEmpty = 0 == basicLength arr
    in
      return $
      if isEmpty then VK.Empty
      else if (code arr) == 4 then extractMessage arr
      else VK.Unsupported

listenUpdates :: (VK.Handle -> IO ()) -> StateT VK.Handle IO ()
listenUpdates listener = do
  h   <- get
  srv <- liftIO $ updateServer h
  put srv
  listenNext listener 
    where
    listenNext :: (VK.Handle -> IO ()) -> StateT VK.Handle IO ()
    listenNext listener = do
      srv <- get
      ev <- liftIO $ getNextEventsRaw srv
      let event = decodeStrict ev >>= parseMaybe parseEvent
      put $ case event of
        Just etw ->
          srv
            &  lastReadUpdate .~ (etw ^. updates)
            &  lastReadTs .~ (etw ^. ts)
            &  comment .~ show event
            &  (serverInfo . _Just . lpTs) .~ (etw ^. ts)
        Nothing -> srv & comment .~ show event
      t <- get
      liftIO $ listener t
      listenNext listener
