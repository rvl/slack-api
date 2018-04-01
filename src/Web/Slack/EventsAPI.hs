{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}

-- | The Slack Events API allows you to implement /slash commands and
-- interactive messages.
--
-- To use the Events API, you must run a HTTP
-- server with the WAI Application provided by this module and
-- configure your Slack integration to connect to it.
--
-- Slack authenticates itself with a token which should match the
-- 'eaVerificationToken'.

module Web.Slack.EventsAPI
  ( EventsApplication(..)
  , CommandRequest(..)
  , EventResponse(..)
  , eventsApplication
  , defaultEventsApplication
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.Wai (Application, Middleware, Request(pathInfo))
import Network.HTTP.Types.Status (unauthorized401, notFound404, badRequest400)
import Web.Scotty
import Data.Aeson hiding (json)
import Data.Aeson.Types (Parser, parseEither)
import Network.URI
import Control.Error (fmapL, note)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid

import Web.Slack.Types.Id
import Web.Slack.Types.Message
import Web.Slack.Types.Event

-- | An Events API application has three endpoints configured with
-- this record.
data EventsApplication = EventsApplication
  { eaVerificationToken :: Text
    -- ^ Authenticates the Slack server
  , eaCommandHandlers   :: [(Text, CommandRequest -> IO (Maybe EventResponse))]
    -- ^ Slash command handlers
  , eaActionHandler     :: Interaction -> IO (Maybe EventResponse)
    -- ^ Interactive message action handler
  , eaEventHandler      :: Event -> IO (Maybe Value)
    -- ^ Events API event handler
  }

-- | An Events API application which does nothing but authenticate
-- requests.
defaultEventsApplication :: Text -> EventsApplication
defaultEventsApplication token = EventsApplication token []
                                 (const (pure Nothing))
                                 (const (pure Nothing))

-- | Slash command details.
data CommandRequest = CommandRequest
  { slackCommandRequestTeamId :: TeamId
  , slackCommandRequestChannelId :: ChannelId
  , slackCommandRequestUserId :: UserId
  , slackCommandRequestText :: Text
  , slackCommandRequestResponseURL :: URI
  , slackCommandRequestTriggerId :: Text
  } deriving (Show, Eq)

-- | A message to send back. It is acceptable to send an empty
-- message.
data EventResponse = EventResponse
  { slackResponseText :: Text
  , slackResponseAttachments :: [Attachment]
  } deriving (Show, Eq)

instance Monoid EventResponse where
  mempty = EventResponse "" []
  mappend (EventResponse t a) (EventResponse u b) = EventResponse (t <> u) (a <> b)

paramVerified :: FromJSON a => Text -> TL.Text -> ActionM a
paramVerified token name = param name >>= jsonDataWithToken' token

-- | A WAI Application with the three endpoints.
eventsApplication :: EventsApplication -> IO Application
eventsApplication EventsApplication{..} = scottyApp $ do
  post "/slack/action" $ do
    paramVerified eaVerificationToken "payload" >>= \case
      InteractiveMessage msg -> do
        (liftAndCatchIO $ eaActionHandler msg) >>= \case
          Just response -> json response
          Nothing -> pure ()
      _ -> do
        status badRequest400
        text "Action is of wrong type"

  post "/slack/event" $ do
    jsonDataWithToken eaVerificationToken >>= \case
      Challenge c -> json $ object ["challenge" .= c]
      ev          -> liftAndCatchIO (eaEventHandler ev) >>= \case
        Just response -> json response
        Nothing       -> pure ()

  post "/slack/command" $ do
    token <- param "token"
    if token == eaVerificationToken
      then do
        command <- param "command"
        req <- CommandRequest
               <$> param "team_id"
               <*> param "channel_id"
               <*> param "user_id"
               <*> param "text"
               <*> param "response_url"
               <*> param "trigger_id"
        case lookup command eaCommandHandlers of
          Just handler -> liftAndCatchIO (handler req) >>= \case
            Just msg -> json msg
            Nothing -> Web.Scotty.text ""
          Nothing -> do
            status notFound404
            text "Unknown command"
      else badToken

tokenParser :: Text -> Value -> Parser ()
tokenParser token = withObject "Authenticated Request" $ \o -> do
  token' <- o .: "token"
  if token' == token
    then pure ()
    else fail "Verification token did not match"

jsonDataWithToken :: FromJSON a => Text -> ActionM a
jsonDataWithToken token = do
  v <- jsonData
  jsonDataWithToken' token v

jsonDataWithToken' :: FromJSON a => Text -> Value -> ActionM a
jsonDataWithToken' token v = case parseEither (tokenParser token) v of
    Right () -> case fromJSON v of
      Success a -> pure a
      Error e -> do
        status badRequest400
        Web.Scotty.text $ TL.pack $ "Problem decoding json: " ++ e ++ ". Value: " ++ show v
        finish
    Left _ -> badToken

badToken :: ActionM a
badToken = do
  status unauthorized401
  Web.Scotty.text ("Verification token did not match")
  finish

instance Parsable Value where
  parseParam = fmapL TL.pack . eitherDecode . TL.encodeUtf8

instance Parsable URI where
  parseParam = note "Could not parse URI" . parseAbsoluteURI . TL.unpack

instance Parsable (Id a) where
  parseParam = fmap Id . parseParam

instance ToJSON EventResponse where
  toJSON (EventResponse t as) = object [ "text" .= t, "attachments" .= as ]
