{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Web.Slack.Types.Event  where

import Web.Slack.Types.Base
import Web.Slack.Types.Bot
import Web.Slack.Types.Channel
import Web.Slack.Types.Comment
import Web.Slack.Types.Error
import Web.Slack.Types.Event.Subtype
import Web.Slack.Types.Message
import Web.Slack.Types.Time
import Web.Slack.Types.File
import Web.Slack.Types.Id
import Web.Slack.Types.IM
import Web.Slack.Types.Item
import Web.Slack.Types.Presence
import Web.Slack.Types.Time
import Web.Slack.Types.User

import Data.Aeson
import Data.Aeson.Types

import Control.Applicative
import Control.Lens.TH
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Network.URI
import Safe (readMay)
import Prelude

type Domain = Text

data Event where
  Hello :: Event
  Message :: ChannelId -> Submitter -> Text -> SlackTimeStamp -> Maybe Subtype -> Maybe Edited -> Event
  HiddenMessage :: ChannelId -> Submitter -> SlackTimeStamp -> Maybe Subtype -> Event
  ChannelMarked :: ChannelId -> SlackTimeStamp -> Event
  ChannelCreated :: Channel -> Event
  ChannelJoined :: Channel -> Event
  ChannelLeft   :: ChannelId -> Event
  ChannelDeleted :: ChannelId -> Event
  ChannelRename :: ChannelRenameInfo -> Event
  ChannelArchive :: ChannelId -> UserId -> Event
  ChannelUnarchive :: ChannelId -> UserId -> Event
  ChannelHistoryChanged :: SlackTimeStamp -> SlackTimeStamp -> SlackTimeStamp -> Event
  ImCreated :: UserId -> IM -> Event
  ImOpen :: UserId -> IMId -> Event
  ImClose :: UserId -> IMId -> Event
  ImMarked :: IMId -> SlackTimeStamp -> Event
  ImHistoryChanged :: SlackTimeStamp -> SlackTimeStamp -> SlackTimeStamp -> Event
  GroupJoined :: Channel -> Event
  GroupLeft :: Channel -> Event
  GroupOpen :: UserId -> ChannelId -> Event
  GroupClose :: UserId -> ChannelId -> Event
  GroupArchive :: ChannelId -> Event
  GroupUnarchive :: ChannelId -> Event
  GroupRename :: ChannelRenameInfo -> Event
  GroupMarked :: ChannelId -> SlackTimeStamp -> Event
  GroupHistoryChanged :: SlackTimeStamp -> SlackTimeStamp -> SlackTimeStamp -> Event
  FileCreated :: File -> Event
  FileShared :: FileReference -> Event
  FileUnshared :: File -> Event
  FilePublic :: FileReference -> Event
  FilePrivate :: FileId -> Event
  FileChange  :: File -> Event
  FileDeleted :: FileId -> SlackTimeStamp -> Event
  FileCommentAdded :: File -> Comment -> Event
  FileCommentEdited :: File -> Comment -> Event
  FileCommentDeleted :: File -> CommentId -> Event
  PresenceChange :: UserId -> Presence -> Event
  ManualPresenceChange :: Presence -> Event
  PrefChange :: Pref -> Event
  UserChange :: User -> Event
  TeamJoin :: User -> Event
  ReactionAdded :: UserId -> Text -> UserId {- item author -} -> EmbeddedItem -> SlackTimeStamp -> Event
  ReactionRemoved :: UserId -> Maybe Text -> EmbeddedItem -> SlackTimeStamp -> Event
  StarAdded :: UserId -> Item -> SlackTimeStamp -> Event
  StarRemoved :: UserId -> Item -> SlackTimeStamp -> Event
  EmojiChanged :: SlackTimeStamp -> Event
  CommandsChanged :: SlackTimeStamp -> Event
  TeamPrefChange :: Pref -> Event
  TeamRenameEvent :: Text -> Event
  TeamDomainChange :: URL -> Domain -> Event
  EmailDomainChange :: Domain -> SlackTimeStamp -> Event
  BotChanged :: Bot -> Event
  BotAdded :: Bot -> Event
  AccountsChanged :: Event
  UserTyping :: ChannelId -> UserId -> Event
  MessageResponse :: Int -> SlackTimeStamp -> Text -> Event
  MessageError :: Int -> SlackError -> Event
  StatusChange :: UserId -> Text -> SlackTimeStamp -> Event
  Pong :: Time -> Event
  ReconnectUrl :: URL -> Event
  TeamMigrationStarted :: Event
  MemberJoinedChannel :: UserId -> ChannelId -> Event
  MemberLeftChannel :: UserId -> ChannelId -> Event
  DesktopNotification :: Text -> Event
  -- Unstable
  PinAdded :: Event
  PinRemoved :: Event
  NoEvent :: Event
  -- Events API
  Challenge :: Text -> Event -- ^ Sent by Slack when adding an integration
  InteractiveMessage :: Interaction -> Event -- ^ User clicks on an action button
  -- Parsing failing of an event
  UnknownEvent :: Value -> Event
  deriving (Show)

data EventMessage = EventMessage
  { eventMessageTimestamp :: POSIXTime
  , eventMessageAttachments :: [Attachment]
  , eventMessageText :: Text
  , eventMessageUser :: UserId
  , eventMessageType :: Text
  , eventMessageBotId :: BotId
  } deriving Show

type Pref = (Text, Value)

instance FromJSON Event where
  parseJSON o@(Object v) = do
    (typ :: Maybe Text) <- v .:? "type"
    case typ of
      Just t -> parseType o t
      Nothing -> do
        (ok :: Bool) <- v .: "ok"
        if ok
          then MessageResponse <$> v .: "reply_to" <*> v .: "ts" <*> v .: "text"
          else MessageError <$> v .: "reply_to" <*> v .: "error"
  parseJSON Null = return NoEvent
  parseJSON _ = error "Expecting object: Event"

parseType :: Value -> Text -> Parser Event
parseType o@(Object v) typ =
    case typ of
      "hello" -> return Hello

      "message" -> do
        subt <- (\case
                  Nothing -> return Nothing
                  Just r  -> Just <$> subtype r o) =<< v .:? "subtype"
        submitter <- case subt of
                      Just (SBotMessage bid _ _) -> return $ BotComment bid
                      _                          -> maybe System UserComment <$> v .:? "user"
        void $ (v .: "channel" :: Parser ChannelId)
        hidden <- (\case {Just True -> True; _ -> False}) <$> v .:? "hidden"
        if not hidden
          then Message <$>  v .: "channel" <*> pure submitter  <*> v .: "text" <*> v .: "ts" <*> pure subt <*> v .:? "edited"
          else HiddenMessage <$>  v .: "channel" <*> pure submitter  <*> v .: "ts" <*> pure subt
      "user_typing" -> UserTyping <$> v .: "channel" <*> v .: "user"
      "presence_change" -> PresenceChange <$> v .: "user" <*> v .: "presence"
      "channel_marked"  -> ChannelMarked <$> v .: "channel" <*> v .: "ts"
      "channel_created" -> ChannelCreated <$> v .: "channel"
      "channel_joined"  -> ChannelJoined <$> v .: "channel"
      "channel_left"    -> ChannelLeft <$> v .: "channel"
      "channel_deleted" -> ChannelDeleted <$> v .: "channel"
      "channel_rename"  -> ChannelRename <$> v .: "channel"
      "channel_archive" -> ChannelArchive <$> v .: "channel" <*> v .: "user"
      "channel_unarchive" -> ChannelUnarchive <$> v .: "channel" <*> v .: "user"
      "channel_history_changed" -> ChannelHistoryChanged <$> v .: "latest" <*> v .: "ts" <*> v .: "event_ts"
      "im_open"     -> ImOpen <$> v .: "user" <*> v .: "channel"
      "im_created" -> ImCreated <$> v .: "user" <*> v .: "channel"
      "im_close" -> ImClose <$> v .: "user" <*> v .: "channel"
      "im_marked" -> ImMarked <$> v .: "channel" <*> v .: "ts"
      "im_history_changed" -> ImHistoryChanged <$> v .: "latest" <*> v .: "ts" <*> v .: "event_ts"
      "group_joined" -> GroupJoined <$> v .: "channel"
      "group_left" ->  GroupLeft <$> v  .: "channel"
      "group_open" ->  GroupOpen <$> v .: "user" <*> v .: "channel"
      "group_close" -> GroupClose <$> v .: "user" <*> v .: "channel"
      "group_archive" -> GroupArchive <$> v .: "channel"
      "group_unarchive" -> GroupUnarchive <$> v .: "channel"
      "group_rename" -> GroupRename <$> v .: "channel"
      "group_marked" -> GroupMarked <$> v .: "channel" <*> v .: "ts"
      "group_history_changed" -> GroupHistoryChanged <$> v .: "latest" <*> v .: "ts" <*> v .: "event_ts"
      "file_created" -> FileCreated <$> v .: "file"
      "file_shared"  -> FileShared <$> v .: "file"
      "file_unshared" -> FileUnshared <$> v .: "file"
      "file_public"  -> FilePublic <$> v .: "file"
      "file_private" -> FilePrivate <$> v .: "file"
      "file_change"  -> FileChange <$> v .: "file"
      "file_deleted"  -> FileDeleted <$> v .: "file_id" <*> v .: "event_ts"
      "file_comment_added" -> FileCommentAdded <$> v .: "file" <*> v .: "comment"
      "file_comment_edited" -> FileCommentEdited <$> v .: "file" <*> v .: "comment"
      "file_comment_deleted" -> FileCommentDeleted <$> v .: "file" <*> v .: "comment"
      "manual_presence_change" -> ManualPresenceChange <$> v .: "presence"
      "pref_change" -> curry PrefChange <$> v .: "name" <*> v .: "value"
      "user_change" -> UserChange <$> v .: "user"
      "team_join"   -> TeamJoin <$> v .: "user"
      "reaction_added" -> ReactionAdded <$> v .: "user" <*> v .: "reaction" <*> v .: "item_user" <*> v .: "item" <*> v .: "event_ts"
      "reaction_removed" -> ReactionRemoved <$> v .: "user" <*> v .:? "name" <*> v .: "item" <*> v .: "event_ts"
      "star_added" ->  StarAdded <$> v .: "user" <*> v .: "item" <*> v .: "event_ts"
      "star_removed" -> StarRemoved <$> v .: "user" <*> v .: "item" <*> v .: "event_ts"
      "emoji_changed" -> EmojiChanged <$> v .: "event_ts"
      "commands_changed" -> CommandsChanged <$> v .: "event_ts"
      "team_pref_change" -> curry TeamPrefChange <$> v .: "name" <*> v .: "value"
      "team_rename" -> TeamRenameEvent <$> v .: "name"
      "team_domain_change" -> TeamDomainChange <$> v .: "url" <*> v .: "domain"
      "email_domain_changed" -> EmailDomainChange <$> v .: "email_domain" <*> v .: "event_ts"
      "bot_added" -> BotAdded <$> v .:  "bot"
      "bot_changed" -> BotChanged <$> v .: "bot"
      "accounts_changed" -> pure AccountsChanged
      "status_change" -> StatusChange <$> v .: "user" <*> v .: "status" <*> v .: "event_ts"
      "pong" -> Pong <$> v .: "timestamp"
      "reconnect_url" -> ReconnectUrl <$> v .: "url"
      "team_migration_started" -> pure TeamMigrationStarted
      "desktop_notification" -> DesktopNotification <$> v .: "msg"
      "pin_added" -> pure PinAdded
      "pin_removed" -> pure PinRemoved
      "member_joined_channel" -> MemberJoinedChannel <$> v .: "user" <*> v .: "channel"
      "member_left_channel" -> MemberLeftChannel <$> v .: "user" <*> v .: "channel"
      "url_verification" -> Challenge <$> v .: "challenge"
      "interactive_message" -> InteractiveMessage <$> parseJSON o
      _ -> return $ UnknownEvent o
parseType _ _ = error "Expecting object"

-- | The contents of an action message
data Interaction = Interaction
  { interactionURI        :: URI
  , interactionActions    :: [Action]
  , interactionMessage    :: Maybe EventMessage
  , interactionCallbackId :: Text
  , interactionTriggerId  :: TriggerId
  , interactionUser       :: UserInfo
  } deriving Show

instance FromJSON Interaction where
  parseJSON = withObject "InteractiveMessage" $ \v ->
    Interaction <$> v .: "response_url"
                <*> v .: "actions"
                <*> v .:? "original_message"
                <*> v .: "callback_id"
                <*> v .: "trigger_id"
                <*> parseUserInfo v

parseUserInfo :: Object -> Parser UserInfo
parseUserInfo o = do
  u <- o .: "user"
  t <- o .: "team"
  UserInfo <$> u .: "id" <*> u .: "name" <*> t .: "id" <*> t .: "domain"

instance FromJSON URI where
  parseJSON = withText "Absolute URI" $ \t ->
    case parseAbsoluteURI (T.unpack t) of
      Just u -> pure u
      Nothing -> error "Could not parse absolute URI reference"

data Submitter = UserComment UserId | BotComment BotId | System deriving (Show, Eq)

data ChannelRenameInfo = ChannelRenameInfo
                       { _channelRenameId      :: ChannelId
                       , _channelRenameName    :: Text
                       , _channelRenameCreated :: Time } deriving Show

makeLenses ''ChannelRenameInfo

instance FromJSON ChannelRenameInfo where
  parseJSON = withObject "ChannelRenameInfo" (\o -> ChannelRenameInfo <$> o .: "id" <*> o .: "name" <*> o .: "created")

instance FromJSON EventMessage where
  parseJSON = withObject "EventMessage" $ \o ->
    EventMessage <$> ts o <*> o .: "attachments" <*> o .: "text" <*> o .: "user" <*> o .: "type" <*> o .: "bot_id"
    where
      ts o = (o .: "ts") <|> ((o .: "ts") >>= str)
      str = withText "timestamp string" $ \s ->
        case (readMay (T.unpack s) :: Maybe Double) of
          Just ts -> pure (realToFrac ts)
          Nothing -> error "couldn't parse timestamp string as double"

instance ToJSON EventMessage where
  toJSON (EventMessage ts as text user t bot) = object [ "ts" .= ts, "attachments" .= as, "text" .= text, "user" .= user, "type" .= t, "bot_id" .= bot ]


makePrisms ''Event
