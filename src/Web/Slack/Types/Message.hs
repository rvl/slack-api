{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Web.Slack.Types.Message where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Web.Slack.Types.Base
import Web.Slack.Types.Id
import Web.Slack.Utils

data MessagePayload = MessagePayload
    { messageId      :: Int
    , messageType    :: T.Text
    , messageChannel :: ChannelId
    , messageText    :: T.Text
    } deriving (Show, Eq)

data PingPayload = PingPayload
    { pingId        :: Int
    , pingType      :: T.Text
    , pingTimestamp :: Int
    } deriving (Show, Eq)

data Attachment = Attachment
    { attachmentFallback :: T.Text
        -- ^ A plain-text summary of the attachment.
    , attachmentColor :: AttachmentColor
        -- ^ Used to color the border along the left side of the message
        -- attachment.
    , attachmentPretext :: Maybe T.Text
        -- ^ Optional text that appears above the message attachment block.
    , attachmentAuthorName :: Maybe T.Text
        -- ^ Small text used to display the author's name.
    , attachmentAuthorLink :: Maybe URL
        -- ^ A valid URL that will hyperlink the author_name text mentioned
        -- above.
    , attachmentAuthorIcon :: Maybe URL
        -- ^ A valid URL that displays a small 16x16px image to the left of
        -- the author_name text.
    , attachmentTitle :: Maybe T.Text
        -- ^ The title is displayed as larger, bold text near the top of
        -- a message attachment.
    , attachmentTitleLink :: Maybe URL
        -- ^ By passing a valid URL, the title text will be hyperlinked.
    , attachmentText :: Maybe T.Text
        -- ^ This is the main text in a message attachment, and can contain
        -- standard message markup.
    , attachmentFields :: [Field]
    , attachmentImageUrl :: Maybe URL
        -- ^ An image file that will be displayed inside a message
        -- attachment. GIF, JPEG, PNG, or BMP; scaled down to 400x500px.
    , attachmentThumbUrl :: Maybe URL
        -- ^ Displayed as a thumbnail on the right side of a message
        -- attachment. GIF, JPEG, PNG, or BMP; scaled down to 75x75px.
    , attachmentFooter :: Maybe T.Text
        -- ^ Add some brief text to help contextualize and identify an
        -- attachment.
    , attachmentFooterIcon :: Maybe URL
        -- ^ Render a small icon beside your footer text. Scaled to 16x16px.
    , attachmentTs :: Maybe POSIXTime
        -- ^ Display an additional timestamp value as part of the
        -- attachment's footer.
    , attachmentActions :: [Action]
        -- ^ Actions such as buttons and menus, allowing for
        -- <https://api.slack.com/interactive-messages interactive messages>.
    , attachmentCallbackId :: Maybe T.Text
        -- ^ An identifier for the set of actions in an interactive attachment.
        -- It is sent back to the action URL when an action is invoked.
    } deriving (Show, Eq)

data Field = Field
    { fieldTitle :: Maybe T.Text
        -- ^ Shown as a bold heading above the value text. It cannot
        -- contain markup and will be escaped for you.
    , fieldValue :: T.Text
        -- ^ The text value of the field. It may contain standard message
        -- markup and must be escaped as normal. May be multi-line.
    , fieldShort :: Bool
        -- ^ Whether the value is short enough to be displayed side-by-side
        -- with other values.
    } deriving (Show, Eq)

data AttachmentColor
    = DefaultColor       -- grey
    | GoodColor          -- green
    | WarningColor       -- yellow
    | DangerColor        -- red
    | CustomColor T.Text -- hexadecimal RGB colour, eg. CustomColor "#439FE0"
    deriving (Show, Eq)

data Action = Action
    { actionName :: T.Text
        -- ^ Returned to your callback URL.
    , actionText :: T.Text
        -- ^ User-facing label. Should have a maximum of 30 characters.
    , actionType :: ActionType
        -- ^ Whether this action is a button or a menu.
    , actionValue :: Maybe T.Text
        -- ^ Provided, along with `actionName`, to your callback URL.
    , actionStyle :: Maybe ButtonStyle
        -- ^ How to style the action. Only used for buttons.
    , actionOptions :: [MenuOption]
        -- ^ Menu options. Only used for menus.
    } deriving (Eq, Show, Read)

data ActionType
    = ButtonType
    | MenuType
    deriving (Eq, Show, Read, Enum)

data ButtonStyle
    = DefaultStyle
    | PrimaryStyle
    | DangerStyle
    deriving (Eq, Show, Read, Enum)

data MenuOption = MenuOption
    { menuOptionText :: T.Text
        -- ^ Short, user-facing label. Should have a maximum of 30 characters.
    , menuOptionValue :: T.Text
        -- ^ A short string that identifies the option to the application. Up
        -- to 2000 characters.
    , menuOptionDescription :: T.Text
        -- ^ User-facing string with more details about option. Should also
        -- have at most 30 characters
    } deriving (Eq, Show, Read)


defaultAttachment :: Attachment
defaultAttachment = Attachment
        { attachmentFallback = ""
        , attachmentColor = DefaultColor
        , attachmentPretext = Nothing
        , attachmentAuthorName = Nothing
        , attachmentAuthorLink = Nothing
        , attachmentAuthorIcon = Nothing
        , attachmentTitle = Nothing
        , attachmentTitleLink = Nothing
        , attachmentText = Nothing
        , attachmentFields = []
        , attachmentImageUrl = Nothing
        , attachmentThumbUrl = Nothing
        , attachmentFooter = Nothing
        , attachmentFooterIcon = Nothing
        , attachmentTs = Nothing
        , attachmentActions = []
        , attachmentCallbackId = Nothing
        }

instance ToJSON ActionType where
    toJSON x = String $ case x of
        ButtonType -> "button"
        MenuType   -> "select"

instance FromJSON ActionType where
  parseJSON = withText "ActionType" $ \case
    "button" -> pure ButtonType
    "select" -> pure MenuType
    t -> fail $ "Unknown action type: " ++ T.unpack t

instance ToJSON ButtonStyle where
    toJSON x = String $ case x of
        DefaultStyle -> ""
        PrimaryStyle -> "primary"
        DangerStyle  -> "danger"

instance FromJSON ButtonStyle where
    parseJSON v = (parseJSON v :: Parser (Maybe T.Text)) >>= \case
      Nothing -> pure DefaultStyle
      Just "" -> pure DefaultStyle
      Just "primary" -> pure PrimaryStyle
      Just "danger" -> pure DangerStyle
      Just s -> fail $ "Unknown button style: " ++ T.unpack s

instance ToJSON AttachmentColor where
    toJSON x = String $ case x of
        DefaultColor  -> ""
        GoodColor     -> "good"
        WarningColor  -> "warning"
        DangerColor   -> "danger"
        CustomColor c -> c

instance FromJSON AttachmentColor where
    parseJSON v = (parseJSON v :: Parser (Maybe T.Text)) >>= \case
      Just "good" -> pure GoodColor
      Just "warning" -> pure WarningColor
      Just "danger" -> pure DangerColor
      Just c -> pure $ CustomColor c
      Nothing -> pure DefaultColor

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o ->
    Attachment
        <$> o .:? "fallback" .!= ""
        <*> o .:? "color" .!= DefaultColor
        <*> o .:? "pretext"
        <*> o .:? "author_name"
        <*> o .:? "author_link"
        <*> o .:? "author_icon"
        <*> o .:? "title"
        <*> o .:? "title_link"
        <*> o .:? "text"
        <*> o .:? "fields" .!= []
        <*> o .:? "image_url"
        <*> o .:? "thumb_url"
        <*> o .:? "footer"
        <*> o .:? "footer_icon"
        <*> o .:? "ts"
        <*> o .:? "actions" .!= []
        <*> o .:? "callback_id"

instance FromJSON Action where
  parseJSON = withObject "Action" $ \o ->
    Action
      <$> o .: "name"
      <*> o .:? "text" .!= ""
      <*> o .:? "type" .!= ButtonType
      <*> o .:? "value"
      <*> o .:? "style"
      <*> o .:? "options" .!= []

$(deriveJSON defaultOptions {fieldLabelModifier = toSnake . drop 7}  ''MessagePayload)
$(deriveJSON defaultOptions {fieldLabelModifier = toSnake . drop 4}  ''PingPayload)
$(deriveJSON defaultOptions {fieldLabelModifier = toSnake . drop 10} ''MenuOption)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 6}  ''Action)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 10} ''Attachment)
$(deriveJSON defaultOptions {fieldLabelModifier = toSnake . drop 5}  ''Field)
