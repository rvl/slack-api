{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Slack.Types.Id
  ( UserId,
    BotId,
    ChannelId,
    FileId,
    CommentId,
    IMId,
    TeamId,
    TriggerId,
    GroupId,
    Id(..),
    getId
  ) where

import Control.Lens.TH
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics

data FieldType = TUser | TBot | TChannel | TFile | TComment | TIM | TTeam | TGroup | TTrigger deriving (Eq, Show)

newtype Id (a :: FieldType) = Id { _getId :: Text } deriving (Show, Eq, Ord, Generic)


instance ToJSON (Id a) where
  toJSON (Id uid) = String uid

instance FromJSON (Id a) where
  parseJSON = withText "Id" (return . Id)

instance Hashable (Id a)

type UserId     = Id 'TUser
type BotId      = Id 'TBot
type ChannelId  = Id 'TChannel
type FileId     = Id 'TFile
type CommentId  = Id 'TComment
type IMId       = Id 'TIM
type TeamId     = Id 'TTeam
type GroupId    = Id 'TGroup
type TriggerId  = Id 'TTrigger

makeLenses ''Id
