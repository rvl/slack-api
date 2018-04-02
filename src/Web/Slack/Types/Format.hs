{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- | Functions for parsing and formatting Slack messages.
-- Based on code from <https://github.com/hlian/linklater>.
module Web.Slack.Types.Format
  ( Format(..)
  , format
  , unformat
  , unformatOnlyText
  , formatParser
  , isMentioned
  , mentionUser
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Monoid
import Data.Attoparsec.Text
import Control.Applicative
import Data.String

import Web.Slack.Types.Id

-- | A little DSL for <https://api.slack.com/docs/formatting Slack formatting>.
data Format =
  -- | @"\<\@U024BE7LH|username>"@
    FormatUser !UserId !(Maybe Text)
  -- | @"\<#C024BE7LR|general>
  | FormatChannel !ChannelId !(Maybe Text)
  -- | @"\<http://www.foo.com|www.foo.com>"@
  | FormatLink !Text !(Maybe Text)
  -- | @"user did this &amp; that"@
  | FormatString !Text
  deriving (Show, Eq)

instance IsString [Format] where
  fromString = format . T.pack

-- | Present a list of format tokens as text.
unformat :: [Format] -> Text
unformat = mconcat . map unformat1

unformat1 :: Format -> Text
unformat1 (FormatUser (Id u) t)    = unformatEntity "@" u t
unformat1 (FormatChannel (Id c) t) = unformatEntity "#" c t
unformat1 (FormatLink url t)       = unformatEntity "" url t
unformat1 (FormatString t)         = escape t

-- | Only the text content of a message -- no mentions, urls, etc.
unformatOnlyText :: [Format] -> Text
unformatOnlyText msg = mconcat [escape t | FormatString t <- msg]

-- | Parse message text from slack into a Format tokens. If parsing
-- fails, an empty list is returned.
format :: Text -> [Format]
format t = case parseOnly (formatParser <* endOfInput) t of
  Right f -> f
  Left _ -> []

unformatEntity :: Text -> Text -> Maybe Text -> Text
unformatEntity p eid mt = mconcat ["<", p, eid, maybe "" ("|" <>) mt, ">"]

escape, unescape :: Text -> Text
(escape, unescape) = (replace escapes, replace (map swap escapes))
  where
    replace = flip (foldr (uncurry T.replace))
    escapes = [("<", "&lt;"), (">", "&gt;"), ("&", "&amp;")]

-- | Attoparsec parser for Slack format strings.
formatParser :: Parser [Format]
formatParser = many' (formatUser <|> formatChannel <|> formatLink <|> formatString)

formatString :: Parser Format
formatString = FormatString . unescape . T.pack <$> many1 (notChar '<')

formatUser :: Parser Format
formatUser = uncurry FormatUser . fstId <$> formatEntity "@"

formatChannel :: Parser Format
formatChannel = uncurry FormatChannel . fstId <$> formatEntity "#"

fstId :: (Text, a) -> (Id f, a)
fstId (t, a) = (Id t, a)

formatLink :: Parser Format
formatLink = uncurry FormatLink <$> formatEntity ""

formatEntity :: Text -> Parser (Text, Maybe Text)
formatEntity p = ((,) <$> (char '<' *> string p *> entityId) <*> linkText) <* char '>'
  where
    entityId :: Parser Text
    entityId = T.pack <$> many1 (satisfy (notInClass "|>"))
    linkText :: Parser (Maybe Text)
    linkText = Just . T.pack <$> (char '|' *> many1 (notChar '>')) <|> pure Nothing

-- | Look in message to see if a user is mentioned
isMentioned :: UserId -> [Format] -> Bool
isMentioned uid msg = or [mention == uid | FormatUser mention _ <- msg]

-- | Prefixes message text with @user
mentionUser :: UserId -> [Format] -> [Format]
mentionUser uid msg = (FormatUser uid Nothing:msg)
