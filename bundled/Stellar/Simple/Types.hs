{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stellar.Simple.Types where

-- global
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.Stellar.TransactionXdr qualified as XDR

newtype Shown a = Shown String
    deriving newtype (Eq)

instance Show (Shown a) where
    show (Shown s) = s

shown :: Show a => a -> Shown a
shown = Shown . show

data Asset = Asset{issuer :: Maybe Text, code :: Text}
    deriving (Eq, Generic, Ord, Read, Show)

-- | Representation is "XLM" or "{code}:{issuer}"
assetToText :: Asset -> Text
assetToText Asset{code, issuer} = code <> maybe "" (":" <>) issuer

assetFromText :: Text -> Asset
assetFromText t
    | Text.null _issuer = Asset{code = t, issuer = Nothing}
    | otherwise         = Asset{code, issuer = Just issuer}
  where
    (code, _issuer) = Text.break (== ':') t
    issuer = Text.drop 1 _issuer

data Memo = MemoNone | MemoText Text | MemoOther (Shown XDR.Memo)
    deriving (Eq, Show)

data PaymentType = DirectPayment | PathPayment
    deriving (Generic, Read, Show)

data DecoratedSignature = DecoratedSignature{hint, signature :: ByteString}
    deriving Show
