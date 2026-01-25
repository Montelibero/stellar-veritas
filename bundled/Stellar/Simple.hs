{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Stellar.Simple where

-- prelude
import Prelude hiding (id)
import Prelude qualified

-- global
import Control.Exception (SomeException (SomeException), catchJust, throwIO)
import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (toList)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (Endo), appEndo)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8', encodeUtf8)
import Data.Typeable (cast)
import Data.Word (Word32, Word8)
import GHC.Stack (HasCallStack)
import Text.Read (readEither, readMaybe)

-- stellar-sdk
import Network.ONCRPC.XDR (XDR, xdrSerialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Keypair qualified as StellarKey
import Network.Stellar.Network (Network, publicNetwork)
import Network.Stellar.Signature qualified as StellarSignature
import Network.Stellar.TransactionXdr (Uint256)
import Network.Stellar.TransactionXdr qualified as XDR

-- component
import Stellar.Simple.Types (Asset (..), DecoratedSignature (..), Memo (..))

identity :: a -> a
identity = Prelude.id

-- | Make asset from the canonical pair of code and issuer
mkAsset :: Text -> Text -> Asset
mkAsset code issuer = Asset{code, issuer = Just issuer}

data Guess a = Already a | Guess
    deriving (Show)

signWithSecret ::
    HasCallStack =>
    -- | "S..." textual secret key
    Text ->
    XDR.TransactionEnvelope ->
    XDR.TransactionEnvelope
signWithSecret secret tx =
    either (error . show) identity $
    StellarSignature.signTx publicNetwork tx [StellarKey.fromPrivateKey' secret]

xdrSerializeBase64T :: XDR a => a -> Text
xdrSerializeBase64T = decodeUtf8Throw . Base64.encode . xdrSerialize

decodeUtf8Throw = either (error . show) identity . decodeUtf8'
