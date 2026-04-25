{-# LANGUAGE OverloadedStrings, Strict #-}

module Pretty where

import qualified Data.ByteString as B
import           Data.ByteString.Base32 (encodeBase32)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Network.ONCRPC.XDR.Array
import Network.Stellar.Keypair (encodePublic)
import Network.Stellar.TransactionXdr
import Numeric

b16 = T.pack . concatMap (\x -> (if x < 16 then "0" else "") ++ showHex x "") . B.unpack . unLengthArray
b32 = encodeBase32 . unLengthArray
utf8s = T.decodeUtf8Lenient . unLengthArray
prettyKey x = encodePublic $ unLengthArray x
prettyAmount amount = T.show ((fromIntegral amount) / 1e7)
prettyUnlines x = T.concat $ intersperse "\n" x
prettyAssetCode x = T.decodeUtf8Lenient $ B.takeWhile (/= 0) $ unLengthArray x

class Pretty a where
  pretty :: a -> T.Text

instance Pretty TransactionEnvelope where
  pretty (TransactionEnvelope'ENVELOPE_TYPE_TX_V0 x) = pretty x
  pretty (TransactionEnvelope'ENVELOPE_TYPE_TX x) = pretty x
  pretty (TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP x) = T.concat ["Fee bump to ", T.show x]

instance Pretty TransactionV0Envelope where
  pretty x = pretty $ transactionV0Envelope'tx x

instance Pretty TransactionV1Envelope where
  pretty x = pretty $ transactionV1Envelope'tx x

instance Pretty TransactionV0 where
  pretty x = prettyUnlines $ map T.concat
    [ ["Fee: ", T.show (transactionV0'fee x)]
    , ["Sequence: ", T.show (transactionV0'seqNum x)]
    ]

instance Pretty Transaction where
  pretty x = prettyUnlines $ map T.concat
    [ ["Source account: ", pretty $ transaction'sourceAccount x]
    , ["Memo: ", pretty $ transaction'memo x]
    , ["Operations:\n", prettyUnlines $ map (\op -> T.concat ["  ", pretty op]) $ V.toList $ unLengthArray $ transaction'operations x]
    , ["Fee: ", T.show (transaction'fee x), " stroops"]
    , ["Sequence: ", T.show (transaction'seqNum x)]
    , ["Conditions: ", pretty (transaction'cond x)]
    ]

instance Pretty Memo where
  pretty Memo'MEMO_NONE = ""
  pretty (Memo'MEMO_ID x) = T.show x
  pretty (Memo'MEMO_HASH x) = b16 x
  pretty (Memo'MEMO_RETURN x) = T.show x
  pretty (Memo'MEMO_TEXT t) = utf8s t

instance Pretty Preconditions where
  pretty Preconditions'PRECOND_NONE = "None"
  pretty (Preconditions'PRECOND_TIME (TimeBounds min max)) = T.concat ["Time ", T.show min, " to ", T.show max]
  pretty (Preconditions'PRECOND_V2 cond) = T.show cond

instance Pretty ClaimPredicate where
  pretty ClaimPredicate'CLAIM_PREDICATE_UNCONDITIONAL = "Unconditional"
  pretty x = T.show x

instance Pretty Operation where
  pretty (Operation Nothing body) = pretty body
  pretty (Operation (Just account) body) = T.concat ["As ", pretty account, " ", pretty body]

instance Pretty OperationBody where
  pretty (OperationBody'CREATE_ACCOUNT x) = pretty x
  pretty (OperationBody'PAYMENT x) = pretty x
--  pretty (OperationBody'PATH_PAYMENT_STRICT_RECEIVE x) = pretty x
  pretty (OperationBody'MANAGE_SELL_OFFER x) = pretty x
--  pretty (OperationBody'CREATE_PASSIVE_SELL_OFFER x) = pretty x
  pretty (OperationBody'SET_OPTIONS x) = pretty x
  pretty (OperationBody'CHANGE_TRUST x) = pretty x
--  pretty (OperationBody'ALLOW_TRUST x) = pretty x
--  pretty (OperationBody'ACCOUNT_MERGE x) = pretty x
  pretty OperationBody'INFLATION = "Inflation"
  pretty (OperationBody'MANAGE_DATA x) = pretty x
--  pretty (OperationBody'BUMP_SEQUENCE x) = pretty x
  pretty (OperationBody'MANAGE_BUY_OFFER x) = pretty x
--  pretty (OperationBody'PATH_PAYMENT_STRICT_SEND x) = pretty x
  pretty (OperationBody'CREATE_CLAIMABLE_BALANCE x) = pretty x
--  pretty (OperationBody'CLAIM_CLAIMABLE_BALANCE x) = pretty x
  pretty (OperationBody'BEGIN_SPONSORING_FUTURE_RESERVES (BeginSponsoringFutureReservesOp account)) = T.concat ["Sponsoring reserves for ", pretty account]
  pretty OperationBody'END_SPONSORING_FUTURE_RESERVES = "No longer sponsored reserves"
--  pretty (OperationBody'REVOKE_SPONSORSHIP x) = pretty x
  pretty (OperationBody'CLAWBACK x) = pretty x
--  pretty (OperationBody'CLAWBACK_CLAIMABLE_BALANCE x) = pretty x
  pretty (OperationBody'SET_TRUST_LINE_FLAGS x) = pretty x
--  pretty (OperationBody'LIQUIDITY_POOL_DEPOSIT x) = pretty x
--  pretty (OperationBody'LIQUIDITY_POOL_WITHDRAW x) = pretty x
  pretty x = T.show x

instance Pretty SetOptionsOp where
  pretty x = let prettyMaybe description prettifier x = pure $ T.concat ["    ", description, prettifier x] in
              T.concat ["Set options:\n", prettyUnlines $ catMaybes
              [ setOptionsOp'inflationDest x >>= prettyMaybe "Set inflation destination to " pretty
              , setOptionsOp'clearFlags x >>= prettyMaybe "Clear flags: " T.show
              , setOptionsOp'setFlags x >>= prettyMaybe "Set flags: " T.show
              , setOptionsOp'masterWeight x >>= prettyMaybe "Master key weight: " T.show
              , setOptionsOp'lowThreshold x >>= prettyMaybe "Low signing threshold: " T.show
              , setOptionsOp'medThreshold x >>= prettyMaybe "Medium signing threshold: " T.show
              , setOptionsOp'highThreshold x >>= prettyMaybe "High signing threshold: " T.show
              , setOptionsOp'homeDomain x >>= prettyMaybe "Home domain: " T.show
              , setOptionsOp'signer x >>= prettyMaybe "Signer: " pretty
              ]]

instance Pretty Signer where
  pretty (Signer key weight) = T.concat [pretty key, " (", T.show weight, ")"]

instance Pretty SignerKey where
  pretty (SignerKey'SIGNER_KEY_TYPE_ED25519 x) = prettyKey x
  pretty x = T.show x

instance Pretty CreateAccountOp where
  pretty (CreateAccountOp dest bal) = T.concat ["Create account ", pretty dest, " with starting balance ", T.show bal]

instance Pretty CreateClaimableBalanceOp where
  pretty (CreateClaimableBalanceOp ass amount claimants) = T.concat ["Create claimable ", prettyAmount amount, " ", pretty ass, " to ", pretty claimants]

instance Pretty SetTrustLineFlagsOp where
  pretty (SetTrustLineFlagsOp trustor ass clearFlags setFlags)
    | setFlags == 1 && clearFlags == 0 = T.concat ["Authorize ", pretty trustor, " ops with asset ", pretty ass]
    | setFlags == 0 && clearFlags == 1 = T.concat ["Clear authorization ", pretty trustor, " ops with asset ", pretty ass]
    | otherwise = T.concat ["Set trust line of ", pretty trustor, " to asset ", pretty ass, ": set flags ", T.show setFlags, ", clear flags ", T.show clearFlags]

instance Pretty ChangeTrustOp where
  pretty (ChangeTrustOp line limit) = T.concat ["Trust ", pretty line, " up to ", T.show limit]

instance Pretty PaymentOp where
  pretty (PaymentOp dest ass amount) = T.concat ["Pay ", prettyAmount amount, " ", pretty ass, " to ", pretty dest]

instance Pretty ManageDataOp where
  pretty (ManageDataOp name Nothing) = T.concat ["Data ", utf8s name, " cleared"]
  pretty (ManageDataOp name (Just value)) = T.concat ["Data ", utf8s name, " = ", utf8s value]

instance Pretty ManageBuyOfferOp where
  pretty (ManageBuyOfferOp sellass buyass buyamount price offerid) = T.concat ["Buy ", prettyAmount buyamount, " ", pretty buyass, " for ", pretty sellass, " price ", pretty price, " ", prettyAssetName sellass, "/", prettyAssetName buyass, if offerid == 0 then " (new offer)" else T.concat [" (update offer ", T.show offerid, ")"]]

instance Pretty ManageSellOfferOp where
  pretty (ManageSellOfferOp sellass buyass sellamount price offerid) = T.concat ["Sell ", prettyAmount sellamount, " ", pretty sellass, " for ", pretty buyass, " price ", pretty price, " ", prettyAssetName buyass, "/", prettyAssetName sellass, if offerid == 0 then " (new offer)" else T.concat [" (update offer ", T.show offerid, ")"]]

instance Pretty Price where
  pretty (Price numerator denominator) = T.show $ fromIntegral numerator / fromIntegral denominator

instance Pretty ClawbackOp where
  pretty (ClawbackOp ass acc amount) = T.concat ["Clawback ", prettyAmount amount, " ", pretty ass, " from ", pretty acc]

instance Pretty MuxedAccount where
  pretty (MuxedAccount'KEY_TYPE_ED25519 x) = prettyKey x
  pretty (MuxedAccount'KEY_TYPE_MUXED_ED25519 id key) = T.concat [b32 key, " (", T.show id, ")"]

instance Pretty PublicKey where
  pretty (PublicKey'PUBLIC_KEY_TYPE_ED25519 x) = prettyKey x

instance Pretty Claimant where
  pretty (Claimant'CLAIMANT_TYPE_V0 acc pred) = T.concat [pretty acc, " ", pretty pred]

instance Pretty Asset where
  pretty Asset'ASSET_TYPE_NATIVE = "XLM"
  pretty (Asset'ASSET_TYPE_CREDIT_ALPHANUM4 x) = pretty x
  pretty (Asset'ASSET_TYPE_CREDIT_ALPHANUM12 x) = pretty x

instance Pretty AlphaNum4 where
  pretty (AlphaNum4 code issuer) = T.concat [prettyAssetCode code, "-", pretty issuer]
instance Pretty AlphaNum12 where
  pretty (AlphaNum12 code issuer) = T.concat [prettyAssetCode code, "-", pretty issuer]

prettyAssetName Asset'ASSET_TYPE_NATIVE = "XLM"
prettyAssetName (Asset'ASSET_TYPE_CREDIT_ALPHANUM4 (AlphaNum4 code _)) = prettyAssetCode code
prettyAssetName (Asset'ASSET_TYPE_CREDIT_ALPHANUM12 (AlphaNum12 code _)) = prettyAssetCode code

instance Pretty a => Pretty (LengthArray b n a) where
  pretty x = pretty $ unLengthArray x

instance Pretty a => Pretty (V.Vector a) where
  pretty x = T.intercalate ", " $ map pretty $ V.toList x
