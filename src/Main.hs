{-# LANGUAGE OverloadedStrings, Strict #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.ONCRPC.XDR.Serial
import Network.Stellar.TransactionXdr
import qualified Stellar.Simple as S
import System.Directory
import System.Environment
import System.IO

import Encryption
import Pretty

main = do
  args <- getArgs
  case args of
    ["encrypt"] -> encrypt
    _ -> sign

encrypt = do
  let key = catch getPrivateKey
        (\e -> do let err = show (e :: IOException)
                  hPutStr stderr ("Warning: Couldn't get key from file (" ++ err ++ "), reading from stdin\n")
                  T.getContents)
  encryptedFile <- T.encodeUtf8 <$> key
  path <- getPrivateKeyPath
  B.writeFile path =<< encryptData encryptedFile

sign = do
  transactionB64 <- B.getContents
  let transaction =  case B64.decode $ BC8.strip transactionB64 of
        Left err -> error err
        Right x -> x
  let parsedTransaction = case xdrDeserialize transaction :: Either String TransactionEnvelope of
        Left err -> error err
        Right x -> x
  T.hPutStrLn stderr $ pretty parsedTransaction
  proceed <- confirm
  when proceed $ do
    key <- getPrivateKey
    let sd = S.signWithSecret key parsedTransaction
    T.putStrLn $ S.xdrSerializeBase64T sd

getPrivateKeyPath :: IO FilePath
getPrivateKeyPath = do
  home <- getHomeDirectory
  pure $ home ++ "/.stellar-veritas-key"

getPrivateKey = do
  file <- B.readFile =<< getPrivateKeyPath
  case B.head file of
    0x53 -> pure $ T.strip $ T.decodeUtf8 file	-- bare unencrypted secret key
    0x45 -> T.decodeUtf8 <$> decryptData (B.tail file)
    _ -> error "garbled private key file"

confirm :: IO Bool
confirm = do
  isTerm <- hIsTerminalDevice stdout
  if isTerm then do
    tty <- openFile "/dev/tty" ReadWriteMode
    hPutStr tty "Sign this transaction? [y/N]: "
    hFlush tty
    response <- hGetLine tty
    hClose tty
    pure $ response `elem` ["y", "Y"]
  else pure True
