{-# LANGUAGE OverloadedStrings, Strict #-}

module Main where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.ONCRPC.XDR.Serial
import Network.Stellar.TransactionXdr
import qualified Stellar.Simple as S
import System.Directory
import System.IO

import Pretty

main = do
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

getPrivateKey = do
  home <- getHomeDirectory
  keyFile <- T.readFile $ home ++ "/.stellar-veritas-key"
  pure $ T.strip keyFile

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
