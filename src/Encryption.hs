{-# LANGUAGE OverloadedStrings #-}

module Encryption where

import Control.Monad
import Crypto.Error
import Crypto.KDF.Argon2
import Crypto.Random.Entropy
import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putWord32be, runPut)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)
import System.CPUTime
import System.IO

-- picoseconds
measureIteration = do
  start <- getCPUTime
  throwaway <- throwCryptoErrorIO $ hash defaultOptions ("666" :: ByteString) ("solasgsdgdsgsd" :: ByteString) 32 :: IO ByteString
  end <- seq throwaway getCPUTime
  pure $ fromIntegral (end - start) / 1e12

getPassword = do
  tty <- openFile "/dev/tty" ReadWriteMode
  BC8.hPutStr tty "Password: "
  hFlush tty
  old <- hGetEcho tty
  hSetEcho tty False
  response <- BC8.hGetLine tty
  hSetEcho tty old
  BC8.hPutStrLn tty ""
  hClose tty
  pure response

xorWithKey :: ByteString -> ByteString -> ByteString
xorWithKey key bs
  | B.null key = error "key must not be empty"
  | otherwise  = B.pack $ B.zipWith xor bs key

-- format: E[16-byte salt][4-byte big-endian iterations][ciphertext]
encryptData :: ByteString -> IO ByteString
encryptData privkey = do
  iterationTime <- measureIteration
  print iterationTime
  let iterations = ceiling $ 1 / iterationTime
  print iterations
  salt <- getEntropy 16 :: IO ByteString
  --let password = "password" :: ByteString
  password <- getPassword
  key <- throwCryptoErrorIO $ hash defaultOptions { iterations = iterations } password salt 56 :: IO ByteString
  let ciphertext = xorWithKey key privkey
  -- encode iterations as Word32 big-endian
  let iterWord :: Word32
      iterWord = fromIntegral iterations
      iterBytes = BL.toStrict $ runPut (putWord32be iterWord)
  pure $ B.concat ["E", salt, iterBytes, ciphertext]

decryptData :: ByteString -> IO ByteString
decryptData contents = do
  password <- getPassword
  -- parse salt (16 bytes), iterations (4 bytes BE), then ciphertext
  when (B.length contents < 76) $ error "encrypted key is unexpectedly short"
  let (salt, rest) = B.splitAt 16 contents
      (iterBytes, ciphertext) = B.splitAt 4 rest
      iterations = runGet getWord32be (BL.fromStrict iterBytes)
  key <- throwCryptoErrorIO $ hash defaultOptions { iterations = iterations } password salt 56 :: IO ByteString
  let plaintext = xorWithKey key ciphertext
  case B.head plaintext of
    0x53 -> pure plaintext
    _ -> error "wrong password"
