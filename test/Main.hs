{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Lightning.Protocol.BOLT2
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt2" [
  ]

-- Helpers ---------------------------------------------------------------------

-- | Decode hex string. Fails the test on invalid hex.
unhex :: BS.ByteString -> BS.ByteString
unhex bs = case B16.decode bs of
  Just r  -> r
  Nothing -> error $ "invalid hex: " ++ show bs
