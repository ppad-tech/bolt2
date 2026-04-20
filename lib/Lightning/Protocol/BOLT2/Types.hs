{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Lightning.Protocol.BOLT2.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Core types for BOLT #2 peer protocol.
--
-- This module provides newtypes for identifiers, amounts, hashes, and
-- keys used in the Lightning Network peer protocol.

module Lightning.Protocol.BOLT2.Types (
  -- * Identifiers (re-exported from BOLT1)
    ChannelId(..)
  , channelId
  , unChannelId

  -- * Amounts (re-exported from BOLT1)
  , Satoshi(..)
  , MilliSatoshi(..)
  , satToMsat
  , msatToSat

  -- * Cryptographic types (re-exported from BOLT1)
  , Signature(..)
  , signature
  , unSignature
  , Point(..)
  , point
  , unPoint
  , PaymentHash(..)
  , paymentHash
  , unPaymentHash
  , PaymentPreimage(..)
  , paymentPreimage
  , unPaymentPreimage
  , PerCommitmentSecret(..)
  , perCommitmentSecret
  , unPerCommitmentSecret

  -- * Chain types (re-exported from BOLT1)
  , ChainHash(..)
  , chainHash
  , unChainHash
  , ShortChannelId(..)
  , shortChannelId
  , scidBlockHeight
  , scidTxIndex
  , scidOutputIndex

  -- * Transaction types
  , TxId(..)
  , mkTxId
  , OutPoint(..)
  , ScriptPubKey
  , scriptPubKey
  , unScriptPubKey

  -- * Protocol identifiers
  , HtlcId
  , htlcId
  , unHtlcId
  , SerialId
  , serialId
  , unSerialId

  -- * Quiescence
  , Initiator(..)

  -- * Protocol types
  , FeatureBits
  , featureBits
  , unFeatureBits
  , OnionPacket
  , onionPacket
  , unOnionPacket

  -- * Constants
  , channelIdLen
  , signatureLen
  , pointLen
  , chainHashLen
  , shortChannelIdLen
  , paymentHashLen
  , paymentPreimageLen
  , onionPacketLen
  , perCommitmentSecretLen
  ) where

import Bitcoin.Prim.Tx (TxId(..), mkTxId, OutPoint(..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim
  ( ChannelId(..), channelId, unChannelId
  , Satoshi(..), MilliSatoshi(..), satToMsat, msatToSat
  , Signature(..), signature, unSignature
  , Point(..), point, unPoint
  , PaymentHash(..), paymentHash, unPaymentHash
  , PaymentPreimage(..), paymentPreimage, unPaymentPreimage
  , PerCommitmentSecret(..), perCommitmentSecret
  , unPerCommitmentSecret
  , ChainHash(..), chainHash, unChainHash
  , ShortChannelId(..), shortChannelId
  , scidBlockHeight, scidTxIndex, scidOutputIndex
  )

-- constants -------------------------------------------------------------------

-- | Length of a channel_id in bytes (32).
channelIdLen :: Int
channelIdLen = 32
{-# INLINE channelIdLen #-}

-- | Length of a signature in bytes (64, compact format).
signatureLen :: Int
signatureLen = 64
{-# INLINE signatureLen #-}

-- | Length of a compressed secp256k1 public key in bytes (33).
pointLen :: Int
pointLen = 33
{-# INLINE pointLen #-}

-- | Length of a chain hash in bytes (32).
chainHashLen :: Int
chainHashLen = 32
{-# INLINE chainHashLen #-}

-- | Length of a short_channel_id in bytes (8).
shortChannelIdLen :: Int
shortChannelIdLen = 8
{-# INLINE shortChannelIdLen #-}

-- | Length of a payment hash in bytes (32).
paymentHashLen :: Int
paymentHashLen = 32
{-# INLINE paymentHashLen #-}

-- | Length of a payment preimage in bytes (32).
paymentPreimageLen :: Int
paymentPreimageLen = 32
{-# INLINE paymentPreimageLen #-}

-- | Length of an onion routing packet in bytes (1366).
onionPacketLen :: Int
onionPacketLen = 1366
{-# INLINE onionPacketLen #-}

-- | Length of a per-commitment secret in bytes (32).
perCommitmentSecretLen :: Int
perCommitmentSecretLen = 32
{-# INLINE perCommitmentSecretLen #-}

-- transaction types -----------------------------------------------------------

-- | A script pubkey (output script).
newtype ScriptPubKey = ScriptPubKey BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'ScriptPubKey' from a 'BS.ByteString'.
scriptPubKey :: BS.ByteString -> ScriptPubKey
scriptPubKey = ScriptPubKey
{-# INLINE scriptPubKey #-}

-- | Extract the underlying 'BS.ByteString' from a 'ScriptPubKey'.
unScriptPubKey :: ScriptPubKey -> BS.ByteString
unScriptPubKey (ScriptPubKey bs) = bs
{-# INLINE unScriptPubKey #-}

-- protocol identifiers -------------------------------------------------------

-- | An HTLC identifier, unique per channel per direction.
newtype HtlcId = HtlcId Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct an 'HtlcId' from a 'Word64'.
htlcId :: Word64 -> HtlcId
htlcId = HtlcId
{-# INLINE htlcId #-}

-- | Extract the underlying 'Word64' from an 'HtlcId'.
unHtlcId :: HtlcId -> Word64
unHtlcId (HtlcId w) = w
{-# INLINE unHtlcId #-}

-- | A serial identifier for interactive transaction construction.
newtype SerialId = SerialId Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'SerialId' from a 'Word64'.
serialId :: Word64 -> SerialId
serialId = SerialId
{-# INLINE serialId #-}

-- | Extract the underlying 'Word64' from a 'SerialId'.
unSerialId :: SerialId -> Word64
unSerialId (SerialId w) = w
{-# INLINE unSerialId #-}

-- quiescence -----------------------------------------------------------------

-- | Role in quiescence (STFU) protocol.
data Initiator
  = IsInitiator   -- ^ This node initiated quiescence.
  | NotInitiator  -- ^ This node did not initiate quiescence.
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Initiator

-- protocol types --------------------------------------------------------------

-- | Feature bits (variable length).
newtype FeatureBits = FeatureBits BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct 'FeatureBits' from a 'BS.ByteString'.
featureBits :: BS.ByteString -> FeatureBits
featureBits = FeatureBits
{-# INLINE featureBits #-}

-- | Extract the underlying 'BS.ByteString' from 'FeatureBits'.
unFeatureBits :: FeatureBits -> BS.ByteString
unFeatureBits (FeatureBits bs) = bs
{-# INLINE unFeatureBits #-}

-- | A 1366-byte onion routing packet.
newtype OnionPacket = OnionPacket BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct an 'OnionPacket' from a 1366-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 1366 bytes.
onionPacket :: BS.ByteString -> Maybe OnionPacket
onionPacket !bs
  | BS.length bs == onionPacketLen = Just $! OnionPacket bs
  | otherwise                      = Nothing
{-# INLINABLE onionPacket #-}

-- | Extract the underlying 'BS.ByteString' from an 'OnionPacket'.
unOnionPacket :: OnionPacket -> BS.ByteString
unOnionPacket (OnionPacket bs) = bs
{-# INLINE unOnionPacket #-}
