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
  -- * Identifiers
    ChannelId
  , channelId
  , unChannelId

  -- * Amounts
  , Satoshis(..)
  , MilliSatoshis(..)
  , satoshisToMsat
  , msatToSatoshis

  -- * Cryptographic types
  , Signature
  , signature
  , unSignature
  , Point
  , point
  , unPoint
  , PaymentHash
  , paymentHash
  , unPaymentHash
  , PaymentPreimage
  , paymentPreimage
  , unPaymentPreimage

  -- * Transaction types
  , TxId
  , txId
  , unTxId
  , Outpoint(..)
  , ScriptPubKey
  , scriptPubKey
  , unScriptPubKey

  -- * Chain types
  , ChainHash
  , chainHash
  , unChainHash
  , ShortChannelId(..)
  , shortChannelId
  , scidBlockHeight
  , scidTxIndex
  , scidOutputIndex

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
  , txIdLen
  , chainHashLen
  , shortChannelIdLen
  , paymentHashLen
  , paymentPreimageLen
  , onionPacketLen
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)

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

-- | Length of a transaction ID in bytes (32).
txIdLen :: Int
txIdLen = 32
{-# INLINE txIdLen #-}

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

-- identifiers -----------------------------------------------------------------

-- | A 32-byte channel identifier.
--
-- Derived from the funding transaction by XORing @funding_txid@ with
-- @funding_output_index@ (big-endian, altering the last 2 bytes).
--
-- For v2 channels, derived as @SHA256(lesser-revocation-basepoint ||
-- greater-revocation-basepoint)@.
newtype ChannelId = ChannelId BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'ChannelId' from a 32-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 32 bytes.
--
-- >>> channelId (BS.replicate 32 0x00)
-- Just (ChannelId ...)
-- >>> channelId (BS.replicate 31 0x00)
-- Nothing
channelId :: BS.ByteString -> Maybe ChannelId
channelId !bs
  | BS.length bs == channelIdLen = Just $! ChannelId bs
  | otherwise                    = Nothing
{-# INLINE channelId #-}

-- | Extract the underlying 'BS.ByteString' from a 'ChannelId'.
unChannelId :: ChannelId -> BS.ByteString
unChannelId (ChannelId bs) = bs
{-# INLINE unChannelId #-}

-- amounts ---------------------------------------------------------------------

-- | Amount in satoshis (1/100,000,000 of a bitcoin).
--
-- Stored as a 'Word64'. Maximum valid value is 21,000,000 * 100,000,000
-- = 2,100,000,000,000,000 satoshis.
newtype Satoshis = Satoshis { unSatoshis :: Word64 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Num, Enum, Real, Integral)

-- | Amount in millisatoshis (1/1000 of a satoshi).
--
-- Stored as a 'Word64'. Used for HTLC amounts and channel balances.
newtype MilliSatoshis = MilliSatoshis { unMilliSatoshis :: Word64 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Num, Enum, Real, Integral)

-- | Convert 'Satoshis' to 'MilliSatoshis'.
--
-- >>> satoshisToMsat (Satoshis 1)
-- MilliSatoshis 1000
satoshisToMsat :: Satoshis -> MilliSatoshis
satoshisToMsat (Satoshis !s) = MilliSatoshis $! s * 1000
{-# INLINE satoshisToMsat #-}

-- | Convert 'MilliSatoshis' to 'Satoshis', rounding down.
--
-- >>> msatToSatoshis (MilliSatoshis 1500)
-- Satoshis 1
msatToSatoshis :: MilliSatoshis -> Satoshis
msatToSatoshis (MilliSatoshis !m) = Satoshis $! m `div` 1000
{-# INLINE msatToSatoshis #-}

-- cryptographic types ---------------------------------------------------------

-- | A 64-byte compact ECDSA signature.
--
-- Used for commitment transaction signatures, HTLC signatures, and
-- closing transaction signatures.
newtype Signature = Signature BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'Signature' from a 64-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 64 bytes.
signature :: BS.ByteString -> Maybe Signature
signature !bs
  | BS.length bs == signatureLen = Just $! Signature bs
  | otherwise                    = Nothing
{-# INLINE signature #-}

-- | Extract the underlying 'BS.ByteString' from a 'Signature'.
unSignature :: Signature -> BS.ByteString
unSignature (Signature bs) = bs
{-# INLINE unSignature #-}

-- | A 33-byte compressed secp256k1 public key.
--
-- Used for funding pubkeys, basepoints, and per-commitment points.
newtype Point = Point BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'Point' from a 33-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 33 bytes.
--
-- Note: This only validates the length. Use secp256k1 libraries for
-- full point validation.
point :: BS.ByteString -> Maybe Point
point !bs
  | BS.length bs == pointLen = Just $! Point bs
  | otherwise                = Nothing
{-# INLINE point #-}

-- | Extract the underlying 'BS.ByteString' from a 'Point'.
unPoint :: Point -> BS.ByteString
unPoint (Point bs) = bs
{-# INLINE unPoint #-}

-- | A 32-byte SHA256 payment hash.
--
-- Used to identify HTLCs. The preimage that hashes to this value is
-- required to claim the HTLC.
newtype PaymentHash = PaymentHash BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'PaymentHash' from a 32-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 32 bytes.
paymentHash :: BS.ByteString -> Maybe PaymentHash
paymentHash !bs
  | BS.length bs == paymentHashLen = Just $! PaymentHash bs
  | otherwise                      = Nothing
{-# INLINE paymentHash #-}

-- | Extract the underlying 'BS.ByteString' from a 'PaymentHash'.
unPaymentHash :: PaymentHash -> BS.ByteString
unPaymentHash (PaymentHash bs) = bs
{-# INLINE unPaymentHash #-}

-- | A 32-byte payment preimage.
--
-- The SHA256 hash of this value produces the corresponding 'PaymentHash'.
-- Knowledge of the preimage allows claiming an HTLC.
newtype PaymentPreimage = PaymentPreimage BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'PaymentPreimage' from a 32-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 32 bytes.
paymentPreimage :: BS.ByteString -> Maybe PaymentPreimage
paymentPreimage !bs
  | BS.length bs == paymentPreimageLen = Just $! PaymentPreimage bs
  | otherwise                          = Nothing
{-# INLINE paymentPreimage #-}

-- | Extract the underlying 'BS.ByteString' from a 'PaymentPreimage'.
unPaymentPreimage :: PaymentPreimage -> BS.ByteString
unPaymentPreimage (PaymentPreimage bs) = bs
{-# INLINE unPaymentPreimage #-}

-- transaction types -----------------------------------------------------------

-- | A 32-byte transaction identifier.
--
-- The double-SHA256 hash of a serialized transaction.
newtype TxId = TxId BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'TxId' from a 32-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 32 bytes.
txId :: BS.ByteString -> Maybe TxId
txId !bs
  | BS.length bs == txIdLen = Just $! TxId bs
  | otherwise               = Nothing
{-# INLINE txId #-}

-- | Extract the underlying 'BS.ByteString' from a 'TxId'.
unTxId :: TxId -> BS.ByteString
unTxId (TxId bs) = bs
{-# INLINE unTxId #-}

-- | A transaction outpoint (txid + output index).
--
-- Identifies a specific output of a transaction.
data Outpoint = Outpoint
  { outpointTxId :: {-# UNPACK #-} !TxId
  , outpointVout :: {-# UNPACK #-} !Word32
  }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Outpoint

-- | A script pubkey (output script).
--
-- Variable length; used in shutdown messages, closing transactions, etc.
newtype ScriptPubKey = ScriptPubKey BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'ScriptPubKey' from a 'BS.ByteString'.
--
-- Accepts any length; validation of script structure is left to higher
-- layers.
scriptPubKey :: BS.ByteString -> ScriptPubKey
scriptPubKey = ScriptPubKey
{-# INLINE scriptPubKey #-}

-- | Extract the underlying 'BS.ByteString' from a 'ScriptPubKey'.
unScriptPubKey :: ScriptPubKey -> BS.ByteString
unScriptPubKey (ScriptPubKey bs) = bs
{-# INLINE unScriptPubKey #-}

-- chain types -----------------------------------------------------------------

-- | A 32-byte chain hash.
--
-- Identifies the blockchain (typically the genesis block hash).
-- Used in @open_channel@ to specify which chain the channel will reside on.
newtype ChainHash = ChainHash BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct a 'ChainHash' from a 32-byte 'BS.ByteString'.
--
-- Returns 'Nothing' if the input is not exactly 32 bytes.
chainHash :: BS.ByteString -> Maybe ChainHash
chainHash !bs
  | BS.length bs == chainHashLen = Just $! ChainHash bs
  | otherwise                    = Nothing
{-# INLINE chainHash #-}

-- | Extract the underlying 'BS.ByteString' from a 'ChainHash'.
unChainHash :: ChainHash -> BS.ByteString
unChainHash (ChainHash bs) = bs
{-# INLINE unChainHash #-}

-- | A short channel identifier (8 bytes).
--
-- Encodes the block height (3 bytes), transaction index (3 bytes), and
-- output index (2 bytes) of the funding transaction output.
--
-- This is a compact representation for referencing channels in gossip
-- and routing.
data ShortChannelId = ShortChannelId
  { scidBytes :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData ShortChannelId

-- | Construct a 'ShortChannelId' from block height, tx index, and
-- output index.
--
-- Returns 'Nothing' if any component exceeds its maximum value:
--
-- * block height: max 16,777,215 (2^24 - 1)
-- * tx index: max 16,777,215 (2^24 - 1)
-- * output index: max 65,535 (2^16 - 1)
--
-- >>> shortChannelId 800000 1234 0
-- Just (ShortChannelId ...)
shortChannelId
  :: Word32  -- ^ Block height (24 bits max)
  -> Word32  -- ^ Transaction index (24 bits max)
  -> Word16  -- ^ Output index
  -> Maybe ShortChannelId
shortChannelId !blockHeight !txIndex !outputIndex
  | blockHeight > 0xFFFFFF = Nothing
  | txIndex > 0xFFFFFF     = Nothing
  | otherwise              = Just $! ShortChannelId scid
  where
    !scid = (fromIntegral blockHeight `unsafeShiftL` 40)
        .|. (fromIntegral txIndex `unsafeShiftL` 16)
        .|. fromIntegral outputIndex
{-# INLINE shortChannelId #-}

-- | Extract the block height from a 'ShortChannelId'.
scidBlockHeight :: ShortChannelId -> Word32
scidBlockHeight (ShortChannelId !w) =
  fromIntegral $! (w `unsafeShiftR` 40) .&. 0xFFFFFF
{-# INLINE scidBlockHeight #-}

-- | Extract the transaction index from a 'ShortChannelId'.
scidTxIndex :: ShortChannelId -> Word32
scidTxIndex (ShortChannelId !w) =
  fromIntegral $! (w `unsafeShiftR` 16) .&. 0xFFFFFF
{-# INLINE scidTxIndex #-}

-- | Extract the output index from a 'ShortChannelId'.
scidOutputIndex :: ShortChannelId -> Word16
scidOutputIndex (ShortChannelId !w) = fromIntegral $! w .&. 0xFFFF
{-# INLINE scidOutputIndex #-}

-- protocol types --------------------------------------------------------------

-- | Feature bits (variable length).
--
-- Encodes supported/required features. Even bits indicate required
-- features; odd bits indicate optional features.
newtype FeatureBits = FeatureBits BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype NFData

-- | Construct 'FeatureBits' from a 'BS.ByteString'.
--
-- Accepts any length; feature bit parsing is left to higher layers.
featureBits :: BS.ByteString -> FeatureBits
featureBits = FeatureBits
{-# INLINE featureBits #-}

-- | Extract the underlying 'BS.ByteString' from 'FeatureBits'.
unFeatureBits :: FeatureBits -> BS.ByteString
unFeatureBits (FeatureBits bs) = bs
{-# INLINE unFeatureBits #-}

-- | A 1366-byte onion routing packet.
--
-- Contains encrypted routing information for HTLC forwarding, as
-- specified in BOLT #4.
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
{-# INLINE onionPacket #-}

-- | Extract the underlying 'BS.ByteString' from an 'OnionPacket'.
unOnionPacket :: OnionPacket -> BS.ByteString
unOnionPacket (OnionPacket bs) = bs
{-# INLINE unOnionPacket #-}
