{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Lightning.Protocol.BOLT2.Codec
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Encode/decode functions for V1 channel establishment and close messages.

module Lightning.Protocol.BOLT2.Codec (
  -- * Error types
    EncodeError(..)
  , DecodeError(..)

  -- * Channel establishment v1
  , encodeOpenChannel
  , decodeOpenChannel
  , encodeAcceptChannel
  , decodeAcceptChannel
  , encodeFundingCreated
  , decodeFundingCreated
  , encodeFundingSigned
  , decodeFundingSigned
  , encodeChannelReady
  , decodeChannelReady

  -- * Channel close
  , encodeStfu
  , decodeStfu
  , encodeShutdown
  , decodeShutdown
  , encodeClosingSigned
  , decodeClosingSigned
  , encodeClosingComplete
  , decodeClosingComplete
  , encodeClosingSig
  , decodeClosingSig
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1
  ( TlvStream(..)
  , TlvError
  , encodeU16
  , encodeU32
  , encodeU64
  , decodeU16
  , decodeU32
  , decodeU64
  , encodeTlvStream
  , decodeTlvStreamRaw
  )
import Lightning.Protocol.BOLT2.Types
import Lightning.Protocol.BOLT2.Messages

-- Error types -----------------------------------------------------------------

-- | Encoding errors.
data EncodeError
  = EncodeLengthOverflow  -- ^ Payload exceeds u16 max (65535 bytes)
  deriving stock (Eq, Show, Generic)

instance NFData EncodeError

-- | Decoding errors.
data DecodeError
  = DecodeInsufficientBytes
  | DecodeInvalidLength
  | DecodeInvalidChannelId
  | DecodeInvalidChainHash
  | DecodeInvalidSignature
  | DecodeInvalidPoint
  | DecodeInvalidTxId
  | DecodeTlvError !TlvError
  deriving stock (Eq, Show, Generic)

instance NFData DecodeError

-- Helpers ---------------------------------------------------------------------

-- | Decode a single byte.
decodeU8 :: BS.ByteString -> Maybe (Word8, BS.ByteString)
decodeU8 !bs
  | BS.null bs = Nothing
  | otherwise  = Just (BS.index bs 0, BS.drop 1 bs)
{-# INLINE decodeU8 #-}

-- | Decode fixed-size bytes.
decodeBytes :: Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
decodeBytes !n !bs
  | BS.length bs < n = Nothing
  | otherwise        = Just (BS.take n bs, BS.drop n bs)
{-# INLINE decodeBytes #-}

-- | Decode a ChannelId (32 bytes).
decodeChannelIdBytes
  :: BS.ByteString -> Either DecodeError (ChannelId, BS.ByteString)
decodeChannelIdBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes channelIdLen bs)
  cid <- maybe (Left DecodeInvalidChannelId) Right (channelId raw)
  Right (cid, rest)
{-# INLINE decodeChannelIdBytes #-}

-- | Decode a ChainHash (32 bytes).
decodeChainHashBytes
  :: BS.ByteString -> Either DecodeError (ChainHash, BS.ByteString)
decodeChainHashBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes chainHashLen bs)
  ch <- maybe (Left DecodeInvalidChainHash) Right (chainHash raw)
  Right (ch, rest)
{-# INLINE decodeChainHashBytes #-}

-- | Decode a Signature (64 bytes).
decodeSignatureBytes
  :: BS.ByteString -> Either DecodeError (Signature, BS.ByteString)
decodeSignatureBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes signatureLen bs)
  sig <- maybe (Left DecodeInvalidSignature) Right (signature raw)
  Right (sig, rest)
{-# INLINE decodeSignatureBytes #-}

-- | Decode a Point (33 bytes).
decodePointBytes
  :: BS.ByteString -> Either DecodeError (Point, BS.ByteString)
decodePointBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes pointLen bs)
  pt <- maybe (Left DecodeInvalidPoint) Right (point raw)
  Right (pt, rest)
{-# INLINE decodePointBytes #-}

-- | Decode a TxId (32 bytes).
decodeTxIdBytes
  :: BS.ByteString -> Either DecodeError (TxId, BS.ByteString)
decodeTxIdBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes txIdLen bs)
  tid <- maybe (Left DecodeInvalidTxId) Right (txId raw)
  Right (tid, rest)
{-# INLINE decodeTxIdBytes #-}

-- | Decode a u16 with error handling.
decodeU16E :: BS.ByteString -> Either DecodeError (Word16, BS.ByteString)
decodeU16E !bs = maybe (Left DecodeInsufficientBytes) Right (decodeU16 bs)
{-# INLINE decodeU16E #-}

-- | Decode a u32 with error handling.
decodeU32E :: BS.ByteString -> Either DecodeError (Word32, BS.ByteString)
decodeU32E !bs = maybe (Left DecodeInsufficientBytes) Right (decodeU32 bs)
{-# INLINE decodeU32E #-}

-- | Decode a u64 as Satoshis.
decodeSatoshis
  :: BS.ByteString -> Either DecodeError (Satoshis, BS.ByteString)
decodeSatoshis !bs = do
  (val, rest) <- maybe (Left DecodeInsufficientBytes) Right (decodeU64 bs)
  Right (Satoshis val, rest)
{-# INLINE decodeSatoshis #-}

-- | Decode a u64 as MilliSatoshis.
decodeMilliSatoshis
  :: BS.ByteString -> Either DecodeError (MilliSatoshis, BS.ByteString)
decodeMilliSatoshis !bs = do
  (val, rest) <- maybe (Left DecodeInsufficientBytes) Right (decodeU64 bs)
  Right (MilliSatoshis val, rest)
{-# INLINE decodeMilliSatoshis #-}

-- | Decode optional TLV stream from remaining bytes.
decodeTlvs :: BS.ByteString -> Either DecodeError TlvStream
decodeTlvs !bs
  | BS.null bs = Right (TlvStream [])
  | otherwise  = either (Left . DecodeTlvError) Right (decodeTlvStreamRaw bs)
{-# INLINE decodeTlvs #-}

-- | Decode a length-prefixed script (u16 length prefix).
decodeScriptPubKey
  :: BS.ByteString -> Either DecodeError (ScriptPubKey, BS.ByteString)
decodeScriptPubKey !bs = do
  (len, rest1) <- decodeU16E bs
  let !scriptLen = fromIntegral len
  unless (BS.length rest1 >= scriptLen) $ Left DecodeInsufficientBytes
  let !script = BS.take scriptLen rest1
      !rest2 = BS.drop scriptLen rest1
  Right (scriptPubKey script, rest2)
{-# INLINE decodeScriptPubKey #-}

-- Channel establishment v1 ----------------------------------------------------

-- | Encode an OpenChannel message (type 32).
--
-- Wire format:
-- - chain_hash: 32 bytes
-- - temporary_channel_id: 32 bytes
-- - funding_satoshis: u64
-- - push_msat: u64
-- - dust_limit_satoshis: u64
-- - max_htlc_value_in_flight_msat: u64
-- - channel_reserve_satoshis: u64
-- - htlc_minimum_msat: u64
-- - feerate_per_kw: u32
-- - to_self_delay: u16
-- - max_accepted_htlcs: u16
-- - funding_pubkey: 33 bytes
-- - revocation_basepoint: 33 bytes
-- - payment_basepoint: 33 bytes
-- - delayed_payment_basepoint: 33 bytes
-- - htlc_basepoint: 33 bytes
-- - first_per_commitment_point: 33 bytes
-- - channel_flags: 1 byte
-- - tlvs: TLV stream
encodeOpenChannel :: OpenChannel -> BS.ByteString
encodeOpenChannel !msg = mconcat
  [ unChainHash (openChannelChainHash msg)
  , unChannelId (openChannelTempChannelId msg)
  , encodeU64 (unSatoshis (openChannelFundingSatoshis msg))
  , encodeU64 (unMilliSatoshis (openChannelPushMsat msg))
  , encodeU64 (unSatoshis (openChannelDustLimitSatoshis msg))
  , encodeU64 (unMilliSatoshis (openChannelMaxHtlcValueInFlight msg))
  , encodeU64 (unSatoshis (openChannelChannelReserveSat msg))
  , encodeU64 (unMilliSatoshis (openChannelHtlcMinimumMsat msg))
  , encodeU32 (openChannelFeeratePerKw msg)
  , encodeU16 (openChannelToSelfDelay msg)
  , encodeU16 (openChannelMaxAcceptedHtlcs msg)
  , unPoint (openChannelFundingPubkey msg)
  , unPoint (openChannelRevocationBasepoint msg)
  , unPoint (openChannelPaymentBasepoint msg)
  , unPoint (openChannelDelayedPaymentBase msg)
  , unPoint (openChannelHtlcBasepoint msg)
  , unPoint (openChannelFirstPerCommitPoint msg)
  , BS.singleton (openChannelChannelFlags msg)
  , encodeTlvStream (openChannelTlvs msg)
  ]

-- | Decode an OpenChannel message (type 32).
decodeOpenChannel
  :: BS.ByteString -> Either DecodeError (OpenChannel, BS.ByteString)
decodeOpenChannel !bs = do
  (chainHash', rest1) <- decodeChainHashBytes bs
  (tempChanId, rest2) <- decodeChannelIdBytes rest1
  (fundingSats, rest3) <- decodeSatoshis rest2
  (pushMsat, rest4) <- decodeMilliSatoshis rest3
  (dustLimit, rest5) <- decodeSatoshis rest4
  (maxHtlcVal, rest6) <- decodeMilliSatoshis rest5
  (chanReserve, rest7) <- decodeSatoshis rest6
  (htlcMin, rest8) <- decodeMilliSatoshis rest7
  (feerate, rest9) <- decodeU32E rest8
  (toSelfDelay, rest10) <- decodeU16E rest9
  (maxHtlcs, rest11) <- decodeU16E rest10
  (fundingPk, rest12) <- decodePointBytes rest11
  (revocBase, rest13) <- decodePointBytes rest12
  (paymentBase, rest14) <- decodePointBytes rest13
  (delayedBase, rest15) <- decodePointBytes rest14
  (htlcBase, rest16) <- decodePointBytes rest15
  (firstCommit, rest17) <- decodePointBytes rest16
  (flags, rest18) <- maybe (Left DecodeInsufficientBytes) Right
                       (decodeU8 rest17)
  tlvs <- decodeTlvs rest18
  let !msg = OpenChannel
        { openChannelChainHash            = chainHash'
        , openChannelTempChannelId        = tempChanId
        , openChannelFundingSatoshis      = fundingSats
        , openChannelPushMsat             = pushMsat
        , openChannelDustLimitSatoshis    = dustLimit
        , openChannelMaxHtlcValueInFlight = maxHtlcVal
        , openChannelChannelReserveSat    = chanReserve
        , openChannelHtlcMinimumMsat      = htlcMin
        , openChannelFeeratePerKw         = feerate
        , openChannelToSelfDelay          = toSelfDelay
        , openChannelMaxAcceptedHtlcs     = maxHtlcs
        , openChannelFundingPubkey        = fundingPk
        , openChannelRevocationBasepoint  = revocBase
        , openChannelPaymentBasepoint     = paymentBase
        , openChannelDelayedPaymentBase   = delayedBase
        , openChannelHtlcBasepoint        = htlcBase
        , openChannelFirstPerCommitPoint  = firstCommit
        , openChannelChannelFlags         = flags
        , openChannelTlvs                 = tlvs
        }
  Right (msg, BS.empty)

-- | Encode an AcceptChannel message (type 33).
--
-- Wire format:
-- - temporary_channel_id: 32 bytes
-- - dust_limit_satoshis: u64
-- - max_htlc_value_in_flight_msat: u64
-- - channel_reserve_satoshis: u64
-- - htlc_minimum_msat: u64
-- - minimum_depth: u32
-- - to_self_delay: u16
-- - max_accepted_htlcs: u16
-- - funding_pubkey: 33 bytes
-- - revocation_basepoint: 33 bytes
-- - payment_basepoint: 33 bytes
-- - delayed_payment_basepoint: 33 bytes
-- - htlc_basepoint: 33 bytes
-- - first_per_commitment_point: 33 bytes
-- - tlvs: TLV stream
encodeAcceptChannel :: AcceptChannel -> BS.ByteString
encodeAcceptChannel !msg = mconcat
  [ unChannelId (acceptChannelTempChannelId msg)
  , encodeU64 (unSatoshis (acceptChannelDustLimitSatoshis msg))
  , encodeU64 (unMilliSatoshis (acceptChannelMaxHtlcValueInFlight msg))
  , encodeU64 (unSatoshis (acceptChannelChannelReserveSat msg))
  , encodeU64 (unMilliSatoshis (acceptChannelHtlcMinimumMsat msg))
  , encodeU32 (acceptChannelMinimumDepth msg)
  , encodeU16 (acceptChannelToSelfDelay msg)
  , encodeU16 (acceptChannelMaxAcceptedHtlcs msg)
  , unPoint (acceptChannelFundingPubkey msg)
  , unPoint (acceptChannelRevocationBasepoint msg)
  , unPoint (acceptChannelPaymentBasepoint msg)
  , unPoint (acceptChannelDelayedPaymentBase msg)
  , unPoint (acceptChannelHtlcBasepoint msg)
  , unPoint (acceptChannelFirstPerCommitPoint msg)
  , encodeTlvStream (acceptChannelTlvs msg)
  ]

-- | Decode an AcceptChannel message (type 33).
decodeAcceptChannel
  :: BS.ByteString -> Either DecodeError (AcceptChannel, BS.ByteString)
decodeAcceptChannel !bs = do
  (tempChanId, rest1) <- decodeChannelIdBytes bs
  (dustLimit, rest2) <- decodeSatoshis rest1
  (maxHtlcVal, rest3) <- decodeMilliSatoshis rest2
  (chanReserve, rest4) <- decodeSatoshis rest3
  (htlcMin, rest5) <- decodeMilliSatoshis rest4
  (minDepth, rest6) <- decodeU32E rest5
  (toSelfDelay, rest7) <- decodeU16E rest6
  (maxHtlcs, rest8) <- decodeU16E rest7
  (fundingPk, rest9) <- decodePointBytes rest8
  (revocBase, rest10) <- decodePointBytes rest9
  (paymentBase, rest11) <- decodePointBytes rest10
  (delayedBase, rest12) <- decodePointBytes rest11
  (htlcBase, rest13) <- decodePointBytes rest12
  (firstCommit, rest14) <- decodePointBytes rest13
  tlvs <- decodeTlvs rest14
  let !msg = AcceptChannel
        { acceptChannelTempChannelId        = tempChanId
        , acceptChannelDustLimitSatoshis    = dustLimit
        , acceptChannelMaxHtlcValueInFlight = maxHtlcVal
        , acceptChannelChannelReserveSat    = chanReserve
        , acceptChannelHtlcMinimumMsat      = htlcMin
        , acceptChannelMinimumDepth         = minDepth
        , acceptChannelToSelfDelay          = toSelfDelay
        , acceptChannelMaxAcceptedHtlcs     = maxHtlcs
        , acceptChannelFundingPubkey        = fundingPk
        , acceptChannelRevocationBasepoint  = revocBase
        , acceptChannelPaymentBasepoint     = paymentBase
        , acceptChannelDelayedPaymentBase   = delayedBase
        , acceptChannelHtlcBasepoint        = htlcBase
        , acceptChannelFirstPerCommitPoint  = firstCommit
        , acceptChannelTlvs                 = tlvs
        }
  Right (msg, BS.empty)

-- | Encode a FundingCreated message (type 34).
--
-- Wire format:
-- - temporary_channel_id: 32 bytes
-- - funding_txid: 32 bytes
-- - funding_output_index: u16
-- - signature: 64 bytes
encodeFundingCreated :: FundingCreated -> BS.ByteString
encodeFundingCreated !msg = mconcat
  [ unChannelId (fundingCreatedTempChannelId msg)
  , unTxId (fundingCreatedFundingTxid msg)
  , encodeU16 (fundingCreatedFundingOutIdx msg)
  , unSignature (fundingCreatedSignature msg)
  ]

-- | Decode a FundingCreated message (type 34).
decodeFundingCreated
  :: BS.ByteString -> Either DecodeError (FundingCreated, BS.ByteString)
decodeFundingCreated !bs = do
  (tempChanId, rest1) <- decodeChannelIdBytes bs
  (fundingTxid, rest2) <- decodeTxIdBytes rest1
  (outIdx, rest3) <- decodeU16E rest2
  (sig, rest4) <- decodeSignatureBytes rest3
  let !msg = FundingCreated
        { fundingCreatedTempChannelId = tempChanId
        , fundingCreatedFundingTxid   = fundingTxid
        , fundingCreatedFundingOutIdx = outIdx
        , fundingCreatedSignature     = sig
        }
  Right (msg, rest4)

-- | Encode a FundingSigned message (type 35).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - signature: 64 bytes
encodeFundingSigned :: FundingSigned -> BS.ByteString
encodeFundingSigned !msg = mconcat
  [ unChannelId (fundingSignedChannelId msg)
  , unSignature (fundingSignedSignature msg)
  ]

-- | Decode a FundingSigned message (type 35).
decodeFundingSigned
  :: BS.ByteString -> Either DecodeError (FundingSigned, BS.ByteString)
decodeFundingSigned !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (sig, rest2) <- decodeSignatureBytes rest1
  let !msg = FundingSigned
        { fundingSignedChannelId = chanId
        , fundingSignedSignature = sig
        }
  Right (msg, rest2)

-- | Encode a ChannelReady message (type 36).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - second_per_commitment_point: 33 bytes
-- - tlvs: TLV stream
encodeChannelReady :: ChannelReady -> BS.ByteString
encodeChannelReady !msg = mconcat
  [ unChannelId (channelReadyChannelId msg)
  , unPoint (channelReadySecondPerCommitPoint msg)
  , encodeTlvStream (channelReadyTlvs msg)
  ]

-- | Decode a ChannelReady message (type 36).
decodeChannelReady
  :: BS.ByteString -> Either DecodeError (ChannelReady, BS.ByteString)
decodeChannelReady !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (secondCommit, rest2) <- decodePointBytes rest1
  tlvs <- decodeTlvs rest2
  let !msg = ChannelReady
        { channelReadyChannelId            = chanId
        , channelReadySecondPerCommitPoint = secondCommit
        , channelReadyTlvs                 = tlvs
        }
  Right (msg, BS.empty)

-- Channel close ---------------------------------------------------------------

-- | Encode a Stfu message (type 2).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - initiator: 1 byte
encodeStfu :: Stfu -> BS.ByteString
encodeStfu !msg = mconcat
  [ unChannelId (stfuChannelId msg)
  , BS.singleton (stfuInitiator msg)
  ]

-- | Decode a Stfu message (type 2).
decodeStfu :: BS.ByteString -> Either DecodeError (Stfu, BS.ByteString)
decodeStfu !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (initiator, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                          (decodeU8 rest1)
  let !msg = Stfu
        { stfuChannelId = chanId
        , stfuInitiator = initiator
        }
  Right (msg, rest2)

-- | Encode a Shutdown message (type 38).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - len: u16
-- - scriptpubkey: len bytes
encodeShutdown :: Shutdown -> Either EncodeError BS.ByteString
encodeShutdown !msg = do
  let !script = unScriptPubKey (shutdownScriptPubkey msg)
      !scriptLen = BS.length script
  if scriptLen > 65535
    then Left EncodeLengthOverflow
    else Right $ mconcat
           [ unChannelId (shutdownChannelId msg)
           , encodeU16 (fromIntegral scriptLen)
           , script
           ]

-- | Decode a Shutdown message (type 38).
decodeShutdown
  :: BS.ByteString -> Either DecodeError (Shutdown, BS.ByteString)
decodeShutdown !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (script, rest2) <- decodeScriptPubKey rest1
  let !msg = Shutdown
        { shutdownChannelId    = chanId
        , shutdownScriptPubkey = script
        }
  Right (msg, rest2)

-- | Encode a ClosingSigned message (type 39).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - fee_satoshis: u64
-- - signature: 64 bytes
-- - tlvs: TLV stream
encodeClosingSigned :: ClosingSigned -> BS.ByteString
encodeClosingSigned !msg = mconcat
  [ unChannelId (closingSignedChannelId msg)
  , encodeU64 (unSatoshis (closingSignedFeeSatoshis msg))
  , unSignature (closingSignedSignature msg)
  , encodeTlvStream (closingSignedTlvs msg)
  ]

-- | Decode a ClosingSigned message (type 39).
decodeClosingSigned
  :: BS.ByteString -> Either DecodeError (ClosingSigned, BS.ByteString)
decodeClosingSigned !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (feeSats, rest2) <- decodeSatoshis rest1
  (sig, rest3) <- decodeSignatureBytes rest2
  tlvs <- decodeTlvs rest3
  let !msg = ClosingSigned
        { closingSignedChannelId   = chanId
        , closingSignedFeeSatoshis = feeSats
        , closingSignedSignature   = sig
        , closingSignedTlvs        = tlvs
        }
  Right (msg, BS.empty)

-- | Encode a ClosingComplete message (type 40).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - len: u16 (closer script length)
-- - closer_script: len bytes
-- - len: u16 (closee script length)
-- - closee_script: len bytes
-- - fee_satoshis: u64
-- - locktime: u32
-- - tlvs: TLV stream
encodeClosingComplete :: ClosingComplete -> Either EncodeError BS.ByteString
encodeClosingComplete !msg = do
  let !closerScript = unScriptPubKey (closingCompleteCloserScript msg)
      !closeeScript = unScriptPubKey (closingCompleteCloseeScript msg)
      !closerLen = BS.length closerScript
      !closeeLen = BS.length closeeScript
  if closerLen > 65535 || closeeLen > 65535
    then Left EncodeLengthOverflow
    else Right $ mconcat
           [ unChannelId (closingCompleteChannelId msg)
           , encodeU16 (fromIntegral closerLen)
           , closerScript
           , encodeU16 (fromIntegral closeeLen)
           , closeeScript
           , encodeU64 (unSatoshis (closingCompleteFeeSatoshis msg))
           , encodeU32 (closingCompleteLocktime msg)
           , encodeTlvStream (closingCompleteTlvs msg)
           ]

-- | Decode a ClosingComplete message (type 40).
decodeClosingComplete
  :: BS.ByteString -> Either DecodeError (ClosingComplete, BS.ByteString)
decodeClosingComplete !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (closerScript, rest2) <- decodeScriptPubKey rest1
  (closeeScript, rest3) <- decodeScriptPubKey rest2
  (feeSats, rest4) <- decodeSatoshis rest3
  (locktime, rest5) <- decodeU32E rest4
  tlvs <- decodeTlvs rest5
  let !msg = ClosingComplete
        { closingCompleteChannelId    = chanId
        , closingCompleteCloserScript = closerScript
        , closingCompleteCloseeScript = closeeScript
        , closingCompleteFeeSatoshis  = feeSats
        , closingCompleteLocktime     = locktime
        , closingCompleteTlvs         = tlvs
        }
  Right (msg, BS.empty)

-- | Encode a ClosingSig message (type 41).
--
-- Wire format:
-- - channel_id: 32 bytes
-- - len: u16 (closer script length)
-- - closer_script: len bytes
-- - len: u16 (closee script length)
-- - closee_script: len bytes
-- - fee_satoshis: u64
-- - locktime: u32
-- - tlvs: TLV stream
encodeClosingSig :: ClosingSig -> Either EncodeError BS.ByteString
encodeClosingSig !msg = do
  let !closerScript = unScriptPubKey (closingSigCloserScript msg)
      !closeeScript = unScriptPubKey (closingSigCloseeScript msg)
      !closerLen = BS.length closerScript
      !closeeLen = BS.length closeeScript
  if closerLen > 65535 || closeeLen > 65535
    then Left EncodeLengthOverflow
    else Right $ mconcat
           [ unChannelId (closingSigChannelId msg)
           , encodeU16 (fromIntegral closerLen)
           , closerScript
           , encodeU16 (fromIntegral closeeLen)
           , closeeScript
           , encodeU64 (unSatoshis (closingSigFeeSatoshis msg))
           , encodeU32 (closingSigLocktime msg)
           , encodeTlvStream (closingSigTlvs msg)
           ]

-- | Decode a ClosingSig message (type 41).
decodeClosingSig
  :: BS.ByteString -> Either DecodeError (ClosingSig, BS.ByteString)
decodeClosingSig !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (closerScript, rest2) <- decodeScriptPubKey rest1
  (closeeScript, rest3) <- decodeScriptPubKey rest2
  (feeSats, rest4) <- decodeSatoshis rest3
  (locktime, rest5) <- decodeU32E rest4
  tlvs <- decodeTlvs rest5
  let !msg = ClosingSig
        { closingSigChannelId    = chanId
        , closingSigCloserScript = closerScript
        , closingSigCloseeScript = closeeScript
        , closingSigFeeSatoshis  = feeSats
        , closingSigLocktime     = locktime
        , closingSigTlvs         = tlvs
        }
  Right (msg, BS.empty)
