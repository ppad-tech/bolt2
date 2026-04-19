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
-- Encode/decode functions for BOLT #2 messages.

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

  -- * Channel establishment v2 (interactive-tx)
  , encodeOpenChannel2
  , decodeOpenChannel2
  , encodeAcceptChannel2
  , decodeAcceptChannel2
  , encodeTxAddInput
  , decodeTxAddInput
  , encodeTxAddOutput
  , decodeTxAddOutput
  , encodeTxRemoveInput
  , decodeTxRemoveInput
  , encodeTxRemoveOutput
  , decodeTxRemoveOutput
  , encodeTxComplete
  , decodeTxComplete
  , encodeTxSignatures
  , decodeTxSignatures
  , encodeTxInitRbf
  , decodeTxInitRbf
  , encodeTxAckRbf
  , decodeTxAckRbf
  , encodeTxAbort
  , decodeTxAbort

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

  -- * Normal operation
  , encodeUpdateAddHtlc
  , decodeUpdateAddHtlc
  , encodeUpdateFulfillHtlc
  , decodeUpdateFulfillHtlc
  , encodeUpdateFailHtlc
  , decodeUpdateFailHtlc
  , encodeUpdateFailMalformedHtlc
  , decodeUpdateFailMalformedHtlc
  , encodeCommitmentSigned
  , decodeCommitmentSigned
  , encodeRevokeAndAck
  , decodeRevokeAndAck
  , encodeUpdateFee
  , decodeUpdateFee

  -- * Channel reestablishment
  , encodeChannelReestablish
  , decodeChannelReestablish
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1
  ( TlvStream
  , unsafeTlvStream
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
  | DecodeInvalidPaymentHash
  | DecodeInvalidPaymentPreimage
  | DecodeInvalidOnionPacket
  | DecodeInvalidSecret
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
                   (decodeBytes 32 bs)
  tid <- maybe (Left DecodeInvalidTxId) Right (mkTxId raw)
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

-- | Decode a u64 as Satoshi.
decodeSatoshi
  :: BS.ByteString -> Either DecodeError (Satoshi, BS.ByteString)
decodeSatoshi !bs = do
  (val, rest) <- maybe (Left DecodeInsufficientBytes) Right (decodeU64 bs)
  Right (Satoshi val, rest)
{-# INLINE decodeSatoshi #-}

-- | Decode a u64 as MilliSatoshi.
decodeMilliSatoshi
  :: BS.ByteString -> Either DecodeError (MilliSatoshi, BS.ByteString)
decodeMilliSatoshi !bs = do
  (val, rest) <- maybe (Left DecodeInsufficientBytes) Right (decodeU64 bs)
  Right (MilliSatoshi val, rest)
{-# INLINE decodeMilliSatoshi #-}

-- | Decode optional TLV stream from remaining bytes.
decodeTlvs :: BS.ByteString -> Either DecodeError TlvStream
decodeTlvs !bs
  | BS.null bs = Right (unsafeTlvStream [])
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

-- | Decode a PaymentHash (32 bytes).
decodePaymentHashBytes
  :: BS.ByteString -> Either DecodeError (PaymentHash, BS.ByteString)
decodePaymentHashBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes paymentHashLen bs)
  ph <- maybe (Left DecodeInvalidPaymentHash) Right (paymentHash raw)
  Right (ph, rest)
{-# INLINE decodePaymentHashBytes #-}

-- | Decode a PaymentPreimage (32 bytes).
decodePaymentPreimageBytes
  :: BS.ByteString -> Either DecodeError (PaymentPreimage, BS.ByteString)
decodePaymentPreimageBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes paymentPreimageLen bs)
  pp <- maybe (Left DecodeInvalidPaymentPreimage) Right (paymentPreimage raw)
  Right (pp, rest)
{-# INLINE decodePaymentPreimageBytes #-}

-- | Decode an OnionPacket (1366 bytes).
decodeOnionPacketBytes
  :: BS.ByteString -> Either DecodeError (OnionPacket, BS.ByteString)
decodeOnionPacketBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes onionPacketLen bs)
  op <- maybe (Left DecodeInvalidOnionPacket) Right (onionPacket raw)
  Right (op, rest)
{-# INLINE decodeOnionPacketBytes #-}

-- | Decode a PerCommitmentSecret (32 bytes).
decodePerCommitmentSecretBytes
  :: BS.ByteString
  -> Either DecodeError (PerCommitmentSecret, BS.ByteString)
decodePerCommitmentSecretBytes !bs = do
  (raw, rest) <- maybe (Left DecodeInsufficientBytes) Right
                   (decodeBytes perCommitmentSecretLen bs)
  sec <- maybe (Left DecodeInvalidSecret) Right
           (perCommitmentSecret raw)
  Right (sec, rest)
{-# INLINE decodePerCommitmentSecretBytes #-}

-- | Encode a u16-prefixed byte string with bounds checking.
encodeU16BytesE :: BS.ByteString -> Either EncodeError BS.ByteString
encodeU16BytesE !bs
  | BS.length bs > 65535 = Left EncodeLengthOverflow
  | otherwise = Right $! encodeU16 (fromIntegral (BS.length bs)) <> bs
{-# INLINE encodeU16BytesE #-}

-- | Check that a list count fits in u16.
checkListCountU16 :: Int -> Either EncodeError Word16
checkListCountU16 !n
  | n > 65535 = Left EncodeLengthOverflow
  | otherwise = Right $! fromIntegral n
{-# INLINE checkListCountU16 #-}

-- | Decode a u16-prefixed byte string.
decodeU16Bytes
  :: BS.ByteString -> Either DecodeError (BS.ByteString, BS.ByteString)
decodeU16Bytes !bs = do
  (len, rest1) <- decodeU16E bs
  let !n = fromIntegral len
  unless (BS.length rest1 >= n) $ Left DecodeInsufficientBytes
  Right (BS.take n rest1, BS.drop n rest1)
{-# INLINE decodeU16Bytes #-}

-- | Decode optional trailing TLV stream.
decodeOptionalTlvs
  :: BS.ByteString -> Either DecodeError (TlvStream, BS.ByteString)
decodeOptionalTlvs !bs
  | BS.null bs = Right (unsafeTlvStream [], BS.empty)
  | otherwise  = case decodeTlvStreamRaw bs of
      Left e  -> Left (DecodeTlvError e)
      Right t -> Right (t, BS.empty)
{-# INLINE decodeOptionalTlvs #-}

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
  , encodeU64 (unSatoshi (openChannelFundingSatoshi msg))
  , encodeU64 (unMilliSatoshi (openChannelPushMsat msg))
  , encodeU64 (unSatoshi (openChannelDustLimitSatoshi msg))
  , encodeU64 (unMilliSatoshi (openChannelMaxHtlcValueInFlight msg))
  , encodeU64 (unSatoshi (openChannelChannelReserveSat msg))
  , encodeU64 (unMilliSatoshi (openChannelHtlcMinimumMsat msg))
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
  (fundingSats, rest3) <- decodeSatoshi rest2
  (pushMsat, rest4) <- decodeMilliSatoshi rest3
  (dustLimit, rest5) <- decodeSatoshi rest4
  (maxHtlcVal, rest6) <- decodeMilliSatoshi rest5
  (chanReserve, rest7) <- decodeSatoshi rest6
  (htlcMin, rest8) <- decodeMilliSatoshi rest7
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
        , openChannelFundingSatoshi      = fundingSats
        , openChannelPushMsat             = pushMsat
        , openChannelDustLimitSatoshi    = dustLimit
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
  , encodeU64 (unSatoshi (acceptChannelDustLimitSatoshi msg))
  , encodeU64 (unMilliSatoshi (acceptChannelMaxHtlcValueInFlight msg))
  , encodeU64 (unSatoshi (acceptChannelChannelReserveSat msg))
  , encodeU64 (unMilliSatoshi (acceptChannelHtlcMinimumMsat msg))
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
  (dustLimit, rest2) <- decodeSatoshi rest1
  (maxHtlcVal, rest3) <- decodeMilliSatoshi rest2
  (chanReserve, rest4) <- decodeSatoshi rest3
  (htlcMin, rest5) <- decodeMilliSatoshi rest4
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
        , acceptChannelDustLimitSatoshi    = dustLimit
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
  , let (TxId bs) = fundingCreatedFundingTxid msg in bs
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
  , encodeU64 (unSatoshi (closingSignedFeeSatoshi msg))
  , unSignature (closingSignedSignature msg)
  , encodeTlvStream (closingSignedTlvs msg)
  ]

-- | Decode a ClosingSigned message (type 39).
decodeClosingSigned
  :: BS.ByteString -> Either DecodeError (ClosingSigned, BS.ByteString)
decodeClosingSigned !bs = do
  (chanId, rest1) <- decodeChannelIdBytes bs
  (feeSats, rest2) <- decodeSatoshi rest1
  (sig, rest3) <- decodeSignatureBytes rest2
  tlvs <- decodeTlvs rest3
  let !msg = ClosingSigned
        { closingSignedChannelId   = chanId
        , closingSignedFeeSatoshi = feeSats
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
           , encodeU64 (unSatoshi (closingCompleteFeeSatoshi msg))
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
  (feeSats, rest4) <- decodeSatoshi rest3
  (locktime, rest5) <- decodeU32E rest4
  tlvs <- decodeTlvs rest5
  let !msg = ClosingComplete
        { closingCompleteChannelId    = chanId
        , closingCompleteCloserScript = closerScript
        , closingCompleteCloseeScript = closeeScript
        , closingCompleteFeeSatoshi  = feeSats
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
           , encodeU64 (unSatoshi (closingSigFeeSatoshi msg))
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
  (feeSats, rest4) <- decodeSatoshi rest3
  (locktime, rest5) <- decodeU32E rest4
  tlvs <- decodeTlvs rest5
  let !msg = ClosingSig
        { closingSigChannelId    = chanId
        , closingSigCloserScript = closerScript
        , closingSigCloseeScript = closeeScript
        , closingSigFeeSatoshi  = feeSats
        , closingSigLocktime     = locktime
        , closingSigTlvs         = tlvs
        }
  Right (msg, BS.empty)

-- Channel establishment v2 (interactive-tx) -----------------------------------

-- | Encode an OpenChannel2 message (type 64).
encodeOpenChannel2 :: OpenChannel2 -> BS.ByteString
encodeOpenChannel2 !msg = mconcat
  [ unChainHash (openChannel2ChainHash msg)
  , unChannelId (openChannel2TempChannelId msg)
  , encodeU32 (openChannel2FundingFeeratePerkw msg)
  , encodeU32 (openChannel2CommitFeeratePerkw msg)
  , encodeU64 (unSatoshi (openChannel2FundingSatoshi msg))
  , encodeU64 (unSatoshi (openChannel2DustLimitSatoshi msg))
  , encodeU64 (unMilliSatoshi (openChannel2MaxHtlcValueInFlight msg))
  , encodeU64 (unMilliSatoshi (openChannel2HtlcMinimumMsat msg))
  , encodeU16 (openChannel2ToSelfDelay msg)
  , encodeU16 (openChannel2MaxAcceptedHtlcs msg)
  , encodeU32 (openChannel2Locktime msg)
  , unPoint (openChannel2FundingPubkey msg)
  , unPoint (openChannel2RevocationBasepoint msg)
  , unPoint (openChannel2PaymentBasepoint msg)
  , unPoint (openChannel2DelayedPaymentBase msg)
  , unPoint (openChannel2HtlcBasepoint msg)
  , unPoint (openChannel2FirstPerCommitPoint msg)
  , unPoint (openChannel2SecondPerCommitPoint msg)
  , BS.singleton (openChannel2ChannelFlags msg)
  , encodeTlvStream (openChannel2Tlvs msg)
  ]

-- | Decode an OpenChannel2 message (type 64).
decodeOpenChannel2
  :: BS.ByteString -> Either DecodeError (OpenChannel2, BS.ByteString)
decodeOpenChannel2 !bs = do
  (ch, rest1) <- decodeChainHashBytes bs
  (tempCid, rest2) <- decodeChannelIdBytes rest1
  (fundingFeerate, rest3) <- decodeU32E rest2
  (commitFeerate, rest4) <- decodeU32E rest3
  (fundingSats, rest5) <- decodeSatoshi rest4
  (dustLimit, rest6) <- decodeSatoshi rest5
  (maxHtlcVal, rest7) <- decodeMilliSatoshi rest6
  (htlcMin, rest8) <- decodeMilliSatoshi rest7
  (toSelfDelay, rest9) <- decodeU16E rest8
  (maxHtlcs, rest10) <- decodeU16E rest9
  (locktime, rest11) <- decodeU32E rest10
  (fundingPk, rest12) <- decodePointBytes rest11
  (revBase, rest13) <- decodePointBytes rest12
  (payBase, rest14) <- decodePointBytes rest13
  (delayBase, rest15) <- decodePointBytes rest14
  (htlcBase, rest16) <- decodePointBytes rest15
  (firstPt, rest17) <- decodePointBytes rest16
  (secondPt, rest18) <- decodePointBytes rest17
  (flags, rest19) <- maybe (Left DecodeInsufficientBytes) Right (decodeU8 rest18)
  tlvs <- decodeTlvs rest19
  let !msg = OpenChannel2
        { openChannel2ChainHash            = ch
        , openChannel2TempChannelId        = tempCid
        , openChannel2FundingFeeratePerkw  = fundingFeerate
        , openChannel2CommitFeeratePerkw   = commitFeerate
        , openChannel2FundingSatoshi      = fundingSats
        , openChannel2DustLimitSatoshi    = dustLimit
        , openChannel2MaxHtlcValueInFlight = maxHtlcVal
        , openChannel2HtlcMinimumMsat      = htlcMin
        , openChannel2ToSelfDelay          = toSelfDelay
        , openChannel2MaxAcceptedHtlcs     = maxHtlcs
        , openChannel2Locktime             = locktime
        , openChannel2FundingPubkey        = fundingPk
        , openChannel2RevocationBasepoint  = revBase
        , openChannel2PaymentBasepoint     = payBase
        , openChannel2DelayedPaymentBase   = delayBase
        , openChannel2HtlcBasepoint        = htlcBase
        , openChannel2FirstPerCommitPoint  = firstPt
        , openChannel2SecondPerCommitPoint = secondPt
        , openChannel2ChannelFlags         = flags
        , openChannel2Tlvs                 = tlvs
        }
  Right (msg, BS.empty)

-- | Encode an AcceptChannel2 message (type 65).
encodeAcceptChannel2 :: AcceptChannel2 -> BS.ByteString
encodeAcceptChannel2 !msg = mconcat
  [ unChannelId (acceptChannel2TempChannelId msg)
  , encodeU64 (unSatoshi (acceptChannel2FundingSatoshi msg))
  , encodeU64 (unSatoshi (acceptChannel2DustLimitSatoshi msg))
  , encodeU64 (unMilliSatoshi (acceptChannel2MaxHtlcValueInFlight msg))
  , encodeU64 (unMilliSatoshi (acceptChannel2HtlcMinimumMsat msg))
  , encodeU32 (acceptChannel2MinimumDepth msg)
  , encodeU16 (acceptChannel2ToSelfDelay msg)
  , encodeU16 (acceptChannel2MaxAcceptedHtlcs msg)
  , unPoint (acceptChannel2FundingPubkey msg)
  , unPoint (acceptChannel2RevocationBasepoint msg)
  , unPoint (acceptChannel2PaymentBasepoint msg)
  , unPoint (acceptChannel2DelayedPaymentBase msg)
  , unPoint (acceptChannel2HtlcBasepoint msg)
  , unPoint (acceptChannel2FirstPerCommitPoint msg)
  , unPoint (acceptChannel2SecondPerCommitPoint msg)
  , encodeTlvStream (acceptChannel2Tlvs msg)
  ]

-- | Decode an AcceptChannel2 message (type 65).
decodeAcceptChannel2
  :: BS.ByteString -> Either DecodeError (AcceptChannel2, BS.ByteString)
decodeAcceptChannel2 !bs = do
  (tempCid, rest1) <- decodeChannelIdBytes bs
  (fundingSats, rest2) <- decodeSatoshi rest1
  (dustLimit, rest3) <- decodeSatoshi rest2
  (maxHtlcVal, rest4) <- decodeMilliSatoshi rest3
  (htlcMin, rest5) <- decodeMilliSatoshi rest4
  (minDepth, rest6) <- decodeU32E rest5
  (toSelfDelay, rest7) <- decodeU16E rest6
  (maxHtlcs, rest8) <- decodeU16E rest7
  (fundingPk, rest9) <- decodePointBytes rest8
  (revBase, rest10) <- decodePointBytes rest9
  (payBase, rest11) <- decodePointBytes rest10
  (delayBase, rest12) <- decodePointBytes rest11
  (htlcBase, rest13) <- decodePointBytes rest12
  (firstPt, rest14) <- decodePointBytes rest13
  (secondPt, rest15) <- decodePointBytes rest14
  tlvs <- decodeTlvs rest15
  let !msg = AcceptChannel2
        { acceptChannel2TempChannelId        = tempCid
        , acceptChannel2FundingSatoshi      = fundingSats
        , acceptChannel2DustLimitSatoshi    = dustLimit
        , acceptChannel2MaxHtlcValueInFlight = maxHtlcVal
        , acceptChannel2HtlcMinimumMsat      = htlcMin
        , acceptChannel2MinimumDepth         = minDepth
        , acceptChannel2ToSelfDelay          = toSelfDelay
        , acceptChannel2MaxAcceptedHtlcs     = maxHtlcs
        , acceptChannel2FundingPubkey        = fundingPk
        , acceptChannel2RevocationBasepoint  = revBase
        , acceptChannel2PaymentBasepoint     = payBase
        , acceptChannel2DelayedPaymentBase   = delayBase
        , acceptChannel2HtlcBasepoint        = htlcBase
        , acceptChannel2FirstPerCommitPoint  = firstPt
        , acceptChannel2SecondPerCommitPoint = secondPt
        , acceptChannel2Tlvs                 = tlvs
        }
  Right (msg, BS.empty)

-- | Encode a TxAddInput message (type 66).
encodeTxAddInput :: TxAddInput -> Either EncodeError BS.ByteString
encodeTxAddInput !msg = do
  prevTxEnc <- encodeU16BytesE (txAddInputPrevTx msg)
  Right $! mconcat
    [ unChannelId (txAddInputChannelId msg)
    , encodeU64 (txAddInputSerialId msg)
    , prevTxEnc
    , encodeU32 (txAddInputPrevVout msg)
    , encodeU32 (txAddInputSequence msg)
    ]

-- | Decode a TxAddInput message (type 66).
decodeTxAddInput
  :: BS.ByteString -> Either DecodeError (TxAddInput, BS.ByteString)
decodeTxAddInput !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (serialId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                         (decodeU64 rest1)
  (prevTx, rest3) <- decodeU16Bytes rest2
  (prevVout, rest4) <- decodeU32E rest3
  (seqNum, rest5) <- decodeU32E rest4
  let !msg = TxAddInput
        { txAddInputChannelId = cid
        , txAddInputSerialId  = serialId
        , txAddInputPrevTx    = prevTx
        , txAddInputPrevVout  = prevVout
        , txAddInputSequence  = seqNum
        }
  Right (msg, rest5)

-- | Encode a TxAddOutput message (type 67).
encodeTxAddOutput :: TxAddOutput -> Either EncodeError BS.ByteString
encodeTxAddOutput !msg = do
  scriptEnc <- encodeU16BytesE (unScriptPubKey (txAddOutputScript msg))
  Right $! mconcat
    [ unChannelId (txAddOutputChannelId msg)
    , encodeU64 (txAddOutputSerialId msg)
    , encodeU64 (unSatoshi (txAddOutputSats msg))
    , scriptEnc
    ]

-- | Decode a TxAddOutput message (type 67).
decodeTxAddOutput
  :: BS.ByteString -> Either DecodeError (TxAddOutput, BS.ByteString)
decodeTxAddOutput !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (serialId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                         (decodeU64 rest1)
  (sats, rest3) <- decodeSatoshi rest2
  (scriptBs, rest4) <- decodeU16Bytes rest3
  let !msg = TxAddOutput
        { txAddOutputChannelId = cid
        , txAddOutputSerialId  = serialId
        , txAddOutputSats      = sats
        , txAddOutputScript    = scriptPubKey scriptBs
        }
  Right (msg, rest4)

-- | Encode a TxRemoveInput message (type 68).
encodeTxRemoveInput :: TxRemoveInput -> BS.ByteString
encodeTxRemoveInput !msg = mconcat
  [ unChannelId (txRemoveInputChannelId msg)
  , encodeU64 (txRemoveInputSerialId msg)
  ]

-- | Decode a TxRemoveInput message (type 68).
decodeTxRemoveInput
  :: BS.ByteString -> Either DecodeError (TxRemoveInput, BS.ByteString)
decodeTxRemoveInput !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (serialId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                         (decodeU64 rest1)
  let !msg = TxRemoveInput
        { txRemoveInputChannelId = cid
        , txRemoveInputSerialId  = serialId
        }
  Right (msg, rest2)

-- | Encode a TxRemoveOutput message (type 69).
encodeTxRemoveOutput :: TxRemoveOutput -> BS.ByteString
encodeTxRemoveOutput !msg = mconcat
  [ unChannelId (txRemoveOutputChannelId msg)
  , encodeU64 (txRemoveOutputSerialId msg)
  ]

-- | Decode a TxRemoveOutput message (type 69).
decodeTxRemoveOutput
  :: BS.ByteString -> Either DecodeError (TxRemoveOutput, BS.ByteString)
decodeTxRemoveOutput !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (serialId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                         (decodeU64 rest1)
  let !msg = TxRemoveOutput
        { txRemoveOutputChannelId = cid
        , txRemoveOutputSerialId  = serialId
        }
  Right (msg, rest2)

-- | Encode a TxComplete message (type 70).
encodeTxComplete :: TxComplete -> BS.ByteString
encodeTxComplete !msg = unChannelId (txCompleteChannelId msg)

-- | Decode a TxComplete message (type 70).
decodeTxComplete
  :: BS.ByteString -> Either DecodeError (TxComplete, BS.ByteString)
decodeTxComplete !bs = do
  (cid, rest) <- decodeChannelIdBytes bs
  let !msg = TxComplete { txCompleteChannelId = cid }
  Right (msg, rest)

-- | Encode a single witness with bounds checking.
encodeWitnessE :: Witness -> Either EncodeError BS.ByteString
encodeWitnessE (Witness !wdata) = encodeU16BytesE wdata

-- | Decode a single witness.
decodeWitness :: BS.ByteString -> Either DecodeError (Witness, BS.ByteString)
decodeWitness !bs = do
  (wdata, rest) <- decodeU16Bytes bs
  Right (Witness wdata, rest)

-- | Encode a TxSignatures message (type 71).
encodeTxSignatures :: TxSignatures -> Either EncodeError BS.ByteString
encodeTxSignatures !msg = do
  let !witnesses = txSignaturesWitnesses msg
  numWit <- checkListCountU16 (length witnesses)
  encodedWits <- traverse encodeWitnessE witnesses
  Right $! mconcat $
    [ unChannelId (txSignaturesChannelId msg)
    , let (TxId bs) = txSignaturesTxid msg in bs
    , encodeU16 numWit
    ] ++ encodedWits

-- | Decode a TxSignatures message (type 71).
decodeTxSignatures
  :: BS.ByteString -> Either DecodeError (TxSignatures, BS.ByteString)
decodeTxSignatures !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (tid, rest2) <- decodeTxIdBytes rest1
  (numWit, rest3) <- decodeU16E rest2
  (witnesses, rest4) <- decodeWitnesses (fromIntegral numWit) rest3
  let !msg = TxSignatures
        { txSignaturesChannelId = cid
        , txSignaturesTxid      = tid
        , txSignaturesWitnesses = witnesses
        }
  Right (msg, rest4)
  where
    decodeWitnesses :: Int -> BS.ByteString
                    -> Either DecodeError ([Witness], BS.ByteString)
    decodeWitnesses 0 !rest = Right ([], rest)
    decodeWitnesses !n !rest = do
      (w, rest') <- decodeWitness rest
      (ws, rest'') <- decodeWitnesses (n - 1) rest'
      Right (w : ws, rest'')

-- | Encode a TxInitRbf message (type 72).
encodeTxInitRbf :: TxInitRbf -> BS.ByteString
encodeTxInitRbf !msg = mconcat
  [ unChannelId (txInitRbfChannelId msg)
  , encodeU32 (txInitRbfLocktime msg)
  , encodeU32 (txInitRbfFeerate msg)
  , encodeTlvStream (txInitRbfTlvs msg)
  ]

-- | Decode a TxInitRbf message (type 72).
decodeTxInitRbf
  :: BS.ByteString -> Either DecodeError (TxInitRbf, BS.ByteString)
decodeTxInitRbf !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (locktime, rest2) <- decodeU32E rest1
  (feerate, rest3) <- decodeU32E rest2
  tlvs <- decodeTlvs rest3
  let !msg = TxInitRbf
        { txInitRbfChannelId = cid
        , txInitRbfLocktime  = locktime
        , txInitRbfFeerate   = feerate
        , txInitRbfTlvs      = tlvs
        }
  Right (msg, BS.empty)

-- | Encode a TxAckRbf message (type 73).
encodeTxAckRbf :: TxAckRbf -> BS.ByteString
encodeTxAckRbf !msg = mconcat
  [ unChannelId (txAckRbfChannelId msg)
  , encodeTlvStream (txAckRbfTlvs msg)
  ]

-- | Decode a TxAckRbf message (type 73).
decodeTxAckRbf
  :: BS.ByteString -> Either DecodeError (TxAckRbf, BS.ByteString)
decodeTxAckRbf !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  tlvs <- decodeTlvs rest1
  let !msg = TxAckRbf
        { txAckRbfChannelId = cid
        , txAckRbfTlvs      = tlvs
        }
  Right (msg, BS.empty)

-- | Encode a TxAbort message (type 74).
encodeTxAbort :: TxAbort -> Either EncodeError BS.ByteString
encodeTxAbort !msg = do
  dataEnc <- encodeU16BytesE (txAbortData msg)
  Right $! mconcat
    [ unChannelId (txAbortChannelId msg)
    , dataEnc
    ]

-- | Decode a TxAbort message (type 74).
decodeTxAbort
  :: BS.ByteString -> Either DecodeError (TxAbort, BS.ByteString)
decodeTxAbort !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (dat, rest2) <- decodeU16Bytes rest1
  let !msg = TxAbort
        { txAbortChannelId = cid
        , txAbortData      = dat
        }
  Right (msg, rest2)

-- Normal operation ------------------------------------------------------------

-- | Encode an UpdateAddHtlc message (type 128).
encodeUpdateAddHtlc :: UpdateAddHtlc -> BS.ByteString
encodeUpdateAddHtlc !m = mconcat
  [ unChannelId (updateAddHtlcChannelId m)
  , encodeU64 (updateAddHtlcId m)
  , encodeU64 (unMilliSatoshi (updateAddHtlcAmountMsat m))
  , unPaymentHash (updateAddHtlcPaymentHash m)
  , encodeU32 (updateAddHtlcCltvExpiry m)
  , unOnionPacket (updateAddHtlcOnionPacket m)
  , encodeTlvStream (updateAddHtlcTlvs m)
  ]
{-# INLINABLE encodeUpdateAddHtlc #-}

-- | Decode an UpdateAddHtlc message (type 128).
decodeUpdateAddHtlc
  :: BS.ByteString -> Either DecodeError (UpdateAddHtlc, BS.ByteString)
decodeUpdateAddHtlc !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (htlcId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                       (decodeU64 rest1)
  (amtMsat, rest3) <- maybe (Left DecodeInsufficientBytes) Right
                        (decodeU64 rest2)
  (pHash, rest4) <- decodePaymentHashBytes rest3
  (cltvExp, rest5) <- decodeU32E rest4
  (onion, rest6) <- decodeOnionPacketBytes rest5
  (tlvs, rest7) <- decodeOptionalTlvs rest6
  let !msg = UpdateAddHtlc
        { updateAddHtlcChannelId   = cid
        , updateAddHtlcId          = htlcId
        , updateAddHtlcAmountMsat  = MilliSatoshi amtMsat
        , updateAddHtlcPaymentHash = pHash
        , updateAddHtlcCltvExpiry  = cltvExp
        , updateAddHtlcOnionPacket = onion
        , updateAddHtlcTlvs        = tlvs
        }
  Right (msg, rest7)
{-# INLINABLE decodeUpdateAddHtlc #-}

-- | Encode an UpdateFulfillHtlc message (type 130).
encodeUpdateFulfillHtlc :: UpdateFulfillHtlc -> BS.ByteString
encodeUpdateFulfillHtlc !m = mconcat
  [ unChannelId (updateFulfillHtlcChannelId m)
  , encodeU64 (updateFulfillHtlcId m)
  , unPaymentPreimage (updateFulfillHtlcPaymentPreimage m)
  , encodeTlvStream (updateFulfillHtlcTlvs m)
  ]

-- | Decode an UpdateFulfillHtlc message (type 130).
decodeUpdateFulfillHtlc
  :: BS.ByteString -> Either DecodeError (UpdateFulfillHtlc, BS.ByteString)
decodeUpdateFulfillHtlc !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (htlcId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                       (decodeU64 rest1)
  (preimage, rest3) <- decodePaymentPreimageBytes rest2
  (tlvs, rest4) <- decodeOptionalTlvs rest3
  let !msg = UpdateFulfillHtlc
        { updateFulfillHtlcChannelId       = cid
        , updateFulfillHtlcId              = htlcId
        , updateFulfillHtlcPaymentPreimage = preimage
        , updateFulfillHtlcTlvs            = tlvs
        }
  Right (msg, rest4)

-- | Encode an UpdateFailHtlc message (type 131).
encodeUpdateFailHtlc :: UpdateFailHtlc -> Either EncodeError BS.ByteString
encodeUpdateFailHtlc !m = do
  reasonEnc <- encodeU16BytesE (updateFailHtlcReason m)
  Right $! mconcat
    [ unChannelId (updateFailHtlcChannelId m)
    , encodeU64 (updateFailHtlcId m)
    , reasonEnc
    , encodeTlvStream (updateFailHtlcTlvs m)
    ]

-- | Decode an UpdateFailHtlc message (type 131).
decodeUpdateFailHtlc
  :: BS.ByteString -> Either DecodeError (UpdateFailHtlc, BS.ByteString)
decodeUpdateFailHtlc !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (htlcId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                       (decodeU64 rest1)
  (reason, rest3) <- decodeU16Bytes rest2
  (tlvs, rest4) <- decodeOptionalTlvs rest3
  let !msg = UpdateFailHtlc
        { updateFailHtlcChannelId = cid
        , updateFailHtlcId        = htlcId
        , updateFailHtlcReason    = reason
        , updateFailHtlcTlvs      = tlvs
        }
  Right (msg, rest4)

-- | Encode an UpdateFailMalformedHtlc message (type 135).
encodeUpdateFailMalformedHtlc :: UpdateFailMalformedHtlc -> BS.ByteString
encodeUpdateFailMalformedHtlc !m = mconcat
  [ unChannelId (updateFailMalformedHtlcChannelId m)
  , encodeU64 (updateFailMalformedHtlcId m)
  , unPaymentHash (updateFailMalformedHtlcSha256Onion m)
  , encodeU16 (updateFailMalformedHtlcFailureCode m)
  ]

-- | Decode an UpdateFailMalformedHtlc message (type 135).
decodeUpdateFailMalformedHtlc
  :: BS.ByteString -> Either DecodeError (UpdateFailMalformedHtlc, BS.ByteString)
decodeUpdateFailMalformedHtlc !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (htlcId, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                       (decodeU64 rest1)
  (sha256Onion, rest3) <- decodePaymentHashBytes rest2
  (failCode, rest4) <- decodeU16E rest3
  let !msg = UpdateFailMalformedHtlc
        { updateFailMalformedHtlcChannelId   = cid
        , updateFailMalformedHtlcId          = htlcId
        , updateFailMalformedHtlcSha256Onion = sha256Onion
        , updateFailMalformedHtlcFailureCode = failCode
        }
  Right (msg, rest4)

-- | Encode a CommitmentSigned message (type 132).
encodeCommitmentSigned :: CommitmentSigned -> Either EncodeError BS.ByteString
encodeCommitmentSigned !m = do
  let !sigs = commitmentSignedHtlcSignatures m
  numHtlcs <- checkListCountU16 (length sigs)
  Right $! mconcat $
    [ unChannelId (commitmentSignedChannelId m)
    , unSignature (commitmentSignedSignature m)
    , encodeU16 numHtlcs
    ] ++ map unSignature sigs
{-# INLINABLE encodeCommitmentSigned #-}

-- | Decode a CommitmentSigned message (type 132).
decodeCommitmentSigned
  :: BS.ByteString -> Either DecodeError (CommitmentSigned, BS.ByteString)
decodeCommitmentSigned !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (sig, rest2) <- decodeSignatureBytes rest1
  (numHtlcs, rest3) <- decodeU16E rest2
  (htlcSigs, rest4) <- decodeSignatures (fromIntegral numHtlcs) rest3
  let !msg = CommitmentSigned
        { commitmentSignedChannelId      = cid
        , commitmentSignedSignature      = sig
        , commitmentSignedHtlcSignatures = htlcSigs
        }
  Right (msg, rest4)
  where
    decodeSignatures :: Int -> BS.ByteString
                     -> Either DecodeError ([Signature], BS.ByteString)
    decodeSignatures !n !input = go n input []
      where
        go :: Int -> BS.ByteString -> [Signature]
           -> Either DecodeError ([Signature], BS.ByteString)
        go 0 !remaining !acc = Right (reverse acc, remaining)
        go !count !remaining !acc = do
          (s, rest) <- decodeSignatureBytes remaining
          go (count - 1) rest (s : acc)
{-# INLINABLE decodeCommitmentSigned #-}

-- | Encode a RevokeAndAck message (type 133).
encodeRevokeAndAck :: RevokeAndAck -> BS.ByteString
encodeRevokeAndAck !m = mconcat
  [ unChannelId (revokeAndAckChannelId m)
  , unPerCommitmentSecret (revokeAndAckPerCommitmentSecret m)
  , unPoint (revokeAndAckNextPerCommitPoint m)
  ]

-- | Decode a RevokeAndAck message (type 133).
decodeRevokeAndAck
  :: BS.ByteString -> Either DecodeError (RevokeAndAck, BS.ByteString)
decodeRevokeAndAck !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (sec, rest2) <- decodePerCommitmentSecretBytes rest1
  (nextPoint, rest3) <- decodePointBytes rest2
  let !msg = RevokeAndAck
        { revokeAndAckChannelId           = cid
        , revokeAndAckPerCommitmentSecret = sec
        , revokeAndAckNextPerCommitPoint  = nextPoint
        }
  Right (msg, rest3)

-- | Encode an UpdateFee message (type 134).
encodeUpdateFee :: UpdateFee -> BS.ByteString
encodeUpdateFee !m = mconcat
  [ unChannelId (updateFeeChannelId m)
  , encodeU32 (updateFeeFeeratePerKw m)
  ]

-- | Decode an UpdateFee message (type 134).
decodeUpdateFee
  :: BS.ByteString -> Either DecodeError (UpdateFee, BS.ByteString)
decodeUpdateFee !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (feerate, rest2) <- decodeU32E rest1
  let !msg = UpdateFee
        { updateFeeChannelId    = cid
        , updateFeeFeeratePerKw = feerate
        }
  Right (msg, rest2)

-- Channel reestablishment -----------------------------------------------------

-- | Encode a ChannelReestablish message (type 136).
encodeChannelReestablish :: ChannelReestablish -> BS.ByteString
encodeChannelReestablish !m = mconcat
  [ unChannelId (channelReestablishChannelId m)
  , encodeU64 (channelReestablishNextCommitNum m)
  , encodeU64 (channelReestablishNextRevocationNum m)
  , unPerCommitmentSecret (channelReestablishYourLastCommitSecret m)
  , unPoint (channelReestablishMyCurrentCommitPoint m)
  , encodeTlvStream (channelReestablishTlvs m)
  ]

-- | Decode a ChannelReestablish message (type 136).
decodeChannelReestablish
  :: BS.ByteString -> Either DecodeError (ChannelReestablish, BS.ByteString)
decodeChannelReestablish !bs = do
  (cid, rest1) <- decodeChannelIdBytes bs
  (nextCommit, rest2) <- maybe (Left DecodeInsufficientBytes) Right
                           (decodeU64 rest1)
  (nextRevoke, rest3) <- maybe (Left DecodeInsufficientBytes) Right
                           (decodeU64 rest2)
  (sec, rest4) <- decodePerCommitmentSecretBytes rest3
  (myPoint, rest5) <- decodePointBytes rest4
  (tlvs, rest6) <- decodeOptionalTlvs rest5
  let !msg = ChannelReestablish
        { channelReestablishChannelId            = cid
        , channelReestablishNextCommitNum        = nextCommit
        , channelReestablishNextRevocationNum    = nextRevoke
        , channelReestablishYourLastCommitSecret = sec
        , channelReestablishMyCurrentCommitPoint = myPoint
        , channelReestablishTlvs                 = tlvs
        }
  Right (msg, rest6)
