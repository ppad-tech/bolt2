{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Weigh allocation benchmarks for BOLT #2 message codecs.

module Main where

import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT1 (TlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT2
import Weigh

-- | Wrapper for encoding functions that return Either.
forceEncode :: Either EncodeError BS.ByteString -> BS.ByteString
forceEncode (Right bs) = bs
forceEncode (Left e)   = error $ "forceEncode: " ++ show e
{-# INLINE forceEncode #-}

-- Test data construction ------------------------------------------------------

-- | 32 zero bytes for channel IDs, chain hashes, etc.
zeroBytes32 :: BS.ByteString
zeroBytes32 = BS.replicate 32 0x00
{-# NOINLINE zeroBytes32 #-}

-- | 33-byte compressed public key (02 prefix + 32 zero bytes).
testPoint :: Point
testPoint = case point (BS.cons 0x02 zeroBytes32) of
  Just p  -> p
  Nothing -> error "testPoint: invalid"
{-# NOINLINE testPoint #-}

-- | 64-byte signature.
testSignature :: Signature
testSignature = case signature (BS.replicate 64 0x01) of
  Just s  -> s
  Nothing -> error "testSignature: invalid"
{-# NOINLINE testSignature #-}

-- | 32-byte channel ID.
testChannelId :: ChannelId
testChannelId = case channelId zeroBytes32 of
  Just c  -> c
  Nothing -> error "testChannelId: invalid"
{-# NOINLINE testChannelId #-}

-- | 32-byte chain hash.
testChainHash :: ChainHash
testChainHash = case chainHash zeroBytes32 of
  Just h  -> h
  Nothing -> error "testChainHash: invalid"
{-# NOINLINE testChainHash #-}

-- | 32-byte txid.
testTxId :: TxId
testTxId = case txId zeroBytes32 of
  Just t  -> t
  Nothing -> error "testTxId: invalid"
{-# NOINLINE testTxId #-}

-- | 32-byte payment hash.
testPaymentHash :: PaymentHash
testPaymentHash = case paymentHash zeroBytes32 of
  Just h  -> h
  Nothing -> error "testPaymentHash: invalid"
{-# NOINLINE testPaymentHash #-}

-- | 1366-byte onion packet.
testOnionPacket :: OnionPacket
testOnionPacket = case onionPacket (BS.replicate 1366 0x00) of
  Just o  -> o
  Nothing -> error "testOnionPacket: invalid"
{-# NOINLINE testOnionPacket #-}

-- | Empty TLV stream.
emptyTlvs :: TlvStream
emptyTlvs = unsafeTlvStream []
{-# NOINLINE emptyTlvs #-}

-- Message constructors --------------------------------------------------------

-- | Construct OpenChannel message.
mkOpenChannel :: ChainHash -> ChannelId -> Point -> TlvStream -> OpenChannel
mkOpenChannel !ch !cid !pt !tlvs = OpenChannel
  { openChannelChainHash            = ch
  , openChannelTempChannelId        = cid
  , openChannelFundingSatoshis      = Satoshis 1000000
  , openChannelPushMsat             = MilliSatoshis 0
  , openChannelDustLimitSatoshis    = Satoshis 546
  , openChannelMaxHtlcValueInFlight = MilliSatoshis 1000000000
  , openChannelChannelReserveSat    = Satoshis 10000
  , openChannelHtlcMinimumMsat      = MilliSatoshis 1000
  , openChannelFeeratePerKw         = 250
  , openChannelToSelfDelay          = 144
  , openChannelMaxAcceptedHtlcs     = 30
  , openChannelFundingPubkey        = pt
  , openChannelRevocationBasepoint  = pt
  , openChannelPaymentBasepoint     = pt
  , openChannelDelayedPaymentBase   = pt
  , openChannelHtlcBasepoint        = pt
  , openChannelFirstPerCommitPoint  = pt
  , openChannelChannelFlags         = 0x00
  , openChannelTlvs                 = tlvs
  }

-- | Construct OpenChannel2 message.
mkOpenChannel2 :: ChainHash -> ChannelId -> Point -> TlvStream -> OpenChannel2
mkOpenChannel2 !ch !cid !pt !tlvs = OpenChannel2
  { openChannel2ChainHash            = ch
  , openChannel2TempChannelId        = cid
  , openChannel2FundingFeeratePerkw  = 2500
  , openChannel2CommitFeeratePerkw   = 250
  , openChannel2FundingSatoshis      = Satoshis 1000000
  , openChannel2DustLimitSatoshis    = Satoshis 546
  , openChannel2MaxHtlcValueInFlight = MilliSatoshis 1000000000
  , openChannel2HtlcMinimumMsat      = MilliSatoshis 1000
  , openChannel2ToSelfDelay          = 144
  , openChannel2MaxAcceptedHtlcs     = 30
  , openChannel2Locktime             = 0
  , openChannel2FundingPubkey        = pt
  , openChannel2RevocationBasepoint  = pt
  , openChannel2PaymentBasepoint     = pt
  , openChannel2DelayedPaymentBase   = pt
  , openChannel2HtlcBasepoint        = pt
  , openChannel2FirstPerCommitPoint  = pt
  , openChannel2SecondPerCommitPoint = pt
  , openChannel2ChannelFlags         = 0x00
  , openChannel2Tlvs                 = tlvs
  }

-- | Construct TxSignatures message.
mkTxSignatures :: ChannelId -> TxId -> [Witness] -> TxSignatures
mkTxSignatures !cid !tid !ws = TxSignatures
  { txSignaturesChannelId = cid
  , txSignaturesTxid      = tid
  , txSignaturesWitnesses = ws
  }

-- | Construct ClosingSigned message.
mkClosingSigned :: ChannelId -> Signature -> TlvStream -> ClosingSigned
mkClosingSigned !cid !sig !tlvs = ClosingSigned
  { closingSignedChannelId   = cid
  , closingSignedFeeSatoshis = Satoshis 1000
  , closingSignedSignature   = sig
  , closingSignedTlvs        = tlvs
  }

-- | Construct UpdateAddHtlc message.
mkUpdateAddHtlc
  :: ChannelId -> PaymentHash -> OnionPacket -> TlvStream -> UpdateAddHtlc
mkUpdateAddHtlc !cid !ph !onion !tlvs = UpdateAddHtlc
  { updateAddHtlcChannelId   = cid
  , updateAddHtlcId          = 0
  , updateAddHtlcAmountMsat  = MilliSatoshis 10000000
  , updateAddHtlcPaymentHash = ph
  , updateAddHtlcCltvExpiry  = 800000
  , updateAddHtlcOnionPacket = onion
  , updateAddHtlcTlvs        = tlvs
  }

-- | Construct CommitmentSigned message.
mkCommitmentSigned :: ChannelId -> Signature -> [Signature] -> CommitmentSigned
mkCommitmentSigned !cid !sig !htlcSigs = CommitmentSigned
  { commitmentSignedChannelId      = cid
  , commitmentSignedSignature      = sig
  , commitmentSignedHtlcSignatures = htlcSigs
  }

-- Pre-constructed messages ----------------------------------------------------

-- | Test OpenChannel message.
testOpenChannel :: OpenChannel
testOpenChannel =
  mkOpenChannel testChainHash testChannelId testPoint emptyTlvs
{-# NOINLINE testOpenChannel #-}

-- | Encoded OpenChannel for decode benchmarks.
encodedOpenChannel :: BS.ByteString
encodedOpenChannel = encodeOpenChannel testOpenChannel
{-# NOINLINE encodedOpenChannel #-}

-- | Test OpenChannel2 message.
testOpenChannel2 :: OpenChannel2
testOpenChannel2 =
  mkOpenChannel2 testChainHash testChannelId testPoint emptyTlvs
{-# NOINLINE testOpenChannel2 #-}

-- | Encoded OpenChannel2 for decode benchmarks.
encodedOpenChannel2 :: BS.ByteString
encodedOpenChannel2 = encodeOpenChannel2 testOpenChannel2
{-# NOINLINE encodedOpenChannel2 #-}

-- | Test witness data.
testWitness :: Witness
testWitness = Witness (BS.replicate 107 0xab)
{-# NOINLINE testWitness #-}

-- | Multiple witnesses for TxSignatures.
testWitnesses :: [Witness]
testWitnesses = replicate 5 testWitness
{-# NOINLINE testWitnesses #-}

-- | Test TxSignatures message.
testTxSignatures :: TxSignatures
testTxSignatures = mkTxSignatures testChannelId testTxId testWitnesses
{-# NOINLINE testTxSignatures #-}

-- | Encoded TxSignatures for decode benchmarks.
encodedTxSignatures :: BS.ByteString
encodedTxSignatures = case encodeTxSignatures testTxSignatures of
  Right bs -> bs
  Left e   -> error $ "encodedTxSignatures: " ++ show e
{-# NOINLINE encodedTxSignatures #-}

-- | Test ClosingSigned message.
testClosingSigned :: ClosingSigned
testClosingSigned = mkClosingSigned testChannelId testSignature emptyTlvs
{-# NOINLINE testClosingSigned #-}

-- | Encoded ClosingSigned for decode benchmarks.
encodedClosingSigned :: BS.ByteString
encodedClosingSigned = encodeClosingSigned testClosingSigned
{-# NOINLINE encodedClosingSigned #-}

-- | Test UpdateAddHtlc message.
testUpdateAddHtlc :: UpdateAddHtlc
testUpdateAddHtlc =
  mkUpdateAddHtlc testChannelId testPaymentHash testOnionPacket emptyTlvs
{-# NOINLINE testUpdateAddHtlc #-}

-- | Encoded UpdateAddHtlc for decode benchmarks.
encodedUpdateAddHtlc :: BS.ByteString
encodedUpdateAddHtlc = encodeUpdateAddHtlc testUpdateAddHtlc
{-# NOINLINE encodedUpdateAddHtlc #-}

-- | HTLC signatures for CommitmentSigned.
testHtlcSigs :: [Signature]
testHtlcSigs = replicate 10 testSignature
{-# NOINLINE testHtlcSigs #-}

-- | Test CommitmentSigned message.
testCommitmentSigned :: CommitmentSigned
testCommitmentSigned =
  mkCommitmentSigned testChannelId testSignature testHtlcSigs
{-# NOINLINE testCommitmentSigned #-}

-- | Encoded CommitmentSigned for decode benchmarks.
encodedCommitmentSigned :: BS.ByteString
encodedCommitmentSigned = case encodeCommitmentSigned testCommitmentSigned of
  Right bs -> bs
  Left e   -> error $ "encodedCommitmentSigned: " ++ show e
{-# NOINLINE encodedCommitmentSigned #-}

-- | Large HTLC signatures for CommitmentSigned (100).
testHtlcSigsLarge :: [Signature]
testHtlcSigsLarge = replicate 100 testSignature
{-# NOINLINE testHtlcSigsLarge #-}

-- | Test CommitmentSigned message (100 sigs).
testCommitmentSignedLarge :: CommitmentSigned
testCommitmentSignedLarge =
  mkCommitmentSigned testChannelId testSignature testHtlcSigsLarge
{-# NOINLINE testCommitmentSignedLarge #-}

-- | Encoded large CommitmentSigned for decode benchmarks.
encodedCommitmentSignedLarge :: BS.ByteString
encodedCommitmentSignedLarge =
  case encodeCommitmentSigned testCommitmentSignedLarge of
    Right bs -> bs
    Left e   -> error $ "encodedCommitmentSignedLarge: " ++ show e
{-# NOINLINE encodedCommitmentSignedLarge #-}

-- | Max HTLC signatures for CommitmentSigned (483).
testHtlcSigsMax :: [Signature]
testHtlcSigsMax = replicate 483 testSignature
{-# NOINLINE testHtlcSigsMax #-}

-- | Test CommitmentSigned message (483 sigs).
testCommitmentSignedMax :: CommitmentSigned
testCommitmentSignedMax =
  mkCommitmentSigned testChannelId testSignature testHtlcSigsMax
{-# NOINLINE testCommitmentSignedMax #-}

-- | Encoded max CommitmentSigned for decode benchmarks.
encodedCommitmentSignedMax :: BS.ByteString
encodedCommitmentSignedMax =
  case encodeCommitmentSigned testCommitmentSignedMax of
    Right bs -> bs
    Left e   -> error $ "encodedCommitmentSignedMax: " ++ show e
{-# NOINLINE encodedCommitmentSignedMax #-}

-- Weigh benchmarks ------------------------------------------------------------

main :: IO ()
main = mainWith $ do
  -- V1 message construction and encoding
  wgroup "v1/open_channel" $ do
    func "construct" (mkOpenChannel testChainHash testChannelId testPoint)
      emptyTlvs
    func "encode" encodeOpenChannel testOpenChannel
    func "decode" decodeOpenChannel encodedOpenChannel

  -- V2 message construction and encoding
  wgroup "v2/open_channel2" $ do
    func "construct" (mkOpenChannel2 testChainHash testChannelId testPoint)
      emptyTlvs
    func "encode" encodeOpenChannel2 testOpenChannel2
    func "decode" decodeOpenChannel2 encodedOpenChannel2

  wgroup "v2/tx_signatures" $ do
    func "construct" (mkTxSignatures testChannelId testTxId) testWitnesses
    func "encode" (forceEncode . encodeTxSignatures) testTxSignatures
    func "decode" decodeTxSignatures encodedTxSignatures

  -- Close messages
  wgroup "close/closing_signed" $ do
    func "construct" (mkClosingSigned testChannelId testSignature) emptyTlvs
    func "encode" encodeClosingSigned testClosingSigned
    func "decode" decodeClosingSigned encodedClosingSigned

  -- Normal operation (hot paths)
  wgroup "normal/update_add_htlc" $ do
    func "construct"
      (mkUpdateAddHtlc testChannelId testPaymentHash testOnionPacket) emptyTlvs
    func "encode" encodeUpdateAddHtlc testUpdateAddHtlc
    func "decode" decodeUpdateAddHtlc encodedUpdateAddHtlc

  wgroup "normal/commitment_signed" $ do
    func "construct" (mkCommitmentSigned testChannelId testSignature)
      testHtlcSigs
    func "encode" (forceEncode . encodeCommitmentSigned) testCommitmentSigned
    func "decode" decodeCommitmentSigned encodedCommitmentSigned

  wgroup "normal/commitment_signed_100" $ do
    func "decode" decodeCommitmentSigned encodedCommitmentSignedLarge

  wgroup "normal/commitment_signed_483" $ do
    func "decode" decodeCommitmentSigned encodedCommitmentSignedMax
