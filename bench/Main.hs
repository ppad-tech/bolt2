{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Criterion timing benchmarks for BOLT #2 message codecs.

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT1 (TlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT2

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

-- V1 messages -----------------------------------------------------------------

-- | Test OpenChannel message.
testOpenChannel :: OpenChannel
testOpenChannel = OpenChannel
  { openChannelChainHash            = testChainHash
  , openChannelTempChannelId        = testChannelId
  , openChannelFundingSatoshis      = Satoshis 1000000
  , openChannelPushMsat             = MilliSatoshis 0
  , openChannelDustLimitSatoshis    = Satoshis 546
  , openChannelMaxHtlcValueInFlight = MilliSatoshis 1000000000
  , openChannelChannelReserveSat    = Satoshis 10000
  , openChannelHtlcMinimumMsat      = MilliSatoshis 1000
  , openChannelFeeratePerKw         = 250
  , openChannelToSelfDelay          = 144
  , openChannelMaxAcceptedHtlcs     = 30
  , openChannelFundingPubkey        = testPoint
  , openChannelRevocationBasepoint  = testPoint
  , openChannelPaymentBasepoint     = testPoint
  , openChannelDelayedPaymentBase   = testPoint
  , openChannelHtlcBasepoint        = testPoint
  , openChannelFirstPerCommitPoint  = testPoint
  , openChannelChannelFlags         = 0x00
  , openChannelTlvs                 = emptyTlvs
  }
{-# NOINLINE testOpenChannel #-}

-- | Encoded OpenChannel for decode benchmarks.
encodedOpenChannel :: BS.ByteString
encodedOpenChannel = encodeOpenChannel testOpenChannel
{-# NOINLINE encodedOpenChannel #-}

-- V2 messages -----------------------------------------------------------------

-- | Test OpenChannel2 message.
testOpenChannel2 :: OpenChannel2
testOpenChannel2 = OpenChannel2
  { openChannel2ChainHash            = testChainHash
  , openChannel2TempChannelId        = testChannelId
  , openChannel2FundingFeeratePerkw  = 2500
  , openChannel2CommitFeeratePerkw   = 250
  , openChannel2FundingSatoshis      = Satoshis 1000000
  , openChannel2DustLimitSatoshis    = Satoshis 546
  , openChannel2MaxHtlcValueInFlight = MilliSatoshis 1000000000
  , openChannel2HtlcMinimumMsat      = MilliSatoshis 1000
  , openChannel2ToSelfDelay          = 144
  , openChannel2MaxAcceptedHtlcs     = 30
  , openChannel2Locktime             = 0
  , openChannel2FundingPubkey        = testPoint
  , openChannel2RevocationBasepoint  = testPoint
  , openChannel2PaymentBasepoint     = testPoint
  , openChannel2DelayedPaymentBase   = testPoint
  , openChannel2HtlcBasepoint        = testPoint
  , openChannel2FirstPerCommitPoint  = testPoint
  , openChannel2SecondPerCommitPoint = testPoint
  , openChannel2ChannelFlags         = 0x00
  , openChannel2Tlvs                 = emptyTlvs
  }
{-# NOINLINE testOpenChannel2 #-}

-- | Encoded OpenChannel2 for decode benchmarks.
encodedOpenChannel2 :: BS.ByteString
encodedOpenChannel2 = encodeOpenChannel2 testOpenChannel2
{-# NOINLINE encodedOpenChannel2 #-}

-- | Test witness data (simulated P2WPKH signature + pubkey).
testWitness :: Witness
testWitness = Witness (BS.replicate 107 0xab)
{-# NOINLINE testWitness #-}

-- | TxSignatures with multiple witnesses.
testTxSignatures :: TxSignatures
testTxSignatures = TxSignatures
  { txSignaturesChannelId = testChannelId
  , txSignaturesTxid      = testTxId
  , txSignaturesWitnesses = replicate 5 testWitness
  }
{-# NOINLINE testTxSignatures #-}

-- | Encoded TxSignatures for decode benchmarks.
encodedTxSignatures :: BS.ByteString
encodedTxSignatures = case encodeTxSignatures testTxSignatures of
  Right bs -> bs
  Left e   -> error $ "encodedTxSignatures: " ++ show e
{-# NOINLINE encodedTxSignatures #-}

-- Close messages --------------------------------------------------------------

-- | Test ClosingSigned message.
testClosingSigned :: ClosingSigned
testClosingSigned = ClosingSigned
  { closingSignedChannelId   = testChannelId
  , closingSignedFeeSatoshis = Satoshis 1000
  , closingSignedSignature   = testSignature
  , closingSignedTlvs        = emptyTlvs
  }
{-# NOINLINE testClosingSigned #-}

-- | Encoded ClosingSigned for decode benchmarks.
encodedClosingSigned :: BS.ByteString
encodedClosingSigned = encodeClosingSigned testClosingSigned
{-# NOINLINE encodedClosingSigned #-}

-- Normal operation messages ---------------------------------------------------

-- | Test UpdateAddHtlc message.
testUpdateAddHtlc :: UpdateAddHtlc
testUpdateAddHtlc = UpdateAddHtlc
  { updateAddHtlcChannelId   = testChannelId
  , updateAddHtlcId          = 0
  , updateAddHtlcAmountMsat  = MilliSatoshis 10000000
  , updateAddHtlcPaymentHash = testPaymentHash
  , updateAddHtlcCltvExpiry  = 800000
  , updateAddHtlcOnionPacket = testOnionPacket
  , updateAddHtlcTlvs        = emptyTlvs
  }
{-# NOINLINE testUpdateAddHtlc #-}

-- | Encoded UpdateAddHtlc for decode benchmarks.
encodedUpdateAddHtlc :: BS.ByteString
encodedUpdateAddHtlc = encodeUpdateAddHtlc testUpdateAddHtlc
{-# NOINLINE encodedUpdateAddHtlc #-}

-- | Test CommitmentSigned message with HTLC signatures (10 sigs).
testCommitmentSigned :: CommitmentSigned
testCommitmentSigned = CommitmentSigned
  { commitmentSignedChannelId      = testChannelId
  , commitmentSignedSignature      = testSignature
  , commitmentSignedHtlcSignatures = replicate 10 testSignature
  }
{-# NOINLINE testCommitmentSigned #-}

-- | Encoded CommitmentSigned for decode benchmarks.
encodedCommitmentSigned :: BS.ByteString
encodedCommitmentSigned = case encodeCommitmentSigned testCommitmentSigned of
  Right bs -> bs
  Left e   -> error $ "encodedCommitmentSigned: " ++ show e
{-# NOINLINE encodedCommitmentSigned #-}

-- | Test CommitmentSigned with many HTLC signatures (100 sigs).
testCommitmentSignedLarge :: CommitmentSigned
testCommitmentSignedLarge = CommitmentSigned
  { commitmentSignedChannelId      = testChannelId
  , commitmentSignedSignature      = testSignature
  , commitmentSignedHtlcSignatures = replicate 100 testSignature
  }
{-# NOINLINE testCommitmentSignedLarge #-}

-- | Encoded large CommitmentSigned for decode benchmarks.
encodedCommitmentSignedLarge :: BS.ByteString
encodedCommitmentSignedLarge =
  case encodeCommitmentSigned testCommitmentSignedLarge of
    Right bs -> bs
    Left e   -> error $ "encodedCommitmentSignedLarge: " ++ show e
{-# NOINLINE encodedCommitmentSignedLarge #-}

-- | Test CommitmentSigned with max HTLC signatures (483 sigs).
testCommitmentSignedMax :: CommitmentSigned
testCommitmentSignedMax = CommitmentSigned
  { commitmentSignedChannelId      = testChannelId
  , commitmentSignedSignature      = testSignature
  , commitmentSignedHtlcSignatures = replicate 483 testSignature
  }
{-# NOINLINE testCommitmentSignedMax #-}

-- | Encoded max CommitmentSigned for decode benchmarks.
encodedCommitmentSignedMax :: BS.ByteString
encodedCommitmentSignedMax =
  case encodeCommitmentSigned testCommitmentSignedMax of
    Right bs -> bs
    Left e   -> error $ "encodedCommitmentSignedMax: " ++ show e
{-# NOINLINE encodedCommitmentSignedMax #-}

-- Benchmark groups ------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "v1"
      [ bgroup "open_channel"
          [ bench "encode" $ nf encodeOpenChannel testOpenChannel
          , bench "decode" $ nf decodeOpenChannel encodedOpenChannel
          ]
      ]
  , bgroup "v2"
      [ bgroup "open_channel2"
          [ bench "encode" $ nf encodeOpenChannel2 testOpenChannel2
          , bench "decode" $ nf decodeOpenChannel2 encodedOpenChannel2
          ]
      , bgroup "tx_signatures"
          [ bench "encode" $ nf encodeTxSignatures testTxSignatures
          , bench "decode" $ nf decodeTxSignatures encodedTxSignatures
          ]
      ]
  , bgroup "close"
      [ bgroup "closing_signed"
          [ bench "encode" $ nf encodeClosingSigned testClosingSigned
          , bench "decode" $ nf decodeClosingSigned encodedClosingSigned
          ]
      ]
  , bgroup "normal"
      [ bgroup "update_add_htlc"
          [ bench "encode" $ nf encodeUpdateAddHtlc testUpdateAddHtlc
          , bench "decode" $ nf decodeUpdateAddHtlc encodedUpdateAddHtlc
          ]
      , bgroup "commitment_signed"
          [ bench "encode" $ nf encodeCommitmentSigned testCommitmentSigned
          , bench "decode" $ nf decodeCommitmentSigned encodedCommitmentSigned
          ]
      , bgroup "commitment_signed_100"
          [ bench "encode" $
              nf encodeCommitmentSigned testCommitmentSignedLarge
          , bench "decode" $
              nf decodeCommitmentSigned encodedCommitmentSignedLarge
          ]
      , bgroup "commitment_signed_483"
          [ bench "encode" $
              nf encodeCommitmentSigned testCommitmentSignedMax
          , bench "decode" $
              nf decodeCommitmentSigned encodedCommitmentSignedMax
          ]
      ]
  ]
