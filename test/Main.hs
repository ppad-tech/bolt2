{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Lightning.Protocol.BOLT1 (TlvStream, unsafeTlvStream)
import Lightning.Protocol.BOLT2
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt2" [
    v1_establishment_tests
  , v2_establishment_tests
  , close_tests
  , normal_operation_tests
  , reestablish_tests
  , error_tests
  , property_tests
  ]

-- Test data helpers -----------------------------------------------------------

-- | Create a valid ChannelId (32 bytes).
testChannelId :: ChannelId
testChannelId = fromJust $ channelId (BS.replicate 32 0xab)

-- | Create a valid ChainHash (32 bytes).
testChainHash :: ChainHash
testChainHash = fromJust $ chainHash (BS.replicate 32 0x01)

-- | Create a valid Point (33 bytes).
testPoint :: Point
testPoint = fromJust $ point (BS.pack $ 0x02 : replicate 32 0xff)

-- | Create a second valid Point (33 bytes).
testPoint2 :: Point
testPoint2 = fromJust $ point (BS.pack $ 0x03 : replicate 32 0xee)

-- | Create a valid Signature (64 bytes).
testSignature :: Signature
testSignature = fromJust $ signature (BS.replicate 64 0xcc)

-- | Create a valid TxId (32 bytes).
testTxId :: TxId
testTxId = fromJust $ mkTxId (BS.replicate 32 0xdd)

-- | Create a valid PaymentHash (32 bytes).
testPaymentHash :: PaymentHash
testPaymentHash = fromJust $ paymentHash (BS.replicate 32 0xaa)

-- | Create a valid PaymentPreimage (32 bytes).
testPaymentPreimage :: PaymentPreimage
testPaymentPreimage = fromJust $ paymentPreimage (BS.replicate 32 0xbb)

-- | Create a valid OnionPacket (1366 bytes).
testOnionPacket :: OnionPacket
testOnionPacket = fromJust $ onionPacket (BS.replicate 1366 0x00)

-- | Create a valid PerCommitmentSecret (32 bytes).
testSecret :: PerCommitmentSecret
testSecret = fromJust $
  perCommitmentSecret (BS.replicate 32 0x11)

-- | Empty TLV stream for messages.
emptyTlvs :: TlvStream
emptyTlvs = unsafeTlvStream []

-- V1 Channel Establishment Tests ----------------------------------------------

v1_establishment_tests :: TestTree
v1_establishment_tests = testGroup "V1 Channel Establishment" [
    testGroup "OpenChannel" [
      testCase "encode/decode roundtrip" $ do
        let msg = OpenChannel
              { openChannelChainHash = testChainHash
              , openChannelTempChannelId = testChannelId
              , openChannelFundingSatoshi = Satoshi 1000000
              , openChannelPushMsat = MilliSatoshi 500000
              , openChannelDustLimitSatoshi = Satoshi 546
              , openChannelMaxHtlcValueInFlight = MilliSatoshi 100000000
              , openChannelChannelReserveSat = Satoshi 10000
              , openChannelHtlcMinimumMsat = MilliSatoshi 1000
              , openChannelFeeratePerKw = 2500
              , openChannelToSelfDelay = 144
              , openChannelMaxAcceptedHtlcs = 483
              , openChannelFundingPubkey = testPoint
              , openChannelRevocationBasepoint = testPoint
              , openChannelPaymentBasepoint = testPoint
              , openChannelDelayedPaymentBase = testPoint
              , openChannelHtlcBasepoint = testPoint
              , openChannelFirstPerCommitPoint = testPoint
              , openChannelChannelFlags = 0x01
              , openChannelTlvs = emptyTlvs
              }
            encoded = encodeOpenChannel msg
        case decodeOpenChannel encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "AcceptChannel" [
      testCase "encode/decode roundtrip" $ do
        let msg = AcceptChannel
              { acceptChannelTempChannelId = testChannelId
              , acceptChannelDustLimitSatoshi = Satoshi 546
              , acceptChannelMaxHtlcValueInFlight = MilliSatoshi 100000000
              , acceptChannelChannelReserveSat = Satoshi 10000
              , acceptChannelHtlcMinimumMsat = MilliSatoshi 1000
              , acceptChannelMinimumDepth = 3
              , acceptChannelToSelfDelay = 144
              , acceptChannelMaxAcceptedHtlcs = 483
              , acceptChannelFundingPubkey = testPoint
              , acceptChannelRevocationBasepoint = testPoint
              , acceptChannelPaymentBasepoint = testPoint
              , acceptChannelDelayedPaymentBase = testPoint
              , acceptChannelHtlcBasepoint = testPoint
              , acceptChannelFirstPerCommitPoint = testPoint
              , acceptChannelTlvs = emptyTlvs
              }
            encoded = encodeAcceptChannel msg
        case decodeAcceptChannel encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "FundingCreated" [
      testCase "encode/decode roundtrip" $ do
        let msg = FundingCreated
              { fundingCreatedTempChannelId = testChannelId
              , fundingCreatedFundingTxid = testTxId
              , fundingCreatedFundingOutIdx = 0
              , fundingCreatedSignature = testSignature
              }
            encoded = encodeFundingCreated msg
        case decodeFundingCreated encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "roundtrip with non-zero output index" $ do
        let msg = FundingCreated
              { fundingCreatedTempChannelId = testChannelId
              , fundingCreatedFundingTxid = testTxId
              , fundingCreatedFundingOutIdx = 42
              , fundingCreatedSignature = testSignature
              }
            encoded = encodeFundingCreated msg
        case decodeFundingCreated encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "FundingSigned" [
      testCase "encode/decode roundtrip" $ do
        let msg = FundingSigned
              { fundingSignedChannelId = testChannelId
              , fundingSignedSignature = testSignature
              }
            encoded = encodeFundingSigned msg
        case decodeFundingSigned encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "ChannelReady" [
      testCase "encode/decode roundtrip" $ do
        let msg = ChannelReady
              { channelReadyChannelId = testChannelId
              , channelReadySecondPerCommitPoint = testPoint
              , channelReadyTlvs = emptyTlvs
              }
            encoded = encodeChannelReady msg
        case decodeChannelReady encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  ]

-- V2 Channel Establishment (Interactive-tx) Tests ----------------------------

v2_establishment_tests :: TestTree
v2_establishment_tests = testGroup "V2 Channel Establishment" [
    testGroup "OpenChannel2" [
      testCase "encode/decode roundtrip" $ do
        let msg = OpenChannel2
              { openChannel2ChainHash = testChainHash
              , openChannel2TempChannelId = testChannelId
              , openChannel2FundingFeeratePerkw = 2500
              , openChannel2CommitFeeratePerkw = 2000
              , openChannel2FundingSatoshi = Satoshi 1000000
              , openChannel2DustLimitSatoshi = Satoshi 546
              , openChannel2MaxHtlcValueInFlight = MilliSatoshi 100000000
              , openChannel2HtlcMinimumMsat = MilliSatoshi 1000
              , openChannel2ToSelfDelay = 144
              , openChannel2MaxAcceptedHtlcs = 483
              , openChannel2Locktime = 0
              , openChannel2FundingPubkey = testPoint
              , openChannel2RevocationBasepoint = testPoint
              , openChannel2PaymentBasepoint = testPoint
              , openChannel2DelayedPaymentBase = testPoint
              , openChannel2HtlcBasepoint = testPoint
              , openChannel2FirstPerCommitPoint = testPoint
              , openChannel2SecondPerCommitPoint = testPoint2
              , openChannel2ChannelFlags = 0x00
              , openChannel2Tlvs = emptyTlvs
              }
            encoded = encodeOpenChannel2 msg
        case decodeOpenChannel2 encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "AcceptChannel2" [
      testCase "encode/decode roundtrip" $ do
        let msg = AcceptChannel2
              { acceptChannel2TempChannelId = testChannelId
              , acceptChannel2FundingSatoshi = Satoshi 500000
              , acceptChannel2DustLimitSatoshi = Satoshi 546
              , acceptChannel2MaxHtlcValueInFlight = MilliSatoshi 100000000
              , acceptChannel2HtlcMinimumMsat = MilliSatoshi 1000
              , acceptChannel2MinimumDepth = 3
              , acceptChannel2ToSelfDelay = 144
              , acceptChannel2MaxAcceptedHtlcs = 483
              , acceptChannel2FundingPubkey = testPoint
              , acceptChannel2RevocationBasepoint = testPoint
              , acceptChannel2PaymentBasepoint = testPoint
              , acceptChannel2DelayedPaymentBase = testPoint
              , acceptChannel2HtlcBasepoint = testPoint
              , acceptChannel2FirstPerCommitPoint = testPoint
              , acceptChannel2SecondPerCommitPoint = testPoint2
              , acceptChannel2Tlvs = emptyTlvs
              }
            encoded = encodeAcceptChannel2 msg
        case decodeAcceptChannel2 encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxAddInput" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxAddInput
              { txAddInputChannelId = testChannelId
              , txAddInputSerialId = 12345
              , txAddInputPrevTx = BS.pack [0x01, 0x02, 0x03, 0x04]
              , txAddInputPrevVout = 0
              , txAddInputSequence = 0xfffffffe
              }
        case encodeTxAddInput msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxAddInput encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "roundtrip with empty prevTx" $ do
        let msg = TxAddInput
              { txAddInputChannelId = testChannelId
              , txAddInputSerialId = 0
              , txAddInputPrevTx = BS.empty
              , txAddInputPrevVout = 0
              , txAddInputSequence = 0
              }
        case encodeTxAddInput msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxAddInput encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxAddOutput" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxAddOutput
              { txAddOutputChannelId = testChannelId
              , txAddOutputSerialId = 54321
              , txAddOutputSats = Satoshi 100000
              , txAddOutputScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                                  BS.replicate 20 0xaa)
              }
        case encodeTxAddOutput msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxAddOutput encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxRemoveInput" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxRemoveInput
              { txRemoveInputChannelId = testChannelId
              , txRemoveInputSerialId = 12345
              }
            encoded = encodeTxRemoveInput msg
        case decodeTxRemoveInput encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxRemoveOutput" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxRemoveOutput
              { txRemoveOutputChannelId = testChannelId
              , txRemoveOutputSerialId = 54321
              }
            encoded = encodeTxRemoveOutput msg
        case decodeTxRemoveOutput encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxComplete" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxComplete { txCompleteChannelId = testChannelId }
            encoded = encodeTxComplete msg
        case decodeTxComplete encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxSignatures" [
      testCase "encode/decode with no witnesses" $ do
        let msg = TxSignatures
              { txSignaturesChannelId = testChannelId
              , txSignaturesTxid = testTxId
              , txSignaturesWitnesses = []
              }
        case encodeTxSignatures msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxSignatures encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "encode/decode with multiple witnesses" $ do
        let w1 = Witness (BS.pack [0x30, 0x44] <> BS.replicate 68 0xaa)
            w2 = Witness (BS.pack [0x02] <> BS.replicate 32 0xbb)
            msg = TxSignatures
              { txSignaturesChannelId = testChannelId
              , txSignaturesTxid = testTxId
              , txSignaturesWitnesses = [w1, w2]
              }
        case encodeTxSignatures msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxSignatures encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxInitRbf" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxInitRbf
              { txInitRbfChannelId = testChannelId
              , txInitRbfLocktime = 800000
              , txInitRbfFeerate = 3000
              , txInitRbfTlvs = emptyTlvs
              }
            encoded = encodeTxInitRbf msg
        case decodeTxInitRbf encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxAckRbf" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxAckRbf
              { txAckRbfChannelId = testChannelId
              , txAckRbfTlvs = emptyTlvs
              }
            encoded = encodeTxAckRbf msg
        case decodeTxAckRbf encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "TxAbort" [
      testCase "encode/decode roundtrip" $ do
        let msg = TxAbort
              { txAbortChannelId = testChannelId
              , txAbortData = "transaction abort reason"
              }
        case encodeTxAbort msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxAbort encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "roundtrip with empty data" $ do
        let msg = TxAbort
              { txAbortChannelId = testChannelId
              , txAbortData = BS.empty
              }
        case encodeTxAbort msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeTxAbort encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  ]

-- Channel Close Tests ---------------------------------------------------------

close_tests :: TestTree
close_tests = testGroup "Channel Close" [
    testGroup "Stfu" [
      testCase "encode/decode initiator=1" $ do
        let msg = Stfu
              { stfuChannelId = testChannelId
              , stfuInitiator = 1
              }
            encoded = encodeStfu msg
        case decodeStfu encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "encode/decode initiator=0" $ do
        let msg = Stfu
              { stfuChannelId = testChannelId
              , stfuInitiator = 0
              }
            encoded = encodeStfu msg
        case decodeStfu encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "Shutdown" [
      testCase "encode/decode with P2WPKH script" $ do
        let script = scriptPubKey (BS.pack [0x00, 0x14] <>
                                   BS.replicate 20 0xaa)
            msg = Shutdown
              { shutdownChannelId = testChannelId
              , shutdownScriptPubkey = script
              }
        case encodeShutdown msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeShutdown encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "encode/decode with P2WSH script" $ do
        let script = scriptPubKey (BS.pack [0x00, 0x20] <>
                                   BS.replicate 32 0xbb)
            msg = Shutdown
              { shutdownChannelId = testChannelId
              , shutdownScriptPubkey = script
              }
        case encodeShutdown msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeShutdown encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "ClosingSigned" [
      testCase "encode/decode roundtrip" $ do
        let msg = ClosingSigned
              { closingSignedChannelId = testChannelId
              , closingSignedFeeSatoshi = Satoshi 1000
              , closingSignedSignature = testSignature
              , closingSignedTlvs = emptyTlvs
              }
            encoded = encodeClosingSigned msg
        case decodeClosingSigned encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "ClosingComplete" [
      testCase "encode/decode roundtrip" $ do
        let closerScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                         BS.replicate 20 0xcc)
            closeeScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                         BS.replicate 20 0xdd)
            msg = ClosingComplete
              { closingCompleteChannelId = testChannelId
              , closingCompleteCloserScript = closerScript
              , closingCompleteCloseeScript = closeeScript
              , closingCompleteFeeSatoshi = Satoshi 500
              , closingCompleteLocktime = 0
              , closingCompleteTlvs = emptyTlvs
              }
        case encodeClosingComplete msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeClosingComplete encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "ClosingSig" [
      testCase "encode/decode roundtrip" $ do
        let closerScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                         BS.replicate 20 0xee)
            closeeScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                         BS.replicate 20 0xff)
            msg = ClosingSig
              { closingSigChannelId = testChannelId
              , closingSigCloserScript = closerScript
              , closingSigCloseeScript = closeeScript
              , closingSigFeeSatoshi = Satoshi 500
              , closingSigLocktime = 100
              , closingSigTlvs = emptyTlvs
              }
        case encodeClosingSig msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeClosingSig encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  ]

-- Normal Operation Tests ------------------------------------------------------

normal_operation_tests :: TestTree
normal_operation_tests = testGroup "Normal Operation" [
    testGroup "UpdateAddHtlc" [
      testCase "encode/decode roundtrip" $ do
        let msg = UpdateAddHtlc
              { updateAddHtlcChannelId = testChannelId
              , updateAddHtlcId = 0
              , updateAddHtlcAmountMsat = MilliSatoshi 10000000
              , updateAddHtlcPaymentHash = testPaymentHash
              , updateAddHtlcCltvExpiry = 800144
              , updateAddHtlcOnionPacket = testOnionPacket
              , updateAddHtlcTlvs = emptyTlvs
              }
            encoded = encodeUpdateAddHtlc msg
        case decodeUpdateAddHtlc encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "UpdateFulfillHtlc" [
      testCase "encode/decode roundtrip" $ do
        let msg = UpdateFulfillHtlc
              { updateFulfillHtlcChannelId = testChannelId
              , updateFulfillHtlcId = 42
              , updateFulfillHtlcPaymentPreimage = testPaymentPreimage
              , updateFulfillHtlcTlvs = emptyTlvs
              }
            encoded = encodeUpdateFulfillHtlc msg
        case decodeUpdateFulfillHtlc encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "UpdateFailHtlc" [
      testCase "encode/decode roundtrip" $ do
        let msg = UpdateFailHtlc
              { updateFailHtlcChannelId = testChannelId
              , updateFailHtlcId = 42
              , updateFailHtlcReason = BS.replicate 32 0xaa
              , updateFailHtlcTlvs = emptyTlvs
              }
        case encodeUpdateFailHtlc msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeUpdateFailHtlc encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "roundtrip with empty reason" $ do
        let msg = UpdateFailHtlc
              { updateFailHtlcChannelId = testChannelId
              , updateFailHtlcId = 0
              , updateFailHtlcReason = BS.empty
              , updateFailHtlcTlvs = emptyTlvs
              }
        case encodeUpdateFailHtlc msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeUpdateFailHtlc encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "UpdateFailMalformedHtlc" [
      testCase "encode/decode roundtrip" $ do
        let msg = UpdateFailMalformedHtlc
              { updateFailMalformedHtlcChannelId = testChannelId
              , updateFailMalformedHtlcId = 42
              , updateFailMalformedHtlcSha256Onion = testPaymentHash
              , updateFailMalformedHtlcFailureCode = 0x8002
              }
            encoded = encodeUpdateFailMalformedHtlc msg
        case decodeUpdateFailMalformedHtlc encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "CommitmentSigned" [
      testCase "encode/decode with no HTLC signatures" $ do
        let msg = CommitmentSigned
              { commitmentSignedChannelId = testChannelId
              , commitmentSignedSignature = testSignature
              , commitmentSignedHtlcSignatures = []
              }
        case encodeCommitmentSigned msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeCommitmentSigned encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    , testCase "encode/decode with HTLC signatures" $ do
        let sig2 = fromJust $ signature (BS.replicate 64 0xdd)
            sig3 = fromJust $ signature (BS.replicate 64 0xee)
            msg = CommitmentSigned
              { commitmentSignedChannelId = testChannelId
              , commitmentSignedSignature = testSignature
              , commitmentSignedHtlcSignatures = [sig2, sig3]
              }
        case encodeCommitmentSigned msg of
          Left e -> assertFailure $ "encode failed: " ++ show e
          Right encoded -> case decodeCommitmentSigned encoded of
            Right (decoded, _) -> decoded @?= msg
            Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "RevokeAndAck" [
      testCase "encode/decode roundtrip" $ do
        let msg = RevokeAndAck
              { revokeAndAckChannelId = testChannelId
              , revokeAndAckPerCommitmentSecret = testSecret
              , revokeAndAckNextPerCommitPoint = testPoint
              }
            encoded = encodeRevokeAndAck msg
        case decodeRevokeAndAck encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  , testGroup "UpdateFee" [
      testCase "encode/decode roundtrip" $ do
        let msg = UpdateFee
              { updateFeeChannelId = testChannelId
              , updateFeeFeeratePerKw = 5000
              }
            encoded = encodeUpdateFee msg
        case decodeUpdateFee encoded of
          Right (decoded, _) -> decoded @?= msg
          Left e -> assertFailure $ "decode failed: " ++ show e
    ]
  ]

-- Reestablish Tests -----------------------------------------------------------

reestablish_tests :: TestTree
reestablish_tests = testGroup "Channel Reestablish" [
    testCase "encode/decode roundtrip" $ do
      let sec = fromJust $ perCommitmentSecret (BS.replicate 32 0x22)
          msg = ChannelReestablish
            { channelReestablishChannelId = testChannelId
            , channelReestablishNextCommitNum = 5
            , channelReestablishNextRevocationNum = 4
            , channelReestablishYourLastCommitSecret = sec
            , channelReestablishMyCurrentCommitPoint = testPoint
            , channelReestablishTlvs = emptyTlvs
            }
          encoded = encodeChannelReestablish msg
      case decodeChannelReestablish encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  , testCase "roundtrip with zero counters" $ do
      let sec = fromJust $ perCommitmentSecret (BS.replicate 32 0x00)
          msg = ChannelReestablish
            { channelReestablishChannelId = testChannelId
            , channelReestablishNextCommitNum = 1
            , channelReestablishNextRevocationNum = 0
            , channelReestablishYourLastCommitSecret = sec
            , channelReestablishMyCurrentCommitPoint = testPoint
            , channelReestablishTlvs = emptyTlvs
            }
          encoded = encodeChannelReestablish msg
      case decodeChannelReestablish encoded of
        Right (decoded, _) -> decoded @?= msg
        Left e -> assertFailure $ "decode failed: " ++ show e
  ]

-- Error Condition Tests -------------------------------------------------------

error_tests :: TestTree
error_tests = testGroup "Error Conditions" [
    testGroup "Insufficient Bytes" [
      testCase "decodeOpenChannel empty" $ do
        case decodeOpenChannel BS.empty of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeOpenChannel too short" $ do
        case decodeOpenChannel (BS.replicate 100 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeAcceptChannel too short" $ do
        case decodeAcceptChannel (BS.replicate 10 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeFundingCreated too short" $ do
        case decodeFundingCreated (BS.replicate 50 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeFundingSigned too short" $ do
        case decodeFundingSigned (BS.replicate 30 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeChannelReady too short" $ do
        case decodeChannelReady (BS.replicate 32 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeStfu too short" $ do
        case decodeStfu (BS.replicate 31 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeShutdown too short" $ do
        case decodeShutdown (BS.replicate 32 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeUpdateAddHtlc too short" $ do
        case decodeUpdateAddHtlc (BS.replicate 100 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeCommitmentSigned too short" $ do
        case decodeCommitmentSigned (BS.replicate 90 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeRevokeAndAck too short" $ do
        case decodeRevokeAndAck (BS.replicate 60 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeTxSignatures too short" $ do
        case decodeTxSignatures (BS.replicate 60 0x00) of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    ]
  , testGroup "EncodeError - Length Overflow" [
      testCase "encodeShutdown with oversized script" $ do
        let script = scriptPubKey (BS.replicate 70000 0x00)
            msg = Shutdown
              { shutdownChannelId = testChannelId
              , shutdownScriptPubkey = script
              }
        case encodeShutdown msg of
          Left EncodeLengthOverflow -> pure ()
          other -> assertFailure $ "expected overflow: " ++ show other
    , testCase "encodeClosingComplete with oversized closer script" $ do
        let oversizedScript = scriptPubKey (BS.replicate 70000 0x00)
            normalScript = scriptPubKey (BS.replicate 22 0x00)
            msg = ClosingComplete
              { closingCompleteChannelId = testChannelId
              , closingCompleteCloserScript = oversizedScript
              , closingCompleteCloseeScript = normalScript
              , closingCompleteFeeSatoshi = Satoshi 500
              , closingCompleteLocktime = 0
              , closingCompleteTlvs = emptyTlvs
              }
        case encodeClosingComplete msg of
          Left EncodeLengthOverflow -> pure ()
          other -> assertFailure $ "expected overflow: " ++ show other
    , testCase "encodeClosingComplete with oversized closee script" $ do
        let normalScript = scriptPubKey (BS.replicate 22 0x00)
            oversizedScript = scriptPubKey (BS.replicate 70000 0x00)
            msg = ClosingComplete
              { closingCompleteChannelId = testChannelId
              , closingCompleteCloserScript = normalScript
              , closingCompleteCloseeScript = oversizedScript
              , closingCompleteFeeSatoshi = Satoshi 500
              , closingCompleteLocktime = 0
              , closingCompleteTlvs = emptyTlvs
              }
        case encodeClosingComplete msg of
          Left EncodeLengthOverflow -> pure ()
          other -> assertFailure $ "expected overflow: " ++ show other
    , testCase "encodeClosingSig with oversized script" $ do
        let oversizedScript = scriptPubKey (BS.replicate 70000 0x00)
            normalScript = scriptPubKey (BS.replicate 22 0x00)
            msg = ClosingSig
              { closingSigChannelId = testChannelId
              , closingSigCloserScript = oversizedScript
              , closingSigCloseeScript = normalScript
              , closingSigFeeSatoshi = Satoshi 500
              , closingSigLocktime = 0
              , closingSigTlvs = emptyTlvs
              }
        case encodeClosingSig msg of
          Left EncodeLengthOverflow -> pure ()
          other -> assertFailure $ "expected overflow: " ++ show other
    ]
  , testGroup "Invalid Field Length" [
      testCase "decodeShutdown with invalid script length" $ do
        -- channel_id (32 bytes) + script length (2 bytes) claiming more
        let encoded = BS.replicate 32 0xab <>
                      BS.pack [0xff, 0xff] <>  -- claims 65535 bytes
                      BS.replicate 10 0x00     -- only 10 bytes
        case decodeShutdown encoded of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeTxAddInput with invalid prevTx length" $ do
        -- channel_id (32) + serial_id (8) + len (2) claiming more
        let encoded = BS.replicate 32 0xab <>
                      BS.replicate 8 0x00 <>
                      BS.pack [0xff, 0xff] <>  -- claims 65535 bytes
                      BS.replicate 10 0x00     -- only 10 bytes
        case decodeTxAddInput encoded of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    , testCase "decodeUpdateFailHtlc with invalid reason length" $ do
        -- channel_id (32) + htlc_id (8) + len (2) claiming more
        let encoded = BS.replicate 32 0xab <>
                      BS.replicate 8 0x00 <>
                      BS.pack [0xff, 0xff] <>  -- claims 65535 bytes
                      BS.replicate 10 0x00     -- only 10 bytes
        case decodeUpdateFailHtlc encoded of
          Left DecodeInsufficientBytes -> pure ()
          other -> assertFailure $ "expected insufficient: " ++ show other
    ]
  ]

-- Property Tests --------------------------------------------------------------

property_tests :: TestTree
property_tests = testGroup "Properties" [
    testProperty "OpenChannel roundtrip" propOpenChannelRoundtrip
  , testProperty "AcceptChannel roundtrip" propAcceptChannelRoundtrip
  , testProperty "FundingCreated roundtrip" propFundingCreatedRoundtrip
  , testProperty "FundingSigned roundtrip" propFundingSignedRoundtrip
  , testProperty "ChannelReady roundtrip" propChannelReadyRoundtrip
  , testProperty "OpenChannel2 roundtrip" propOpenChannel2Roundtrip
  , testProperty "AcceptChannel2 roundtrip" propAcceptChannel2Roundtrip
  , testProperty "TxAddInput roundtrip" propTxAddInputRoundtrip
  , testProperty "TxAddOutput roundtrip" propTxAddOutputRoundtrip
  , testProperty "TxRemoveInput roundtrip" propTxRemoveInputRoundtrip
  , testProperty "TxRemoveOutput roundtrip" propTxRemoveOutputRoundtrip
  , testProperty "TxComplete roundtrip" propTxCompleteRoundtrip
  , testProperty "TxSignatures roundtrip" propTxSignaturesRoundtrip
  , testProperty "TxInitRbf roundtrip" propTxInitRbfRoundtrip
  , testProperty "TxAckRbf roundtrip" propTxAckRbfRoundtrip
  , testProperty "TxAbort roundtrip" propTxAbortRoundtrip
  , testProperty "Stfu roundtrip" propStfuRoundtrip
  , testProperty "Shutdown roundtrip" propShutdownRoundtrip
  , testProperty "ClosingSigned roundtrip" propClosingSignedRoundtrip
  , testProperty "ClosingComplete roundtrip" propClosingCompleteRoundtrip
  , testProperty "ClosingSig roundtrip" propClosingSigRoundtrip
  , testProperty "UpdateAddHtlc roundtrip" propUpdateAddHtlcRoundtrip
  , testProperty "UpdateFulfillHtlc roundtrip" propUpdateFulfillHtlcRoundtrip
  , testProperty "UpdateFailHtlc roundtrip" propUpdateFailHtlcRoundtrip
  , testProperty "UpdateFailMalformedHtlc roundtrip"
      propUpdateFailMalformedHtlcRoundtrip
  , testProperty "CommitmentSigned roundtrip" propCommitmentSignedRoundtrip
  , testProperty "RevokeAndAck roundtrip" propRevokeAndAckRoundtrip
  , testProperty "UpdateFee roundtrip" propUpdateFeeRoundtrip
  , testProperty "ChannelReestablish roundtrip" propChannelReestablishRoundtrip
  ]

-- Property: OpenChannel roundtrip
propOpenChannelRoundtrip :: Property
propOpenChannelRoundtrip = property $ do
  let msg = OpenChannel
        { openChannelChainHash = testChainHash
        , openChannelTempChannelId = testChannelId
        , openChannelFundingSatoshi = Satoshi 1000000
        , openChannelPushMsat = MilliSatoshi 500000
        , openChannelDustLimitSatoshi = Satoshi 546
        , openChannelMaxHtlcValueInFlight = MilliSatoshi 100000000
        , openChannelChannelReserveSat = Satoshi 10000
        , openChannelHtlcMinimumMsat = MilliSatoshi 1000
        , openChannelFeeratePerKw = 2500
        , openChannelToSelfDelay = 144
        , openChannelMaxAcceptedHtlcs = 483
        , openChannelFundingPubkey = testPoint
        , openChannelRevocationBasepoint = testPoint
        , openChannelPaymentBasepoint = testPoint
        , openChannelDelayedPaymentBase = testPoint
        , openChannelHtlcBasepoint = testPoint
        , openChannelFirstPerCommitPoint = testPoint
        , openChannelChannelFlags = 0x01
        , openChannelTlvs = emptyTlvs
        }
      encoded = encodeOpenChannel msg
  case decodeOpenChannel encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: AcceptChannel roundtrip
propAcceptChannelRoundtrip :: Property
propAcceptChannelRoundtrip = property $ do
  let msg = AcceptChannel
        { acceptChannelTempChannelId = testChannelId
        , acceptChannelDustLimitSatoshi = Satoshi 546
        , acceptChannelMaxHtlcValueInFlight = MilliSatoshi 100000000
        , acceptChannelChannelReserveSat = Satoshi 10000
        , acceptChannelHtlcMinimumMsat = MilliSatoshi 1000
        , acceptChannelMinimumDepth = 3
        , acceptChannelToSelfDelay = 144
        , acceptChannelMaxAcceptedHtlcs = 483
        , acceptChannelFundingPubkey = testPoint
        , acceptChannelRevocationBasepoint = testPoint
        , acceptChannelPaymentBasepoint = testPoint
        , acceptChannelDelayedPaymentBase = testPoint
        , acceptChannelHtlcBasepoint = testPoint
        , acceptChannelFirstPerCommitPoint = testPoint
        , acceptChannelTlvs = emptyTlvs
        }
      encoded = encodeAcceptChannel msg
  case decodeAcceptChannel encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: FundingCreated roundtrip
propFundingCreatedRoundtrip :: Word16 -> Property
propFundingCreatedRoundtrip outIdx = property $ do
  let msg = FundingCreated
        { fundingCreatedTempChannelId = testChannelId
        , fundingCreatedFundingTxid = testTxId
        , fundingCreatedFundingOutIdx = outIdx
        , fundingCreatedSignature = testSignature
        }
      encoded = encodeFundingCreated msg
  case decodeFundingCreated encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: FundingSigned roundtrip
propFundingSignedRoundtrip :: Property
propFundingSignedRoundtrip = property $ do
  let msg = FundingSigned
        { fundingSignedChannelId = testChannelId
        , fundingSignedSignature = testSignature
        }
      encoded = encodeFundingSigned msg
  case decodeFundingSigned encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: ChannelReady roundtrip
propChannelReadyRoundtrip :: Property
propChannelReadyRoundtrip = property $ do
  let msg = ChannelReady
        { channelReadyChannelId = testChannelId
        , channelReadySecondPerCommitPoint = testPoint
        , channelReadyTlvs = emptyTlvs
        }
      encoded = encodeChannelReady msg
  case decodeChannelReady encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: OpenChannel2 roundtrip
propOpenChannel2Roundtrip :: Property
propOpenChannel2Roundtrip = property $ do
  let msg = OpenChannel2
        { openChannel2ChainHash = testChainHash
        , openChannel2TempChannelId = testChannelId
        , openChannel2FundingFeeratePerkw = 2500
        , openChannel2CommitFeeratePerkw = 2000
        , openChannel2FundingSatoshi = Satoshi 1000000
        , openChannel2DustLimitSatoshi = Satoshi 546
        , openChannel2MaxHtlcValueInFlight = MilliSatoshi 100000000
        , openChannel2HtlcMinimumMsat = MilliSatoshi 1000
        , openChannel2ToSelfDelay = 144
        , openChannel2MaxAcceptedHtlcs = 483
        , openChannel2Locktime = 0
        , openChannel2FundingPubkey = testPoint
        , openChannel2RevocationBasepoint = testPoint
        , openChannel2PaymentBasepoint = testPoint
        , openChannel2DelayedPaymentBase = testPoint
        , openChannel2HtlcBasepoint = testPoint
        , openChannel2FirstPerCommitPoint = testPoint
        , openChannel2SecondPerCommitPoint = testPoint2
        , openChannel2ChannelFlags = 0x00
        , openChannel2Tlvs = emptyTlvs
        }
      encoded = encodeOpenChannel2 msg
  case decodeOpenChannel2 encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: AcceptChannel2 roundtrip
propAcceptChannel2Roundtrip :: Property
propAcceptChannel2Roundtrip = property $ do
  let msg = AcceptChannel2
        { acceptChannel2TempChannelId = testChannelId
        , acceptChannel2FundingSatoshi = Satoshi 500000
        , acceptChannel2DustLimitSatoshi = Satoshi 546
        , acceptChannel2MaxHtlcValueInFlight = MilliSatoshi 100000000
        , acceptChannel2HtlcMinimumMsat = MilliSatoshi 1000
        , acceptChannel2MinimumDepth = 3
        , acceptChannel2ToSelfDelay = 144
        , acceptChannel2MaxAcceptedHtlcs = 483
        , acceptChannel2FundingPubkey = testPoint
        , acceptChannel2RevocationBasepoint = testPoint
        , acceptChannel2PaymentBasepoint = testPoint
        , acceptChannel2DelayedPaymentBase = testPoint
        , acceptChannel2HtlcBasepoint = testPoint
        , acceptChannel2FirstPerCommitPoint = testPoint
        , acceptChannel2SecondPerCommitPoint = testPoint2
        , acceptChannel2Tlvs = emptyTlvs
        }
      encoded = encodeAcceptChannel2 msg
  case decodeAcceptChannel2 encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: TxAddInput roundtrip with varying data
propTxAddInputRoundtrip :: [Word8] -> Word32 -> Word32 -> Property
propTxAddInputRoundtrip prevTxBytes vout seqNum = property $ do
  let prevTx = BS.pack (take 1000 prevTxBytes)  -- limit size
      msg = TxAddInput
        { txAddInputChannelId = testChannelId
        , txAddInputSerialId = 12345
        , txAddInputPrevTx = prevTx
        , txAddInputPrevVout = vout
        , txAddInputSequence = seqNum
        }
  case encodeTxAddInput msg of
    Left _ -> False
    Right encoded -> case decodeTxAddInput encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: TxAddOutput roundtrip
propTxAddOutputRoundtrip :: Word64 -> [Word8] -> Property
propTxAddOutputRoundtrip sats scriptBytes = property $ do
  let script = scriptPubKey (BS.pack (take 100 scriptBytes))
      msg = TxAddOutput
        { txAddOutputChannelId = testChannelId
        , txAddOutputSerialId = 54321
        , txAddOutputSats = Satoshi sats
        , txAddOutputScript = script
        }
  case encodeTxAddOutput msg of
    Left _ -> False
    Right encoded -> case decodeTxAddOutput encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: TxRemoveInput roundtrip
propTxRemoveInputRoundtrip :: Word64 -> Property
propTxRemoveInputRoundtrip serialId = property $ do
  let msg = TxRemoveInput
        { txRemoveInputChannelId = testChannelId
        , txRemoveInputSerialId = serialId
        }
      encoded = encodeTxRemoveInput msg
  case decodeTxRemoveInput encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: TxRemoveOutput roundtrip
propTxRemoveOutputRoundtrip :: Word64 -> Property
propTxRemoveOutputRoundtrip serialId = property $ do
  let msg = TxRemoveOutput
        { txRemoveOutputChannelId = testChannelId
        , txRemoveOutputSerialId = serialId
        }
      encoded = encodeTxRemoveOutput msg
  case decodeTxRemoveOutput encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: TxComplete roundtrip
propTxCompleteRoundtrip :: Property
propTxCompleteRoundtrip = property $ do
  let msg = TxComplete { txCompleteChannelId = testChannelId }
      encoded = encodeTxComplete msg
  case decodeTxComplete encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: TxSignatures roundtrip with varying witnesses
propTxSignaturesRoundtrip :: [[Word8]] -> Property
propTxSignaturesRoundtrip witnessList = property $ do
  let wits = map (Witness . BS.pack . take 200) (take 10 witnessList)
      msg = TxSignatures
        { txSignaturesChannelId = testChannelId
        , txSignaturesTxid = testTxId
        , txSignaturesWitnesses = wits
        }
  case encodeTxSignatures msg of
    Left _ -> False
    Right encoded -> case decodeTxSignatures encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: TxInitRbf roundtrip
propTxInitRbfRoundtrip :: Word32 -> Word32 -> Property
propTxInitRbfRoundtrip locktime feerate = property $ do
  let msg = TxInitRbf
        { txInitRbfChannelId = testChannelId
        , txInitRbfLocktime = locktime
        , txInitRbfFeerate = feerate
        , txInitRbfTlvs = emptyTlvs
        }
      encoded = encodeTxInitRbf msg
  case decodeTxInitRbf encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: TxAckRbf roundtrip
propTxAckRbfRoundtrip :: Property
propTxAckRbfRoundtrip = property $ do
  let msg = TxAckRbf
        { txAckRbfChannelId = testChannelId
        , txAckRbfTlvs = emptyTlvs
        }
      encoded = encodeTxAckRbf msg
  case decodeTxAckRbf encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: TxAbort roundtrip
propTxAbortRoundtrip :: [Word8] -> Property
propTxAbortRoundtrip dataBytes = property $ do
  let abortData = BS.pack (take 1000 dataBytes)
      msg = TxAbort
        { txAbortChannelId = testChannelId
        , txAbortData = abortData
        }
  case encodeTxAbort msg of
    Left _ -> False
    Right encoded -> case decodeTxAbort encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: Stfu roundtrip
propStfuRoundtrip :: Word8 -> Property
propStfuRoundtrip initiator = property $ do
  let msg = Stfu
        { stfuChannelId = testChannelId
        , stfuInitiator = initiator
        }
      encoded = encodeStfu msg
  case decodeStfu encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: Shutdown roundtrip
propShutdownRoundtrip :: [Word8] -> Property
propShutdownRoundtrip scriptBytes = property $ do
  let script = scriptPubKey (BS.pack (take 100 scriptBytes))
      msg = Shutdown
        { shutdownChannelId = testChannelId
        , shutdownScriptPubkey = script
        }
  case encodeShutdown msg of
    Left _ -> False
    Right encoded -> case decodeShutdown encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: ClosingSigned roundtrip
propClosingSignedRoundtrip :: Word64 -> Property
propClosingSignedRoundtrip feeSats = property $ do
  let msg = ClosingSigned
        { closingSignedChannelId = testChannelId
        , closingSignedFeeSatoshi = Satoshi feeSats
        , closingSignedSignature = testSignature
        , closingSignedTlvs = emptyTlvs
        }
      encoded = encodeClosingSigned msg
  case decodeClosingSigned encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: ClosingComplete roundtrip
propClosingCompleteRoundtrip :: Word64 -> Word32 -> Property
propClosingCompleteRoundtrip feeSats locktime = property $ do
  let closerScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                   BS.replicate 20 0xcc)
      closeeScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                   BS.replicate 20 0xdd)
      msg = ClosingComplete
        { closingCompleteChannelId = testChannelId
        , closingCompleteCloserScript = closerScript
        , closingCompleteCloseeScript = closeeScript
        , closingCompleteFeeSatoshi = Satoshi feeSats
        , closingCompleteLocktime = locktime
        , closingCompleteTlvs = emptyTlvs
        }
  case encodeClosingComplete msg of
    Left _ -> False
    Right encoded -> case decodeClosingComplete encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: ClosingSig roundtrip
propClosingSigRoundtrip :: Word64 -> Word32 -> Property
propClosingSigRoundtrip feeSats locktime = property $ do
  let closerScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                   BS.replicate 20 0xee)
      closeeScript = scriptPubKey (BS.pack [0x00, 0x14] <>
                                   BS.replicate 20 0xff)
      msg = ClosingSig
        { closingSigChannelId = testChannelId
        , closingSigCloserScript = closerScript
        , closingSigCloseeScript = closeeScript
        , closingSigFeeSatoshi = Satoshi feeSats
        , closingSigLocktime = locktime
        , closingSigTlvs = emptyTlvs
        }
  case encodeClosingSig msg of
    Left _ -> False
    Right encoded -> case decodeClosingSig encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: UpdateAddHtlc roundtrip
propUpdateAddHtlcRoundtrip :: Word64 -> Word64 -> Word32 -> Property
propUpdateAddHtlcRoundtrip htlcId amountMsat cltvExpiry = property $ do
  let msg = UpdateAddHtlc
        { updateAddHtlcChannelId = testChannelId
        , updateAddHtlcId = htlcId
        , updateAddHtlcAmountMsat = MilliSatoshi amountMsat
        , updateAddHtlcPaymentHash = testPaymentHash
        , updateAddHtlcCltvExpiry = cltvExpiry
        , updateAddHtlcOnionPacket = testOnionPacket
        , updateAddHtlcTlvs = emptyTlvs
        }
      encoded = encodeUpdateAddHtlc msg
  case decodeUpdateAddHtlc encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: UpdateFulfillHtlc roundtrip
propUpdateFulfillHtlcRoundtrip :: Word64 -> Property
propUpdateFulfillHtlcRoundtrip htlcId = property $ do
  let msg = UpdateFulfillHtlc
        { updateFulfillHtlcChannelId = testChannelId
        , updateFulfillHtlcId = htlcId
        , updateFulfillHtlcPaymentPreimage = testPaymentPreimage
        , updateFulfillHtlcTlvs = emptyTlvs
        }
      encoded = encodeUpdateFulfillHtlc msg
  case decodeUpdateFulfillHtlc encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: UpdateFailHtlc roundtrip
propUpdateFailHtlcRoundtrip :: Word64 -> [Word8] -> Property
propUpdateFailHtlcRoundtrip htlcId reasonBytes = property $ do
  let failReason = BS.pack (take 1000 reasonBytes)
      msg = UpdateFailHtlc
        { updateFailHtlcChannelId = testChannelId
        , updateFailHtlcId = htlcId
        , updateFailHtlcReason = failReason
        , updateFailHtlcTlvs = emptyTlvs
        }
  case encodeUpdateFailHtlc msg of
    Left _ -> False
    Right encoded -> case decodeUpdateFailHtlc encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: UpdateFailMalformedHtlc roundtrip
propUpdateFailMalformedHtlcRoundtrip :: Word64 -> Word16 -> Property
propUpdateFailMalformedHtlcRoundtrip htlcId failCode = property $ do
  let msg = UpdateFailMalformedHtlc
        { updateFailMalformedHtlcChannelId = testChannelId
        , updateFailMalformedHtlcId = htlcId
        , updateFailMalformedHtlcSha256Onion = testPaymentHash
        , updateFailMalformedHtlcFailureCode = failCode
        }
      encoded = encodeUpdateFailMalformedHtlc msg
  case decodeUpdateFailMalformedHtlc encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: CommitmentSigned roundtrip with varying HTLC count
propCommitmentSignedRoundtrip :: NonNegative Int -> Property
propCommitmentSignedRoundtrip (NonNegative n) = property $ do
  let numHtlcs = n `mod` 10  -- limit to 10 HTLCs for test speed
      htlcSigs = replicate numHtlcs testSignature
      msg = CommitmentSigned
        { commitmentSignedChannelId = testChannelId
        , commitmentSignedSignature = testSignature
        , commitmentSignedHtlcSignatures = htlcSigs
        }
  case encodeCommitmentSigned msg of
    Left _ -> False
    Right encoded -> case decodeCommitmentSigned encoded of
      Right (decoded, _) -> decoded == msg
      Left _ -> False

-- Property: RevokeAndAck roundtrip
propRevokeAndAckRoundtrip :: Property
propRevokeAndAckRoundtrip = property $ do
  let msg = RevokeAndAck
        { revokeAndAckChannelId = testChannelId
        , revokeAndAckPerCommitmentSecret = testSecret
        , revokeAndAckNextPerCommitPoint = testPoint
        }
      encoded = encodeRevokeAndAck msg
  case decodeRevokeAndAck encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: UpdateFee roundtrip
propUpdateFeeRoundtrip :: Word32 -> Property
propUpdateFeeRoundtrip feerate = property $ do
  let msg = UpdateFee
        { updateFeeChannelId = testChannelId
        , updateFeeFeeratePerKw = feerate
        }
      encoded = encodeUpdateFee msg
  case decodeUpdateFee encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Property: ChannelReestablish roundtrip
propChannelReestablishRoundtrip :: Word64 -> Word64 -> Property
propChannelReestablishRoundtrip nextCommit nextRevoke = property $ do
  let sec = fromJust $ perCommitmentSecret (BS.replicate 32 0x22)
      msg = ChannelReestablish
        { channelReestablishChannelId = testChannelId
        , channelReestablishNextCommitNum = nextCommit
        , channelReestablishNextRevocationNum = nextRevoke
        , channelReestablishYourLastCommitSecret = sec
        , channelReestablishMyCurrentCommitPoint = testPoint
        , channelReestablishTlvs = emptyTlvs
        }
      encoded = encodeChannelReestablish msg
  case decodeChannelReestablish encoded of
    Right (decoded, _) -> decoded == msg
    Left _ -> False

-- Helpers ---------------------------------------------------------------------

-- | Decode hex string. Returns Nothing on invalid hex.
unhex :: BS.ByteString -> Maybe BS.ByteString
unhex = B16.decode
