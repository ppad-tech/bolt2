{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Lightning.Protocol.BOLT2.Messages
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Message types for BOLT #2 peer protocol.
--
-- This module defines per-message record types and a top-level Message
-- sum type for all BOLT #2 messages.

module Lightning.Protocol.BOLT2.Messages (
  -- * Message type codes
    MsgType(..)
  , msgTypeWord

  -- * Top-level message type
  , Message(..)

  -- * Channel establishment v1
  , OpenChannel(..)
  , AcceptChannel(..)
  , FundingCreated(..)
  , FundingSigned(..)
  , ChannelReady(..)

  -- * Channel establishment v2
  , OpenChannel2(..)
  , AcceptChannel2(..)
  , TxAddInput(..)
  , TxAddOutput(..)
  , TxRemoveInput(..)
  , TxRemoveOutput(..)
  , TxComplete(..)
  , TxSignatures(..)
  , TxInitRbf(..)
  , TxAckRbf(..)
  , TxAbort(..)

  -- * Channel close
  , Stfu(..)
  , Shutdown(..)
  , ClosingSigned(..)
  , ClosingComplete(..)
  , ClosingSig(..)

  -- * Normal operation
  , UpdateAddHtlc(..)
  , UpdateFulfillHtlc(..)
  , UpdateFailHtlc(..)
  , UpdateFailMalformedHtlc(..)
  , CommitmentSigned(..)
  , RevokeAndAck(..)
  , UpdateFee(..)

  -- * Reestablishment
  , ChannelReestablish(..)

  -- * Witness data
  , Witness(..)
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (TlvStream)
import Lightning.Protocol.BOLT2.Types

-- Message type codes ----------------------------------------------------------

-- | BOLT #2 message type codes.
data MsgType
  = MsgStfu                    -- ^ 2
  | MsgOpenChannel             -- ^ 32
  | MsgAcceptChannel           -- ^ 33
  | MsgFundingCreated          -- ^ 34
  | MsgFundingSigned           -- ^ 35
  | MsgChannelReady            -- ^ 36
  | MsgShutdown                -- ^ 38
  | MsgClosingSigned           -- ^ 39
  | MsgClosingComplete         -- ^ 40
  | MsgClosingSig              -- ^ 41
  | MsgOpenChannel2            -- ^ 64
  | MsgAcceptChannel2          -- ^ 65
  | MsgTxAddInput              -- ^ 66
  | MsgTxAddOutput             -- ^ 67
  | MsgTxRemoveInput           -- ^ 68
  | MsgTxRemoveOutput          -- ^ 69
  | MsgTxComplete              -- ^ 70
  | MsgTxSignatures            -- ^ 71
  | MsgTxInitRbf               -- ^ 72
  | MsgTxAckRbf                -- ^ 73
  | MsgTxAbort                 -- ^ 74
  | MsgUpdateAddHtlc           -- ^ 128
  | MsgUpdateFulfillHtlc       -- ^ 130
  | MsgUpdateFailHtlc          -- ^ 131
  | MsgCommitmentSigned        -- ^ 132
  | MsgRevokeAndAck            -- ^ 133
  | MsgUpdateFee               -- ^ 134
  | MsgUpdateFailMalformedHtlc -- ^ 135
  | MsgChannelReestablish      -- ^ 136
  deriving stock (Eq, Ord, Show, Generic)

instance NFData MsgType

-- | Get the numeric type code for a message type.
msgTypeWord :: MsgType -> Word16
msgTypeWord MsgStfu                    = 2
msgTypeWord MsgOpenChannel             = 32
msgTypeWord MsgAcceptChannel           = 33
msgTypeWord MsgFundingCreated          = 34
msgTypeWord MsgFundingSigned           = 35
msgTypeWord MsgChannelReady            = 36
msgTypeWord MsgShutdown                = 38
msgTypeWord MsgClosingSigned           = 39
msgTypeWord MsgClosingComplete         = 40
msgTypeWord MsgClosingSig              = 41
msgTypeWord MsgOpenChannel2            = 64
msgTypeWord MsgAcceptChannel2          = 65
msgTypeWord MsgTxAddInput              = 66
msgTypeWord MsgTxAddOutput             = 67
msgTypeWord MsgTxRemoveInput           = 68
msgTypeWord MsgTxRemoveOutput          = 69
msgTypeWord MsgTxComplete              = 70
msgTypeWord MsgTxSignatures            = 71
msgTypeWord MsgTxInitRbf               = 72
msgTypeWord MsgTxAckRbf                = 73
msgTypeWord MsgTxAbort                 = 74
msgTypeWord MsgUpdateAddHtlc           = 128
msgTypeWord MsgUpdateFulfillHtlc       = 130
msgTypeWord MsgUpdateFailHtlc          = 131
msgTypeWord MsgCommitmentSigned        = 132
msgTypeWord MsgRevokeAndAck            = 133
msgTypeWord MsgUpdateFee               = 134
msgTypeWord MsgUpdateFailMalformedHtlc = 135
msgTypeWord MsgChannelReestablish      = 136
{-# INLINE msgTypeWord #-}

-- Channel establishment v1 ----------------------------------------------------

-- | The open_channel message (type 32).
--
-- Contains information about a node and indicates its desire to set up
-- a new channel.
data OpenChannel = OpenChannel
  { openChannelChainHash             :: !ChainHash
  , openChannelTempChannelId         :: !ChannelId
  , openChannelFundingSatoshi       :: {-# UNPACK #-} !Satoshi
  , openChannelPushMsat              :: {-# UNPACK #-} !MilliSatoshi
  , openChannelDustLimitSatoshi     :: {-# UNPACK #-} !Satoshi
  , openChannelMaxHtlcValueInFlight  :: {-# UNPACK #-} !MilliSatoshi
  , openChannelChannelReserveSat     :: {-# UNPACK #-} !Satoshi
  , openChannelHtlcMinimumMsat       :: {-# UNPACK #-} !MilliSatoshi
  , openChannelFeeratePerKw          :: {-# UNPACK #-} !Word32
  , openChannelToSelfDelay           :: {-# UNPACK #-} !Word16
  , openChannelMaxAcceptedHtlcs      :: {-# UNPACK #-} !Word16
  , openChannelFundingPubkey         :: !Point
  , openChannelRevocationBasepoint   :: !Point
  , openChannelPaymentBasepoint      :: !Point
  , openChannelDelayedPaymentBase    :: !Point
  , openChannelHtlcBasepoint         :: !Point
  , openChannelFirstPerCommitPoint   :: !Point
  , openChannelChannelFlags          :: {-# UNPACK #-} !Word8
  , openChannelTlvs                  :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData OpenChannel

-- | The accept_channel message (type 33).
--
-- Contains information about a node and indicates its acceptance of
-- the new channel.
data AcceptChannel = AcceptChannel
  { acceptChannelTempChannelId       :: !ChannelId
  , acceptChannelDustLimitSatoshi   :: {-# UNPACK #-} !Satoshi
  , acceptChannelMaxHtlcValueInFlight :: {-# UNPACK #-} !MilliSatoshi
  , acceptChannelChannelReserveSat   :: {-# UNPACK #-} !Satoshi
  , acceptChannelHtlcMinimumMsat     :: {-# UNPACK #-} !MilliSatoshi
  , acceptChannelMinimumDepth        :: {-# UNPACK #-} !Word32
  , acceptChannelToSelfDelay         :: {-# UNPACK #-} !Word16
  , acceptChannelMaxAcceptedHtlcs    :: {-# UNPACK #-} !Word16
  , acceptChannelFundingPubkey       :: !Point
  , acceptChannelRevocationBasepoint :: !Point
  , acceptChannelPaymentBasepoint    :: !Point
  , acceptChannelDelayedPaymentBase  :: !Point
  , acceptChannelHtlcBasepoint       :: !Point
  , acceptChannelFirstPerCommitPoint :: !Point
  , acceptChannelTlvs                :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData AcceptChannel

-- | The funding_created message (type 34).
--
-- Describes the outpoint which the funder has created for the initial
-- commitment transactions.
data FundingCreated = FundingCreated
  { fundingCreatedTempChannelId   :: !ChannelId
  , fundingCreatedFundingTxid     :: !TxId
  , fundingCreatedFundingOutIdx   :: {-# UNPACK #-} !Word16
  , fundingCreatedSignature       :: !Signature
  } deriving stock (Eq, Show, Generic)

instance NFData FundingCreated

-- | The funding_signed message (type 35).
--
-- Gives the funder the signature for the first commitment transaction.
data FundingSigned = FundingSigned
  { fundingSignedChannelId  :: !ChannelId
  , fundingSignedSignature  :: !Signature
  } deriving stock (Eq, Show, Generic)

instance NFData FundingSigned

-- | The channel_ready message (type 36).
--
-- Indicates that the funding transaction has sufficient confirms for
-- channel use.
data ChannelReady = ChannelReady
  { channelReadyChannelId            :: !ChannelId
  , channelReadySecondPerCommitPoint :: !Point
  , channelReadyTlvs                 :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData ChannelReady

-- Channel establishment v2 ----------------------------------------------------

-- | The open_channel2 message (type 64).
--
-- Initiates the v2 channel establishment workflow.
data OpenChannel2 = OpenChannel2
  { openChannel2ChainHash            :: !ChainHash
  , openChannel2TempChannelId        :: !ChannelId
  , openChannel2FundingFeeratePerkw  :: {-# UNPACK #-} !Word32
  , openChannel2CommitFeeratePerkw   :: {-# UNPACK #-} !Word32
  , openChannel2FundingSatoshi      :: {-# UNPACK #-} !Satoshi
  , openChannel2DustLimitSatoshi    :: {-# UNPACK #-} !Satoshi
  , openChannel2MaxHtlcValueInFlight :: {-# UNPACK #-} !MilliSatoshi
  , openChannel2HtlcMinimumMsat      :: {-# UNPACK #-} !MilliSatoshi
  , openChannel2ToSelfDelay          :: {-# UNPACK #-} !Word16
  , openChannel2MaxAcceptedHtlcs     :: {-# UNPACK #-} !Word16
  , openChannel2Locktime             :: {-# UNPACK #-} !Word32
  , openChannel2FundingPubkey        :: !Point
  , openChannel2RevocationBasepoint  :: !Point
  , openChannel2PaymentBasepoint     :: !Point
  , openChannel2DelayedPaymentBase   :: !Point
  , openChannel2HtlcBasepoint        :: !Point
  , openChannel2FirstPerCommitPoint  :: !Point
  , openChannel2SecondPerCommitPoint :: !Point
  , openChannel2ChannelFlags         :: {-# UNPACK #-} !Word8
  , openChannel2Tlvs                 :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData OpenChannel2

-- | The accept_channel2 message (type 65).
--
-- Indicates acceptance of the v2 channel.
data AcceptChannel2 = AcceptChannel2
  { acceptChannel2TempChannelId        :: !ChannelId
  , acceptChannel2FundingSatoshi      :: {-# UNPACK #-} !Satoshi
  , acceptChannel2DustLimitSatoshi    :: {-# UNPACK #-} !Satoshi
  , acceptChannel2MaxHtlcValueInFlight :: {-# UNPACK #-} !MilliSatoshi
  , acceptChannel2HtlcMinimumMsat      :: {-# UNPACK #-} !MilliSatoshi
  , acceptChannel2MinimumDepth         :: {-# UNPACK #-} !Word32
  , acceptChannel2ToSelfDelay          :: {-# UNPACK #-} !Word16
  , acceptChannel2MaxAcceptedHtlcs     :: {-# UNPACK #-} !Word16
  , acceptChannel2FundingPubkey        :: !Point
  , acceptChannel2RevocationBasepoint  :: !Point
  , acceptChannel2PaymentBasepoint     :: !Point
  , acceptChannel2DelayedPaymentBase   :: !Point
  , acceptChannel2HtlcBasepoint        :: !Point
  , acceptChannel2FirstPerCommitPoint  :: !Point
  , acceptChannel2SecondPerCommitPoint :: !Point
  , acceptChannel2Tlvs                 :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData AcceptChannel2

-- Interactive transaction construction ----------------------------------------

-- | The tx_add_input message (type 66).
--
-- Adds a transaction input to the collaborative transaction.
data TxAddInput = TxAddInput
  { txAddInputChannelId :: !ChannelId
  , txAddInputSerialId  :: !SerialId
  , txAddInputPrevTx    :: !BS.ByteString
  , txAddInputPrevVout  :: {-# UNPACK #-} !Word32
  , txAddInputSequence  :: {-# UNPACK #-} !Word32
  } deriving stock (Eq, Show, Generic)

instance NFData TxAddInput

-- | The tx_add_output message (type 67).
--
-- Adds a transaction output to the collaborative transaction.
data TxAddOutput = TxAddOutput
  { txAddOutputChannelId :: !ChannelId
  , txAddOutputSerialId  :: !SerialId
  , txAddOutputSats      :: {-# UNPACK #-} !Satoshi
  , txAddOutputScript    :: !ScriptPubKey
  } deriving stock (Eq, Show, Generic)

instance NFData TxAddOutput

-- | The tx_remove_input message (type 68).
--
-- Removes a previously added input from the collaborative transaction.
data TxRemoveInput = TxRemoveInput
  { txRemoveInputChannelId :: !ChannelId
  , txRemoveInputSerialId  :: !SerialId
  } deriving stock (Eq, Show, Generic)

instance NFData TxRemoveInput

-- | The tx_remove_output message (type 69).
--
-- Removes a previously added output from the collaborative transaction.
data TxRemoveOutput = TxRemoveOutput
  { txRemoveOutputChannelId :: !ChannelId
  , txRemoveOutputSerialId  :: !SerialId
  } deriving stock (Eq, Show, Generic)

instance NFData TxRemoveOutput

-- | The tx_complete message (type 70).
--
-- Signals the conclusion of a peer's transaction contributions.
data TxComplete = TxComplete
  { txCompleteChannelId :: !ChannelId
  } deriving stock (Eq, Show, Generic)

instance NFData TxComplete

-- | Witness data for tx_signatures.
data Witness = Witness
  { witnessData :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData Witness

-- | The tx_signatures message (type 71).
--
-- Contains signatures for the collaborative transaction.
data TxSignatures = TxSignatures
  { txSignaturesChannelId :: !ChannelId
  , txSignaturesTxid      :: !TxId
  , txSignaturesWitnesses :: ![Witness]
  } deriving stock (Eq, Show, Generic)

instance NFData TxSignatures

-- | The tx_init_rbf message (type 72).
--
-- Initiates a replacement of the transaction after it's been completed.
data TxInitRbf = TxInitRbf
  { txInitRbfChannelId :: !ChannelId
  , txInitRbfLocktime  :: {-# UNPACK #-} !Word32
  , txInitRbfFeerate   :: {-# UNPACK #-} !Word32
  , txInitRbfTlvs      :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData TxInitRbf

-- | The tx_ack_rbf message (type 73).
--
-- Acknowledges an RBF attempt.
data TxAckRbf = TxAckRbf
  { txAckRbfChannelId :: !ChannelId
  , txAckRbfTlvs      :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData TxAckRbf

-- | The tx_abort message (type 74).
--
-- Aborts the collaborative transaction negotiation.
data TxAbort = TxAbort
  { txAbortChannelId :: !ChannelId
  , txAbortData      :: !BS.ByteString
  } deriving stock (Eq, Show, Generic)

instance NFData TxAbort

-- Channel close ---------------------------------------------------------------

-- | The stfu message (type 2).
--
-- Indicates "SomeThing Fundamental is Underway" - used for channel
-- quiescence.
data Stfu = Stfu
  { stfuChannelId :: !ChannelId
  , stfuInitiator :: !Initiator
  } deriving stock (Eq, Show, Generic)

instance NFData Stfu

-- | The shutdown message (type 38).
--
-- Initiates closing of the channel.
data Shutdown = Shutdown
  { shutdownChannelId    :: !ChannelId
  , shutdownScriptPubkey :: !ScriptPubKey
  } deriving stock (Eq, Show, Generic)

instance NFData Shutdown

-- | The closing_signed message (type 39).
--
-- Used in legacy closing negotiation.
data ClosingSigned = ClosingSigned
  { closingSignedChannelId   :: !ChannelId
  , closingSignedFeeSatoshi :: {-# UNPACK #-} !Satoshi
  , closingSignedSignature   :: !Signature
  , closingSignedTlvs        :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData ClosingSigned

-- | The closing_complete message (type 40).
--
-- Proposes a closing transaction in the new closing protocol.
data ClosingComplete = ClosingComplete
  { closingCompleteChannelId       :: !ChannelId
  , closingCompleteCloserScript    :: !ScriptPubKey
  , closingCompleteCloseeScript    :: !ScriptPubKey
  , closingCompleteFeeSatoshi     :: {-# UNPACK #-} !Satoshi
  , closingCompleteLocktime        :: {-# UNPACK #-} !Word32
  , closingCompleteTlvs            :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData ClosingComplete

-- | The closing_sig message (type 41).
--
-- Signs a closing transaction in the new closing protocol.
data ClosingSig = ClosingSig
  { closingSigChannelId       :: !ChannelId
  , closingSigCloserScript    :: !ScriptPubKey
  , closingSigCloseeScript    :: !ScriptPubKey
  , closingSigFeeSatoshi     :: {-# UNPACK #-} !Satoshi
  , closingSigLocktime        :: {-# UNPACK #-} !Word32
  , closingSigTlvs            :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData ClosingSig

-- Normal operation ------------------------------------------------------------

-- | The update_add_htlc message (type 128).
--
-- Offers an HTLC to the other node, redeemable in return for a payment
-- preimage.
data UpdateAddHtlc = UpdateAddHtlc
  { updateAddHtlcChannelId       :: !ChannelId
  , updateAddHtlcId              :: !HtlcId
  , updateAddHtlcAmountMsat      :: {-# UNPACK #-} !MilliSatoshi
  , updateAddHtlcPaymentHash     :: !PaymentHash
  , updateAddHtlcCltvExpiry      :: {-# UNPACK #-} !Word32
  , updateAddHtlcOnionPacket     :: !OnionPacket
  , updateAddHtlcTlvs            :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData UpdateAddHtlc

-- | The update_fulfill_htlc message (type 130).
--
-- Supplies the preimage to fulfill an HTLC.
data UpdateFulfillHtlc = UpdateFulfillHtlc
  { updateFulfillHtlcChannelId       :: !ChannelId
  , updateFulfillHtlcId              :: !HtlcId
  , updateFulfillHtlcPaymentPreimage :: !PaymentPreimage
  , updateFulfillHtlcTlvs            :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData UpdateFulfillHtlc

-- | The update_fail_htlc message (type 131).
--
-- Indicates an HTLC has failed.
data UpdateFailHtlc = UpdateFailHtlc
  { updateFailHtlcChannelId :: !ChannelId
  , updateFailHtlcId        :: !HtlcId
  , updateFailHtlcReason    :: !BS.ByteString
  , updateFailHtlcTlvs      :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData UpdateFailHtlc

-- | The update_fail_malformed_htlc message (type 135).
--
-- Indicates an HTLC could not be parsed.
data UpdateFailMalformedHtlc = UpdateFailMalformedHtlc
  { updateFailMalformedHtlcChannelId   :: !ChannelId
  , updateFailMalformedHtlcId          :: !HtlcId
  , updateFailMalformedHtlcSha256Onion :: !PaymentHash
  , updateFailMalformedHtlcFailureCode :: {-# UNPACK #-} !Word16
  } deriving stock (Eq, Show, Generic)

instance NFData UpdateFailMalformedHtlc

-- | The commitment_signed message (type 132).
--
-- Applies pending changes and provides signatures for the commitment
-- transaction.
data CommitmentSigned = CommitmentSigned
  { commitmentSignedChannelId      :: !ChannelId
  , commitmentSignedSignature      :: !Signature
  , commitmentSignedHtlcSignatures :: ![Signature]
  } deriving stock (Eq, Show, Generic)

instance NFData CommitmentSigned

-- | The revoke_and_ack message (type 133).
--
-- Revokes the previous commitment transaction and acknowledges receipt
-- of the commitment_signed.
data RevokeAndAck = RevokeAndAck
  { revokeAndAckChannelId             :: !ChannelId
  , revokeAndAckPerCommitmentSecret   :: !PerCommitmentSecret
  , revokeAndAckNextPerCommitPoint    :: !Point
  } deriving stock (Eq, Show, Generic)

instance NFData RevokeAndAck

-- | The update_fee message (type 134).
--
-- Updates the fee rate for commitment transactions.
data UpdateFee = UpdateFee
  { updateFeeChannelId    :: !ChannelId
  , updateFeeFeeratePerKw :: {-# UNPACK #-} !Word32
  } deriving stock (Eq, Show, Generic)

instance NFData UpdateFee

-- Reestablishment -------------------------------------------------------------

-- | The channel_reestablish message (type 136).
--
-- Used to re-establish a channel after reconnection.
data ChannelReestablish = ChannelReestablish
  { channelReestablishChannelId            :: !ChannelId
  , channelReestablishNextCommitNum        :: {-# UNPACK #-} !Word64
  , channelReestablishNextRevocationNum    :: {-# UNPACK #-} !Word64
  , channelReestablishYourLastCommitSecret :: !PerCommitmentSecret
  , channelReestablishMyCurrentCommitPoint :: !Point
  , channelReestablishTlvs                 :: !TlvStream
  } deriving stock (Eq, Show, Generic)

instance NFData ChannelReestablish

-- Top-level message type ------------------------------------------------------

-- | All BOLT #2 messages.
data Message
  -- Channel establishment v1
  = MsgOpenChannelVal !OpenChannel
  | MsgAcceptChannelVal !AcceptChannel
  | MsgFundingCreatedVal !FundingCreated
  | MsgFundingSignedVal !FundingSigned
  | MsgChannelReadyVal !ChannelReady
  -- Channel establishment v2
  | MsgOpenChannel2Val !OpenChannel2
  | MsgAcceptChannel2Val !AcceptChannel2
  | MsgTxAddInputVal !TxAddInput
  | MsgTxAddOutputVal !TxAddOutput
  | MsgTxRemoveInputVal !TxRemoveInput
  | MsgTxRemoveOutputVal !TxRemoveOutput
  | MsgTxCompleteVal !TxComplete
  | MsgTxSignaturesVal !TxSignatures
  | MsgTxInitRbfVal !TxInitRbf
  | MsgTxAckRbfVal !TxAckRbf
  | MsgTxAbortVal !TxAbort
  -- Channel close
  | MsgStfuVal !Stfu
  | MsgShutdownVal !Shutdown
  | MsgClosingSignedVal !ClosingSigned
  | MsgClosingCompleteVal !ClosingComplete
  | MsgClosingSigVal !ClosingSig
  -- Normal operation
  | MsgUpdateAddHtlcVal !UpdateAddHtlc
  | MsgUpdateFulfillHtlcVal !UpdateFulfillHtlc
  | MsgUpdateFailHtlcVal !UpdateFailHtlc
  | MsgUpdateFailMalformedHtlcVal !UpdateFailMalformedHtlc
  | MsgCommitmentSignedVal !CommitmentSigned
  | MsgRevokeAndAckVal !RevokeAndAck
  | MsgUpdateFeeVal !UpdateFee
  -- Reestablishment
  | MsgChannelReestablishVal !ChannelReestablish
  deriving stock (Eq, Show, Generic)

instance NFData Message
