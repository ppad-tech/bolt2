{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT2
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Peer protocol for the Lightning Network, per
-- [BOLT #2](https://github.com/lightning/bolts/blob/master/02-peer-protocol.md).

module Lightning.Protocol.BOLT2 (
  -- * Core types
  -- | Re-exported from "Lightning.Protocol.BOLT2.Types".
    module Lightning.Protocol.BOLT2.Types

  -- * Message types
  -- | Re-exported from "Lightning.Protocol.BOLT2.Messages".
  , module Lightning.Protocol.BOLT2.Messages

  -- * Codec functions
  -- | Re-exported from "Lightning.Protocol.BOLT2.Codec".
  , module Lightning.Protocol.BOLT2.Codec

  -- $messagetypes

  -- ** Channel establishment (v1)
  -- $v1establishment

  -- ** Channel establishment (v2)
  -- $v2establishment

  -- ** Channel close
  -- $close

  -- ** Normal operation
  -- $normal

  -- ** Message reestablishment
  -- $reestablish
  ) where

import Lightning.Protocol.BOLT2.Codec
import Lightning.Protocol.BOLT2.Messages
import Lightning.Protocol.BOLT2.Types

-- $messagetypes
--
-- BOLT #2 defines the following message types:
--
-- * 2: stfu
-- * 32: open_channel
-- * 33: accept_channel
-- * 34: funding_created
-- * 35: funding_signed
-- * 36: channel_ready
-- * 38: shutdown
-- * 39: closing_signed
-- * 40: closing_complete
-- * 41: closing_sig
-- * 64: open_channel2
-- * 65: accept_channel2
-- * 66: tx_add_input
-- * 67: tx_add_output
-- * 68: tx_remove_input
-- * 69: tx_remove_output
-- * 70: tx_complete
-- * 71: tx_signatures
-- * 72: tx_init_rbf
-- * 73: tx_ack_rbf
-- * 74: tx_abort
-- * 128: update_add_htlc
-- * 130: update_fulfill_htlc
-- * 131: update_fail_htlc
-- * 132: commitment_signed
-- * 133: revoke_and_ack
-- * 134: update_fee
-- * 135: update_fail_malformed_htlc
-- * 136: channel_reestablish

-- $v1establishment
--
-- Channel establishment v1 messages:
--
-- * open_channel (32)
-- * accept_channel (33)
-- * funding_created (34)
-- * funding_signed (35)
-- * channel_ready (36)

-- $v2establishment
--
-- Channel establishment v2 (interactive-tx) messages:
--
-- * open_channel2 (64)
-- * accept_channel2 (65)
-- * tx_add_input (66)
-- * tx_add_output (67)
-- * tx_remove_input (68)
-- * tx_remove_output (69)
-- * tx_complete (70)
-- * tx_signatures (71)
-- * tx_init_rbf (72)
-- * tx_ack_rbf (73)
-- * tx_abort (74)

-- $close
--
-- Channel close messages:
--
-- * stfu (2)
-- * shutdown (38)
-- * closing_signed (39)
-- * closing_complete (40)
-- * closing_sig (41)

-- $normal
--
-- Normal operation messages:
--
-- * update_add_htlc (128)
-- * update_fulfill_htlc (130)
-- * update_fail_htlc (131)
-- * commitment_signed (132)
-- * revoke_and_ack (133)
-- * update_fee (134)
-- * update_fail_malformed_htlc (135)

-- $reestablish
--
-- Message reestablishment:
--
-- * channel_reestablish (136)
