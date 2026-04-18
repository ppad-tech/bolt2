# ppad-bolt2

[![](https://img.shields.io/hackage/v/ppad-bolt2?color=blue)](https://hackage.haskell.org/package/ppad-bolt2)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt2-lightblue)](https://docs.ppad.tech/bolt2)

Haskell implementation of BOLT #2 (Lightning Network peer protocol),
including message types and codecs for channel establishment, normal
operation, and channel close.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > import qualified Data.ByteString as BS
  > import qualified Lightning.Protocol.BOLT2 as BOLT2
  > import Lightning.Protocol.BOLT1 (TlvStream(..))
  >
  > -- construct an open_channel message
  > let Just cid = BOLT2.channelId (BS.replicate 32 0x00)
  > let Just ch = BOLT2.chainHash (BS.replicate 32 0x01)
  > let Just pk = BOLT2.point (BS.cons 0x02 (BS.replicate 32 0xff))
  >
  > let msg = BOLT2.OpenChannel
  >       { BOLT2.openChannelChainHash = ch
  >       , BOLT2.openChannelTempChannelId = cid
  >       , BOLT2.openChannelFundingSatoshis = BOLT2.Satoshis 1000000
  >       , BOLT2.openChannelPushMsat = BOLT2.MilliSatoshis 0
  >       , BOLT2.openChannelDustLimitSatoshis = BOLT2.Satoshis 546
  >       , BOLT2.openChannelMaxHtlcValueInFlight = BOLT2.MilliSatoshis 100000000
  >       , BOLT2.openChannelChannelReserveSat = BOLT2.Satoshis 10000
  >       , BOLT2.openChannelHtlcMinimumMsat = BOLT2.MilliSatoshis 1000
  >       , BOLT2.openChannelFeeratePerKw = 2500
  >       , BOLT2.openChannelToSelfDelay = 144
  >       , BOLT2.openChannelMaxAcceptedHtlcs = 483
  >       , BOLT2.openChannelFundingPubkey = pk
  >       , BOLT2.openChannelRevocationBasepoint = pk
  >       , BOLT2.openChannelPaymentBasepoint = pk
  >       , BOLT2.openChannelDelayedPaymentBase = pk
  >       , BOLT2.openChannelHtlcBasepoint = pk
  >       , BOLT2.openChannelFirstPerCommitPoint = pk
  >       , BOLT2.openChannelChannelFlags = 0x01
  >       , BOLT2.openChannelTlvs = TlvStream []
  >       }
  >
  > -- encode and decode
  > let encoded = BOLT2.encodeOpenChannel msg
  > BS.length encoded
  319
  > BOLT2.decodeOpenChannel encoded
  Right (OpenChannel {..}, "")
```

## Message Types

Supported BOLT #2 messages:

- Channel establishment v1: `open_channel`, `accept_channel`,
  `funding_created`, `funding_signed`, `channel_ready`
- Channel establishment v2: `open_channel2`, `accept_channel2`,
  `tx_add_input`, `tx_add_output`, `tx_remove_input`, `tx_remove_output`,
  `tx_complete`, `tx_signatures`, `tx_init_rbf`, `tx_ack_rbf`, `tx_abort`
- Channel close: `stfu`, `shutdown`, `closing_signed`, `closing_complete`,
  `closing_sig`
- Normal operation: `update_add_htlc`, `update_fulfill_htlc`,
  `update_fail_htlc`, `update_fail_malformed_htlc`, `commitment_signed`,
  `revoke_and_ack`, `update_fee`
- Reestablishment: `channel_reestablish`

## Documentation

Haddocks are hosted at [docs.ppad.tech/bolt2][hadoc].

## Security

This is a pre-release version of the library and makes no guarantees about
security whatsoever.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal build
$ cabal test
$ cabal bench
```

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/bolt2
