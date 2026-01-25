# ARCH1: BOLT2 architecture

Goal: implement the Lightning Network peer protocol (BOLT #2) with
strong types, total decoding, and high performance, using only core and
ppad libraries.

## Scope

- Messages defined in BOLT #2 (v1 and v2 channel establishment, close,
  normal operation, reestablish).
- Binary encoding/decoding of messages and TLVs.
- Validation of message invariants at boundary.
- Tests against spec vectors and property tests for roundtrips.
- Benchmarks for encode/decode hot paths.

Out of scope: wire transport, encryption, gossip, BOLT #3/7 logic.

## Module layout (proposed)

- `Lightning.Protocol.BOLT2`
  - Public re-exports, type aliases, and high level API.
- `Lightning.Protocol.BOLT2.Types`
  - Newtypes and ADTs for identifiers, amounts, features, and message
    payloads.
- `Lightning.Protocol.BOLT2.Message`
  - Sum type of all BOLT2 messages and message type tags.
- `Lightning.Protocol.BOLT2.Codec`
  - Encoding/decoding for each message and the top-level dispatcher.
- `Lightning.Protocol.BOLT2.TLV`
  - TLV stream types and known TLV records.
- `Lightning.Protocol.BOLT2.Validation`
  - Smart constructors and invariants for all public types.

A module split keeps API clean and allows smaller, INLINE-friendly
functions in the codec.

## Types and invariants

- Use newtypes for identifiers and amounts:
  - `ChannelId`, `ShortChannelId`, `Satoshis`, `MilliSatoshis`,
    `BlockHeight`, `FeeratePerKw`, `CsvDelay`, `HtlcId`, `CltvExpiry`.
- Use fixed-size ByteString newtypes:
  - `PubKey33`, `Signature64`, `Sha256`, `ChainHash32`.
- Encode legal states in ADTs:
  - `ShutdownScript` as a sum of allowed script forms.
  - `CloseFee` either `CloseFeeProposed` or `CloseFeeNone` for v2 flow.
- Smart constructors validate lengths and numeric bounds.

All public constructors should be total, validated, and return
`Either DecodeError a` or `Maybe a`.

## Encoding and decoding

- Use BOLT1 primitives (`encodeU16`, `decodeU16`, `encodeBigSize`, ...).
- Provide `encodeMessage :: Message -> ByteString` and
  `decodeMessage :: ByteString -> Either DecodeError Message`.
- Dispatch on message type tag; for known tags, parse body strictly.
- Keep parsers total; never throw exceptions; return `DecodeError`.
- For TLVs, parse as `TLVStream` with known/unknown TLVs preserved.

## TLV handling

- Implement a small TLV framework using BigSize types.
- Known TLVs decode into typed records; unknown TLVs preserved as raw
  `(type, bytes)` for roundtrip correctness.
- For each message with TLVs, keep `tlvs :: TLVStream` field.

## Validation

- Validate per-message invariants described in BOLT #2, for example:
  - channel reserve <= funding amount.
  - dust limits within limits.
  - maximum HTLC values consistent with fee rate.
  - feature bit constraints for v2 flows.
- Provide `validateMessage :: Message -> Either ValidationError Message`.

## Error model

- `DecodeError` enumerates: short input, unknown tag, invalid length,
  invalid field, invalid TLV ordering, and overflow.
- `ValidationError` enumerates semantic violations.

## Performance

- Strict fields with `BangPatterns` and `UNPACK` where practical.
- Small, INLINE encode/decode helpers for hot paths.
- Avoid intermediate lists in codecs; use ByteString builders.

## Tests

- Unit tests from BOLT #2 examples.
- Roundtrip tests for each message:
  - `decodeMessage (encodeMessage m) == Right m`.
- Property tests for TLV ordering and unknown TLV preservation.
- Totality tests for decoders with short or malformed inputs.

## Benchmarks

- Encode/decode benchmarks for:
  - open_channel, accept_channel, commitment_signed,
    update_add_htlc, channel_reestablish.
- Separate allocation benchmarks in `bench/Weight.hs`.

## Deliverables

- New modules under `lib/Lightning/Protocol/BOLT2/*`.
- Tests under `test/` and benchmarks under `bench/`.
- Updated `ppad-bolt2.cabal` exports and test/bench stanzas.

