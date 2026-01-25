# IMPL1: BOLT2 implementation plan

Goal: implement full BOLT #2 message types, codecs, validation, tests,
benchmarks.

## Work breakdown

1) Core types
- Implement newtypes for identifiers, amounts, hashes, and keys.
- Provide smart constructors and validation helpers.
- Add NFData instances for benchmark use.

2) Message ADTs
- Define per-message record types with strict fields.
- Define the top-level `Message` sum type and tag mapping.

3) TLV framework
- Implement `TLVStream` type and parser.
- Encode/decode BigSize, ordering, and unknown TLV retention.

4) Codec layer
- Implement `encodeMessage` and `decodeMessage` dispatchers.
- Implement codecs for each message type using BOLT1 primitives.

5) Validation layer
- Implement `validateMessage` and per-message validators.
- Enforce invariants that cannot be encoded in types.

6) Tests
- Add unit tests from BOLT #2 vectors.
- Add property tests for roundtrip and TLV ordering.

7) Benchmarks
- Add criterion benchmarks for hot paths.
- Add weigh benchmarks for allocation profile.

8) Cabal + exports
- Update `ppad-bolt2.cabal` to expose modules and tests/benchmarks.
- Keep `Lightning.Protocol.BOLT2` as the public entry point.

## Parallelizable tasks

- TLV framework can be built independently from message codecs.
- Core types can be built in parallel with TLV and message ADTs.
- Tests can be written in parallel once codecs and TLVs are sketched.
- Benchmarks can be added after codecs are in place.

## Suggested subagent delegation

- Agent A: Core types + validation helpers.
- Agent B: TLV framework + TLV tests.
- Agent C: Message ADTs + tag mapping.
- Agent D: Codecs for v1 establishment + close messages.
- Agent E: Codecs for v2 interactive-tx messages.
- Agent F: Normal operation + reestablish messages.
- Agent G: Test vectors + property tests.
- Agent H: Benchmarks + NFData instances.

## Risks / notes

- Some invariants depend on other BOLTs or feature bits. Keep validation
  modular and allow partial validation when necessary.
- Ensure total decoders and avoid partial pattern matches.
- Keep lines under 80 chars; use strict fields and UNPACK.
- Do not add external deps without explicit approval.
- If other BOLT implementations are needed (e.g. BOLT1), add them as
  flake inputs (e.g. `../bolt1`) and consume via Nix, not ad hoc paths.

## Acceptance criteria

- All BOLT #2 messages are representable and roundtrip.
- All public constructors are total and validated.
- All tests and benchmarks compile and run via cabal.
- No partial functions; no unchecked indexing in tests.
