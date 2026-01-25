# REVIEW1: PTAL findings

## Findings

1) High: u16-length fields can silently truncate on encode.
- Helper `encodeU16Bytes` uses `fromIntegral` without bounds checks.
- Affected encoders: TxAddInput, TxAddOutput, Witness, TxAbort,
  UpdateFailHtlc.
- Files:
  - lib/Lightning/Protocol/BOLT2/Codec.hs:283
  - lib/Lightning/Protocol/BOLT2/Codec.hs:879
  - lib/Lightning/Protocol/BOLT2/Codec.hs:909
  - lib/Lightning/Protocol/BOLT2/Codec.hs:986
  - lib/Lightning/Protocol/BOLT2/Codec.hs:1074
  - lib/Lightning/Protocol/BOLT2/Codec.hs:1157

2) Medium: fixed-size 32-byte secrets are unvalidated in message types.
- `RevokeAndAck` and `ChannelReestablish` store secrets as raw
  ByteString; encoders don't validate length.
- Files:
  - lib/Lightning/Protocol/BOLT2/Messages.hs:527
  - lib/Lightning/Protocol/BOLT2/Messages.hs:550
  - lib/Lightning/Protocol/BOLT2/Codec.hs:1247
  - lib/Lightning/Protocol/BOLT2/Codec.hs:1289

3) Medium: TLV decoding allows unknown even types by default.
- `decodeTlvStreamRaw` does not enforce the unknown-even rule.
- Files:
  - lib/Lightning/Protocol/BOLT2/Codec.hs:226
  - lib/Lightning/Protocol/BOLT2/Codec.hs:297

4) Low: list counts can overflow Word16 on encode.
- `length` of witnesses/signatures is truncated to Word16 without
  checking.
- Files:
  - lib/Lightning/Protocol/BOLT2/Codec.hs:996
  - lib/Lightning/Protocol/BOLT2/Codec.hs:1210

5) Low: flake uses absolute path for ppad-bolt1.
- `flake.nix` points to `/Users/jtobin/src/ppad/bolt1`.
- File:
  - flake.nix:5

6) Low: tests are empty and helper uses partial `error`.
- No tests in `test/Main.hs`; `unhex` uses `error`.
- File:
  - test/Main.hs:15

## Notes

- Decide whether TLV unknown-even enforcement belongs in codec or in a
  validation layer.
- Consider making u16-length encoders return `Either EncodeError` to
  prevent silent truncation.

