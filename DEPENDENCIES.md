# Required Opam Packages

- `bip340` — Schnorr (BIP-340) signing via libsecp256k1, matching nostr-tools’ finalize flow.
- `bech32` — Decode the `nsec` Bech32 secret key before signing.
- `hex` — Hex encode/decode helpers for pubkeys, ids, and signatures (mirrors nostr-tools usage).
