# Stellar Veritas

The aim is to create a trustworthy Stellar transaction signer (and, by necessity, a pretty printer) using only Glasgow Haskell compiler code and Haskell Core libraries, reducing the possible supply chain attack surface.

## Installation and usage

To build and run it, install `cabal-install` and use `cabal run`. The program expects your transaction coming from the standard input, and your private (S...) key residing in `~/.stellar-veritas-key`. It will produce the decoded transaction description afterwards, with simple descriptions of the most popular operations. When it is ran from an interactive terminal, it will ask for a confirmation before signing the transaction.

You can put the binary (`cabal list-bin stellar-veritas` to get the path) into your `$PATH` and use it directly.

## Encryption

The project supports key encryption. Encrypting your key protects you from key file theft (cracking a 8-character password is estimated to cost over $1M (in 2026), scales with password complexity and the performance of the computer at the moment of encryption).

To encrypt your key, run `cabal run stellar-veritas -- encrypt` (or just `stellar-veritas encrypt` after installation) and enter your password. Afterwards you'll be prompted your password every time you want to sign anything. `encrypt`ing an already-encrpyted key would result in decryption and re-encryption with new parameters and salt.

## Technical details

The project contains the code of trimmed-down non-core dependencies, mainly cryptographic libraries. To avoid using bundled libraries (to build against the current Hackage), do the same in the `src` directory. To further reduce the amount of code under audit, weeder can be used, although the utility is dubious.

Key encryption is implemented via XOR with Argon2i KDF with an adaptive iteration count.

### Bundled libraries

* base32 0.4
* base64-bytestring 1.2.1.0
* cereal 0.5.8.3
* ed25519 0.0.5.0
* scientific 0.3.8.1
* SHA 1.6.4.4
* integer-logarithms 1.0.5
* cryptonite 0.30
* memory 0.18.0
* basement 0.0.16
* stellar-sdk & stellar-horizon git+https://github.com/cblp/haskell-stellar-sdk 756e3aa04c28fde502e239fac9da3dd1d1ee31bb
