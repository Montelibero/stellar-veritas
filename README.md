# Stellar Veritas

The aim is to create a trustworthy Stellar transaction signer (and, by necessity, a pretty printer) using only Glasgow Haskell compiler code and Haskell Core libraries, reducing the possible supply chain attack surface.

To build and run it, install `cabal-install` and use `cabal run`.

The project contains the code of trimmed-down non-core dependencies, mainly cryptographic libraries. To avoid using bundled libraries (to build against the current Hackage), do the same in the `src` directory. To further reduce the amount of code under audit, weeder can be used, although the utility is dubious.
