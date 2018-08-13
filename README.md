# beaker-preprocessor

[![CircleCI](https://circleci.com/gh/Daolab/beaker-preprocessor.svg?style=svg&circle-token=94c1ada8b1bd409ae2f7355cb4c76d4082cc1ad9)](https://circleci.com/gh/Daolab/beaker-preprocessor)

This is the Beaker preprocessor. It contains a library and CLI interface for
making Ethereum smart contracts compatible with BeakerOS. It provides examples
of verification and code injection.

This is configured to build using
[stack](https://docs.haskellstack.org/en/stable/README/).

## Bulding:

```sh
stack build
```

## Testing:

In order to run the tests, you will need to install a solidity compiler. It is
currently configured to use the solcjs compiler from npm. You will also need a
locally running test network to run on-chain tests. Ganache works well for this.

```sh
npm i -D solc
npm i ganache-cli
./node_modules/.bin/ganache-cli
stack test
```

## Documentation:

In order to produce documentation for the Haskell code run:

```sh
stack haddock --open .
```
