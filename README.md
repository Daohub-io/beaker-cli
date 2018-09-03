# beaker-cli

[![CircleCI](https://circleci.com/gh/Daolab/beaker-preprocessor.svg?style=svg&circle-token=94c1ada8b1bd409ae2f7355cb4c76d4082cc1ad9)](https://circleci.com/gh/Daolab/beaker-preprocessor)

This is the Beaker preprocessor. It contains a library and CLI interface for
making Ethereum smart contracts compatible with BeakerOS. It provides examples
of verification and code injection.

This is configured to build using
[stack](https://docs.haskellstack.org/en/stable/README/).

## Building

```sh
stack build
```

## Testing

In order to run the tests, you will need to install a solidity compiler. It is
currently configured to use the solcjs compiler from npm. You will also need a
locally running test network to run on-chain tests. Ganache works well for this.

```sh
npm i -D solc
npm i ganache-cli
./node_modules/.bin/ganache-cli
stack test
```

## Documentation

In order to produce documentation for the Haskell code run:

```sh
stack haddock --open .
```

## Using the CLI

The CLI contains a number of commands. Where a contract is read from file there
is the `--read READ-TYPE` options. `READ-TYPE` is the format of the input file.
By default it binary (`bin`), but it can also accept hex-encoded, and
take the output from the solc compiler. The following types are accepted.

* `bin`
* `hex`
* `solc`

All of the test files in this repo currently require the `--read solc`
option (which is not the default).

### To Print the Opcodes

This prints the parsed opcodes of the contract in the internal format.

```sh
stack exec -- beaker opcodes INPUT-PATH --read READ-TYPE
```

### To Print the Structured Code

This prints the internal structured code representation.

```sh
stack exec -- beaker opcodes INPUT-PATH --read READ-TYPE
```

### To Check Compliance with Beaker

The `check` command lists all of the non-compliances with all of the beaker
procedure standard requirements. If non are listed the procedure is compliant.

```sh
stack exec -- beaker check INPUT-PATH --read READ-TYPE
```

`INPUT-PATH` is the path of the file where the contract is contained.

