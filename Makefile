build:
	stack build
	cargo build

clean:
	stack clean
	cargo clean

test:
	stack test
	cargo test

contracts:
	solc beakeros/contracts/Kernel.sol -o Kernel --abi --bin --overwrite
	solc beakeros/contracts/test/valid/Adder.sol -o Adder --abi --bin --overwrite

.PHONY: build test clean
