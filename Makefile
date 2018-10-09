build:
	stack build
	cargo build

clean:
	stack clean
	cargo clean

test:
	stack test
	cargo test

.PHONY: build test clean
