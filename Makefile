build:
	cd beaker-haskell && stack build
	cd beaker-wrapper && cargo build

clean:
	cd beaker-haskell && stack clean
	cd beaker-wrapper && cargo clean

test:
	cd beaker-haskell && stack test
	cd beaker-wrapper && cargo test

.PHONY: build test clean
