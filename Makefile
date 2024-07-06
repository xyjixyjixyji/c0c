default: all

all: c0c

c0c: ../bin/c0c
../bin/c0c: release
	mkdir -p ../bin
	cp target/release/c0c ../bin/c0c

release:
	cargo build --release --bins

debug:
	cargo build

clean:
	cargo clean
	rm -f ../bin/c0c

check:
	cargo fmt -- --check
	cargo clippy

.PHONY: c0c debug release clean check
