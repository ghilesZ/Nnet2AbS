# Frontend to dune.

.PHONY: default build install uninstall test clean

default: build

run: build
	dune exec src/main.exe

build:
	@mkdir -p abs
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
