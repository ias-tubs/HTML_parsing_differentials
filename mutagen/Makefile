generate: build
	dune exec gen

run: build
	bash ./run_local.sh

build:
	dune build

profile: clean
	OCAML_LANDMARKS=auto dune build --instrument-with landmarks

clean:
	dune clean

test: build
	dune runtest --force

reformat:
	dune build @fmt --auto-promote
