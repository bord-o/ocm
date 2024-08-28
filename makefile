run:
	dune exec ocm --profile=release

build:
	dune build --profile=release

watch:
	dune build -w
