make :
	dune build --profile=release
	./_build/default/gol.exe
