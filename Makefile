all:
	ocamlbuild -yaccflag -v -lib unix -lib nums src/main.native

clean:
	rm -rfd _build/ main.native
