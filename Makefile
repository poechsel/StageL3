all:
	ocamlbuild -yaccflag -v -lib unix -lib nums main.native

clean:
	rm -rfd _build/ main.native
