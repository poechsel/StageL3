all:
	ocamlbuild -yaccflag -v -lib unix -lib nums main.native
