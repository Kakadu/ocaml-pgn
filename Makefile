.PHONY: all lib test clean

all:
	ocamlbuild -use-ocamlfind pgn.cma pgn.cmxa test.native

test: all
	./test.native

clean:
	rm -fr _build test*.native test*.byte

install:
	ocamlfind install pgn2 META _build/pgn.cma _build/pgn.cmxa

uninstall:
	ocamlfind remove pgn2

