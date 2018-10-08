
all: game.native

game.native:
	ocamlbuild -use-ocamlfind -I src game.native

fix-stuff:
	eval $(opam config env)

.PHONY: game.native fix-stuff

