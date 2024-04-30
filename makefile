all: evaluation expr miniml test

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

test: test.ml
	ocamlbuild -use-ocamlfind test.byte

clean:
	rm -rf _build *.byte