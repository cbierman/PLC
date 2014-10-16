.PHONY: test

test: hw2-check
	./hw2-check

hw2-check: hw2.cmo checker.cmo
	ocamlc -g -o $@ $^

hw2.cmo: hw2.ml
	ocamlc -c $^

checker.cmo: checker.ml
	ocamlc -c $^
