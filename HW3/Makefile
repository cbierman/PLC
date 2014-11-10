all: test-1 test-2

.PHONY: romanChecker.native test-1 test-1-v ocamlChecker.native test-2 test-2-v

test-1: romanChecker.native
	./romanChecker.native

test-1-v: romanChecker.native
	./romanChecker.native -v

test-2: ocamlChecker.native
	./ocamlChecker.native

test-2-v: ocamlChecker.native
	./ocamlChecker.native -v

romanChecker.native: 
	ocamlbuild -use-menhir -menhir "menhir --explain --infer --strict" romanChecker.native

ocamlChecker.native:
	ocamlbuild -use-menhir -menhir "menhir --explain --infer --strict" ocamlChecker.native
	if [ -r _build/ocamlParser.conflicts ]; then cp _build/ocamlParser.conflicts .; fi
clean:
	ocamlbuild -clean

