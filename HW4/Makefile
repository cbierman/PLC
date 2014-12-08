all: test-gc

.PHONY: clean test-gc gcChecker.native

test-gc: clean gcChecker.native
	./gcChecker.native

gcChecker.native: 
	ocamlbuild gcChecker.native

clean:
	ocamlbuild -clean

