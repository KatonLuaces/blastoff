.PHONY : test
test : all testall.sh
	./testall.sh

.PHONY : all
all : blastoff.native backend.o

# "make blastoff.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

blastoff.native : blastoff.ml ast.ml parser.mly scanner.mll semant.ml codegen.ml backend.bc
	opam config exec -- \
	ocamlbuild -use-ocamlfind blastoff.native -pkgs llvm.bitreader

# backend.c tester
backend : backend.c
	cc -g -Wall -o backend -DRUN_TEST backend.c -lgraphblas

backend.bc : backend.c
	clang -g -emit-llvm -o backend.bc -c backend.c -Wno-varargs

# "make clean" removes all generated files
.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff *.ll *.s *.o *.exe *.out *.err \
		parser.ml parser.mli blastoff.native \
		backend backend.o backend.bc \

.PHONY : fresh
fresh : clean test

# Building the tarball

TESTS = \
  func1

FAILS = \
  assign

TESTFILES = $(TESTS:%=test-%.bl) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.bl) $(FAILS:%=fail-%.out)

TARFILES = ast.ml codegen.ml Makefile _tags blastoff.ml parser.mly \
	README.md scanner.mll semant.ml definitions.ml\
	testlocal.sh testall.sh \
	backend.c graphblas.h\
	Dockerfile \
	.ocamlformat \
	tests/

blastoff.tar.gz : $(TARFILES)
	cd .. && tar czf blastoff/blastoff.tar.gz \
		$(TARFILES:%=blastoff/%)
