# "make test" compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable

.PHONY : all
all : blastoff.native graphblas.o

# "make blastoff.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

blastoff.native : blastoff.ml ast.ml blastoffparser.mly scanner.mll codegen.ml graphblas.bc
	opam config exec -- \
	ocamlbuild -use-ocamlfind blastoff.native -pkgs llvm.bitreader

#build graphblas operations file

graphblas : graphblas.c
	cc -o graphblas -DRUN_TEST graphblas.c

graphblas.bc : graphblas.c
	clang -emit-llvm -o graphblas.bc -c graphblas.c -Wno-varargs

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff *.ll *.s *.o *.exe \
		blastoffparser.ml blastoffparser.mli blastoff.native \
		graphblas graphblas.o \


# Building the tarball

TESTS = \
  func1

FAILS = \
  assign

TESTFILES = $(TESTS:%=test-%.bl) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.bl) $(FAILS:%=fail-%.out)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags blastoff.ml blastoffparser.mly \
	README scanner.mll semant.ml testall.sh \
	arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%)

blastoff.tar.gz : $(TARFILES)
	cd .. && tar czf blastoff/blastoff.tar.gz \
		$(TARFILES:%=blastoff/%)

