# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : blastoff.native printbig.o

# "make blastoff.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

blastoff.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind blastoff.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff *.ll blastoffparser.ml blastoffparser.mli

# Testing the "printbig" example

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# Building the tarball

TESTS = \
  func1 

FAILS = \
  assign

TESTFILES = $(TESTS:%=test-%.bl) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.bl) $(FAILS:%=fail-%.out)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags blastoff.ml blastoffparse.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

blastoff.tar.gz : $(TARFILES)
	cd .. && tar czf blastoff/blastoff.tar.gz \
		$(TARFILES:%=blastoff/%)
