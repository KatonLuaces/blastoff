#
# Patterns
#

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

#
# Compilation targets
#

# TBD

#
# Tests
#

ast_test: ast_test.cmo
	ocamlc -o $@ $<

scanner_test: scanner_test.cmo
	ocamlc -o $@ $<

tests/ast/%.out: %.in ast_test
	ast_test $< > $@

tests/scanner/%.out: %.in scanner_test
	scanner_test $< > $@

run_ast_test: $(wildcard tests/ast/*.out)

run_scanner_test: $(wildcard tests/scanner/*.out)
