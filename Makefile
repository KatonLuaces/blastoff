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
	ocamlc -o $@ $(OBJS) ast_test.cmo

tests/%.out: %.in ast_test
	ast_test $< > $@

run_ast_test: $(wildcard tests/*.out)
