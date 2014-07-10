.PHONY: clean default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS =

bin = test 

par_prefix = HTTP
lex_prefix = $(par_prefix)Lex
yacc_prefix = $(par_prefix)Parse
libs = unix

interfaces = $(yacc_prefix).mli $(wildcard *.mli)
sources = $(yacc_prefix).ml $(lex_prefix).ml
sources += HTTPTypes.ml HTTP.ml Test.ml

objects = $(patsubst %.ml,%.cmo,$(sources)) 

depfile = Makefile.d

default: $(bin)

$(depfile): $(yacc_prefix).mli $(yacc_prefix).ml $(lex_prefix).ml
$(lex_prefix).ml: $(yacc_prefix).mli

$(bin): $(objects)
	ocamlc $(OCAMLFLAGS) -o $@ $(addsuffix .cma,$(libs)) $^

$(lex_prefix).ml: $(par_prefix).mll
	ocamllex $(OCAMLLEX_FLAGS) -o $@ $<

$(yacc_prefix).ml $(yacc_prefix).mli: $(par_prefix).mly
	ocamlyacc $(OCAMLYACC_FLAGS) -b$(yacc_prefix) $<

$(depfile): $(sources) $(interfaces)
	ocamldep $^ > $@

clean:
	-rm -f $(bin) *.cmo *.cmi *.o *.cmx
	-rm -f $(lex_prefix).ml $(wildcard $(yacc_prefix).ml*)
	-rm -f $(depfile)

include ../common/Makefile.implicit
-include $(depfile) 
