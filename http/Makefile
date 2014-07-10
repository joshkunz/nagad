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
sources += Test.ml

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

%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c $<

$(depfile): $(sources) $(interfaces)
	ocamldep $(OCAMLFLAGS) $^ > $@

clean:
	-rm -f $(bin) *.cmo *.cmi *.o *.cmx
	-rm -f $(lex_prefix).ml $(wildcard $(yacc_prefix).ml*)
	-rm -f $(depfile)

-include $(depfile) 
	
