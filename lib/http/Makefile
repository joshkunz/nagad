.PHONY: clean default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS =

lib = http.cma

par_prefix = HTTP
lexp = $(par_prefix)Lex
yaccp = $(par_prefix)Parse
libs = unix

interfaces = $(yaccp).mli $(wildcard *.mli)
sources = $(yaccp).ml $(lexp).ml
sources += HTTPTypes.ml HTTP.ml

objects = $(patsubst %.ml,%.cmo,$(sources)) 

depfile = Makefile.d

$(lib): $(objects)

includes: $(interfaces)
	cp $(interfaces) $(include)

$(depfile): $(yaccp).mli $(yaccp).ml $(lexp).ml
$(depfile): $(sources) $(interfaces)

$(lexp).ml: $(yaccp).mli

#$(lex_prefix).ml: $(par_prefix).mll
#	ocamllex $(OCAMLLEX_FLAGS) -o $@ $<
#
#$(yacc_prefix).ml $(yacc_prefix).mli: $(par_prefix).mly
#	ocamlyacc $(OCAMLYACC_FLAGS) -b$(yacc_prefix) $<

clean:
	-rm -f $(lib) *.cmo *.cmi *.o *.cmx
	-rm -f $(lexp).ml $(wildcard $(yaccp).ml*)
	-rm -f $(depfile)

include ../../common/Makefile.implicit
-include $(depfile) 
