.PHONY: clean

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = 

bin = naga

par_prefix = Datalog
lex_prefix = $(par_prefix)Lex
yacc_prefix = $(par_prefix)Parse

objects = $(yacc_prefix).cmo $(lex_prefix).cmo 
objects += Datalog.cmo Naga.cmo

$(bin): $(objects)
	ocamlc $(OCAMLFLAGS) -o $@ $^

Datalog.cmi: Datalog.cmo

$(yacc_prefix).cmi: Datalog.cmi
$(yacc_prefix).cmo: Datalog.cmi $(yacc_prefix).cmi
$(lex_prefix).cmo: $(yacc_prefix).cmi

$(lex_prefix).ml: $(par_prefix).mll
	ocamllex $(OCAMLLEX_FLAGS) -o $@ $<

$(yacc_prefix).ml $(yacc_prefix).mli: $(par_prefix).mly
	ocamlyacc $(OCAMLYACC_FLAGS) -b$(yacc_prefix) $<

%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c $<

clean:
	-rm -f $(bin) *.cmo *.cmi
	-rm -f $(lex_prefix).ml $(wildcard $(yacc_prefix).ml*)
