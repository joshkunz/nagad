.PHONY: clean default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = 

lib = jsonm.cma

depfile = Makefile.d

uutf_src = $(wildcard uutf/src/*)
jsonm_src = $(wildcard jsonm/src/*)
source = $(notdir $(uutf_src) $(jsonm_src))
interfaces = $(patsubst %.mli,%.cmi,$(filter %.mli,$(source)))
objects = $(patsubst %.ml,%.cmo,$(filter %.ml,$(source)))

$(lib): $(objects) 
	ocamlc $(OCAMLFLAGS) -a -o $@ $^

jsonm.cmo: uutf.cmo

uutf.ml uutf.mli: $(uutf_src) 
	cp $^ .

jsonm.ml jsonm.mli: $(jsonm_src)
	cp $^ .

$(depfile): $(source)
	ocamldep $(OCAMLFLAGS) $^ > $@

-include $(depfile) 

%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c $<

clean:
	-rm -f $(objects) $(interfaces) $(lib) $(depfile) $(source)
