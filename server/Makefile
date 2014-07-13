.PHONY: clean default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS =  -thread -I ../include/

bin = nagad

sources = KG.ml Query.ml Nagad.ml 
interfaces = $(patsubst %.mli,%.cmi,$(filter %.mli,$(sources)))
interfaces += $(patsubst %.ml,%.cmi,$(filter %.ml,$(sources)))
objects = $(patsubst %.ml,%.cmo,$(filter %.ml,$(sources)))
system_libs = unix threads
local_libs = jsonm http 
depfile = Makefile.d
libloc = ../lib
abs_libs = $(addprefix $(libloc)/,$(addsuffix .cma,$(local_libs)))

default: $(bin)

$(abs_libs):
	$(foreach lib,$(addsuffix .cma,$(local_libs)),$(MAKE) -C $(libloc) $(lib);)

$(depfile): $(sources)

$(bin): $(abs_libs) $(objects)
	ocamlc $(OCAMLFLAGS) -o $@ $(addsuffix .cma,$(system_libs)) $^

clean:
	-rm -f $(bin) $(objects) $(interfaces) $(depfile)

include ../common/Makefile.implicit
-include $(depfile) 
