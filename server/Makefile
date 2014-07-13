.PHONY: clean default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = -thread -I ../include/

bin = nagad

sources = KG.ml Query.ml Nagad.ml 
interfaces = $(patsubst %.mli,%.cmi,$(filter %.mli,$(sources)))
interfaces += $(patsubst %.ml,%.cmi,$(filter %.ml,$(sources)))
objects = $(patsubst %.ml,%.cmo,$(filter %.ml,$(sources)))
libs = json http 
depfile = Makefile.d

abs_libs = $(addprefix ../lib/,$(addsuffix .cma,$(libs)))

default: $(bin)

$(depfile): $(sources)

$(bin): $(abs_libs) $(objects)
	ocamlc $(OCAMLFLAGS) -o $@ $^

clean:
	-rm -f $(bin) $(objects) $(interfaces) $(depfile)

include ../common/Makefile.implicit
-include $(depfile) 
