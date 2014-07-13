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
local_libs = json http 
depfile = Makefile.d

default: $(bin)

$(depfile): $(sources)

abs_libs = $(addprefix ../lib/,$(addsuffix .cma,$(local_libs)))
$(bin): $(abs_libs) $(objects)
	ocamlc $(OCAMLFLAGS) -o $@ $(addsuffix .cma,$(system_libs)) $^

clean:
	-rm -f $(bin) $(objects) $(interfaces) $(depfile)

include ../common/Makefile.implicit
-include $(depfile) 
