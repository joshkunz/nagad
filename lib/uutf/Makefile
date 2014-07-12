.PHONY: clean includes default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = 

lib = uutf.cma

depfile = Makefile.d

source = $(wildcard uutf-src/src/*)
interfaces = $(patsubst %.mli,%.cmi,$(filter %.mli,$(source)))
objects = $(patsubst %.ml,%.cmo,$(filter %.ml,$(source)))
I = $(foreach intf,$(interfaces),$(dir $(intf)))

$(lib): $(objects) 
$(depfile): $(source)

includes:
	cp $(interfaces) $(include)

$(objects): OCAMLFLAGS = -I $(I)

clean:
	-rm -f $(objects) $(interfaces) $(lib) $(depfile)

include ../../common/Makefile.implicit
-include $(depfile) 
