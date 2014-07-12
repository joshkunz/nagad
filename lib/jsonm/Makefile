.PHONY: clean includes default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = -I ../../include

depfile = Makefile.d

lib = jsonm.cma

src = jsonm-src/src
source = $(wildcard $(src)/*)
interfaces = $(patsubst %.mli,%.cmi,$(filter %.mli,$(source)))
objects = $(patsubst %.ml,%.cmo,$(filter %.ml,$(source)))
I = $(foreach intf,$(interfaces),$(dir $(intf)))
libs = ../uutf.cma

$(lib): $(libs) $(objects)
$(src)/jsonm.cmo: $(libs)
$(depfile): $(source)
$(objects): OCAMLFLAGS += -I $(I)

includes: $(interfaces)
	cp $(interfaces) $(include)

../uutf.cma:
	$(MAKE) -C ../ uutf.cma

clean:
	-rm -f $(objects) $(interfaces) $(lib) $(depfile)

include ../../common/Makefile.implicit
-include $(depfile) 
