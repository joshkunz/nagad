.PHONY: clean libs default

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = 

libsrc = libs
build = $(libsrc)/_scratch

$(libsrc)/jsonm.cma: 
	@echo "****************** Error ********************"
	@echo "It looks like you haven't set up the required"
	@echo "libraries yet (missing $(libsrc)/jsonm.cma)."
	@echo "You can build them by running:"
	@echo "    $$ git submodule init"
	@echo "    $$ git submodule update"
	@echo "    $$ make libs"

libs:
	$(MAKE) -C $(libsrc) jsonm.cma

clean:
	$(MAKE) -C $(libsrc) clean

