.PHONY: clean libs

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = 

libsrc = lib
srvsrc = server
clisrc = client

$(libsrc)/%.cma: 
	@echo "****************** Error ********************"
	@echo "It looks like you haven't set up the required"
	@echo "libraries yet (missing $@)."
	@echo "You can build them by running:"
	@echo "    $$ make libs"
	@echo "From the root of the repository. This is split"
	@echo "into a seperate command because it clones"
	@echo "repositories off of the internet by running:"
	@echo "    $$ git submodule init"
	@echo "    $$ git submodule update"
	@echo "    $$ make libs"

libs:
	git submodule init
	git submodule update
	$(MAKE) -C $(libsrc) libs

clean:
	$(MAKE) -C $(libsrc) clean
	$(MAKE) -C $(srvsrc) clean
