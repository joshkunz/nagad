.PHONY: clean libs

OCAMLLEX_FLAGS =
OCAMLYACC_FLAGS = 
OCAMLFLAGS = 

libsrc = lib
srvsrc = server
clisrc = client

srvbin = nagad

$(srvbin):
	$(MAKE) -C $(srvsrc) $@
	cp $(srvsrc)/$@ .

%.cma: 
	@echo "****************** Error ********************"
	@echo "It looks like you haven't set up the required"
	@echo "libraries yet (missing $@)."
	@echo "You can build them by running:"
	@echo "    $$ git submodule init"
	@echo "    $$ git submodule update"
	@echo "    $$ make libs"
	@echo "From the root of the repository."
	@exit 1

libs:
	$(MAKE) -C $(libsrc) libs

clean:
	$(MAKE) -C $(libsrc) clean
	$(MAKE) -C $(srvsrc) clean
	-rm $(srvbin)
