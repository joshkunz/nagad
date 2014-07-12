.PHONY: clean libs 

libs = jsonm.cma uutf.cma http.cma
include_dir = ../include

libs: $(libs)

$(include_dir): 
	mkdir $@ 

%.cma: %/ | $(include_dir)
	$(MAKE) -C $< $@
	$(MAKE) -C $< include="../$(include_dir)/" includes
	cp $<$@ .

clean:
	-$(foreach lib,$(libs),$(MAKE) -C "$(basename $(lib))/" clean;)
	-rm -f $(wildcard $(include_dir)/*)
	-rm -f $(libs)
