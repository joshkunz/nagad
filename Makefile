.PHONY: clean

OCAMLFLAGS = 

objects = naga.cmo
bin = naga

$(bin): $(objects)
	ocamlc -o $@ $^

%.cmo: %.ml
	ocamlc -c $^

clean:
	-rm -f $(bin) *.cmo *.cmi
