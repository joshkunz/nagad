Naive implementation of the naga query language and graph representation based
heavily on the algorithms given in the naga paper.

# Building and Running:

First things first you're going to need is an OCaml compiler/interpreter. You
can usually obtain this using your OS's package manager. For example, on OSX
you can run `brew install ocaml`, or on Debian based Linux distros you can run
`apt-get install ocaml`.

### Running Under an Interpreter

    $ ocaml naga.ml

### Running Natively

    $ make
    ocamlc -c naga.ml
    ocamlc -o naga naga.cmo
    $ ./naga

Once you have the interpreter running you can type in facts in the form:
`fact(X, Y, Z).<ENTER>`. When you're done typing in facts you can type:
`finish.<ENTER>` and the system will echo your facts back to you.

# Status:

* Parsing fact statements given on standard in into a fact table.
