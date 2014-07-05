Naive implementation of the naga query language and graph representation based
heavily on the algorithms given in the naga paper.

# Status:

* Parsing fact statements given on standard in into a fact table.
* Generating textual DOT graphs of the fact table.
* Parsing Queries (but not executing them yet).

# Building and Running:

First things first you're going to need is an OCaml compiler/interpreter. You
can usually obtain this using your OS's package manager. For example, on OSX
you can run `brew install ocaml`, or on Debian based Linux distros you can run
`apt-get install ocaml`.

To make naga, simply run the 'make' command, and then invoke the generated
'naga' binary.

    $ make
        ...
    $ ./naga

When invoked, the NAGA binary will start a command oriented REPL, you can
see the list of commands from within the REPL by running `help.` from
within the REPL. The Full Documentation is given in the following section.

# Documentation

The Naga REPL language is a simple Line-oriented Datalog-like language.
The Datalog language is as follows:

    statement = NAME. | NAME(values).
    values = variable [, values] | value [, values]
    variable = *starts with uppercase*
    value = *starts with lowercase*

The system is manipulated through commands, as listing of which is given below.
Commands are expressed as basic datatlog statements that provide a specific
function. The facts in the system (inserted with the 'fact' command) can be
queried by specifying a statement with at least one variable. Additionaly,
AND constraints can be placed on the query by seperating statements with
a comma.

Commands:

|   |   |
|---|---|
| `finish.`, `end.`, `done.`, `exit.` | Exit the program. |
| `fact(a, b, c).` | Add a fact to the database.|
|`facts.` | Display facts in the fact base. |
| `facts(name).` | Write a list of the facts in the fact base to a file named 'name', any files with the same name are overwritten. |
| `graph.` | Print out the DOT representation of this graph. |
| `graph(name).` | Write out a PDF of the knowledge graph to a file named 'name'. Overwrites any file with that name in this directory. |
| `source(name).` | Read and run all commands in the file named 'name' with the current fact database. This can be used to load fact databases saved previously with the 'facts' command, or to automate queries in an interactive session. |
| `help.`, `commands.` | Prints a command listing. |
