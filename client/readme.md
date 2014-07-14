# Documentation

The Naga program is primarily concerned with the manipulation and querying
of a "Fact Database" or "Knowledge Graph" (the two terms are synonymous). The
database is manipulated and queried through the use of the Datalog language. For
example, we can add fact to the fact database by using the `fact` statement:

    > fact(a, b, c).

By entering the above into the REPL we are notifying Naga of an edge in the
knowledge graph that connects node 'a' to node 'c' via an edge labeled 'b'.
We can specify edges multiple times. For example, if we wanted to say that there
were two links from node 'a' to node 'c' labeled 'b' we could enter that
command again.

A listing of all facts currently known (i.e. previously entered) by Naga can
be obtained by using the command `facts`:

    > facts.
    fact(a, b, c).

We can also save a snapshot of the current fact database for later by using
an extended version of the `facts` command. We can also render a PDF representation
of the fact database as a graph by using the `graph` command:

    > facts("example.facts").
    > graph("example.pdf").

This code snippet above also shows another feature of the language, escaped 
strings. In the previous snippets we had omitted the double-quotes since we
weren't using any reserved characters like `(`, `)`, `,`, or `.`. However, to spell
file-names we need to use the `.` character so we escaped the string. Escaped
strings can be used anywhere un-escaped strings are, but they are considered
variables if the first character in the string is upper-case (more on that next).

Also, previously-saved fact databases can be added to the current fact-database
by using the `source` command:

    > source("example.facts").

To help aid in the writing of longer programs that can be supplied to the
Naga program with the `-f` switch, comments can be written by using the
`#` character. Comments proceed from the `#` character to the end of the line.

## Queries

The real power of the Naga system is its ability to let you query the fact
database. Remember that the fact database is just another way of saying
"the knowledge graph". A query in Naga is specified as a graph where nodes
and edges can be *variables*. The result of a query is a set of sub-graphs 
of the knowledge graph that 'match' the query graph. A subgraph 'matches' the
query graph if the edge and node labels match the sub-graph's edge and node labels
and any variables can be assigned consistently. Now, this may seem a little
abstract, so lets look at an example. Lets say we have a fact database that looks
like this:
    
    > facts.
    fact(not-c, not-b, not-f).
    fact(not-a, not-b, not-c).
    fact(c, b, f).
    fact(a, b, c).

Now, lets say we want to find a subgraphs that have end-nodes X and Y, and
intermediate node Z, and all edges are of type Q. We can have naga find all
of these for us by writing:

    > fact(X, Q, Z), fact(Z, Q, Y).
    Result 0:
    fact(not-c, not-b, not-f).
    fact(not-a, not-b, not-c).
    Result 1:
    fact(c, b, f).
    fact(a, b, c).
    End of results.

You can see that Naga found all sub-graphs that matched that abstract description
and then wrote their facts out. Remember that all strings that start with 
an upper-case letter are considered to be variables, if we wanted to only
include results where the edge Q was equal to 'b', we could re-write our
query to be:

    > fact(X, b, Z), fact(Z, b, Y).
    Result 0:
    fact(c, b, f).
    fact(a, b, c).
    End of results.

As you can see, the first result is now omitted since it's edges were of type
'not-b'. You can also see how we built the graph by chaining together multiple
fact-statements. Since each fact is an edge, this is an easy way of specifying
a graph.

Now, even though Naga is capable of running these queries and discovering the
subgraphs, trying to read them is this textual form can be quite difficult and
error-prone. To help make understanding the results easier, the result graphs can
be rendered as a pdf by using the structure:

    > graph("example-result.pdf") :- fact(X, b, Z), fact(Z, b, Y).

Which will render the query we provided on the right-hand site of the `:-`
(implication) using the graph command we specified on the left-hand side.

## Command Listing

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
