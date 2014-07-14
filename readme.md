# Building and Running:

First things first you're going to need is an OCaml compiler/interpreter. You
can usually obtain this using your OS's package manager. For example, on OSX
you can run `brew install ocaml`, or on Debian based Linux distros 
(for example Ubuntu) you can run `apt-get install ocaml`.

To build naga, you first need to retrieve and build its dependencies. 
Since these are handled with git submodules, it's fairly easy to do:
    
    $ git submodule init
    $ git submodule update
    $ make libs

Then, you just have to run make to build the naga server program `nagad`:

    $ make
        ...
    $ ./nagad

# API Documentation

The NAGA server exposes an HTTP interface that allows you to manipulate and
query the knowledge graph using JSON.

## Graph Representation

Since essentially all API methods involve transferring a graph between the
client and the server, NAGA uses a standard graph representation for these
graphs. It looks like this:

    {
        "node A": [ { "label": "some label", "to": "some node" },
                    { "label": "other label", "to": "other node" } ],
        "some node": [ { "label": "some label", "to": "other node" } ]
    }

Graphs in NAGA are represented as an *adjacency list*, the top-level key
(for example "node A") is a vertex in the graph, and it's value is a list
of it's edges. Each edge has two fields, a "label" that is the label for that
edge and, a "to" field which is the name of the vertex on the opposite edge.
All edges are directional, there is an edge from "node A" to "some node" but
not from "some node" to "node A". Also, there are no repeated edges. You can
supply them in your message but they will removed.

In the following sections I will refer to this representation as a 'graph'.

## Endpoints 

### /graph

Performing a __GET__  request against this endpoint will return the current
knowledge graph in the form explained above. Python example 
(using the requests library):

    import requests
    response = requests.get("/graph")

If you perform a __POST__ request against this endpoint and the body
of the __POST__ request is a graph then the knowledge graph managed by 
the server is updated to contain the union of the submitted graph and the
graph on the server.

    import requests, json
    graph = ...
    response = requests.post("/graph", data = json.dumps(graph))

### /query

If you perform __POST__ request against this endpoint and the body
of the __POST__ request is a query graph, the query will be performed and
the results will be returned. A query graph is supplied in exactly the same
format as any other graph except that any vertices or edge-labels that start
with a capital letter are considered to be variables.

The result of a query is a list of "query result" objects. A query result object
has two fields. The "graph" field contains the graph that was matched to the
query graph. The "context" field contains a mapping from variables -> values,
if you supplied a query graph with the variable "A", then the context would contain
a mapping { ... "A": "value" ... } assuming that the variable "A" was bound
to the value "value".

    import requests, json
    query_graph = ...
    response = requests.post("/query", data = json.dumps(query_graph))
