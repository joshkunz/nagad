# Nagad

This is an implementation of the basic query algorithm described in
[NAGA: Searching and Ranking Knowledge](http://www.webcitation.org/6errPGHTQ)
over a non-persistent graph database.

The directory `interactive-naga` contains a very simple implementation that
is built around a datalog-like query interface. The version in `server` answers
queries posed in a json format over HTTP (see below for details). 
To build the old interactive version, see the readme in that directory.

This is code that was developed by me at the University of Utah. I'm re-hosting
it here for disoverability (github has good google-juice for code). The
"canonical" repository can be found on Flux Research Group's Gitlab installation:
<https://gitlab.flux.utah.edu/kg/naga-impl>. I put canonical in quotes because
this code is no longer used, and is being posted here for reference only.

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

On success, the status code __200__ will be returned and the json-representation
of the knowledge graph will be in the response body. *Example response graph*:

    {"a": [{"label": "b", "to": "q"}, {"label": "b", "to": "z"}],
     "b": [{"label": "c", "to": "x"}, {"label": "c", "to": "z"}],
     "z": [{"label": "b", "to": "a"}]}

If you perform a __POST__ request against this endpoint and the body
of the __POST__ request is a graph then the knowledge graph managed by 
the server is updated to contain the union of the submitted graph and the
graph on the server.

    import requests, json
    graph = ... 
    response = requests.post("/graph", data = json.dumps(graph))

An empty response and __200__ response code means that the update was
successful.

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

On success, the response code will be __200__ and the query result in JSON format
will be in the response body. *Example response*:

    [{"context": {"A": "z", "B": "b", "C": "a"},
      "graph": {"a": [{"label": "b", "to": "z"}],
                 "z": [{"label": "b", "to": "a"}]}},
     {"context": {"A": "a", "B": "b", "C": "z"},
      "graph": {"a": [{"label": "b", "to": "z"}],
                 "z": [{"label": "b", "to": "a"}]}}]
