# com.danielkeogh.graph

This library contains graph datastructures and algorithms.

[Getting started](#getting-started)

[What is a graph?](#what-is-a-graph)

[Graph API](#graph-api)

[Algorithms API](#algorithms-api)

[History and inspiration](#history-and-inspiration)

# Getting started

Create and manipulate graphs like this:

```lisp
(defpackage #:my-package
  (:use #:cl)
  (:local-nicknames (:com.danielkeogh.graph :g)))

(in-package #:my-package)

(let ((graph (g:make-bidirectional-graph)))
  (g:add-vertices graph 0 1)
  (g:add-edge graph 0 1))
```

The `com.danielkeogh.graph` system consists of multiple packages. These are as follows:

#### com.danielkeogh.graph

The main API for building and traversing graphs.

#### com.danielkeogh.graph.algorithms

A collection of useful algorithms that can be used against graphs that implement the API.

## Structure specific packages

Each graph implementation also has its own package that you can use if you want to avoid the performance overhead of using generics in the main API. These are often compiled with `(declare (optimize (speed 3) (safety 0)))`, and so not recommended by default.

* `com.danielkeogh.graph.adjacency`
* `com.danielkeogh.graph.bidirectional`
* `com.danielkeogh.graph.bidirectional-matrix`
* `com.danielkeogh.graph.undirected`
* `com.danielkeogh.graph.edge`

# What is a graph?

A graph is a datastructure that represents connections between nodes. In [graph theory](https://en.wikipedia.org/wiki/Graph_theory) these nodes are called [vertices](#vertex) and the links between them are called [edges](#edge).

## Defining terms

The names used in the API are derived from graph theory terminology. For those unfamiliar with graph theory, or for those needing a refresher, here are some definitions:

### Vertex

A vertex is a node or point in a graph. Multiple are called vertices. ("Vertexes" is a valid alternative, but is less preferred by most surveyed programmers.)

### Edge

An edge is a connection between two vertices. Edges may be directed or undirected, also known as arcs or lines.

### Parallel Edges

If a graph allows more than one edge between the same pair of vertices, it is a [multi-graph](#multi-graph)

### Multi-graph

A graph that allows more than one edge between the same pair of vertices.

### Directed Graph

A graph where each edge has a direction. That is an edge from A to B is not the same as an edge from B to A.

### Directed Acyclic Graph

A directed acyclic graph (DAG) is a [Directed Graph](#directed-graph) where no edges form any cycles or loops.

### Strongly connected

Vertices are considered strongly connected when they are a part of a loop. That is, any given vertex in the set of strongly connected vertices can be used as a starting point to traverse to any other.

### Clique

A clique is a [complete](#complete-graph) sub-graph of another graph. It is a subset of vertices where all vertices are connected via edges to all other vertices in the set.

### Maximum Clique

The [clique](#clique) with the maximum number of vertices in a graph.

### Maximal Clique

A [clique](#clique) that is not sub-graph of any other clique.

### Complete Graph

An undirected graph in which every vertex is connected to every other vertex by a single edge.

### Graph Partitioning

[Graph Partioning](https://en.wikipedia.org/wiki/Graph_partition) is the process of dividing a graph into smaller sub-graphs by grouping sets of related vertices into a single vertex. The edges connecting the grouped vertices ot other vertices are maintained. A well-known algorithm for graph partitioning is the [Kernighan-Lin algorithm](#kerninghan-lin-algorithm).

# Graph API

## Constructors

These are functions that instantiate the different graph and edge types supported by this library.

If you need help choosing, `make-bidirectional-graph` and `make-undirected-graph` are the most flexible graph implementations for directed and undirected graphs respectively.

### make-adjacency-graph

`(make-adjacency-graph &key (allow-parallel-edges t) (vertex-equality-fn #'eql))`

Create directed graph optimized for `out-edges` access.

* `allow-parallel-edges` controls whether multiple edges can exist between the same two vertices
* `vertex-equality-fn` should be a hash-table compatible comparer function.

### make-bidirectional-graph

`(make-bidirectional-graph &key (allow-parallel-edges t) (vertex-equality-fn #'eql))`

Create a directed graph optimized for `out-edges` and `in-edges` access.

* `allow-parallel-edges` controls whether multiple edges can exist between the same two vertices
* `vertex-equality-fn` should be a hash-table compatible comparer function.

### make-bidirectional-matrix-graph

`(make-bidirectional-matrix-graph vertex-count)`

Create a directed graph optimized for finding an edge between any two vertices quickly. This graph type does not support `add-vertex` or `remove-vertex`.

* `vertex-count` determines the number of vertices in a graph, which are represented as `fixnum` starting from `0`.

### make-undirected-graph

`(make-undirected-graph &key (allow-parallel-edges t) (vertex-equality-fn #'eql))`

Create an undirected graph optimized for finding edges of a given vertex quickly.

* `allow-parallel-edges` controls whether multiple edges can exist between the same two vertices
* `vertex-equality-fn` should be a hash-table compatible comparer function.

### make-edge

`(make-edge source target)`

Create an edge that can be added to graphs between two vertices.

## Builders

These are functions and methods for modifying the edges and vertices in a graph.

### add-vertex

`(add-vertex graph vertex)`

Add a vertex to a graph that supports the adding of new vertices. (I'm not sure what you expected.)

### add-vertices

`(add-vertices graph &rest vertices)`

Add a set of `vertices` to a graph.

### add-edge

`(add-edge graph edge)`

Add an edge to a graph.

### add-edges

`(add-edges graph &rest edges)`

Add a set of `edges` to a graph.

### add-edges-and-vertices

`(add-edges-and-vertices graph &rest edges)`

Add a set of `edges` to a graph, also adding any vertices in those edges that aren't in a graph yet.

### add-edge-between

`(add-edge-between graph vertex1 vertex2)`

Create and add an edge to a graph between two vertices.

### add-edges-and-vertices-between

`(add-edges-and-vertices-between graph &rest pairs)`

Create and add edges and any vertices not already a part of a graph in a plist.

e.g.

```lisp
(add-edges-and-vertices-between graph
    (1 2)
    (1 3)
    (2 3))
```

### remove-vertex

`(remove-vertex graph vertex)`

Remove a vertex and all connecting edges from a graph.

### remove-edge

`(remove-edge graph vertex)`

Remove an edge between two vertices.

### remove-edge-between

`(remove-edge-between graph vertex1 vertex2)`

Remove any edges in a graph between two vertices.

## Tests and Predicates

### is-directed

`(is-directed graph)`

Return `t` if it's a directed graph.

### has-vertex

`(has-vertex graph vertex)`

Return `t` if the vertex is in a graph.

### has-edge

`(has-edge graph edge)`

Return `t` if the edge is in a graph.

### has-edge-between

`(has-edge-between graph vertex1 vertex2)`

Return `t` if there is at least one edge between `vertex1` and `vertex2`.

### vertex-equals

`(vertex-equals graph vertex1 vertex2)`

Use a graph's equality function to check if any pair of vertices are equal.

## Graph Accessors

These are functions and methods that access the edges and vertices of a graph.

### for-in-out-edges

`(for-in-out-edges graph vertex fn)`

Apply the function `fn` to all edges connected to a given `vertex`.

* `fn` should be like `(lambda (edge) ...)`

### for-vertices

`(for-vertices graph fn)`

Apply the function `fn` to all vertices in a graph.

* `fn` should be like `(lambda (vertex) ...)`

### for-roots

`(for-roots graph fn)`

Apply the function `fn` to all vertices without in-edges a directed graph.

* `fn` should be like `(lambda (vertex) ...)`

### for-edges

`(for-edges graph fn)`

Apply the function `fn` to all edges in a graph.

* `fn` should be like `(lambda (edge) ...)`

### adjacent-edges

`(adjacent-edges graph vertex)`

Return a list of the edges connected to a vertex in an undirected graph.

### out-edges

`(out-edges graph vertex)`

Return a list of the outbound edges of a vertex in a directed graph.

### in-edges

`(in-edges graph vertex)`

Return a list of the inbound edges of a vertex in a directed graph.

### vertices

`(vertices graph)`

Return a list of all vertices in a graph.

### roots

`(roots graph)`

Return a list of all vertices with no inbound edges in a directed graph.

### edges

`(edges graph)`

Return a list of all edges in a graph.

### vertex-count

`(vertex-count graph)`

Return the number of vertices in a graph.

### edge-count

`(edge-count graph)`

Return the number of edge in a graph.

### graph-vertex-equality-fn

`(graph-vertex-equality-fn graph)`

Return the function that is used to check if two vertices are the same for a graph.

* The function will take the form of `(lambda (vertex1 vertex2) ...)` and return `t` if the vertexes are equal.
* For most graphs types, the default value type is `eql`.

## Edge Readers

Functions to access slots in `edge` objects.

### edge-source

`(edge-source edge)`

Returns the source vertex of an edge.

* Edges in an undirected graph still have a source and target value.

### edge-target

`(edge-target edge)`

Returns the target vertex of an edge.

* Edges in an undirected graph still have a source and target value.

## Dynamic Builders

These functions and macros are for building a graph without using the graph as a parameter.

### with-graph*

`(with-graph* (graph) &body body)`

A macro to set the dynamic variable `*graph*`, which is used by the dynamic graph building functions.

### add-vertex*

`(add-vertex* vertex)`

Add a vertex to `*graph*`.

### add-edge*

`(add-edge* edge)`

Add an edge to `*graph*`.

### add-edges-and-vertices*

`(add-edges-and-vertices* &rest edges)`

Add a set of edges to `*graph*`. Also ensure all new vertices are added.

### add-edges-and-vertices-between*

`(add-edges-and-vertices-between* &rest pairs)`

Make a set of edges from a `plist` and add them to a graph. Also ensure all new vertices are added.

e.g.

```lisp
(add-edges-and-vertices-between*
   (1 2)
   (1 3)
   (2 3))
```

## Utilities

Helpful utilities.

### pretty-print

`(pretty-print graph &optional (stream t))`

Prints the list of all vertices and then edges in the graph to `stream`.
e.g.

```
VERTICES: 1, 2, 3
EDGES: 1->2, 2->3, 1->3
```

### graph-equals

`(graph-equals graph1 graph2)`

Check if two graphs have the same set of edges and vertices.

## Conditions

Conditions that can be thrown by this library.

### unsupported-generic

`(unsupported-generic :supported-by)`

Will be thrown when a method cannot be implemented by a given graph type. This includes, but is not limited to:

* Trying to access `out-edges` or `in-edges` of an undirected graph.
* Trying to access `adjacent` edges of a directed graph.
* Trying to `add-vertex` or `remove-vertex` from a `bidirectional-matrix-graph`

The `supported-by` slot describes the types that are supported by the method as a helpful hint.

# Algorithms API

This library provides a collection of popular graph-traversing algorithms for your convenience.

## Search Algorithms

### Bidirectional Depth First Search Algorithm

### Breadth First Search Algorithm

### Depth First Search Algorithm

## Connected Components Algorithms

### Strongly Connected Components Algorithm

Also see [utilities](#connected-components-algorithm-utilities) for convience functions related to this algorithm.

### Weakly Connected Components Algorithm

Also see [utilities](#connected-components-algorithm-utilities) for convience functions related to this algorithm.

### Connected Components Algorithm Utilities

## Condensation Algorithms

### Condensate Vertices Algorithm

This algorithm condensates either a graphs strongly or weakly connected sets of vertices into a new graph, where each vertex is a graph containing the condensed vertices and their connected edges.

TODO: Decide whether or not condensation just belongs under (Connnected Components Algorithm Utilities)[#connected-component-algorithm-utilities]

## Graph Partition Algorithms

### Kerninghan-Lin Algorithm

# History and inspiration

This library has largely been inspired by the library [QuikGraph](https://github.com/KeRNeLith/QuikGraph), with many ideas and algorithms ripped from it wholesale.

I wrote this library because I could not find any graph libraries that I loved in the Common Lisp ecosytem and when I have time, I enjoy solving [Project Euler](https://projecteuler.net/) problems, which are often sanely modelled in graphs.
