# com.danielkeogh.graph

This library contains graph datastructures and algorithms.

[Getting started](# Getting started)
[What is a graph?](# What is a graph?)
[API](# API)
[History and inspiration](#History and inspiration)

# Getting started

Create and manipulate graphs like this:

```lisp
(defpackage #:my-package
  (:use #:cl)
  (:local-nicknames (:com.danielkeogh.graph :g)))

(in-package #:my-package)

(let ((graph (g:make-bidirectional-graph)))
  (g:add-vertex graph 0)
  (g:add-vertex graph 1)
  (g:add-edge 0 1))
```

# What is a graph?

A graph is a datastructure that represents links between nodes. In [graph theory](https://en.wikipedia.org/wiki/Graph_theory) these nodes are called [verticies](### Vertex) and the links between them are called [edges](### Edge). 

## Defining terms

The names used in the API are derived from graph theory terminology. For the sake of those who've never studied graph theory, or for those that haven't looked at it for a while, here are some definitions.

### Vertex

A vertex is a node or point in a graph. Multiple are called vertexes or vertices. We prefer to use "vertices" in this library.

### Edge

A link between two vertices. Edges may be directed or undirected. Also called arcs, or lines.

### Parallel Edges

If a graph allows more than one edge between the same pair of vertices, it is a [multi-graph](### Multi-graph)

### Multi-graph

A graph that allows more than one edge between the same pair of vertices.

### Directed Graph

A graph where each edge has a direction. That is an edge from A to B is not the same as an edge from B to A.

### Directed Acyclic Graph

A directed acyclic graph (DAG) is a [Directed Graph](### Directed Graph) where no edges form any cycles or loops.

### Strongly connected

Vertices are considered strongly connected when they are a part of a loop. That is, any given vertex in the set of strongly connected vertices can be used as a starting point to traverse to any other.

# API

## Constructors

### `make-bidirectional-graph`

### `make-adjacency-graph`

# History and inspiration

This library has largely been inspired by the library [QuikGraph](https://github.com/KeRNeLith/QuikGraph), with many ideas and algorithms ripped from it wholesale.

I (Daniel Keogh) wrote this library because I could not find any fantastic graph libraries that are part of the common lisp ecosytem. When I have time, I enjoy solving [Project Euler](https://projecteuler.net/) problems, which are often most sanely modelled in graphs.