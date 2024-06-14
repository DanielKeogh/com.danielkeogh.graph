# com.danielkeogh.graph

This library contains graph datastructures and algorithms.

[Getting started](#getting-started)

[What is a graph?](#what-is-a-graph)

[API](#api)

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

# What is a graph?

A graph is a datastructure that represents connections between nodes. In [graph theory](https://en.wikipedia.org/wiki/Graph_theory) these nodes are called [verticies](#vertex) and the links between them are called [edges](#edge). 

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

The [clique](#clique) with the maximum number of vertices in the graph.

### Maximal Clique

A [clique](#clique) that is not sub-graph of any other clique.

### Complete Graph

An undirected graph in which every vertex is connected to every other vertex by a single edge.

### Graph Partitioning

[Graph Partioning](https://en.wikipedia.org/wiki/Graph_partition) is the process of dividing a graph into smaller sub-graphs by grouping sets of related vertices into a single vertex. The edges connecting the grouped vertices ot other vertices are maintained. A well-known algorithm for graph partitioning is the [Kernighan-Lin algorithm](#kerninghan-lin-algorithm).

# API

## Constructors

# History and inspiration

This library has largely been inspired by the library [QuikGraph](https://github.com/KeRNeLith/QuikGraph), with many ideas and algorithms ripped from it wholesale.

I wrote this library because I could not find any graph libraries that I loved in the Common Lisp ecosytem and when I have time, I enjoy solving [Project Euler](https://projecteuler.net/) problems, which are often sanely modelled in graphs.


# Algorithms

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

