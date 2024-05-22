;;;; algorithms/package.lisp

(defpackage #:com.danielkeogh.graph.algorithms
  (:use #:cl)
  (:local-nicknames
   (#:utils #:com.danielkeogh.graph.utils)
   (#:edge #:com.danielkeogh.graph.edge)
   (#:graph #:com.danielkeogh.graph))
  (:export
   ;; search
   #:bidirectional-breadth-first-search
   #:breadth-first-search
   #:depth-first-search
   ;; connected components
   #:strongly-connected-components
   #:weakly-connected-components
   #:connected-components->graphs
   ;; condensation
   #:condensate-vertices
   #:condensate-strongly-connected-vertices
   ;; ??
   #:minimum-spanning-tree))
