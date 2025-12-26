;;;; tests/algorithms/package.lisp

(defpackage #:com.danielkeogh.graph-tests.algorithms
  (:use #:cl #:fiveam)
  (:local-nicknames (#:alg #:com.danielkeogh.graph.algorithms)
                    (#:api #:com.danielkeogh.graph)
                    (#:edge #:com.danielkeogh.graph.edge)
                    (#:utils #:com.danielkeogh.graph.utils))
  (:export #:run!
           #:bidirectional-breadth-first-search-tests
           #:breadth-first-search-tests
           #:condensate-vertices-tests
           #:depth-first-search-tests
           #:undirected-depth-first-search-tests
           #:kernighan-lin-partition-tests
           #:minimum-spanning-tree-tests
           #:strongly-connected-components-tests
           #:weakly-connected-components-tests))
