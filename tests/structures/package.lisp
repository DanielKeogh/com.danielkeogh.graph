;;;; tests/structures/package.lisp

(defpackage #:com.danielkeogh.graph-tests.structures
  (:use #:cl #:fiveam)
  (:local-nicknames (#:api #:com.danielkeogh.graph)
                    (#:adjacency #:com.danielkeogh.graph.adjacency)
                    (#:bidirectional #:com.danielkeogh.graph.bidirectional)
                    (#:edge #:com.danielkeogh.graph.edge))
  (:export #:run!
           #:bidirectional-graph-tests))

