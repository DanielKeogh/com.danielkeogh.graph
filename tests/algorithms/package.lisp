;;;; tests/algorithms/package.lisp

(defpackage #:com.danielkeogh.graph-tests.algorithms
  (:use #:cl #:fiveam)
  (:local-nicknames (#:alg #:com.danielkeogh.graph.algorithms)
                    (#:api #:com.danielkeogh.graph)
                    (#:edge #:com.danielkeogh.graph.edge)
                    (#:utils #:com.danielkeogh.graph.utils))
  (:export #:run!
           #:depth-first-search-tests))
