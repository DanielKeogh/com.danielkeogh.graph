;;;; tests/structures/package.lisp

(defpackage #:com.danielkeogh.graph-tests.structures
  (:use #:cl #:fiveam)
  (:local-nicknames (#:bidirectional #:com.danielkeogh.graph.bidirectional)
                    (#:edge #:com.danielkeogh.graph.edge)
                    (#:api #:com.danielkeogh.graph))
  (:export #:run!
           #:bidirectional-graph-tests))

