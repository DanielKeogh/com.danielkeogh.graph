;;;; tests/algorithms/bidirectional-breadth-first-search.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite bidirectional-breadth-first-search-tests
  :description "Tests for bidirectional-breadth-first-search algorithm")

(in-suite bidirectional-breadth-first-search-tests)

(test touch-all-vertexes
  (let ((g (api:make-bidirectional-graph)))
    (api:add-vertex g :1)
    (api:add-vertex g :2a)
    (api:add-vertex g :2b)
    (api:add-vertex g :3)
    (api:add-vertex g :4)
    (api:add-vertex g :5)
    (api:add-vertex g :6)
    (api:add-vertex g :5a)
    (api:add-vertex g :6a)

    ;; Diamond from 1->3
    (api:add-edge-between g :1 :2a)
    (api:add-edge-between g :1 :2b)
    (api:add-edge-between g :2a :3)
    (api:add-edge-between g :2b :3)

    (api:add-edge-between g :3 :4)

    ;; Fork after 4
    (api:add-edge-between g :4 :5)
    (api:add-edge-between g :5 :6)

    (api:add-edge-between g :4 :5a)
    (api:add-edge-between g :5a :6a)

    ;; Loop
    (api:add-edge-between g :6a :5a) 

    ;; TODO
    ))
