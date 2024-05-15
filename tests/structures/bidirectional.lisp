;;;; tests/structures/bidirectional.lisp

(in-package #:com.danielkeogh.graph-tests.structures)

(def-suite bidirectional-graph-tests
  :description "bidirectional graph tests")

(in-suite bidirectional-graph-tests)

(test make-graph
  (is-true (bidirectional:make-graph)))

(test add-vertex
  (let ((g (bidirectional:make-graph)))
    (is (= 0 (length (bidirectional:vertices g))))
    (bidirectional:add-vertex g 1)
    (is (= 1 (length (bidirectional:vertices g))))))

(test remove-vertex
  (let ((g (bidirectional:make-graph)))
    (bidirectional:add-vertex g 1)
    (bidirectional:remove-vertex g 1)
    (bidirectional:remove-vertex g 2)
    (is (= 0 (length (bidirectional:vertices g))))))

(test add-edge
  (let ((g (bidirectional:make-graph)))
    (bidirectional:add-vertex g 1)
    (bidirectional:add-vertex g 2)
    (bidirectional:add-edge g (edge:make-edge 1 2))
    (is (= 1 (length (bidirectional:edges g))))))

(test remove-edge
  (let ((g (bidirectional:make-graph)))
    (bidirectional:add-vertex g 1)
    (bidirectional:add-vertex g 2)
    (let ((edge (edge:make-edge 1 2)))
      (bidirectional:add-edge g edge)
      (bidirectional:remove-edge g edge))
    (is (= 0 (length (bidirectional:edges g))))))

(test has-vertex
  (let ((g (bidirectional:make-graph)))
    (is-false (bidirectional:has-vertex g 1))
    (bidirectional:add-vertex g 1)
    (is-true (bidirectional:has-vertex g 1))))

(test has-edge
  (let ((g (bidirectional:make-graph)))
    (is-false (bidirectional:has-edge g 1 2))
    (bidirectional:add-vertex g 1)
    (bidirectional:add-vertex g 2)
    (bidirectional:add-edge-between g 1 2)
    (is-true (bidirectional:has-edge g 1 2))))

(test edges
  (let ((g (bidirectional:make-graph)))
    (is-false (bidirectional:edges g))
    (bidirectional:add-vertex g 1)
    (bidirectional:add-vertex g 2)
    (bidirectional:add-edge-between g 1 2)
    (is-true (bidirectional:edges g))))

(test vertices
  (let ((g (bidirectional:make-graph)))
    (is-false (bidirectional:edges g))
    (bidirectional:add-vertex g 1)
    (bidirectional:add-vertex g 2)
    (is (equal (list 1 2) (sort (bidirectional:vertices g) #'<)))))
