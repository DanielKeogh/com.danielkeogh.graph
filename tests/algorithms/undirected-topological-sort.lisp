;;;; tests/algorithms/undirected-topological-sort.lisp

(in-package :com.danielkeogh.graph-tests.algorithms)

(def-suite undirected-topological-sort-tests
  :description "Tests for undirected-topological-sort algorithm")

(in-suite undirected-topological-sort-tests)

(test simple-graph
  (let ((g (api:make-undirected-graph)))
    (api:add-edges-and-vertices-between g
      1 2    2 3
      4 2    4 5
      5 6    7 5
      7 8)
    ;; TODO: Is this ok?
    (is (equalp #(1 2 3 4 5 6 7 8) (alg:undirected-topological-sort g :allow-cyclic-graph t)))))
