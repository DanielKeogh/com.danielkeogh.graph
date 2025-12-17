;;;; tests/algorithms/topological-sort.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite topological-sort-tests
  :description "Tests for topological sort algorithm")

(in-suite topological-sort-tests)

(test two-elements-topological-sort
  (let ((g (api:make-adjacency-graph)))
    (api:add-edges-and-vertices-between g 1 2)
    (is (equalp #(1 2) (alg:topological-sort g))))
  (let ((g (api:make-adjacency-graph)))
    (api:add-edges-and-vertices-between g 2 1)
    (is (equalp #(2 1) (alg:topological-sort g)))))

(test topological-sort
  (let ((g (api:make-adjacency-graph)))
    (api:add-edges-and-vertices-between g
                                        1 2
                                        2 3
                                        2 6
                                        2 8
                                        4 2
                                        4 5
                                        5 6
                                        7 5
                                        7 8)
    (is (equalp #(7 4 5 1 2 3 6 8) (alg:topological-sort g))))

  (let ((g (api:make-adjacency-graph)))
    (api:add-edges-and-vertices-between g
                                        0 1
                                        1 2
                                        1 3
                                        2 3
                                        3 4
                                        5 6)
    (is (equalp #(5 6 0 1 2 3 4) (alg:topological-sort g)))))
