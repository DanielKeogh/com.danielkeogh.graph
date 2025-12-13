;;;; test/algorithms/condensate-vertices.lisp

(in-package :com.danielkeogh.graph-tests.algorithms)

(def-suite condensate-vertices-tests
  :description "Tests for condensate-vertices algorithm")

(in-suite condensate-vertices-tests)

(test condensate-strongly-connected-vertices
  (let ((g (api:make-bidirectional-graph)))
    (api:add-edges-and-vertices g
      ;; loop 1
      (api:make-edge 0 1) (api:make-edge 1 0)

      ;; loop 2
      (api:make-edge 2 3) (api:make-edge 3 4)
      (api:make-edge 4 5) (api:make-edge 5 2)
      
      ;; loop-with-tail
      (api:make-edge 6 7) (api:make-edge 7 8)
      (api:make-edge 8 8) (api:make-edge 8 9)

      ;; loop-with-head
      (api:make-edge 10 11) (api:make-edge 11 12)
      (api:make-edge 12 13) (api:make-edge 13 11)

      ;; self-loop
      (api:make-edge 14 14)

      ;; line
      (api:make-edge 15 16) (api:make-edge 16 17))
    (let ((condensed-graph (alg:condensate-strongly-connected-vertices g)))
      (is (= 12 (api:vertex-count condensed-graph))))))
