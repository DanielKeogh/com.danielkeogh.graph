;;;; tests/algorithms/minimum-spanning-tree.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite minimum-spanning-tree-tests
  :description "Tests for minimum-spanning-tree algorithm")

(in-suite minimum-spanning-tree-tests)

(test find-edges
  (let ((g (api:make-bidirectional-graph))
        (costs (make-hash-table))
        (is-minimal (make-hash-table)))
    (labels ((e (source target weight &optional minimal)
               (let ((edge (api:make-edge source target)))
                 (setf (gethash edge costs) weight)
                 (setf (gethash edge is-minimal) minimal)
                 edge)))
      (api:add-edges-and-vertices g
        (E 0 1 16 t)
        (E 0 2 12 t)
        (E 0 3 21)
        (E 1 3 17 t)
        (E 1 4 20)
        (E 2 3 28)
        (E 2 5 31)
        (E 3 4 18 t)
        (E 3 5 19 t)
        (E 3 6 23)
        (E 4 6 11 t)
        (E 5 6 27))

      (loop for edge in (alg:minimum-spanning-tree g (lambda (edge) (gethash edge costs)))
            do (is-true (gethash edge is-minimal))))))
