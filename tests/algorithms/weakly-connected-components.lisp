;;;; test/algorithms/weakly-connected-components.lisp

(in-package :com.danielkeogh.graph-tests.algorithms)

(def-suite weakly-connected-components-tests
  :description "Tests for weakly-connected-components algorithm")

(in-suite weakly-connected-components-tests)

(test weakly-connected-components
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
    (multiple-value-bind (components component-count)
        (alg:weakly-connected-components g)
      (let* ((graphs (alg:connected-components->graphs g components component-count)))
        (is (= 6 (length graphs)))
        (is-true (find (api:with-graph* ((api:make-bidirectional-graph))
                         (api:add-edges-and-vertices-between*
                          0 1
                          1 0))
                       graphs :test #'api:graph-equals))

        (is-true (find (api:with-graph* ((api:make-bidirectional-graph))
                         (api:add-edges-and-vertices-between* 14 14))
                       graphs :test #'api:graph-equals))
        
        (is-true (find (api:with-graph* ((api:make-bidirectional-graph))
                         (api:add-edges-and-vertices-between*
                          10 11
                          11 12
                          12 13
                          13 11))
                       graphs :test #'api:graph-equals))))))
