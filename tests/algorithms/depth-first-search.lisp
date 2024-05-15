;;;; tests/algorithms/depth-first-search.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite depth-first-search-tests
  :description "Tests for depth-first-search algorithm")

(in-suite depth-first-search-tests)

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

    (labels ((print-visited-vertex (stream)
               (lambda (vertex) (format stream "~A " vertex))))
      (utils:let-with-string-streams (vertex-finished discover-vertex back-edge forward-or-cross-edge)
          (alg:depth-first-search
           g :1
           :on-vertex-finished-fn (print-visited-vertex vertex-finished) 
           :on-discover-vertex-fn (print-visited-vertex discover-vertex)
           :on-back-edge-fn (print-visited-vertex back-edge)
           :on-forward-or-cross-edge-fn (print-visited-vertex forward-or-cross-edge))

        (is (equalp "6A 5A 6 5 4 3 2B 2A 1 " vertex-finished))
        (is (equalp "1 2B 3 4 5A 6A 5 6 2A " discover-vertex))
        (is (equalp "5a 3 " back-edge))
        (is (equalp "" forward-or-cross-edge))))))

#|
TODO:

|#
