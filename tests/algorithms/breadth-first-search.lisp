;;;; tests/algorithms/breadth-first-search.lisp

(in-package #:com.danielkeogh.graph-tests.algorithms)

(def-suite breadth-first-search-tests
  :description "Tests for breadth-first-search algorithm")

(in-suite breadth-first-search-tests)

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
      (utils:let-with-string-streams (vertex-finished discover-vertex examine-vertex
                                                      gray-target black-target)
          (alg:breadth-first-search g
           :root-vertex :1
           :on-vertex-finished-fn (print-visited-vertex vertex-finished) 
           :on-discover-vertex-fn (print-visited-vertex discover-vertex)
           :on-examine-vertex-fn (print-visited-vertex examine-vertex)
           :on-gray-target-fn (print-visited-vertex gray-target)
           :on-black-target-fn (print-visited-vertex black-target))

        (is (equalp "1 2B 2A 3 4 5A 5 6A 6 " vertex-finished))
        (is (equalp "1 2B 2A 3 4 5A 5 6A 6 " discover-vertex))
        (is (equalp "1 2B 2A 3 4 5A 5 6A 6 " examine-vertex))
        (is (equalp "2A->3 " gray-target))
        (is (equalp "6A->5A " black-target))))))
