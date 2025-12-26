;;;; tests/algorithms/undirected-depth-first-search.lisp

(in-package :com.danielkeogh.graph-tests.algorithms)

(def-suite undirected-depth-first-search-tests
  :description "Tests for undirected-depth-first-search algorithm")

(in-suite undirected-depth-first-search-tests)

(test touch-all-verticies
  (let ((g (api:make-undirected-graph)))
    (api:add-edges-and-vertices-between g
      1 2    1 3
      2 1    2 4
      2 5    6 7
      6 8    8 6)
    (labels ((print-visited (stream)
               (lambda (&rest args) (format stream "~A " (car args)))))
      (utils:let-with-string-streams (on-start on-discover on-examine on-tree on-back on-cross)
          (alg:undirected-depth-first-search g
           :on-start-vertex-fn (print-visited on-start)
           :on-discover-vertex-fn (print-visited on-discover)
           :on-examine-edge-fn (print-visited on-examine)
           :on-tree-edge-fn (print-visited on-tree)
           :on-back-edge-fn (print-visited on-back)
           :on-forward-or-cross-edge-fn (print-visited on-cross))
        (is (equalp "1 6 " on-start))
        (is (equalp "1 2 5 4 3 6 8 7 " on-discover))
        (is (equalp "2->1 2->5 2->5 2->4 2->4 2->1 1->2 1->3 1->3 1->2 8->6 8->6 6->8 6->8 6->7 6->7 " on-examine))
        (is (equalp "2->1 2->5 2->4 1->3 8->6 6->7 " on-tree))
        (is (equalp "2->5 2->4 2->1 1->2 1->3 8->6 6->8 6->7 " on-back))
        (is (equalp "1->2 6->8 " on-cross))))))

(test root-vertex
  (let ((g (api:make-undirected-graph)))
    (api:add-edges-and-vertices-between g
      1 2    1 3
      9 10   10 11)
    (let (starts visited)
      (alg:undirected-depth-first-search g
       :root-vertex 1
       :on-start-vertex-fn (lambda (v) (push v starts))
       :on-discover-vertex-fn (lambda (v) (push v visited)))
      (is (equalp (list 1) starts))
      (is (equalp (list 1 2 3) (sort visited '<))))

    (let (starts visited)
      (alg:undirected-depth-first-search g
       :root-vertex 1
       :process-all-vertices t
       :on-start-vertex-fn (lambda (v) (push v starts))
       :on-discover-vertex-fn (lambda (v) (push v visited)))
      (is (equalp (list 9 1) starts))
      (is (equalp (list 1 2 3 9 10 11) (sort visited '<))))))
