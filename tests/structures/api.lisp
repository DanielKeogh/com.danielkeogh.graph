;;;; tests/structures/api.lisp

(in-package #:com.danielkeogh.graph-tests.structures)

(def-suite structures-tests
  :description "api graph tests")

(in-suite structures-tests)

;; helpers

(defun make-bidirectional-matrix-graph-for-test (&rest args)
  (declare (ignore args))
  (api:make-bidirectional-matrix-graph 100))

(defparameter *constructors* '(api:make-adjacency-graph
                               api:make-bidirectional-graph
                               api:make-undirected-graph
                               make-bidirectional-matrix-graph-for-test))

(defmacro without-constructors ((&rest constructors) &body body)
  (alexandria:with-gensyms (c)
    `(let ((*constructors* (remove-if (lambda (,c) (find ,c ,constructors)) *constructors*)))
       ,@body)))

(defmacro without-matrix (&body body)
  `(without-constructors '(make-bidirectional-matrix-graph-for-test)
     ,@body))

(defmacro do-all-graphs ((var &rest args) &body body)
  (alexandria:with-gensyms (constructor)
    `(dolist (,constructor *constructors*)
       (let ((,var (apply ,constructor (list ,@args))))
         ,@body))))

(defmacro do-all-directed-graphs ((var &rest args) &body body)
  `(do-all-graphs (,var ,@args)
     (when (api:is-directed ,var)
       ,@body)))

(defmacro do-all-undirected-graphs ((var &rest args) &body body)
  `(do-all-graphs (,var ,@args)
     (unless (api:is-directed ,var)
       ,@body)))

(defun sort-edges (edges)
  (sort edges
        (lambda (edge1 edge2)
          (if (/= (edge:edge-source edge1) (edge:edge-source edge2))
              (< (edge:edge-source edge1) (edge:edge-source edge2))
              (< (edge:edge-target edge1) (edge:edge-target edge2))))))

;; constructors

(test make-graph
  (do-all-graphs (g)
    (is-true g)))

(test edge
  (is-true (api:make-edge 0 1)))

;; builders

(test add-vertex
  (without-matrix
    (do-all-graphs (g)
      (is (= 0 (length (api:vertices g))))
      (api:add-vertex g 1)
      (is (= 1 (length (api:vertices g)))))))

(test add-edge
  (do-all-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-edge g (edge:make-edge 1 2))
    (is (= 1 (length (api:edges g))))))

(test add-edges
  (do-all-graphs (g)
    (api:add-vertices g 0 1 2)
    (let ((e1 (api:make-edge 0 1))
          (e2 (api:make-edge 1 2)))
      (api:add-edges g e1 e2)
      (is-true (api:has-edge g e1))
      (is-true (api:has-edge g e2)))))

(test add-edges-and-vertices
  (do-all-graphs (g)
    (api:add-edges-and-vertices g
      (api:make-edge 0 1)
      (api:make-edge 1 2)
      (api:make-edge 2 3))
    (is-true (api:has-vertex g 0))
    (is-true (api:has-vertex g 1))
    (is-true (api:has-vertex g 2))
    (is-true (api:has-vertex g 3))
    (is-true (api:has-edge-between g 0 1))
    (is-true (api:has-edge-between g 1 2))
    (is-true (api:has-edge-between g 2 3))))

(test remove-vertex
  (without-matrix
    (do-all-graphs (g)
      (api:add-vertex g 1)
      (api:remove-vertex g 1)
      (api:remove-vertex g 2)
      (is (= 0 (length (api:vertices g)))))))

(test remove-edge
  (do-all-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (let ((edge (edge:make-edge 1 2)))
      (api:add-edge g edge)
      (api:remove-edge g edge))
    (is (= 0 (length (api:edges g))))))

(test remove-edge-between
  (do-all-graphs (g)
    (api:add-edges-and-vertices g
      (api:make-edge 0 1))
    (is (= 1 (length (api:edges g))))

    (api:remove-edge-between g 0 1)
    (is (= 0 (length (api:edges g))))))

;; tests

(test has-vertex
  (without-matrix
    (do-all-graphs (g)
      (is-false (api:has-vertex g 1))
      (api:add-vertex g 1)
      (is-true (api:has-vertex g 1)))))

(test has-edge
  (do-all-graphs (g)
    (let ((edge (api:make-edge 0 1)))
      (is-false (api:has-edge g edge))
      (api:add-vertex g 0)
      (is-false (api:has-edge g edge))
      (api:add-vertex g 1)
      (is-false (api:has-edge g edge))
      (api:add-edge g edge)
      (is-true (api:has-edge g edge)))))

(test has-edge-between
  (do-all-graphs (g)
    (is-false (api:has-edge-between g 1 2))
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-edge-between g 1 2)
    (is-true (api:has-edge-between g 1 2))))

;; graph accessors

(test out-edges
  (do-all-directed-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-vertex g 3)
    (is-false (api:out-edges g 1))
    (let ((edge1 (api:add-edge-between g 1 2))
          (edge2 (api:add-edge-between g 1 3))
          (edge3 (api:add-edge-between g 2 3)))
      (is (equal (list edge3) (api:out-edges g 2)))
      (is (equal (list edge1 edge2) (sort-edges (api:out-edges g 1)))))))

(test in-edges
  (do-all-directed-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-vertex g 3)
    (is-false (api:in-edges g 1))
    (let ((edge1 (api:add-edge-between g 2 1))
          (edge2 (api:add-edge-between g 3 1))
          (edge3 (api:add-edge-between g 3 2)))
      (is (equal (list edge3) (api:in-edges g 2)))
      (is (equal (list edge1 edge2) (sort-edges (api:in-edges g 1)))))))

(test adjacent-edges
  (do-all-undirected-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-vertex g 3)
    (is-false (api:adjacent-edges g 1))
    (let ((edge1 (api:add-edge-between g 2 1))
          (edge2 (api:add-edge-between g 3 1))
          (edge3 (api:add-edge-between g 3 2)))
      (is (equal (list edge1 edge3) (sort-edges (api:adjacent-edges g 2))))
      (is (equal (list edge1 edge2) (sort-edges (api:adjacent-edges g 1)))))))

(test vertices
  (without-matrix
    (do-all-graphs (g)
      (is-false (api:edges g))
      (api:add-vertex g 1)
      (api:add-vertex g 2)
      (is (equal (list 1 2) (sort (api:vertices g) #'<))))))

(test roots
  (without-constructors '(api:make-undirected-graph
                          make-bidirectional-matrix-graph-for-test)
    (do-all-graphs (g)
      (api:add-edges-and-vertices g
        (api:make-edge 0 :a)
        (api:make-edge 1 :a)
        (api:make-edge :a :b)
        (api:make-edge :b :c))
      (is (equal '(0 1) (sort (api:roots g) '<))))))

(test edges
  (do-all-graphs (g)
    (is-false (api:edges g))
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-edge-between g 1 2)
    (is-true (api:edges g))))

(test graph-vertex-equality-fn
  (without-matrix
    (do-all-graphs (g)
      (is (eql #'eql (api:graph-vertex-equality-fn g)))))
  (is (eql #'= (api:graph-vertex-equality-fn
                (api:make-bidirectional-matrix-graph 10)))))

(test vertex-count
  (without-matrix
    (do-all-graphs (g)
      (is (= 0 (api:vertex-count g)))
      (api:add-edges-and-vertices g
        (api:make-edge 1 1)
        (api:make-edge 2 2)
        (api:make-edge 3 3)
        (api:make-edge 4 4)
        (api:make-edge 5 5))
      (is (= 5 (api:vertex-count g))))))

(test edge-count
  (do-all-graphs (g)
    (is (= 0 (api:edge-count g)))
    (api:add-edges-and-vertices g
      (api:make-edge 1 1)
      (api:make-edge 2 2)
      (api:make-edge 3 3)
      (api:make-edge 4 4)
      (api:make-edge 5 5))
    (is (= 5 (api:edge-count g)))))

;; edge accessors

(test edge-accessors
  (let ((e (api:make-edge 0 1)))
    (is (= 0 (api:edge-source e)))
    (is (= 1 (api:edge-target e)))))

;; dynamic builders

(test dynamic-builders
  (do-all-graphs (g)
    (api:with-graph* (g)
      (api:add-vertex* 0)
      (api:add-vertex* 1)
      (api:add-edge* (api:make-edge 0 1)))
    (is-true (api:has-edge-between g 0 1))))

;; parallel edges

(test allow-parallel-edges
  (without-matrix
    (do-all-graphs (g :allow-parallel-edges t)
      (api:add-vertices g 0 1)
      (let ((e1 (api:make-edge 0 1))
            (e2 (api:make-edge 0 1)))

        (is-true (api:add-edge g e1))
        (is-true (api:add-edge g e2))
        (is-true (api:has-edge g e1))
        (is-true (api:has-edge g e2))))))

(test disallow-parallel-edges
  (do-all-graphs (g :allow-parallel-edges nil)
    (api:add-vertices g 0 1)
    (let ((e1 (api:make-edge 0 1))
          (e2 (api:make-edge 0 1)))

      (is-true (api:add-edge g e1))
      (is-false (api:add-edge g e2))
      (is-true (api:has-edge g e1))
      (is-false (api:has-edge g e2)))))

(test clone
  (do-all-graphs (g)
    (api:add-vertices g 0 1)
    (api:add-edge-between g 0 1)

    (let ((clone (api:clone g)))
      (is-true (api:has-edge-between clone 0 1))
      (is-true (api:has-vertex clone 0))
      (is-true (api:has-vertex clone 1))
      (is (eq (api:graph-vertex-equality-fn clone) (api:graph-vertex-equality-fn g))))))
