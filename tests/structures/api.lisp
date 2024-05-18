;;;; tests/structures/api.lisp

(in-package #:com.danielkeogh.graph-tests.structures)

(def-suite api-graph-tests
  :description "api graph tests")

(in-suite api-graph-tests)

;; helpers

(defparameter *constructors* '(api:make-adjacency-graph
                               api:make-bidirectional-graph))

(defmacro without-constructors ((&rest constructors) &body body)
  (alexandria:with-gensyms (c)
    `(let ((*constructors* (remove-if (lambda (,c) (find ,c ,constructors)) *constructors*)))
       ,@body)))

(defmacro do-all-graphs ((var &rest args) &body body)
  (alexandria:with-gensyms (constructor)
    `(dolist (,constructor *constructors*)
       (let ((,var (apply ,constructor ,args)))
         ,@body))))

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
  (do-all-graphs (g)
    (is (= 0 (length (api:vertices g))))
    (api:add-vertex g 1)
    (is (= 1 (length (api:vertices g))))))

(test add-edge
  (do-all-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-edge g (edge:make-edge 1 2))
    (is (= 1 (length (api:edges g))))))

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
  (do-all-graphs (g)
    (api:add-vertex g 1)
    (api:remove-vertex g 1)
    (api:remove-vertex g 2)
    (is (= 0 (length (api:vertices g))))))

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
    (is (= 2 (length (api:vertices g))))
    (is (= 1 (length (api:edges g))))

    (api:remove-edge-between g 0 1)
    (is (= 0 (length (api:edges g))))))

;; tests

(test has-vertex
  (do-all-graphs (g)
    (is-false (api:has-vertex g 1))
    (api:add-vertex g 1)
    (is-true (api:has-vertex g 1))))

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
  (do-all-graphs (g)
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
  (do-all-graphs (g)
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-vertex g 3)
    (is-false (api:in-edges g 1))
    (let ((edge1 (api:add-edge-between g 2 1))
          (edge2 (api:add-edge-between g 3 1))
          (edge3 (api:add-edge-between g 3 2)))
      (is (equal (list edge3) (api:in-edges g 2)))
      (is (equal (list edge1 edge2) (sort-edges (api:in-edges g 1)))))))

(test vertices
  (do-all-graphs (g)
    (is-false (api:edges g))
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (is (equal (list 1 2) (sort (api:vertices g) #'<)))))

(test roots
  (do-all-graphs (g)
    (api:add-edges-and-vertices g
      (api:make-edge 0 :a)
      (api:make-edge 1 :a)
      (api:make-edge :a :b)
      (api:make-edge :b :c))
    (is (equal '(0 1) (sort (api:roots g) '<)))))

(test edges
  (do-all-graphs (g)
    (is-false (api:edges g))
    (api:add-vertex g 1)
    (api:add-vertex g 2)
    (api:add-edge-between g 1 2)
    (is-true (api:edges g))))

(test graph-vertex-equality-fn
  (do-all-graphs (g)
    (is (eql #'eql (api:graph-vertex-equality-fn g)))))

(test vertex-count
  (do-all-graphs (g)
    (is (= 0 (api:vertex-count g)))
    (api:add-edges-and-vertices g
      (api:make-edge 1 1)
      (api:make-edge 2 2)
      (api:make-edge 3 3)
      (api:make-edge 4 4)
      (api:make-edge 5 5))
    (is (= 5 (api:vertex-count g)))))

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
