;;;; structures/bidirectional-matrix.lisp

(defpackage #:com.danielkeogh.graph.bidirectional-matrix
  (:use #:cl)
  (:local-nicknames
   (#:utils #:com.danielkeogh.graph.utils)
   (#:edge #:com.danielkeogh.graph.edge)
   (#:a #:alexandria))
  (:export
   ;; constructors
   #:make-graph

   ;; builders
   #:add-edge
   #:add-edge-between
   #:remove-edge
   #:remove-edge-between

   ;; test
   #:has-vertex
   #:has-edge
   #:has-edge-between

   ;; utils
   #:vertices
   #:edges
   #:out-edges
   #:in-edges
   #:vertex-count
   #:edge-count
   #:clone

   ;; accessors
   #:graph-vertex-equality-fn

   ;; looping without malloc
   #:for-edges
   #:for-in-out-eges
   #:for-vertices
   #:for-out-edges
   #:for-in-edges

   ;; types
   #:bidirectional-matrix-graph
   #:bidirectional-matrix-graph-p))

(in-package #:com.danielkeogh.graph.bidirectional-matrix)

;; struct definitions

(defstruct (bidirectional-matrix-graph (:conc-name graph-))
  (vertex-count (utils:required-argument "vertex-count") :type fixnum :read-only t)
  (edge-count 0 :type fixnum)
  (edges (utils:required-argument "edges") :type (simple-array (or null edge:edge) (* *)) :read-only t))

;; constructors

(declaim (ftype (function (fixnum)
                          (values bidirectional-matrix-graph &optional))
                make-graph))
(defun make-graph (vertex-count)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type fixnum vertex-count))
  (check-type vertex-count (integer 0 #.most-positive-fixnum))
  (make-bidirectional-matrix-graph
   :vertex-count vertex-count
   :edges (make-array (list vertex-count vertex-count)
                      :element-type '(or null edge:edge)
                      :initial-element nil)))

;; api

(declaim (ftype (function (bidirectional-matrix-graph t)
                          (values boolean &optional))
                has-vertex))
(defun has-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type fixnum vertex))
  (< vertex (graph-vertex-count graph)))

(declaim (ftype (function (bidirectional-matrix-graph t t)
                          (values boolean &optional))
                has-edge-between))
(defun has-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (aref (graph-edges graph) source target))

(declaim (ftype (function (bidirectional-matrix-graph edge:edge)
                          (values boolean &optional))
                has-edge))
(defun has-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (has-edge-between graph (edge:edge-source edge) (edge:edge-target edge)))

(declaim (ftype (function (bidirectional-matrix-graph edge:edge)
                          (values (or null edge:edge) &optional))
                %add-edge))
(defun %add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type edge:edge edge))
  (let ((source (edge:edge-source edge))
        (target (edge:edge-target edge)))
    (when (aref (graph-edges graph) source target)
      (error "Cannot add parallel edge ~S to bidirectional-matrix-graph" edge))
    (setf (aref (graph-edges graph) source target) edge)
    (incf (graph-edge-count graph))
    edge))

(declaim (ftype (function (bidirectional-matrix-graph edge:edge)
                          (values (or null edge:edge) &optional))
                add-edge))
(defun add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type edge:edge edge))
  (%add-edge graph edge))

(declaim (ftype (function (bidirectional-matrix-graph t t)
                          (values (or null edge:edge) &optional))
                add-edge-between))
(defun add-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type fixnum source target))
  (%add-edge graph (edge:make-edge source target)))

(declaim (ftype (function (bidirectional-matrix-graph t t)
                          (values boolean &optional))
                remove-edge-between))
(defun remove-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type fixnum source target))
  (when (aref (graph-edges graph) source target) 
    (setf (aref (graph-edges graph) source target) nil)
    (decf (graph-edge-count graph))
    t))

(declaim (ftype (function (bidirectional-matrix-graph edge:edge)
                          (values boolean &optional))
                remove-edge))
(defun remove-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type edge:edge edge))
  (remove-edge-between graph (edge:edge-source edge) (edge:edge-target edge)))

;; utils

(declaim (ftype (function (bidirectional-matrix-graph)
                          (values list &optional))
                edges))
(defun edges (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (loop for source below (graph-vertex-count graph)
        nconc (loop for target below (graph-vertex-count graph)
                    for edge = (aref (graph-edges graph) source target)
                    when edge collect edge)))

(declaim (ftype (function (bidirectional-matrix-graph)
                          (values list &optional))
                vertices))
(defun vertices (graph)
  (declare (type bidirectional-matrix-graph graph))
  (loop for i below (graph-vertex-count graph)
        collect i))

(declaim (ftype (function (bidirectional-matrix-graph t)
                          (values list &optional))
                out-edges))
(defun out-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (loop for target below (graph-vertex-count graph)
        for edge = (aref (graph-edges graph) vertex target)
        when edge collect edge))

(declaim (ftype (function (bidirectional-matrix-graph t)
                          (values list &optional))
                in-edges))
(defun in-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (loop for source below (graph-vertex-count graph)
        for edge = (aref (graph-edges graph) source vertex)
        when edge collect edge))

(declaim (ftype (function (bidirectional-matrix-graph)
                          (values fixnum &optional))
                vertex-count))
(defun vertex-count (graph)
  (declare (type bidirectional-matrix-graph graph))
  (graph-vertex-count graph))

(declaim (ftype (function (bidirectional-matrix-graph)
                          (values fixnum &optional))
                edge-count))
(defun edge-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (graph-edge-count graph))

(declaim (ftype (function (bidirectional-matrix-graph)
                          (values bidirectional-matrix-graph &optional))
                clone))
(defun clone (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (let ((clone (make-graph (graph-vertex-count graph))))
    (dolist (edge (edges graph))
      (add-edge-between clone (edge:edge-source edge) (edge:edge-target edge)))
    clone))

;; looping without malloc

(declaim (ftype (function (bidirectional-matrix-graph (function (edge:edge)))
                          (values null &optional))
                for-edges))
(defun for-edges (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type function fn))
  (dotimes (source (graph-vertex-count graph))
    (dotimes (target (graph-vertex-count graph))
      (let ((edge (aref (graph-edges graph) source target)))
        (when edge
          (funcall fn edge))))))

(declaim (ftype (function (bidirectional-matrix-graph (function (t)))
                          (values null &optional))
                for-vertices))
(defun for-vertices (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type function fn))
  (dotimes (i (graph-vertex-count graph))
    (funcall fn i)))

(declaim (ftype (function (bidirectional-matrix-graph t (function (edge:edge)))
                          (values null &optional))
                for-out-edges))
(defun for-out-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type function fn))
  (dotimes (target (graph-vertex-count graph))
    (let ((edge (aref (graph-edges graph) vertex target)))
      (when edge (funcall fn edge)))))

(declaim (ftype (function (bidirectional-matrix-graph t (function (edge:edge)))
                          (values null &optional))
                for-in-edges))
(defun for-in-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type function fn))
  (dotimes (source (graph-vertex-count graph))
    (let ((edge (aref (graph-edges graph) source vertex)))
      (when edge (funcall fn edge)))))

(declaim (ftype (function (bidirectional-matrix-graph t (function (edge:edge)))
                          (values null &optional))
                for-in-out-edges))
(defun for-in-out-eges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type bidirectional-matrix-graph graph))
  (declare (type function fn))
  (for-in-edges graph vertex fn)
  (for-out-edges graph vertex fn))
 
(declaim (ftype (function (bidirectional-matrix-graph)
                          (values (function (t t) (values boolean &optional)) &optional))
                graph-vertex-equality-fn))
(defun graph-vertex-equality-fn (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (ignore graph))
  #'=)
