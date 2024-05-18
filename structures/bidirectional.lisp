;;;; bidirectional.lisp

(defpackage #:com.danielkeogh.graph.bidirectional
  (:use #:cl)
  (:local-nicknames
   (#:utils #:com.danielkeogh.graph.utils)
   (#:edge #:com.danielkeogh.graph.edge)
   (#:a #:alexandria))
  (:export
   ;; constructors
   #:make-graph

   ;; builders
   #:add-vertex
   #:remove-vertex
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

   ;; accessors
   #:graph-vertex-equality-fn

   ;; looping without malloc
   #:for-edges
   #:for-in-out-eges
   #:for-vertices
   #:for-out-edges
   #:for-in-edges

   ;; types
   #:bidirectional-graph
   #:bidirectional-graph-p))

(in-package #:com.danielkeogh.graph.bidirectional)

;;; struct definitions

(defstruct (bidirectional-graph (:conc-name graph-))
  (allow-parallel-edges (utils:required-argument "allow-parallel-edges") :type boolean :read-only t)
  (vertex-in-edges (utils:required-argument "vertex-in-edges") :type hash-table :read-only t)
  (vertex-out-edges (utils:required-argument "vertex-out-edges") :type hash-table :read-only t)
  (vertex-equality-fn (utils:required-argument "vertex-equality-fn") :type function :read-only t))

;;; constructors

(defun make-graph (&key
                     (allow-parallel-edges t)
                     (vertex-equality-fn #'eql))
  (make-bidirectional-graph
   :allow-parallel-edges allow-parallel-edges
   :vertex-in-edges (make-hash-table :test vertex-equality-fn)
   :vertex-out-edges (make-hash-table :test vertex-equality-fn)
   :vertex-equality-fn vertex-equality-fn))

(defun make-edge-list ()
  (list))

;; macros

(defmacro ensure-vertex (graph vertex)
  `(unless (has-vertex ,graph ,vertex)
     (error "Unrecognized ~S vertex ~S. Call add-vertex first." (prin1-to-string ',vertex) ,vertex)))

;; utils

(defun edge-equal (graph edge source target)
  (let ((test (graph-vertex-equality-fn graph)))
    (and (funcall test source (edge:edge-source edge))
         (funcall test target (edge:edge-target edge)))))

;; api

(defun has-vertex (graph vertex)
  (declare (type bidirectional-graph graph))
  (when (nth-value 1 (gethash vertex (graph-vertex-out-edges graph)))
    t)) ;; return boolean type for speed and to protect against memory leaks

(defun has-edge (graph edge)
  (declare (type bidirectional-graph graph))
  (a:when-let (edges (gethash (edge:edge-source edge) (graph-vertex-out-edges graph)))
    (find edge edges :test #'eql)))

(defun has-edge-between (graph source target)
  (declare (type bidirectional-graph graph))
  (a:when-let (edges (gethash source (graph-vertex-out-edges graph)))
    (loop for edge in edges
            thereis (edge-equal graph edge source target))))

(defun add-vertex (graph vertex)
  (declare (type bidirectional-graph graph))
  (unless (has-vertex graph vertex)
    (setf (gethash vertex (graph-vertex-in-edges graph)) (make-edge-list)
          (gethash vertex (graph-vertex-out-edges graph)) (make-edge-list))))

(defun %add-edge (graph edge)
  (declare (type bidirectional-graph graph)
           (type edge:edge edge)
           (optimize (speed 3) (safety 0)))
  (labels ((add-edge-to-hash (hashtable vertex edge)
             (push edge (gethash vertex hashtable))))
    (let ((source (edge:edge-source edge))
          (target (edge:edge-target edge)))
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (add-edge-to-hash (graph-vertex-out-edges graph) source edge)
      (add-edge-to-hash (graph-vertex-in-edges graph) target edge)))
  edge)

(defun add-edge (graph edge)
  (declare (type bidirectional-graph graph)
           (type edge:edge edge))
  (%add-edge graph edge))

(defun add-edge-between (graph source target)
  (declare (type bidirectional-graph graph))
  (%add-edge graph (edge:make-edge source target)))

(defun remove-edge (graph edge)
  (declare (type bidirectional-graph graph)
           (type edge:edge edge))
  (with-accessors ((out graph-vertex-out-edges)
                   (in graph-vertex-in-edges))
      graph
    (edge:with-edge (source target) edge
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (setf (gethash source out) (remove edge (gethash source out))
            (gethash target in) (remove edge (gethash target in))))))

(defun remove-edge-between (graph source target)
  (declare (type bidirectional-graph graph))
  (ensure-vertex graph source)
  (ensure-vertex graph target)
  (with-accessors ((out graph-vertex-out-edges)
                   (in graph-vertex-in-edges))
      graph
    (let ((should-remove-edge (lambda (edge) (edge-equal graph edge source target))))
      (setf (gethash source out) (remove-if should-remove-edge (gethash source out))
            (gethash target in) (remove-if should-remove-edge (gethash target in))))))

(defun remove-vertex (graph vertex)
  (with-accessors ((in graph-vertex-in-edges)
                   (out graph-vertex-out-edges))
      graph
    (loop for edge in (gethash vertex in)
          do (setf (gethash (edge:edge-source edge) out)
                   (remove edge (gethash (edge:edge-source edge) out))))
    
    (loop for edge in (gethash vertex out)
          do (setf (gethash (edge:edge-target edge) in)
                   (remove edge (gethash (edge:edge-target edge) in))))

    (remhash vertex in)
    (remhash vertex out)))

;;; utils

(defun edges (graph)
  (declare (type bidirectional-graph graph))
  (loop for edge-collection being the hash-values of (graph-vertex-out-edges graph)
        nconc (copy-list edge-collection)))

(defun vertices (graph)
  (declare (type bidirectional-graph graph))
  (loop for vertex being the hash-keys of (graph-vertex-out-edges graph)
        collect vertex))

(defun out-edges (graph vertex)
  (declare (type bidirectional-graph graph))
  (gethash vertex (graph-vertex-out-edges graph)))

(defun in-edges (graph vertex)
  (declare (type bidirectional-graph graph))
  (gethash vertex (graph-vertex-in-edges graph)))

(defun vertex-count (graph)
  (declare (type bidirectional-graph graph))
  (hash-table-count (graph-vertex-out-edges graph)))

(defun edge-count (graph)
  (declare (type bidirectional-graph graph))
  (loop for edge-collection being the hash-values of (graph-vertex-out-edges graph)
        sum (length edge-collection)))

;;; looping without malloc

(defun for-edges (graph fn)
  (declare (type bidirectional-graph graph)
           (type function fn))
  (loop for edge-collection being the hash-values of (graph-vertex-out-edges graph)
        do (loop for edge in edge-collection
                 do (funcall fn edge))))

(defun for-vertices (graph fn)
  (declare (type bidirectional-graph graph)
           (type function fn))
  (loop for vertex being the hash-keys of (graph-vertex-out-edges graph)
        do (funcall fn vertex)))

(defun for-out-edges (graph vertex fn)
  (declare (type bidirectional-graph graph)
           (type function fn))
  (loop for edge in (out-edges graph vertex)
        do (funcall fn edge)))

(defun for-in-edges (graph vertex fn)
  (declare (type bidirectional-graph graph)
           (type function fn))
  (loop for edge in (in-edges graph vertex)
        do (funcall fn edge)))

(defun for-in-out-edges (graph vertex fn)
  (declare (type bidirectional-graph graph)
           (type function fn))
  (for-in-edges graph vertex fn)
  (for-out-edges graph vertex fn))
