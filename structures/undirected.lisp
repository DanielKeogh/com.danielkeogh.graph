;;;; undirected.lisp

(defpackage #:com.danielkeogh.graph.undirected
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
   #:adjacent-edges
   #:vertex-count
   #:edge-count
   #:clone

   ;; accessors
   #:graph-vertex-equality-fn

   ;; looping without malloc
   #:for-edges
   #:for-adjacent-eges
   #:for-vertices

   ;; types
   #:undirected-graph
   #:undirected-graph-p))

(in-package #:com.danielkeogh.graph.undirected)

;;; struct definitions

(defstruct (undirected-graph (:conc-name graph-))
  (allow-parallel-edges (utils:required-argument "allow-parallel-edges") :type boolean :read-only t)
  (vertex-edges (utils:required-argument "vertex-edges") :type hash-table :read-only t)
  (vertex-equality-fn (utils:required-argument "vertex-equality-fn") :type function :read-only t))

;;; constructors

(declaim (ftype (function (&key (:allow-parallel-edges boolean)
                                (:vertex-equality-fn (function (t t) boolean)))
                          (values undirected-graph &optional))
                make-graph))
(defun make-graph (&key
                     (allow-parallel-edges t)
                     (vertex-equality-fn #'eql))
  (declare #.utils:*internal-optimize-settings*)
  (make-undirected-graph
   :allow-parallel-edges allow-parallel-edges
   :vertex-edges (make-hash-table :test vertex-equality-fn)
   :vertex-equality-fn vertex-equality-fn))

(declaim (ftype (function () (values list &optional))
                make-edge-list))
(defun make-edge-list ()
  (list))

;; macros

(defmacro ensure-vertex (graph vertex)
  `(unless (has-vertex ,graph ,vertex)
     (error "Unrecognized ~S vertex ~S. Call add-vertex first." (prin1-to-string ',vertex) ,vertex)))

;; utils

(declaim (ftype (function (undirected-graph edge:edge t t)
                          (values boolean &optional))
                edge-equal))
(defun edge-equal (graph edge source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (let ((test (graph-vertex-equality-fn graph)))
    (or (and (funcall test source (edge:edge-source edge))
             (funcall test target (edge:edge-target edge)))
        (and (funcall test target (edge:edge-source edge))
             (funcall test source (edge:edge-target edge))))))

;; api

(declaim (ftype (function (undirected-graph t)
                          (values boolean &optional))
                has-vertex))
(defun has-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (when (nth-value 1 (gethash vertex (graph-vertex-edges graph)))
    t)) ;; return boolean type for speed and to protect against memory leaks

(declaim (ftype (function (undirected-graph edge:edge)
                          (values boolean &optional))
                has-edge))
(defun has-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let ((edges (gethash (edge:edge-source edge) (graph-vertex-edges graph))))
    (declare (type (or list null) edges))
    (when edges
        (find edge edges :test #'eql))))

(declaim (ftype (function (undirected-graph t t)
                          (values boolean &optional))
                has-edge-between))
(defun has-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let ((edges (gethash source (graph-vertex-edges graph))))
    (declare (type (or list null) edges))
    (when edges
      (loop for edge in edges
              thereis (edge-equal graph edge source target)))))

(declaim (ftype (function (undirected-graph t)
                          (values boolean &optional))
                add-vertex))
(defun add-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (unless (has-vertex graph vertex)
    (setf (gethash vertex (graph-vertex-edges graph)) (make-edge-list))
    t))

(declaim (ftype (function (undirected-graph edge:edge)
                          (values (or null edge:edge) &optional))
                %add-edge))
(defun %add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (labels ((add-edge-to-hash (hashtable vertex edge)
             (push edge (gethash vertex hashtable))))
    (let ((source (edge:edge-source edge))
          (target (edge:edge-target edge)))
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (when (or (graph-allow-parallel-edges graph)
                (not (has-edge-between graph source target)))
        (add-edge-to-hash (graph-vertex-edges graph) source edge)
        (unless (eq source target)
          (add-edge-to-hash (graph-vertex-edges graph) target edge))
        edge))))

(declaim (ftype (function (undirected-graph edge:edge)
                          (values (or null edge:edge) &optional))
                add-edge))
(defun add-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (%add-edge graph edge))

(declaim (ftype (function (undirected-graph t t)
                          (values (or null edge:edge) &optional))
                add-edge-between))
(defun add-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (%add-edge graph (edge:make-edge source target)))

(declaim (ftype (function (undirected-graph edge:edge)
                          (values null &optional))
                remove-edge))
(defun remove-edge (graph edge)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type edge:edge edge))
  (let ((edgemap (graph-vertex-edges graph)))
    (edge:with-edge (source target) edge
      (ensure-vertex graph source)
      (ensure-vertex graph target)
      (setf (gethash source edgemap) (remove edge (gethash source edgemap))
            (gethash target edgemap) (remove edge (gethash target edgemap)))))
  nil)

(declaim (ftype (function (undirected-graph t t)
                          (values null &optional))
                remove-edge-between))
(defun remove-edge-between (graph source target)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (ensure-vertex graph source)
  (ensure-vertex graph target)
  (let ((edgemap (graph-vertex-edges graph))
        (should-remove-edge (lambda (edge) (edge-equal graph edge source target))))
    (setf (gethash source edgemap) (remove-if should-remove-edge (gethash source edgemap))
          (gethash target edgemap) (remove-if should-remove-edge (gethash target edgemap))))
  nil)

(declaim (ftype (function (undirected-graph t)
                          (values null &optional))
                remove-vertex))
(defun remove-vertex (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let* ((edgemap (graph-vertex-edges graph))
         (edges (gethash vertex edgemap)))
    (remhash vertex edgemap)
    (dolist (edge edges)
      (edge:with-edge (source target) edge
        (if (gethash source edgemap)
            (setf (gethash source edges) (remove edge (gethash source edges)))
            (setf (gethash target edges) (remove edge (gethash target edges)))))))
  nil)

;;; looping without malloc

(declaim (ftype (function (undirected-graph (function (edge:edge)))
                          (values null &optional))
                for-edges))
(defun for-edges (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type function fn))
  (maphash (lambda (vertex edges)
             (dolist (edge edges)
               (when (eq (edge:edge-source edge) vertex)
                 (funcall fn edge))))
           (graph-vertex-edges graph)))

(declaim (ftype (function (undirected-graph (function (t)))
                          (values null &optional))
                for-vertices))
(defun for-vertices (graph fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type function fn))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        do (funcall fn vertex)))

(declaim (ftype (function (undirected-graph t (function (edge:edge)))
                          (values null &optional))
                for-adjacent-edges))
(defun for-adjacent-edges (graph vertex fn)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (declare (type function fn))
  (loop for edge in (adjacent-edges graph vertex)
        do (funcall fn edge)))

;;; utils

(declaim (ftype (function (undirected-graph)
                          (values list &optional))
                edges))
(defun edges (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (utils:with-collector (collect)
    (for-edges graph #'collect)))

(declaim (ftype (function (undirected-graph)
                          (values list &optional))
                vertices))
(defun vertices (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (loop for vertex being the hash-keys of (graph-vertex-edges graph)
        collect vertex))

(declaim (ftype (function (undirected-graph t)
                          (values list &optional))
                adjacent-edges))
(defun adjacent-edges (graph vertex)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (nth-value 0 (gethash vertex (graph-vertex-edges graph))))

(declaim (ftype (function (undirected-graph)
                          (values fixnum &optional))
                vertex-count))
(defun vertex-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (hash-table-count (graph-vertex-edges graph)))

(declaim (ftype (function (undirected-graph)
                          (values fixnum &optional))
                edge-count))
(defun edge-count (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (utils:with-counter (cnt)
    (for-edges graph #'cnt)))

(declaim (ftype (function (undirected-graph)
                          (values undirected-graph &optional))
                clone))
(defun clone (graph)
  (declare #.utils:*internal-optimize-settings*)
  (declare (type undirected-graph graph))
  (let ((clone (make-graph
                :allow-parallel-edges (graph-allow-parallel-edges graph)
                :vertex-equality-fn (graph-vertex-equality-fn graph))))
    (dolist (edge (edges graph))
      (add-edge-between clone (edge:edge-source edge) (edge:edge-target edge)))
    clone))

