;;;; api.lisp

(defpackage #:com.danielkeogh.graph
  (:use #:cl)
  (:local-nicknames
   (#:bidirectional #:com.danielkeogh.graph.bidirectional)
   (#:edge #:com.danielkeogh.graph.edge)
   (#:utils #:com.danielkeogh.graph.utils))
  (:export
   ;; constructors
   #:make-bidirectional-graph
   #:make-adjacency-graph
   #:make-edge

   ;; builders
   #:add-edge
   #:add-vertex
   #:add-edges-and-vertices
   #:add-edge-between
   #:remove-edge
   #:remove-vertex
   #:remove-edge-between

   ;; graph accessors
   #:for-in-out-edges
   #:for-vertices
   #:for-roots
   #:for-edges

   #:out-edges
   #:in-edges
   #:vertices
   #:roots
   #:edges

   #:graph-vertex-equality-fn
   
   ;; edge accessors
   #:edge-source
   #:edge-target
   
   ;; dynamic builders
   #:with-graph*
   #:add-edge*
   #:add-vertex*))

(in-package #:com.danielkeogh.graph)

;;; generics

;; builders

(defgeneric add-vertex (graph v))

(defgeneric add-edge (graph edge)
  (:documentation "Add an edge to a graph. Will error if the edge refers to a vertex that is not yet added to the graph."))

(defgeneric add-edge-between (graph vertex1 vertex2)
  (:documentation "Create a new edge between two vertices on a graph. Will error if the edge refers to a vertex that is not yet added to the graph."))

(defgeneric remove-edge-between (graph vertex1 vertex2)
  (:documentation "Remove all edges edge between two vertexes"))

(defgeneric remove-edge (graph edge)
  (:documentation "Remove all edges edge between two vertexes"))

;; graph accessors

(defgeneric graph-vertex-equality-fn (graph)
  (:documentation "Get the function that checks if two vertices in the graph are the same"))

(defgeneric out-edges (graph vertex)
  (:documentation "Get outbound edges for a given vertex."))

(defgeneric in-edges (graph vertex)
  (:documentation "Get inbound edges for a given vertex."))

(defgeneric for-vertices (graph fn)
  (:documentation "Apply a function to all verticies in the graph."))


(defgeneric for-edges (graph fn)
  (:documentation "Apply a function to all edges in the graph."))

(trivial-indent:define-indentation for-vertices (4 &lambda))
(trivial-indent:define-indentation for-edges (4 &lambda))

;;; impl

;; edge

(defun make-edge (source target)
  (edge:make-edge source target))

;; bidirectional

(defun make-bidirectional-graph (&key (allow-parallel-edges t) (vertex-equality-fn #'equal))
  (bidirectional:make-graph :allow-parallel-edges allow-parallel-edges :vertex-equality-fn vertex-equality-fn))

(defmethod add-edge ((graph bidirectional:bidirectional-graph) edge)
  (bidirectional:add-edge graph edge))

(defmethod add-edge-between ((graph bidirectional:bidirectional-graph) vertex1 vertex2)
  (bidirectional:add-edge-between graph vertex1 vertex2))

(defmethod add-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:add-vertex graph vertex))

(defmethod remove-edge ((graph bidirectional:bidirectional-graph) edge)
  (bidirectional:remove-edge graph edge))

(defmethod remove-edge-between ((graph bidirectional:bidirectional-graph) vertex1 vertex2)
  (bidirectional:remove-edge-between graph vertex1 vertex2))

(defmethod remove-vertex ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:remove-vertex graph vertex))

(defmethod in-edges ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:in-edges graph vertex))

(defmethod out-edges ((graph bidirectional:bidirectional-graph) vertex)
  (bidirectional:out-edges graph vertex))

(defmethod graph-vertex-equality-fn ((graph bidirectional:bidirectional-graph))
  (bidirectional:graph-vertex-equality-fn graph))

(defmethod for-vertices ((graph bidirectional:bidirectional-graph) fn)
  (bidirectional:for-vertices graph fn))

(defmethod for-edges ((graph bidirectional:bidirectional-graph) fn)
  (bidirectional:for-edges graph fn))

;; edge accessors

(defun edge-source (edge)
  (edge:edge-source edge))

(defun edge-target (edge)
  (edge:edge-target edge))

;; utils

(defun add-edges-and-vertices (graph &rest edges)
  "Add a collection of edges to the graph, ensuring that their vertices are also added"
  (dolist (edge edges)
    (let ((source (edge-source edge))
          (target (edge-target edge)))
      (add-vertex graph source)
      (add-vertex graph target)
      (add-edge graph edge))))

(trivial-indent:define-indentation add-edges-and-vertices (4 &body))

(defun verticies (graph)
  "Get all verticies in the graph as a list."
  (utils:with-collector (collect)
    (for-vertices graph #'collect)))

(defun edges (graph)
  "Get all edges in the graph as a list."
  (utils:with-collector (collect)
    (for-edges graph #'collect)))


(defun for-roots (graph fn)
  "Call a function on all vertices with no inbound edges."
  (for-vertices graph (lambda (vertex)
                        (unless (in-edges graph vertex)
                          (funcall fn vertex)))))
(trivial-indent:define-indentation for-roots (4 &lambda))

(defun roots (graph)
  "Get all vertices with no inbound edges."
  (utils:with-collector (collect)
    (for-roots graph #'collect)))

(defun for-in-out-edges (graph vertex fn)
  (loop for edge in (in-edges graph vertex)
    do (funcall fn edge))
  (loop for edge in (out-edges graph vertex)
        do (funcall fn edge)))
(trivial-indent:define-indentation for-in-out-edges (4 4 &lambda))


;;; with-graph* utils

(defvar *graph*)

(defmacro with-graph* ((graph) &body body)
  `(let ((*graph* ,graph))
     ,@body))

(defmacro add-edge* (edge)
  (add-edge *graph* edge))

(defmacro add-vertex* (vertex)
  (add-vertex *graph* vertex))
