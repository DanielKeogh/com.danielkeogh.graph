;;;; structures/edge.lisp

(defpackage #:com.danielkeogh.graph.edge
  (:use #:cl)
  (:local-nicknames
   (#:utils #:com.danielkeogh.graph.utils))
  (:export
   ;; constructors
   #:make-edge
   ;; accessors
   #:edge-source
   #:edge-target
   ;; macros
   #:with-edge
   ;; types
   #:edge
   #:edge-p))

(in-package :com.danielkeogh.graph.edge)

(defstruct (edge
            (:constructor make-edge (source target)))
  (source (utils:required-argument "source") :type (not null) :read-only t)
  (target (utils:required-argument "target") :type (not null) :read-only t))

(defmacro with-edge ((source target) edge &body body)
  `(let ((,source (edge-source ,edge))
         (,target (edge-target ,edge)))
     (declare (ignorable ,source ,target))
     ,@body))

(defmethod print-object ((edge edge) stream)
  (format stream "~A->~A" (edge-source edge) (edge-target edge)))
