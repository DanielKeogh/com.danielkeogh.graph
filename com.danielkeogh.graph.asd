;;;; com.danielkeogh.graph.asd

(asdf:defsystem #:com.danielkeogh.graph
  :description "A fast an reliable graph library."
  :author "Daniel Keogh"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:alexandria :cl-speedy-queue :trivial-indent)
  :components ((:file "utils")
               (:module "structures"
                :pathname "structures"
                :depends-on ("utils")
                :components ((:file "edge")
                             (:file "adjacency" :depends-on ("edge"))
                             (:file "bidirectional" :depends-on ("edge"))
                             (:file "bidirectional-matrix" :depends-on ("edge"))
                             (:file "undirected" :depends-on ("edge"))))
               (:file "api" :depends-on ("structures"))
               (:module "algorithms"
                :pathname "algorithms"
                :depends-on ("structures" "utils" "api")
                :components ((:file "package")
                             ;; assignment
                             (:file "hungarian-assignment" :depends-on ("package"))
                             ;; search
                             (:file "bidirectional-breadth-first-search" :depends-on ("package"))
                             (:file "breadth-first-search" :depends-on ("package"))
                             (:file "depth-first-search" :depends-on ("package"))
                             ;; connected components
                             (:file "strongly-connected-components" :depends-on ("package" "depth-first-search"))
                             (:file "weakly-connected-components" :depends-on ("package" "depth-first-search"))
                             (:file "connected-components-utils" :depends-on ("package"))
                             ;; condensation
                             (:file "condensate-vertices" :depends-on ("package"))
                             ;; graph partition
                             (:file "kernighan-lin" :depends-on ("package"))
                             ;; minimum spanning tree
                             (:file "minimum-spanning-tree" :depends-on ("package" "bidirectional-breadth-first-search"))
                             ;; toplogical-sort
                             (:file "topological-sort" :depends-on ("package" "depth-first-search"))))))
