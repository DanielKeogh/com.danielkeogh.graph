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
                             (:file "bidirectional" :depends-on ("edge"))
                             (:file "adjacency" :depends-on ("edge"))
                             (:file "api" :depends-on ("adjacency" "bidirectional" "edge"))))
               (:module "algorithms"
                :pathname "algorithms"
                :depends-on ("structures" "utils")
                :components ((:file "package")
                             (:file "depth-first-search" :depends-on ("package"))
                             (:file "breadth-first-search" :depends-on ("package"))
                             (:file "bidirectional-breadth-first-search" :depends-on ("package"))
                             (:file "minimum-spanning-tree" :depends-on ("package"
                                                                         "bidirectional-breadth-first-search"))))))
