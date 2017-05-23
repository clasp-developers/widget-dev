
(asdf:defsystem #:cl-jupyter-widgets
    :description "Widgets for An Enhanced Interactive Shell for Common Lisp (based on the Jupyter protocol)."
    :version "0.6"
    :author "Christian Schafmeister"
    :license "BSD 2-Clause. See LICENSE."
    :depends-on (:cl-jupyter)
    :serial t
    :components ((:file "packages")
		 (:file "manager")
		 (:file "comm")
		 (:file "interface")))
