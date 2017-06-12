
(asdf:defsystem #:cl-jupyter-widgets
    :description "Widgets for An Enhanced Interactive Shell for Common Lisp (based on the Jupyter protocol)."
    :version "0.6"
    :author "Christian Schafmeister"
    :license "BSD 2-Clause. See LICENSE."
    :serial t
    :components ((:file "packages")
		 (:file "tools")
		 (:file "manager")
		 (:file "interface")
		 (:file "traitlets")
		 (:file "widget")
		 (:file "comm")
		 ))


;;; Note - if this is loaded with quicklisp the first kernel-start-hook call needs
;;;        to be done by hand.

