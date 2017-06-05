
(load "~/quicklisp/setup.lisp")
(progn
  (asdf:load-asd "/Users/meister/Development/widget-dev/cl-jupyter-widgets/src/cl-jupyter-widgets.asd")
  (asdf:load-system :cl-jupyter-widgets :verbose t))





(in-package :cl-jupyter-widgets)

(defparameter *w* (make-instance 'int))

(instance nil)

(tuple 
