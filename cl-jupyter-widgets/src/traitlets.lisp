
(in-package :cl-jupyter-widgets)


;;; trait-type is copying the Python traitlets package
;;;  https://github.com/ipython/traitlets/blob/master/traitlets/traitlets.py

(defclass trait-type ()
  ((%default-value :initarg :default-value :accessor default-value)
   (%help :initarg :help :accessor help :initform "")
   (%allow-none :initarg :allow-none :accessor allow-none :initform nil)  ; :NONE is a special value
   (%read-only :initarg :read-only :accessor read-only :initform nil)  ; :NONE is a special value
   (%info-text :initarg :info-text :accessor info-text :initform "any value")
   (%metadata :initarg :metadata :accessor metadata :initform nil)))

(defclass unicode (trait-type)
  ()
  (:default-initargs
   :default-value (make-array 0 :element-type 'character)
    :info-text 'A unicode value'))

(defun unicode (str &rest args)
  (apply #'make-instance 'unicode :default-value str args))

(defclass bool (trait-type)
  ()
  (:default-initargs
   :allow-none nil))

(defun bool (val &rest args)
  (apply #'make-instance 'bool :default-value val args))

(defclass instance (trait-type)
  ((%klass :initarg :klass :accessor klass)))

(defun instance (class-name &rest args)
  (apply #'make-instance 'instance :klass class-name args))

(defclass container (instance)
  ())


(defun tuple (parts &rest args)
  (apply #'make-instance 'tuple :klass 'tuple args))

