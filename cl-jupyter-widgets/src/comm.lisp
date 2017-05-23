;;;
;;; Comments are copied from ipykernel
(in-package #:cl-jupyter-widgets)

(defclass comm ()
  ((kernel :allocator :class :initform *kernel* :reader kernel)
   (comm-id :initform (uuid:make-v4-uuid) :reader comm-id)))

(defparameter *open-data* nil
  "Data (if any) to send with comm_open")
(defparameter *close-data* nil
  "Data (if any) to send with comm_close")


(defmethod %publish-msg ((self comm) msg-type &rest rest &key data metadata buffers)
  (error "implement me"))

#|
(defmethod open ((self comm) &key data metadata buffers)
  (let ((data (or data *open-data*))
	(comm-manager (cl-jupyter:comm-manager *kernel-comm-managers*)))
    (register-comm comm-manager 
  
|#  
