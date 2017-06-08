(in-package :cl-jupyter-widgets)

(defgeneric json-info (w)
  (:method-combination append))

(defclass widget ()
  ((%%widget-construction-callback :initform nil :accessor widget-construction-callback)
   (%%widgets :allocation :class :initform (make-hash-table) :accessor widgets)
   (%%widget-types :allocation :class :initform (make-hash-table) :accessor widget-types)
   ;; Traits
   (%model-module :initarg :model-module :accessor model-module)
   (%model-name :initarg :model-name :accessor model-name)
   (%view-module :initarg :view-module :accessor view-module)
   (%view-name :initarg :view-name :accessor view-name)
   (%comm :initarg :comm :accessor comm)
   (%msg-throttle :initarg :msg-throttle :accessor msg-throttle)
   (%keys :initarg :keys :accessor keys)
   (%property-lock :initarg :property-lock :accessor property-lock)
   (%holding-sync :initarg :holding-sync :accessor holding-sync)
   (%states-to-send :initarg :states-to-send :accessor states-to-send)
   (%display-callbacks :initarg :display-callbacks :accessor display-callbacks)
   (%msg-callbacks :initarg :msg-callbacks :accessor msg-callbacks)
   (%model-id :initarg :model-id :accessor model-id)
   ))


(defun call-widget-constructed (w)
  (when (widget-construction-callback w)
    (funcall widget-construction-callback w)))

(defmethod initialize-instance :around ((w widget) &rest initargs)
  (let ((w (call-next-method)))
    (call-widget-constructed w)
    (open w)))

(defmethod open ((self widget))
  (multiple-value-bind (state buffer-keys buffers)
      (split-state-buffers (get-state self))
    (let ((args (list (cons "target_name" "jupyter.widget")
			 (cons "data" state))))
	(when (model-id self)
	  (push (cons "comm_id" (model-id self)) args))
	(setf (comm self) (make-comm args))
	(when buffers
	  ;; See comment about buffers at
	  ;; https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L205
	  (send-state self)))))


(defmethod send-state ((self widget) &key key)
  "From https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L252
   Sends the widget state, or a part of the widget state to the front-end.
*Arguments
key : a key or a list of keys (optional)
      A property's name or a list of property names to sync with the front-end"
  (let ((state (get-state self :key key)))
    (multiple-value-bind (state buffer-keys buffers)
	(split-state-buffers state)
      (let ((msg (list (cons "method" "update")
		       (cons "state" state)
		       (cons "buffers" buffer-keys))))
	(send* self msg :buffers buffers)))))

(defmethod send ((self widget) content &key buffers)
  "Send a custom msg to the widget model in the front-end.
*Arguments
content : alist - Content of the message to send
buffers : list  - A list of binary buffers "
  (send* self (list (cons "method" "custom")
		    (cons "content" content))
	 :buffers buffers))

(defmethod send* ((self widget) msg &key buffers)
  "Sends a message to the widget model in the front-end.
See: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L485
Sends a message to the model in the front-end."
  (send (comm self) :data msg :buffers buffers))


  
(defmethod json-info ((w widget))
  '((:model-id model-id "_model_module")
    (:model-name model-name "_model_name")
    (:view-module view-module "_view_module")
    (:view-name view-name "_view_name")))

  

(defclass dom-widget (widget)
  ()
  (:default-initargs
   :model-name (unicode "DOMWidgetModel" :metadata '( :sync T))
    :visible (bool :true :allow-none T
		   :help "Whether the widget is visible. :false collapses the empty space, while NIL preserves the empty space.")
    :dom-classes (tuple nil :help "DOM classes applied to widget.$el." :metadata '(:sync T))
    :layout (make-layout :allow-none T :metadata (list* :sync T widget-serialization))))



(defclass int (dom-widget)
  ((%value :initform 0 :initarg :value :accessor value)
   (%disabled :initform (bool nil :help "enable or disable user changes" :initarg :disabled :accessor disabled))))



(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))


(defgeneric get-state (w))

(defmethod get-state ((w widget))
  (loop for slot-def in (clos:class-slots (class-of w))
     when (eq (clos:slot-definition-allocation slot-def) :instance)
     collect (cons (clos:slot-definition-name slot-def) (widget-slot-value w (clos:slot-definition-name slot-def)))))
    
(defclass layout (widget)
  ((%align-content :initarg :align-content :accessor align-content :type 'cunicode)
   (%align-items :initarg :align-items :accessor align-items :type 'cunicode)
   (%align-self :initarg :align-self :accessor align-self :type 'cunicode)
   (%bottom :initarg :bottom :accessor bottom :type 'cunicode)
   (%border :initarg :border :accessor border :type 'cunicode)
   (%display :initarg :display :accessor display :type 'cunicode)
   (%flex :initarg :flex :accessor flex :type 'cunicode)
   (%flex-flow :initarg :flex-flow :accessor flex-flow :type 'cunicode)
   (%height :initarg :height :accessor height :type 'cunicode)
   (%justify-content :initarg :justify-content :accessor justify-content :type 'cunicode)
   (%left :initarg :left :accessor left :type 'cunicode)
   (%margin :initarg :margin :accessor margin :type 'cunicode)
   (%max-height :initarg :max-height :accessor max-height :type 'cunicode)
   (%max-width :initarg :max-width :accessor max-width :type 'cunicode)
   (%min-height :initarg :min-height :accessor min-height :type 'cunicode)
   (%min-width :initarg :min-width :accessor min-width :type 'cunicode)
   (%overflow :initarg :overflow :accessor overflow :type 'cunicode)
   (%overflow-x :initarg :overflow-x :accessor overflow-x :type 'cunicode)
   (%overflow-y :initarg :overflow-y :accessor overflow-y :type 'cunicode)
   (%padding :initarg :padding :accessor padding :type 'cunicode)
   (%right :initarg :right :accessor right :type 'cunicode)
   (%top :initarg :top :accessor top :type 'cunicode)
   (%visibility :initarg :visibility :accessor visibility :type 'cunicode)
   (%width :initarg :width :accessor width :type 'cunicode)
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "LayoutView")
    :model-name (unicode "LayoutModel"))
  (:documentation
   "From ipywidgets/widgets/widget_layout.py
Layout specification

    Defines a layout that can be expressed using CSS.  Supports a subset of
    https://developer.mozilla.org/en-US/docs/Web/CSS/Reference

    When a property is also accessible via a shorthand property, we only
    expose the shorthand.

    For example:
    - ``flex-grow``, ``flex-shrink`` and ``flex-basis`` are bound to ``flex``.
    - ``flex-wrap`` and ``flex-direction`` are bound to ``flex-flow``.
    - ``margin-[top/bottom/left/right]`` values are bound to ``margin``, etc.
    "
   ))


