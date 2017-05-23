(in-package #:cl-jupyter-widgets)



(defun kernel-start-hook (kernel)
  (let ((comm-manager (make-instance 'comm-manager :kernel kernel)))
    (setf (gethash kernel *kernel-comm-managers*) comm-manager)))

(defun kernel-shutdown-hook (kernel)
  (let ((comm-manager (gethash kernel *kernel-comm-managers*)))
    (if comm-manager
	(clrhash kernel *kernel-comm-managers*)
	(warn "The kernel ~a was shutdown but no comm-manager could be found for it" kernel))))


(defun handle-comm-open (shell identities msg buffers)
    (format t "[Shell] handling 'comm_open' - parsing message~%")
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "busy" :key (cl-jupyter::kernel-key shell))'
  (format t "[Shell] done sending busy~%")
  (unwind-protect
       (progn
	 (format t "[Shell] Parsing message~%")
	 (let ((content (myjson:parse-json-from-string (cl-jupyter::message-content msg))))
	   (format t "  ==> msg = ~W~%" msg)
	   (format t "  ==> comm_open Message content = ~W~%" content)))
    (format t "    Unwound when trying to parse-json-from-string ~%")
    (finish-output))
  ;; status back to idle
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "idle" :key (cl-jupyter::kernel-key shell)))

(defun handle-comm-msg (shell identities msg buffers)
  (warn "Handle comm_msg: ~a" msg))

(defun handle-comm-close (shell identities msg buffers)
  (warn "Handle comm_close: ~a" msg))

(eval-when (:load-toplevel :execute)
  (setf cl-jupyter:*kernel-start-hook* #'kernel-start-hook)
  (setf cl-jupyter:*kernel-shutdown-hook* #'kernel-shutdown-hook)
  (setf cl-jupyter:*handle-comm-open-hook* #'handle-comm-open)
  (setf cl-jupyter:*handle-comm-msg-hook* #'handle-comm-msg)
  (setf cl-jupyter:*handle-comm-close-hook* #'handle-comm-close)
  )




(defun send-comm-open (content)
  (let* ((msg (cl-jupyter::make-message cl-jupyter::*parent-msg* "comm_open" nil content))
	 (shell (cl-jupyter::*shell*)))
    #++(let ((json-str (encode-json-to-string content :indent 4)))
      (format t "Sending comm_open~%")
      (format t "parent-msg -> ~s~%" *parent-msg*)
      (format t "content:   ~s~%" content)
      (format t "json:  ---> ~%")
      (format t "~s~%" json-str))
    (cl-jupyter::message-send
     (cl-jupyter::iopub-socket (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)))
     msg
     :identities '("comm_open")
     :key (cl-jupyter::kernel-key shell))))

(defun send-comm-msg (content)
  (let* ((msg (cl-jupyter::make-message cl-jupyter::*parent-msg* "comm_msg" nil content))
	 (shell (cl-jupyter::*shell*)))
    #++(let ((json-str (encode-json-to-string content :indent 4)))
      (format t "Sending comm_msg~%")
      (format t "parent-msg -> ~s~%" *parent-msg*)
      (format t "content:   ~s~%" content)
      (format t "json:  ---> ~%")
      (format t "~s~%" json-str))
    (cl-jupyter::message-send
     (cl-jupyter::iopub-socket (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)))
     msg
     :identities '("comm_msg")
     :key (cl-jupyter::kernel-key shell))))
