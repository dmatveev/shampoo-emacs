;;; shampoo-state.el --- Shampoo global state container
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-dict)
(require 'shampoo-response)
(require 'shampoo-networking)
(require 'shampoo-faces)

(defstruct shampoo-current
  connection
  connection-info
  namespace
  class
  class-category
  category
  side
  smalltalk
  main-windows
  busy-ids
  last-id
  workspaces
  response-subscribers
  fileout-configs)

(defvar shampoo-current-state nil)

(defmacro with-~shampoo~ (&rest body)
  `(when shampoo-current-state
     (let ((~shampoo~ shampoo-current-state))
       ,@body)))

(defun shampoo-send-message (msg)
  (with-~shampoo~
   ; (message "Sending \"%s\"" msg)
   (shampoo-net-send
    (shampoo-current-connection ~shampoo~)
    msg)))

(defun shampoo-reset-state (connection connection-info)
  (setq
   shampoo-current-state 
   (make-shampoo-current
    :connection           connection
    :connection-info      connection-info
    :side                 :instance
    :main-windows         (make-shampoo-dict)
    :busy-ids             (make-shampoo-dict)
    :last-id              1
    :workspaces           nil
    :response-subscribers (make-shampoo-dict)
    :fileout-configs      '())))

(defun shampoo-subscribe (response-id action)
  (with-~shampoo~
   (shampoo-dict-put
    :key   response-id
    :value action
    :into  (shampoo-current-response-subscribers ~shampoo~))))

(defun shampoo-inform (response)
  (with-~shampoo~
   (let ((action
          (shampoo-dict-get
           (shampoo-response-id response)
           (shampoo-current-response-subscribers ~shampoo~))))
     (when action
       (funcall action response)))))

(defun shampoo-get-current-namespace ()
  (with-~shampoo~
   (shampoo-current-namespace ~shampoo~)))

(defun shampoo-get-current-class ()
  (with-~shampoo~
   (shampoo-current-class ~shampoo~)))

(defun shampoo-get-current-category ()
  (with-~shampoo~
   (let ((cat (shampoo-current-category ~shampoo~)))
     (if cat cat "*"))))

(defun shampoo-get-current-class-category ()
  (with-~shampoo~
   (shampoo-current-class-category ~shampoo~)))

(defun shampoo-next-id (base)
  (let ((next-id (1+ base)))
    (mod next-id 65536)))

(defun shampoo-id-is-busy (id)
  (with-~shampoo~
   (shampoo-dict-has id (shampoo-current-busy-ids ~shampoo~))))

(defun shampoo-give-id ()
  (with-~shampoo~
   (let ((next-id
          (shampoo-next-id (shampoo-current-last-id ~shampoo~))))
     (while (shampoo-id-is-busy next-id)
       (setq next-id (shampoo-next-id next-id)))
     (shampoo-dict-put
      :key next-id
      :value t
      :into (shampoo-current-busy-ids ~shampoo~))
     (setf (shampoo-current-last-id ~shampoo~) next-id)
     next-id)))

(defun shampoo-release-id (id)
  (with-~shampoo~
   (shampoo-dict-drop id (shampoo-current-busy-ids ~shampoo~))))

(defun shampoo-side-is (side)
  (with-~shampoo~
   (eq (shampoo-current-side ~shampoo~) side)))

(provide 'shampoo-state)

;;; shampoo-state.el ends here.