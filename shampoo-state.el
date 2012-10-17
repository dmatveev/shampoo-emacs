;;; shampoo-state.el --- Shampoo global state container
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'cl)
(require 'shampoo-dict)

(defstruct shampoo-current
  connection
  connection-info
  namespace
  class
  side
  smalltalk
  main-windows
  busy-ids
  last-id
  workspaces
  printit-subscribers)

(defvar shampoo-current-state nil)

(defmacro with-~shampoo~ (&rest body)
  `(when shampoo-current-state
     (let ((~shampoo~ shampoo-current-state))
       ,@body)))

(defun shampoo-reset-state (connection connection-info)
  (setq
   shampoo-current-state 
   (make-shampoo-current
    :connection connection
    :connection-info connection-info
    :side :instance
    :main-windows (make-shampoo-dict)
    :busy-ids (make-shampoo-dict)
    :last-id 1
    :workspaces nil
    :printit-subscribers (make-shampoo-dict))))

(defun shampoo-get-current-namespace ()
  (with-~shampoo~
   (shampoo-current-namespace ~shampoo~)))

(defun shampoo-get-current-class ()
  (with-~shampoo~
   (shampoo-current-class ~shampoo~)))

(provide 'shampoo-state)

;;; shampoo-state.el ends here.