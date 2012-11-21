;;; shampoo-networking.el --- Shampoo network layer
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-auth)

(defstruct shampoo-connection
  process)

(defun shampoo-net-sentinel (connection user-down-fcn)
  (lexical-let ((c connection)
                (f user-down-fcn))
    (lambda (process event)
      (when (not (shampoo-net-is-alive c))
        (funcall f)))))

(defun shampoo-net-receiver (connection user-recv-fcn)
  (lexical-let ((f user-recv-fcn))
    (lambda (process str)
      (funcall f str))))

(defun* shampoo-connection-setup 
    (connection &key on-receive on-shutdown)
  (let ((p (shampoo-connection-process connection)))
    (when on-receive
      (set-process-filter
       p
       (shampoo-net-receiver connection on-receive)))
    (when on-shutdown
      (set-process-sentinel
       p
       (shampoo-net-sentinel connection on-shutdown)))))

(defun shampoo-net-connect (connect-info)
  (make-shampoo-connection
   :process (open-network-stream "shampoo" nil
             (shampoo-connect-info-host connect-info)
             (shampoo-connect-info-port connect-info))))

(defun shampoo-net-is-alive (connection)
  (let ((proc (shampoo-connection-process connection)))
    (not (eq 'closed (process-status proc)))))

(defun shampoo-net-mkmsg (str)
  (format "Content-Length: %d\r\n\r\n%s" (length str) str))

(defun shampoo-net-send (connection msg)
  ; (message "Sending: %s" msg)
  (process-send-string
   (shampoo-connection-process connection)
   (shampoo-net-mkmsg msg)))

(defun shampoo-net-disconnect (connection)
  (when connection
    (delete-process (shampoo-connection-process connection))))

(provide 'shampoo-networking)

;;; shampoo-networking.el ends here.
