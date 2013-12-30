;;; shampoo.el --- A remote Smalltalk development mode
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

;;; Code:

(eval-when-compile (require 'cl))
(require 'shampoo-auth)
(require 'shampoo-compile)
(require 'shampoo-dialect)
(require 'shampoo-fetcher)
(require 'shampoo-layout)
(require 'shampoo-modes)
(require 'shampoo-networking)
(require 'shampoo-requests)
(require 'shampoo-response)
(require 'shampoo-state)
(require 'shampoo-tools)
(require 'shampoo-utils)
(require 'shampoo-fileout)

(defvar *shampoo-code-compile* 'shampoo-compile-class)

(defconst *shampoo-buffer-info*
  '(("Namespaces" . "*shampoo-namespaces*")
    ("Classes"    . "*shampoo-classes*"   )
    ("Categories" . "*shampoo-categories*")
    ("Methods"    . "*shampoo-methods*"   )))

(defconst *shampoo-response-handlers*
  '(("MethodSource"        . shampoo-handle-source-response)
    ("OperationalResponse" . shampoo-handle-operational-response)
    ("Class"               . shampoo-handle-class-response)
    ("Info"                . shampoo-handle-server-info-response)
    ("PrintIt"             . shampoo-handle-printit)
    ("Echo"                . shampoo-handle-transcript)
    ("Magic"               . shampoo-handle-auth)
    ("FileOut"             . shampoo-handle-fileout)))

(defun shampoo-update-headers ()
  (let ((header (shampoo-make-header)))
    (do-workspaces (each)
      (when (buffer-live-p each)
        (shampoo-update-header-at each header)))
    (shampoo-update-header-at
     (get-buffer-create "*shampoo-code*")
     header)))

(defun shampoo-handle-auth (resp)
  (let* ((pass (read-passwd "Password: "))
         (magic (shampoo-response-enclosed-string resp)))
    (with-~shampoo~
     (shampoo-send-message
      (shampoo-make-login-rq
       :id (shampoo-give-id)
       :user (shampoo-connect-info-login
              (shampoo-current-connection-info ~shampoo~))
       :encd-pass (shampoo-prepare-pass magic pass)))
     (shampoo-send-message
      (shampoo-make-namespaces-rq :id (shampoo-give-id))))))

(defun shampoo-handle-server-info-response (resp)
  (with-~shampoo~
   (setf
    (shampoo-current-smalltalk ~shampoo~)
    (shampoo-dialect-for (shampoo-response-enclosed-string resp))))
  (shampoo-update-headers))

(defun shampoo-handle-printit (resp)
  ;; do nothing, all work is done in the subscriber lambda.
  )

(defun shampoo-handle-fileout (resp)
  ;; do nothing, all work is done in the subscriber lambda.
  )

(defun shampoo-handle-transcript (resp)
  (let ((buffer (get-buffer "*shampoo-transcript*")))
    (when (null buffer)
      (let ((frame (make-frame)))
        (raise-frame frame)
        (setq buffer (get-buffer-create "*shampoo-transcript*"))
        (set-window-buffer (frame-first-window frame) buffer)))
    (with-current-buffer buffer
      (save-excursion
        (setq header-line-format (shampoo-make-header))
        (goto-char (point-max))
        (insert (shampoo-response-enclosed-string resp))))))

          
(defun shampoo-handle-source-response (resp)
  (with-current-buffer (get-buffer-create "*shampoo-code*")
    (save-excursion
      (erase-buffer)
      (insert (shampoo-response-enclosed-string resp)))))

(defun shampoo-handle-operational-response (resp)
  (if (shampoo-response-is-success resp)
      (message "Shampoo: operation successful")
    (message "Shampoo: operation failed -- %s"
             (shampoo-response-enclosed-string resp))))

(defun shampoo-buffer-for (response-type)
  (cdr (assoc response-type *shampoo-buffer-info*)))

(defun shampoo-handler-for (response-type)
  (cdr (assoc response-type *shampoo-response-handlers*)))

(defun shampoo-handle-aggregate-response (resp buffer)
  (with-current-buffer buffer
    (save-excursion
      (let ((buffer-read-only nil))
        (shampoo-clear-buffer-with-dependent)
        (when-shampoo-t pre-insert-hook (funcall pre-insert-hook))
        (dolist (item (shampoo-response-items resp))
        (let ((text (shampoo-response-aggr-item item)))
          (when text
            (insert text)
            (newline))))
        (goto-char (point-min))
        (when-shampoo-t dependent-buffer (shampoo-open-from-list))
        (when (and (boundp 'update-source-buffer)
                   (boundp 'force-update-buffer))
          (when force-update-buffer (funcall update-source-buffer)))
        (when (equal "Namespaces" (shampoo-response-type resp))
          (with-~shampoo~
           (setf (shampoo-current-class-category ~shampoo~) nil)))))))

(defun shampoo-handle-response (response)
  (let* ((type    (shampoo-response-type response))
         (buffer  (shampoo-buffer-for type))
         (handler (shampoo-handler-for type)))
    (if handler
        (funcall handler response)
      (shampoo-handle-aggregate-response response buffer))
    (shampoo-inform response)
    (when (shampoo-response-is-last-in-sequence response)
      (shampoo-release-id (shampoo-response-id response)))))

(defun shampoo-handle-incoming (str)
  (dolist (msg (shampoo-fetcher-process str))
    ; (message "Fetched message: %s, processing..." msg)
    (shampoo-handle-response (shampoo-response-from msg))))

(defun shampoo-handle-shutdown ()
  (message "Shampoo: connection terminated"))

(defun shampoo-connect (login-info)
  (interactive "sConnect to a Shampoo image: ")
  (let ((connect-info (shampoo-parse-login login-info)))
    (if connect-info
        (let ((connection (shampoo-net-connect connect-info)))
          (shampoo-disconnect)
          (shampoo-reset-state connection connect-info)
          (shampoo-connection-setup
           connection
           :on-receive   'shampoo-handle-incoming
           :on-shutdown  'shampoo-handle-shutdown)
          (message "Shampoo: connected successfully")
          (shampoo-create-layout)
          (shampoo-update-current-side))
      (message "Shampoo: incorrect login info"))))

(defun shampoo-reconnect ()
  (interactive)
  (with-~shampoo~
   (shampoo-connect
    (shampoo-connect-info-str
     (shampoo-current-connection-info ~shampoo~)))))

(defun shampoo-disconnect ()
  (interactive)
  (with-~shampoo~
   (let ((connection (shampoo-current-connection ~shampoo~)))
     (when connection
      (shampoo-net-disconnect connection)
      (dolist (buffer-info *shampoo-buffer-info*)
        (shampoo-clear-buffer (cdr buffer-info)))
      (shampoo-clear-buffer "*shampoo-code*")))))

(provide 'shampoo)

;;; shampoo.el ends here.
