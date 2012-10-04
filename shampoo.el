;;; shampoo.el --- Shampoo, a remote Smalltalk developemnt
;;                 mode for GNU Emacs
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'shampoo-auth)
(require 'shampoo-compile)
(require 'shampoo-fetcher)
(require 'shampoo-layout)
(require 'shampoo-modes)
(require 'shampoo-networking)
(require 'shampoo-requests)
(require 'shampoo-response)
(require 'shampoo-tools)
(require 'shampoo-utils)

;; Globals
;; TODO refactor the state
(defvar *shampoo-current-connection-info* nil)
(defvar *shampoo-connection-closure* nil)

(defvar *shampoo-current-namespace* nil)
(defvar *shampoo-current-class* nil)
(defvar *shampoo-current-method* nil)
(defvar *shampoo-code-compile* nil)
(defvar *shampoo-current-side* :instance)
(defvar *shampoo-current-smalltalk* nil)

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
    ("Magic"               . shampoo-handle-auth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shampoo-side-is (side)
  (eq *shampoo-current-side* side))

(defun shampoo-header ()
  (concat (shampoo-connect-info-str *shampoo-current-connection-info*)
          ", "
          *shampoo-current-smalltalk*))

(defun shampoo-update-headers ()
  (let ((header (shampoo-header)))
    (do-workspaces (each)
      (when (buffer-live-p each)
        (shampoo-update-header-at each header)))
    (shampoo-update-header-at
     (get-buffer-create "*shampoo-code*")
     header)))

(defun shampoo-handle-auth (resp)
  (let* ((pass (read-passwd "Password: "))
         (magic (shampoo-response-enclosed-string resp)))
    (shampoo-send-message
     (shampoo-make-login-rq
      :id 1
      :user (shampoo-connect-info-login
             *shampoo-current-connection-info*)
      :encd-pass (shampoo-prepare-pass magic pass)))
    (shampoo-send-message
     (shampoo-make-namespaces-rq :id 1))))

(defun shampoo-handle-server-info-response (resp)
  (setq *shampoo-current-smalltalk*
        (shampoo-response-enclosed-string resp))
  (shampoo-update-headers))

(defun shampoo-handle-printit (resp)
  (save-excursion
    ;; dirtiest hack ever
    (set-buffer *shampoo-last-active-workspace*)
    (insert (shampoo-response-enclosed-string resp))))

(defun shampoo-handle-transcript (resp)
  (let ((buffer (get-buffer "*shampoo-transcript*")))
    (when (null buffer)
      (let ((frame (make-frame)))
        (raise-frame frame)
        (setq buffer (get-buffer-create "*shampoo-transcript*"))
        (set-window-buffer (frame-first-window frame) buffer)))
    (save-excursion
      (set-buffer buffer)
      (setq header-line-format (shampoo-header))
      (goto-char (point-max))
      (insert (shampoo-response-enclosed-string resp)))))

(defun shampoo-current-class-name ()
  (if (eq *shampoo-current-side* :instance)
      *shampoo-current-class*
    (concat *shampoo-current-class* " class")))

(defun shampoo-build-method-name ()
  (format "%s>>%s"
          (shampoo-current-class-name)
          *shampoo-current-method*))
          
(defun shampoo-handle-source-response (resp)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
    (setq header-line-format
          (format "%s    %s"
                  (shampoo-header)
                  (shampoo-build-method-name)))
    (erase-buffer)
    (insert (shampoo-response-enclosed-string resp))))

(defun shampoo-handle-operational-response (resp)
  (let ((status (shampoo-response-attr 'status resp)))
    (message
     (concat "Shampoo: operation "
             (cond ((equal status "success") "successful")
                   ((equal status "failure") "failed"))))))

(defun shampoo-buffer-for (response-type)
  (cdr (assoc response-type *shampoo-buffer-info*)))

(defun shampoo-handler-for (response-type)
  (cdr (assoc type *shampoo-response-handlers*)))

(defun shampoo-handle-aggregate-response (resp buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (shampoo-clear-buffer-with-dependent)
      (when (boundp 'pre-insert-hook) (funcall pre-insert-hook))
      (dolist (item (shampoo-response-items resp))
        (let ((text (shampoo-response-aggr-item item)))
          (when text
            (insert text)
            (newline))))
      (goto-line 1)
      (when (boundp 'dependent-buffer)
        (shampoo-open-from-list))
      (when (and (boundp 'update-source-buffer) force-update-buffer)
        (funcall update-source-buffer)))))

(defun shampoo-handle-response (response)
  (let* ((type (shampoo-response-type response))
         (buffer (shampoo-buffer-for type))
         (handler (shampoo-handler-for type)))
    (if handler
        (funcall handler response)
      (shampoo-handle-aggregate-response response buffer))))
  
(defun shampoo-send-message (msg)
  (shampoo-net-send ~connection~ msg))

(defun shampoo-send-closure (connection)
  (lexical-let ((c connection))
    (lambda (str)
      (shampoo-net-send c str))))

(defun shampoo-handle-incoming (str)
  (dolist (msg (shampoo-fetcher-process str))
    ; (message "Fetched message: %s, processing..." msg)
    (shampoo-handle-response (shampoo-response-from msg))))

(defun shampoo-handle-shutdown ()
  (message "Shampoo: connection terminated"))

(defun shampoo-set-current-connection (connection)
  (lexical-let ((c connection))
    (setq *shampoo-connection-closure*
          (lambda () c))))

(defun shampoo-get-current-connection ()
  (when *shampoo-connection-closure*
    (funcall *shampoo-connection-closure*)))

(defmacro with-shampoo-connection (&rest body)
  `(let ((~connection~ (shampoo-get-current-connection)))
     ,@body))

(defun shampoo-connect (login-info)
  (interactive "sConnect to a Shampoo image: ")
  (let ((connect-info (shampoo-parse-login login-info)))
    (if connect-info
        (let ((connection (shampoo-net-connect connect-info)))
          (shampoo-disconnect)
          (setq *shampoo-current-connection-info* connect-info
                *shampoo-current-connection* connection)
          (shampoo-connection-setup
           connection
           :on-receive   'shampoo-handle-incoming
           :on-shutdown  'shampoo-handle-shutdown)
          (message "Shampoo: connected successfully")
          (shampoo-create-layout)
          (shampoo-set-current-connection connection)
          (shampoo-update-send-closures
           (shampoo-send-closure connection))
          (shampoo-update-current-side))
      (message "Shampoo: incorrect login info"))))

(defun shampoo-disconnect ()
  (interactive)
  (let ((connection (shampoo-get-current-connection)))
    (when connection
      (shampoo-net-disconnect connection)
      (dolist (buffer-info *shampoo-buffer-info*)
        (shampoo-clear-buffer (cdr buffer-info)))
      (shampoo-clear-buffer "*shampoo-code*"))))

(provide 'shampoo)

;; shampoo.el ends here
