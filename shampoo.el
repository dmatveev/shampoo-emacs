;;; shampoo.el --- Shampoo, a remote Smalltalk developemnt
;;                 mode for GNU Emacs
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

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
    ("Magic"               . shampoo-handle-auth)))

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

(defun shampoo-make-header ()
  (with-~shampoo~
   (format
    "%s    %s"
    (shampoo-connect-info-str
     (shampoo-current-connection-info ~shampoo~))
    (shampoo-set-str-face
     (shampoo-dialect-specific-version
      (shampoo-current-smalltalk ~shampoo~))
     'shampoo-smalltalk-version))))

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

(defun shampoo-handle-transcript (resp)
  (let ((buffer (get-buffer "*shampoo-transcript*")))
    (when (null buffer)
      (let ((frame (make-frame)))
        (raise-frame frame)
        (setq buffer (get-buffer-create "*shampoo-transcript*"))
        (set-window-buffer (frame-first-window frame) buffer)))
    (save-excursion
      (set-buffer buffer)
      (setq header-line-format (shampoo-make-header))
      (goto-char (point-max))
      (insert (shampoo-response-enclosed-string resp)))))

(defun shampoo-format-class-name (name)
  (if (shampoo-side-is :instance)
      name
    (format "%s class" name)))

(defun shampoo-build-method-name (class method)
  (with-~shampoo~
   (shampoo-set-str-face
    (format "%s>>%s" (shampoo-format-class-name class) method)
    'shampoo-method-name)))
          
(defun shampoo-handle-source-response (resp)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-code*"))
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
      (shampoo-handle-aggregate-response response buffer))
    (shampoo-inform response)
    (shampoo-release-id (shampoo-response-id response))))
  
(defun shampoo-send-message (msg)
  (with-~shampoo~
   ; (message "Sending \"%s\"" msg)
   (shampoo-net-send
    (shampoo-current-connection ~shampoo~)
    msg)))

(defun shampoo-make-class-opener (class-to-open)
  (lexical-let ((class class-to-open))
    (lambda (resp)
      (shampoo-open-at-list "*shampoo-classes*" class))))

(defun* shampoo-reload-class-list (&optional open-then)
  (let ((request-id (shampoo-give-id)))
    (when open-then
      (shampoo-subscribe
       request-id
       (shampoo-make-class-opener open-then)))
    (shampoo-send-message
     (shampoo-make-classes-rq
      :id request-id
      :ns (shampoo-get-current-namespace)))))

(defun* shampoo-reload-categories-list (&optional open-then)
  (shampoo-send-message
   (shampoo-make-cats-rq
    :id (shampoo-give-id)
    :ns (shampoo-get-current-namespace)
    :class (shampoo-get-current-class)
    :side (shampoo-side))))

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

;; shampoo.el ends here.