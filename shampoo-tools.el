;;; shampoo-tools.el --- Shampoo Worskspace and Transcript
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-modes)
(require 'shampoo-state)

(define-derived-mode shampoo-workspace-mode
  text-mode "Shampoo workspace mode"
  (set (make-local-variable 'font-lock-defaults)  
       shampoo-smalltalk-font-lock-keywords-list)
  (set-syntax-table shampoo-smalltalk-mode-syntax-table))

(defun shampoo-do-it (from to)
  (interactive "r")
  (shampoo-send-message
   (shampoo-make-eval-rq
    :id (shampoo-give-id)
    :type "DoIt"
    :code (buffer-substring from to))))

(defun shampoo-printit-to (buffer)
  (lexical-let ((buff buffer))
    (lambda (resp)
      (when (not (shampoo-response-is-failure resp))
        (with-current-buffer buff
          (save-excursion
            (insert (shampoo-response-enclosed-string resp))))))))

(defun shampoo-print-it (from to)
  (interactive "r")
  (let ((request-id (shampoo-give-id)))
    (shampoo-subscribe
     request-id
     (shampoo-printit-to (current-buffer)))
    (shampoo-send-message
     (shampoo-make-eval-rq
      :id request-id
      :type "PrintIt"
      :code (buffer-substring from to)))))

(define-key shampoo-workspace-mode-map "\C-c\C-d" 'shampoo-do-it)
(define-key shampoo-workspace-mode-map "\C-c\C-p" 'shampoo-print-it)

(defun shampoo-open-workspace ()
  (interactive)
  (let ((frame  (make-frame))
        (buffer (generate-new-buffer "*shampoo-workspace*")))
    (with-~shampoo~
     (pushnew buffer (shampoo-current-workspaces ~shampoo~)))
    (raise-frame frame)
    (set-window-buffer (frame-first-window frame) buffer)
    (with-current-buffer buffer
      (shampoo-workspace-mode)
      (setq header-line-format (shampoo-make-header)))))

(defmacro do-workspaces (evar &rest body)
  (destructuring-bind (var) evar
    `(with-~shampoo~
      (dolist (,var (shampoo-current-workspaces ~shampoo~))
        ,@body))))

(provide 'shampoo-tools)

;;; shampoo-tools.el ends here.
