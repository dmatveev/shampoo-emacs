;;; shampoo-tools.el --- Shampoo Worskspace and Transcript
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-modes)

(defvar *shampoo-workspaces* nil)

;; TODO associate the doit/printit initiator buffer with the
;; request/response id
(defvar *shampoo-last-active-workspace* nil)

(define-derived-mode shampoo-workspace-mode
  text-mode "Shampoo workspace mode"
  (set (make-local-variable 'font-lock-defaults)  
       smalltalk-font-lock-keywords-list))

(defmacro shampoo-mk-tool (name type)
  `(defun ,name (from to)
     (interactive "r")
     (setq *shampoo-last-active-workspace* (current-buffer))
     (shampoo-send-message
      (shampoo-make-eval-rq
       :id 1
       :type ,type
       :code (buffer-substring from to)))))

(shampoo-mk-tool shampoo-do-it    "DoIt")
(shampoo-mk-tool shampoo-print-it "PrintIt")

(define-key shampoo-workspace-mode-map "\C-c\C-d" 'shampoo-do-it)
(define-key shampoo-workspace-mode-map "\C-c\C-p" 'shampoo-print-it)

(defun shampoo-open-workspace ()
  (interactive)
  (let ((frame (make-frame))
        (buffer (generate-new-buffer "*shampoo-workspace*")))
    (pushnew buffer *shampoo-workspaces*)
    (raise-frame frame)
    (set-window-buffer (frame-first-window frame) buffer)
    (save-excursion
      (set-buffer buffer)
      (shampoo-workspace-mode)
      (setq header-line-format (shampoo-make-header)))))

(defmacro do-workspaces (evar &rest body)
  (destructuring-bind (var) evar
    `(dolist (,var *shampoo-workspaces*)
       ,@body)))

(provide 'shampoo-tools)

;;; shampoo-tools.el ends here.
