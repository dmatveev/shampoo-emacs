;;; shampoo-list-mode.el --- a "Basic class" for Shampoo list modes
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-state)
(require 'shampoo-utils)
(require 'shampoo-faces)
(require 'shampoo-requests)

(define-derived-mode shampoo-list-mode
  text-mode "Shampoo generic mode for list buffers"
  (setq buffer-read-only t)
  (shampoo-mklocal set-current-item)
  (shampoo-mklocal produce-request)
  (shampoo-mklocal pre-insert-hook)
  (shampoo-mklocal dependent-buffer)
  (shampoo-mklocal update-source-buffer)
  (shampoo-mklocal force-update-buffer)
  (shampoo-mklocal code-compile 'shampoo-compile-method)
  (shampoo-mklocal remove-item)
  (shampoo-mklocal fileout-item))

(defun shampoo-open-at-list (list-buff-name item)
  (with-current-buffer list-buff-name
    (save-excursion
      (goto-char (point-min))
      (if (null item)
          ;; Just open the fist item
          (shampoo-list-on-select)
        ;; Search for the specified one
        (block search-block
          (while (search-forward item nil t)
            (when (equal item (shampoo-this-line))
                (shampoo-open-from-list)
                (return-from search-block))))))))

(defun shampoo-open-from-list ()
  (interactive)
  (let ((this-line (shampoo-this-line)))
    (when (not (equal this-line ""))
      (shampoo-reset-buffer-faces)
      (shampoo-set-line-face 'shampoo-selected-list-item)
      (when-shampoo-t set-current-item
        (funcall set-current-item this-line))
      (shampoo-send-message
       (funcall (shampoo-getv produce-request) this-line)))))

(defun shampoo-clear-buffer-with-dependent ()
  (let ((buffer-read-only nil))
    (erase-buffer)
    (when-shampoo-t depd-buffer
      (with-current-buffer depd-buffer
        (shampoo-clear-buffer-with-dependent)))))

(defun shampoo-list-on-select ()
  (interactive)
  (shampoo-setq *shampoo-code-compile* (shampoo-getv code-compile))
  (when-shampoo-t dependent-buffer
    (shampoo-open-from-list))
  (when-shampoo-t update-source-buffer
    (funcall update-source-buffer)))

(defun shampoo-list-on-click (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos    (posn-point  (event-end event))))
    (when (windowp window)
      (with-current-buffer (window-buffer window)
        (goto-char pos)
        (shampoo-list-on-select)))))

(defun shampoo-list-remove-item ()
  (interactive)
  (when-shampoo-t remove-item
    (let ((this-line (shampoo-this-line)))
      (when (not (equal this-line ""))
        (funcall remove-item this-line)))))

(defun shampoo-list-fileout-item ()
  (interactive)
  (when-shampoo-t fileout-item
    (let ((this-line (shampoo-this-line)))
      (when (not (equal this-line ""))
        (funcall fileout-item this-line)))))

(define-key shampoo-list-mode-map
  [return]
  'shampoo-list-on-select)

(define-key shampoo-list-mode-map
  [mouse-1]
  'shampoo-list-on-click)

(define-key shampoo-list-mode-map
  "\C-c\C-t"
  'shampoo-toggle-side)

(define-key shampoo-list-mode-map
  "\C-c\C-d"
  'shampoo-list-remove-item)

(define-key shampoo-list-mode-map
  "\C-c\C-f"
  'shampoo-list-fileout-item)

(defun shampoo-make-class-opener (class-to-open)
  (lexical-let ((class class-to-open))
    (lambda (resp)
      (shampoo-open-at-list "*shampoo-classes*" class))))

(defun shampoo-make-category-opener (cat-to-open)
  (lexical-let ((cat cat-to-open))
    (lambda (resp)
      (shampoo-open-at-list "*shampoo-categories*" cat))))

(defun* shampoo-make-class-reloader (&optional class-to-open)
  (lexical-let ((open-then class-to-open))
    (lambda (resp)
      (when (shampoo-response-is-success resp)
        (shampoo-reload-class-list open-then)))))
  
(defun* shampoo-reload-class-list (&optional open-then)
  (let ((request-id (shampoo-give-id)))
    (shampoo-subscribe
     request-id
     (shampoo-make-class-opener open-then))
    (shampoo-send-message
     (shampoo-make-classes-rq
      :id request-id
      :ns (shampoo-get-current-namespace)))))

(defun* shampoo-reload-categories-list (&key open-then need-open)
  (let ((request-id (shampoo-give-id)))
    (when need-open
      (shampoo-subscribe
       request-id
       (shampoo-make-category-opener open-then)))
    (shampoo-send-message
     (shampoo-make-cats-rq
      :id request-id
      :ns (shampoo-get-current-namespace)
      :class (shampoo-get-current-class)
      :side (shampoo-side)))))

(provide 'shampoo-list-mode)

;;; shampoo-list-mode.el ends here.
