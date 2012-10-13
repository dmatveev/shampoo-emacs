;;; shampoo-utils.el --- Shampoo utility funcitons
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'cl)

(defun shampoo-log (&rest args)
  (save-excursion
    (set-buffer (get-buffer-create "*shampoo-log*"))
    (insert (apply 'format args))
    (newline)))

(defun shampoo-buffer-contents (buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (buffer-substring (point-min) (point-max))))

(defun shampoo-this-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun shampoo-next-line ()
  (save-excursion
    (next-line)
    (shampoo-this-line)))

(defun shampoo-buffer-num-lines ()
  (count-lines (point-min) (point-max)))

(defun shampoo-delete-this-line ()
  (let* ((true-end (line-end-position))
         (incr-end (1+ true-end))
         (del-end (if (> incr-end (point-max)) true-end incr-end)))
  (delete-region (line-beginning-position) del-end)))

(defun shampoo-clear-buffer (buffer-name)
  (save-excursion
    (set-buffer (get-buffer buffer-name))
    (let ((buffer-read-only nil))
      (erase-buffer))))

(defun shampoo-update-header-at (buffer string)
  (save-excursion
    (set-buffer buffer)
    (setq header-line-format string)))

(defun shampoo-split-string (string)
  (if (null string) '()
    (remove-if (lambda (x) (equal x "")) (split-string string "\s"))))

(defun shampoo-join-strings (with strings)
  (reduce (lambda (a b) (concat a with b)) strings))

(defun shampoo-side-sym-as-param (sym)
  (cdr (assoc sym '((:instance . "instance")
                    (:class    . "class")))))

(defun shampoo-side ()
  (shampoo-side-sym-as-param *shampoo-current-side*))

(defmacro when-shampoo-alive (instance &rest body)
  `(when (and (processp ,instance)
              (not (eql (process-status ,instance) 'closed)))
     ,@body))

(defmacro when-shampoo-alive-and (clause instance body)
  `(when ,clause (when-shampoo-alive ,instance ,body)))

(defun shampoo-curry (fcn &rest args)
  (lexical-let ((f fcn)
                (a args))
    (lambda (&rest rest-args)
      (apply f (concatenate 'list a rest-args)))))

(provide 'shampoo-utils)

;;; shampoo-utils.el ends here.
