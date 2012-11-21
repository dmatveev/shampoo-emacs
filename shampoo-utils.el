;;; shampoo-utils.el --- Shampoo utility funcitons
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
;; YES, I WILL USE GENSYM AND OTHER COOL CL STUFF!
(require 'cl)
(require 'shampoo-state)
(require 'shampoo-dialect)

(defun shampoo-log (&rest args)
  (with-current-buffer (get-buffer-create "*shampoo-log*")
    (insert (apply 'format args))
    (newline)))

(defun shampoo-buffer-contents (buffer-name)
  (with-current-buffer buffer-name
    (buffer-substring (point-min) (point-max))))

(defun shampoo-buffer-lines (buffer-name)
  (with-current-buffer buffer-name
    (save-excursion
      (goto-char (point-min))
      (let ((total (shampoo-buffer-num-lines)))
        (loop while (/= total (shampoo-this-line-no))
              collect (shampoo-this-line)
              do (forward-line))))))

(defun shampoo-this-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun shampoo-next-line ()
  (save-excursion
    (forward-line)
    (shampoo-this-line)))

(defun shampoo-this-line-no ()
  (count-lines (point-min) (point)))

(defun shampoo-buffer-num-lines ()
  (count-lines (point-min) (point-max)))

(defun shampoo-delete-this-line ()
  (let* ((true-end (line-end-position))
         (incr-end (1+ true-end))
         (del-end  (if (> incr-end (point-max)) true-end incr-end)))
  (delete-region (line-beginning-position) del-end)))

(defun shampoo-clear-buffer (buffer-name)
  (with-current-buffer buffer-name
    (let ((buffer-read-only nil))
      (erase-buffer))))

(defun shampoo-update-header-at (buffer string)
  (with-current-buffer buffer
    (setq header-line-format string)))

(defun shampoo-split-string (string)
  (if (null string) '()
    (remove-if (lambda (x) (equal x "")) (split-string string "\s"))))

(defun shampoo-join-strings (with strings)
  (if (null strings)
      ""
    (reduce (lambda (a b) (concat a with b)) strings)))

(defun shampoo-side-sym-as-param (sym)
  (cdr (assoc sym '((:instance . "instance")
                    (:class    . "class")))))

(defun shampoo-side ()
  (with-~shampoo~
   (shampoo-side-sym-as-param
    (shampoo-current-side ~shampoo~))))

(defun shampoo-curry (fcn &rest args)
  (lexical-let ((f fcn)
                (a args))
    (lambda (&rest rest-args)
      (apply f (concatenate 'list a rest-args)))))

(defun shampoo-capitalize (str)
  (concat (capitalize (substring str 0 1))
          (substring str 1)))

(defun* shampoo-ask (&key prompt from default)
  (if from
      (completing-read
       prompt (shampoo-buffer-lines from) nil t default)
    (read-string prompt default)))

(defmacro* shampoo-msum (&rest forms)
  (let ((block-name (gensym))
        (value-name (gensym)))
    `(block ,block-name
       ,@(loop for each in forms collect
               `(let ((,value-name ,each))
                  (when ,value-name
                    (return-from ,block-name ,value-name)))))))

(defmacro* shampoo-mklocal (variable-name &optional value-form)
  `(set (make-local-variable (quote ,variable-name)) ,value-form))

(defmacro shampoo-setq (variable-name value-form)
  `(if (boundp (quote ,variable-name))
       (setq ,variable-name ,value-form)
     (error "Variable %s is unbound" (quote ,variable-name))))

(defmacro shampoo-getv (variable-name)
  `(if (boundp (quote ,variable-name))
       ,variable-name
     (error "Variable %s is unbound" (quote ,variable-name))))

(defmacro when-shampoo-t (variable-name &rest body)
  `(if (boundp (quote ,variable-name))
       (if (not (null ,variable-name))
           ,@body)))

(provide 'shampoo-utils)

;;; shampoo-utils.el ends here.
