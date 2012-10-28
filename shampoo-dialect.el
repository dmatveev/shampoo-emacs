;;; shampoo-dialect.el --- Dialect-specific Smalltalk routines
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'shampoo-regexp)
(require 'shampoo-state)

(defstruct shampoo-dialect-specific message-template version)

(defconst *shampoo-dialect-table*
  '(("GNU Smalltalk" . shampoo-make-gnu-dialect)
    ("Squeak"        . shampoo-make-squeak-dialect)
    ("Pharo"         . shampoo-make-squeak-dialect)))

(defun shampoo-gnu-smalltalk-message-template ()
  (format
"messageSelectorAndArgumentNames [
	\"comment stating purpose of message\"

    <category: '%s'>
	| temporary variable names |
	statements\n]"
    (let ((cat (shampoo-get-current-category)))
      (if (equal cat "*") "still unclassified" cat))))

(defun shampoo-squeak-message-template ()
"messageSelectorAndArgumentNames
	\"comment stating purpose of message\"

	| temporary variable names |
	statements")
 
(defun shampoo-dialect-for (version)
  (dolist (entry *shampoo-dialect-table*)
    (when (string-match (car entry) version)
      (return (funcall (cdr entry) version)))))

(defun shampoo-make-gnu-dialect (version)
  (make-shampoo-dialect-specific
   :message-template 'shampoo-gnu-smalltalk-message-template
   :version version))

(defun shampoo-make-squeak-dialect (version)
  (make-shampoo-dialect-specific
   :message-template 'shampoo-squeak-message-template
   :version version))

(defun shampoo-dialect-message-template (dialect)
  (funcall (shampoo-dialect-specific-message-template dialect)))

(provide 'shampoo-dialect)

;;; shampoo-dialect.el ends here.
