;;; shampoo-dialect.el --- Dialect-specific Smalltalk routines
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'shampoo-regexp)

(defstruct shampoo-dialect-specific message-template version)

(defconst *shampoo-dialect-table*
  '(("GNU Smalltalk" . shampoo-make-gnu-dialect)
    ("Squeak"        . shampoo-make-squeak-dialect)
    ("Pharo"         . shampoo-make-squeak-dialect)))

(defconst *shampoo-gnu-smalltalk-message-template*
"messageSelectorAndArgumentNames [
	\"comment stating purpose of message\"

	| temporary variable names |
	statements\n]")

(defconst *shampoo-squeak-message-template*
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
   :message-template *shampoo-gnu-smalltalk-message-template*
   :version version))

(defun shampoo-make-squeak-dialect (version)
  (make-shampoo-dialect-specific
   :message-template *shampoo-squeak-message-template*
   :version version))

(provide 'shampoo-dialect)

;;; shampoo-dialect.el ends here.
