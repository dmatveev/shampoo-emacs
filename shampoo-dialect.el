;;; shampoo-dialect.el --- Dialect-specific Smalltalk routines
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-regexp)
(require 'shampoo-state)

(defstruct shampoo-dialect-specific
  extract-parent
  message-template
  version)

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
    statements
]"
    (let ((cat (shampoo-get-current-category)))
      (if (equal cat "*") "still unclassified" cat))))

(defun shampoo-gnu-smalltalk-extract-parent (class-name)
  (let ((parsed (shampoo-regexp-parse class-name '(:Wd "\\." :Wd))))
    (if parsed
        (values (shampoo-regexp-extract 0 parsed)
                (shampoo-regexp-extract 1 parsed))
      (values nil class-name))))

(defun shampoo-squeak-message-template ()
"messageSelectorAndArgumentNames
    \"comment stating purpose of message\"

    | temporary variable names |
    statements")

(defun shampoo-squeak-extract-parent (class-name)
  (values nil class-name))
 
(defun shampoo-dialect-for (version)
  (dolist (entry *shampoo-dialect-table*)
    (when (string-match (car entry) version)
      (return (funcall (cdr entry) version)))))

(defun shampoo-make-gnu-dialect (version)
  (make-shampoo-dialect-specific
   :message-template 'shampoo-gnu-smalltalk-message-template
   :extract-parent   'shampoo-gnu-smalltalk-extract-parent
   :version          version))

(defun shampoo-make-squeak-dialect (version)
  (make-shampoo-dialect-specific
   :message-template 'shampoo-squeak-message-template
   :extract-parent   'shampoo-squeak-extract-parent
   :version          version))

(defun shampoo-dialect-message-template (dialect)
  (funcall (shampoo-dialect-specific-message-template dialect)))

(defun shampoo-dialect-extract-parent (dialect class-name)
  (funcall (shampoo-dialect-specific-extract-parent dialect) class-name))

(provide 'shampoo-dialect)

;;; shampoo-dialect.el ends here.
