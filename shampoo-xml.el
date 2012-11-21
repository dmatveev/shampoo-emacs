;;; shampoo-xml.el --- Shampoo XML processing functions
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'cl)
(require 'xml)

(defsubst shampoo-replace-in-string (str regexp newtext)
  (replace-regexp-in-string regexp newtext str t t))

;; This function has been taken from emacs-jabber.
;; Thanks to its authors.
(defun shampoo-escape-xml (str)
  (if (stringp str)
      (let ((newstr (concat str))
            (re     "[\000-\010\013\014\016-\037]"))
        (setq newstr (shampoo-replace-in-string newstr "\f" "\n"))
        (setq newstr (shampoo-replace-in-string newstr re   " "))
        (setq newstr (shampoo-replace-in-string newstr "&"  "&amp;"))
        (setq newstr (shampoo-replace-in-string newstr "<"  "&lt;"))
        (setq newstr (shampoo-replace-in-string newstr ">"  "&gt;"))
        (setq newstr (shampoo-replace-in-string newstr "'"  "&apos;"))
        (setq newstr (shampoo-replace-in-string newstr "\"" "&quot;"))
        newstr)
    str))

(defun shampoo-xml (tagname attrs &optional text subnodes)
  (with-output-to-string
    (princ (concat "<" (symbol-name tagname)))
    (mapc (lambda (attr)
            (if (keywordp attr)
                (princ (concat
                        " "
                        (substring (symbol-name attr) 1)
                        "=\""))
              (progn (princ attr)
                     (princ "\""))))
          attrs)
    (if (or text subnodes)
        (progn
          (princ ">")
          (when text (princ (shampoo-escape-xml text)))
          (when subnodes (dolist (subnode subnodes) (princ subnode)))
          (princ (concat "</" (symbol-name tagname) ">")))
      (princ " />"))))

(defun shampoo-parse-xml (str)
  (with-temp-buffer
    (insert str)
    (car (xml-parse-region (point-min) (point-max)))))

(defun shampoo-xml-attrs-hash (xml-attrs-list)
  (let ((result (make-hash-table :test 'equal)))
    (dolist (pair xml-attrs-list)
      (puthash (car pair) (cdr pair) result))
    result))

(defun shampoo-xml-nodes-named (symbol data)
  (remove-if (lambda (x)
               (or (stringp x)
                   (not (equal (car x) symbol))))
             data))

(provide 'shampoo-xml)

;;; shampoo-xml ends here.
