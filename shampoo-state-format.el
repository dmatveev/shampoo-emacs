;;; shampoo-state-format.el --- Some state-dependent functions
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'shampoo-state)
(require 'shampoo-dialect)

(defun shampoo-format-class-name (name)
  (if (shampoo-side-is :instance)
      name
    (format "%s class" name)))

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

(defun shampoo-build-method-name (class method)
  (with-~shampoo~
   (shampoo-set-str-face
    (format "%s>>%s" (shampoo-format-class-name class) method)
    'shampoo-method-name)))

(provide 'shampoo-state-format)

;;; shampoo-state-format.el ends here.