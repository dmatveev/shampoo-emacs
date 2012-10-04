;;; shampoo-layout.el --- Shampoo layout builder
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(require 'shampoo-modes)

(defmacro shampoo-generate-layout-impl (surface &rest data-list)
  (let ((decls '()) (commands '()) (buffa (make-symbol "buffa")))
    (dolist (data data-list)
      (destructuring-bind (sym wnd bfr code mode) data
        (pushnew `(let ((,buffa (get-buffer-create ,bfr)))
                    (set-window-buffer ,wnd ,buffa)
                    (save-excursion
                      (set-buffer ,buffa)
                      (,mode))
                    (puthash ',sym ,wnd surface))
                 commands)
        (pushnew `(,wnd ,code) decls)))
    `(let* (,@decls) ,@commands)))

;; TODO refactor this
(defun shampoo-create-layout ()
  (delete-other-windows)
  (let ((root (selected-window))
        (surface (make-hash-table)))
    (shampoo-generate-layout-impl
     surface
     (nspc-w nspc "*shampoo-namespaces*" root shampoo-namespaces-list-mode)
     (mets-w meth "*shampoo-methods*"    (split-window cats nil t) shampoo-methods-list-mode)
     (clss-w clss "*shampoo-classes*"    (split-window root nil t) shampoo-classes-list-mode)
     (cats-w cats "*shampoo-categories*" (split-window root nil t) shampoo-cats-list-mode)
     (code-w code "*shampoo-code*"       (split-window root) shampoo-code-mode))
    surface))

(provide 'shampoo-layout)

;;; shampoo-layout.el ends here.
