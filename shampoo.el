(require 'cl)

(defmacro shampoo-generate-layout-impl (surface &rest data-list)
  (let ((decls '()) (commands '()))
    (dolist (data data-list)
      (destructuring-bind (hash-symbol window buffer split-code) data
        (pushnew `(puthash (quote hash-symbol) ,window surface) commands)
        (pushnew `(set-window-buffer ,window (get-buffer-create ,buffer)) commands)
        (pushnew `(,window ,split-code) decls)))
    `(let* (,@decls) ,@commands)))


(defun shampoo-create-layout ()
  (interactive)
  (delete-other-windows)
  (let ((root-window (selected-window))
        (surface (make-hash-table)))
    (shampoo-generate-layout-impl
     surface
     (nspc-w nspc-window "*shampoo-namespaces*" root-window)
     (mets-w meth-window "*shampoo-methods*"    (split-window cats-window nil t))
     (clss-w clss-window "*shampoo-classes*"    (split-window root-window nil t))
     (cats-w cats-window "*shampoo-categories*" (split-window root-window nil t))
     (code-w code-window "*shampoo-code*"       (split-window root-window)))
    surface))
