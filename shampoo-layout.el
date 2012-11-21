;;; shampoo-layout.el --- Shampoo layout builder
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-modes)
(require 'shampoo-state)
(require 'shampoo-dict)

(defun shampoo-generic-splitter (args split-fcn)
  (lexical-let ((row-funcs args)
                (splitter  split-fcn))
    (lambda (wnd)
      (let ((head (first row-funcs))
            (tail (rest row-funcs)))
        (dolist (f (reverse tail))
          (funcall f (funcall splitter wnd)))
        (funcall head wnd)))))

(defun shampoo-rows (&rest args)
  (shampoo-generic-splitter args 'split-window))

(defun shampoo-cols (&rest args)
  (shampoo-generic-splitter args (lambda (w) (split-window w nil t))))

(defun shampoo-build-layout (layout-desc)
  (delete-other-windows)
  (funcall layout-desc (selected-window))
  (balance-windows))

(defun* shampoo-make-window-setup
    (&key buffer-name mode-to-use set-binding)
  (lexical-let ((buffer  buffer-name)
                (mode    mode-to-use)
                (binding set-binding))
    (lambda (wnd)
      (let ((buff (get-buffer-create buffer)))
        (set-window-buffer wnd buff)
        (with-current-buffer buff
          (funcall mode))
        (with-~shampoo~
         (shampoo-dict-put
          :key binding
          :value wnd
          :into (shampoo-current-main-windows ~shampoo~)))))))

(defun shampoo-layout-desc ()
  (shampoo-rows
   (shampoo-cols (shampoo-setup-namespaces-window)
                 (shampoo-setup-classes-window)
                 (shampoo-setup-categories-window)
                 (shampoo-setup-methods-window))
   (shampoo-setup-source-window)))

(defun shampoo-setup-namespaces-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-namespaces*"
   :mode-to-use 'shampoo-namespaces-list-mode
   :set-binding :namespaces))

(defun shampoo-setup-classes-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-classes*"
   :mode-to-use 'shampoo-classes-list-mode
   :set-binding :classes))

(defun shampoo-setup-categories-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-categories*"
   :mode-to-use 'shampoo-cats-list-mode
   :set-binding :categories))

(defun shampoo-setup-methods-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-methods*"
   :mode-to-use 'shampoo-methods-list-mode
   :set-binding :methods))

(defun shampoo-setup-source-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-code*"
   :mode-to-use 'shampoo-code-mode
   :set-binding :source))

(defun shampoo-create-layout ()
  (shampoo-build-layout (shampoo-layout-desc)))

(defun shampoo-restore-layout ()
  (interactive)
  (shampoo-create-layout))

(provide 'shampoo-layout)

;;; shampoo-layout.el ends here.
