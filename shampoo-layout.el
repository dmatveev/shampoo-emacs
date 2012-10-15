;;; shampoo-layout.el --- Shampoo layout builder
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-modes)

(defun shampoo-generic-splitter (args split-fcn)
  (lexical-let ((row-funcs args)
                (splitter split-fcn))
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

(defun* shampoo-make-window-setup (&key buffer-name mode-to-use)
  (lexical-let ((buffer buffer-name)
                (mode mode-to-use))
    (lambda (wnd)
      (let ((buff (get-buffer-create buffer)))
        (set-window-buffer wnd buff)
        (save-excursion
          (set-buffer buff)
          (funcall mode))))))

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
   :mode-to-use 'shampoo-namespaces-list-mode))

(defun shampoo-setup-classes-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-classes*"
   :mode-to-use 'shampoo-classes-list-mode))

(defun shampoo-setup-categories-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-categories*"
   :mode-to-use 'shampoo-cats-list-mode))

(defun shampoo-setup-methods-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-methods*"
   :mode-to-use 'shampoo-methods-list-mode))

(defun shampoo-setup-source-window ()
  (shampoo-make-window-setup
   :buffer-name "*shampoo-code*"
   :mode-to-use 'shampoo-code-mode))

(defun shampoo-create-layout ()
  (shampoo-build-layout (shampoo-layout-desc)))

(defun shampoo-restore-layout ()
  (interactive)
  (shampoo-create-layout))

(provide 'shampoo-layout)

;;; shampoo-layout.el ends here.
