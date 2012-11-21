;;; shampoo-fetcher.el --- Shampoo message fetcher
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-modes)
(require 'shampoo-utils)

;; Message fetcher can be described as a finite state machine:
;;
;;                                   got "Content-Length: XXX\r\n"
;;   ********     +----------------+     "\r\n"
;;   * INIT *---->| Parsing header |-----------------.
;;   ********     +----------------+                 V
;;                       ^                 +-----------------+
;;                       '-----------------| Parsing payload |
;;         got that XXX bytes and "\r\n"   +-----------------+
;;

(defstruct shampoo-fetcher-fsm state bytes-awaiting)

(defun shampoo-fetcher-fsm-switch-to-header (fsm)
  (setf (shampoo-fetcher-fsm-state fsm) :header))

(defun shampoo-fetcher-fsm-switch-to-payload (fsm payload-len)
  (setf (shampoo-fetcher-fsm-state fsm) :payload)
  (setf (shampoo-fetcher-fsm-bytes-awaiting fsm) payload-len))

(defun shampoo-fetcher-can-process-header (fsm)
  (<= 2 (shampoo-buffer-num-lines)))

(defun shampoo-fetcher-can-process-payload (fsm)
  (<= (shampoo-fetcher-fsm-bytes-awaiting fsm)
      (- (point-max) (point-min))))

(defun shampoo-fetcher-can-process (fsm)
  (let ((state (shampoo-fetcher-fsm-state fsm)))
    (cond ((eq state :header)
           (shampoo-fetcher-can-process-header fsm))
          ((eq state :payload)
           (shampoo-fetcher-can-process-payload fsm)))))

(defun shampoo-crp (str)
  (let ((l (length str)))
    (and (>= l 1)
         (equal "\r" (substring str (- l 1))))))

(defun shampoo-uncr (str)
  (if (shampoo-crp str)
      (substring str 0 (- (length str) 1))
    str))

(defun shampoo-fetcher-fsm-process-header (fsm)
  (let* ((this-str     (shampoo-uncr (shampoo-this-line)))
         (next-str     (shampoo-uncr (shampoo-next-line)))
         (maybe-header (concat this-str "\r\n" next-str "\r\n"))
         (pattern      '("Content-Length:" :sp :D :cr :lf :cr :lf))
         (parsed       (shampoo-regexp-parse maybe-header pattern)))
    ; (message "Trying to parse \"%s\"" maybe-header)
    (if parsed
        (save-excursion
          ; (message "Parsed ok!")
          (let ((len
                 (string-to-number (shampoo-regexp-extract 0 parsed))))
            (goto-char (point-min))
            (loop repeat 2 do (shampoo-delete-this-line))
            (shampoo-fetcher-fsm-switch-to-payload fsm len)))
      (erase-buffer)))
    nil)

(defun shampoo-fetcher-fsm-process-payload (fsm)
  (let* ((len (shampoo-fetcher-fsm-bytes-awaiting fsm))
         (cur (point))
         (end (+ cur len))
         (str (buffer-substring (point) end)))
    (save-excursion
      (delete-region cur end)
      (goto-char (point-min))
      (shampoo-delete-this-line))
    (shampoo-fetcher-fsm-switch-to-header fsm)
    str))
          
(defun shampoo-fetcher-fsm-process (fsm)
  (let ((state (shampoo-fetcher-fsm-state fsm)))
    (cond ((eq state :header)
           ; (message "Processing header")
           (shampoo-fetcher-fsm-process-header fsm))
          ((eq state :payload)
           ; (message "Processing payload")
           (shampoo-fetcher-fsm-process-payload fsm)))))

(defun shampoo-fetcher-process (str)
  (with-current-buffer (shampoo-fetcher-buffer)
    (when (boundp 'buflocal-fsm)
      (goto-char (point-max))
      (insert str)
      (goto-char (point-min))
      (loop while (shampoo-fetcher-can-process buflocal-fsm)
            for payload = (shampoo-fetcher-fsm-process buflocal-fsm)
            unless (null payload)
            collect payload))))

(defun shampoo-fetcher-buffer ()
  (let* ((bufname "*shampoo-working-buffer*")
         (buffer  (get-buffer bufname)))
    (or buffer
        (with-current-buffer (get-buffer-create bufname)
          (shampoo-working-mode)
          (erase-buffer)
          (if (boundp 'buflocal-fsm)
              (setq buflocal-fsm
                    (make-shampoo-fetcher-fsm :state :header 
                                              :bytes-awaiting 0))
            (error "No FSM created in the working buffer!"))
          (current-buffer)))))

(provide 'shampoo-fetcher)

;;; shampoo-fetcher.el ends here.
