;;; shampoo-auth.el --- Shampoo authentication routines
;;
;; Copyright (C) 2010 - 2012 Dmitry Matveev <me@dmitrymatveev.co.uk>
;;
;; This software is released under terms of the MIT license,
;; please refer to the LICENSE file for details.

(eval-when-compile (require 'cl))
(require 'shampoo-regexp)

(defstruct shampoo-connect-info
  login host port)

(defun shampoo-connect-info-str (info)
  (format "%s@%s:%d"
          (shampoo-connect-info-login info)
          (shampoo-connect-info-host info)
          (shampoo-connect-info-port info)))

(defun shampoo-parse-login (str)
  (let ((parsed (shampoo-regexp-parse str '(:Wd "@" :Wa ":" :D))))
    (when parsed
      (make-shampoo-connect-info
         :login (shampoo-regexp-extract 0 parsed)
         :host  (shampoo-regexp-extract 1 parsed)
         :port  (string-to-number
                 (shampoo-regexp-extract 2 parsed))))))

(defun shampoo-prepare-pass (magic pass)
  (md5 (concat magic (md5 pass))))

(provide 'shampoo-auth)
  
;;; shampoo-auth.el ends here.
