;;; org-everything.el --- Everything integration with Consult -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: You
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (consult "1.0"))

;;; Commentary:

;; Minimal Everything integration with Consult. Requires Everything CLI `es` in PATH.

;;; Code:

(require 'consult)

(defcustom org-everything-args
  "es -r"
  "Command line for Everything CLI and default arguments."
  :type 'string)

(defun org--everything-builder (input)
  "Build command line for Everything from INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-empty-p arg)
      (cons (append (consult--build-args org-everything-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

;;;###autoload
(defun org-everything (&optional initial)
  "Search with Everything for files matching input regexp given INITIAL input."
  (interactive)
  (find-file (consult--find "Everything: " #'org--everything-builder initial)))

(provide 'org-everything)

;;; org-everything.el ends here