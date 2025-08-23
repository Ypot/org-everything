(require 'consult)

(defcustom org-everything-args
  "es -r"
  "Everything CLI command and default arguments."
  :type 'string)

(defun org--everything-builder (input)
  "Build command list for Everything from INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (when (and (stringp arg) (> (length arg) 0))
      (cons (append (consult--build-args org-everything-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

;;;###autoload
(defun org-everything (&optional initial)
  "Search with Everything for files matching INPUT."
  (interactive)
  (find-file (consult--find "Everything: " #'org--everything-builder initial)))

(provide 'org-everything)