;;; embark-org-everything-simple.el --- Simple Embark config for org-everything -*- lexical-binding: t; -*-

;; Simple configuration to enable Dired-like actions on org-everything results
;; Based on the user's requested actions:
;; - dired-do-delete (C-c C-x C-w)
;; - dired-do-copy (C-c C-x M-w) 
;; - org-store-link (C-c l)
;; - browse-url-of-dired-file (w)
;; - dired-copy-filename-as-kill (M-w)
;; - diredp-copy-abs-filenames-as-kill (C-w)

;;; Code:

(require 'embark)
(require 'dired)
(require 'org)

;; Simple action implementations for org-everything results

(defun embark-org-everything-delete (file)
  "Delete FILE found by org-everything (equivalent to dired-do-delete)."
  (interactive "fFile to delete: ")
  (let ((file-path (expand-file-name file)))
    (when (file-exists-p file-path)
      (if (y-or-n-p (format "Delete file %s? " file-path))
          (delete-file file-path)
        (message "Deletion cancelled")))))

(defun embark-org-everything-copy (file)
  "Copy FILE found by org-everything (equivalent to dired-do-copy)."
  (interactive "fFile to copy: ")
  (let ((file-path (expand-file-name file)))
    (when (file-exists-p file-path)
      (let ((dest (read-file-name "Copy to: " 
                                  (file-name-directory file-path))))
        (copy-file file-path dest t)
        (message "Copied %s to %s" file-path dest)))))

(defun embark-org-everything-store-link (file)
  "Store org link for FILE found by org-everything (equivalent to org-store-link)."
  (interactive "fFile to store link for: ")
  (let ((file-path (expand-file-name file)))
    (when (file-exists-p file-path)
      (org-store-link file-path)
      (message "Stored org link for %s" file-path))))

(defun embark-org-everything-browse-url (file)
  "Browse URL for FILE found by org-everything (equivalent to browse-url-of-dired-file)."
  (interactive "fFile to browse: ")
  (let ((file-path (expand-file-name file)))
    (when (file-exists-p file-path)
      (browse-url-of-file file-path))))

(defun embark-org-everything-copy-filename (file)
  "Copy filename of FILE found by org-everything (equivalent to dired-copy-filename-as-kill)."
  (interactive "fFile to copy filename: ")
  (let ((file-path (expand-file-name file)))
    (when (file-exists-p file-path)
      (kill-new (file-name-nondirectory file-path))
      (message "Copied filename: %s" (file-name-nondirectory file-path)))))

(defun embark-org-everything-copy-abs-filename (file)
  "Copy absolute filename of FILE found by org-everything (equivalent to diredp-copy-abs-filenames-as-kill)."
  (interactive "fFile to copy absolute filename: ")
  (let ((file-path (expand-file-name file)))
    (when (file-exists-p file-path)
      (kill-new file-path)
      (message "Copied absolute filename: %s" file-path))))

;; Configure Embark to recognize file targets in org-everything results
(defun embark-org-everything-target-finder ()
  "Find file targets in org-everything results."
  (when (and (derived-mode-p 'consult--read-mode)
             (string-match "Everything:" (buffer-name)))
    (let ((candidate (embark--target-at-point)))
      (when (and candidate (file-exists-p candidate))
        candidate))))

;; Add the target finder to Embark
(add-to-list 'embark-target-finders #'embark-org-everything-target-finder)

;; Create a keymap for the specific actions
(defvar embark-org-everything-keymap
  (let ((map (make-sparse-keymap)))
    ;; Map the specific keys mentioned by the user
    (define-key map (kbd "C-c C-x C-w") #'embark-org-everything-delete)
    (define-key map (kbd "C-c C-x M-w") #'embark-org-everything-copy)
    (define-key map (kbd "C-c l") #'embark-org-everything-store-link)
    (define-key map (kbd "w") #'embark-org-everything-browse-url)
    (define-key map (kbd "M-w") #'embark-org-everything-copy-filename)
    (define-key map (kbd "C-w") #'embark-org-everything-copy-abs-filename)
    ;; Add some additional useful shortcuts
    (define-key map (kbd "d") #'embark-org-everything-delete)
    (define-key map (kbd "c") #'embark-org-everything-copy)
    (define-key map (kbd "l") #'embark-org-everything-store-link)
    (define-key map (kbd "y") #'embark-org-everything-copy-filename)
    (define-key map (kbd "Y") #'embark-org-everything-copy-abs-filename)
    map)
  "Keymap for org-everything file actions in Embark.")

;; Add the keymap to Embark's keymap alist
(add-to-list 'embark-keymap-alist '(file . embark-org-everything-keymap))

;; Alternative: Define actions that can be selected from a menu
(setq embark-org-everything-actions
      '(("Delete file (C-c C-x C-w)" . embark-org-everything-delete)
        ("Copy file (C-c C-x M-w)" . embark-org-everything-copy)
        ("Store org link (C-c l)" . embark-org-everything-store-link)
        ("Browse URL (w)" . embark-org-everything-browse-url)
        ("Copy filename (M-w)" . embark-org-everything-copy-filename)
        ("Copy absolute filename (C-w)" . embark-org-everything-copy-abs-filename)))

;; Add these actions to Embark's action alist
(add-to-list 'embark-action-alist '(file . embark-org-everything-actions))

(message "Embark org-everything simple integration loaded")

(provide 'embark-org-everything-simple)

;;; embark-org-everything-simple.el ends here