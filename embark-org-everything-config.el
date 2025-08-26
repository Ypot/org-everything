;;; embark-org-everything-config.el --- Embark configuration for org-everything -*- lexical-binding: t; -*-

;; This configuration enables Embark actions on org-everything search results
;; allowing you to perform Dired-like operations on files found by Everything

;;; Commentary:

;; This file configures Embark to work with org-everything search results.
;; It adds actions for file operations like copy, delete, rename, etc.
;; 
;; Usage:
;; 1. Load this file in your Emacs configuration
;; 2. Use org-everything to search for files
;; 3. Press C-. (or your embark-act key) on a file to see available actions
;; 4. Select an action to perform it on the file
;;
;; Key actions available:
;; - Delete files (C-c C-x C-w equivalent)
;; - Copy files (C-c C-x M-w equivalent) 
;; - Store org links (C-c l equivalent)
;; - Browse URLs
;; - Copy filenames to kill ring
;; - And many more...

;;; Code:

(require 'embark)
(require 'dired)
(require 'dired-aux)
(require 'org)

;; Define actions for files found by org-everything
(defvar embark-org-everything-actions
  '(("Delete file" . embark-org-everything-delete-file)
    ("Copy file" . embark-org-everything-copy-file)
    ("Rename file" . embark-org-everything-rename-file)
    ("Move file" . embark-org-everything-move-file)
    ("Store org link" . embark-org-everything-store-link)
    ("Browse URL" . embark-org-everything-browse-url)
    ("Copy filename" . embark-org-everything-copy-filename)
    ("Copy absolute filename" . embark-org-everything-copy-abs-filename)
    ("Open with default app" . embark-org-everything-open-with-default)
    ("Open in dired" . embark-org-everything-open-in-dired)
    ("Open in finder/explorer" . embark-org-everything-open-in-finder)
    ("Properties" . embark-org-everything-properties)
    ("Chmod" . embark-org-everything-chmod)
    ("Touch" . embark-org-everything-touch))
  "Actions available for files in org-everything results.")

;; Helper function to get file path from org-everything result
(defun embark-org-everything--get-file-path (target)
  "Extract file path from org-everything TARGET."
  (if (stringp target)
      (expand-file-name target)
    (error "Invalid target: %s" target)))

;; Helper function to check if file exists
(defun embark-org-everything--file-exists-p (file)
  "Check if FILE exists and is accessible."
  (and (file-exists-p file) (file-readable-p file)))

;; Helper function to get directory of file
(defun embark-org-everything--get-directory (file)
  "Get directory containing FILE."
  (file-name-directory (expand-file-name file)))

;; Action implementations

(defun embark-org-everything-delete-file (file)
  "Delete FILE found by org-everything."
  (interactive "fFile to delete: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (if (y-or-n-p (format "Delete file %s? " file-path))
          (delete-file file-path)
        (message "Deletion cancelled")))))

(defun embark-org-everything-copy-file (file)
  "Copy FILE found by org-everything."
  (interactive "fFile to copy: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (let ((dest (read-file-name "Copy to: " 
                                  (embark-org-everything--get-directory file-path))))
        (copy-file file-path dest t)
        (message "Copied %s to %s" file-path dest)))))

(defun embark-org-everything-rename-file (file)
  "Rename FILE found by org-everything."
  (interactive "fFile to rename: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (let ((new-name (read-file-name "Rename to: " 
                                     (embark-org-everything--get-directory file-path)
                                     file-path)))
        (rename-file file-path new-name)
        (message "Renamed %s to %s" file-path new-name)))))

(defun embark-org-everything-move-file (file)
  "Move FILE found by org-everything."
  (interactive "fFile to move: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (let ((dest (read-directory-name "Move to directory: " 
                                      (embark-org-everything--get-directory file-path))))
        (rename-file file-path (expand-file-name (file-name-nondirectory file-path) dest))
        (message "Moved %s to %s" file-path dest)))))

(defun embark-org-everything-store-link (file)
  "Store org link for FILE found by org-everything."
  (interactive "fFile to store link for: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (org-store-link file-path)
      (message "Stored org link for %s" file-path))))

(defun embark-org-everything-browse-url (file)
  "Browse URL for FILE found by org-everything."
  (interactive "fFile to browse: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (browse-url-of-file file-path))))

(defun embark-org-everything-copy-filename (file)
  "Copy filename of FILE found by org-everything to kill ring."
  (interactive "fFile to copy filename: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (kill-new (file-name-nondirectory file-path))
      (message "Copied filename: %s" (file-name-nondirectory file-path)))))

(defun embark-org-everything-copy-abs-filename (file)
  "Copy absolute filename of FILE found by org-everything to kill ring."
  (interactive "fFile to copy absolute filename: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (kill-new file-path)
      (message "Copied absolute filename: %s" file-path))))

(defun embark-org-everything-open-with-default (file)
  "Open FILE found by org-everything with default application."
  (interactive "fFile to open: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (start-process "open-file" nil "xdg-open" file-path))))

(defun embark-org-everything-open-in-dired (file)
  "Open dired buffer showing FILE found by org-everything."
  (interactive "fFile to show in dired: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (dired (embark-org-everything--get-directory file-path))
      (dired-goto-file file-path))))

(defun embark-org-everything-open-in-finder (file)
  "Open FILE found by org-everything in system file manager."
  (interactive "fFile to show in file manager: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (start-process "open-finder" nil "xdg-open" 
                     (embark-org-everything--get-directory file-path)))))

(defun embark-org-everything-properties (file)
  "Show properties of FILE found by org-everything."
  (interactive "fFile to show properties: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (dired-do-chmod file-path))))

(defun embark-org-everything-chmod (file)
  "Change permissions of FILE found by org-everything."
  (interactive "fFile to change permissions: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (dired-do-chmod file-path))))

(defun embark-org-everything-touch (file)
  "Touch FILE found by org-everything (update timestamp)."
  (interactive "fFile to touch: ")
  (let ((file-path (embark-org-everything--get-file-path file)))
    (when (embark-org-everything--file-exists-p file-path)
      (set-file-times file-path)
      (message "Touched %s" file-path))))

;; Configure Embark to use these actions for file targets
(defun embark-org-everything-setup ()
  "Setup Embark actions for org-everything file results."
  (add-to-list 'embark-target-finders #'embark-org-everything-target-finder)
  (add-to-list 'embark-keymap-alist '(file . embark-org-everything-keymap)))

(defun embark-org-everything-target-finder ()
  "Find file targets in org-everything results."
  (when (and (derived-mode-p 'consult--read-mode)
             (string-match "Everything:" (buffer-name)))
    (let ((candidate (embark--target-at-point)))
      (when (and candidate (file-exists-p candidate))
        candidate))))

;; Create keymap for org-everything file actions
(defvar embark-org-everything-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'embark-org-everything-delete-file)
    (define-key map (kbd "c") #'embark-org-everything-copy-file)
    (define-key map (kbd "r") #'embark-org-everything-rename-file)
    (define-key map (kbd "m") #'embark-org-everything-move-file)
    (define-key map (kbd "l") #'embark-org-everything-store-link)
    (define-key map (kbd "w") #'embark-org-everything-browse-url)
    (define-key map (kbd "y") #'embark-org-everything-copy-filename)
    (define-key map (kbd "Y") #'embark-org-everything-copy-abs-filename)
    (define-key map (kbd "o") #'embark-org-everything-open-with-default)
    (define-key map (kbd "D") #'embark-org-everything-open-in-dired)
    (define-key map (kbd "f") #'embark-org-everything-open-in-finder)
    (define-key map (kbd "p") #'embark-org-everything-properties)
    (define-key map (kbd "t") #'embark-org-everything-touch)
    map)
  "Keymap for org-everything file actions in Embark.")

;; Alternative: Use the actions list directly
(setq embark-org-everything-actions-map
      (let ((map (make-sparse-keymap)))
        (dolist (action embark-org-everything-actions)
          (define-key map (vector (intern (car action))) (cdr action)))
        map))

;; Setup function to be called after loading
(defun embark-org-everything-init ()
  "Initialize Embark integration with org-everything."
  (embark-org-everything-setup)
  (message "Embark org-everything integration loaded"))

;; Auto-setup when this file is loaded
(embark-org-everything-init)

(provide 'embark-org-everything-config)

;;; embark-org-everything-config.el ends here