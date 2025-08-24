;;; org-everything.el --- Everything integration with Consult -*- lexical-binding: t; -*-

;; Copyright (C) John Haman 2022, Modified by Ypot 2025
;; Original Author: John Haman <mail@johnhaman.org>
;; Modified by: Ypot <ypo@foro.aleeas.com>
;; Original Homepage: https://github.com/jthaman/consult-everything
;; Package-Requires: ((emacs "25.1") (consult "0.15"))
;; Version: 0.2.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package integrates Voidtools Everything (es.exe) with Consult.
;; It keeps the original behavior by default and offers optional tuning via
;; customizable variables with extensive documentation.

;;; License:

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'consult)
(require 'subr-x)
(require 'cl-lib)

;;;###autoload
(defgroup org-everything nil
  "Everything + Consult integration and performance options (Everything es.exe)."
  :group 'tools
  :prefix "org-everything-")

;; Core CLI args (keeps original default and behavior)
(defcustom org-everything-args
  "es -r -n 300"
  "Command line to invoke Everything CLI (es.exe) and its flags.

This string is split using `consult--build-args` and sent as the base command
for every incremental search performed by `org-everything`.

Defaults and safety:
- Default is 'es -r'. This matches the original behavior (regex enabled),
  assuming es.exe is on PATH. Nothing else is changed unless you customize it.

Common useful flags to add here (examples):
- Ignore case: add '-i' (Everything matches case-insensitively)
- Limit results: add '-n 500' (show at most 500 items per update)
- Sorting by name: add '-sort name-ascending' or '-sort name-descending'
- Sorting by date (newest first): add '-sort date-descending'
- Sorting by date (oldest first): add '-sort date-ascending'
- Sorting by size: add '-sort size-descending' or '-sort size-ascending'

Notes:
- You can combine flags, for example: 'es -r -i -n 500 -sort date-descending'
- If you prefer case-sensitive matching, omit '-i'.
- If you want more/less candidates shown quickly, adjust the '-n' value.
- Sorting is performed by Everything (fast) before results reach Emacs."
  :type 'string
  :group 'org-everything)

(defcustom org-everything-default-query-prefix ""
  "Prefix added in front of the user input before sending it to es.exe.

Purpose:
- Pre-filter at the Everything level using its native syntax to reduce the
  candidate space early, improving responsiveness even on huge trees.

What you can put here (Everything filters):
- 'ext:pdf '           → only PDF files
- 'file:'              → files only (exclude directories)
- 'folder:'            → directories only
- 'path:src '          → restrict matches to paths containing 'src'
- 'size:>1mb '         → larger than 1 MB
- 'dm:last10days '     → modified in the last 10 days
- You can chain them, e.g., 'path:docs ext:pdf '

Examples:
- Set to 'ext:pdf ' and then search by filename; Everything will only return
  PDFs, keeping interaction snappy.
- Set to 'path:src ' to restrict results to your source tree.

Leave empty ('') to not alter the user input."
  :type 'string
  :group 'org-everything)

;; Consult async knobs (use concrete, helpful defaults)
(defcustom org-everything-consult-min-input 2
  "Minimum number of input characters before starting an asynchronous search.

Effects:
- Lower values (e.g., 1) trigger very early and can spawn frequent subprocesses
  on large trees.
- A value of 2 balances early feedback and reduced process churn.

Tune this higher if you experience many rapid updates before the query is
meaningful (e.g., set it to 3)."
  :type 'integer
  :group 'org-everything)

(defcustom org-everything-consult-refresh-delay 0.05
  "Delay (in seconds) before Consult refreshes the candidate list after input.

Effects:
- Smaller values (e.g., 0.05) show results sooner; larger values (e.g., 0.15)
  batch updates, reducing flicker and CPU use.

If initial display feels slow, try reducing to 0.05."
  :type 'number
  :group 'org-everything)

(defcustom org-everything-consult-input-throttle 0.25
  "Minimum time (in seconds) between consecutive asynchronous updates.

Acts as a rate limiter for background updates; helps on huge trees or slower
machines by reducing refresh bursts while typing fast."
  :type 'number
  :group 'org-everything)

(defcustom org-everything-consult-input-debounce 0.08
  "Waiting time (in seconds) after the last key press before updating.

Consolidates bursts of keystrokes into a single refresh; slightly larger values
favor stability over immediacy."
  :type 'number
  :group 'org-everything)

(defcustom org-everything-toggles nil
  "Interactive toggles to modify Everything arguments while using `org-everything`.

Each element is a list of five items in this order:
  (KEY ARGS-ON GROUP DESCRIPTION INITIAL)

- ARGS-ON: List of strings added to es.exe when this toggle is active
- GROUP: Symbol (or nil). Within the same GROUP only one toggle can be active
         at a time (useful for mutually exclusive sorts). Different groups do not
         affect each other.
- DESCRIPTION: Human-friendly text shown in on-demand help.
- INITIAL: t to start active when `org-everything` opens; nil to start inactive.

Notes:
- This base implementation only adds ARGS-ON from active toggles to the command.
  It does not remove flags from `org-everything-args`. Prefer to keep
  `org-everything-args` minimal and add behavior via toggles.
- Press C-c ? in the minibuffer to show a brief help of defined toggles."
  :type '(repeat (list :tag "Toggle"
                       (string :tag "Key (kbd string)")
                       (repeat :tag "Args to add when ON" string)
                       (choice :tag "Group" (const nil) symbol)
                       (string :tag "Description")
                       (boolean :tag "Initially enabled")))
  :group 'org-everything)

(defvar-local org-everything--toggle-specs nil
  "Toggle spec list for the current org-everything session (buffer-local).")

(defvar-local org-everything--toggle-state nil
  "Alist of (INDEX . ACTIVE) for toggles in this session (buffer-local).")

(defvar-local org-everything--active-toggle-args nil
  "Flattened list of args from active toggles (buffer-local).")

(defun org-everything--recompute-active-toggle-args ()
  "Recompute `org-everything--active-toggle-args` from `org-everything--toggle-state`."
  (setq org-everything--active-toggle-args
        (let ((args '()))
          (cl-loop for (idx . on) in org-everything--toggle-state do
                   (when on
                     (let* ((spec (nth idx org-everything--toggle-specs))
                            (args-on (nth 1 spec))
                            (args-on-str (mapcar (lambda (x) (if (stringp x) x (format "%s" x))) args-on)))
                       (setq args (append args args-on-str)))))
          args)))

(defun org-everything--toggle-help ()
  "Show a brief help with the configured toggles in the echo area."
  (let ((msg (mapconcat
              (lambda (spec-idx)
                (let* ((idx (car spec-idx))
                       (spec (nth idx org-everything--toggle-specs))
                       (key (key-description (nth 0 spec)))
                       (desc (nth 3 spec))
                       (on  (alist-get idx org-everything--toggle-state)))
                  (format "%s: %s%s" key desc (if on " [ON]" ""))))
              (cl-loop for i from 0 below (length org-everything--toggle-specs) collect (cons i t))
              "  • ")))
    (when (> (length msg) 0)
      (message "%s" msg))))

(defun org-everything--toggle-by-index (idx)
  "Toggle entry at IDX respecting mutual exclusion by GROUP."
  (let* ((spec (nth idx org-everything--toggle-specs))
         (group (nth 2 spec))
         (current (alist-get idx org-everything--toggle-state)))
    (if (and group (not current))
        ;; Activate this and deactivate others in the same group
        (progn
          (setq org-everything--toggle-state
                (mapcar (lambda (cell)
                          (let ((j (car cell))
                                (on (cdr cell))
                                (g (nth 2 (nth (car cell) org-everything--toggle-specs))))
                            (cons j (if (and (eq g group) (/= j idx)) nil (if (= j idx) t on)))))
                        org-everything--toggle-state))
          (org-everything--recompute-active-toggle-args))
      ;; Simple flip
      (setf (alist-get idx org-everything--toggle-state) (not current))
      (org-everything--recompute-active-toggle-args)))
  ;; Poke minibuffer input to refresh Consult
  (when (minibufferp)
    (save-excursion (insert " ") (backward-delete-char 1))))

(defun org-everything--setup-minibuffer-toggles ()
  "Initialize toggle state and keybindings in the minibuffer."
  (setq org-everything--toggle-specs org-everything-toggles)
  (setq org-everything--toggle-state
        (cl-loop for i from 0 below (length org-everything--toggle-specs)
                 collect (cons i (nth 4 (nth i org-everything--toggle-specs)))))
  (org-everything--recompute-active-toggle-args)
  ;; Bind keys
  (dolist (spec-idx (cl-loop for i from 0 below (length org-everything--toggle-specs) collect i))
    (let* ((spec (nth spec-idx org-everything--toggle-specs))
           (key (nth 0 spec))
           (fn  (lambda () (interactive) (org-everything--toggle-by-index spec-idx))))
      (when key (local-set-key (kbd key) fn))))
  ;; Help key
  (local-set-key (kbd "C-c ?") (lambda () (interactive) (org-everything--toggle-help))))

;; ===== Helpers =====

(defun org-everything--effective-args ()
  "Return the final argument vector for es.exe based on `org-everything-args'."
  (consult--build-args org-everything-args))

(defun org--everything-builder (input)
  "Build command line from INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (append (consult--build-args org-everything-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

;;;###autoload
(defun org-everything (&optional initial)
  "Search with `everything' for files matching input regexp given INITIAL input."
  (interactive)
  (find-file (consult--find "Everything: " #'org--everything-builder initial)))

(provide 'org-everything)

;;; org-everything.el ends here
