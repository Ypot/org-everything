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
- Default is ‘es -r’. This matches the original behavior (regex enabled),
  assuming es.exe is on PATH. Nothing else is changed unless you customize it.

Common useful flags to add here (examples):
- Ignore case: add ‘-i’ (Everything matches case-insensitively)
- Limit results: add ‘-n 500’ (show at most 500 items per update)
- Sorting by name: add ‘-sort name-ascending’ or ‘-sort name-descending’
- Sorting by date (newest first): add ‘-sort date-descending’
- Sorting by date (oldest first): add ‘-sort date-ascending’
- Sorting by size: add ‘-sort size-descending’ or ‘-sort size-ascending’

Notes:
- You can combine flags, for example: ‘es -r -i -n 500 -sort date-descending’
- If you prefer case-sensitive matching, omit ‘-i’.
- If you want more/less candidates shown quickly, adjust the ‘-n’ value.
- Sorting is performed by Everything (fast) before results reach Emacs."
  :type 'string
  :group 'org-everything)

(defcustom org-everything-default-query-prefix ""
  "Prefix added in front of the user input before sending it to es.exe.

Purpose:
- Pre-filter at the Everything level using its native syntax to reduce the
  candidate space early, improving responsiveness even on huge trees.

What you can put here (Everything filters):
- ‘ext:pdf ’           → only PDF files
- ‘file:’              → files only (exclude directories)
- ‘folder:’            → directories only
- ‘path:src ’          → restrict matches to paths containing ‘src’
- ‘size:>1mb ’         → larger than 1 MB
- ‘dm:last10days ’     → modified in the last 10 days
- You can chain them, e.g., ‘path:docs ext:pdf ’

Examples:
- Set to ‘ext:pdf ’ and then search by filename; Everything will only return
  PDFs, keeping interaction snappy.
- Set to ‘path:src ’ to restrict results to your source tree.

Leave empty (‘’) to not alter the user input."
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

(defcustom org-everything-consult-preview-key nil
  "Local preview behavior for Consult while running `org-everything`.

Meaning:
- nil → inherit your global `consult-preview-key` (no local change)
- ‘any → preview on any selection change
- KEY or list of KEYs → preview only when you press those keys (e.g., ‘M-.’)

Trade-offs:
- ‘any is very immediate but can be noisy on big projects.
- A dedicated trigger (e.g., ‘M-.’) keeps navigation fast and previews on-demand.
- Inheriting (nil) is simplest if you already tuned preview globally."
  :type '(choice (const :tag "Inherit global preview (default)" nil)
                 (const :tag "Preview on any selection change" any)
                 key-sequence
                 (repeat key-sequence))
  :group 'org-everything)

;; ===== Helpers =====

(defun org-everything--effective-args ()
  "Return the final argument vector for es.exe based on `org-everything-args'."
  (consult--build-args org-everything-args))

(defun org--everything-builder (input)
  "Build Everything CLI command from INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (let* ((pref org-everything-default-query-prefix)
           (final-arg (if (and (stringp pref) (not (string-empty-p pref))
                               (not (string-blank-p arg)))
                          (concat pref arg)
                        arg)))
      (unless (string-blank-p final-arg)
        (cons (append (org-everything--effective-args)
                      (consult--split-escaped final-arg) opts)
              (cdr (consult--default-regexp-compiler input 'basic t)))))))

;;;###autoload
(defun org-everything (&optional initial)
  "Search with Everything for files matching input regexp given INITIAL input.

Tips to improve perceived startup speed:
- Consider adding ‘-n 300’ to `org-everything-args` to limit initial output.
- Lower `org-everything-consult-refresh-delay` to 0.05 for faster first paint.
- Use a restrictive `org-everything-default-query-prefix` (e.g., ‘path:src ’).
These reduce work before the first results appear."
  (interactive)
  (let ((consult-async-min-input       (if (numberp org-everything-consult-min-input)
                                          org-everything-consult-min-input
                                        consult-async-min-input))
        (consult-async-refresh-delay   (if (numberp org-everything-consult-refresh-delay)
                                          org-everything-consult-refresh-delay
                                        consult-async-refresh-delay))
        (consult-async-input-throttle  (if (numberp org-everything-consult-input-throttle)
                                          org-everything-consult-input-throttle
                                        consult-async-input-throttle))
        (consult-async-input-debounce  (if (numberp org-everything-consult-input-debounce)
                                          org-everything-consult-input-debounce
                                        consult-async-input-debounce))
        (consult-preview-key           (if (not (null org-everything-consult-preview-key))
                                          org-everything-consult-preview-key
                                        consult-preview-key)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)))
      (find-file (consult--find "Everything: " #'org--everything-builder initial))))

(provide 'org-everything)

;;; org-everything.el ends here
