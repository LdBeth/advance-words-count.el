;;; advance-wc-mode.el --- Advance `wc-mode'
;;
;; Filename: advance-wc-mode.el
;; Description:
;; Author: LdBeth
;; Maintainer:
;; Created: Fri Mar 31 23:00:08 2017 (+0800)
;; Version: 0.8.8
;; Package-Requires: (wc-mode)
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;; (require 'wc-mode)
(require 'advance-words-count)

(defgroup advance-wc-mode nil
  "Advanced `wc-mode'."
  :group 'andvance-words-count)

(defcustom advance-wc-modeline-format "[%W%w/%tw]"
  "The format string for the modeline.
The detailed information for this minor mode can be shown in many
ways in the modeline. The formatting strings recognized in this
format are as follows.

  %W  Original word count (before changes)
  %w  Change in words
  %gw Word change goal
  %tw Total words in buffer

The default modeline, WC[%W%w/%tw], will display the original number
of words followed by the change in words (delta), followed by the total
number of words in the buffer.
It will looks something like WC[742+360/1100] in the modeline."
  :group 'advance-wc-mode)

(defcustom advance-wc-mode-hook nil
  "Hook to run when entering wc-mode."
  :type 'hook
  :group 'advance-wc-mode)

(defface advance-wc-goal-face
  '((t (:inherit highlight)))
  "Face for modeline when goal is reached"
  :group 'advance-wc-mode)

(defvar advance-wc-orig-words nil
  "Original count of words in the buffer.
Can be a list.")
(make-variable-buffer-local 'wc-orig-words)

(defvar advance-wc-words-delta 0
  "Change in word count.
Can be a list.")
(make-variable-buffer-local 'wc-words-delta)

(defvar advance-wc-word-goal nil
  "Goal for number of words added.
Can be a list.")
(make-variable-buffer-local 'wc-word-goal)

(defvar wc-modeline-format-alist
  '(("%W" . (number-to-string advance-wc-orig-words))
    ("%w" . (wc-prepend-sign advance-wc-words-delta))
    ("%gw" . (wc-prepend-sign advance-wc-word-goal))
    ("%tw" . (number-to-string (+ advance-wc-orig-words advance-wc-words-delta))))
  "Format and value pairs.
Format will be evaluated in `advance-wc-generate-modeline'")

(defsubst advance-wc-format-modeline-string (fmt)
  "Format the modeline string according to FMT."
  (let ((case-fold-search nil)
        (str fmt))
    (dolist (pair wc-modeline-format-alist str)
      (when (string-match (car pair) str)
        (setq str (replace-match (eval (cdr pair)) t t str))))))

(defsubst advance-wc-prepend-sign (val)
  "Add a sign to the beginning of VAL.
Also cheat here a bit and add nil-value processing."
  (if val
      (format "%s%d"
        (if (< val 0)
		  "-" "+")
	      (abs val))
    "none"))

(defun advance-wc-reset ()
  "Reset the original word count to their current value."
  (interactive)
  (setq advance-wc-orig-words nil)
  (advance-wc-mode-update))

(defun advance-wc-set-word-goal (goal)
  "Set a GOAL for adding or removing words in the buffer."
  (interactive "nHow many words: ")
  (setq advance-wc-word-goal goal)
  (advance-wc-reset)
  (message "Goal set at %d." goal))

(defsubst advance-wc-goal-reached ()
  "Return t when the goal change is reached."
   (if advance-wc-word-goal
       (if (< advance-wc-word-goal 0)
	   (<= advance-wc-words-delta advance-wc-word-goal)
	 (>= advance-wc-words-delta advance-wc-word-goal))))

;; (defalias 'wc 'wc-count
;;   "Alias function `wc-count' to the more legible `wc'.")

(defun advance-wc-generate-modeline ()
  "Generate the modeline."
  (let ((modeline (advance-wc-format-modeline-string advance-wc-modeline-format)))
    (when (advance-wc-goal-reached)
      (put-text-property 0 (length modeline) 'face 'wc-goal-face modeline))
    (list " " modeline)))

(defun advance-wc-mode-update ()
  "Return a string to update the modeline appropriately."
  (let* ((stats (words-count (point-min) (point-max) words-count-rule-ansci)))
    (unless advance-wc-orig-words
      (setq advance-wc-orig-words stats))
    (setq advance-wc-words-delta (- stats advance-wc-orig-words))
    (advance-wc-generate-modeline)))

;;;###autoload
(define-minor-mode advance-wc-mode
  "Toggle `advance-wc-mode' with no argument, this command
toggles the mode. Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Wc mode is enabled on a buffer, it counts the current words
in the buffer and keeps track of a differential of added or
subtracted words.

A goal of number of words added/subtracted can be set while using
this mode. Upon completion of the goal, the modeline text will
highlight indicating that the goal has been reached.

Entry to this mode calls the value of `advance-wc-mode-hook' if that
value is non-nil."
  ;; initial value (off)
  :init-value nil
  ;; The indicator for the mode line
  :lighter (:eval (advance-wc-mode-update))
  ;; The customization group
  :group 'advance-wc-mode
  ;; The local keymap to use
  :keymap wc-mode-map
  ;; The mode body code
  (if advance-wc-mode
      (run-mode-hooks  'advance-wc-mode-hooks)))

;;;###autoload
(defmacro advance-wc-setup (mode &rest body)
  "Define a function to be called to set up `wc-mode'.
Hook `wc-mode' to MODE and define corresponding functions.
If BODY is specified, do BODY when enters MODE."
  (declare (indent 1))
  (let* ((name (intern (format "advance-wc-init-%S" mode)))
         (hook (intern (format "%s-hook" mode))))
    (progn
      `(add-hook (quote ,hook) 'advance-wc-mode)
      (if body
          `(defun ,name ()
             "set up `advance-wc-mode' when called."
             ,(append '(progn) body))
        `(add-hook (quote ,hook) ,name)))))

(provide 'advance-wc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; advance-wc-mode.el ends here
