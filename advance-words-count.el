;;; advance-words-count.el --- A Package Provides Extended `count-words'
;;
;; Filename: advance-words-count.el
;; Description: Provides Extended `count-words' function.
;; Author: LdBeth <andpuke@foxmail.com>
;; Maintainer: LdBeth <andpuke@foxmail.com>
;; Created: Wed Mar 29 14:42:25 2017 (+0800)
;; Version: 0.8.8
;; Package-Requires: (pos-tip)
;; Last-Updated:
;;           By:
;;     Update #: 1
;; URL: https://github.com/LdBeth/advance-words-count.el/
;; Doc URL:
;; Keywords: wp
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
(defgroup andvance-words-count nil
  "Extended `count-words' function."
  :prefix "words-count"
  :group 'text)

(defcustom words-count-rule-CJK "\\cc"
  "A regexp string to match CJK characters."
  :group 'advance-words-count
  :type 'string)

(defcustom words-count-rule-nonespace "[^[:space:]]"
  "A regexp string to match none space characters."
  :group 'advance-words-count
  :type 'string)

(defcustom words-count-rule-ansci "[A-Za-z0-9][A-Za-z0-9[:punct:]]*"
  "A regexp string to match ANSCII characters."
  :group 'advance-words-count
  :type 'string)

(defcustom words-count-regexp-list
  (list words-count-rule-CJK
        words-count-rule-nonespace
        words-count-rule-ansci)
  "A list for the regexp used in `advance-words-count'."
  :group 'advance-words-count
  :type '(repeat regexp))
(make-variable-buffer-local 'words-count-regexp-list)

(defcustom words-count-message-function 'words-count--format-message
  "The function used to format message in `advance-words-count'."
  :group 'advance-words-count
  :type '(function))
(make-variable-buffer-local 'words-count-message-function)

(defcustom words-count-message-display 'minibuffer
  "The way how `words-count--message' display messages."
  :group 'advance-words-count
  :type '(choice (const minibuffer)
                 (const pos-tip)))

(defsubst words-count (start end regexp)
  "Count the word from START to END with REGEXP."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (goto-char start)
        (while (and (< (point) end) (re-search-forward regexp end t))
          (setq count (1+ count)))))
    count))

;; This function is perserved as an exmple for configuration.
(defun words-count--format-message (cons &optional arg)
  "Format a string to be shown for `words-count--message'.
Using the CONS passed form `advance-words-count'. See
`count-lines' & `count-words'. When ARG is specified, display
verbosely."
  (let ((start (car cons))
        (end (cdr cons))
        list)
    (setq list (advance-words-count start end))
    (format
     (if arg
         "
-----------~*~ Words Count ~*~----------

 Characters (without Space) .... %d
 Characters (all) .............. %d
 Number of Lines ............... %d
 ANSCII Words .................. %d
%s
========================================
"
       "Ns:%d, Al:%d, Ln:%d, An:%d, %s")
     (cadr list)
     (- end start)
     (count-lines start end)
     (car (last list))
     (if (= 0 (car list))
         (format (if arg
                     " Latin Words ................... %d\n"
                   "La:%d")
                 (count-words start end))
       (format (if arg
                   " CJK Chars ..................... %d
 Word Count .................... %d\n"
                 "Ha:%d, Wc:%d")
               (car list)
               (+ (car list) (car (last list))))))))

;;;###autoload
(defmacro define-words-count-function
    (name message rules &optional bind regexp)
  "Define the function used to format the strings displayed.

NAME    = Function's name.
MESSAGE = A string used to display.
RULES   = A list of functions to form the string.
BIND    = A boolean, if ture, bind the function to
          `words-count-message-function'.
REGEXP  = A list of regexp to call, if not specified, use
          `words-count-regexp-list'."
  `(progn
     (defun ,name (cons &optional arg)
       "Format a string to be shown for `words-count--message'.
Using the CONS passed form `advance-words-count'. See
`count-lines' & `count-words'. When ARG is specified, display
verbosely."
       (let ,(append
              '((start (car cons)))
              '((end (cdr cons)))
              (if regexp
                  `(words-count-regexp-list ,regexp))
              '(list))
         (setq list (advance-words-count start end))
         ,(append `(format ,message) rules)))
     (if ,bind
         (setq words-count-message-function (function ,name)))))

;;TODO Improve this macro tosupport more complicate functions
;; (defmacro words-count-define-func
;;     (name message rules &optional bind verbo opt end)
;;   "foo"
;;   (let* ((string (if verbo
;;                      `(if arg
;;                           ,message
;;                         ,verbo)
;;                    message))
;;          body)
;;     (if opt
;;         (setq body `(concat
;;                      (format
;;                       ,string ,rules)
;;                      ,(words-count--build-cond opt)
;;                      end)
;;               (setq body `(format ,message ,rules)))
;;       `(progn
;;          (defun ,name (list start end &optional arg)
;;            "Format a string to be shown for `words-count--message'.
;; Using the LIST passed form `advance-words-count'. START & END are
;; required to call extra functions, see `count-lines' &
;; `count-words'. When ARG is specified, display verbosely."
;;            body)
;;          (if ,bind
;;              (setq words-count-message-func ,name))))))

;; (defun words-count--build-cond (opt)
;;   "zz"
;;   (let ((cache opt)
;;         arg
;;         op)
;;     (when cache
;;       (progn
;;         (setq arg (pop cache))
;;         (list 'if (car arg) 'arg (cadr arg))))))

(autoload 'pos-tip-show "pos-tip")

(defun words-count--message (cons &optional arg)
  "Display the word count message.
Using tool specified in `words-count-message-display'. The CONS
will be passed to `words-count--format-message'. See
`words-count-message-function'. If ARG is not ture, display in the
minibuffer."
  (let ((opt words-count-message-display)
        (string (funcall words-count-message-function cons arg))
        (time nil))
    (if (null arg)
        (message string)
      (cond
       ((eq opt 'pos-tip)
        ;; Use `run-at-time' to avoid the problem caued by `flycheck-pos-tip'.
        (run-at-time 0.1 nil #'pos-tip-show string nil nil nil -1))
       ((eq 'minibuffer opt)
        (message string))))))

;;;###autoload
(defun advance-words-count (beg end &optional arg)
  "Chinese user preferred word count.
If BEG = END, count the whole buffer. If called initeractively,
use minibuffer to display the messages. The optional ARG will be
passed to `words-count--format-message' to decide the style of
display.

See also `special-words-count'."
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end)
                         (or current-prefix-arg nil))
                 (list nil nil (or current-prefix-arg nil))))
  (if (called-interactively-p 'any)
      (let ((min (or beg (point-min)))
            (max (or end (point-max))))
        (words-count--message (cons min max) (if arg t)))
        (mapcar
         (lambda (r) (words-count beg end r))
         words-count-regexp-list)))

(provide 'advance-words-count)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; advance-words-count.el ends here
