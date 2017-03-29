;;; advance-words-count.el --- A Package Provides Extended `count-words'
;; 
;; Filename: advance-words-count.el
;; Description: Provides Extended `count-words' function.
;; Author: LdBeth
;; Maintainer: LdBeth
;; Created: Wed Mar 29 14:42:25 2017 (+0800)
;; Version: 0.8.0
;; Package-Requires: ()
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
(defgroup andvance-words-count nil
  "Extended `count-words' function."
  :prefix "words-count"
  :group 'text)

(defcustom words-count-rule-chinese "\\cc"
  "A regexp string to match Chinese characters."
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
  (list words-count-rule-chinese
        words-count-rule-nonespace
        words-count-rule-ansci)
  "A list for the regexp used in `advance-words-count'."
  :group 'advance-words-count
  :type '(repeat regexp))

(defcustom words-count-message-extra nil
  "The function used to format message in `advance-words-count'."
  :group 'advance-words-count
  :type '(repeat function))

(defun special--words-count (start end regexp)
  "Count the word from START to END with REGEXP."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (goto-char start)
        (while (and (< (point) end) (re-search-forward regexp end t))
          (setq count (1+ count)))))
    count))

(defun message--words-count (list start end &optional arg)
  "Display the word count message.
Using the LIST passed form `advance-words-count'. START & END are
required to call extra functions, see `count-lines' &
`count-words'. When ARG is specified, display a verbose buffer."
  (message
   (if arg
       "
-----------~*~ Words Count ~*~----------

 Word Count .................... %d
 Characters (without Space) .... %d
 Characters (all) .............. %d
 Number of Lines ............... %d
 ANSCII Chars .................. %d
%s
========================================
"
     "Wc:%d,Ns:%d,Al:%d,Ln:%d,An:%d,%s")
   (+ (car list) (car (last list)))
   (cadr list)
   (- end start)
   (count-lines start end)
   (car (last list))
   (concat
    (unless (= 0 (car list))
      (format (if arg
                  " Chinese Chars ................. %d\n"
                "Zh:%d,")
              (car list)))
    (format (if arg
                " English Words ................. %d\n"
              "En:%d")
            (count-words start end))
    (if words-count-message-extra
        (eval words-count-message-extra)))))

;;;###autoload
(defun advance-words-count (beg end &optional arg)
  "Chinese user preferred word count.
If BEG = END, count the whole buffer. If called initeractively,
use minibuffer to display the messages. The optional ARG will be
passed to `message--words-count'.

See also `special-words-count'."
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end)
                         (or current-prefix-arg nil))
                 (list nil nil (or current-prefix-arg nil))))
  (let ((min (or beg (point-min)))
        (max (or end (point-max)))
        list)
    (setq list
          (mapcar
           (lambda (r) (special--words-count min max r))
           words-count-regexp-list))
    (if (called-interactively-p 'any)
        (message--words-count list min max arg)
      list)))

(provide 'advance-words-count)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; advance-words-count.el ends here
