;;; advance-wc-mode.el --- Advance useage of `wc-mode'
;;
;; Filename: advance-wc-mode.el
;; Description:
;; Author: LdBeth
;; Maintainer:
;; Created: Fri Mar 31 23:00:08 2017 (+0800)
;; Version:
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

;;;###autoload
(defmacro words-count-wc-setup (mode &rest body)
  "Define a function to be called to set up `wc-mode'.
Hook `wc-mode' to MODE and define corresponding functions.
If BODY is specified, do BODY when enters MODE."
  (declare (indent 1))
  (let* ((name (intern (format "words-count-init-%S" mode)))
         (hook (intern (format "%s-hook" mode))))
    (progn
      `(add-hook (quote ,hook) 'wc-mode)
      (if body
          `(defun ,name ()
             "set up `wc-mode' when called."
             ,(append '(progn) body))
        `(add-hook (quote ,hook) ,name)))))

(provide 'advance-wc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; advance-wc-mode.el ends here
