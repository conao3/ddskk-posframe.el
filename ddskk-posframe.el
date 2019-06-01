;;; ddskk-posframe.el --- Show Henkan tooltip for ddskk via posframe       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/conao3/ddskk-posframe.el
;; Package-Requires: ((emacs "26.1") (ddskk "16.2.50"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ddskk-posframe.el provide Henkan tooltip for ddskk via posframe.
;;
;; More information is [[https://github.com/conao3/ddskk-posframe.el][here]]

;;; Code:

(require 'posframe)
(require 'skk)

(defgroup ddskk-posframe nil
  "Show Henkan tooltip for `skk' via `posframe'."
  :group 'lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Custom variables
;;

(defcustom ddskk-posframe-style 'window-bottom-left
  "The style of ddskk-posframe."
  :group 'ddskk-posframe
  :type 'string)

(defcustom ddskk-posframe-font nil
  "The font used by ddskk-posframe.
When nil, Using current frame's font as fallback."
  :group 'ddskk-posframe
  :type 'string)

(defcustom ddskk-posframe-width nil
  "The width of ddskk-posframe."
  :group 'ddskk-posframe
  :type 'number)

(defcustom ddskk-posframe-height nil
  "The height of ddskk-posframe."
  :group 'ddskk-posframe
  :type 'number)

(defcustom ddskk-posframe-min-width nil
  "The width of ivy-min-posframe."
  :group 'ddskk-posframe
  :type 'number)

(defcustom ddskk-posframe-min-height nil
  "The height of ivy-min-posframe."
  :group 'ddskk-posframe
  :type 'number)

(defcustom ddskk-posframe-border-width 1
  "The border width used by ddskk-posframe.
When 0, no border is showed."
  :group 'ddskk-posframe
  :type 'number)

(defcustom ddskk-posframe-parameters nil
  "The frame parameters used by ddskk-posframe."
  :group 'ddskk-posframe
  :type 'string)

(defface ddskk-posframe
  '((t (:inherit default)))
  "Face used by the ddskk-posframe."
  :group 'ddskk-posframe)

(defface ddskk-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the ddskk-posframe's border."
  :group 'ddskk-posframe)

(defvar ddskk-posframe-buffer " *ddskk-posframe-buffer*"
  "The posframe-buffer used by ddskk-posframe.")

(defvar ddskk-posframe--display-p nil
  "The status of `ddskk-posframe--display'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Functions
;;

(defun ddskk-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe."
  (if (not (posframe-workable-p))
      (warn "ddskk-posframe is busy now!")
    (setq ddskk-posframe--display-p t)
    (posframe-show
     ddskk-posframe-buffer
     :font ddskk-posframe-font
     :string str
     :position (point)
     :poshandler poshandler
     :background-color (face-attribute 'ddskk-posframe :background nil t)
     :foreground-color (face-attribute 'ddskk-posframe :foreground nil t)
     :height ddskk-posframe-height
     :width ddskk-posframe-width
     :min-height (or ddskk-posframe-min-height (+ skk-henkan-number-to-display-candidates 1))
     :min-width (or ddskk-posframe-min-width (round (* (frame-width) 0.62)))
     :internal-border-width ddskk-posframe-border-width
     :internal-border-color (face-attribute 'ddskk-posframe-border :background nil t)
     :override-parameters ddskk-posframe-parameters)))

(defun ddskk-posframe-cleanup ()
  "Cleanup ddskk-posframe."
  (when (ddskk-posframe-workable-p)
    (posframe-hide ddskk-posframe-buffer)
    (setq ddskk-posframe--display-p nil)))

(defun ddskk-posframe-workable-p ()
  "Test ddskk-posframe workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                (not (display-graphic-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Advices
;;

(defvar ddskk-posframe-advice-alist
  '((skk-henkan-in-minibuff . ddskk-posframe--skk-henkan-in-minibuff)))

(defun ddskk-posframe--skk-henkan-in-minibuff (fn &rest args)
  "Around advice for `skk-henkan-in-minibuff'."
  (with-current-buffer ddskk-posframe-buffer
    (erase-buffer)
    (insert "↓辞書登録中↓"))
  (funcall fn args))

;;;###autoload
(define-minor-mode ddskk-posframe-mode
  "Enable ddskk-posframe-mode."
  :init-value nil
  :global t
  :lighter " SKK-pf"
  :group 'ddskk-posframe
  (if ddskk-posframe-mode
      (progn
        (mapc (lambda (elm)
                (advice-add (car elm) :around (cdr elm)))
              ddskk-posframe-advice-alist)
        (setq-default skk-show-tooltip t)
        (setq-default skk-tooltip-function ddskk-posframe-display))
    (mapc (lambda (elm)
                (advice-remove (car elm) (cdr elm)))
              ddskk-posframe-advice-alist)
    (setq-default skk-show-tooltip nil)
    (setq-default skk-tooltip-function ddskk-posframe-display)))

(provide 'ddskk-posframe)
;;; ddskk-posframe.el ends here
