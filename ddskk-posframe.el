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

(require 'skk)

(defgroup ddskk-posframe nil
  "Show Henkan tooltip for `skk' via `posframe'."
  :group 'lisp)


(provide 'ddskk-posframe)
;;; ddskk-posframe.el ends here
