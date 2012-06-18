;;; markup-preview.el --- Emacs Interface for markup-preview-command

;; Copyright (C) 2012 mori_dev

;; Author: mori_dev <mori.dev.asdf@gmail.com>
;; Version: 1.0
;; Keywords: markup, convenience
;; Prefix: mp:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; This program is dependent on followings:
;; - markup-preview-command (https://github.com/wakaran/markup-preview-command)

;; Put markup-preview.el in your load-path, and add following code.

;; (require 'markup-preview)
;;
;; M-x markup-preview
;;  Or,
;; (global-set-key (kbd "M--") 'markup-preview) ; key bind example

;;; History:

;; Revision 1.0  2012/01/08  mori_dev
;; Initial revision


(defvar mp:preferred-format nil)

(defvar mp:extension-markup-alist
      '(("md" . "Markdown")
        ("mkd" . "Markdown")
        ("mkdn" . "Markdown")
        ("mdown" . "Markdown")
        ("markdown" . "Markdown")
        ("rst" . "rest")
        ("rest" . "rest")
        ("org" . "Org")
        ("textile" . "Textile")
        ("pod" . "Pod")
        ("creole" . "creole")
        ("rdoc" . "rdoc")
        ("mediawiki" . "MediaWiki")))

(defun mp:get-markup (extension)
  (if mp:preferred-format
      mp:preferred-format
    (cdr (assoc extension mp:extension-markup-alist))))

(defun mp:get-target-string ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mp:call-markup-preview-command (temp-file markup)
  (unless (executable-find "markup-preview")
    (error "markup-preview not found"))
  (call-process-shell-command
     (concat "markup-preview " "--filepath " temp-file " --markup " markup " --output browser"))
  (delete-file temp-file))

(defun markup-preview ()
  (interactive)
  (let ((str (mp:get-target-string))
        (temp-file (make-temp-file "markup-preview-region-"))
        (markup (mp:get-markup (file-name-extension (buffer-file-name)))))
    (unless temp-file (error "fail to get temp-file"))
    (unless markup (error "fail to get markup"))
    (with-temp-file temp-file (insert str))
    (mp:call-markup-preview-command temp-file markup)))

(provide 'markup-preview)
