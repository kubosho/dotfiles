;; ------------------------------------------------------------------------
;; @ anything

(require 'anything-startup)
(define-key global-map (kbd "C-x b") 'anything-filelist+)

;; ------------------------------------------------------------------------
;; @ auto-async-byte-compile

(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; ------------------------------------------------------------------------
;; @ auto-install

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/")
(auto-install-compatibility-setup)
;; (auto-install-update-emacswiki-package-name t)

;; ------------------------------------------------------------------------
;; @ auto-save-buffers

(require 'auto-save-buffers)
(run-with-idle-timer 0.1 t 'auto-save-buffers)

;; ------------------------------------------------------------------------
;; @ color-theme

(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)
(color-theme-molocai)
;; (color-theme-solarized-dark)

;; (color-theme-tangotango)
;; (color-theme-arjen)
;; (color-theme-molokai)

;; ------------------------------------------------------------------------
;; @ c/migemo

(require 'migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)
(set-process-query-on-exit-flag migemo-process nil)

;; ------------------------------------------------------------------------
;; @ edit-server

(if (and (daemonp) (locate-library "edit-server"))
     (progn
       (require 'edit-server)
       (edit-server-start)))
(setq edit-server-new-frame nil)

;; ------------------------------------------------------------------------
;; @ flyspell

(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; ------------------------------------------------------------------------
;; @ grep

;; grep-a-lot
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; grep-edit
(require 'grep-edit)

;; ------------------------------------------------------------------------
;; @ highlight indentation

(require 'highlight-indentation)
(add-hook 'nxml-mode-hook 'highlight-indentation-mode)
(add-hook 'html-mode-hook 'highlight-indentation-mode)
(add-hook 'scss-mode-hook 'highlight-indentation-mode)
(add-hook 'css-mode-hook  'highlight-indentation-mode)

(require 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'ibuffer)

;; ------------------------------------------------------------------------
;; @ markup-preview

(require 'markup-preview)
;(global-set-key (kbd "M--") 'markup-preview) ; key bind example
;(defalias 'mp 'markup-preview)

;; ------------------------------------------------------------------------
;; @ recentf-ext

(require 'recentf-ext)
(setq recentf-max-saved-items 3000)

;; ------------------------------------------------------------------------
;; @ popwin

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
