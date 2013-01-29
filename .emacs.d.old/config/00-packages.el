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
;; @ color-theme

(require 'color-theme)
;; (require 'color-theme-solarized)

(color-theme-initialize)
(color-theme-molokai)
;; (color-theme-solarized-dark)
;; (color-theme-tangotango)
;; (color-theme-arjen)

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
;; @ grep

;; grep-a-lot
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; grep-edit
(require 'grep)
(require 'grep-edit)

(defadvice grep-edit-change-file (around inhibit-read-only activate)
  ""
  (let ((inhibit-read-only t))
    ad-do-it))
;; (progn (ad-disable-advice 'grep-edit-change-file 'around 'inhibit-read-only)(ad-update 'grep-edit-change-file))

(defun my-grep-edit-setup ()
  (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)

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
;; @ recentf-ext

(require 'recentf-ext)
(setq recentf-max-saved-items 3000)

;; ------------------------------------------------------------------------
;; @ smartchr
;; (auto-install-from-url "https://raw.github.com/imakado/emacs-smartchr/master/smartchr.el")

(require 'smartchr)
(define-key global-map (kbd "(")  (smartchr '("(`!!')" "(")))
(define-key global-map (kbd "{")  (smartchr '("{`!!'}" "{")))
(define-key global-map (kbd "[")  (smartchr '("[`!!']" "[")))
(define-key global-map (kbd "'")  (smartchr '("'`!!''" "'")))
(define-key global-map (kbd "\"") (smartchr '("\"`!!'\"" "\"")))

;; ------------------------------------------------------------------------
;; @ popwin

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
