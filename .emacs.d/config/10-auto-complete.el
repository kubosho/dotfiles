;; ------------------------------------------------------------------------
;; @ auto-complete

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
(ac-config-default)
(setq ac-delay 0.1)
(setq ac-use-menu-map t)
(set-default 'ac-sources '(ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-words-in-buffer))

;; ruby-modeにてendで補完させない設定
(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))

(setq ac-modes
      '(css-mode
        js2-mode
        nxml-mode
        html-mode
        scss-mode
        emacs-lisp-mode
        ))

;; key bind
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
