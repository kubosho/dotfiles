;; ------------------------------------------------------------------------
;; @ js2-mode

;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-bounce-indent-flag nil js2-basic-offset 4)

;; ------------------------------------------------------------------------
;; @ js3-mode

(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist'("\\.js$". js3-mode))

(setq js3-lazy-commas t)
(setq js3-lazy-operators t)
(setq js3-lazy-dots t)
(setq js3-expr-indent-offset 2)
(setq js3-paren-indent-offset 2)
(setq js3-square-indent-offset 2)
(setq js3-curly-indent-offset 2)
(setq js3-auto-indent-p t)
(setq js3-enter-indents-newline t)
(setq js3-indent-on-enter-key t)
(setq js3-cleanup-whitespace nil)
