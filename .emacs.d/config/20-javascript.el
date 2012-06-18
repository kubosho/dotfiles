;; ------------------------------------------------------------------------
;; @ js2-mode

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-flag nil js2-basic-offset 4)

;; (add-hook 'js2-mode-hook
;;           (lambda ()
;; (define-key js2-mode-map (kbd "M-n") 'js2-next-error)
;; (define-key js2-mode-map (kbd "M-p") 'previous-error)))
