;; ------------------------------------------------------------------------
;; @ css

(defun my-css-electric-pair-brace ()
  (interactive)
  (insert "{")(newline-and-indent)
  (newline-and-indent)
  (insert "}")
  (indent-for-tab-command)
  (previous-line)(indent-for-tab-command))

(defun my-semicolon-ret ()
  (interactive)
  (insert ";")
  (newline-and-indent))

;; ------------------------------------------------------------------------
;; @ scss-mode

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.\\(scss\\|css\\)\\'" . scss-mode))
(add-hook 'scss-mode-hook 'ac-css-mode-setup)
(setq scss-compile-at-save nil) ;; SCSSのコンパイルを無効
(add-hook 'scss-mode-hook
          (lambda ()
            ;; (flymake-mode t) ;; flymake
            (define-key scss-mode-map "\M-{" 'my-css-electric-pair-brace)
            (setq comment-start "// ")
            (setq comment-end "")))

;; ------------------------------------------------------------------------
;; @ auto-complete settings

(eval-when-compile
  (require 'cl)
  (require 'auto-complete-config))
(defvar ac-source-css-property-names
  '((candidates . (loop for property in ac-css-property-alist
                        collect (car property)))))
(defun my-css-mode-hook ()
  (add-to-list 'ac-sources 'ac-source-css-property)
  (add-to-list 'ac-sources 'ac-source-css-property-names))
(add-hook 'css-mode-hook 'my-css-mode-hook)
