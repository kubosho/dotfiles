;; ------------------------------------------------------------------------
;; @ nXML-mode

(setq auto-mode-alist
(append '(("\\.\\(html\\|xhtml\\|shtml\\|tpl\\|mt\\|tx\\)\\'" . nxml-mode)) auto-mode-alist))

(load "rng-auto.el" 't)
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq auto-fill-mode -1)
            (setq nxml-slash-auto-complete-flag t)       ;; スラッシュの入力で終了タグを自動補完
            (setq nxml-child-indent 4)                   ;; タグのインデント幅
            (setq nxml-attribute-indent 4)               ;; 属性のインデント幅
            (setq nxml-bind-meta-tab-to-complete-flag t) ;; M-TABでタグ補完
            (setq nxml-slash-auto-complete-flag t)       ;; </の入力で閉じタグを補完する
            (setq nxml-char-ref-display-glyph-flag nil)  ;; グリフは非表示
            (define-key nxml-mode-map (kbd "C-m") 'newline-and-indent) ;; 改行 + インデント
            ))

;; ------------------------------------------------------------------------
;; @ html5-el

(autoload 'whattf-dt "whattf-dt" nil t) ;; autoloadにすることで約1.7秒起動短縮される
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/site-lisp/html5-el/schemas.xml"))

;; ------------------------------------------------------------------------
;; @ zencoding-mode

(require 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)

(define-key zencoding-mode-keymap (kbd "C-z") 'zencoding-expand-line)







