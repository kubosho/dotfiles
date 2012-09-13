;; ------------------------------------------------------------------------
;; @ auto-complete

(when (require 'auto-complete-config nil t)
  (ac-config-default)

  (setq ac-auto-start 1)
  (setq ac-dwim t)
  (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能

  (setq ac-modes
      '(html-mode
        nxml-mode
        css-mode
        scss-mode
        javascript-mode
        js2-mode
        js3-mode
        ))

  (add-to-list 'ac-sources 'ac-source-yasnippet) ;; 常にYASnippetを補完候補に
  (setq ac-dictionary-directories "~/.emacs.d/ac-dict") ;; 辞書ファイルのディレクトリ
  (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat") ;; 補完履歴のキャッシュ先
)
