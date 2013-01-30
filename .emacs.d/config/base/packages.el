;; -------------------------------------------
;; anything
;; 統合インターフェース
;; -------------------------------------------

(require 'anything-startup)
(define-key global-map (kbd "C-x b") 'anything-filelist+)

;; -------------------------------------------
;; auto-complete
;; 入力補完
;; -------------------------------------------

(when (require 'auto-complete-config nil t)
  (ac-config-default)

  (setq ac-auto-start 1)
  (setq ac-dwim t)

  ;; C-n/C-pで候補を選択可能にする
  (setq ac-use-menu-map t)

  (setq ac-modes
      '(html-mode
        nxml-mode
        css-mode
        scss-mode
        javascript-mode
        js2-mode
        js3-mode
        ))

  ;; 常にYASnippetを補完候補に
  ; (add-to-list 'ac-sources 'ac-source-yasnippet)

  ;; 辞書ファイルのディレクトリ
  (setq ac-dictionary-directories "~/.emacs.d/ac-dict")

  ;; 補完履歴のキャッシュ先
  (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")
)

;; -------------------------------------------
;; auto-save-buffers-enhanced
;; バッファを自動保存
;; -------------------------------------------

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-interval 1)

;; -------------------------------------------
;; flycheck
;; flymakeの各言語用設定まとめパッケージ
;; -------------------------------------------

;; Ruby
(add-hook 'ruby-mode-hook 'flycheck-mode)

;; -------------------------------------------
;; popwin
;; ヘルプバッファや補完バッファをポップアップで表示
;; -------------------------------------------

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; -------------------------------------------
;; smart-compile
;; 編集中のファイルをその場でコンパイル/実行
;; -------------------------------------------

(require 'smart-compile)
  ;; ruby-mode
  ;; (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
  ;; (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))

;; -------------------------------------------
;; wdired
;; ディレクトリの名前を編集する
;; -------------------------------------------

(require 'wdired)
(setq wdired-allow-to-change-permissions t)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
