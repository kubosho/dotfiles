;; -------------------------------------------
;; anything
;; 統合インターフェース
;; -------------------------------------------

(require 'anything-startup)
(define-key global-map (kbd "C-x b") 'anything-filelist+)

;; -------------------------------------------
;; auto-save-buffers-enhanced
;; バッファを自動保存
;; -------------------------------------------

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-interval 1)

;; -------------------------------------------
;; smart-compile
;; 編集中のファイルをその場でコンパイル/実行
;; -------------------------------------------

(require 'smart-compile)
  ;; ruby-mode
  ;; (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
  ;; (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))

;; -------------------------------------------
;; el-get
;; Elispパッケージ管理システム
;; -------------------------------------------

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
    (url-retrieve-synchronously
      "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; レシピ置き場
(add-to-list 'el-get-recipe-path
  (concat (file-name-directory load-file-name) "/el-get/recipes"))

;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;; 取得するパッケージ
(el-get 'sync
  '(el-get
    anything
    auto-save-buffers-enhanced
    el-init
    markdown-mode
    smart-compile
    wdired
  ))
