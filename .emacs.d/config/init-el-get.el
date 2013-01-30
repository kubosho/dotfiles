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
    auto-complete
    auto-save-buffers-enhanced
    el-init
    emacs-powerline
    flycheck
    markdown-mode
    smart-compile
    wdired
  ))
