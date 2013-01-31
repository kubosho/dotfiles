;; -------------------------------------------
;; load-path
;; -------------------------------------------

(setq load-path (append '(
  "~/.emacs.d"
) load-path))

(let ((default-directory "~/.emacs.d/el-get"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; -------------------------------------------
;; load(el-get)
;; -------------------------------------------

(load "config/init-el-get")

;; -------------------------------------------
;; load(el-init)
;; -------------------------------------------

(require 'el-init)
(el-init:provide)
(setq el-init:load-directory-list '("base" "builtins" "package" "mode"))

(el-init:load "~/.emacs.d/config")

;; -------------------------------------------
;; Encoding
;; -------------------------------------------

(prefer-coding-system 'utf-8)

;; -------------------------------------------
;; IME
;; -------------------------------------------

;; Mac OS Xのみ適用
(cond
  ((string-match "apple-darwin" system-configuration)
    ;; インラインパッチ適応されているかどうか
    (defvar is_inline-patch (eq (boundp 'mac-input-method-parameters) t))

    ;; インラインパッチ適応の場合は次の設定にする
    (when is_inline-patch
      (setq default-input-method "MacOSX")

      (mac-set-input-method-parameter
        "com.google.inputmethod.Japanese.base" 'cursor-color "red")
      (mac-set-input-method-parameter
        "com.google.inputmethod.Japanese.base" `title "漢"))))

;; -------------------------------------------
;; カーソル
;; -------------------------------------------

;; カーソル点滅させない
(blink-cursor-mode 0)

;; 現在の行を目立たせる
(global-hl-line-mode)

;; -------------------------------------------
;; キーバインド
;; -------------------------------------------

;; Mac OS Xのみ適用
(cond
 ((string-match "apple-darwin" system-configuration)
   ;; フルスクリーン切り替え
   (define-key global-map [s-return] 'ns-toggle-fullscreen)

   ;; 入力切り替え
   (define-key global-map (kbd "C-o") 'toggle-input-method)))

;; 上に戻る
(define-key global-map (kbd "C-u") 'scroll-down)

;; リージョンをC-hで削除
(delete-selection-mode 1)

;; C-hでバックスペース
(define-key global-map (kbd "C-h") 'delete-backward-char)

;; コメントアウト
(define-key global-map (kbd "C-;") 'comment-dwim)

;; ヘルプ
(define-key global-map (kbd "M-?") 'help-for-help)

;; インデント
(define-key global-map (kbd "C-c i") 'indent-region)

;; isearch時にC-hで文字削除
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; -------------------------------------------
;; コメント・インデント
;; -------------------------------------------

;; コメントスタイル
(setq comment-style 'multi-line)

;; インデント
(setq-default  c-basic-offset 2      ;; インデント量
               tab-width 4           ;; タブでインデントされていた場合の幅
               indent-tabs-mode nil) ;; インデントにスペースを使う

;; -------------------------------------------
;; スクロール
;; -------------------------------------------

;; 1行ずつスクロール
(setq scroll-conservatively 1)

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up-around)
  (interactive)
  (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
    (goto-char (point-max))
    (let* ( (end_num (+ 1 (count-lines (point-min) (point))) ) )
      (goto-line start_num )
      (let* ( (limit_num (- (- end_num start_num) (window-height)) ))
        (if (< (- (- end_num start_num) (window-height)) 0)
            (goto-char (point-max))
          ad-do-it)) )) )
(ad-activate 'scroll-up)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down-around)
  (interactive)
  (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
    (if (< start_num (window-height))
        (goto-char (point-min))
      ad-do-it) ))
(ad-activate 'scroll-down)

;; -------------------------------------------
;; フォント
;; -------------------------------------------

(set-face-attribute 'default nil
                  :family "DejaVu Sans Mono"
                  :height 120)
;; 日本語設定
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("ricty" . "iso10646-1"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("ricty" . "iso10646-1"))

;; -------------------------------------------
;; 色
;; -------------------------------------------

;; 予約後などに色つける
(global-font-lock-mode t)
(setq-default transient-mark-mode t)

;; -------------------------------------------
;; 括弧
;; -------------------------------------------

;; 対応する括弧を光らせる
(show-paren-mode 1)

;: 対応する括弧を強調表示
(show-paren-mode t)

;; 対応しない括弧を探す
(setq show-paren-ring-bell-on-mismatch t)

;; -------------------------------------------
;; 透過
;; -------------------------------------------

(set-frame-parameter (selected-frame) 'alpha '(90 0))

;; -------------------------------------------
;; 補完
;; -------------------------------------------

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 補完可能なものを随時表示
(icomplete-mode 1)

;; -------------------------------------------
;; 履歴
;; -------------------------------------------

;; 履歴数を大きくする
(setq history-length 1000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 1000)

;; -------------------------------------------
;; 有効・無効
;; -------------------------------------------

(custom-set-variables
  '(tool-bar-mode nil)    ;; ツールバー無効
  '(scroll-bar-mode nil)  ;; スクロールバー無効
  '(menu-bar-mode nil))   ;; メニューバー無効

;; コマンドラインから起動している場合はメニューバーを有効にする
(if (eq window-system 'ns)
(add-hook 'window-setup-hook 'menu-bar-mode t))

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; scratchのメッセージを無しにする
(setq initial-scratch-message "")

;; ヴィジュアルベル無効
(setq visible-bell nil)

;; ビープ音無効
(setq ring-bell-function '(lambda ()))

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存したファイルを自動的に削除する
(setq delete-auto-save-files t)

;; Mac OS Xのみ適用
(cond
  ((string-match "apple-darwin" system-configuration)
  ;; 関連づけなどからファイルを開く場合に新規ウィンドウを開かない
  (setq ns-pop-up-frames nil)))

;; -------------------------------------------
;; 表示・非表示
;; -------------------------------------------

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)

;; 電池残量表示
(display-battery-mode t)

;; Mac OS Xのみ適用
(cond
  ((string-match "apple-darwin" system-configuration)
;; Emacs起動後にフルスクリーンにする
(add-hook 'window-setup-hook 'ns-toggle-fullscreen)))

;; ファイルサイズ表示
(size-indication-mode t)

;; モードラインの割合表示を総行数表示にする
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
    (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
    (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
      ((eq line-number-mode t)
        (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
          (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
    '(:eval (format my-mode-line-format
      (count-lines (point-max) (point-min))))))

;; 選択範囲の文字数カウント
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))

(add-to-list 'default-mode-line-format
  '(:eval (count-lines-and-chars)))

;; -------------------------------------------
;; その他
;; -------------------------------------------

;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; 「yes or no」を「y or n」にする
(fset 'yes-or-no-p 'y-or-n-p)

;; 縦分割でも行を折り返す
(setq truncate-partial-width-windows nil)

;; 先頭に#!...があるファイルを保存した時に実行権をを追加
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
