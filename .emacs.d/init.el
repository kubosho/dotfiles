;; ------------------------------------------------------------------------
;; @ load-path

(setq load-path (append
                 '("~/.emacs.d")
                 load-path))

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; ------------------------------------------------------------------------
;; @ path

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; ------------------------------------------------------------------------
;; @ init-loader

(require 'init-loader)
(init-loader-load "~/.emacs.d/config")

;; ------------------------------------------------------------------------
;; @ key bind

;; command to control
(setq mac-command-modifier 'control)

(define-key global-map (kbd "C-c a") 'align)
(define-key global-map (kbd "C-c f") 'ns-toggle-fullscreen)
(define-key global-map (kbd "C-c i") 'indent-region)
;; (define-key global-map (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(define-key global-map (kbd "C-h")   'delete-backward-char)
;; C-hでリージョンを一括削除
(delete-selection-mode 1)
(define-key global-map (kbd "C-o") 'toggle-input-method)
(define-key global-map (kbd "C-u") 'scroll-down)
(define-key global-map (kbd "C-t") 'next-multiframe-window)
(define-key global-map (kbd "C-T") 'previous-multiframe-window)
(define-key global-map (kbd "M-g") 'goto-line)

;; dired
(define-key dired-mode-map (kbd "C-t") 'next-multiframe-window)
(define-key dired-mode-map (kbd "C-T") 'previous-multiframe-window)

;; isearch
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; ------------------------------------------------------------------------
;; @ locale

(set-locale-environment nil)
(prefer-coding-system 'utf-8-unix)

;; ------------------------------------------------------------------------
;; @ IME

;; インラインパッチ適応されているかどうか
(defvar is_inline-patch (eq (boundp 'mac-input-method-parameters) t))

;; インラインパッチ適応の場合は次の設定にする
(when is_inline-patch
  (setq default-input-method "MacOSX")
  (mac-set-input-method-parameter
   "com.google.inputmethod.Japanese.base" 'cursor-color "red")
  (mac-set-input-method-parameter
   "com.google.inputmethod.Japanese.base" `title "漢")
  )

;; ------------------------------------------------------------------------
;; @ Disable

(custom-set-variables
 '(tool-bar-mode nil)    ; hide tool bar
 '(scroll-bar-mode nil)  ; hide scroll bar
 '(menu-bar-mode nil))

(if (eq window-system 'ns)
(add-hook 'window-setup-hook 'menu-bar-mode t))

;; 起動時のメッセージ
(setq inhibit-startup-message t)

;; scratchのメッセージ
(setq initial-scratch-message "")

;; ------------------------------------------------------------------------
;; @ window

;; フルスクリーン
(if (eq window-system 'ns)
(add-hook 'window-setup-hook 'ns-toggle-fullscreen))

;; 行の折り返し
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; ------------------------------------------------------------------------
;; @ color

;; 予約語色分け
(global-font-lock-mode t)

;; Transient color
(setq transient-mark-mode t)

;; 括弧の色
(defvar paren-face 'paren-face)
  (make-face 'paren-face)
  (set-face-foreground 'paren-face "#88aaff")

  (defvar brace-face 'brace-face)
  (make-face 'brace-face)
  (set-face-foreground 'brace-face "#ffaa88")

  (defvar bracket-face 'bracket-face)
  (make-face 'bracket-face)
  (set-face-foreground 'bracket-face "#aaaa00")

  (setq lisp-font-lock-keywords-2
        (append '(("(\\|)" . paren-face))
                lisp-font-lock-keywords-2))

  (add-hook 'scheme-mode-hook
            '(lambda ()
               (setq scheme-font-lock-keywords-2
                     (append '(("(\\|)" . paren-face))
                             scheme-font-lock-keywords-2))))

;; ------------------------------------------------------------------------
;; @ alpha

(set-frame-parameter (selected-frame) 'alpha '(90 80))

;; ------------------------------------------------------------------------
;; @ font

(set-face-attribute 'default nil
                  :family "DejaVu Sans Mono"
                  :height 120)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("ricty" . "iso10646-1"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("ricty" . "iso10646-1"))

;; ------------------------------------------------------------------------
;; @ backup

;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;; 変更ファイルの番号つきバックアップ
(setq version-control nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ先
;; (setq backup-directory-alist (cons "." "~/.emacs.d/backups/"))
;; (setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; 編集中ファイルのバックアップ間隔(秒)
(setq auto-save-timeout 30)

;; 編集中ファイルのバックアップ間隔(打鍵)
(setq auto-save-interval 100)

;; バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)

;; 古いバックアップファイルの削除
(setq delete-old-versions t)

;; ------------------------------------------------------------------------
;; @ history

;; 履歴数を大きくする
(setq history-length 10000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)

;; ------------------------------------------------------------------------
;; @ coding

;; コメントスタイル
(setq comment-style 'multi-line)

;; インデント
(setq-default    c-basic-offset 4      ;; インデント量
                 tab-width 8           ;; タブでインデントされていた場合の幅
                 indent-tabs-mode nil) ;; インデントにスペースを使う

;; C-kで行全体を消去
(setq kill-whole-line t)

;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;; ------------------------------------------------------------------------
;; @ cursor

(blink-cursor-mode 0)

;; 現在行を目立たせる
(global-hl-line-mode)

;; ------------------------------------------------------------------------
;; @ scroll

;; 1行ずつスクロール
(setq scroll-conservatively 1)

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up-around)
  (interactive)
  (let* ( (start_num (+ 1 (count-lines (point-min) (point))) ) )
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

;; ------------------------------------------------------------------------
;; @ trash

(setq delete-by-moving-to-trash t)

;; ------------------------------------------------------------------------
;; @ dictionary

(defun dictionary ()
  "dictionary.app"
  (interactive)

  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end)

    (if (and mark-active
             (<= (region-beginning) pt) (<= pt (region-end)) )
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (setq end (progn (forward-word) (point)))
        (setq beg (progn (backward-word) (point)))
        ))

    (browse-url
     (concat "dict:///"
             (url-hexify-string (buffer-substring-no-properties beg end))))))

(define-key global-map (kbd "C-c w") 'dictionary)

;; ------------------------------------------------------------------------
;; @ etc

;; face名調査
(defun describe-face-at-point ()
 (interactive)
 (message "%s" (get-char-property (point) 'face)))

;; #!が先頭にあるファイルは保存するときに実行権をつける
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; yes or no, y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; エラー時に音と画面のフラッシュを起こさせないようにする
(setq ring-bell-function 'ignore)
