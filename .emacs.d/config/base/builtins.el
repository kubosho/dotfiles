;; -------------------------------------------
;; cua-mode
;; 矩形選択を提供するモード
;; -------------------------------------------

(cua-mode t)
(setq cua-enable-cua-keys nil)

;; -------------------------------------------
;; flyspell
;; スペルチェック
;; -------------------------------------------

(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; -------------------------------------------
;; generic-x
;; hostsやhttpd.confを見やすくする
;; -------------------------------------------

(require 'generic-x)

;; -------------------------------------------
;; recentf-mode
;; 最近使ったファイルを表示する
;; -------------------------------------------

(require 'cl)

(defvar my-recentf-list-prev nil)

(defadvice recentf-save-list
  (around no-message activate)
  "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
  (unless (equal recentf-list my-recentf-list-prev)
    (flet ((message (format-string &rest args)
      (eval `(format ,format-string ,@args)))
      (write-file (file &optional confirm)
        (let ((str (buffer-string)))
        (with-temp-file file
        (insert str)))))
      ad-do-it
      (setq my-recentf-list-prev recentf-list))))

(defadvice recentf-cleanup
  (around no-message activate)
  "suppress the output from `message' to minibuffer"
  (flet ((message (format-string &rest args)
    (eval `(format ,format-string ,@args))))
    ad-do-it))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(run-with-idle-timer 30 t 'recentf-save-list)
(recentf-mode 1)

;; -------------------------------------------
;; saveplace
;; 前回の編集場所を記録する
;; -------------------------------------------

(require 'saveplace)
(setq-default save-place t)

;; -------------------------------------------
;; uniquify
;; 同名ファイルがあった時に識別しやすくする
;; -------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; -------------------------------------------
;; whitespace
;; 行末空白などの可視化
;; -------------------------------------------

(require 'whitespace)
(global-whitespace-mode 1)

;; 1行が80桁を超えたら長すぎると判断する。
(setq whitespace-line-column 80)
(setq whitespace-style '(face              ;; faceを使って視覚化する。
                         trailing          ;; 行末空白
                         lines-tail        ;; whitespace-line-column
                         space-before-tab  ;; タブの前にあるスペースを対象とする。
                         space-after-tab)) ;; タブの後にあるスペースを対象とする。
