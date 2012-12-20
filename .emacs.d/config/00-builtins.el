;; ------------------------------------------------------------------------
;; @ cua

(cua-mode t)
(setq cua-enable-cua-keys nil)

;; ------------------------------------------------------------------------
;; @ complete

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 補完可能なものを随時表示
(icomplete-mode 1)

;; ------------------------------------------------------------------------
;; @ generic-x

(require 'generic-x)

;; ------------------------------------------------------------------------
;; @ saveplace

(require 'saveplace)
(setq-default save-place t)

;; ------------------------------------------------------------------------
;; @ server

(require 'server)
(unless (server-running-p)
  (server-start))

;; ------------------------------------------------------------------------
;; @ white space

(require 'whitespace)
(global-whitespace-mode 1)

;; 1行が80桁を超えたら長すぎると判断する。
(setq whitespace-line-column 80)
(setq whitespace-style '(face              ;; faceを使って視覚化する。
                         trailing          ;; 行末空白
                         lines-tail        ;; whitespace-line-column
                         space-before-tab  ;; タブの前にあるスペースを対象とする。
                         space-after-tab)) ;; タブの後にあるスペースを対象とする。

;; ------------------------------------------------------------------------
;; @ parent

;; 対応する括弧を光らせる
(show-paren-mode 1)

;: 対応する括弧を強調表示
(show-paren-mode t)

;; 対応しない括弧を探す
(setq show-paren-ring-bell-on-mismatch t)

;; ------------------------------------------------------------------------
;; @ uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
