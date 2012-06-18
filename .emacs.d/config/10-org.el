(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c C-l") 'org-store-link)
(define-key global-map (kbd "C-c C-a") 'org-agenda)

(setq org-return-follows-link t)     ; RETでカーソル下のリンクを開く
(setq org-startup-truncated nil)     ; 右端で折り返す
(setq org-log-done 'time)            ; DONE時にタイムスタンプ
(setq org-use-fast-todo-selection t) ; TODO項目の入力補助
(setq org-support-shift-select t)    ; Shiftキー入力補助

(setq org-todo-keywords
 '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
   (sequence "APPT(a)"                           "|" "DONE(x)" "CANCEL(c)")))
