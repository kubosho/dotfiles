;; -------------------------------------------
;; ruby-mode
;; -------------------------------------------

;; インデントはスペース2個
(setq ruby-indent-level 2)

;; タブはやめる
(setq ruby-indent-tabs-mode nil)

;; ruby-end
(require 'ruby-end)

;; electric-pair-mode
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))

;; RSense
(setq rsense-home "/usr/local/lib/rsense")
(add-to-list 'load-path (concat rsense-home "/etc"))

(require 'rsense)

(setq rsense-rurema-home (concat rsense-home "/doc/ruby-refm-1.9.3-dynamic-snapshot"))
(setq rsense-rurema-refe "refe-1_9_3")

(add-hook 'ruby-mode-hook
  '(lambda ()
    ;; .や::を入力直後から補完開始
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant)
    ;; C-c ,で補完出来るようキーを設定
    (define-key ruby-mode-map (kbd "C-c ,") 'ac-complete-rsense)))

;; Rinari
(require 'rinari)

;; rhtml-mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
  (lambda () (rinari-launch)))
