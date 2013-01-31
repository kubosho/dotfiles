;; -------------------------------------------
;; emacs-lisp-mode
;; -------------------------------------------

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)))
