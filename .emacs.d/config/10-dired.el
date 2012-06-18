;; ------------------------------------------------------------------------
;; @ dired

;; wdired
(require 'wdired)
(setq wdired-allow-to-change-permissions t)

(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;; diredを便利にする
(require 'dired-x)

;; スペースでマークする(FD like)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."

  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    (dired-previous-line 1)))

(define-key dired-mode-map " " 'dired-toggle-mark)
