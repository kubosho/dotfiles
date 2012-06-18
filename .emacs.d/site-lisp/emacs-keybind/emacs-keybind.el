;;; emacs-keybind.el

;;; Commentary:
;; $BA4$F$N%3%^%s%I<B9TMzNr$r%U%!%$%k$KJ]B8$9$k(B

;;; $B@_DjMQJQ?t(B
(defvar emacs-keybind-program-file
  "~/.emacs.d/emacs_keybind.rb"
  "Place of emacs_keybind.rb")

(defvar emacs-keybind-keyboard-kind
  "ascii"
  "ascii or japanese")

(defvar emacs-keybind-work-dir
  "~/.emacs.d"
  "")

(defvar emacs-keybind-history-save-file-name
  "emacs-keybind-history-save-file"
  "Name of file where history information is stored.")

(defvar emacs-keybind-show-file-name
  "emacs-keybind.html"
  "")

(defvar emacs-keybind-show-with-history-file-name
  "emacs-keybind-with-history.html"
  "")

;; $B%f!<%F%#%j%F%#4X?t(B
(defun emacs-keybind-make-path (fname)
  (concat emacs-keybind-work-dir "/" fname))

;;; $B%R%9%H%j$NDI2C$HJ]B8(B
(setq emacs-keybind-history-work nil)

(defun emacs-keybind-history-save ()
  "Save history information to file given by `emacs-keybind-history-save-file-name'."
  (interactive)
  (with-temp-buffer
    (insert ";; " (format-time-string "%Y/%m/%d %H:%M" (current-time)) "\n")
    (prin1 emacs-keybind-history-work (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) (emacs-keybind-make-path emacs-keybind-history-save-file-name) t)))

(defun emacs-keybind-history-add (&optional arg)
  "The command history is added to `emacs-keybind-history-work'."
  (setq emacs-keybind-history-work (append (list this-command) emacs-keybind-history-work)))

(add-hook 'kill-emacs-hook 'emacs-keybind-history-save)
(add-hook 'pre-command-hook 'emacs-keybind-history-add)

;;; $B%-!<%P%$%s%I%l%]!<%H$NI=<((B
(defun emacs-keybind-show ()
  "The key bind report is displayed."
  (interactive)
  (describe-bindings)
  (switch-to-buffer "*Help*")
  (call-process-region 
   (point-min) (point-max)
   emacs-keybind-program-file
   nil
   (generate-new-buffer "*emacs-report*")
   nil
   "-k" emacs-keybind-keyboard-kind
   "-f" "html")
  (switch-to-buffer "*emacs-report*")
  (let ((file-name (emacs-keybind-make-path emacs-keybind-show-file-name)))
    (write-file file-name)
    (browse-url-of-file file-name)
    (kill-buffer (current-buffer))))

(defun emacs-keybind-show-with-history ()
  "The key bind report is displayed including history information."
  (interactive)
  (describe-bindings)
  (switch-to-buffer "*Help*")
  (call-process-region 
   (point-min) (point-max)
   emacs-keybind-program-file
   nil
   (generate-new-buffer "*emacs-report*")
   nil
   "-k" emacs-keybind-keyboard-kind
   "-h" (emacs-keybind-make-path emacs-keybind-history-save-file-name)
   "-f" "html")
  (switch-to-buffer "*emacs-report*")
  (let ((file-name (emacs-keybind-make-path emacs-keybind-show-with-history-file-name)))
    (write-file file-name)
    (browse-url-of-file file-name)
    (kill-buffer (current-buffer))))

(provide 'emacs-keybind)
