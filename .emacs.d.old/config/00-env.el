;; environment variables
(eval-when-compile (require 'cl))

;; exec-pathにshellから得られる$PATHを追加
(loop for x in (reverse
                (split-string (substring (shell-command-to-string "echo $PATH") 0 -1) ":"))
      do (add-to-list 'exec-path x))

;; process-environmentも変更
(setenv "PATH" (substring (shell-command-to-string "echo $PATH") 0 -1))
