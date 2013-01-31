(require 'yasnippet)

(setq yas-snippet-dirs
  '("~/.emacs.d/el-get/yasnippet/snippets"))
(yas-global-mode 1)

(custom-set-variables '(yas-trigger-key "TAB"))

;; anything interface
(eval-after-load "anything-config"
  '(progn
    (defun my-yas/prompt (prompt choices &optional display-fn)
      (let* ((names (loop for choice in choices
        collect (or (and display-fn (funcall display-fn choice))
          choice)))
        (selected (anything-other-buffer
          `(((name . ,(format "%s" prompt))
            (candidates . names)
            (action . (("Insert snippet" . (lambda (arg) arg))))))
              "*anything yas/prompt*")))
        (if selected
          (let ((n (position selected names :test 'equal)))
            (nth n choices))
        (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))))
