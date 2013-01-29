;; ------------------------------------------------------------------------
;; @ yasnippet

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/extras/imported")
(yas/global-mode t)

(global-set-key (kbd "C-c s") 'yas/insert-snippet)

(setq yas/prompt-functions '(yas/dropdown-prompt))

(set-face-background  'yas/field-highlight-face nil)
(set-face-underline-p 'yas/field-highlight-face t)

;; anything-c-yasnippetを使わずにyasnippetをanythingインタフェースで選択する
;; http://d.hatena.ne.jp/sugyan/20120111/1326288445
(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  coice)))
         (selected (anything-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*anything yas/prompt*")))
    (if selected
        (let ((n (position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))
(custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
