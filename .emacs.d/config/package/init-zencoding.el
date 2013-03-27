;; zencoding-mode
(when (require 'zencoding-mode nil t)
  (setq zencoding-block-tags
        (append (list
                 "article"
                 "section"
                 "aside"
                 "nav"
                 "figure"
                 "address"
                 "header"
                 "footer")
                zencoding-block-tags))
  (setq zencoding-inline-tags
        (append (list
                 "textarea"
                 "small"
                 "time" "del" "ins"
                 "sub"
                 "sup"
                 "i" "s" "b"
                 "ruby" "rt" "rp"
                 "bdo"
                 "iframe" "canvas"
                 "audio" "video"
                 "ovject" "embed"
                 "map"
                 )
                zencoding-inline-tags))
  (setq zencoding-self-closing-tags
        (append (list
                 "wbr"
                 "object"
                 "source"
                 "area"
                 "param"
                 "option"
                 )
                zencoding-self-closing-tags))
  (add-hook 'html-mode-hook 'zencoding-mode)
  (add-hook 'php-mode-hook 'zencoding-mode)
  (add-hook 'rhtml-mode-hook 'zencoding-mode)

  ;; yasnippetを使わない場合
  ;; (define-key zencoding-mode-keymap (kbd "C-,") 'zencoding-expand-line)

  ;; yasnippetと連携する場合
  (define-key zencoding-mode-keymap (kbd "C-,") 'zencoding-expand-yas))
