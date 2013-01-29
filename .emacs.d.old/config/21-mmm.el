; mmm-mode in php
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

(setq mmm-submode-decoration-level 2)
(setq mmm-font-lock-available-p t)

(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)

(mmm-add-classes
'((html-php
:submode php-mode
:front "<\\?\\(php\\)?"
:back "\\?>")))

(add-to-list 'auto-mode-alist '("\\.php?\\'" . nxml-mode))
