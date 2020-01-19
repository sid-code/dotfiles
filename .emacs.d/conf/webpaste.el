
(use-package webpaste
  :ensure t
  :defer t
  :init
  (setq webpaste-copy-to-clipboard t)
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))
