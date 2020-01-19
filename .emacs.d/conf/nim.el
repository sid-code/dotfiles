
(use-package nim-mode
  :ensure t
  :defer t
  :init
  (setq nimsuggest-path "/home/sid/.nimble/bin/nimsuggest")
  :hook ((nim-mode . (lambda () (setq evil-shift-width 2)))
         (nim-mode . nimsuggest-mode)
         (nimsuggest-mode . company-mode)
         (nimsuggest-mode . flycheck-nimsuggest-setup)))

(use-package flycheck-nimsuggest
  :ensure t
  :defer t)
