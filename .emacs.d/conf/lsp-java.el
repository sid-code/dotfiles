
(use-package lsp-java
  :ensure t
  :defer t
  :config
  (setq lsp-java--workspace-folders (list "TBA"))
  (add-hook 'java-mode-hook (lambda ()
                              (company-mode)))
  :init
  (require 'lsp-java))
