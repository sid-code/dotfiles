(use-package lsp-ui
  :ensure t
  :defer t
  :requires lsp-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
