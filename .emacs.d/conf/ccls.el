(use-package ccls
  :requires lsp
  :ensure t
  :init
  (require 'ccls)
  (setq ccls-executable "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda ()
           (require 'ccls)
	   (c-set-style "linux")
           (c-set-offset 'innamespace 0)
           (setq-local evil-shift-width 8)
           (lsp))))
