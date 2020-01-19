
(use-package go-mode
  :ensure t
  :defer t
  :hook ((before-save . gofmt-before-save)))
