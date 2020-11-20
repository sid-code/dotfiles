
(use-package tuareg
  :ensure t
  :defer t
  :requires caml)

(use-package caml
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode)))

(use-package lsp-ocaml)
