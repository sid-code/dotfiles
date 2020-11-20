(use-package caml
  :init
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode)))

(use-package lsp-ocaml)
