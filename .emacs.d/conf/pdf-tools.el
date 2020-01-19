(use-package pdf-tools
  :ensure t
  :defer t
  :bind (:map pdf-view-mode-map ("j" . pdf-view-next-line-or-next-page)
         :map pdf-view-mode-map ("k" . pdf-view-previous-line-or-previous-page)))
