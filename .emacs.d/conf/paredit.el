(use-package paredit
  :requires evil-paredit
  :ensure t
  :hook (paredit-mode . evil-paredit-mode))

(use-package evil-paredit
  :ensure t)
