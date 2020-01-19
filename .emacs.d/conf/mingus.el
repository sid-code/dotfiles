(use-package mingus
  :defer t
  :config
  (add-hook 'mingus-browse-hook 'evil-emacs-state)
  (add-hook 'mingus-help-hook 'evil-emacs-state)
  (add-hook 'mingus-playlist-hooks 'evil-emacs-state)) ;hookS?
