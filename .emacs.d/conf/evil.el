(use-package evil
  :ensure t
  :defer t
  :init
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map (kbd "Z Z") 'server-edit)
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (delete 'circe-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'circe-mode)
  (delete 'shell-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'shell-mode))
