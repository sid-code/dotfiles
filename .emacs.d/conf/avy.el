(use-package avy
  :ensure t
  :defer t

  :config
  (evil-define-key 'normal 'global "S" nil)
  (evil-define-key 'normal 'global "SA" 'avy-goto-char)
  (evil-define-key 'normal 'global "SS" 'avy-goto-char-2)
  (evil-define-key 'normal 'global "SD" 'avy-goto-char-timer)
  (evil-define-key 'normal 'global "Sw" 'avy-goto-word-1)
  (evil-define-key 'normal 'global "SW" 'avy-goto-word-0)
  (evil-define-key 'normal 'global "SL" 'avy-goto-line)
  :bind (("C-:" . avy-goto-char)
         ("C-;" . avy-goto-char-2)))
