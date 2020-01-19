(use-package magit
  :ensure t
  :defer t
  :config
  (require 'evil-magit))

(use-package evil-magit
  :requires (evil magit)
  :ensure t
  :defer t)
