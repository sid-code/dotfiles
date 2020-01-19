(use-package slime
  :requires (paredit evil-paredit evil)
  :commands (slime)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 1024")
  :bind (:map lisp-mode-map
              ("C-c c" . slime-eval-buffer))
  :hook (lisp-mode . paredit-mode)
  :hook (lisp-mode . evil-paredit-mode))
