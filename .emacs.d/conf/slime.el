(use-package slime
  :requires (paredit evil-paredit evil)
  :commands (slime)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 16384")
  :bind (:map lisp-mode-map
              ("C-c c" . slime-eval-buffer)
              ("C-c C-b" . slime-interrupt))
  :hook (lisp-mode . paredit-mode)
  :hook (lisp-mode . evil-paredit-mode))
