(use-package epa-file
  :requires pinentry
  :config
  (setq epa-pinentry-mode 'loopback)
  :init
  (require 'epa-file)
  (epa-file-enable))
