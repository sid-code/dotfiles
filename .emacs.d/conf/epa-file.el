(use-package epa-file
  :requires pinentry
  :init
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback))
