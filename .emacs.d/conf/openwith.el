(use-package openwith
  :ensure t
  :init (openwith-mode t)
  :config
  (setq openwith-associations`(("\\.pdf\\'" "evince" (file)))))
