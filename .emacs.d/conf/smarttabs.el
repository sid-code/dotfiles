(use-package smart-tabs-mode
  :ensure t
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  (smart-tabs-insinuate 'c 'c++ 'java)
  (smart-tabs-mode)

  :config
  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 4
                                    tab-width 4
                                    indent-tabs-mode t)))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq c-basic-offset 8
                                        tab-width 8
                                        indent-tabs-mode t))))

