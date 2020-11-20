(use-package vc-hg
  :init
  (setq vc-handled-backends
        (delete 'Fig (delete 'Hg vc-handled-backends))))
