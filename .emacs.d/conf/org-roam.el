(use-package org-roam
  :ensure
  :hook (org-mode . org-roam-mode)
  :custom (org-roam-directory "/home/sid/sync/org/roam")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
         :map org-mode-map
	      (("C-c n i" . org-roam-insert))))
