(use-package org-journal
  :requires epa-file
  :ensure t
  :init
  (setq org-journal-dir "~/txt/journal/")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-prefix "# -*- epa-file-encrypt-to: (\"kulkarnisidharth1@gmail.com\") -*-\n"))

