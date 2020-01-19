(use-package org
  :ensure t
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-M-RET-may-split-line nil)
  ;; org-mode latex commands should be allowed to span 4 lines
  ;; hahahaah just kidding this doesn't work
  (setcar (nthcdr 2 (car (nthcdr 2 org-latex-regexps))) 4)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 4)

  ;; org-agenda
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down category-keep deadline-up)
          (tags priority-down catgory-keep)
          (search category-keep)))

  (require 'ob-ditaa)
  (require 'ob-dot)
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "dot")
             (string= lang "ditaa"))))  ; don't ask for these
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar"))

(use-package org-ref
  :ensure t
  :defer t
  :init

  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex --shell-escape -interaction nonstopmode' -pdf -f %f"
          "bibtex %f")))
