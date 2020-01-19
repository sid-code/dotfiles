(use-package tide
  :ensure t
  :defer t
  :config
  (defun setup-tide-mode ()
    "Set up tide mode."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  :hook ((before-save . tide-format-before-save)
         (typescript-mode . setup-tide-mode)))
