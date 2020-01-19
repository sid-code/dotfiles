(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("M-x" . execute-extended-command))
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done))
