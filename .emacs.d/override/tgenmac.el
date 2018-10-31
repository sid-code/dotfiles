;; woo

;; we don't want to use exwm
(setq sid/with-exwm nil)

;; use /bin/zsh instead of the usual /usr/bin/zsh
(setq sid/shell-program "bin/zsh")

(global-set-key (kbd "s-x") 'counsel-M-x)
