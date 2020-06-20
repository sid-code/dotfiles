
;; misc configuration
(setq confirm-nonexistent-file-or-buffer t)
(menu-bar-mode -1)

(xterm-mouse-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(display-battery-mode 1)

(set-face-attribute
 'default nil
 :family "Monospace"
 :height (cond
          ((string-equal sid/system-id "harth") 115)
          ((string-equal sid/system-id "doa") 120)
          ((string-equal sid/system-id "archlinux") 100)
          ((string-equal sid/system-id "fish") 105)
          (t 115)))

(setq tab-width 8)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(setq select-enable-clipboard t)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")
(display-time-mode t)

(defvar sid/shell-program "/usr/bin/zsh" "My shell program.")

(provide 'baseconfig)
;;; baseconfig.el ends here
