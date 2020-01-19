(defun sid/vterm-kill-on-exit ()
  (set-process-sentinel (get-buffer-process (buffer-name))
                        (lambda (_ _) (kill-buffer (buffer-name)))))


(use-package term-utils)
(use-package vterm
  :load-path "emacs-libvterm"
  :commands vterm
  :hook (vterm-mode . sid/vterm-kill-on-exit)
  :init
  (require 'keybinder)
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (delete 'vterm-mode evil-normal-state-modes)
  (exwm-input-set-key (kbd "s-t") 'sid/open-new-terminal)
  (exwm-input-set-key (kbd "s-T") 'sid/open-new-terminal-other-window))
