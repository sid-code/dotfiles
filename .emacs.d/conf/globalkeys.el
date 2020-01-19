;; universal keybinds
(progn
  (exwm-input-set-key (kbd "s-a") 'org-agenda)

  (exwm-input-set-key (kbd "s-z") 'winner-undo)
  (exwm-input-set-key (kbd "s-y") 'winner-redo)

  (exwm-input-set-key (kbd "s-Q") 'delete-other-windows)

  (global-set-key (kbd "C-x C-x") (lambda () (interactive) (switch-to-buffer nil)))
  (global-set-key (kbd "C-x C-r") 'rename-buffer)
  (exwm-input-set-key (kbd "C-x M-f") 'sid/find-class)


  (exwm-input-set-key (kbd "s-C") #'kill-buffer-and-window)

  (exwm-input-set-key (kbd "s-q") 'delete-window)
  (exwm-input-set-key (kbd "s-h") 'windmove-left)
  (exwm-input-set-key (kbd "s-j") 'windmove-down)
  (exwm-input-set-key (kbd "s-k") 'windmove-up)
  (exwm-input-set-key (kbd "s-l") 'windmove-right)

  (exwm-input-set-key (kbd "s-L") (lambda () (interactive)
                                    (call-interactively #'split-window-right)
                                    (call-interactively #'windmove-right)))
  (exwm-input-set-key (kbd "s-J") (lambda () (interactive)
                                    (call-interactively #'split-window-below)
                                    (call-interactively #'windmove-down)))
  (exwm-input-set-key (kbd "s-H") 'split-window-right)
  (exwm-input-set-key (kbd "s-K") 'split-window-below)

  (exwm-input-set-key (kbd "<pause>") (lambda () (interactive)
                                        (start-process "" nil "/home/sid/sync/bin/unipause")))
  (exwm-input-set-key (kbd "S-<pause>") (lambda () (interactive)
                                        (start-process-shell-command "" nil "/home/sid/bin/rmpc toggle")))

  (defun sid/buffer-search-switch (bufname)
    "Switch to window containing a buffer named (exactly) BUFNAME.  Do nothing if not possible."
    (let ((matches (get-buffer-window-list bufname)))
      (if matches
          (select-window (car matches))
        nil)))
  (exwm-input-set-key (kbd "s-f") (lambda () (interactive) (sid/buffer-search-switch "Firefox"))))
