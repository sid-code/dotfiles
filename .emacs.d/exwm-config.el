;;; exwm-init-script --- my exwm init configuration
;;; Commentary:

;;; Code:

(use-package exwm
  :ensure t
  :init

  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-randr)

  ;; Thinkpad (fatman) cannot handle yanking exwm buffers across
  ;; workspaces.
  (let ((is-fatman (string-equal sid/system-id "fatman"))
        (allow-yanking-exwm-buffers (not is-fatman)))

    (setq-default exwm-workspace-number 10
                  exwm-workspace-show-all-buffers allow-yanking-exwm-buffers
                  exwm-layout-show-all-buffers allow-yanking-exwm-buffers
                  exwm-manage-force-tiling t))

  ;; xrandr settings
  (defun sid/exwm-randr-screen-change ()
    nil)
  (setq exwm-randr-workspace-output-plist '())
  (add-hook 'exwm-randr-screen-change-hook #'sid/exwm-randr-screen-change)
  (exwm-randr-enable)

  ;; window titles: every time a window updates its class or hook, we
  ;; need to rename the buffer (with some exceptions).
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; keybinds
  (setq exwm-input-simulation-keys nil)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)
  (exwm-input-set-key (kbd "s-s") 'exwm-workspace-swap)

  (defvar sid/exwm-last-workspace exwm-workspace--current "The last active EXWM workspace.")
  (defun sid/exwm-record-last-workspace (orig-fn &rest args)
    (setq sid/exwm-last-workspace exwm-workspace--current)
    (apply orig-fn args))

  (defun sid/exwm-workspace-switch-last () (interactive)
         (exwm-workspace-switch sid/exwm-last-workspace))

  (advice-add 'exwm-workspace-switch :around #'sid/exwm-record-last-workspace)

  (exwm-input-set-key (kbd "s-<tab>") 'sid/exwm-workspace-switch-last)

  (exwm-input-set-key (kbd "s-`") (lambda () (interactive) (exwm-workspace-switch 0)))
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  (exwm-input-set-key (kbd "s-R") (lambda () (interactive) (call-interactively #'exwm-reset)))
  (exwm-input-set-key (kbd "s-r") (lambda (command) (interactive (list (read-shell-command "$ ")))
                                    (start-process-shell-command command nil command)))
  (exwm-input-set-key (kbd "s-o")
                      (lambda () (interactive) (start-process "" nil "/usr/bin/slock")))
  (exwm-input-set-key (kbd "s-<return>")
                      (lambda () (interactive) (start-process "" nil "/usr/bin/urxvt")))
  (exwm-input-set-key (kbd "s-c")
                      (lambda() (interactive) (start-process "" nil "/home/sid/bin/qute-launch")))

  (exwm-input-set-key (kbd "s-i")
                      (lambda () (interactive) (call-interactively #'exwm-input-release-keyboard)))
  (exwm-input-set-key (kbd "s-<escape>")
                      (lambda () (interactive) (call-interactively #'exwm-input-grab-keyboard)))

  (exwm-input-set-key (kbd "s-<f2>")
                      (lambda () (interactive)
                        (shell-command "/home/sid/code/dwmbar/dwmstatus/rawstatus --once")))

  (exwm-input-set-key (kbd "s-x") (lambda () (interactive)
                                    (call-interactively #'exwm-floating-toggle-floating)))


  (exwm-enable))
