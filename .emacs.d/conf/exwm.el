;;; exwm-init-script --- my exwm init configuration
;;; Commentary:
;;; this is horrible

;;; Code:

(defun sid/exwm-maybe-rename-buffer-to-class ()
  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
              (string= "gimp" exwm-instance-name))
    (exwm-workspace-rename-buffer exwm-class-name)))

(defun sid/exwm-maybe-rename-buffer-to-title ()
  (when (or (not exwm-instance-name)
            (string-prefix-p "sun-awt-X11-" exwm-instance-name)
            (string= "gimp" exwm-instance-name)
            (string= "evince" exwm-instance-name)
            (string= "qutebrowser" exwm-instance-name)
            (string= "qutebrowser.py" exwm-instance-name))
    (exwm-workspace-rename-buffer exwm-title)))

(use-package exwm
  :ensure t
  :init
  
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-randr)
  

  (setq-default exwm-workspace-number 10
                exwm-workspace-show-all-buffers t
                exwm-layout-show-all-buffers t
                exwm-manage-force-tiling t)


  (defun sid/exwm-randr-screen-change ()
    (start-process-shell-command "xrandr" nil "xrandr --output DVI-D-0 --pos 0x0")
    (start-process-shell-command "xrandr" nil "xrandr --output HDMI-A-0 --right-of DVI-D-0 --pos 1920x0"))
  (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-A-0" 1 "DVI-D-0" 2 "DVI-D-0" 3 "DVI-D-0" 4 "DVI-D-0"))
  (add-hook 'exwm-randr-screen-change-hook #'sid/exwm-randr-screen-change)
  (exwm-randr-enable)

  (add-hook 'exwm-update-class-hook 'sid/exwm-maybe-rename-buffer-to-class)
  (add-hook 'exwm-update-title-hook 'sid/exwm-maybe-rename-buffer-to-title)

  (setq exwm-input-simulation-keys nil)


  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line.
                                        ; (setq exwm-workspace-minibuffer-position 'bottom)
                                        ; (setq helm-echo-input-in-header-line t)

  ;;make exwm windows default to char instead of line mode
  ;;(add-hook 'exwm-manage-finish-hook
  ;;          (lambda () (call-interactively #'exwm-input-release-keyboard)
  ;;            (exwm-layout-hide-mode-line)))

                                        ;send all keypresses to emacs in line mode
                                        ;(setq exwm-input-line-mode-passthrough t)

  ;; Do not forget to enable EXWM. It will start by itself when things are
  ;; ready.  You can put it _anywhere_ in your configuration.
  (exwm-enable)


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
                                    (call-interactively #'exwm-floating-toggle-floating))))

