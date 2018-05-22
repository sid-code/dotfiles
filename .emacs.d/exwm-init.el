;;; exwm-init-script --- my exwm init configuration
;;; Commentary:
;;; this is horrible

;;; Code:

(use-package exwm
  :init
  
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-randr)
  

  (setq-default exwm-workspace-number 10
                exwm-workspace-show-all-buffers t
                exwm-layout-show-all-buffers t
                exwm-manage-force-tiling t)


  (setq exwm-randr-workspace-output-plist '(2 "HDMI-A-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-A-0 --auto --left-of eDP")))
  (exwm-randr-enable)

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
  (exwm-config-ido))

