
(defun stackoverflow/comint-send-input-maybe ()
  "Only `comint-send-input' when point is after the latest prompt.

Otherwise move to the end of the buffer.

Source: https://stackoverflow.com/a/52212547/945873"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (>= (point) (marker-position (process-mark proc))))
        (comint-send-input)
      (goto-char (point-max)))))

(use-package comint
  :requires evil
  :config
  (add-to-list 'evil-emacs-state-modes 'comint-mode)
  (delete 'comint-mode evil-insert-state-modes)
  (define-key comint-mode-map [remap comint-send-input] 'stackoverflow/comint-send-input-maybe)
  (setq comint-move-point-for-output t))
