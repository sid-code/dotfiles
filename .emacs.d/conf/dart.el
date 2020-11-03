(defcustom dartfmt-bin-path
  "/home/sid/bin/mydartfmt"
  "Absolute path to dartfmt binary."
  :type 'string)
(defun sid/run-dartfmt ()
  (start-process "dartfmt" "*dartfmt-output*" dartfmt-bin-path))

(defun sid/dartfmt-sentinel (proc event)
  (message "Formatted with dartfmt")
  (print (process-exit-status proc)))

(defun sid/dartfmt-this-file ()
  (interactive)
  (when (not (equal major-mode 'dart-mode))
    (error "only call dartfmt-buffer in a dart-mode buffer"))
  (let ((proc (sid/run-dartfmt)))
    (set-process-sentinel proc #'sid/dartfmt-sentinel)
    (process-send-string proc (buffer-string))
    (process-send-eof proc)
    (while (accept-process-output proc))
    (let ((old-point (point)))
      (erase-buffer)
      (insert (with-current-buffer (process-buffer proc) (buffer-string)))
      (goto-char old-point))))

(use-package dart-mode
  :config
  :bind (:map dart-mode-map ("C-c C-c" . sid/dartfmt-this-file)))
