
;;; Code:

(defvar commonrun-map (make-sparse-keymap))
(defmacro commonrun-no-terminal (cmd)
  "Create a lambda to run CMD from a shell.
CMD should start a graphical app, or perhaps a daemon."
  `(lambda () (interactive)
     ,(format "Run the program '%s'." cmd)
     (start-process-shell-command ,cmd nil ,cmd)))

(defmacro commonrun-with-terminal (cmd &optional bufname)
  "Create a lambda to run CMD in a terminal.
The buffer is named BUFNAME, or if not specified CMD."
  (unless bufname (setf bufname cmd))
  `(lambda () (interactive)
     ,(format "Runs the command '%s' in a terminal." cmd)
     (setq existing-buf (get-buffer ,bufname))
     (if existing-buf
         (switch-to-buffer existing-buf)
       (let ((vterm-shell ,cmd))
         (vterm ,bufname)))))

(define-key commonrun-map (kbd "q")
  (commonrun-no-terminal "myqutebrowser"))

(define-key commonrun-map (kbd "n")
  (commonrun-with-terminal "/usr/bin/ncmpcpp" "ncmpcpp term"))

(define-key commonrun-map (kbd "u")
  (commonrun-with-terminal "tail -f ~/unison.log" "unison log term"))

(require 'keybinder)
(exwm-input-set-key (kbd "s-e") commonrun-map)

(provide 'commonrun)
;;; commonrun.el ends here
