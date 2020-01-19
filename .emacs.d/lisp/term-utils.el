(defvar sid/default-terminal-name "term"
  "The default name of a terminal when using `open-new-terminal'.")

(setq vterm-shell sid/shell-program)

(defun sid/read-terminal-name ()
  "Read a terminal name from the minibuffer."
  (list (read-string (format "Terminal name (%s): " sid/default-terminal-name)
                     nil nil
                     sid/default-terminal-name
                     nil)))

(defun sid/open-new-terminal (name)
  "Opens a new terminal named NAME.
NAME can be interactively provided.
The default value for this parameter is in the variable `default-terminal-name'."
  (interactive (sid/read-terminal-name))
  (let ((buf (vterm)))
    (rename-buffer name buf)
    buf))

(defun sid/open-new-terminal-other-window (name)
  (interactive (sid/read-terminal-name))
  (let ((buf (sid/open-new-terminal name)))
    (bury-buffer buf)
    (switch-to-buffer-other-window buf)))

(provide 'term-utils)
;;; term-utils.el ends here.
