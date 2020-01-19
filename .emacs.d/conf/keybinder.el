
(if (not sid/with-exwm)
    (defun exwm-input-set-key (key val)
      "Forward KEY and VAL to global-set-key."
      (global-set-key key val)))

(provide 'keybinder)
;;; keybinder.el ends here
