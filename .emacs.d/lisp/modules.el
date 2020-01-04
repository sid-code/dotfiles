
;;; Code:
(defun sid/load-config-module (name)
  "Load a configuration module by NAME.  Do not specify .el."
  (interactive)
  (message (expand-file-name (concat name ".el") sid/config-module-basepath))
  (load (expand-file-name (concat name ".el") sid/config-module-basepath)))

(defun sid/load-config-module-for-systems (name &rest systems)
  "Load a configuration module by NAME, but only if the system name is in SYSTEMS.  Do not specify .el in the NAME."
  (interactive)
  (if (or (equal systems '(:all)) (member sid/system-id systems))
      (sid/load-config-module name)))

(defun sid/load-override-module ()
  "Load a override module by NAME for the current system."
  (interactive)
  (let ((fname (expand-file-name
                (concat sid/system-id ".el")
                sid/config-override-basepath)))
    (if (file-exists-p fname)
        (load-file fname)
      (message (format "could not find override file: %s" fname)))))

(provide 'modules)
;;; modules.el ends here
