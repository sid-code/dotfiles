;; Quick class navigation
(defvar sid/school-path (concat sid/homedir "/asu") "School files base directory.")
(defun sid/get-class-alist (base-path)
  "Gets an alist of class descriptions and their directories.
Use BASE-PATH as the base path."
  (map 'list
       (lambda (x)
         (cons (with-temp-buffer
                 (insert-file-contents x)
                 (buffer-string))
               (file-name-directory x)))
       (file-expand-wildcards (concat (file-name-as-directory base-path) "*/.metadata"))))

(defun sid/find-class ()
  "Open a menu to select a class."
  (interactive)
  (ivy-read "Select a class: " (sid/get-class-alist sid/school-path)
            :action (lambda (pair)
                      (find-file (cdr pair)))))
