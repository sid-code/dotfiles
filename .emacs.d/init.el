;;; init.el --- my emacs init.el
;;; Commentary:
;;; this is a file Emacs runs on startup
;;; Code:

;; load essential packages

(if (version< emacs-version "27.0")
    (package-initialize)
  'package-initialization-skipped)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(require 'use-package)
(setq use-package-verbose t)

;; general-configuration

(defvar sid/homedir "/home/sid" "Home directory.")

;; misc configuration
(progn
  (setq confirm-nonexistent-file-or-buffer t)
  (menu-bar-mode -1)

  (xterm-mouse-mode 1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)

  (setq winner-dont-bind-my-keys t)
  (winner-mode 1)

  (display-battery-mode 1)

  (set-face-attribute
   'default nil
   :height (cond
            ((string-equal (system-name) "harth") 115)
            ((string-equal (system-name) "doa") 120)
            ((string-equal (system-name) "archlinux") 100)
            (t 115)))

  (setq tab-width 8)
  (setq c-default-style "bsd")

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

  (setq display-time-24hr-format t)
  (setq display-time-format "%H:%M - %d %B %Y")
  (display-time-mode t))

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


;; Release C-c in term-- use C-x as prefix key
(eval-after-load "term"
  '(progn (term-set-escape-char ?\C-x)
          (define-key term-raw-map (kbd "C-c") 'term-send-raw)))

(server-start)

;; exwm
(load-file "~/.emacs.d/exwm-init.el")

;; keybinds
(progn
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)
  (exwm-input-set-key (kbd "s-s") 'exwm-workspace-swap)
  (exwm-input-set-key (kbd "s-a") 'org-agenda)

  (exwm-input-set-key (kbd "s-z") 'winner-undo)
  (exwm-input-set-key (kbd "s-y") 'winner-redo)

  (global-set-key (kbd "s-Q") (lambda () (interactive) (other-window -1)))

  (global-set-key (kbd "C-x C-r") 'rename-buffer)
  (exwm-input-set-key (kbd "C-x M-f") 'sid/find-class)

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

  (exwm-input-set-key (kbd "s-C") #'kill-buffer-and-window)

  (exwm-input-set-key (kbd "s-i")
                      (lambda () (interactive) (call-interactively #'exwm-input-release-keyboard)))
  (exwm-input-set-key (kbd "s-<escape>")
                      (lambda () (interactive) (call-interactively #'exwm-input-grab-keyboard)))

  (exwm-input-set-key (kbd "s-<f2>")
                      (lambda () (interactive)
                        (shell-command "/home/sid/code/dwmbar/dwmstatus/rawstatus --once")))

  (defun sid/multi-term-dedicated-toggle-smart () (interactive)
         (progn
           (call-interactively 'multi-term-dedicated-toggle)
           (if (window-valid-p multi-term-dedicated-window)
               (call-interactively 'multi-term-dedicated-select))))

  (exwm-input-set-key (kbd "<f5>") 'sid/multi-term-dedicated-toggle-smart)
  (global-set-key (kbd "s-<f5>") 'sid/multi-term-dedicated-toggle-smart)
  (exwm-input-set-key (kbd "<f6>") 'multi-term-dedicated-select)

  (exwm-input-set-key (kbd "s-q") 'delete-window)
  (exwm-input-set-key (kbd "s-h") 'windmove-left)
  (exwm-input-set-key (kbd "s-j") 'windmove-down)
  (exwm-input-set-key (kbd "s-k") 'windmove-up)
  (exwm-input-set-key (kbd "s-l") 'windmove-right)

  (exwm-input-set-key (kbd "s-L") (lambda () (interactive)
                                    (call-interactively #'split-window-right)
                                    (call-interactively #'windmove-right)))
  (exwm-input-set-key (kbd "s-J") (lambda () (interactive)
                                    (call-interactively #'split-window-below)
                                    (call-interactively #'windmove-down)))
  (exwm-input-set-key (kbd "s-H") 'split-window-right)
  (exwm-input-set-key (kbd "s-K") 'split-window-below)
  (exwm-input-set-key (kbd "s-x") (lambda () (interactive)
                                    (call-interactively #'exwm-floating-toggle-floating)))

  (exwm-input-set-key (kbd "<pause>") (lambda () (interactive)
                                        (start-process "" nil "/home/sid/bin/unipause")))

  (defun sid/buffer-search-switch (bufname)
    "Switch to window containing a buffer named (exactly) BUFNAME.  Do nothing if not possible."
    (let ((matches (get-buffer-window-list bufname)))
      (if matches
          (select-window (car matches))
        nil)))
  (exwm-input-set-key (kbd "s-f") (lambda () (interactive) (sid/buffer-search-switch "Firefox")))

  (defvar sid/default-terminal-name "term"
    "The default name of a terminal when using `open-new-terminal'.")

  (defun sid/open-new-terminal (name)
    "Opens a new terminal named NAME.
NAME can be interactively provided.
The default value for this parameter is in the variable `default-terminal-name'."
    (interactive
     (list (read-string (format "Terminal name (%s): " sid/default-terminal-name)
                        nil nil
                        sid/default-terminal-name
                        nil)))
    (rename-buffer name (term "/usr/bin/zsh")))

  (exwm-input-set-key (kbd "s-t") 'sid/open-new-terminal))


(use-package neotree
  :defer t)

(use-package pdf-tools
  :ensure t
  :defer t
  :bind (:map pdf-view-mode-map ("j" . pdf-view-next-line-or-next-page)
         :map pdf-view-mode-map ("k" . pdf-view-previous-line-or-previous-page)))

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("M-x" . counsel-M-x))
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done))

(use-package counsel
  :ensure t
  :defer t
  :bind (("C-x C-/" . counsel-ag)))

(use-package projectile
  :ensure t
  :defer t
  :init
  (setq projectile-keymap-prefix (kbd "C-x p")))

(use-package counsel-projectile
  :ensure t
  :defer t
  :bind (:map projectile-mode-map ("C-x p f" . counsel-projectile)
         :map projectile-mode-map ("C-x C-/" . counsel-projectile-ag)))

(use-package tramp
  :ensure t
  :defer t
  :init
  (setq tramp-backup-directory-alist `(("." . "~/.emacs.d/saves")))

  :config
  (defvar disable-tramp-backups '(all))

  ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               ;; Disable all tramp backups
               (and disable-tramp-backups
                    (member 'all disable-tramp-backups)
                    (not (file-remote-p name 'method)))
               (not ;; disable backup for tramp with the listed methods
                (let ((method (file-remote-p name 'method)))
                  (when (stringp method)
                    (member method disable-tramp-backups)))))))

  (defun tramp-set-auto-save--check (original)
    (if (funcall backup-enable-predicate (buffer-file-name))
        (funcall original)
      (auto-save-mode -1)))

  (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)

  ;; Use my ~/.ssh/config control master settings according to https://puppet.com/blog/speed-up-ssh-by-reusing-connections
  (setq tramp-ssh-controlmaster-options ""))

(use-package magit
  :ensure t
  :defer t
  :config
  (require 'evil-magit))

(use-package evil
  :ensure t
  :defer t
  :init
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map (kbd "Z Z") 'server-edit)
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode))

(use-package workgroups
  :ensure t)

(use-package persp-mode
  :ensure t
  :defer t
  :config
  (persp-set-keymap-prefix (kbd "C-x C-p"))
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak))

(use-package which-key
  :ensure t
  :init
  (which-key-mode t))

(use-package smart-tabs-mode
  :ensure t
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  (smart-tabs-insinuate 'c 'c++ 'java)
  (smart-tabs-mode)

  :config
  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 4
                                    tab-width 4
                                    indent-tabs-mode t)))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq c-basic-offset 8
                                        tab-width 8
                                        indent-tabs-mode t))))

(use-package flycheck
  :defer t
  :ensure t
  :init (global-flycheck-mode))

(use-package auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)

  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex)))

(use-package company
  :defer t
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(use-package web-mode
  :defer t
  :init
  (setq web-mode-enable-optional-tags t)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package webpaste
  :ensure t
  :defer t
  :init
  (setq webpaste-copy-to-clipboard t)
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))

(use-package simpleclip
  :ensure t
  :defer t
  :config
  (simpleclip-mode 1)
  (unbind-key "s-c" simpleclip-mode-map)
  (unbind-key "s-x" simpleclip-mode-map)
  (unbind-key "s-v" simpleclip-mode-map))

(use-package pinentry
  :ensure t
  :defer t)

(use-package org
  :ensure t
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-M-RET-may-split-line nil)
  ;; org-mode latex commands should be allowed to span 4 lines
  ;; hahahaah just kidding this doesn't work
  (setcar (nthcdr 2 (car (nthcdr 2 org-latex-regexps))) 4)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 4)

  ;; org-agenda
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down category-keep deadline-up)
          (tags priority-down catgory-keep)
          (search category-keep)))

  (require 'ob-ditaa)
  (require 'ob-dot)
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "dot")
             (string= lang "ditaa"))))  ; don't ask for these
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar"))

(use-package org-journal
  :ensure t
  :defer t
  :init
  (setq org-journal-dir "~/txt/journal/")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-prefix "# -*- epa-file-encrypt-to: (\"kulkarnisidharth1@gmail.com\") -*-\n"))

(use-package epa-file
  :defer t
  :init
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback))

(use-package tide
  :ensure t
  :defer t
  :config
  (defun setup-tide-mode ()
    "Set up tide mode."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  :hook ((before-save . tide-format-before-save)
         (typescript-mode . setup-tide-mode)))

(use-package go-mode
  :ensure t
  :defer t
  :hook ((before-save . gofmt-before-save)))

(use-package nim-mode
  :ensure t
  :defer t
  :init
  (setq nimsuggest-path nil))
  ;:hook ((nim-mode-hook . nimsuggest-mode)
  ;       (nimsuggest-mode-hook . company-mode)
  ;       (nimsuggest-mode-hook . flycheck-mode)))

(use-package cquery
  :ensure t
  :defer t
  :init
  (setq cquery-executable "/usr/bin/cquery")

  (defun sid/cquery-enable ()
    (interactive)
    "Enable cquery for the current buffer"

    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil)))

  (add-hook 'c-mode-hook #'sid/cquery-enable))

(use-package lsp-mode
  :ensure t
  :defer t
  :init)

(use-package lsp-java
  :ensure t
  :defer t
  :config
  (setq lsp-java--workspace-folders (list "TBA"))
  (add-hook 'java-mode-hook (lambda ()
                              (lsp-java-enable)
                              (company-mode)
                              (lsp-ui-mode)))
  :init
  (require 'lsp-java))

(use-package lsp-ui
  :ensure t
  :defer t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package mingus
  :defer t
  :config
  (add-hook 'mingus-browse-hook 'evil-emacs-state)
  (add-hook 'mingus-help-hook 'evil-emacs-state)
  (add-hook 'mingus-playlist-hooks 'evil-emacs-state)) ;hookS?

(use-package notmuch
  :defer
  :config
  (setq mail-user-agent 'message-user-agent
        user-mail-address "kulkarnisidharth1@gmail.com"
        user-full-name "Sidharth Kulkarni"
        smtpmail-smpt-server "smtp.gmail.com"
        message-send-mail-function 'message-smtp-send-it)

  (setq notmuch-search-oldest-first nil)

  (defun sid/exec-mbsync ()
    "Execute mbsync"
    (interactive)
    (set-process-sentinel
     (start-process-shell-command "mbsync"
                                  "*mbsync*"
                                  "mbsync -a && notmuch new")
     '(lambda (process event)
        (notmuch-refresh-all-buffers)
        (let ((w (get-buffer-window "*mbsync*")))
          (when w
            (with-selected-window w (recenter window-end)))))))

  (defun sid/notmuch-archive ()
    (interactive)
    (notmuch-show-tag-message "-inbox"))

  :init
  (require 'notmuch)

  :bind (:map notmuch-common-keymap ("S" . sid/exec-mbsync)
         :map notmuch-search-mode-map ("d" . notmuch-search-archive-thread)
         :map notmuch-show-mode-map ("D" . notmuch-show-archive-thread)))


;(use-package rcirc
;  :ensure t
;  :defer t
;  
;  :hook ((rcirc-mode . rcirc-omit-mode))
;  :config
;  (setq rcirc-server-alist
;        '(("donot.violates.me"
;           :encryption tls
;           :port 7776
;           :nick "bozaloshtsh/freenode"
;           :password "bozaloshtsh:wend111"
;           :full-name "bozaloshtsh"
;           :channels ("#emacs" "#nim"))))
;  
;  :init
;  (rcirc-track-minor-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(doc-view-continuous t)
 '(org-agenda-files (quote ("~/sync/org/school.org")))
 '(org-babel-load-languages (quote ((C . t) (emacs-lisp . t))))
 '(package-selected-packages
   (quote
    (gnuplot gnuplot-mode which-key counsel-projectile dmenu cquery-mode cquery lsp-mode google-this dix-evil pdf-tools nim-mode epa-file org-mode org-journal emms w3m simpleclip webpaste web-mode matlab-mode all-the-icons-dired neotree doom-themes company tide csv-mode use-package flycheck volume pulseaudio-control markdown-mode ess polymode smart-mode-line auctex pc-bufsw multi-term magit evil-magit smart-tabs-mode switch-window exwm projectile helm go-mode nlinum evil)))
 '(reb-re-syntax (quote string))
 '(select-enable-clipboard nil)
 '(tramp-default-method "ssh"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
