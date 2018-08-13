;;; init.el --- my emacs init.el
;;; Commentary:
;;; this is a file Emacs runs on startup
;;; Code:

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; general-configuration
(progn
  (menu-bar-mode -1)
  (xterm-mouse-mode 1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)
  (winner-mode 1)
  (display-time-mode t)
  (display-battery-mode 1))

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(server-start)

(set-face-attribute 'default nil :height 110)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(require 'use-package)
(setq use-package-verbose t)

(use-package neotree
  :defer t)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :config
  (load-theme 'doom-vibrant t)

  (doom-themes-visual-bell-config)

  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ivy
  :ensure t
  :init
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done))

(use-package counsel
  :ensure t
  :defer t
  :bind (("C-c C-/" . counsel-grep)))

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
  (evil-define-key 'normal evil-normal-state-map
    "ZQ" 'with-editor-cancel
    "ZZ" 'with-editor-finish)
  (delete 'term-mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes 'term-mode))

(use-package smart-tabs-mode
  :ensure t
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  (smart-tabs-insinuate 'c 'c++)

  :config
  (add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t))))

(use-package pc-bufsw
  :defer t
  :bind (("s-<tab>" . pc-bufsw-mru)
         ("s-`" . pc-bufsw-lru)))

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

  (require 'ob-ditaa)
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "ditaa")))  ; don't ask for ditaa
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

;; exwm
(load-file "~/.emacs.d/exwm-init.el")

;; keybinds
(progn

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)

  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)
  (exwm-input-set-key (kbd "s-s") 'exwm-workspace-swap)

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

  (defvar default-terminal-name "term"
    "The default name of a terminal when using `open-new-terminal'.")

  (defun open-new-terminal (name)
    "Opens a new terminal named NAME.
NAME can be interactively provided.
The default value for this parameter is in the variable `default-terminal-name'."
    (interactive
     (list (read-string (format "Terminal name (%s): " default-terminal-name)
                        nil nil
                        default-terminal-name
                        nil)))
    (rename-buffer name (term "/usr/bin/zsh")))

  (exwm-input-set-key (kbd "s-t") 'open-new-terminal))


;; misc settings
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

(setq tab-width 8)
(setq c-basic-offset 8)

(add-hook 'term-mode-hook
   (lambda ()
     ;; C-x is the prefix command, rather than C-c
     (term-set-escape-char ?\C-x)))

(use-package go-mode
  :ensure t
  :defer t
  :hook ((before-save . gofmt-before-save)))

(use-package nim-mode
  :ensure t
  :defer t
  :init
  (setq nimsuggest-path "/home/sid/bin/nimsuggest")
  :hook ((nim-mode-hook . nimsuggest-mode)
         (nimsuggest-mode-hook . company-mode)
         (nimsuggest-mode-hook . flycheck-mode)))

(use-package cquery
  :ensure t
  :defer t
  :init
  (setq cquery-executable "/usr/bin/cquery")
  (defun cquery//enable ()
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil)))
  (add-hook 'c-mode-hook #'cquery//enable))

(use-package lsp-java
  :ensure t
  :defer t
  :init
  (setq lsp-java--workspace-folders (list "~/code/mws-example"))
  (add-hook 'java-mode-hook #'lsp-java-enable))

(use-package lsp-ui
  :ensure t
  :defer t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  
;;(use-package google-this
;;  :ensure t
;;  :defer t
;;  :init
;;  (google-this-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(doc-view-continuous t)
 '(org-babel-load-languages '((C . t) (emacs-lisp . t)))
 '(package-selected-packages
   '(cquery-mode cquery lsp-mode google-this dix-evil pdf-tools nim-mode epa-file org-mode org-journal emms w3m simpleclip webpaste web-mode matlab-mode all-the-icons-dired neotree doom-themes company tide csv-mode use-package flycheck volume pulseaudio-control markdown-mode ess polymode smart-mode-line auctex pc-bufsw multi-term magit evil-magit smart-tabs-mode switch-window exwm projectile helm go-mode nlinum evil))
 '(reb-re-syntax 'string)
 '(select-enable-clipboard nil)
 '(tramp-default-method "ssh"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

