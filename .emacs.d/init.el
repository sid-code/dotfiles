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
(defvar sid/with-exwm t "Are we using EXWM?")
(defvar sid/config-module-basepath (concat user-emacs-directory "conf/")
  "Directory where extra config modules are to be loaded.")
(defvar sid/config-override-basepath (concat user-emacs-directory "override/")
  "Directory from where override modules are to be loaded.")

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(defvar sid/system-id
  (system-name)
  "The name we will use to identify this system.")

;;}}}
;;{{{

(use-package modules)

(sid/load-config-module "baseconfig")

(server-start)
(sid/load-override-module)

(if sid/with-exwm
  (sid/load-config-module "exwm"))

(sid/load-config-module "keybinder")
(sid/load-config-module "globalkeys")
(sid/load-config-module-for-systems "framemove" "doa")

(sid/load-config-module "smarttabs")
(sid/load-config-module "help")

(sid/load-config-module "doom-themes")

(sid/load-config-module "evil")
(sid/load-config-module "ivy")
(sid/load-config-module "comint")
(sid/load-config-module "vterm")

; (sid/load-config-module "tramp")
(sid/load-config-module "pdf-tools")
(sid/load-config-module "openwith")
(sid/load-config-module "org")
(sid/load-config-module "org-roam")
(sid/load-config-module "irc")

(sid/load-config-module-for-systems "notmuch" "doa")

(sid/load-config-module "counsel")

(sid/load-config-module "lsp-mode")
(sid/load-config-module "company")
(sid/load-config-module "ccls")
(sid/load-config-module "lsp-ui")
(sid/load-config-module "lsp-java")
(sid/load-config-module "paredit")
(sid/load-config-module "evil-paredit")
(sid/load-config-module "magit")
(sid/load-config-module "slime")

(sid/load-config-module "lua")
(sid/load-config-module "typescript")
(sid/load-config-module "go")
(sid/load-config-module "lisp")

(sid/load-config-module "elisp")
(sid/load-config-module "latex")
(sid/load-config-module "web")
(sid/load-config-module "nim")

(sid/load-config-module "pinentry")
(sid/load-config-module "epa-file")

(sid/load-config-module "avy")
(sid/load-config-module "webpaste")
(sid/load-config-module "simpleclip")
(sid/load-config-module "commonrun")
(sid/load-config-module "schoolnav")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
