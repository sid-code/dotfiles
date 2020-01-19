(use-package notmuch
  :defer
  :config
  (setq mail-user-agent 'message-user-agent
        user-mail-address "sid@skulk.org"
        user-full-name "Sidharth Kulkarni"
        smtpmail-smtp-server "skulk.org"
        message-send-mail-function 'message-smtpmail-send-it)

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
