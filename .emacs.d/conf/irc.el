(use-package circe
  :requires password-store
  :ensure t
  :defer t
  :config
  (defun circe-init ()
    "Initialize circe, with passwords."
    (interactive)
    (require 'password-store)
    (setq circe-reduce-lurker-spam t
          circe-format-say "<{nick}> {body}"
          circe-network-options
          (let ((znc-password (concat "bozaloshtsh:" (password-store-get "znc"))))
            `(("znc-freenode"
               :host "skulk.org"
               :port 7776
               :tls t
               :nick "bozaloshtsh/freenode"
               :pass ,znc-password))))

    (require 'circe-color-nicks)
    (enable-circe-color-nicks)
    (circe "znc-freenode")))
