# zsh aliases

alias ls="ls --color $*"
alias jc="source ~/j64-806-user/jvars.sh;\$j64"
alias gl="git log --oneline --all --graph --decorate $*"
alias xc="xclip -selection clipboard"
alias xco="xclip -o -selection clipboard"
alias xcs="xclip -selection primary"
alias xcso="xclip -o -selection primary"

alias agent="pkill ssh-agent;eval \$(ssh-agent);ssh-add"

alias nc="nim c"
alias ncr="nim c -r"

alias py2env="source ~/.python2-venv/bin/activate"
alias py3env="source ~/.python-venv/bin/activate"

alias vim="emacsclient --alternate-editor=\"nvim\""

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

alias off="shutdown now"

alias dcd="sudo systemctl start dhcpcd"
alias hwifi="sudo netctl stop-all;sudo netctl start wlp2s0-Buffalo-A-326A"
alias hhwifi="sudo netctl stop-all;sudo netctl start wlp2s0-kulkarni-5G"
alias awifi="sudo netctl stop-all;sudo netctl start wlp2s0-asu"

alias dback="ssh skulkarni@dback-login1.tgen.org"

extip() {
  curl -sf ipecho.net/plain && echo
}
