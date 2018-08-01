export ZDOTDIR=$HOME/.zsh

# oh-my-zsh {{{
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="my" # random for random theme
DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="mm/dd/yyyy"

plugins=(git archlinux z)


source $ZSH/oh-my-zsh.sh
# }}}

for file in $ZDOTDIR/*.sh; do
    echo "Sourcing $file"
    source $file
done

bindkey "^?" backward-delete-char

#eval "$(direnv hook zsh)"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

# vim: fdm=marker
