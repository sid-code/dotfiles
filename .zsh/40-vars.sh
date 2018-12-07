export EDITOR="emacsclient -a /usr/bin/vim"
export GOPATH="$HOME/code/go"
export ANDROID_HOME="$HOME/Android/Sdk/"

export NPM_PACKAGES="$HOME/.npm-packages"

# Add new PATH entries here
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$HOME/.local/bin" # pip and perhaps other things
export PATH="$PATH:$HOME/Nim/bin"
export PATH="$PATH:$HOME/.nimble/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/sync/bin"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$NPM_PACKAGES/bin"

unset MANPATH
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

# export PATH="$PATH:$ANDROID_HOME/tools"
# export PATH="$PATH:$ANDROID_HOME/platform-tools"
# export PATH="$PATH:/usr/lib/jvm/java-8-openjdk/bin"

