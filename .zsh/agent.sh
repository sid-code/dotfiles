if ! which keychain 2>&1 >/dev/null ; then
    echo "no keychain!"
else
    if ! pgrep ssh-agent 2>&1 >/dev/null ; then
        rm $HOME/ssh_auth_sock
        eval `keychain --quiet --eval id_rsa`
        ln -s $SSH_AUTH_SOCK $HOME/ssh_auth_sock
    else
        echo "agent already running!"
    fi
fi
