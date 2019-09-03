if ! which keychain 2>&1 >/dev/null ; then
    echo "no keychain!"
else
    rm -r $HOME/ssh_auth_sock
    eval `keychain --quiet --eval id_ecdsa`
    ln -s $SSH_AUTH_SOCK $HOME/ssh_auth_sock
fi
