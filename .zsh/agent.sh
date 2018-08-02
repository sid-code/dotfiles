if ! which keychain 2>&1 >/dev/null ; then
    echo "no keychain!"
else
    rm /home/sid/ssh_auth_sock
    eval `keychain --quiet --eval id_rsa`
    ln -s $SSH_AUTH_SOCK /home/sid/ssh_auth_sock
fi
