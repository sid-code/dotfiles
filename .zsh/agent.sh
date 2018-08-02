if ! which keychain 2>&1 >/dev/null ; then
    echo "no keychain!"
else
    eval `keychain --eval id_rsa`
fi
