# stop gpg from trying to store sockets in eg.,
# /run/user/${UID}/gnupg, forcing it to fall back to the home
# directory. it might be in /run or /var/run. less likely, but it
# could also be /run/gnupg or /var/run/gnupg if it was built with
# --enable-run-gnupg-user-socket
for base in /run /var/run /run/gnupg /var/run/gnupg
do
    # existence check to prevent us from trying to create the same
    # file twice if the filesystems are the same (eg., /run and
    # /var/run are often bind-mounted or symlinked)
    [ -d ${base}/user/${UID} -a ! -e ${base}/user/${UID}/gnupg ] && \
        touch ${base}/user/${UID}/gnupg
done
