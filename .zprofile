typeset -U path
for p in /usr/local/bin /sbin /usr/sbin /usr/local/sbin /usr/pkg/bin /usr/pkg/sbin ~/bin ~/.cargo/bin;
do
    [[ -d $p ]] && path=($p $path)
done
