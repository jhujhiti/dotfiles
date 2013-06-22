typeset -U path
for p in /sbin /usr/sbin /usr/local/sbin /usr/pkg/bin /usr/pkg/sbin ~/bin;
do
    [[ -d $p ]] && path=($p $path)
done

typeset -U fpath
fpath=($fpath ~/.zsh/zsh-git)
