typeset -U path
for p in /sbin /usr/sbin /usr/local/sbin /usr/pkg/bin /usr/pkg/sbin ~/bin ~/.rvm/bin;
do
    [[ -d $p ]] && path=($p $path)
done

typeset -U fpath
fpath=($fpath ~/.zsh/zsh-git)

# do this here because it sets PATH stuff
[ -f ~/.rvm/scripts/rvm ] && source ~/.rvm/scripts/rvm
