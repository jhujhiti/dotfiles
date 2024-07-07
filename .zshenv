typeset -U fpath
fpath=($fpath ~/.zsh/zsh-git)

typeset -U path
for p in /opt/homebrew/bin /opt/homebrew/sbin /usr/local/bin /sbin /usr/sbin /usr/local/sbin /usr/pkg/bin /usr/pkg/sbin ~/bin ~/.cargo/bin ~/.krew/bin;
do
    [[ -d $p ]] && path=($p $path)
done

