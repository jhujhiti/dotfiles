# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=5000

platform=$(uname -s)
linux() {
    if [[ "$platform" == "Linux" ]]; then
        return 0
    else
        return 1
    fi
}
freebsd() {
    if [[ "$platform" == "FreeBSD" ]]; then
        return 0
    else
        return 1
    fi
}
darwin() {
    if [[ "$platform" == "Darwin" ]]; then
        return 0
    else
        return 1
    fi
}

# the path to our dotfiles repository
if linux; then
    dotfiles=$(dirname $(readlink -f ~/.zshrc))
else
    dotfiles=$(dirname $(realpath ~/.zshrc))
fi

# enable extended globbing
setopt extendedglob
# warn me if a glob doesn't produce anything
setopt nomatch
# traps and options set inside a function will revert to their original
# setting after function return
setopt local_{traps,options}
# don't just execute bang history (eg, !!) -- show me the expanded command
setopt hist_verify
# append to the history file
setopt appendhistory
# and do it as it happens
setopt inc_append_history
# and *don't* share it with all of the other zshs
unsetopt share_history
# don't keep duplicate commands in history
setopt hist_ignore_all_dups
# ignore lines that begin with space
setopt hist_ignore_space
# and the history and fc commands
setopt hist_no_store
# allow substitution in prompts
setopt promptsubst
# print a carriage return before prompt (on by default, necessary for prompt_sp)
setopt prompt_cr
# preserve lines before prompts that don't end with newlines (the reverse-video % thing)
setopt prompt_sp
# stop beeping at me
unsetopt beep
# don't cd to a directory automatically if i forget the cd
unsetopt autocd
# don't interrupt me if a background job finishes while i'm typing
unsetopt notify

# / is a path separator, not part of a word (^W should stop at /)
WORDCHARS=${WORDCHARS/\/}

# emacs line editing mode
bindkey -e

# completion stuff
zstyle :compinstall filename '~/.zshrc'
# find all the git-* commands and let me complete them
zstyle ':completion:*:*:git:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}

autoload -Uz compinit
compinit
autoload -U colors
colors

# which on different platforms behaves differently
safe_which() {
    local TMP
    TMP=`which $1 2>/dev/null`
    ret=$?
    if [ $ret -eq 0 ]; then
        # for Solaris:
        if [ -z "${TMP/*no $1*}" ]; then
            echo ""
            return 1
        fi
    fi
    # for OpenBSD:
    if [ -z "${TMP/*Command not found.}" ]; then
        echo ""
        return 1
    fi
    echo $TMP
    return $ret
}

quick_which() {
    safe_which "$1" 1>/dev/null
    return $?
}

if linux; then
    alias ls="ls --color=yes -Fh"
    alias ll="ls --color=yes -Fhl"
elif freebsd || darwin; then
    alias ls="ls -FGh"
    alias ll="ls -FGhl"
else
    alias ls="ls -Fh"
    alias ll="ls -Fhl"
fi

# tmux is a huge pain in the ass, so here's a hack to set TERM to a 256color
# tmux-compatible setting if and only if the parent shell was using a 256color
# TERM. this is used in conjunction with the update-environment setting in tmux
term_exists() {
    # hilariously, tput with an unknown terminal type on OS X causes an
    # infinite loop. so that's pretty terrible. i really didn't want to have to
    # look through the filesystem for termcap/terminfos, but here we are
    if darwin; then
        return $(find /usr/share/terminfo -type f -name "$1" 2>&1 | grep '.' >/dev/null 2>&1)
    else
        return $(tput -T "$1" init >/dev/null 2>&1)
    fi
}
# there's probably a cross-platform trick to get tput -T <term> {colors,Co} to
# work, but i can't figure it out. zsh always seems to have the correct value
# in $terminfo[colors], so we can spawn another shell to use that
terminfo_colors() {
    echo $(TERM="$1" zsh -c 'echo $terminfo[colors]')
}
if [[ -n "$TMUX" ]]; then
    # candidate terms. we'll use the first one that seems to exist
    terms=(tmux screen)
    if [[ $(terminfo_colors $_PARENT_TERM) -eq 256 ]]; then
        # the parent was 256-capable, so prepend these to the list of
        # candidates
        terms=(tmux-256color screen-256color $terms)
    fi
    # set TERM to whichever we find first
    for t in $terms
    do
        if $(term_exists $t); then
            export TERM=$t
            break
        fi
    done
else
    # set _PARENT_TERM if we are not running inside tmux (save it for later)
    export _PARENT_TERM=$TERM
fi

quick_which vim && export EDITOR=vim

# we're going to try to replace ssh-agent with gpg-agent.
# as of gpg 2.2, we have some new tricks.
if $(quick_which gpg-agent) && $(quick_which gpg-connect-agent) && $(quick_which gpgconf); then
    # only try to touch this if we're sshed here. otherwise we break agent forwarding
    if [ -z "${SSH_CONNECTION}" ]; then
        # gpg-connect-agent will try to start one.
        # the config file will enable the ssh socket when the agent starts.
        # if any of this fails we'll just not touch the environment and ssh-agent itself should work.
        if gpg-connect-agent -q /bye 2>&1 >/dev/null; then
            sock=$(gpgconf -q --list-dir agent-ssh-socket)
            if [ -S "${sock}" ]; then
                export SSH_AUTH_SOCK="${sock}"
                export GPG_TTY=$(tty)
            fi
        fi
    fi
fi

if $(quick_which ssh-add); then
    for k in id_rsa id_ed25519
    do
        if [[ -r ~/.ssh/${k} ]]; then
            pubkey=$(ssh-keygen -yf ~/.ssh/${k} | awk '{ print $2; }' 2>/dev/null)
            ssh-add -L 2>/dev/null | grep "${pubkey}" > /dev/null
            if [ $? -eq 1 ]; then
                ssh-add ~/.ssh/${k} 2>/dev/null
            fi
        fi
    done
fi

# nix-specific stuff. the check for NIX_REMOTE will be true on
# multi-user nix installations and is faster than the which
if [ -n "$NIX_REMOTE" ] || $(quick_which nix-shell); then
    # set up nix-shell to use zsh by default (unless --pure is passed)
    . ${dotfiles}/submodules/zsh-nix-shell/nix-shell.plugin.zsh
fi
nix_prompt() {
    [ -n "$IN_NIX_SHELL" ] && echo " %F{red}nix:${IN_NIX_SHELL}%f"
}

# disable terminal flow control. good riddance
if $(quick_which stty); then
    stty stop '^@' 2>/dev/null
    stty start '^@' 2>/dev/null
fi

# python virtualenvwrapper
# use pyenv if we have the virtualenv and virtualenvwrapper plugins installed
if $(quick_which pyenv) && [ $(pyenv commands --no-sh | grep -Ec '^virtualenv(wrapper)?$' 2>/dev/null) = 2 ]; then
    # setup:
    # pyenv install <version>
    # pyenv global <version>
    # git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
    # git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git $(pyenv root)/plugins/pyenv-virtualenvwrapper
    eval "$(pyenv init --path)"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    pyenv virtualenvwrapper
elif $(quick_which virtualenvwrapper.sh); then
    # does anything use this anymore?
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    . virtualenvwrapper.sh
# debian package
elif [ -r /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]; then
    # does anything use this anymore?
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    . /usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi
venv_prompt() {
    # bail if we're going to manage the prompt with the default virtualenv script instead
    [ -z "$VIRTUAL_ENV_DISABLE_PROMPT" ] && return
    # or if we're not actually in a virtualenv
    [ -z "$VIRTUAL_ENV" ] && return
    venv=$(basename "$VIRTUAL_ENV")
    echo " venv:${venv}"
}

# prompt
autoload -U add-zsh-hook
autoload -U zgitinit
zgitinit

#    echo -n "\n\xc2\xb1 "
scm_prompt() {
    # this code is largely translated from https://github.com/olivierverdier/zsh-git-prompt/blob/master/gitstatus.py
    zgit_isgit || return
    zgit_inworktree || return
    local -A sym
    sym[ahead_of]="↑"
    sym[behind]="↓"
    sym[prehash]=":"
    sym[staged]="●"
    sym[unstaged]="✚"
    sym[conflicts]="✖"
    sym[clean]="✔"
    sym[untracked]="…"
    sym[stashed]="↤"
    sym[sep]="|"

    head=$(zgit_head)

    local -a changed_files
    changed_files=("${(f)$(git diff --name-status | cut -c1)}")
    if [ "${changed_files[1]}" = "" ]; then
        changed_files=()
    fi
    local -a staged_files
    staged_files=("${(f)$(git diff --staged --name-status | cut -c1)}")
    if [ "${staged_files[1]}" = "" ]; then
        staged_files=()
    fi
    changed_U=0
    for f in $changed_files
    do
        [ "$f" = "U" ] && changed_U=$(($changed_U+1))
    done
    changed=$(($#changed_files - $changed_U))
    conflicts=0
    for f in $staged_files
    do
        [ "$f" = "U" ] && conflicts=$(($conflicts+1))
    done
    staged=$(($#staged_files - $conflicts))
    untracked_files=("${(f)$(git ls-files --other --exclude-standard)}")
    if [ "${untracked_files[1]}" = "" ]; then
        untracked_files=()
    fi
    untracked=$#untracked_files

    if [ -f "${zgit_info[dir]}/refs/stash" ]; then
        stashed=0
    else
        stashed=1
    fi

    if [ $changed -eq 0 -a $staged -eq 0 -a $untracked -eq 0 -a $conflicts -eq 0 -a $stashed -eq 1 ]; then
        clean=0
    else
        clean=1
    fi

    remote=""
    if [ -z "$head" ]; then
        head="${sym[prehash]}$(git rev-parse --short HEAD)"
    else
        remote_name="$(git config branch.${head}.remote)"
        if [ -n "$remote_name" ]; then
            merge_name="$(git config branch.${head}.merge)"
            if [ "$remote_name" = "." ]; then # local
                remote_ref=merge_name
            else
                remote_ref="refs/remotes/${remote_name}/${merge_name[12,$#merge_name]}"
            fi
            revgit="$(git rev-list --left-right ${remote_ref}...HEAD)"
            # might need to git rev-list --left-to-right $merge_name...HEAD if that ever fails?
            local -a revs
            if [ -z "$revgit" ]; then
                revs=()
            else
                revs=("${(f)revgit}")
            fi
            ahead=0
            for l in $revs
            do
                [ "$l[1]" = ">" ] && ahead=$(($ahead+1))
            done
            behind=$(($#revs - $ahead))
            if [ $behind -gt 0 ]; then
                remote="${remote}${sym[behind]}${behind}"
            fi
            if [ $ahead -gt 0 ]; then
                remote="${remote}${sym[ahead_of]}${ahead}"
            fi
        fi
    fi

    # finally, make the prompt
    if [ $clean -eq 0 ]; then
        stat="${sym[clean]}"
    else
        stat=""
        if [ $conflicts -gt 0 ]; then
            stat="${stat}${sym[conflicts]}${conflicts}"
        fi
        if [ $staged -gt 0 ]; then
            stat="${stat}${sym[staged]}${staged}"
        fi
        if [ $changed -gt 0 ]; then
            stat="${stat}${sym[unstaged]}${changed}"
        fi
        if [ $untracked -gt 0 ]; then
            stat="${stat}${sym[untracked]}"
        fi
        if [ $stashed -eq 0 ]; then
            stat="${stat}${sym[stashed]}"
        fi
    fi
    echo " ${head}${remote}${sym[sep]}${stat}"
}

if [[ "$IN_TMPDIR" == "yes" ]]; then
    ps1_tmpdir=' %F{red}(tmp)%f'
fi

PS1="[%D{%b %d %H:%M} %~"'$(venv_prompt)$(nix_prompt)$(scm_prompt)'"]
%m${ps1_tmpdir}%# "
RPROMPT=''

# xterm title
my_precmd_hook() {
    print -Pn "\e]0;%m %~\a"
}
add-zsh-hook precmd my_precmd_hook

# these two environment variables are handy for automated debian changelog
# editing and probably other things too
export NAME="Erick Turnquist"
export EMAIL="jhujhiti@adjectivism.org"
# also for debian, from the "New Maintainer's Guide"
export DEBEMAIL="$EMAIL"
export DEBFULLNAME="$NAME"
# also handy. i can't for the life of me remember this
export GPGKEYID="07EEE1676D88BFEB3DBC24F7172E752D0D9E700C"

quick_which less && export MANPAGER="less"

# pager for quagga vtysh
export VTYSH_PAGER=cat

# timezone
export TZ="America/New_York"

chpwd() {
    # we want to switch to an appropriate virtualenv automatically on cd.
    # the mapping variable here can be defined anywhere, probably in .zshrc_local.
    # eg., typeset -A venvmap ; venvmap[${HOME}/myproject]=myvenv
    # TODO: should we leave the venv automatically too?
    if [ -n "$(type workon)" ]; then
        if [[ -v venvmap[$(pwd)] ]]; then
            workon "${venvmap[$(pwd)]}"
        fi
    fi
}

# run a shell in a temporary directory and clean it up afterwards
function tmpdir {
    root=${1:-/tmp}
    d=$(TMPDIR="${root}" mktemp -d)
    pushd ${d} > /dev/null
    export IN_TMPDIR=yes
    ${SHELL}
    unset IN_TMPDIR
    popd > /dev/null
    rm -rf $d
}

# wrap the aws cli
# if we have it installed, and are able to find a filter file for the given
# command, filter the output through jq
# additional options:
# --filter: run the output through this additional jq filter
# --full:   don't apply the default jq filter that we have
function aws {
    local -a opts
    local -A filter
    local filename
    zparseopts -D -E -- -full=opts -filter:=filter
    if [ $# -ge 2 ]; then
        filename=${dotfiles}/aws/${1}/${2}
    fi
    if [[ $#opts -gt 0 || $#filter -gt 0 ]]; then
        if $(quick_which jq); then
            jq1='.'
            if [[ -n $filename && -f $filename && ${opts[(r)--full]} != --full ]]; then
                jq1="-f $filename"
            fi
            if [ -n "$filter" ]; then
                command aws $@ | jq ${=jq1} | jq $filter
            else
                command aws $@ | jq ${=jq1}
            fi
        else
            >&2 echo "jq must be installed in order to use options"
            return 1
        fi
    else
        if $(quick_which jq) && [[ -n "$filename" && -f "$filename" ]]; then
            command aws $@ | jq -f $filename
        else
            command aws $@
        fi
    fi
}

[ -f ~/.zshrc_local ] && source ~/.zshrc_local
