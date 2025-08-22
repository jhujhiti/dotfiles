.SECONDEXPANSION:

LINKS?=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .zprofile \
      .zshenv .zshrc .zsh .Xdefaults .tmux.conf .termcap .mailcap \
      .xsession .nethackrc
BASE?=$(shell basename `pwd`)

REAL_LINKS=$(addprefix ../,$(LINKS))

# FIXME: we can do much better than this
BINS=$(shell ls bin)

REAL_BINS=$(addprefix ../bin/,$(BINS))

all: git links dirs bins

links: $(LINKS) authorized_keys ssh_config gpg xmonad gitignore emacs

git:
	git submodule init
	git submodule sync
	git submodule update
	git status submodules/ --porcelain=v1 | grep '^?? ' | cut -d' ' -f2- | xargs rm -rf

$(REAL_LINKS):
	ln -s $(BASE)/$(@F) ../$(@F)

emacs: ../.emacs.d ../.emacs.d/init.el ../.emacs.d/lisp ../.emacs.d/transient $(addprefix ../.emacs.d/transient/,levels.el values.el)

../.emacs.d:
	mkdir -p $@

../.emacs.d/init.el:
	ln -s ../$(BASE)/emacs/init.el $@

../.emacs.d/lisp:
	ln -s ../$(BASE)/emacs/lisp $@

../.emacs.d/transient: ../.emacs.d
	mkdir -p $@

$(addprefix ../.emacs.d/transient/,levels.el values.el):
	ln -s ../../$(BASE)/emacs/transient/$(@F) $@

gitignore: ../.gitignore

../.gitignore:
	ln -s $(BASE)/global.gitignore ../.gitignore

xmonad: ../.xmonad ../.xmonad/xmonad.hs ../.config/xmobar/xmobarrc ../.xmobar

../.xmonad:
	mkdir -p ../.xmonad

../.xmonad/xmonad.hs:
	ln -s ../$(BASE)/xmonad/xmonad.hs ../.xmonad/xmonad.hs

../.config/xmobar: | ../.config
	mkdir -p ../.config/xmobar

../.config/xmobar/xmobarrc: | ../.config/xmobar
	ln -s ../../$(BASE)/xmonad/xmobarrc ../.config/xmobar/xmobarrc

../.xmobar:
	ln -s .config/xmobar ../.xmobar

authorized_keys: ../.ssh ../.ssh/authorized_keys

ssh_config: ../.ssh ../.ssh/config

../.ssh/authorized_keys:
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys

../.ssh/config:
	ln -s ../$(BASE)/ssh_config ../.ssh/config

gpg: ../.gnupg $(addprefix ../.gnupg/,gpg.conf gpg-agent.conf)

../.gnupg/gpg.conf:
	ln -s ../$(BASE)/gpg.conf ../.gnupg/gpg.conf

../.gnupg/gpg-agent.conf:
	ln -s ../$(BASE)/gpg-agent.conf ../.gnupg/gpg-agent.conf

../.gnupg ../.ssh:
	install -m 0700 -d $@

# we need the sockets in a known location for forwarding over ssh
ifneq (,$(shell which gpgconf))
GPG_SOCKETDIR:=$(shell gpgconf -q --list-dir socketdir)
# it would be nice if we could rely on the shell to trick gpg into not
# storing sockets in /run/user/UID/gnupg, but it won't work during the
# initial dotfiles setup, since the idea is to run this Makefile in
# one shot and then open a new shell. it almost seems like we could
# make the socketdir target depend on .zshrc and then execute the
# gpgconf inside a shell, but that might be a bit too clever. the
# shell might not be set yet, and running zsh explicitly will fail if
# it's not even installed yet. so, let's make this dummy file here as
# well.
# only bother with this if gpgconf says it wants to use /run or
# /var/run (ie., if it's on an affected platform that hasn't been
# fixed by the shell already).
ifneq (,$(filter /run/% /var/run/%,$(GPG_SOCKETDIR)))
$(GPG_SOCKETDIR):
# gpg won't use the directory if it can't create it, so we don't need
# to care if the touch fails
# FIXME: we really need to run this multiple times until we no longer
# get these directories back from gpgconf...
	rm -rf $(GPG_SOCKETDIR)
	touch $(GPG_SOCKETDIR) || echo ''

../.gnupg/socket: $(GPG_SOCKETDIR)
endif

../.gnupg/socket: | ../.gnupg
	install -m 0700 -d $@

gpg: ../.gnupg/socket

GPG_SOCKET_TYPES:=gpg-agent gpg-agent.ssh gpg-agent.browser gpg-agent.extra dirmngr keyboxd
GPG_SOCKETS:=$(addprefix ../.gnupg/S.,$(GPG_SOCKET_TYPES))
gpg: $(GPG_SOCKETS)
$(GPG_SOCKETS): | ../.gnupg/socket
	rm -f $@
	echo "%Assuan%\nsocket=\$${HOME}/.gnupg/socket/\$${GPG_SOCKET_PREFIX}$(@F)" > $@
# these redirect files might be left over sockets (real sockets) from
# gpg-agent. we can conditionally force a remake by marking them phony
# and letting the above rm take care of the dangling socket (or
# soon-to-be dangling on the other side...)
.PHONY: $(shell find $(GPG_SOCKETS) -type s 2>/dev/null)

ifneq (,$(shell which systemctl))
# systemd socket units are named like the actual sockets but with -
# instead of . (gpg-agent-ssh.socket vs gpg-agent.ssh)
SYSTEMD_GPG_SOCKET_UNITS:=$(addsuffix .socket,$(subst .,-,$(GPG_SOCKET_TYPES)))
SYSTEMD_GPG_SOCKET_OVERRIDES:=$(addprefix ../.config/systemd/user/,$(addsuffix .d/socket-path.conf,$(SYSTEMD_GPG_SOCKET_UNITS)))
# map socket override back to gpg's filename
../.config/systemd/user/gpg-agent.socket.d/socket-path.conf: GPG_SOCKET:=S.gpg-agent
../.config/systemd/user/gpg-agent-ssh.socket.d/socket-path.conf: GPG_SOCKET:=S.gpg-agent.ssh
../.config/systemd/user/gpg-agent-browser.socket.d/socket-path.conf: GPG_SOCKET:=S.gpg-agent.browser
../.config/systemd/user/gpg-agent-extra.socket.d/socket-path.conf: GPG_SOCKET:=S.gpg-agent.extra
../.config/systemd/user/dirmngr.socket.d/socket-path.conf: GPG_SOCKET:=S.dirmngr
../.config/systemd/user/keyboxd.socket.d/socket-path.conf: GPG_SOCKET:=S.keyboxd
# we need an absolute path so these can't be symlinked from files
# committed to git
$(SYSTEMD_GPG_SOCKET_OVERRIDES): | $$(@D)/ ../.gnupg/socket
	echo "[Socket]\nListenStream=\nListenStream=$(abspath ../.gnupg/socket)/$(GPG_SOCKET)" > $@
$(dir $(SYSTEMD_GPG_SOCKET_OVERRIDES)): | ../.config/systemd/user
	mkdir $@

../.config/systemd/user/.gpg-override-stamp: $(SYSTEMD_GPG_SOCKET_OVERRIDES)
	systemctl --user daemon-reload
	touch $@
gpg: ../.config/systemd/user/.gpg-override-stamp
endif

# TODO: clean up later
# old symlink. delete it if it exists
ifneq (,$(wildcard ../.gnupg/socketdir))
.PHONY: clean-gpg-socketdir
clean-gpg-socketdir:
	rm -f ../.gnupg/socketdir
gpg: clean-gpg-socketdir
endif
endif

bins: ../bin $(REAL_BINS)

$(REAL_BINS):
	ln -s ../$(BASE)/bin/$(@F) ../bin/$(@F)

dirs: ../bin ../tmp

../bin:
	mkdir -p ../bin

../tmp:
	mkdir -p ../tmp

../.config:
	mkdir $@

../.config/systemd: | ../.config
	mkdir $@

../.config/systemd/user: | ../.config/systemd
	mkdir $@

.PHONY: all git links $(LINKS) authorized_keys ssh_config gpg xmonad gitignore emacs dirs bins $(BINS)

$(LINKS): ../$$@

$(BINS): ../$$@
