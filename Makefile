LINKS?=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .zprofile \
      .zshenv .zshrc .zsh .Xdefaults .tmux.conf .termcap .mailcap \
      .xsession .nethackrc
BASE?=$(shell basename `pwd`)

REAL_LINKS=$(addprefix ../,$(LINKS))

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

xmonad: ../.xmonad ../.xmonad/xmonad.hs ../.config ../.config/xmobar ../.config/xmobar/xmobarrc ../.xmobar

../.xmonad:
	mkdir -p ../.xmonad

../.xmonad/xmonad.hs:
	ln -s ../$(BASE)/xmonad/xmonad.hs ../.xmonad/xmonad.hs

../.config:
	mkdir -p ../.config

../.config/xmobar:
	mkdir -p ../.config/xmobar

../.config/xmobar/xmobarrc:
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
../.gnupg/socketdir: ../.gnupg
	ln -s $$(gpgconf -q --list-dir socketdir) $@

gpg: ../.gnupg/socketdir
endif

bins: ../bin $(REAL_BINS)

$(REAL_BINS):
	ln -s ../$(BASE)/bin/$(@F) ../bin/$(@F)

dirs: ../bin ../tmp

../bin:
	mkdir -p ../bin

../tmp:
	mkdir -p ../tmp

.SECONDEXPANSION:

.PHONY: all git links $(LINKS) authorized_keys ssh_config gpg xmonad gitignore emacs dirs bins $(BINS)

$(LINKS): ../$$@

$(BINS): ../$$@
