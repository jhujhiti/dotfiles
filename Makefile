LINKS?=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .kernel-pkg.conf \
      .zprofile .zshenv .zshrc .zsh .Xdefaults .tmux.conf .termcap .mailcap \
      .xsession .nethackrc
BASE?=$(shell basename `pwd`)

REAL_LINKS=$(addprefix ../,$(LINKS))

BINS=$(shell ls bin)

REAL_BINS=$(addprefix ../bin/,$(BINS))

all: git links dirs bins

links: $(LINKS) authorized_keys ssh_config gpg xmonad gitignore

git:
	git submodule init
	git submodule sync
	git submodule update
	git status submodules/ --porcelain=v1 | grep '^?? ' | cut -d' ' -f2- | xargs rm -rf

$(REAL_LINKS):
	ln -s $(BASE)/$(@F) ../$(@F)

gitignore:
	ln -s ../$(BASE)/.gitignore ../.gitignore

xmonad: ../.xmonad ../.xmonad/xmonad.hs ../.config ../.config/xmobar ../.config/xmobar/xmobarrc

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

authorized_keys: ../.ssh ../.ssh/authorized_keys

ssh_config: ../.ssh ../.ssh/config

../.ssh/authorized_keys:
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys

../.ssh/config:
	ln -s ../$(BASE)/ssh_config ../.ssh/config

../.ssh:
	mkdir -p -m 700 ../.ssh

gpg: ../.gnupg ../.gnupg/gpg.conf ../.gnupg/gpg-agent.conf

../.gnupg/gpg.conf:
	ln -s ../$(BASE)/gpg.conf ../.gnupg/gpg.conf

../.gnupg/gpg-agent.conf:
	ln -s ../$(BASE)/gpg-agent.conf ../.gnupg/gpg-agent.conf

../.gnupg:
	mkdir -p ../.gnupg

bins: ../bin $(REAL_BINS)

$(REAL_BINS):
	ln -s ../$(BASE)/bin/$(@F) ../bin/$(@F)

dirs: ../bin ../tmp

../bin:
	mkdir -p ../bin

../tmp:
	mkdir -p ../tmp

.SECONDEXPANSION:

.PHONY: all git links $(LINKS) authorized_keys ssh_config gpg xmonad dirs bins $(BINS)

$(LINKS): ../$$@

$(BINS): ../$$@
