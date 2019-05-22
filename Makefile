LINKS?=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .kernel-pkg.conf \
      .zprofile .zshenv .zshrc .zsh .Xdefaults .tmux.conf .termcap .mailcap \
      .xsession .nethackrc
BASE?=$(shell basename `pwd`)

REAL_LINKS=$(addprefix ../,$(LINKS))

BINS=$(shell ls bin)

REAL_BINS=$(addprefix ../bin/,$(BINS))

# directories to create in our home directory
DIRS?=bin tmp .emacs.d .config .xmonad .gnupg

all: git links $(REAL_BINS) ../tmp

links: $(LINKS) authorized_keys ssh_config gpg xmonad ../.gitignore emacs

git:
	git submodule init
	git submodule sync
	git submodule update
	git status submodules/ --porcelain=v1 | grep '^?? ' | cut -d' ' -f2- | xargs rm -rf

$(REAL_LINKS):
	ln -s $(BASE)/$(@F) ../$(@F)

emacs: ../.emacs.d/init.el

../.emacs.d/init.el: | $(@D)
	ln -s ../$(BASE)/emacs/init.el $@

../.gitignore:
	ln -s $(BASE)/global.gitignore $@

xmonad: ../.xmonad/xmonad.hs ../.config/xmobar/xmobarrc

../.xmonad/xmonad.hs: | $(@D)
	ln -s ../$(BASE)/xmonad/xmonad.hs $@

../.config/xmobar: | $(@D)
	mkdir -p $@

../.config/xmobar/xmobarrc: | $(@D)
	ln -s ../../$(BASE)/xmonad/xmobarrc $@

authorized_keys: ../.ssh/authorized_keys

ssh_config: ../.ssh/config

../.ssh/authorized_keys: | $(@D)
	ln -s ../$(BASE)/authorized_keys $@

../.ssh/config: | $(@D)
	ln -s ../$(BASE)/ssh_config $@

../.ssh:
	mkdir -p -m 700 $@

gpg: ../.gnupg/gpg.conf ../.gnupg/gpg-agent.conf

../.gnupg/gpg.conf: | $(@D)
	ln -s ../$(BASE)/gpg.conf $@

../.gnupg/gpg-agent.conf: | $(@D)
	ln -s ../$(BASE)/gpg-agent.conf $@

$(REAL_BINS):
	ln -s ../$(BASE)/bin/$(@F) ../bin/$(@F)

$(addprefix ../,$(DIRS)):
	mkdir -p $@

.SECONDEXPANSION:

.PHONY: all git links $(LINKS) authorized_keys ssh_config gpg xmonad emacs $(BINS)

$(LINKS): ../$$@

$(BINS): ../$$@
