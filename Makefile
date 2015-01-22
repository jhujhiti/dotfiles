LINKS?=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .kernel-pkg.conf \
      .zshenv .zshrc .zsh .Xdefaults .tmux.conf .termcap
BASE?=$(shell basename `pwd`)

REAL_LINKS=$(addprefix ../,$(LINKS))

all: git links

links: $(LINKS) authorized_keys ssh_config gpg.conf

git:
	git submodule init
	git submodule sync
	git submodule update

$(REAL_LINKS):
	ln -s $(BASE)/$(@F) ../$(@F)

authorized_keys: ../.ssh ../.ssh/authorized_keys

ssh_config: ../.ssh ../.ssh/config

../.ssh/authorized_keys:
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys

../.ssh/config:
	ln -s ../$(BASE)/ssh_config ../.ssh/config

../.ssh:
	mkdir -p -m 700 ../.ssh

gpg.conf: ../.gnupg ../.gnupg/gpg.conf

../.gnupg/gpg.conf:
	ln -s ../$(BASE)/gpg.conf ../.gnupg/gpg.conf

../.gnupg:
	mkdir -p ../.gnupg

.SECONDEXPANSION:

.PHONY: all git links $(LINKS) authorized_keys ssh_config gpg.conf

$(LINKS): ../$$@
