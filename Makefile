LINKS=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .kernel-pkg.conf \
      .zshenv .zshrc .zsh
BASE=$(shell basename `pwd`)
setup:
	git submodule init
	git submodule update
	git submodule foreach git submodule init
	git submodule foreach git submodule update
	for file in $(LINKS) ; do \
	    rm -f ../$$file ; \
	    ln -s $(BASE)/$$file ../$$file; \
	    done
	mkdir -p ../.ssh ../.gnupg
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys
	ln -s ../$(BASE)/gpg.conf ../.gnupg/gpg.conf

x11: setup
	ln -s $(BASE)/.Xdefaults ../.Xdefaults

all: setup
