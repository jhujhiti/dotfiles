LINKS=.gitconfig .inputrc .screenrc .vim .vimrc .muttrc .kernel-pkg.conf
BASE=$(shell basename `pwd`)
setup:
	git submodule init
	git submodule update
	git submodule foreach git submodule init
	git submodule foreach git submodule update
	rm -f ../.bash_profile ../.bashrc
	ln -s $(BASE)/.bash_profile ../.bash_profile
	ln -s $(BASE)/.bash_profile ../.bashrc
	for file in $(LINKS) ; do \
	    ln -s $(BASE)/$$file ../$$file; \
	    done
	mkdir -p ../.ssh ../.gnupg
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys
	ln -s ../$(BASE)/gpg.conf ../.gnupg/gpg.conf

x11: setup
	ln -s $(BASE)/.Xdefaults ../.Xdefaults

all: setup
