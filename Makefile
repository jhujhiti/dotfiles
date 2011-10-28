LINKS=.gitconfig .inputrc .screenrc .vim .vimrc
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
	mkdir -p ../.ssh
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys

x11: setup
	ln -s $(BASE)/.gtkrc-2.0 ../.gtkrc-2.0
	ln -s $(BASE)/.Xdefaults ../.Xdefaults

all: setup
