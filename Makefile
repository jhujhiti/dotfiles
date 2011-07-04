LINKS=.gitconfig .inputrc .screenrc .vim .vimrc
BASE=$(shell basename `pwd`)
setup:
	rm -f ../.bash_profile ../.bashrc
	ln -s $(BASE)/.bash_profile ../.bash_profile
	ln -s $(BASE)/.bash_profile ../.bashrc
	for file in $(LINKS) ; do \
	    ln -s $(BASE)/$$file ../$$file; \
	    done
	mkdir -p ../.ssh
	ln -s ../$(BASE)/authorized_keys ../.ssh/authorized_keys

all: setup
