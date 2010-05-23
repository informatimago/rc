all:help
help:
	@echo 'make symlinks # makes the symbolic links between ~/rc and ~/.'
symlinks:
	ln -sf rc/Xresources                  $$HOME/.Xresources
	ln -sf rc/bash_env                    $$HOME/.bash_env
	ln -sf rc/bash_login                  $$HOME/.bash_login
	ln -sf rc/bash_logout                 $$HOME/.bash_logout
	ln -sf rc/bash_profile                $$HOME/.bash_profile
	ln -sf rc/bashrc                      $$HOME/.bashrc
	ln -sf rc/bashrc_emacs                $$HOME/.bashrc_emacs
	ln -sf rc/.clash.lisp                 $$HOME/.clash.lisp
	ln -sf rc/.clinit.cl                  $$HOME/.clinit.cl
	ln -sf rc/.clisprc.lisp               $$HOME/.clisprc.lisp
	ln -sf rc/.cmucl-init.lisp            $$HOME/.cmucl-init.lisp
	ln -sf rc/cvspass                     $$HOME/.cvspass
	ln -sf rc/cvsrc                       $$HOME/.cvsrc
	ln -sf rc/eclrc.lisp                  $$HOME/.eclrc
	ln -sf rc/emacs.el                    $$HOME/.emacs
	ln -sf rc/emacs-cl.lisp               $$HOME/.emacs-cl.lisp
	ln -sf rc/emacs-bash                  $$HOME/.emacs_bash
	ln -sf rc/gclrc.lisp                  $$HOME/.gclrc.lisp
	ln -sf rc/geek                        $$HOME/.geek
	ln -sf rc/irbrc                       $$HOME/.irbrc
	ln -sf rc/ircrc                       $$HOME/.ircrc
	ln -sf rc/kermrc                      $$HOME/.kermrc
	ln -sf rc/plan                        $$HOME/.plan
	ln -sf rc/profile                     $$HOME/.profile
	ln -sf rc/sbclrc.lisp                 $$HOME/.sbclrc
	ln -sf rc/screenrc                    $$HOME/.screenrc
	ln -sf rc/screenrc-daemon             $$HOME/.screenrc-daemon
	ln -sf rc/swank.lisp                  $$HOME/.swank.lisp
	ln -sf rc/wgetrc                      $$HOME/.wgetrc
	ln -sf rc/xmodmap-daskeyboard-3       $$HOME/.xmodmap
	ln -sf rc/clisprc.lisp                $$HOME/clisprc.lisp
	ln -sf rc/openmcl-init.lisp           $$HOME/openmcl-init.lisp

showlinks:
	cd $$HOME ; ls -la |awk '/-> rc/{printf "ln -sf %-30s $$$$HOME/%s\n",$$11,$$9;}'
