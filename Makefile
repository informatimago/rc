all:help
help:
	@echo 'make symlinks # makes the symbolic links between ~/rc and ~/.'
save:
	- mkdir -p $$HOME/old-dotfiles
	- [ -r $$HOME/.Xresources         -a ! -L $$HOME/.Xresources         ] && mv -iv $$HOME/.Xresources          $$HOME/old-dotfiles/
	- [ -r $$HOME/.bash_env           -a ! -L $$HOME/.bash_env           ] && mv -iv $$HOME/.bash_env            $$HOME/old-dotfiles/
	- [ -r $$HOME/.bash_login         -a ! -L $$HOME/.bash_login         ] && mv -iv $$HOME/.bash_login          $$HOME/old-dotfiles/
	- [ -r $$HOME/.bash_logout        -a ! -L $$HOME/.bash_logout        ] && mv -iv $$HOME/.bash_logout         $$HOME/old-dotfiles/
	- [ -r $$HOME/.bash_profile       -a ! -L $$HOME/.bash_profile       ] && mv -iv $$HOME/.bash_profile        $$HOME/old-dotfiles/
	- [ -r $$HOME/.bashrc             -a ! -L $$HOME/.bashrc             ] && mv -iv $$HOME/.bashrc              $$HOME/old-dotfiles/
	- [ -r $$HOME/.bashrc_emacs       -a ! -L $$HOME/.bashrc_emacs       ] && mv -iv $$HOME/.bashrc_emacs        $$HOME/old-dotfiles/
	- [ -r $$HOME/.clash.lisp         -a ! -L $$HOME/.clash.lisp         ] && mv -iv $$HOME/.clash.lisp          $$HOME/old-dotfiles/
	- [ -r $$HOME/.clinit.cl          -a ! -L $$HOME/.clinit.cl          ] && mv -iv $$HOME/.clinit.cl           $$HOME/old-dotfiles/
	- [ -r $$HOME/.clisprc.lisp       -a ! -L $$HOME/.clisprc.lisp       ] && mv -iv $$HOME/.clisprc.lisp        $$HOME/old-dotfiles/
	- [ -r $$HOME/.cmucl-init.lisp    -a ! -L $$HOME/.cmucl-init.lisp    ] && mv -iv $$HOME/.cmucl-init.lisp     $$HOME/old-dotfiles/
	- [ -r $$HOME/.cvspass            -a ! -L $$HOME/.cvspass            ] && mv -iv $$HOME/.cvspass             $$HOME/old-dotfiles/
	- [ -r $$HOME/.cvsrc              -a ! -L $$HOME/.cvsrc              ] && mv -iv $$HOME/.cvsrc               $$HOME/old-dotfiles/
	- [ -r $$HOME/.eclrc              -a ! -L $$HOME/.eclrc              ] && mv -iv $$HOME/.eclrc               $$HOME/old-dotfiles/
	- [ -r $$HOME/.emacs              -a ! -L $$HOME/.emacs              ] && mv -iv $$HOME/.emacs               $$HOME/old-dotfiles/
	- [ -r $$HOME/.emacs-cl.lisp      -a ! -L $$HOME/.emacs-cl.lisp      ] && mv -iv $$HOME/.emacs-cl.lisp       $$HOME/old-dotfiles/
	- [ -r $$HOME/.emacs_bash         -a ! -L $$HOME/.emacs_bash         ] && mv -iv $$HOME/.emacs_bash          $$HOME/old-dotfiles/
	- [ -r $$HOME/.gclrc.lisp         -a ! -L $$HOME/.gclrc.lisp         ] && mv -iv $$HOME/.gclrc.lisp          $$HOME/old-dotfiles/
	- [ -r $$HOME/.geek               -a ! -L $$HOME/.geek               ] && mv -iv $$HOME/.geek                $$HOME/old-dotfiles/
	- [ -r $$HOME/.irbrc              -a ! -L $$HOME/.irbrc              ] && mv -iv $$HOME/.irbrc               $$HOME/old-dotfiles/
	- [ -r $$HOME/.ircrc              -a ! -L $$HOME/.ircrc              ] && mv -iv $$HOME/.ircrc               $$HOME/old-dotfiles/
	- [ -r $$HOME/.kermrc             -a ! -L $$HOME/.kermrc             ] && mv -iv $$HOME/.kermrc              $$HOME/old-dotfiles/
	- [ -r $$HOME/.plan               -a ! -L $$HOME/.plan               ] && mv -iv $$HOME/.plan                $$HOME/old-dotfiles/
	- [ -r $$HOME/.profile            -a ! -L $$HOME/.profile            ] && mv -iv $$HOME/.profile             $$HOME/old-dotfiles/
	- [ -r $$HOME/.sbclrc             -a ! -L $$HOME/.sbclrc             ] && mv -iv $$HOME/.sbclrc              $$HOME/old-dotfiles/
	- [ -r $$HOME/.screenrc           -a ! -L $$HOME/.screenrc           ] && mv -iv $$HOME/.screenrc            $$HOME/old-dotfiles/
	- [ -r $$HOME/.screenrc-daemon    -a ! -L $$HOME/.screenrc-daemon    ] && mv -iv $$HOME/.screenrc-daemon     $$HOME/old-dotfiles/
	- [ -r $$HOME/.swank.lisp         -a ! -L $$HOME/.swank.lisp         ] && mv -iv $$HOME/.swank.lisp          $$HOME/old-dotfiles/
	- [ -r $$HOME/.wgetrc             -a ! -L $$HOME/.wgetrc             ] && mv -iv $$HOME/.wgetrc              $$HOME/old-dotfiles/
	- [ -r $$HOME/.xmodmap            -a ! -L $$HOME/.xmodmap            ] && mv -iv $$HOME/.xmodmap             $$HOME/old-dotfiles/
	- [ -r $$HOME/clisprc.lisp        -a ! -L $$HOME/clisprc.lisp        ] && mv -iv $$HOME/clisprc.lisp         $$HOME/old-dotfiles/
	- [ -r $$HOME/openmcl-init.lisp   -a ! -L $$HOME/openmcl-init.lisp   ] && mv -iv $$HOME/openmcl-init.lisp    $$HOME/old-dotfiles/

symlinks: save
	ln -sf rc/Xresources                  $$HOME/.Xresources
	ln -sf rc/bash_env                    $$HOME/.bash_env
	ln -sf rc/bash_login                  $$HOME/.bash_login
	ln -sf rc/bash_logout                 $$HOME/.bash_logout
	ln -sf rc/bash_profile                $$HOME/.bash_profile
	ln -sf rc/bashrc                      $$HOME/.bashrc
	ln -sf rc/bashrc_emacs                $$HOME/.bashrc_emacs
	ln -sf rc/clash.lisp                  $$HOME/.clash.lisp
	ln -sf rc/clinit.cl                   $$HOME/.clinit.cl
	ln -sf rc/clisprc.lisp                $$HOME/.clisprc.lisp
	ln -sf rc/cmucl-init.lisp             $$HOME/.cmucl-init.lisp
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
