all:help

SRCDIR=rc

SAVEDIR=old-rc-files

FILES= \
	.Xresources \
	.bash_login \
	.bash_logout \
	.bash_profile \
	.bashrc \
	.bashrc_emacs \
	.clash.lisp \
	.clinit.cl \
	.clisprc.lisp \
	.cmucl-init.lisp \
	.cvspass \
	.cvsrc \
	.eclrc \
	.emacs \
	.emacs-cl.lisp \
	.emacs_bash \
	.gclrc.lisp \
	.geek \
	.gitconfig \
	.irbrc \
	.ircrc \
	.kermrc \
	.plan \
	.profile \
	.sbclrc \
	.screenrc \
	.screenrc-daemon \
	.swank.lisp \
	.wgetrc \
	.xmodmap \
	.ratpoisonrc \
	.abclrc \
	ccl-init.lisp \
	openmcl-init.lisp

help:
	@echo 'make symlinks # makes the symbolic links between ~/$(SRCDIR) and ~/.'

save:
	-@ mkdir -p $$HOME/$(SAVEDIR)
	 @ for f in $(FILES) ; do [ -r $$HOME/$$f -a ! -L $$HOME/$$f ] && mv -iv $$HOME/$$f  $$HOME/$(SAVEDIR)/ || true ; done

symlinks: save
	ln -sf $(SRCDIR)/Xresources                       $$HOME/.Xresources
	ln -sf $(SRCDIR)/bash_login                       $$HOME/.bash_login
	ln -sf $(SRCDIR)/bash_logout                      $$HOME/.bash_logout
	ln -sf $(SRCDIR)/bash_profile                     $$HOME/.bash_profile
	ln -sf $(SRCDIR)/bashrc                           $$HOME/.bashrc
	ln -sf $(SRCDIR)/bashrc_emacs                     $$HOME/.bashrc_emacs
	ln -sf $(SRCDIR)/clash.lisp                       $$HOME/.clash.lisp
	ln -sf $(SRCDIR)/clinit.cl                        $$HOME/.clinit.cl
	ln -sf $(SRCDIR)/clisprc.lisp                     $$HOME/.clisprc.lisp
	ln -sf $(SRCDIR)/cmucl-init.lisp                  $$HOME/.cmucl-init.lisp
	ln -sf $(SRCDIR)/cvspass                          $$HOME/.cvspass
	ln -sf $(SRCDIR)/cvsrc                            $$HOME/.cvsrc
	ln -sf $(SRCDIR)/eclrc.lisp                       $$HOME/.eclrc
	ln -sf $(SRCDIR)/emacs.el                         $$HOME/.emacs
	ln -sf $(SRCDIR)/emacs-cl.lisp                    $$HOME/.emacs-cl.lisp
	ln -sf $(SRCDIR)/emacs-bash                       $$HOME/.emacs_bash
	ln -sf $(SRCDIR)/gclrc.lisp                       $$HOME/.gclrc.lisp
	ln -sf $(SRCDIR)/geek                             $$HOME/.geek
	ln -sf $(SRCDIR)/irbrc                            $$HOME/.irbrc
	ln -sf $(SRCDIR)/ircrc                            $$HOME/.ircrc
	ln -sf $(SRCDIR)/kermrc                           $$HOME/.kermrc
	ln -sf $(SRCDIR)/plan                             $$HOME/.plan
	ln -sf $(SRCDIR)/profile                          $$HOME/.profile
	ln -sf $(SRCDIR)/sbclrc.lisp                      $$HOME/.sbclrc
	ln -sf $(SRCDIR)/screenrc                         $$HOME/.screenrc
	ln -sf $(SRCDIR)/screenrc-daemon                  $$HOME/.screenrc-daemon
	ln -sf $(SRCDIR)/swank.lisp                       $$HOME/.swank.lisp
	ln -sf $(SRCDIR)/wgetrc                           $$HOME/.wgetrc
	ln -sf $(SRCDIR)/xmodmap-daskeyboard-3-evdev      $$HOME/.xmodmap
	ln -sf $(SRCDIR)/ccl-init.lisp                    $$HOME/ccl-init.lisp
	ln -sf $(SRCDIR)/abclrc.lisp                      $$HOME/.abclrc
	ln -sf $(SRCDIR)/openmcl-init.lisp                $$HOME/openmcl-init.lisp
	ln -sf $(SRCDIR)/ratpoisonrc                      $$HOME/.ratpoisonrc

#	ln -sf $(SRCDIR)/gitconfig                        $$HOME/.gitconfig

showlinks:
	cd     $$HOME ; ls -la |awk "/-> $(SRCDIR)/{printf "ln -sf %-30s $$    $$HOME/%s\n",$$11,$$9;}"
