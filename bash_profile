# -*- mode:shell-script -*-
# .bash_profile
# The  personal  initialization  file,  executed  for login shells

function set_terminal(){
    if [ "$TERM" != emacs ] ; then

        tset -Q -c
        case $TERM in
        xterm*)
            stty erase  2>/dev/null
            ;;
        linux)
            [ $UID -eq 0 ] && loadkeys /root/pjb.kmap
            ;;
        esac

    fi
}

set_terminal

# Get the aliases and functions
[ -f ~/.bashrc ] && . ~/.bashrc

# added by Anaconda3 5.2.0 installer
export PATH="/opt/anaconda3/bin:$PATH"

# opam configuration
test -r /Users/pjb/.opam/opam-init/init.sh && . /Users/pjb/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

#### THE END ####

##
# Your previous /Users/pjb/.bash_profile file was backed up as /Users/pjb/.bash_profile.macports-saved_2025-11-14_at_20:58:16
##

# MacPorts Installer addition on 2025-11-14_at_20:58:16: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

