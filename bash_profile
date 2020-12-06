# -*- mode:shell-script -*-
# .bash_profile
# The  personal  initialization  file,  executed  for login shells

function set_terminal(){
    tset -Q

    case $TERM in
    xterm*)
        stty erase  2>/dev/null
        ;;
    linux)
        [ $UID -eq 0 ] && loadkeys /root/pjb.kmap
        ;;
    esac
}

if [  ${TRAMP:-no} = yes ] ; then
    return
fi

set_terminal

# Get the aliases and functions
[ -f ~/.bashrc ] && . ~/.bashrc

#### THE END ####

# added by Anaconda3 5.2.0 installer
export PATH="/anaconda3/bin:$PATH"

# added by Anaconda3 5.2.0 installer
export PATH="/opt/anaconda3/bin:$PATH"

# opam configuration
test -r /Users/pjb/.opam/opam-init/init.sh && . /Users/pjb/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
