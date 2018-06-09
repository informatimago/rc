# -*- mode:shell-script -*-
# .bash_profile
# The  personal  initialization  file,  executed  for login shells

tset -Q

case $TERM in
xterm*)
    stty erase  2>/dev/null
    ;;
linux)
    [ $UID -eq 0 ] && loadkeys /root/pjb.kmap
    ;;
esac

# Get the aliases and functions
[ -f ~/.bashrc ] && . ~/.bashrc

#### THE END ####

# added by Anaconda3 5.2.0 installer
export PATH="/anaconda3/bin:$PATH"

# added by Anaconda3 5.2.0 installer
export PATH="/opt/anaconda3/bin:$PATH"
