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

##
# Your previous /Users/pjb/.bash_profile file was backed up as /Users/pjb/.bash_profile.macports-saved_2016-10-16_at_16:35:49
##

# MacPorts Installer addition on 2016-10-16_at_16:35:49: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

