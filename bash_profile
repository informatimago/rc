# .bash_profile
# -*- Mode: shell-script -*-
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
# Your previous /Users/pjb/.bash_profile file was backed up as /Users/pjb/.bash_profile.macports-saved_2013-08-25_at_03:55:34
##

# MacPorts Installer addition on 2013-08-25_at_03:55:34: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

