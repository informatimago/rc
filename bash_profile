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

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
