# -*- mode:shell-script;coding:iso-8859-1 -*-
#
# .profile is read for all login shells

# sh login shell startup file.

# all other interactive shells will read .bashrc
# So read .bashrc also from .profile and make all changes to .bashrc.
# Then you should always have your correct setup.

# This file is not used by bash when .bash_profile or .bash_login exist.

# test -z "$PROFILEREAD" && [ -r /etc/profile ] && . /etc/profile


if test -f "$HOME/.bashrc" ; then
    echo "Sourcing $HOME/.bashrc from $HOME/.profile"
	source "$HOME/.bashrc"
fi


# if [ -n "$DISPLAY" ] ; then
#     /usr/X11R6/bin/xhost \
#         +local: \
#         +thalassa.informatimago.com \
#         +naiad.informatimago.com \
#         +larissa.informatimago.com
# #
# #        +galatea.informatimago.com \
# #        +despina.informatimago.com \
# #        +nereide.informatimago.com \
# #
# fi



#
# some people don't like fortune.  If you have humor, please enable it by
# uncommenting the following lines.
#

#if [ -x /usr/bin/fortune ] ; then
#    echo
#    /usr/bin/fortune
#    echo
#fi

#### THE END ####

