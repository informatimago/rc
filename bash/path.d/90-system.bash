#!/bin/bash
# 90-system.bash -- system defaults, lowest priority.
#
# These come last so any of the layers above can shadow them.  Kept
# minimal: just the four POSIX-ish dirs plus sbin counterparts.

path_add /usr/bin
path_add /bin
path_add /usr/sbin
path_add /sbin

manpath_add /usr/share/man /usr/man
infopath_add /usr/share/info
