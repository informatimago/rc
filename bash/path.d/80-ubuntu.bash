#!/bin/bash
# 80-ubuntu.bash -- ubuntu paths.
#
# These come before last so any of the layers above can shadow them.  Kept
# minimal: just the four POSIX-ish dirs plus sbin counterparts.

path_add /snap/bin

# manpath_add /usr/share/man /usr/man
# infopath_add /usr/share/info
