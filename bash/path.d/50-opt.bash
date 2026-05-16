#!/bin/bash
# 50-opt.bash -- /opt and ~/opt user-installed trees.
#
# These are individually-installed tools (Postgres, JDKs, vendored
# toolchains).  After user/host/msystem so they don't shadow more
# specific picks, but before /usr/bin so the user-installed version
# wins over distro defaults.

path_add /usr/local/sbin /usr/local/bin

# ~/opt/<distrib>/bin -- the legacy be_generate logic, simplified.
if [ -x "$HOME/bin/distribution" ] ; then
    _pjb_distrib="$("$HOME/bin/distribution" -i 2>/dev/null)"
    [ -n "$_pjb_distrib" ] && path_add "$HOME/opt/$_pjb_distrib/bin"
    unset _pjb_distrib
fi

# /opt single-version trees -- enumerate explicitly, not by glob,
# so the ordering is deterministic.
path_add /opt/local/sbin
path_add /opt/local/bin
path_add /opt/X11/bin
