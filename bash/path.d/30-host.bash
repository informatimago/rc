#!/bin/bash
# 30-host.bash -- per-host overlay.
#
# Source ~/.config/host-path if present; that file is the host's
# private list of `path_add` / `env_set` calls (paths for a Java
# install on one machine, a vendored toolchain on another, etc.).
# Kept out of the repo so secrets-y paths stay local.
#
# Also delegate to a hostname-specific layer file inside the repo,
# so committed per-host setup has a clear home.

if [ -r "$HOME/.config/host-path" ] ; then
    source "$HOME/.config/host-path"
fi

# pjb_bash_profile_name is defined in bash/lib/context.bash and
# returns something like "host-sncf-reseau"; reuse it here.
if declare -F pjb_bash_profile_name >/dev/null ; then
    _pjb_host="$(pjb_bash_profile_name)"
    if [ -r "$PJB_BASH_RC_ROOT/bash/path.d/host/${_pjb_host}.bash" ] ; then
        source "$PJB_BASH_RC_ROOT/bash/path.d/host/${_pjb_host}.bash"
    fi
    unset _pjb_host
fi
