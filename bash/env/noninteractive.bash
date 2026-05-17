#!/bin/bash

if [ -z "${PJB_BASH_RC_ROOT:-}" ] ; then
    export PJB_BASH_RC_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
fi

# Defensive: on MSYS2 a non-interactive shell that never went through
# /etc/profile will be missing MINGW_PREFIX, ACLOCAL_PATH, etc, and the
# cache key would silently degrade. MSYSTEM itself is exported by the
# MSYS2 launcher so the key still resolves; the guard is a no-op when
# /etc/profile has already run.
if [ -z "${MINGW_PREFIX:-}" ] && [ -r /etc/profile ] && [ -n "${MSYSTEM:-}" ] ; then
    . /etc/profile
fi

if [ -z "${PJB_BASH_OS:-}" ] ; then
    export PJB_BASH_OS="$(uname -s 2>/dev/null || echo unknown)"
fi

if [ -z "${PJB_BASH_HOSTNAME:-}" ] ; then
    if [ -r "$HOME/.config/host" ] ; then
        export PJB_BASH_HOSTNAME="$(cat "$HOME/.config/host")"
    else
        export PJB_BASH_HOSTNAME="$(hostname -f 2>/dev/null || hostname 2>/dev/null || echo localhost)"
    fi
fi

if [ -z "${PJB_BASH_CACHE_DIR:-}" ] ; then
    export PJB_BASH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/bash"
fi

if [ -z "${PJB_BASH_ENV_CACHE_KEY:-}" ] ; then
    export PJB_BASH_ENV_CACHE_KEY="${MSYSTEM:-$PJB_BASH_OS}"
fi

if [ -z "${PJB_BASH_ENV_CACHE_FILE:-}" ] ; then
    export PJB_BASH_ENV_CACHE_FILE="$PJB_BASH_CACHE_DIR/env-${PJB_BASH_HOSTNAME}-${PJB_BASH_ENV_CACHE_KEY}.bash"
fi

[ -r "$PJB_BASH_ENV_CACHE_FILE" ] && source "$PJB_BASH_ENV_CACHE_FILE"

# Recompose PATH/MANPATH/INFOPATH from the path.d/ layers so a
# non-interactive shell launched inside a project dir (`bash -c ...`,
# makepkg, etc.) picks up the project's .bash-path overlay instead of
# being stuck with whatever cwd built the cache.
if [ -r "$PJB_BASH_RC_ROOT/bash/lib/path-compose.bash" ] ; then
    source "$PJB_BASH_RC_ROOT/bash/lib/path-compose.bash"
    path_compose
fi
