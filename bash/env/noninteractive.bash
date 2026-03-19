#!/bin/bash

if [ -z "${PJB_BASH_RC_ROOT:-}" ] ; then
    export PJB_BASH_RC_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
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

if [ -z "${PJB_BASH_ENV_CACHE_FILE:-}" ] ; then
    export PJB_BASH_ENV_CACHE_FILE="$PJB_BASH_CACHE_DIR/env-${PJB_BASH_HOSTNAME}-${PJB_BASH_OS}.bash"
fi

[ -r "$PJB_BASH_ENV_CACHE_FILE" ] && source "$PJB_BASH_ENV_CACHE_FILE"
