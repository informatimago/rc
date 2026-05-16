#!/bin/bash

function pjb_bash_source_if_readable(){
    local path="$1"
    [ -r "$path" ] && source "$path"
}

function pjb_bash_interactive_p(){
    [[ $- == *i* ]]
}

function pjb_bash_has_tty_p(){
    [ -t 0 ] || [ -t 1 ] || [ -t 2 ]
}

function pjb_bash_compute_rc_root(){
    local source_path
    source_path="${BASH_SOURCE[0]}"
    cd "$(dirname "$source_path")/../.." && pwd -P
}

if [ -z "${PJB_BASH_RC_ROOT:-}" ] ; then
    export PJB_BASH_RC_ROOT="$(pjb_bash_compute_rc_root)"
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

if [ -z "${PJB_BASH_ENV_FILE:-}" ] ; then
    export PJB_BASH_ENV_FILE="$PJB_BASH_RC_ROOT/bash/env/noninteractive.bash"
fi

if [ -z "${PJB_BASH_ENV_CACHE_KEY:-}" ] ; then
    # On MSYS2, $MSYSTEM (MSYS, MINGW64, UCRT64, CLANG64, CLANGARM64)
    # disambiguates environments that all report the same `uname -s`.
    # Elsewhere it is unset, so we fall back to the OS name.
    export PJB_BASH_ENV_CACHE_KEY="${MSYSTEM:-$PJB_BASH_OS}"
fi

if [ -z "${PJB_BASH_ENV_CACHE_FILE:-}" ] ; then
    export PJB_BASH_ENV_CACHE_FILE="$PJB_BASH_CACHE_DIR/env-${PJB_BASH_HOSTNAME}-${PJB_BASH_ENV_CACHE_KEY}.bash"
fi

function pjb_bash_profile_name(){
    case "$PJB_BASH_HOSTNAME" in
        larissa*)          printf '%s\n' host-larissa ;;
        PF5S26BT)          printf '%s\n' host-sncf-reseau ;;
        fr*)               printf '%s\n' host-harman ;;
        vm-u1404|L0253344) printf '%s\n' host-span ;;
        *trustonic.local)  printf '%s\n' host-trustonic ;;
        *)                 printf '%s\n' host-default ;;
    esac
}
