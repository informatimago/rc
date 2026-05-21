#!/bin/bash
# path-compose.bash -- declarative PATH composition.
#
# SKETCH (not wired into bashrc-engine yet).
#
# Why
# ----
# The current rc/bash code reads $PATH and prepends to it, which means
# every shell launch piles new dirs on top of whatever the previous one
# left behind.  Across MSYSTEMs (UCRT64 vs MINGW64 vs MSYS) this leaks
# the wrong toolchain into the wrong env; across projects it forces
# either global "pyenv shim everything" hacks or per-shell discipline.
#
# Model
# -----
# PATH is rebuilt from scratch from a stack of *layers*.  Each layer
# is one file in $PJB_BASH_RC_ROOT/bash/path.d/ named NN-name.bash and
# sourced in lexical order.  Within a layer, dirs declared earlier
# come first in PATH.  Files with a lower NN sit closer to the front
# of PATH (higher priority).
#
# Conventional numbering:
#
#   10-project.bash    project-local overlay (.bash-path walked up from $PWD)
#   20-user.bash       ~/bin, ~/.local/bin
#   30-host.bash       per-host overrides
#   40-msystem.bash    MSYS2 toolchain bin for the active MSYSTEM
#   50-opt.bash        ~/opt/* and /opt/* trees
#   90-system.bash     /usr/local/bin, /usr/bin, /bin, ...
#
# Each layer file is *pure*: it only calls `path_add`, `manpath_add`,
# `infopath_add`, `env_set`.  No prepending, no reading $PATH, no test
# for "is this already in PATH" -- the composer dedupes globally.
# A dir is included only if it actually exists on disk.
#
# Usage from the engine
# ---------------------
#   source $PJB_BASH_RC_ROOT/bash/lib/path-compose.bash
#   path_compose       # rebuilds PATH/MANPATH/INFOPATH from path.d/*
#
# Re-run path_compose after `cd` (via PROMPT_COMMAND, optional) to
# pick up project layer changes.  In the cache-snapshot writer, run
# path_compose once before serialising and the cache will hold the
# composed PATH -- no `declare -px PATH` override gymnastics needed.

# -- state ---------------------------------------------------------------

case "$BASH_VERSION" in
([123].*)
    declare -a PJB_PATH_BUF
    declare -a PJB_MANPATH_BUF
    declare -a PJB_INFOPATH_BUF
    ;;
(*)    
    declare -ga PJB_PATH_BUF
    declare -ga PJB_MANPATH_BUF
    declare -ga PJB_INFOPATH_BUF
    ;;
esac

# -- layer-facing API ----------------------------------------------------

# path_add DIR [DIR...]
# Append each existing DIR to the current composition buffer.
# Non-existent dirs are silently dropped.  Dedupe happens at compose time.
path_add(){
    local d
    for d in "$@" ; do
        [ -d "$d" ] && PJB_PATH_BUF+=("$d")
    done
}

manpath_add(){
    local d
    for d in "$@" ; do
        [ -d "$d" ] && PJB_MANPATH_BUF+=("$d")
    done
}

infopath_add(){
    local d
    for d in "$@" ; do
        [ -d "$d" ] && PJB_INFOPATH_BUF+=("$d")
    done
}

# env_set NAME VALUE
# Plain `export NAME=VALUE`.  Lives next to path_add so a layer that
# pins a toolchain (e.g. MINGW_PREFIX=/ucrt64) reads cleanly.
env_set(){
    export "$1=$2"
}

# -- composer ------------------------------------------------------------

# _pjb_join_dedup SEP ARR...
# echo a SEP-joined string of ARR, preserving first-occurrence order.
_pjb_join_dedup(){
    local sep="$1" ; shift
    local seen=() out=() x
    for x in "$@" ; do
        case " ${seen[*]} " in
            *" $x "*) : ;;
            *) seen+=("$x") ; out+=("$x") ;;
        esac
    done
    local IFS="$sep"
    printf '%s' "${out[*]}"
}

# path_compose
# Source every $PJB_BASH_RC_ROOT/bash/path.d/[0-9]*.bash in lexical
# order, then assemble PATH / MANPATH / INFOPATH from the buffers.
path_compose(){
    PJB_PATH_BUF=()
    PJB_MANPATH_BUF=()
    PJB_INFOPATH_BUF=()

    local f
    for f in "$PJB_BASH_RC_ROOT"/bash/path.d/[0-9]*.bash ; do
        [ -r "$f" ] && source "$f"
    done

    PATH="$(_pjb_join_dedup ':' "${PJB_PATH_BUF[@]}")"
    export PATH
    if [ "${#PJB_MANPATH_BUF[@]}" -gt 0 ] ; then
        MANPATH="$(_pjb_join_dedup ':' "${PJB_MANPATH_BUF[@]}")"
        export MANPATH
    fi
    if [ "${#PJB_INFOPATH_BUF[@]}" -gt 0 ] ; then
        INFOPATH="$(_pjb_join_dedup ':' "${PJB_INFOPATH_BUF[@]}")"
        export INFOPATH
    fi
}
