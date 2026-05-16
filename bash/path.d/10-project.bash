#!/bin/bash
# 10-project.bash -- project-local PATH overlay (highest priority).
#
# Walk up from $PWD looking for a `.bash-path` file.  Each file we
# find is sourced (closest-to-$PWD first), giving project-tree
# overlays.  A `.bash-path` is itself a sequence of `path_add ...`
# calls so a project can pin a specific toolchain version without
# touching $PATH directly:
#
#     # ~/src/embedded-foo/.bash-path
#     path_add  /opt/xtensa-esp32-elf-12/bin
#     env_set   IDF_PATH "$HOME/esp/esp-idf-v5.2"
#     BASH_PATH_STOP=1      # this is the project root -- don't inherit from above
#
# Stop conditions
# ---------------
# The walk stops climbing when one of these is true after handling
# the current directory:
#
#   (a) the sourced `.bash-path` set BASH_PATH_STOP (explicit marker
#       a project root commits to scope inheritance);
#
#   (b) the current directory is in $BASH_PATH_CUTOFF -- a list of
#       boundary directories above which we never read.  "Home" is
#       multiple on Windows + MSYS2 (HOME may point to AppData/Roaming,
#       /home/$USER is a figment maintained by MSYS2 over the real
#       c:/msys64/home/$USER, the file-browser home is c:/Users/$USER,
#       AppData can be redirected to a network share, etc.) so the
#       default list enumerates the usual candidates;
#
#   (c) the current directory contains one of $BASH_PATH_PROJECT_MARKERS
#       (.git, .hg, .svn by default).  This makes every git/hg/svn
#       checkout an implicit cutoff, so a submodule does not silently
#       inherit a `.bash-path` from a sibling repo above.  Override
#       with an explicit cutoff or by clearing the array.
#
# Configuration
# -------------
# Either of $BASH_PATH_CUTOFF / $BASH_PATH_PROJECT_MARKERS can be set
# before this file is sourced; the defaults below only fire when the
# variable is unset (not when it is set to an empty array, so you
# can explicitly disable a category with `BASH_PATH_PROJECT_MARKERS=()`).

# --- defaults ------------------------------------------------------------

if [ -z "${BASH_PATH_CUTOFF+x}" ] ; then
    _u="${USER:-${USERNAME:-}}"
    BASH_PATH_CUTOFF=(
        "$HOME"
        # MSYS2 / Cygwin-style home paths
        "/home/$_u"
        "/c/msys64/home/$_u"
        # Windows user profile, MSYS2 view and native view
        "/c/Users/$_u"
        "/c/users/$_u"
        # Roaming AppData (Emacs default HOME on native Windows;
        # may be redirected to a network share but worth listing
        # the usual location for the case where it is local)
        "/c/Users/$_u/AppData/Roaming"
        "/c/users/$_u/AppData/Roaming"
        "$HOME/AppData/Roaming"
        # Filesystem root, just in case.
        "/"
    )
    unset _u
fi

if [ -z "${BASH_PATH_PROJECT_MARKERS+x}" ] ; then
    BASH_PATH_PROJECT_MARKERS=( .git .hg .svn .bzr _darcs )
fi

# --- helpers -------------------------------------------------------------

# _pjb_canon DIR
# Echo DIR in its canonical bash form (resolves symlinks, case, etc).
# Falls back to the literal DIR if it does not exist.
_pjb_canon(){
    local d="$1"
    if [ -d "$d" ] ; then
        (cd "$d" 2>/dev/null && pwd -P) || printf '%s' "$d"
    else
        printf '%s' "$d"
    fi
}

# _pjb_is_cutoff DIR
# Return 0 iff DIR (already canonical) matches any cutoff entry,
# comparing both literal and canonical forms.
_pjb_is_cutoff(){
    local d="$1" c cc
    for c in "${BASH_PATH_CUTOFF[@]}" ; do
        [ -z "$c" ] && continue
        [ "$d" = "$c" ] && return 0
        cc="$(_pjb_canon "$c")"
        [ "$d" = "$cc" ] && return 0
    done
    return 1
}

# _pjb_has_project_marker DIR
# Return 0 iff DIR contains any entry from BASH_PATH_PROJECT_MARKERS.
_pjb_has_project_marker(){
    local d="$1" m
    for m in "${BASH_PATH_PROJECT_MARKERS[@]:-}" ; do
        [ -z "$m" ] && continue
        [ -e "$d/$m" ] && return 0
    done
    return 1
}

# --- walk ----------------------------------------------------------------

_pjb_project_walk(){
    local d parent
    d="$(_pjb_canon "$PWD")"
    while [ -n "$d" ] && [ "$d" != / ] ; do
        if [ -r "$d/.bash-path" ] ; then
            BASH_PATH_STOP=
            source "$d/.bash-path"
            [ -n "$BASH_PATH_STOP" ] && break
        fi
        _pjb_is_cutoff          "$d" && break
        _pjb_has_project_marker "$d" && break
        parent="$(dirname "$d")"
        [ "$parent" = "$d" ] && break    # reached fs root
        d="$parent"
    done
}

_pjb_project_walk

unset -f _pjb_project_walk _pjb_is_cutoff _pjb_has_project_marker _pjb_canon
unset BASH_PATH_STOP
