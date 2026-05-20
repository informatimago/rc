#!/bin/bash

# path-compose is the authoritative composer for PATH/MANPATH/INFOPATH.
# It is sourced here so its functions are available to the cache writer
# (pjb_bash_build_env_cache) and to any caller that wants to recompose
# at shell-startup time after the cache has been loaded.
if [ -r "$PJB_BASH_RC_ROOT/bash/lib/path-compose.bash" ] ; then
    source "$PJB_BASH_RC_ROOT/bash/lib/path-compose.bash"
fi

function pjb_bash_env_cache_sources(){
    # Roots scanned for staleness: the bash/ subtree plus the bashrc-*
    # personality files at $PJB_BASH_RC_ROOT.  Anything older than the
    # newest of these is considered stale.  Missing paths are silently
    # skipped by find via -prune-on-error semantics (we filter them).
    local p
    for p in \
        "$PJB_BASH_RC_ROOT/bash" \
        "$PJB_BASH_RC_ROOT/bashrc-engine" \
        "$PJB_BASH_RC_ROOT/bashrc-pjb" \
        "$PJB_BASH_RC_ROOT/bashrc-mts" \
        "$PJB_BASH_RC_ROOT/bashrc-span" \
        "$PJB_BASH_RC_ROOT/bashrc-trustonic" \
        "$PJB_BASH_RC_ROOT/bashrc-harman" \
        "$PJB_BASH_RC_ROOT/bashrc-nvidia" \
        "$PJB_BASH_RC_ROOT/bashrc-google-cloud" \
        "$PJB_BASH_RC_ROOT/bashrc-keys"
    do
        [ -e "$p" ] && printf '%s\n' "$p"
    done
}

function pjb_bash_env_cache_stale_p(){
    local sources newer

    [ ! -f "$PJB_BASH_ENV_CACHE_FILE" ] && return 0

    mapfile -t sources < <(pjb_bash_env_cache_sources)
    [ "${#sources[@]}" -eq 0 ] && return 1

    newer="$(find "${sources[@]}" -newer "$PJB_BASH_ENV_CACHE_FILE" -print -quit 2>/dev/null)"
    [ -n "$newer" ] && return 0
    return 1
}

function pjb_bash_prepare_env_cache_dir(){
    if [ ! -d "$PJB_BASH_CACHE_DIR" ] ; then
        mkdir -p "$PJB_BASH_CACHE_DIR" 2>/dev/null || return 1
    fi
}

function pjb_bash_capture_path_vars(){
    # Emit `declare -x` for the path-shaped variables that path_compose
    # composes (PATH, MANPATH, INFOPATH) plus the MSYSTEM-specific
    # companions set by /etc/profile/profile.d on MSYS2.  Variables that
    # are unset in the current shell are simply skipped, so this runs
    # cleanly on Linux too (just emits PATH/MANPATH).
    local v vars=()
    for v in \
        PATH MANPATH INFOPATH \
        MSYSTEM MINGW_PREFIX MINGW_CHOST MINGW_PACKAGE_PREFIX \
        PKG_CONFIG_PATH PKG_CONFIG_SYSTEM_INCLUDE_PATH PKG_CONFIG_SYSTEM_LIBRARY_PATH \
        ACLOCAL_PATH CMAKE_PREFIX_PATH
    do
        [ -n "${!v:-}" ] && vars+=("$v")
    done
    if [ "${#vars[@]}" -gt 0 ] ; then
        declare -px -- "${vars[@]}"
    fi
}

function pjb_bash_build_env_cache(){
    local legacy_root tmp_file final_file

    pjb_bash_prepare_env_cache_dir || return 0

    legacy_root="$PJB_BASH_RC_ROOT/bash/legacy/monolith.bash"
    final_file="$PJB_BASH_ENV_CACHE_FILE"
    tmp_file="${final_file}.$$"

    [ -r "$legacy_root" ] || return 0

    # NB: keep this as `local` + plain source.  The temp-env-prefixed
    # form (`VAR=1 source file`) interacts badly with bash's scope
    # stack inside a function: it shows up as "pop_scope: head of
    # shell_variables not a temporary environment scope" when the
    # sourced file is large and itself defines/calls functions.
    local PJB_BASH_LEGACY_NO_AUTORUN=1
    source "$legacy_root"
    bashrc_set_host_uname

    de="$PJB_BASH_CACHE_DIR/default-env.bash"
    he="$final_file"
    be="$tmp_file"

    if pjb_bash_env_cache_stale_p ; then
        rm -f "$tmp_file"
        be_generate
        # path_compose is now the authoritative source for PATH /
        # MANPATH / INFOPATH on every platform.  It rebuilds PATH from
        # scratch via the bash/path.d/* layers, then we append the
        # result to the cache tail so it overrides whatever
        # be_generate's Linux-flavoured PATH munging wrote earlier in
        # the file.  Fallback: if path-compose.bash was not sourced
        # for some reason, leave be_generate's PATH in place.
        if [ -r "$final_file" ] && declare -F path_compose >/dev/null 2>&1 ; then
            path_compose
            {
                printf '\n# path_compose snapshot (overrides be_generate above)\n'
                pjb_bash_capture_path_vars
            } >> "$final_file"
        elif [ -n "${MSYSTEM:-}" ] && [ -r "$final_file" ] ; then
            {
                printf '\n# MSYSTEM=%s overrides (appended after be_generate)\n' \
                       "$MSYSTEM"
                pjb_bash_capture_path_vars
            } >> "$final_file"
        fi
        if [ -r "$PJB_BASH_RC_ROOT/bashrc-keys" ] ; then
            source "$PJB_BASH_RC_ROOT/bashrc-keys"
        fi
    fi

    unset -f bashrc 2>/dev/null
    unset -f bashrc_set_host_uname 2>/dev/null
    unset -f be_generate 2>/dev/null
    unset -f be_comment 2>/dev/null
    unset -f be_variable 2>/dev/null
    unset -f be_unset 2>/dev/null
    unset -f be_terminate 2>/dev/null
    unset -f be_append_terminate 2>/dev/null
    unset -f first_locale 2>/dev/null
    unset -f quote 2>/dev/null
    unset -f member 2>/dev/null
    unset -f reverse 2>/dev/null
    unset -f remove 2>/dev/null
    unset -f joinWithSeparator 2>/dev/null
    unset -f appendToListVariable 2>/dev/null
    unset -f appendNewToStringVariableDirectoryIfExists 2>/dev/null
    unset -f prependNewToStringVariableDirectoryIfExists 2>/dev/null
    unset -f prependIfDirectoryExists 2>/dev/null
    unset -f pjb_bash_env_cache_sources 2>/dev/null
    unset -f pjb_bash_env_cache_stale_p 2>/dev/null
}

function pjb_bash_load_env_cache(){
    pjb_bash_source_if_readable "$PJB_BASH_ENV_CACHE_FILE"
}
