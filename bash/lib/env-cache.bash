#!/bin/bash

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

function pjb_bash_capture_msys_vars(){
    # Emit `declare -x` for the MSYSTEM-specific variables that are
    # set by /etc/profile/profile.d on MSYS2 and that the legacy
    # be_generate() either ignores or rewrites in a Linux-centric way.
    # Appended to the cache after be_generate so these win on source.
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

    PJB_BASH_LEGACY_NO_AUTORUN=1 source "$legacy_root"
    bashrc_set_host_uname

    de="$PJB_BASH_CACHE_DIR/default-env.bash"
    he="$final_file"
    be="$tmp_file"

    if pjb_bash_env_cache_stale_p ; then
        rm -f "$tmp_file"
        be_generate
        if [ -n "${MSYSTEM:-}" ] && [ -r "$final_file" ] ; then
            {
                printf '\n# MSYSTEM=%s overrides (appended after be_generate)\n' \
                       "$MSYSTEM"
                pjb_bash_capture_msys_vars
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
