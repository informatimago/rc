#!/bin/bash

function pjb_bash_prepare_env_cache_dir(){
    if [ ! -d "$PJB_BASH_CACHE_DIR" ] ; then
        mkdir -p "$PJB_BASH_CACHE_DIR" 2>/dev/null || return 1
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

    if [ ! -f "$final_file" ] || [ "$PJB_BASH_RC_ROOT/bashrc" -nt "$final_file" ] || [ "$PJB_BASH_RC_ROOT/bash/legacy/monolith.bash" -nt "$final_file" ] ; then
        rm -f "$tmp_file"
        be_generate
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
}

function pjb_bash_load_env_cache(){
    pjb_bash_source_if_readable "$PJB_BASH_ENV_CACHE_FILE"
}
