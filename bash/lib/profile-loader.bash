#!/bin/bash

function pjb_bash_load_profiles(){
    local os_profile profile

    # Load once per shell (bash_profile then bashrc), but again in each
    # child shell: aliases, functions and arrays are not inherited.  The
    # flag is not exported; unset the old exported one from older sessions.
    unset PJB_BASH_PROFILES_LOADED
    if [ "${pjb_bash_profiles_loaded:-0}" -eq 1 ] ; then
        return
    fi

    pjb_bash_source_if_readable "$PJB_BASH_RC_ROOT/bash/profiles/base.bash"

    case "$PJB_BASH_OS" in
        Darwin)  os_profile="os-darwin" ;;
        Linux)   os_profile="os-linux" ;;
        CYGWIN*|MINGW*|MSYS*) os_profile="os-windows" ;;
        *)       os_profile='' ;;
    esac
    [ -n "$os_profile" ] && pjb_bash_source_if_readable "$PJB_BASH_RC_ROOT/bash/profiles/${os_profile}.bash"

    profile="$(pjb_bash_profile_name)"
    pjb_bash_source_if_readable "$PJB_BASH_RC_ROOT/bash/profiles/${profile}.bash"
    pjb_bash_profiles_loaded=1
}
