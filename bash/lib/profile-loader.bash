#!/bin/bash

function pjb_bash_load_profiles(){
    local os_profile profile

    if [ "${PJB_BASH_PROFILES_LOADED:-0}" -eq 1 ] ; then
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
    export PJB_BASH_PROFILES_LOADED=1
}
