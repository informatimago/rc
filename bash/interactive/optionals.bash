#!/bin/bash

function pjb_bash_maybe_load_rvm(){
    true
}

function pjb_bash_maybe_load_shelly(){
    local shelly_home="$HOME/.shelly"
    if [ -s "${shelly_home}/lib/shelly/init.sh" ] ; then
        source "${shelly_home}/lib/shelly/init.sh"
    fi
}

function pjb_bash_maybe_load_gnustep(){
    local gsr
    if [ "x$GNUSTEP_MAKEFILES" = "x" ] ; then
        for gsr in /usr/lib/GNUstep /usr/share/GNUstep / /GNUstep /opt/local/GNUstep/share/GNUstep/ ; do
            if [[ -d $gsr/System/Makefiles ]] ; then
                gsr=$gsr/System
                break
            fi
            [[ -d $gsr/Makefiles ]] && break
        done
        [[ -f $gsr/Makefiles/GNUstep.sh ]] && source "$gsr/Makefiles/GNUstep.sh"
    fi
    if [ -d "$GNUSTEP_SYSTEM_ROOT" ] ; then
        export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$GNUSTEP_SYSTEM_ROOT/lib"
        export MANPATH="$GNUSTEP_SYSTEM_ROOT/Library/Documentation/man:${MANPATH:-/opt/local/share/man:/usr/share/man}"
    fi
    if [ -s "$GNUSTEP_LOCAL_ROOT" ] ; then
        export MANPATH="$GNUSTEP_LOCAL_ROOT/Library/Documentation/man:${MANPATH:-/opt/local/share/man:/usr/share/man}"
    fi

    function wmdock (){
        wmweather -s LELC -metric -kPa &
        wmglobe &
        wmspaceweather &
        wmsun -lat 42 -lon 0 &
    }

    function _gopen (){
        local cur app
        cur=${COMP_WORDS[COMP_CWORD]}
        # shellcheck disable=SC2034
        app=$(for i in $GNUSTEP_LOCAL_ROOT/Applications/*.app  $GNUSTEP_SYSTEM_ROOT/Applications/*.app ; do basename "$i" ; done)
        # shellcheck disable=SC2016
        COMPREPLY=($(compgen -W '$app' |grep "^$cur"))
        return 0
    }
    complete -F _gopen -o dirnames gopen
    complete -f -X '!*.@(app)' openapp
}

function pjb_bash_load_optionals(){
    pjb_bash_maybe_load_rvm
    case "${PJB_BASH_HOSTNAME}" in
        *trustonic.local) ;;
        *)
            pjb_bash_maybe_load_shelly
            pjb_bash_maybe_load_gnustep
            ;;
    esac
}
