# -*- mode:shell-script -*-
# .bash_profile
# The  personal  initialization  file,  executed  for login shells

function set_terminal(){
    if [ "$TERM" != emacs ] ; then
        if [ -t 1 ]; then
            tset -Q -c
            case $TERM in
            xterm*)
                stty erase  2>/dev/null
                ;;
            linux)
                [ $UID -eq 0 ] && loadkeys /root/pjb.kmap
                ;;
            esac
        fi
    fi
}

pjb_bash_source="${BASH_SOURCE[0]:-}"
if [ -z "$pjb_bash_source" ] ; then
    pjb_bash_source="$HOME/.bash_profile"
fi

while [ -L "$pjb_bash_source" ] ; do
    pjb_bash_dir="$(cd "$(dirname "$pjb_bash_source")" && pwd -P)"
    pjb_bash_source="$(readlink "$pjb_bash_source")"
    case "$pjb_bash_source" in
        /*) ;;
        *) pjb_bash_source="$pjb_bash_dir/$pjb_bash_source" ;;
    esac
done

pjb_bash_dir="$(cd "$(dirname "$pjb_bash_source")" && pwd -P)"
if [ ! -r "$pjb_bash_dir/bash/lib/context.bash" ] && [ -r "$HOME/rc/bash/lib/context.bash" ] ; then
    pjb_bash_dir="$(cd "$HOME/rc" && pwd -P)"
fi

source "$pjb_bash_dir/bash/lib/context.bash"
source "$PJB_BASH_RC_ROOT/bash/lib/profile-loader.bash"

pjb_bash_load_profiles

case $- in
    *i*)
        set_terminal
        [ -f ~/.bashrc ] && . ~/.bashrc
        ;;
esac

unset pjb_bash_dir pjb_bash_source
