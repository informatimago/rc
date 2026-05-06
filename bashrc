# -*- mode: shell-script;coding:utf-8 -*-
# .bashrc

pjb_bashrc_source="${BASH_SOURCE[0]:-}"
if [ -z "$pjb_bashrc_source" ] ; then
    pjb_bashrc_source="$HOME/.bashrc"
fi

while [ -L "$pjb_bashrc_source" ] ; do
    pjb_bashrc_dir="$(cd "$(dirname "$pjb_bashrc_source")" && pwd -P)"
    pjb_bashrc_target="$(readlink "$pjb_bashrc_source")"
    if [ -z "$pjb_bashrc_target" ] ; then
        break
    fi
    pjb_bashrc_source="$pjb_bashrc_target"
    case "$pjb_bashrc_source" in
        /*) ;;
        *) pjb_bashrc_source="$pjb_bashrc_dir/$pjb_bashrc_source" ;;
    esac
done

pjb_bashrc_dir="$(cd "$(dirname "$pjb_bashrc_source")" && pwd -P)"
if [ -r "$pjb_bashrc_dir/bashrc-engine" ] ; then
    source "$pjb_bashrc_dir/bashrc-engine"
else
    source "$HOME/rc/bashrc-engine"
fi

unset pjb_bashrc_dir pjb_bashrc_source pjb_bashrc_target
