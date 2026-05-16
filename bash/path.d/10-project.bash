#!/bin/bash
# 10-project.bash -- project-local PATH overlay (highest priority).
#
# Walk up from $PWD looking for a `.bash-path` file.  If found, source
# it.  That file is itself a sequence of `path_add ...` calls so a
# project can pin a specific toolchain version without touching $PATH
# directly:
#
#     # ~/src/embedded-foo/.bash-path
#     path_add  /opt/xtensa-esp32-elf-12/bin
#     env_set   IDF_PATH "$HOME/esp/esp-idf-v5.2"
#
# This is direnv-lite -- no `eval "$(direnv hook bash)"`, no allow
# database -- just a file the project commits and that everyone
# working in the tree gets automatically.  Scope: PATH-y things only;
# secrets and credentials go elsewhere.
#
# Stop walking at $HOME so we don't read files from above the user.

_pjb_project_walk(){
    local d="$PWD"
    while [ -n "$d" ] && [ "$d" != / ] && [ "$d" != "$HOME" ] ; do
        if [ -r "$d/.bash-path" ] ; then
            source "$d/.bash-path"
            return
        fi
        d="$(dirname "$d")"
    done
}

_pjb_project_walk
unset -f _pjb_project_walk
