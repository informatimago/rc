#!/bin/bash
# 20-user.bash -- the user's own bin dirs.
#
# These come after project overlays so a project can shadow a
# user-installed tool with a pinned version, but before any system
# directory so user-installed tools shadow distro tools.

path_add "$HOME/bin"
path_add "$HOME/.local/bin"

# Ruby gems & friends -- only if the dir actually exists.
path_add "$HOME/.rbenv/bin"
path_add "$HOME/.rbenv/libexec"
path_add "$HOME/.rvm/bin"
path_add "${GEM_HOME:-$HOME/.gem}/bin"
