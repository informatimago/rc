#!/bin/bash

export PJB_BASH_SKIP_LEGACY_CORE=1
export PJB_BASH_SKIP_LEGACY_HOST=1
export PJB_BASH_SKIP_LEGACY_ENV=1
export PJB_BASH_SKIP_LEGACY_OPTIONALS=1
export PJB_BASH_SKIP_LEGACY_FLIGHTGEAR=1
export PJB_BASH_SKIP_LEGACY_ALIASES=1
export PJB_BASH_SKIP_LEGACY_LINUX=1
export PJB_BASH_SKIP_LEGACY_AUTH=1
unset PJB_BASH_SKIP_LEGACY_INTERACTIVE

source "$PJB_BASH_RC_ROOT/bash/interactive/auth.bash"
source "$PJB_BASH_RC_ROOT/bash/interactive/aliases.bash"
source "$PJB_BASH_RC_ROOT/bash/interactive/helpers.bash"
source "$PJB_BASH_RC_ROOT/bash/interactive/linux-helpers.bash"
source "$PJB_BASH_RC_ROOT/bash/interactive/optionals.bash"
source "$PJB_BASH_RC_ROOT/bash/interactive/flightgear.bash"
source "$PJB_BASH_RC_ROOT/bash/interactive/shortcuts.bash"
source "$PJB_BASH_RC_ROOT/bash/legacy/monolith.bash"

pjb_bash_load_auth
pjb_bash_load_aliases
pjb_bash_load_general_helpers
pjb_bash_load_linux_helpers
pjb_bash_load_optionals
pjb_bash_load_flightgear_aliases
pjb_bash_load_directory_shortcuts
