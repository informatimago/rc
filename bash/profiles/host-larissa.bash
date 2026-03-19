#!/bin/bash

source "$PJB_BASH_RC_ROOT/bash/profiles/site-user.bash"
source "$PJB_BASH_RC_ROOT/bash/profiles/site-mts.bash"
pjb_bash_source_if_readable "$PJB_BASH_RC_ROOT/bashrc-nvidia"
