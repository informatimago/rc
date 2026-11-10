#!/bin/bash

source "$PJB_BASH_RC_ROOT/bash/profiles/host-default.bash"

alias firefox='env -u BASH_ENV XAUTHORITY=$HOME/.Xauthority /snap/bin/firefox  > "/tmp/firefox-${UID}-$!".log 2>&1 & disown'
alias chatgpt='env -u BASH_ENV XAUTHORITY=$HOME/.Xauthority /usr/bin/chatgpt   > "/tmp/chatgpt-${UID}-$!".log 2>&1 & disown'
