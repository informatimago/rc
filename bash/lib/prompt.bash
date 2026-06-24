#!/bin/bash
# prompt.bash -- interactive PS1 prompt construction.
#
# Extracted from legacy/monolith.bash (chipping the monolith into lib
# modules, one cluster at a time).  Sourced from monolith.bash; the actual
# prompt is installed by bashrc_set_prompt, which monolith's bashrc() calls
# for interactive shells.
#
# Public entry point:
#   bashrc_set_prompt        -- build PS1/PROMPT_COMMAND for this shell.
#
# Helpers (also used at prompt-display time, so they must stay defined):
#   short_pwd                -- last path component, elided to <=13 chars.
#   git_prompt_branch_update -- refresh $PJB_GIT_PROMPT_BRANCH from git.
#   git_prompt_command       -- PROMPT_COMMAND hook (git branch + chained cmd).

function short_pwd() {
    local dir="${PWD##*/}"
    local max=13

    if (( ${#dir} > max )); then
        printf "…%s" "${dir: -max}"
    else
        printf "%s" "$dir"
    fi
}

function git_prompt_branch_update() {
    local branch

    if branch="$(git symbolic-ref --quiet --short HEAD 2>/dev/null)" && [ -n "$branch" ] ; then
        PJB_GIT_PROMPT_BRANCH="$branch"
    else
        unset PJB_GIT_PROMPT_BRANCH
    fi
}

function git_prompt_command() {
    git_prompt_branch_update

    if [ -n "${PJB_PROMPT_COMMAND:-}" ] ; then
        eval "$PJB_PROMPT_COMMAND"
    fi
}

function bashrc_set_prompt(){
    # Thanks Twitter @climagic for the # prefix advice.
    #
    # COLOR_PROMPT, INSIDE_EMACS, etc,  are not transmitted in
    # chroots, and other subprocesses where the environment is
    # reset.  Therefore we reset use_color from the terminal below.
    #

    local use_color="${COLOR_PROMPT:-false}"
    local escape=$'\033'
    local bold="${escape}"'[1m'
    local underline="${escape}"'[4m'
    local blink="${escape}"'[5m'
    local invert="${escape}"'[7m'
    local no_bold="${escape}"'[22m'
    local no_underline="${escape}"'[24m'
    local no_blink="${escape}"'[25m'
    local no_invert="${escape}"'[27m'
    local black="${escape}"'[30m'
    local red="${escape}"'[31m'
    local green="${escape}"'[32m'
    local yellow="${escape}"'[33m'
    local blue="${escape}"'[34m'
    local magenta="${escape}"'[35m'
    local cyan="${escape}"'[36m'
    local white="${escape}"'[37m'
    local black_back="${escape}"'[40m'
    local red_back="${escape}"'[41m'
    local green_back="${escape}"'[42m'
    local yellow_back="${escape}"'[43m'
    local blue_back="${escape}"'[44m'
    local magenta_back="${escape}"'[45m'
    local cyan_back="${escape}"'[46m'
    local white_back="${escape}"'[47m'
    local normal="${escape}"'[0m'
    # ---
    local chroot=''
    local prefix=''
    local cookie=''
    local ibam=''
    # shellcheck disable=SC2016
    local display='$(case "$DISPLAY" in (*/*) basename "$DISPLAY" ;; (*) echo "$DISPLAY" ;; esac)'
    local available='$(/bin/df -h .|(read line ; awk '\''{print $4}'\''))'
    local time='$(date +%H:%M)'
    local base='[\u@\h '"${display}"' $(short_pwd) '"${available}"']'
    local git_branch='${PJB_GIT_PROMPT_BRANCH}'
    local newline='
'
    local prompt='$ '

    if ((UID==0)) ; then
        prompt='# '
    fi

    if [ -n "${INSIDE_EMACS:-}" ] ; then
        TERM=emacs
    fi
    case "$TERM" in
    (dumb)
        # -n = non-zero string
        if [ -n "${INSIDE_EMACS:-}" -o -n "${SCHROOT_CHROOT_NAME:-}" ] ; then
            prefix="\\w"
            use_color=true
        fi
        ;;
    (emacs)
        prefix="\\w"
        use_color=true
        ;;
    (xterm)
        use_color=false
        ;;
    (xterm-256color)
        use_color=true
        ;;
    esac
    export COLOR_PROMPT="${use_color}"

    if type -path period-cookie >/dev/null 2>&1 ; then
        # shellcheck disable=SC2016
        cookie='$('"$(type -path period-cookie)"')'
    fi

    if type -p ibam >/dev/null 2>&1 ; then
        ibam="\$(ibam|head -1|sed -e 's/Charge time left: */C\//' -e 's/Battery time left: */B\//' -e 's/Total battery time: */F\//')"
    fi

    if [ -n "${SCHROOT_CHROOT_NAME:=}" ] ; then
        chroot="${SCHROOT_CHROOT_NAME:=}"
    fi

    if [ -n "$chroot" ] ; then
        chroot="(${chroot})"
    fi
    if $use_color ; then
        if [ -n "$chroot" ] ; then
            chroot="\[${red_back}${black}\]${chroot}\[${normal}\]"
        fi
        # if [ -n "$cookie" ] ; then
        #     cookie="\[${black_back}${cyan}\]${cookie}\[${normal}\]"
        # fi
        if [ -n "$prefix" ] ; then
            prefix="\[${yellow}\]${prefix}\[${normal}\]"
        fi
        if [ -n "$ibam" ] ; then
            ibam="\[${black}${yellow_back}\]${ibam}\[${normal}\]"
        fi
        if [ -n "$time" ] ; then
            time="\[${yellow}${black_back}\]${time}\[${normal}\]"
        fi
        if [ -n "$base" ] ; then
            base="\[${cyan}${black_back}\]${base}\[${normal}\]"
        fi
        if [ -n "$git_branch" ] ; then
            git_branch="\[${green}${black_back}\]${git_branch}\[${normal}\]"
        fi
        if [ -n "$prompt" ] ; then
            prompt="\[${red}${black_back}\]${prompt}\[${normal}\]"
        fi
    fi
    if [ -n "$prefix" ] ; then
        prefix="\n${prefix}\n"
    fi
    git_branch="${git_branch}"'${PJB_GIT_PROMPT_BRANCH:+'"$newline"'}'
    export PS1="${chroot}${cookie}${prefix}${git_branch}${ibam}${time}${base}${prompt}"
    if [ -n "${PROMPT_COMMAND:-}" ] && [ "$PROMPT_COMMAND" != git_prompt_command ] ; then
        PJB_PROMPT_COMMAND="$PROMPT_COMMAND"
    fi
    PROMPT_COMMAND=git_prompt_command

    export SAVED_PS1="$PS1"
    #PS1='$(echo "${BLUE}    3.8.15.16.32-2.8    ${CYAN}   1.8.32.41.49-5    ${NORMAL}")'"$SAVED_PS1"
    #PS1="$(echo "${BLUE}    3.8.15.16.32-2.8    ${CYAN}       ${NORMAL}")$SAVED_PS1"
}
