#!/bin/bash

function pjb_bash_load_aliases(){
    if type -p tree >/dev/null 2>&1 ; then
        alias lstree=tree
    else
        alias lstree='ls -R'
    fi
    alias more=less
    alias vi='emacs -nw -q'
    alias nano='emacs -nw -q'

    case "${PJB_BASH_OS}" in
        Darwin) alias df='df -h' ;;
        *)      alias df='df -ah' ;;
    esac

    alias duh='du -h'
    alias sdiff='diff --exclude \#\*  --exclude \*~   --exclude \*TAGS   --exclude .git --exclude .hg --exclude .svn --exclude CVS --exclude _darcs   --exclude \*.x86f --exclude \*.fasl --exclude \*.fas --exclude \*.lib --exclude \*.[oa] --exclude \*.so    --exclude \*.orig --exclude \*.rej    --exclude \*.apk --exclude \*.ap_ --exclude \*.class --exclude \*.dex  --exclude \*.jar  --exclude \*.zip    --exclude \*.png --exclude \*.jpg --exclude \*.jpeg  --exclude \*.gif --exclude \*.pdf --exclude \*.zargo --exclude \*.svg --exclude \*.xlsx --exclude \*.graffle --exclude .gradle --exclude .idea --exclude .DS_Store --exclude \*.iml --exclude build'
    alias ..='cd ..'
    alias ...='cd ../..'
    alias …='cd ../..'
    alias sl=ls
    alias gss='git status --short'
    alias gdiff='git diff'

    case "${PJB_BASH_OS}" in
        Darwin)
            local ou
            ou="$(umask)"
            umask 077
            env | sed -n -e '/^LC.*=.*UTF-8$/d' -e '/^LC.*=C$/d' -e 's/^/export /' -e '/LC_/s/$/.UTF-8/p' > "/tmp/env.$$"
            source "/tmp/env.$$"
            rm -f "/tmp/env.$$"
            umask "$ou"

            function ls() {
                if [ -t 1 ] ; then
                    if [ -x /opt/local/bin/gls ] ; then
                        LC_COLLATE="C" command /opt/local/bin/gls -aBCFN "$@"
                    else
                        LC_COLLATE="C" command /bin/ls -aBCF "$@"
                    fi
                else
                    if [ -x /opt/local/bin/gls ] ; then
                        LC_COLLATE="C" command /opt/local/bin/gls -aBCFN "$@" | expand -t 8
                    else
                        LC_COLLATE="C" command /bin/ls -aBCF "$@" | expand -t 8
                    fi
                fi
            }

            function lsv() {
                if [ -t 1 ] ; then
                    if [ -x /opt/local/bin/gls ] ; then
                        LC_COLLATE="C" command /opt/local/bin/gls -BCFN "$@"
                    else
                        LC_COLLATE="C" command /bin/ls -BCF "$@"
                    fi
                else
                    if [ -x /opt/local/bin/gls ] ; then
                        LC_COLLATE="C" command /opt/local/bin/gls -BCFN "$@" | expand -t 8
                    else
                        LC_COLLATE="C" command /bin/ls -BCF "$@" | expand -t 8
                    fi
                fi
            }

            alias mysqlstart='sudo /opt/local/bin/mysqld_safe5 &'
            alias mysqlstop='/opt/local/bin/mysqladmin5 -u root -p shutdown'
            alias mysqlping='/opt/local/bin/mysqladmin5 -u root -p ping'
            alias mysql='/opt/local/bin/mysql5'
            alias mysqlshow='/opt/local/bin/mysqlshow5'
            ;;
        *)
            alias ls='LC_COLLATE="C" /bin/ls -aBCFN'
            alias lsv='LC_COLLATE="C" /bin/ls -BCFN'
            ;;
    esac

    alias rmerge='echo "rmerge src/ dst" ; rsync -HSWacvxz --progress -e "ssh -x"'
    alias rsynch='echo "rsynch src/ dst" ; rsync -HSWacvxz --progress -e "ssh -x" --force --delete --delete-after'
    alias rcopy='echo  "rcopy  src/ dst" ; rsync -HSWavx   --progress -e "ssh -x"'
    alias screen='screen -A'

    if [[ -x /data/src/emulators/macemu/BasiliskII/src/Unix/BasiliskII ]] ; then
        function basilisk(){ /data/src/emulators/macemu/BasiliskII/src/Unix/BasiliskII "$@" ; }
        function macos(){ /data/src/emulators/macemu/BasiliskII/src/Unix/BasiliskII "$@" ; }
    fi

    if [[ -x /Applications/VirtualBox.app/Contents/MacOS/VBoxManage ]] ; then
        function vboxmanage(){ /Applications/VirtualBox.app/Contents/MacOS/VBoxManage "$@" ; }
        function vbox(){ /Applications/VirtualBox.app/Contents/MacOS/VBoxManage "$@" ; }
    fi

    function play(){ command mplayer -nojoystick -quiet -noconsolecontrols -nomouseinput -nolirc -noar "$@" ; }
    function mplayer(){ command mplayer -nojoystick -quiet "$@" ; }
}
