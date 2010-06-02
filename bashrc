# -*- mode: shell-script;coding:iso-8859-1 -*-
# .bashrc
# The individual per-interactive-shell startup file


# Source global definitions
#[ -f /etc/bashrc ] && . /etc/bashrc


if [ $UID -eq 0 ] ; then
    umask 022 # rwxr-xr-x
else
    umask 022 # rwxr-xr-x And we'll set the access rights of the directories...
fi

# User specific environment and startup programs
export BASH_ENV=$HOME/.bash_env
[ -f $BASH_ENV ] && . $BASH_ENV
### PATH and other environment variables are set in .bash_env

source $HOME/opt/env.sh




if [ -n "$DISPLAY" ] ; then
    export XAUTHORITY=$HOME/.Xauthority
    function xauth { if [ "$1" = "list" ] ; then command xauth list | awk '{printf "%-36s %-20s %s\n",$1,$2,$3;}' ; else command xauth $@ ; fi }
fi



# On Darwin, we don't want to mess with X11 so much.
# This is probably a hint we shouldn't do that here anyways.
if [ $(uname) = Darwin ] ; then
    case "$DISPLAY" in
    :[0-1].[0-9])
        xrdb -merge ~/.Xresources
        ;;
    *)
        true
        ;;
    esac
else
    case "$DISPLAY" in
    :[0-1].[0-9])
        xrdb -merge ~/.Xresources
        xmodmap ~/.xmodmap
    #xset s 300
        xset dpms $(( 60 * 10 ))  $(( 60 * 15 ))  $(( 60 * 20 )) 
        ;;
    *)
        true
        ;;
    esac
fi



# Read first /etc/inputrc if the variable is not defined, and after 
# the /etc/inputrc include the ~/.inputrc
[ -z $INPUTRC ] && export INPUTRC=/etc/inputrc
stty erase  >/dev/null 2>&1

unset LS_COLORS
if [ $UID -eq 0 ] ; then
    export PS1='[\u@\h $DISPLAY \W]# '
elif type -path period-cookie >/dev/null 2>&1 ; then
    export PS1='`period-cookie`[\u@\h $DISPLAY \W]\$ '
else
    export PS1='[\u@\h $DISPLAY \W]$ '
fi


function ds () {
    local i=0
    local f
    for f in $(dirs) ; do
        echo "$i $f"
        i=$(($i + 1))
    done
}



# bash specific aliases:
alias rmerge='rsync -HSWacvxz --progress -e ssh '
alias rsynch='rsync -HSWacvxz --progress -e ssh --force --delete --delete-after'
alias rehash='hash -r'
alias which='type -path'
alias mplayer='mplayer -quiet'
# general aliases:
alias more=less
#alias ec='/usr/local/emacs-multitty/bin/emacsclient -c --no-wait'
alias ec='emacsclient --no-wait'
alias vi='emacs -nw -q'
alias nano='emacs -nw -q'
alias df='df -ah'
alias du='du -h'
alias sbcl='sbcl --noinform'
# alias nslookup='nslookup -silent'
# alias torrent='/usr/local/src/BitTornado-CVS/btdownloadheadless.py'
alias diff='diff --exclude .svn --exclude CVS --exclude _darcs --exclude \*~ --exclude \*.x86f --exclude \*.fasl --exclude \*.fas --exclude \*.lib --exclude \*.[oa] --exclude \*.so  --exclude \#\* --exclude \*.orig --exclude \*.rej'

alias dw='darcs whatsnew -sl'
alias dr='darcs record -am'
alias ds='darcs push'
alias dl='darcs pull'


export CVSEDITOR=emacsclient


# system specific aliases:
#if type -path qpkg >/dev/null 2>&1 ; then alias qpkg="$(type -p qpkg) -nC" ; fi
if [ $(uname) = Darwin ] ; then
    ou=$(umask);umask 077
    env|sed -n -e '/UTF-8/d' -e'/=C$/d' -e 's/^/export /' -e '/LC_/s/$/.UTF-8 /p'>/tmp/$$
    . /tmp/$$ ; rm /tmp/$$
    umask $ou
    alias ls='LC_COLLATE="C" /bin/ls -aBCF'
    alias lsv='LC_COLLATE="C" /bin/ls -CF'

    alias mysqlstart='sudo /opt/local/bin/mysqld_safe5 &'
    alias mysqlstop='/opt/local/bin/mysqladmin5 -u root -p shutdown'
    alias mysqlping='/opt/local/bin/mysqladmin5 -u root -p ping'
    alias mysql='/opt/local/bin/mysql5'
    alias mysqlshow='/opt/local/bin/mysqlshow5'

else
    alias ls='LC_COLLATE="C" /bin/ls -aBCFN'
    alias lsv='LC_COLLATE="C" /bin/ls -BCFN'
fi


# ----------------------------------------
# gnustep specific environment:
# ----------------------------------------
# alias gsgdb=/usr/local/GNUstep/System/Tools/ix86/linux-gnu/gdb
# alias dread="defaults read "
# alias dwrite="defaults write "

_gopen () {
    local cur app
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    app=`for i in $GNUSTEP_LOCAL_ROOT/Applications/*.app  $GNUSTEP_SYSTEM_ROOT/Applications/*.app ; do basename $i ; done `
    COMPREPLY=($(compgen -W '$app' |grep ^$cur))
    return 0
}
complete -F _gopen -o dirnames gopen
complete -f -X '!*.@(app)' openapp


# ----------------------------------------
# gentoo
# ----------------------------------------

ew () {
    local key="$1"
    key="${key/\/}"
    esearch -c -F "$key" | sed -e "s/$key//"
}

# ----------------------------------------
# Linux rc
# ----------------------------------------
function status  { /etc/init.d/$1 status;  }
function start   { /etc/init.d/$1 start;   }
function stop    { /etc/init.d/$1 stop;    }
function restart { /etc/init.d/$1 restart; }
function reload  { /etc/init.d/$1 reload;  }


#END#


#    WHEN starting
#     AND ( interactive AND login ) OR ( non-interactive AND --login ) )
#      DO /etc/profile 
#         THEN    ~/.bash_profile 
#         OR ELSE ~/.bash_login
#         OR ELSE ~/.profile
#
#    WHEN starting
#     AND interactive AND NOT login
#      DO ~/.bashrc
#
#    WHEN starting
#     AND NOT interactive
#      DO $BASH_ENV
#
#    WHEN exiting
#     AND login
#      DO ~/.bash_logout
      


#       When  bash is invoked as an interactive login shell, or as
#       a non-interactive shell with the --login option, it  first
#       reads and executes commands from the file /etc/profile, if
#       that file exists.  After reading that file, it  looks  for
#       ~/.bash_profile,  ~/.bash_login,  and  ~/.profile, in that
#       order, and reads and executes commands from the first  one
#       that  exists  and is readable.  The --noprofile option may
#       be used when the shell is started to inhibit  this  behav­
#       ior.
#
#       When a login shell exits, bash reads and executes commands
#       from the file ~/.bash_logout, if it exists.
#
#       When an interactive shell that is not  a  login  shell  is
#       started,  bash reads and executes commands from ~/.bashrc,
#       if that file exists.  This may be inhibited by  using  the
#       --norc  option.   The --rcfile file option will force bash
#       to  read  and  execute  commands  from  file  instead   of
#       ~/.bashrc.
#
#       When  bash  is  started  non-interactively, to run a shell
#       script, for example, it looks for the variable BASH_ENV in
#       the  environment,  expands  its value if it appears there,
#       and uses the expanded value as the name of a file to  read
#       and  execute.   Bash  behaves  as if the following command
#       were executed:
#              if [ -n "$BASH_ENV" ]; then . "$BASH_ENV"; fi
#       but the value of the PATH variable is not used  to  search
#       for the file name.
#
#       If bash is invoked with the name sh, it tries to mimic the
#       startup behavior of historical versions of sh  as  closely
#       as  possible,  while  conforming  to the POSIX standard as
#       well.  When invoked as an interactive login  shell,  or  a
#       non-interactive  shell  with  the --login option, it first
#       attempts to read and execute  commands  from  /etc/profile
#       and ~/.profile, in that order.  The --noprofile option may
#       be used to inhibit this  behavior.   When  invoked  as  an
#       interactive  shell  with  the  name sh, bash looks for the
#       variable ENV, expands its value if it is defined, and uses
#       the  expanded value as the name of a file to read and exe­
#       cute.  Since a shell invoked as sh  does  not  attempt  to
#       read  and  execute  commands from any other startup files,
#       the --rcfile option  has  no  effect.   A  non-interactive
#       shell  invoked  with  the name sh does not attempt to read
#       any other startup files.  When invoked as sh, bash  enters
#       posix mode after the startup files are read.
#
#       When  bash  is  started in posix mode, as with the --posix
#       command line option, it follows  the  POSIX  standard  for
#       startup  files.   In  this mode, interactive shells expand
#       the ENV variable and commands are read and  executed  from
#       the  file  whose  name  is  the  expanded value.  No other
#       startup files are read.
#
#       Bash attempts to determine when it is  being  run  by  the
#       remote  shell daemon, usually rshd.  If bash determines it
#       is being run by rshd, it reads and executes commands  from
#       ~/.bashrc,  if  that file exists and is readable.  It will
#       not do this if invoked as sh.  The --norc  option  may  be
#       used to inhibit this behavior, and the --rcfile option may
#       be used to force another file to be read,  but  rshd  does
#       not generally invoke the shell with those options or allow
#       them to be specified.
#
#       If the shell is started with the effective user (group) id
#       not  equal  to the real user (group) id, and the -p option
#       is not supplied, no startup files are  read,  shell  func­
#       tions  are  not  inherited from the environment, the SHEL­
#       LOPTS variable, if  it  appears  in  the  environment,  is
#       ignored, and the effective user id is set to the real user
#       id.  If the -p  option  is  supplied  at  invocation,  the
#       startup behavior is the same, but the effective user id is
#       not reset.
#
#### .bashrc                          --                     --          ####
