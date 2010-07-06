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



function member(){
    local item="$1" ; shift
    for arg ; do 
        if [ "$item" = "$arg" ] ; then
            echo T
            return 0
        fi
    done
    echo NIL
    return 1
}

function reverse(){
    args=("$@")
    i=${#args[@]}
    while [ $i -ge 0 ] ; do
        echo "${args[$i]}"
        i=$(( $i - 1 ))
    done
}


function appendIfPresent(){
    # appendIfPresent VARIABLE dir...
    # Appends to the VARIABLE each directory, if it exists as a directory [ -d dir ].
    local var=$1 ; shift
    ps=( $(eval "if [ -z \"\$${var}\" ] ; then true ; else echo \"\$${var}\"|tr ':' '\012' ; fi") )
    for dir ; do
        if [ -d "${dir}" -a $(member "${dir}" "${ps[@]}") = NIL ] ; then
            eval "if [ -z \"\$${var}\" ] ; then ${var}=\"${dir}\" ; else ${var}=\"\$${var}:${dir}\" ; fi"
        fi
    done
}


function prependIfPresent(){
    # prependIfPresent VARIABLE dir...
    # Prepend to the VARIABLE each directory, if it exists as a directory [ -d dir ].
    local var=$1 ; shift
    ps=( $(eval "if [ -z \"\$${var}\" ] ; then true ; else echo \"\$${var}\"|tr ':' '\012' ; fi") )
    for dir in $(reverse "$@" ) ; do
        if [ -d "${dir}" -a $(member "${dir}" "${ps[@]}") = NIL ] ; then
            eval "if [ -z \"\$${var}\" ] ; then ${var}=\"${dir}\" ; else ${var}=\"${dir}:\$${var}\" ; fi"
        fi
    done
}




# User specific environment and startup programs
export BASH_ENV=$HOME/.bash_env
########################################################################
### Generation of bash_env

be=$HOME/.bash_env.$$

function be_comment(){
    printf "# %s\n" "$@" >> "$be"
}

function quote_shell_argument(){
    echo "$1" | sed -e 's,\([^-+=:/_,.~^A-Za-z0-9]\),\\\1,g'
}

function be_variable(){
    local name="$1"
    local value="$2"
    printf "%s=%s\nexport %s\n" "$name" "$(quote_shell_argument "$value")" "$name" >> "$be"
}

function be_unset(){
    local name="$1"
    printf "unset %s\n" "$name" >> "$be"
}

function be_terminate(){
    mv "$be" "$BASH_ENV"
}


function be_generate(){
    local bindirs
    local mandirs
    local lddirs
    local editors
    local list

    bindirs=( 
        /opt/*/bin      /opt/*/sbin 
        /usr/local/bin  /usr/local/sbin
        /usr/X11R6/bin  /usr/X11/bin /usr/games 
        /usr/bin        /usr/sbin
        /bin            /sbin
        /opt/local/lib/postgresql84/bin 
        /usr/local/cint
        /Developer/Tools 
        /usr/local/apps/netscape 
        /usr/local/apps/Acrobat4/bin 
        /usr/local/apps/WordPerfect/wpbin 
        /Library/PostgreSQL8/bin 
        /Developer/Tools 
        $HOME/bin 
    )

    mandirs=( 
        /usr/man /usr/share/man /usr/X11R6/man /usr/X11/man  
        /usr/local/bin /usr/local/share/man 
        /opt/local/man /opt/local/share/man 
        /usr/local/languages/fpc/man 
        /usr/local/languages/clisp/share/man 
        /usr/local/cint/doc
    )

    lddirs=( 
        /lib /usr/lib /usr/X11R6/lib /usr/X11/lib 
        /usr/local/lib 
        /opt/local/lib 
        /opt/*/lib 
        /usr/lib/Real 
        /usr/local/apps/rvplayer5.0 
    )

    editors=( 
        $HOME/bin/ec 
        /opt/emacs-23.1/bin/emacsclient 
        /opt/emacs-22.1/bin/emacsclient 
        /opt/emacs-21.3/bin/emacsclient 
        /usr/local/emacs-multitty/bin/emacsclient 
        /usr/local/bin/emacsclient 
        /sw/bin/emacsclient 
        /usr/bin/emacsclient 
        /bin/emacsclient 
        /bin/ed 
        /usr/bin/vi 
    )
    
    be_comment '-*- mode:shell-script;coding:iso-8859-1 -*-'
    be_comment '.bash_env'
    be_comment 'Non interactive shells'
    be_comment '########################################################################'
    be_comment ''
    be_comment 'Unfortunately, we must be careful not launching subshells  from'
    be_comment '.bash_env,  since it is loaded by subshells; that gives infinite'
    be_comment 'recursions).  '
    be_comment ''
    be_comment 'So we will have only constant variable definitions here, and this'
    be_comment 'file will be generated from ~/.bashrc from time to time...'
    be_comment ''

    be_variable USERNAME "$USER"

    be_comment 'My compilation environment:'
    be_variable COMMON "$HOME/src/public/common"
    be_variable MAKEDIR "$COMMON/makedir"
    be_variable TARGET $(uname)


    for e in "${editors[@]}" ; do
        if [ -x "$e" ] ; then
            be_variable EDITOR    "$e"
            be_variable VISUAL    "$e"
            be_variable CVSEDITOR "$e"
            break
        fi
    done

    list="$PATH"
    prependIfPresent list  ${bindirs[@]}
    be_variable PATH "$list"

    list="$MANPATH"
    prependIfPresent list  ${mandirs[@]}
    be_variable MANPATH "$list"

    list="$LD_LIBRARY_PATH"
    prependIfPresent list ${lddirs[@]}
    be_variable LD_LIBRARY_PATH "$list"


    be_comment 'ANSI terminal codes:'
    be_variable CYAN_BACK          "[46m"
    be_variable MAGENTA_BACK       "[45m"
    be_variable BLUE_BACK          "[44m"
    be_variable YELLOW_BACK        "[43m"
    be_variable GREEN_BACK         "[42m"
    be_variable RED_BACK           "[41m"
    be_variable BLACK_BACK         "[40m"
    be_variable WHITE_BACK         "[47m"
    be_variable WHITE              "[37m"
    be_variable CYAN               "[36m"
    be_variable MAGENTA            "[35m"
    be_variable BLUE               "[34m"
    be_variable YELLOW             "[33m"
    be_variable GREEN              "[32m"
    be_variable RED                "[31m"
    be_variable BLACK              "[30m"
    be_variable NO_INVERT          "[27m"
    be_variable NO_BLINK           "[25m"
    be_variable NO_UNDERLINE       "[24m"
    be_variable NO_BOLD            "[22m"
    be_variable INVERT             "[7m"
    be_variable BLINK              "[5m"
    be_variable UNDERLINE          "[4m"
    be_variable BOLD               "[1m"
    be_variable NORMAL             "[0m"
    be_variable GOTO_HOME          ""
    be_variable CLEAR_HOME         ""

    be_variable CVSROOT            '' 
    be_variable CVS_RSH            ssh 


    if [ -d /usr/share/kaffe/ ] ; then
        be_variable KAFFEHOME /usr/share/kaffe
        list=''
        appendIfPresent list \
            "$JAVA_HOME"/lib/java.io.zip \
            $KAFFEHOME/Klasses.jar \
            /usr/local/share/kaffe/pizza.jar \
            "$JAVA_BASE"/mSQL-JDBC_1.0b3/imaginary.zip \
            "$JAVA_HOME"/lib/classes.zip \
            "$JAVA_HOME"/lib/i18n.jar \
            "$JAVA_HOME"/lib/rt.jar
        be_variable classpath_kaffe "$list"
        be_variable PATH "$JAVA_HOME"/bin:"$PATH"
    fi

    if [ -d /usr/local/languages/java/ ] ; then
        be_variable JAVA_BASE /usr/local/languages/java
        be_variable JAVA_HOME "$JAVA_BASE"/jdk1.1.6/
        list=''
        appendIfPresent list \
            "$JAVA_HOME"/lib/classes.zip \
            "$JAVA_BASE"/swing-1.0.3/swingall.jar \
            "$JAVA_BASE"/jaccess-1.0/jaccess.jar \
            "$JAVA_BASE"/mSQL-JDBC_1.0b3/imaginary.zip
        be_variable classpath_jdk "$list"
        be_variable CLASSPATH "$classpath_jdk"
        if [ -d /usr/local/JavaApps/ ] ; then
            be_variable PATH /usr/local/JavaApps:"$JAVA_HOME"/bin:"$PATH"
        else
            be_variable PATH "$JAVA_HOME"/bin:"$PATH"
        fi
    fi

    be_comment 'Generic environment:'
    be_variable TZ                      Europe/Madrid
    be_variable LC_MONETARY             es_ES.UTF-8
    be_variable LC_MESSAGES             en_US.UTF-8
    be_variable LC_NUMERIC              en_US.UTF-8
    be_variable LC_TIME                 en_US.UTF-8
    be_unset LC_MONETARY 
    be_unset LC_MESSAGES
    be_unset LC_NUMERIC 
    be_unset LC_TIME
    be_variable LC_COLLATE              C # fr_FR
    be_variable LC_CTYPE                C # fr_FR.UTF-8
    be_unset LC_ALL
    be_unset LANG
    be_variable REPLYTO                 'Pascal J. Bourguignon <pjb@informatimago.com>'
    be_variable MAILHOST                mail.informatimago.com
    be_variable MAIL                    /var/spool/mail/$USER  # It's the default.
    be_variable MAILPATH                ${MAIL}:/larissa/root/var/spool/mail/$USER
    be_variable ESHELL                  /bin/bash
    be_variable NNTPSERVER              news.individual.net
    be_variable IRCNICK                 pjb
    be_variable IRCNAME                 'Pascal J. Bourguignon'
    be_variable IRCSERVER               irc.informatimago.com
    be_variable BROWSER                 /usr/bin/lynx
    be_variable LYNX_CFG                "$HOME"/.lynx.cfg


    be_comment 'Application environments:'

    be_variable WRITE_ASF          1 # aviplay record:
    be_variable MOZILLA_HOME       /usr/local/apps/netscape
    be_variable WGET_UA            "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.9) Gecko/20020513"
    be_variable wgetua             "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.9) Gecko/20020513"
    be_variable MINICOM            '-M'
    be_variable CDR_DEVICE         0,0,0
    be_variable CDR_SPEED          4
    be_variable CDR_FIFOSIZE       16M
    be_variable ENSCRIPT           ' -TA4 -fCourier10 -FCourier-Bold12 -B -h --header="" --margins=:::12 '
    be_variable GENSCRIPT          "$ENSCRIPT"
    be_variable NENSCRIPT          "$ENSCRIPT"
    be_variable HTML_TIDY          $HOME/public_html/tidy.config
    be_variable ETAGS              
    be_variable CTAGS              

    # be_variable ORACLE_BASE       /home/oracle/app/oracle
    # be_variable ORACLE_HOME       "$ORACLE_BASE"/product/8.0.5
    # be_variable ORACLE_SID        orcl
    # be_variable NLS_LANG          fr_FR.WE8ISO8859P1
    # be_variable NLS_DATE_FORMAT   'SYYYY-MM-DD HH24:MI:SS'
    # be_variable LD_LIBRARY_PATH   "$ORACLE_HOME"/lib:"$ORACLE_HOME"/jdbc/lib:"$LD_LIBRARY_PATH"
    # be_variable PGDATABASE        quotedb

    be_comment 'antialiasing in QT applications'
    be_variable QT_XFT             1
    be_variable SHOOPSH            /usr/local/share/shoop/shoop.sh
    be_variable SHOOPMOD           /usr/local/share/shoop/modules
    be_variable SHOOPPATH          $SHOOPMOD

    if [ -d /usr/local/cint/ ] ; then
        be_variable CINTSYSDIR     /usr/local/cint
    fi


    # # GNUstep environment
    # 
    # if [ "x$GNUSTEP_MAKEFILES" = "x" ] ; then
    #     for gsr in / /gnustep /GNUstep /local/gnustep /local/GNUstep NOWHERE ; do
    #         if [ -d $gsr/System/Makefiles ] ; then
    #            gsr=$gsr/System
    #            break
    #         fi
    #         [ -d $gsr/Makefiles ] && break
    #     done
    #     [ -f $gsr/Makefiles/GNUstep.sh ] && .  $gsr/Makefiles/GNUstep.sh
    # fi
    # export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GNUSTEP_SYSTEM_ROOT/lib

    be_terminate
}
########################################################################
if [ -f $BASH_ENV ] ; then
    if [ $HOME/.bashrc -nt $BASH_ENV ] ; then
        be_generate
    fi
else
    be_generate
fi
source $BASH_ENV



wget_cookies=( --user-agent 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.9) Gecko/20020513' --cookies=on  --load-cookies /home/pascal/.mozilla/pascal/iolj6mzg.slt/cookies.txt )


# Moved to rc/xsession. Perhaps there's an even better place for this?
#
# if [ -n "$DISPLAY" ] ; then
#     export XAUTHORITY=$HOME/.Xauthority
#     function xauth { if [ "$1" = "list" ] ; then command xauth list | awk '{printf "%-36s %-20s %s\n",$1,$2,$3;}' ; else command xauth $@ ; fi }
# 
#     xrdb -merge ~/.Xresources
# 
#     # On Darwin, we don't want to mess with X11 so much.
#     # This is probably a hint we shouldn't do that here anyways.
#     if [ $(uname) != Darwin ] ; then
#         xrdb -merge ~/.Xresources
#         xmodmap ~/.xmodmap
#         # xset s 300
#         xset dpms $(( 60 * 10 ))  $(( 60 * 15 ))  $(( 60 * 20 )) 
#     fi
# fi



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
#### THE END ####
