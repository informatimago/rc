# -*- mode: shell-script;coding:utf-8 -*-
# .bashrc
# Note:  no interactive stuff here, ~/.bashrc is loaded by all scripts thru ~/.profile!

# Source global definitions
#[ -f /etc/bashrc ] && . /etc/bashrc


if [ $UID -eq 0 ] ; then
    umask 022 # rwxr-xr-x
else
    umask 022 # rwxr-xr-x And we'll set the access rights of the directories...
fi

if [ -r ~/.config/host ] ; then
    host=$(cat ~/.config/host)
else
    host=$(hostname -f)
fi


# Read first /etc/inputrc if the variable is not defined, and after 
# the /etc/inputrc include the ~/.inputrc
[ -z $INPUTRC ] && export INPUTRC=/etc/inputrc

case "$DISPLAY" in
/tmp/launch-*/org.x:0) export DISPLAY=:0.0 ;;
esac
export DISPLAY=:0.0

XDG_DATA_DIRS="$(echo "$XDG_DATA_DIRS"|sed -e 's/^:\+//' -e 's/:\+$//' -e 's/:\+/:/g')"


unset LS_COLORS
if [ $UID -eq 0 ] ; then
    export PS1='[\u@\h $DISPLAY \W]# '
elif [ "$TERM" = "emacs" ] ; then
    export PS1="\n\\w\n[\\u@\\h $DISPLAY]\\$ "
elif type -path period-cookie >/dev/null 2>&1 ; then
    export PS1='`period-cookie`[\u@\h $DISPLAY \W]\$ '
else
    export PS1='[\u@\h $DISPLAY \W]$ '
fi

PROMPT_COMMAND='export CDPATH="$(pwd -L)"'

uname="$(uname -s)"
case "$uname" in 
Darwin)
    ulimit -s 32768
    stty erase  >/dev/null 2>&1
    ;;
*)
    case $(uname -o) in
    Cygwin)
        true 
        ;;
    *)
        ulimit -s 32768
        ;;
    esac
esac



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
    local args=("$@")
    i=${#args[@]}
    while [ $i -ge 0 ] ; do
        echo "${args[$i]}"
        i=$(( $i - 1 ))
    done
}


function remove(){
    local element="$1" ; shift
    for arg ; do
        if [ "$element" != "$arg" ] ; then
            echo "$arg "
        fi
    done
}


function joinWithSeparator(){
    local separator="$1" ; shift
    local sep=''
    for arg ; do
        echo -n "${sep}${arg}"
        sep="$separator"
    done
    echo ''
}


function appendToListVariable(){
    # appendToList VARIABLE element...
    # Appends to the array VARIABLE each element.
    # Example:  a=(1 2 3) ; appendToList a 4 5 6 ; echo ${a[@]} --> 1 2 3 4 5 6
    local var=$1 ; shift
    for element ; do
        eval "$var[\${#$var[@]}]=\"\$element\""
    done
}


function appendNewToStringVariableDirectoryIfExists(){
    # appendNewToStringVariableDirectoryIfExists VARIABLE dir...
    # Appends to the VARIABLE each directory, if it exists as a directory [ -d dir ].
    local var=$1 ; shift
    ps=( $(eval "if [ -z \"\$${var}\" ] ; then true ; else echo \"\$${var}\"|tr ':' '\012' ; fi") )
    for dir ; do
        if [ -d "${dir}/." -a $(member "${dir}" "${ps[@]}") = NIL ] ; then
            eval "if [ -z \"\$${var}\" ] ; then ${var}=\"${dir}\" ; else ${var}=\"\$${var}:${dir}\" ; fi"
        fi
    done
}


function prependNewToStringVariableDirectoryIfExists(){
    # prependNewToStringVariableDirectoryIfExists VARIABLE dir...
    # Prepend to the VARIABLE each directory, if it exists as a directory [ -d dir ].
    # The last processed will be before in the resulting list.
    local var=$1 ; shift
    ps=( $(eval "if [ -z \"\$${var}\" ] ; then true ; else echo \"\$${var}\"|tr ':' '\012' ; fi") )
    for dir in "$@" ; do
        if [ -d "${dir}/." -a $(member "${dir}" "${ps[@]}") = NIL ] ; then
            eval "if [ -z \"\$${var}\" ] ; then ${var}=\"${dir}\" ; else ${var}=\"${dir}:\$${var}\" ; fi"
        fi
    done
}



function prependIfDirectoryExists(){
    local dir
    local result=()
    for dir in $(reverse $@) ; do
        if [ NIL != $(member $dir ${result[@]}) ] ; then
            result=($dir $(remove $dir ${result[@]}))
        elif [ -d $dir ] ; then
            result=($dir ${result[@]})
        fi
    done
    echo ${result[@]}
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
        $HOME/src/fast-export
        $HOME/src/reposurgeon
        $HOME/Tools
        $HOME/.rvm/bin # Add RVM to PATH for scripting
    
        $HOME/bin  $HOME/opt/bin
        /usr/local/bin  /usr/local/sbin /usr/local/opt
        /opt/local/libexec/gnubin/
        /opt/local/lib/postgresql84/bin  # on galatea
        /opt/local/libexec/gnubin
        /opt/*/bin      /opt/*/sbin 
        /opt/bin        /opt/sbin
        # /data/languages/sbcl/bin
        /data/languages/ecl/bin
        /data/languages/cmucl/bin
        /data/languages/clisp/bin
        /data/languages/ccl/bin
        /data/languages/acl82express
        /data/languages/abcl
        /Developer/Tools 
        /usr/X11R6/bin  /usr/X11/bin /usr/games 
        /usr/bin        /usr/sbin
        /bin            /sbin
    )

    sharedirs=(
        /opt/*/share
    )

    mandirs=( 
        /opt/local/man /opt/local/share/man 
        /usr/local/bin /usr/local/share/man 
        /usr/man /usr/share/man /usr/X11R6/man /usr/X11/man  
    )

    lddirs=( 
        /opt/*/lib 
        /opt/local/lib 
        /usr/local/lib 
        /lib /usr/lib /usr/X11R6/lib /usr/X11/lib 
    )

    editors=(
        /opt/local/bin/emacsclient 
        /usr/local/bin/emacsclient 
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
    be_variable COMMON  "$HOME/src/public/common"
    be_variable MAKEDIR "$COMMON/makedir"
    be_variable TARGET   $(uname)

    case "$uname" in 
    Darwin)
        if [ 1 = $(mfod -l|wc -l) ] ; then
            mfod -s 1
        fi
        socket=()
        e="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --socket-name=/tmp/emacs${UID}/server"
        alias ec="$e --no-wait"
        be_variable EDITOR    "$e"
        be_variable VISUAL    "$e"
        be_variable CVSEDITOR "$e"
        ;;
    *)
        for e in "${editors[@]}" ; do
            if [ -x "$e" ] ; then
                alias ec="$e --no-wait"
                be_variable EDITOR    "$e"
                be_variable VISUAL    "$e"
                be_variable CVSEDITOR "$e"
                break
            fi
        done
        ;;
    esac


    be_variable PATH "$(joinWithSeparator \: $(prependIfDirectoryExists ${bindirs[@]} ${PATH//:/ }))"

    # TODO: Check same thing is done elsewhere:
    list="$(joinWithSeparator \: $(prependIfDirectoryExists ${sharedirs[@]}))"
    if [ -s "$list" ] ; then
        if [ -s "$XDG_DATA_DIRS" ] ; then
            be_variable XDG_DATA_DIRS "$list:$XDG_DATA_DIRS"
        else
            be_variable XDG_DATA_DIRS "$list"
        fi
    fi

    be_variable MANPATH         "$(joinWithSeparator \: $(prependIfDirectoryExists ${mandirs[@]} ${MANPATH//:/ }))"
    be_variable LD_LIBRARY_PATH "$(joinWithSeparator \: $(prependIfDirectoryExists ${lddirs[@]}  ${LD_LIBRARY_PATH//: / }))"


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


    if [ -d /usr/share/kaffe/. ] ; then
        be_variable KAFFEHOME /usr/share/kaffe
        list=''
        appendNewToStringVariableDirectoryIfExists list \
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

    if [ -d /usr/local/languages/java/. ] ; then
        be_variable JAVA_BASE /usr/local/languages/java
        be_variable JAVA_HOME "$JAVA_BASE"/jdk1.1.6/
        list=''
        appendNewToStringVariableDirectoryIfExists list \
            "$JAVA_HOME"/lib/classes.zip \
            "$JAVA_BASE"/swing-1.0.3/swingall.jar \
            "$JAVA_BASE"/jaccess-1.0/jaccess.jar \
            "$JAVA_BASE"/mSQL-JDBC_1.0b3/imaginary.zip
        be_variable classpath_jdk "$list"
        be_variable CLASSPATH "$classpath_jdk"
        if [ -d /usr/local/JavaApps/. ] ; then
            be_variable PATH /usr/local/JavaApps:"$JAVA_HOME"/bin:"$PATH"
        else
            be_variable PATH "$JAVA_HOME"/bin:"$PATH"
        fi
    fi

    if [ -d /opt/local/share/java/gradle ] ; then
        be_variable GRADLE_HOME /opt/local/share/java/gradle
    fi
    
    be_comment 'Generic environment:'
    be_variable TZ                      Europe/Paris

    be_unset GNOME_KEYRING_CONTROL

    # Most prioritary:
    be_variable LC_ALL                    C
    # If LC_ALL is not defined:
    be_unset    LC_MONETARY               fr_FR.UTF-8
    be_unset    LC_MESSAGES               en_US.UTF-8
    be_unset    LC_NUMERIC                fr_FR.UTF-8
    be_unset    LC_TIME                   fr_FR.UTF-8
    be_variable LC_COLLATE                C
    be_variable LC_CTYPE                  C
    # If the above are not defined:
    be_variable LANG                      en_US.UTF-8

    be_unset XMODIFIERS

    # if [ $(hostname) = iMac-Core-i5.local ] ; then
    # 
    #     be_variable REPLYTO                 'Pascal Bourguignon <pbourguignon@dxo.com>'
    #     be_variable MAILHOST                localhost
    #     be_variable MAIL                    /var/spool/mail/$USER  # It's the default.
    #     be_variable MAILPATH                ${MAIL} # ${MAIL}:/larissa/root/var/spool/mail/$USER
    # 
    # else

        be_variable REPLYTO                 'Pascal J. Bourguignon <pjb@informatimago.com>'
        be_variable MAILHOST                mail.informatimago.com
        be_variable MAIL                    /var/spool/mail/$USER  # It's the default.
        be_variable MAILPATH                ${MAIL}:/larissa/root/var/spool/mail/$USER

    # fi

    be_variable SHELL                   /bin/bash # Seems it's not defined in cygwin bash...
    be_variable ESHELL                  /bin/bash
    be_variable NNTPSERVER              news.individual.net
    be_variable IRCNICK                 pjb
    be_variable IRCNAME                 'Pascal J. Bourguignon'
    be_variable IRCSERVER               irc.freenode.org
    be_variable BROWSER                 /usr/bin/lynx
    be_variable LYNX_CFG                "$HOME/.lynx.cfg"


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
    be_variable HTML_TIDY          "$HOME/public_html/tidy.config"
    be_variable ETAGS              
    be_variable CTAGS              
    be_variable GDFONTPATH          /usr/share/fonts/ttf-bitstream-vera

    be_variable DTK_PROGRAM         espeak


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
    be_variable SHOOPPATH          "$SHOOPMOD"

    if [ -d /usr/local/cint/. ] ; then
        be_variable CINTSYSDIR     /usr/local/cint
    fi


    be_terminate
}
########################################################################
if [ -f $BASH_ENV ] ; then
    if [ $HOME/rc/bashrc -nt $BASH_ENV ] ; then
        be_generate
    fi
else
    be_generate
fi
source $BASH_ENV

case "$host" in
    *macbook?trustonic.local)
        true ;;
    *)      
        wget_cookies=( --user-agent 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.9) Gecko/20020513' --cookies=on  --load-cookies /home/pascal/.mozilla/pascal/iolj6mzg.slt/cookies.txt )

        SHELLY_HOME=/home/pjb/.shelly; [ -s "$SHELLY_HOME/lib/shelly/init.sh" ] && . "$SHELLY_HOME/lib/shelly/init.sh"


        # GNUstep environment:
        if [ "x$GNUSTEP_MAKEFILES" = "x" ] ; then
            for gsr in /usr/lib/GNUstep /usr/share/GNUstep / /GNUstep /opt/local/GNUstep/share/GNUstep/ ; do
                #echo "$gsr/System/Makefiles"
                if [ -d $gsr/System/Makefiles ] ; then
                    gsr=$gsr/System
                    break
                fi
                [ -d $gsr/Makefiles ] && break
            done
            [ -f $gsr/Makefiles/GNUstep.sh ] && source $gsr/Makefiles/GNUstep.sh
        fi
        if [ -d "$GNUSTEP_SYSTEM_ROOT" ] ; then 
            export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GNUSTEP_SYSTEM_ROOT/lib
            export MANPATH=$GNUSTEP_SYSTEM_ROOT/Library/Documentation/man:${MANPATH:-/opt/local/share/man:/usr/share/man}
        fi
        if [ -s "$GNUSTEP_LOCAL_ROOT" ] ; then
            export MANPATH=$GNUSTEP_LOCAL_ROOT/Library/Documentation/man:${MANPATH:-/opt/local/share/man:/usr/share/man}
        fi

        ;;
esac


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



function ds () {
    local i=0
    local f
    for f in $(dirs) ; do
        echo "$i $f"
        i=$(($i + 1))
    done
}


# alias intersection='grep -Fxf' # Nope, doesn't work eg. on (armv7 armv7s arm64)inter(armv7 armv7s arm64).
# alias difference='grep -vFxf'

# bash specific aliases:
alias rmerge='echo "rmerge src/ dst" ; rsync -HSWacvxz --progress -e "ssh -x"'
alias rsynch='echo "rsynch src/ dst" ; rsync -HSWacvxz --progress -e "ssh -x" --force --delete --delete-after'
alias rcopy='echo  "rcopy  src/ dst" ; rsync -HSWavx   --progress -e "ssh -x"'
alias rehash='hash -r'
alias which='type -path'
alias mplayer='mplayer -quiet'
# general aliases:
alias more=less
alias vi='emacs -nw -q'
alias nano='emacs -nw -q'
case "$uname" in 
    Darwin)
        alias df='df -h'
        socket=(/tmp/emacs${UID}/server-*)
        alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait --socket-name=${socket[@]}" 
        ;;
    *)
        alias df='df -ah'
        alias ec='emacsclient --no-wait'
        #alias ec='/usr/local/emacs-multitty/bin/emacsclient -s /tmp/emacs${UID}/server-* -c --no-wait'
        ;;
esac
alias du='du -h'
# alias sbcl='sbcl --noinform'
# alias nslookup='nslookup -silent'
# alias torrent='/usr/local/src/BitTornado-CVS/btdownloadheadless.py'
alias diff='diff --exclude \#\*  --exclude \*~   --exclude \*TAGS   --exclude .git --exclude .hg --exclude .svn --exclude CVS --exclude _darcs   --exclude \*.x86f --exclude \*.fasl --exclude \*.fas --exclude \*.lib --exclude \*.[oa] --exclude \*.so    --exclude \*.orig --exclude \*.rej    --exclude \*.apk --exclude \*.ap_ --exclude \*.class --exclude \*.dex  --exclude \*.jar  --exclude \*.zip    --exclude \*.png --exclude \*.jpg --exclude \*.jpeg  --exclude \*.gif --exclude \*.pdf --exclude \*.zargo --exclude \*.svg --exclude \*.xlsx --exclude \*.graffle --exclude .gradle --exclude .idea --exclude .DS_Store --exclude \*.iml --exclude build'

alias basilisk=/data/src/emulators/macemu/BasiliskII/src/Unix/BasiliskII
alias macos=/data/src/emulators/macemu/BasiliskII/src/Unix/BasiliskII

# alias dw='darcs whatsnew -sl'
# alias dr='darcs record -am'
# alias ds='darcs push'
# alias dl='darcs pull'

alias ..='cd ..'
alias ...='cd ../..'
alias …='cd ../..'

alias play='mplayer -quiet -nojoystick -noconsolecontrols -nomouseinput -nolirc -noar'
alias mplayer='mplayer -nojoystick'

function dirs(){
    local i=1
    for dir in "${DIRSTACK[@]}" ; do
        printf "%2d) %s\n" $i "$dir"
        i=$(( $i + 1 ))
    done
}

function cdd(){
    local diri
    local i=1
    dirs
    read -p "Change to what directory? " diri
    for dir in "${DIRSTACK[@]}" ; do
        if [ "$diri" -eq "$i" ] ; then
            cd "$dir"
            return
        fi
        i=$(( $i + 1 ))
    done
    cd "$diri"
}

function pushdd(){
    local diri
    local i=1
    dirs
    read -p "Change to what directory? " diri
    for dir in "${DIRSTACK[@]}" ; do
        if [ "$diri" -eq "$i" ] ; then
            pushd "$dir"
            return
        fi
        i=$(( $i + 1 ))
    done
    pushd "$diri"
}


# system specific aliases:
#if type -path qpkg >/dev/null 2>&1 ; then alias qpkg="$(type -p qpkg) -nC" ; fi
if [ $(uname) = Darwin ] ; then
    ou=$(umask);umask 077
    env|sed -n -e '/^LC.*=.*UTF-8$/d' -e'/^LC.*=C$/d' -e 's/^/export /' -e '/LC_/s/$/.UTF-8/p' >/tmp/$$
    . /tmp/$$ ; rm /tmp/$$
    umask $ou
    if [ -x /opt/local/bin/gls ] ; then
	alias  ls='LC_COLLATE="C" /opt/local/bin/gls -aBCFN'
	alias lsv='LC_COLLATE="C" /opt/local/bin/gls -BCFN'
    else
	alias  ls='LC_COLLATE="C" /bin/ls -aBCF'
	alias lsv='LC_COLLATE="C" /bin/ls -CF'
    fi
    alias mysqlstart='sudo /opt/local/bin/mysqld_safe5 &'
    alias mysqlstop='/opt/local/bin/mysqladmin5 -u root -p shutdown'
    alias mysqlping='/opt/local/bin/mysqladmin5 -u root -p ping'
    alias mysql='/opt/local/bin/mysql5'
    alias mysqlshow='/opt/local/bin/mysqlshow5'

else
    alias ls='LC_COLLATE="C" /bin/ls -aBCFN'
    alias lsv='LC_COLLATE="C" /bin/ls -BCFN'
fi

fgfs=false
fgfs_other_root=/other/fgfs
fgfs_next_root=/data/src/simulation/fg/fgdata



if [ -x /usr/games/bin/fgfs ] ; then
    fgfs=/usr/games/bin/fgfs
    fgfs_gentoo_root=/usr/games/share/FlightGear
    fgfs_root=$fgfs_other_root
fi


# if [ -x /opt/fgfs/bin/fgfs ] ; then
#     fgfs=/opt/fgfs/bin/fgfs
#     fgfs_opt_root=/opt/fgfs/share/flightgear
#     fgfs_opt_root=$fgfs_next_root
#     fgfs_root=$fgfs_opt_root
# fi

if [ -x /opt/fgfs-240/bin/fgfs ] ; then
    fgfs=/opt/fgfs-240/bin/fgfs
    fgfs_opt_root=/opt/fgfs-240/share/flightgear
    fgfs_root=$fgfs_opt_root
fi


if [ -s "$fgfs" ] ; then

    fgfs_base_options=(
        --enable-anti-alias-hud
        --enable-clouds3d
        --enable-distance-attenuation
        --enable-enhanced-lighting
        --enable-horizon-effect
        --enable-hud-3d
        --enable-mouse-pointer
        --enable-real-weather-fetch
        --enable-skyblend
        --enable-sound
        --enable-specular-highlight
        --enable-splash-screen
        --enable-textures
        --enable-random-objects
        --enable-skyblend
    )

    fgfs_festival_options=(
        --prop:/sim/sound/voices/enabled=true 
    )

    fgfs_default_options=(
        ${fgfs_base_options[@]}
        --enable-random-objects
        --enable-ai-models
    )


    fgfs_scenery_options=(
        --fg-root=$fgfs_root
        --fg-scenery=$fgfs_other_root/Scenery-Airspace:$fgfs_other_root/Scenery-AirportsOverlay:$fgfs_other_root/Scenery-Photo:$fgfs_root/Scenery
    )

    fgfs_scenery_options=(
        --fg-root=$fgfs_root
        --fg-scenery=$fgfs_other_root/Scenery-AirportsOverlay:$fgfs_root/Scenery
    )

    fgfs_scenery_options=(
        --fg-root=$fgfs_root
        --fg-scenery=$fgfs_gentoo_root/Scenery:$fgfs_root/Scenery
    )

    fgfs_scenery_options=(
        --fg-root=$fgfs_root
        --fg-scenery=$fgfs_root/Scenery:$fgfs_other_root/Scenery
    )


    fgfs_nimitz_options=(
        ${fgfs_base_options[@]}
        ${fgfs_scenery_options[@]}
    )



    # fgfs_scenery_options=(
    #     --fg-root=/data/src/simulation/fg/fgdata
    #     --fg-scenery=/other/fgfs/Scenery-AirportsOverlay:/other/fgfs/Scenery
    # )
    # fgfs=/opt/fgfs/bin/fgfs
 
    fgfs_server=mpserver12.flightgear.org
    fgfs_period=20


    fgfs_port_ac112p=5112
    fgfs_port_ac112q=5113
    fgfs_port_ac112r=5114
    fgfs_port_bk1p=5115
    fgfs_port_f_pjb=5116
    fgfs_port_nimits=5117



    function netfs1(){ 
        cd ~/fgfs/ 
        "$fgfs" \
            ${fgfs_default_options[@]} \
            ${fgfs_scenery_options[@]} \
            --multiplay=out,${fgfs_period},${fgfs_server},5000  --multiplay=in,${fgfs_period},,${fgfs_port:-5001} \
            "$@" ; # > /tmp/netfs1.$$.out 2>&1 ; 
    }
    function netfs2(){
        cd ~/fgfs/ 
        "$fgfs" \
            ${fgfs_default_options[@]} \
            ${fgfs_scenery_options[@]} \
            --multiplay=out,${fgfs_period},${fgfs_server},5000  --multiplay=in,${fgfs_period},,${fgfs_port:-5001} \
            "$@" ; # > /tmp/netfs2.$$.out 2>&1  ; 
    }


cat > /dev/null <<EOF

(defun gen-parking-positions (base-name heading start-lon start-lat &optional end-lon end-lat n)
  (flet ((gen (name lon lat)
           (insert (format "%s=(--heading=%f --lon=%f --lat=%f)\n" name heading lon lat))))
    (if (or (null n) (<= n 1))
        (gen base-name start-lon start-lat)
        (loop
           repeat n
           for i from 1
           for cur-lon from start-lon by (/ (- end-lon start-lon) (1- n))
           for cur-lat from start-lat by (/ (- end-lat start-lat) (1- n))
           do (gen (format "%s_%d" base-name i) cur-lon cur-lat)))))

(progn
  (insert "\nEOF\n\n")
  (gen-parking-positions
     "KSUU_parking"
     145
     (- (dms-d 121 56 1.1))   
     (dms-d 38 16 0.4)
     (- (dms-d 121 55 30.7))
     (dms-d 38 16 22.3)
     20))

EOF

KSUU_parking_1=(--heading=145.000000 --lon=-121.933639 --lat=38.266778)
KSUU_parking_2=(--heading=145.000000 --lon=-121.933194 --lat=38.267098)
KSUU_parking_3=(--heading=145.000000 --lon=-121.932750 --lat=38.267418)
KSUU_parking_4=(--heading=145.000000 --lon=-121.932306 --lat=38.267738)
KSUU_parking_5=(--heading=145.000000 --lon=-121.931861 --lat=38.268058)
KSUU_parking_6=(--heading=145.000000 --lon=-121.931417 --lat=38.268379)
KSUU_parking_7=(--heading=145.000000 --lon=-121.930972 --lat=38.268699)
KSUU_parking_8=(--heading=145.000000 --lon=-121.930528 --lat=38.269019)
KSUU_parking_9=(--heading=145.000000 --lon=-121.930083 --lat=38.269339)
KSUU_parking_10=(--heading=145.000000 --lon=-121.929639 --lat=38.269659)
KSUU_parking_11=(--heading=145.000000 --lon=-121.929194 --lat=38.269980)
KSUU_parking_12=(--heading=145.000000 --lon=-121.928750 --lat=38.270300)
KSUU_parking_13=(--heading=145.000000 --lon=-121.928306 --lat=38.270620)
KSUU_parking_14=(--heading=145.000000 --lon=-121.927861 --lat=38.270940)
KSUU_parking_15=(--heading=145.000000 --lon=-121.927417 --lat=38.271260)
KSUU_parking_16=(--heading=145.000000 --lon=-121.926972 --lat=38.271580)
KSUU_parking_17=(--heading=145.000000 --lon=-121.926528 --lat=38.271901)
KSUU_parking_18=(--heading=145.000000 --lon=-121.926083 --lat=38.272221)
KSUU_parking_19=(--heading=145.000000 --lon=-121.925639 --lat=38.272541)
KSUU_parking_20=(--heading=145.000000 --lon=-121.925194 --lat=38.272861)


    function typhoon-1(){ fgfs_port=${fgfs_port_bk1p}   ; netfs1  --callsign=F-PJB   --aircraft=typhoon "$@" ; }
    function typhoon(){   typhoon-1 --control=joystick "@" ; }
    function f14-1(){     fgfs_port=${fgfs_port_ac112p} ; netfs1  --callsign=AC112P  --aircraft=f-14b   "$@" ; }
    function f14-2(){     fgfs_port=${fgfs_port_ac112q} ; netfs1  --callsign=AC112Q  --aircraft=f-14b   "$@" ; }
    function f14-3(){     fgfs_port=${fgfs_port_ac112r} ; netfs1  --callsign=AC112R  --aircraft=f-14b   "$@" ; }
    function f14(){       f14-1 --control=joystick  "$@" ; }
    function f16-1(){     fgfs_port=${fgfs_port_ac112p} ; netfs1  --callsign=BK1P    --aircraft=f16     "$@" ; }
    function f16-1(){     fgfs_port=${fgfs_port_ac112q} ; netfs1  --callsign=BK1Q    --aircraft=f16     "$@" ; }
    function f16(){       f16-1 --control=joystick   "$@" ; }
    function f18-1(){     fgfs_port=${fgfs_port_ac112p} ; netfs1  --callsign=BK1P    --aircraft=f18     "$@" ; }
    function f18-2(){     fgfs_port=${fgfs_port_ac112q} ; netfs1  --callsign=BK1Q    --aircraft=f18     "$@" ; }
    function f18(){       f18-1 --control=joystick   "$@" ; }


    function f14main(){
        local slaveIP=localhost
        netfs2  --callsign=AC112M  --aircraft=f-14b  \
            --native-fdm=socket,out,${fgfs_period},${slaveIP},5510,udp \
            --native-ctrls=socket,out,${fgfs_period},${slaveIP},5511,udp \
            "$@" ; }

    function f14slave(){    
       ( export DISPLAY=192.168.7.160:0.0 ;
         netfs2  --callsign=AC112S  --aircraft=f-14b  \
            --native-fdm=socket,in,${fgfs_period},,5510,udp \
            --native-ctrls=socket,in,${fgfs_period},,5511,udp \
            --fdm=null \
            --enable-panel \
            --disable-hud \
            --disable-sound \
            --prop:/sim/ai/enabled=false \
            --prop:/sim/ai-traffic/enabled=false \
            --prop:/sim/rendering/bump-mapping=false \
            --prop:/sim/rendering/draw-otw=false \
            "$@" ) ; }


    fgfs_disable_everything=(
        --disable-hud 
        --disable-anti-alias-hud
        --disable-hud-3d
        --disable-random-objects
        # --disable-ai-models
        --disable-ai-traffic
        --disable-freeze
        --disable-clock-freeze
        --disable-sound
        --disable-splash-screen
        --fog-disable
        --disable-enhanced-lighting
        --disable-distance-attenuation
        --disable-horizon-effect
        --disable-specular-highlight
        --disable-fullscreen
        --disable-skyblend
        --disable-textures
        --disable-clouds
        --disable-clouds3d
        )

    function nimitz(){
        local cs=CVN68
        cd ~/fgfs/ 
        "$fgfs" \
            ${fgfs_nimitz_options[@]} \
            --multiplay=out,${fgfs_period},${fgfs_server},5000  --multiplay=in,${fgfs_period},,${fgfs_port_nimitz} \
            --callsign=$cs \
            --aircraft=nimitz \
            --prop:/sim/mp-carriers/nimitz-callsign=$cs \
            ${fgfs_disable_everything[@]} \
            "$@"  # > /tmp/nimitz.$$.out 2>&1
        # --ai-scenario=nimitz_demo \
        #
    }

    function netfs1n(){ cd ~/fgfs/ ; /usr/games/bin/fgfs  ${fgfs_nimitz_options[@]} ${fgfs_scenery_options[@]} --multiplay=out,20,mpserver10.flightgear.org,5000  --multiplay=in,10,,5001 "$@" > /tmp/netfs1.$$.out 2>&1 ; }
    function f14n(){  netfs1n  --callsign=AC112P  --aircraft=f-14b "$@" ; }


fi

# ----------------------------------------
# gnustep specific environment:
# ----------------------------------------
# alias gsgdb=/usr/local/GNUstep/System/Tools/ix86/linux-gnu/gdb
# alias dread="defaults read "
# alias dwrite="defaults write "

function wmdock (){
    wmweather -s LELC -metric -kPa &
    wmglobe &
    wmspaceweather &
    wmsun -lat 42 -lon 0 &
}

function _gopen (){
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

function ew () {
    local key="$1"
    key="${key/\/}"
    esearch -c -F "$key" | sed -e "s/$key//"
}

# ----------------------------------------
# Linux rc
# ----------------------------------------
function status  (){ /etc/init.d/$1 status;  }
function start   (){ /etc/init.d/$1 start;   }
function stop    (){ /etc/init.d/$1 stop;    }
function restart (){ /etc/init.d/$1 restart; }
function reload  (){ /etc/init.d/$1 reload;  }

#  export CFLAGS=-I/opt/local/include ; export LDFLAGS=-L/opt/local/lib


# ----------------------------------------
# Some commands in $HOME/bin/* have a bash auto-completion feature.
# ----------------------------------------
case "$BASH_VERSION" in
    4.[1-9]*)
        if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
            . /opt/local/etc/profile.d/bash_completion.sh
        fi
        ;;
esac

quote(){
    for arg ; do
        local slash=${arg//\\/\\\\}
        local quote=\'${slash//\'/\'\\\'\'}\' # no "${...}" here! It would break the \'
        printf "%s " ${quote}
    done
    printf "\n"
}


if [ "$(uname)" != 'CYGWIN_NT-6.1-WOW64' ] ; then
    for script in radio fpm newpassword religion ; do
	eval $( $script --bash-completion-function )
    done
fi


# ----------------------------------------
# one liners with a certain utility
# ----------------------------------------
function get-directory   (){ awk '{if($1=="'"$1"'"){print $2}}' ~/directories.txt ; }

function timestamp       (){ date +%Y%m%dT%H%M%S ; }
function ip-address      (){ ifconfig|awk '/inet /{if($2!="127.0.0.1"){print $2;exit;}}' ; }

function sort-host       (){ tr '.' '@' | sort -t@ -n +0 -1 +1 -2 +2 -3 +3 -4 | tr '@' '.' ; }
    # We have to replace dots by something else since they are taken for
    # decimal points by sort -n.

function usb-devices     (){ awk 'BEGIN{line="======================================";line=line line;}
/^T/{printf "\n%s\n",line;print $0;next;}
/^[IC]/{printf "\n";print $0;next;}
{print $0;}
END{printf "\n%s\n",line;}
' < /proc/bus/usb/devices 
}


function mmencode        (){ base64 "$@" ; }
function msum            (){ md5sum "$1" ; sumseg 9728000 "$1" ; }

function rm-symlinks     (){ ls -l |grep -e '->'|awk '{print $9}'|xargs rm ; }

function all-disk-stat   (){ dstat -d -D total,$(cd /dev ; echo hd? sd? |tr ' ' ',') "$@" ; }
function sysexits        (){ grep '#define' /usr/include/sysexits.h|sed -e 's/#define[ 	][ 	]*\([^ 	][^ 	]*\)[ 	][ 	]*\([0-9][0-9]*\).*/export \1=\2/' ; }

function screen-size     (){ xwininfo -root|egrep 'Width|Height' ; }
function xauth-add       (){ xauth add $(echo "${DISPLAY:-DISPLAY-UNSET}" | sed 's/.*\(:.*\)/\1/') . $(mcookie) ; }
function xset-on         (){ ( export DISPLAY=:0.0 ; xset s 7200 0 ; xset dpms force on ; xset dpms 7200 8000 9000 ) ; }

function yls             (){ /bin/ls -1 $@ | sed -e 's/\(.*-[12][90][0-9][0-9][-.].*\)/\1 \1/' -e 's/^.*-\([12][90][0-9][0-9]\)[-.][^ ]* /\1  /' | sort -n ; }
# function wls(){  COLUMNS=$(stty -a|sed -n  -e '/columns;/s/.* \([0-9]*\) columns;.*/\1/p' -e '/; columns/s/.*columns \([0-9]\+\);.*/\1/p') ; ls -1 | sed -e 's/\(.................................\).*/\1/' |   COLUMNS=${COLUMNS:-80}  columnify ; }
function wls(){  c=$COLUMNS ; ls -1 | sed -e 's/\(.................................\).*/\1/' |   COLUMNS=${c:-80}  columnify ; }
function files           (){ if [ $# -eq 0 ] ; then find . -type f -print ; else find "$@" -type f -print ; fi | sort ; }

function c-to-digraph    (){ sed -e 's,#,%:,g' -e 's,\[,<:,g' -e 's,],:>,g' -e 's,{,<%,g' -e 's,},%>,g' ; }
function c-to-trigraph   (){ sed -e 's,#,??=,g' -e 's,\\,??/,g' -e 's,\\^,??'\'',g' -e 's,\[,??(,g' -e 's,],??),g' -e 's,|,??!,g' -e 's,{,??<,g' -e 's,},??>,g' -e 's,~,??-,g' ; }

function ec              (){ ( unset TMPDIR ; emacsclient "$@" ) ; }
function erc             (){ ( export EMACS_BG=\#fcccfefeebb7 ; emacs --eval "(irc)" ) ; }
function gnus            (){ ( export EMACS_BG=\#ccccfefeebb7 ; emacs --eval "(gnus)" ) ; }
function browse-file     (){ local file="$1" ; case "$file" in /*)  emacsclient -e "(browse-url \"file://${file}\")" ;; *)  emacsclient -e "(browse-url \"file://$(pwd)/${file}\")" ;; esac ; }



function subx            (){ Xnest -geometry 640x480 :4 -broadcast ; }
function opencyc         (){ ( cd /opt/opencyc-1.0/scripts/ ; ./run-cyc.sh ) ; }
# function xvv             (){ xv -windowid $(xwininfo -int  2> /dev/null |awk '/Window id/{print $4}') -maxpect -smooth "$@" ;}
function xvv             (){ xv -maxpect -smooth "$@" ;}

function svn-changes     (){ svn status | grep -e '^[?AMD]' ; }
function svn-status      (){ svn status --ignore-externals $1 | grep -v -e '^[?X]' ; }
function svn-obsolete    (){ for f in "$@" ; do mv "$f" "$f"-obsolete && svn update "$f" ; diff "$f" "$f"-obsolete  ; done ; }
function svn-keep        (){ for f ; do mv "${f}" "${f}-keep" && svn update "${f}" && mv "${f}" "${f}-old" && mv "${f}-keep" "${f}" ; done ; }


function swiki           (){ cd /srv/local/ComSwiki ; ./squeak -headless squeak.image & echo http://localhost:8888 ; }

function asx             (){ tr -d '\012' < "$1" | sed -e 's/.*[Rr][Ee][Ff]  *[Hh][Rr][Ee][Ff] *= *"\([^"]*\)".*/\1/' ; printf '\n' ; }
function aspx            (){ tr -d '\015\012' < "$1" | tr '<>' '\012\012' | sed -n -e 's/.*href="\(.*\.mov\)".*/\1/p' | head -1 ; }


function dui             (){ local f="$1" ; cp "$f" "${f}~" ;  iconv -f utf-8 -t iso-8859-1  < "${f}~" > $"$f" || cat "${f}~" > $"$f" ; }


function lisps           (){ clall -r '(lisp-implementation-version)' ; }

function history-graph   (){ history | awk '{h[$2]++}END{for(i in h){print h[i],i|"sort -rn|head -20"}}' | awk '{if(!m)m=$1;r="";i=s=60*$1/m;while(i-->0)r=r"#";printf "%15s %5d %s %s",$2,$1,r,"\n";}' ; }

# ----------------------------------------


function update-localized-xibs() {
    if [ $(basename $(pwd)) = "Resources" ] ; then
        for xibFile in "$@" ; do
            xibName="$(echo "$(basename ${xibFile})"|sed -e 's/.xib$//')"
            xibFile="${xibName}.xib"
            cp "English.lproj/${xibFile}" "English.lproj/${xibName}-UP.xib"
            svn revert "English.lproj/${xibFile}" 
            ibtool --previous-file "English.lproj/${xibFile}" --incremental-file "German.lproj/${xibFile}"   --localize-incremental --write "German.lproj/${xibFile}"   "English.lproj/${xibName}-UP.xib"
            ibtool --previous-file "English.lproj/${xibFile}" --incremental-file "French.lproj/${xibFile}"   --localize-incremental --write "French.lproj/${xibFile}"   "English.lproj/${xibName}-UP.xib"
            ibtool --previous-file "English.lproj/${xibFile}" --incremental-file "Japanese.lproj/${xibFile}" --localize-incremental --write "Japanese.lproj/${xibFile}" "English.lproj/${xibName}-UP.xib"
            rm "English.lproj/${xibFile}"
            mv "English.lproj/${xibName}-UP.xib" "English.lproj/${xibFile}"
        done
        svn status 
    else
        echo "Please, cd to a Resources directory."
        return 1
    fi
}

# ----------------------------------------
# old one liners
# ----------------------------------------
function remote-nntp     (){ sudo ssh -L 119:news.free.fr:119 pjb@free.informatimago.com ; }
function atc             (){ xterm -bg green -fg black +sb -fn '-misc-fixed-medium-r-normal-*-*-140-75-*-*-*-iso8859-1' -T atc -e bash -c "while true ; do /usr/games/bin/atc -g ${1:-Atlantis} ; sleep 5 ; done" ; }
function atc-b           (){ xterm +sb -bg green -fg black -fn '-*-courier-bold-r-*-*-24-*-*-*-*-*-*-*' -e '/usr/games/bin/atc -g Atlantis' ; }


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*



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


if [ -r ~/.config/host ] ; then
    host=$(cat ~/.config/host)
else
    host=$(hostname)
fi

case "$host" in
*macbook?trustonic.local|vmdevlinux)
    source ~/rc/bashrc-trustonic
    ;;
*)
    source ~/rc/bashrc-pjb
    ;;
esac


# Note:  no interactive stuff here, ~/.bashrc is loaded by all scripts thru ~/.profile and ~/.bash_profile!
#### THE END ####


