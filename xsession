#!/bin/bash
#
# .xsession ; actually sourced by /etc/X11/Xsession.d/40x11-common_xsessionrc
#

touch ~/.xsession-errors
mv    ~/.xsession-errors ~/.xsession-errors-current
touch ~/.xsession-errors

xset s on
xset s 400
xset -dpms

#xsetroot -solid '#d0e0f0'
#xsetroot -solid '#80a0c0'
xsetroot -solid '#4080a0'
#xsetroot -solid '#204050'

#
# If ssh is configured and ssh-agent is wanted set "yes"
#
usessh="no"

#
# If gpg is configured and gpg-agent is wanted set "yes"
#
usegpg="no"

#
# What we do if we fail at least ... emergency fall back.
#
failsafe="xterm -ls -T Failsave -geometry 80x24-0-0"
trap "exec $failsafe" EXIT SIGHUP SIGINT SIGPIPE SIGTERM SIGIO

#
# Some bash (1 and 2) settings to avoid trouble on a
# failed program call.
#
test -n "$BASH" && set +o posix
no_exit_on_failed_exec=1
type shopt > /dev/null 2>&1 && shopt -s execfail
set +e > /dev/null 2>&1

#
# Save some settings
#
__HOME="$HOME"         # Users HOME determined by xdm
__DISPLAY="$DISPLAY"      # The DISPLAY set by xdm for this session
__XAUTHORITY="$XAUTHORITY"   # Should be empty in the most cases
readonly __HOME __DISPLAY __XAUTHORITY

#
# Read system and user profile if not already done
#
if test -z "$XSESSION_IS_UP" ; then
    readonly XSESSION_IS_UP=yes
    export XSESSION_IS_UP
    test -r /etc/profile && { . /etc/profile > /dev/null 2>&1 ; }
    set +u > /dev/null 2>&1
    test -r ${HOME}/.profile && { . ${HOME}/.profile > /dev/null 2>&1 ; }
    set +u > /dev/null 2>&1
fi

#
# Read the profile of tcsh users
#
case "$SHELL" in
*/csh|*/tcsh)
    while read env ; do
	    var="${env%%=*}" ; val="${env#*=}"
	    test "$var" = "umask"  && umask $val
	    test "$var" = "$val"   && continue
	    eval export $var="\$val"
    done < <($SHELL -c 'test -r /etc/csh.login && source /etc/csh.login;
			test -r $HOME/.login   && source $HOME/.login  ;
			printenv; echo -n umask=; umask')
    ;;
esac

#
# Restore
#
set +u > /dev/null 2>&1
HOME="$__HOME"
DISPLAY="$__DISPLAY"
XAUTHORITY="$__XAUTHORITY"

#
# Run ssh-agent only if ssh is configured and avaliable.
#
sshagent="no"
if test "$usessh" = "yes" -a -d $HOME/.ssh ; then
    type -p ssh-agent > /dev/null 2>&1 && sshagent="yes"
    SSH_ASKPASS="/usr/lib/ssh/x11-ssh-askpass"
fi

#
# Run gpg-agent only if gpg-agent is configured and avaliable.
#
if test "$usegpg" = "yes" -a -d $HOME/.gnupg ; then
    if type -p gpg-agent > /dev/null 2>&1; then
        GPG_AGENT="gpg-agent --daemon --no-detach --keep-display"
    fi
fi





if [ -n "$DISPLAY" ] ; then
    export XAUTHORITY=$HOME/.Xauthority
    function xauth { if [ "$1" = "list" ] ; then command xauth list | awk '{printf "%-36s %-20s %s\n",$1,$2,$3;}' ; else command xauth $@ ; fi }

    xrdb -merge ~/.Xresources

    # On Darwin, we don't want to mess with X11 so much.
    # This is probably a hint we shouldn't do that here anyways.
    if [ $(uname) != Darwin ] ; then
        xrdb -merge ~/.Xresources
        xmodmap ~/.xmodmap
        # xset s 300
        xset dpms $(( 60 * 10 ))  $(( 60 * 15 ))  $(( 60 * 20 ))
    fi
fi


# exec dbus-launch --exit-with-session /usr/bin/your-window-manager

if test -f $HOME/.xinitrc ; then
    if test "$sshagent" = "yes" ; then
	    test -x "$SSH_ASKPASS" && export SSH_ASKPASS
	    exec $GPG_AGENT ssh-agent $HOME/.xinitrc
    else
	    exec $GPG_AGENT /bin/bash $HOME/.xinitrc
    fi
else
    test -n "$WINDOWMANAGER" && WINDOWMANAGER=`type -p $WINDOWMANAGER`
    if [ "$(uname)" = "Darwin" ] ; then
        wms=(
            /opt/X11/bin/quartz-wm
            /usr/X11/bin/quartz-wm
            /usr/X11/bin/twm
            $( if [ -x /usr/X11R6/bin/wmlist ] ; then /usr/X11R6/bin/wmlist ; fi )
        )
    else
        wms=(
            /usr/bin/wmaker
            /usr/bin/stumpwm
            /usr/bin/ratpoison
            /opt/local/bin/ratpoison
            /opt/X11/bin/quartz-wm
            /usr/X11/bin/quartz-wm
            /usr/bin/sawfish
            /usr/X11R6/bin/kde
            /usr/X11R6/bin/startkde
            /usr/X11R6/bin/fvwm2
            $( [ -x /usr/X11R6/bin/wmlist ] && /usr/X11R6/bin/wmlist || true )
        )
    fi
    for wm in "${wms[@]}" ; do
        if test -x "$wm" ; then
            WINDOWMANAGER="$(type -p "$wm")"
            test -n "$WINDOWMANAGER" && break
        fi
    done
    if test -z "$WINDOWMANAGER" ; then
	    xmessage -timeout 10 -default 'Too bad' -center -file - <<-EOF
		.xsession
                Error. Can not find a window manager.             -
                Please install one.                               -
		Exit for now.                                             -
		EOF
    else
	    # Start the XIM server:
	    test -r $HOME/.xim && source $HOME/.xim
	    if test "$sshagent" = "yes" ; then
	        test -x "$SSH_ASKPASS" && export SSH_ASKPASS
	        exec $GPG_AGENT ssh-agent $WINDOWMANAGER
	    else
	        exec $GPG_AGENT $WINDOWMANAGER
	    fi
    fi
fi

# call failsafe
exit 0
