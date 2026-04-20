#!/bin/bash

function pjb_bash_load_general_helpers(){
    function variable-list(){
        local var_name
        local char
        if [[ -n "${1-}" ]] ; then
            for var_name in $(eval "echo \${!${1}*}"); do
                echo "$var_name"
            done
        else
            for char in _ {a..z} {A..Z} ; do
                variable-list "$char"
            done
        fi
    }

    function function-list(){ typeset -f | sed -n -e 's/^\([a-zA-Z_].*\) () *$/\1/p' ; }
    function function-source(){ for fun ; do declare -f "$fun" ; done ; }

    function find-in-directories(){
        local file="$1"
        shift
        local dir
        for dir in "$@" ; do
            if [[ -f "$dir/$file" ]] ; then
                echo "$dir/$file"
                return 0
            fi
        done
        return 1
    }

    function rehash(){ hash -r ; }
    alias which='type -path'

    function ds() {
        local i=1
        local dir
        for dir in "${DIRSTACK[@]}" ; do
            printf "%2d) %s\n" "$i" "$dir"
            ((i++))
        done
    }

    function cdd(){
        local diri
        local i=1
        local dir
        ds
        read -p "Change to what directory? " diri
        for dir in "${DIRSTACK[@]}" ; do
            if [ "$diri" -eq "$i" ] ; then
                cd "$dir" || return
                return
            fi
            ((i++))
        done
        cd "$diri"
    }

    function pushdd(){
        local diri
        local i=1
        local dir
        ds
        read -p "Change to what directory? " diri
        for dir in "${DIRSTACK[@]}" ; do
            if [ "$diri" -eq "$i" ] ; then
                pushd "$dir" >/dev/null || return
                return
            fi
            ((i++))
        done
        pushd "$diri" >/dev/null
    }

    function all-git-status(){ find . -name .git | while read -r f ; do (echo "==== $f" ; cd "$f/.." && git status) ; done ; }
    function get-directory(){ awk '{if($1=="'"$1"'"){print $2}}' ~/directories.txt ; }
    function timestamp(){ date +%Y%m%dT%H%M%S ; }
    function ip-address(){ ifconfig | awk '/inet /{if($2!="127.0.0.1"){print $2;exit;}}' ; }
    function ip-broadcast-address(){ ifconfig | grep -i Bcast | tr ' ' '\012' | awk -F: '/Bcast/{print $2}' ; }

    function get_ip_address_from_MAC(){
        local remote_MAC="$1"
        local bcast
        bcast="$(ip-broadcast-address)"
        case "$bcast" in
            *" "*) printf "Multiple broadcast addresses! %s\n" "$bcast"; return 1 ;;
        esac
        ping -c 3 "$bcast" >/dev/null 2>&1
        arp -n | awk "/$remote_MAC/"'{print $1;}'
    }

    function sort-host(){ tr '.' '@' | sort -t@ -n +0 -1 +1 -2 +2 -3 +3 -4 | tr '@' '.' ; }
    function usb-devices(){
        case "${PJB_BASH_OS}" in
            Darwin) system_profiler SPUSBDataType ;;
            Linux)
                awk 'BEGIN{line="======================================";line=line line;}
/^T/{printf "\n%s\n",line;print $0;next;}
/^[IC]/{printf "\n";print $0;next;}
{print $0;}
END{printf "\n%s\n",line;}
' < /proc/bus/usb/devices
                ;;
            *) printf "Error: unknown system type.\n"; return 1 ;;
        esac
    }

    function mmencode(){ base64 "$@" ; }
    function msum(){ md5sum "$1" ; sumseg 9728000 "$1" ; }
    function rm-symlinks(){ find . -maxdepth 1 -type l -exec rm {} + ; }
    function all-disk-stat(){ dstat -d -D total,$(cd /dev && echo hd? sd? | tr ' ' ',') "$@" ; }
    function sysexits(){ sed -n -e 's/#define[     ][     ]*\([^     ][^     ]*\)[     ][     ]*\([0-9][0-9]*\).*/export \1=\2/p' /usr/include/sysexits.h ; }

    function screen-size(){
        if [[ -n "${DISPLAY:-}" ]] ; then
            xwininfo -root | egrep 'Width|Height'
        fi
        if [[ "${PJB_BASH_OS}" = "Darwin" ]] ; then
            system_profiler SPDisplaysDataType | grep Resolution
        fi
    }

    function xauth-add(){ xauth add "$(echo "${DISPLAY:-DISPLAY-UNSET}" | sed 's/.*\(:.*\)/\1/')" . "$(mcookie)" ; }
    function xset-on(){ ( export DISPLAY=:0.0 ; xset s 7200 0 ; xset dpms force on ; xset dpms 7200 8000 9000 ) ; }
    function yls(){ /bin/ls -1 "$@" | sed -e 's/\(.*-\([12][90][0-9][0-9]\)\([-.].*\)\?\)$/\2 \1/' | sort -n ; }
    function wls(){ local c=$COLUMNS ; ls -1 | sed -e 's/\(.................................\).*/\1/' | COLUMNS=${c:-80} columnify ; }
    function files(){ if [ $# -eq 0 ] ; then find . -type f -print ; else find "$@" -type f -print ; fi | sort ; }
    function c-to-digraph(){ sed -e 's,#,%:,g' -e 's,\[,<:,g' -e 's,],:>,g' -e 's,{,<%,g' -e 's,},%>,g' ; }
    function c-to-trigraph(){ sed -e 's,#,??=,g' -e 's,\\,??/,g' -e 's,\\^,??'\'',g' -e 's,\[,??(,g' -e 's,],??),g' -e 's,|,??!,g' -e 's,{,??<,g' -e 's,},??>,g' -e 's,~,??-,g' ; }
    function ec(){ ( unset TMPDIR ; emacsclient --socket-name="$EMACS_SERVER_FILE" "$@" ) ; }
    function erc(){ ( export EMACS_BG=\#fcccfefeebb7 ; emacs --eval "(irc)" ) ; }
    function gnus(){ ( export EMACS_BG=\#ccccfefeebb7 ; emacs --eval "(gnus)" ) ; }
    function browse-file(){ local file="$1" ; case "$file" in /*) emacsclient -e "(browse-url \"file://${file}\")" ;; *) emacsclient -e "(browse-url \"file://$(pwd)/${file}\")" ;; esac ; }
    function subx(){ Xnest -geometry 640x480 :4 -broadcast ; }
    function opencyc(){ ( cd /opt/opencyc-1.0/scripts/ && ./run-cyc.sh ) ; }
    function xvv(){ xv -maxpect -smooth "$@" ; }
    function svn-changes(){ svn status | grep -e '^[?AMD]' ; }
    function svn-status(){ svn status --ignore-externals "$1" | grep -v -e '^[?X]' ; }
    function svn-obsolete(){ for f in "$@" ; do mv "$f" "$f"-obsolete && svn update "$f" && diff "$f" "$f"-obsolete ; done ; }
    function svn-keep(){ for f ; do mv "${f}" "${f}-keep" && svn update "${f}" && mv "${f}" "${f}-old" && mv "${f}-keep" "${f}" ; done ; }
    function swiki(){ cd /srv/local/ComSwiki && ./squeak -headless squeak.image & echo http://localhost:8888 ; }
    function asx(){ tr -d '\012' < "$1" | sed -e 's/.*[Rr][Ee][Ff]  *[Hh][Rr][Ee][Ff] *= *"\([^"]*\)".*/\1/' ; printf '\n' ; }
    function aspx(){ tr -d '\015\012' < "$1" | tr '<>' '\012\012' | sed -n -e 's/.*href="\(.*\.mov\)".*/\1/p' | head -1 ; }
    function dui(){ local f="$1" ; cp "$f" "${f}~" ; iconv -f utf-8 -t iso-8859-1 < "${f}~" > "$f" || cat "${f}~" > "$f" ; }
    function lisps(){ clall -r '(lisp-implementation-version)' ; }
    function history-graph(){ history | awk '{h[$2]++}END{for(i in h){print h[i],i|"sort -rn|head -20"}}' | awk '{if(!m)m=$1;r="";i=s=60*$1/m;while(i-->0)r=r"#";printf "%15s %5d %s %s",$2,$1,r,"\n";}' ; }

    function update-localized-xibs() {
        local xibFile xibName
        if [ "$(basename "$(pwd)")" = "Resources" ] ; then
            for xibFile in "$@" ; do
                xibName="$(echo "$(basename "${xibFile}")" | sed -e 's/.xib$//')"
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
}
