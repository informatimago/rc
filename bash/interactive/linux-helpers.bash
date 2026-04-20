#!/bin/bash

function pjb_bash_load_linux_helpers(){
    if [[ $PJB_BASH_OS != Linux ]] ; then
        return
    fi

    function status  (){ sudo "/etc/init.d/$1" status;  }
    function start   (){ sudo "/etc/init.d/$1" start;   }
    function stop    (){ sudo "/etc/init.d/$1" stop;    }
    function restart (){ sudo "/etc/init.d/$1" restart; }
    function reload  (){ sudo "/etc/init.d/$1" reload;  }

    if [[ -r /etc/gentoo-release ]] ; then
        function ew () {
            local key="$1"
            key="${key/\/}"
            esearch -c -F "$key" | sed -e "s/$key//"
        }
    fi
}
