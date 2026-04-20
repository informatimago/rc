#!/bin/bash

function pjb_bash_load_flightgear_aliases(){
    local fgfs=false
    local fgfs_other_root=/other/fgfs
    local fgfs_root
    local fgfs_opt_root
    local fgfs_gentoo_root

    if [ -x /usr/games/bin/fgfs ] ; then
        fgfs=/usr/games/bin/fgfs
        fgfs_gentoo_root=/usr/games/share/FlightGear
        fgfs_root=$fgfs_other_root
    fi

    if [ -x /opt/fgfs-240/bin/fgfs ] ; then
        fgfs=/opt/fgfs-240/bin/fgfs
        fgfs_opt_root=/opt/fgfs-240/share/flightgear
        fgfs_root=$fgfs_opt_root
    fi

    if [ ! -s "$fgfs" ] ; then
        return
    fi

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

    fgfs_default_options=(
        ${fgfs_base_options[@]}
        --enable-random-objects
        --enable-ai-models
    )

    fgfs_scenery_options=(
        --fg-root=$fgfs_root
        --fg-scenery=$fgfs_root/Scenery:$fgfs_other_root/Scenery
    )

    fgfs_nimitz_options=(
        ${fgfs_base_options[@]}
        ${fgfs_scenery_options[@]}
    )

    fgfs_server=mpserver12.flightgear.org
    fgfs_period=20

    fgfs_port_ac112p=5112
    fgfs_port_ac112q=5113
    fgfs_port_ac112r=5114
    fgfs_port_bk1p=5115

    function netfs1(){
        cd ~/fgfs/ || return
        "$fgfs" \
            "${fgfs_default_options[@]}" \
            "${fgfs_scenery_options[@]}" \
            --multiplay="out,${fgfs_period},${fgfs_server},5000" --multiplay="in,${fgfs_period},,${fgfs_port:-5001}" \
            "$@"
    }
    function netfs2(){
        cd ~/fgfs/ || return
        "$fgfs" \
            "${fgfs_default_options[@]}" \
            "${fgfs_scenery_options[@]}" \
            --multiplay="out,${fgfs_period},${fgfs_server},5000" --multiplay="in,${fgfs_period},,${fgfs_port:-5001}" \
            "$@"
    }

    function typhoon-1(){ fgfs_port=${fgfs_port_bk1p}   ; netfs1 --callsign=F-PJB  --aircraft=typhoon "$@" ; }
    function typhoon(){   typhoon-1 --control=joystick "$@" ; }
    function f14-1(){     fgfs_port=${fgfs_port_ac112p} ; netfs1 --callsign=AC112P --aircraft=f-14b "$@" ; }
    function f14-2(){     fgfs_port=${fgfs_port_ac112q} ; netfs1 --callsign=AC112Q --aircraft=f-14b "$@" ; }
    function f14-3(){     fgfs_port=${fgfs_port_ac112r} ; netfs1 --callsign=AC112R --aircraft=f-14b "$@" ; }
    function f14(){       f14-1 --control=joystick "$@" ; }
    function f16-1(){     fgfs_port=${fgfs_port_ac112p} ; netfs1 --callsign=BK1P --aircraft=f16 "$@" ; }
    function f16-2(){     fgfs_port=${fgfs_port_ac112q} ; netfs1 --callsign=BK1Q --aircraft=f16 "$@" ; }
    function f16(){       f16-1 --control=joystick "$@" ; }
    function f18-1(){     fgfs_port=${fgfs_port_ac112p} ; netfs1 --callsign=BK1P --aircraft=f18 "$@" ; }
    function f18-2(){     fgfs_port=${fgfs_port_ac112q} ; netfs1 --callsign=BK1Q --aircraft=f18 "$@" ; }
    function f18(){       f18-1 --control=joystick "$@" ; }

    function f14main(){
        local slaveIP=localhost
        netfs2 --callsign=AC112M --aircraft=f-14b \
               --native-fdm=socket,out,${fgfs_period},${slaveIP},5510,udp \
               --native-ctrls=socket,out,${fgfs_period},${slaveIP},5511,udp \
               "$@"
    }

    function f14slave(){
        (
            export DISPLAY=192.168.7.160:0.0
            netfs2 --callsign=AC112S --aircraft=f-14b \
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
                   "$@"
        )
    }

    fgfs_disable_everything=(
        --disable-hud
        --disable-anti-alias-hud
        --disable-hud-3d
        --disable-random-objects
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
        cd ~/fgfs/ || return
        "$fgfs" \
            "${fgfs_nimitz_options[@]}" \
            --multiplay="out,${fgfs_period},${fgfs_server},5000" \
            --multiplay="in,${fgfs_period},,${fgfs_port}" \
            --callsign="$cs" \
            --aircraft=nimitz \
            --prop:/sim/mp-carriers/nimitz-callsign="$cs" \
            "${fgfs_disable_everything[@]}" \
            "$@"
    }

    function netfs1n(){
        cd ~/fgfs/ || return
        /usr/games/bin/fgfs "${fgfs_nimitz_options[@]}" "${fgfs_scenery_options[@]}" \
                             --multiplay=out,20,mpserver10.flightgear.org,5000 \
                             --multiplay=in,10,,5001 "$@" \
                             > /tmp/netfs1.$$.out 2>&1
    }
    function f14n(){ netfs1n --callsign=AC112P --aircraft=f-14b "$@" ; }
}
