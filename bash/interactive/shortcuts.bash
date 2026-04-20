#!/bin/bash

function pjb_bash_load_directory_shortcuts(){
    function cdmts(){        cd "$HOME/works/mts/" ; }
    function cdpa(){         cd "$HOME/works/patchwork/src/patchwork" ; }
    function cdui(){         cd "$HOME/works/patchwork/src/mclgui" ; }
    function cdsb(){         cd "$HOME/works/sbde" ; }
    function cdsmt(){        cd "$HOME/works/sbde/smt" ; }
    function cdball(){       cd "$HOME/works/sbde/ball" ; }
    function cdbox(){        cd "$HOME/works/sbde/laboite" ; }
    function cdmini(){       cd "$HOME/works/sbde/stockme-minidemo-ios" ; }
    function cdmicro(){      cd "$HOME/works/sbde/stockme-microdemo-ios" ; }
    function cdschmidt(){    cd "$HOME/works/synth/schmidt" ; }
    function cdschmidtlib(){ cd "$HOME/works/synth/schmidt/sources/SchmidtSynthesizerLibrarian/SchmidtSynthesizerLibrarian" ; }
    function cdmanif(){      cd "$HOME/works/manif" ; }
    function panic(){ echo -e "\n\n\n\n\n                                    \033[;5;35m Don't Panic \033[0m\n\n\n\n\n" ; }
}
