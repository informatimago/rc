#!/bin/bash

function pjb_bash_load_directory_shortcuts(){
    function cdmts(){        cd "$HOME/works/mts/" ; }
    function cdpa(){         cd "$HOME/works/patchwork/src/patchwork" ; }
    function cdui(){         cd "$HOME/works/patchwork/src/mclgui" ; }
    function cdschmidt(){    cd "$HOME/works/synth/schmidt" ; }
    function cdschmidtlib(){ cd "$HOME/works/synth/schmidt/sources/SchmidtSynthesizerLibrarian/SchmidtSynthesizerLibrarian" ; }
    function cdmanif(){      cd "$HOME/works/manif" ; }
    function panic(){ echo -e "\n\n\n\n\n                                    \033[;5;35m Don't Panic \033[0m\n\n\n\n\n" ; }
    # Shortcuts to client work trees: private companion repository.
    pjb_bash_source_if_readable "$PJB_BASH_RC_ROOT/private/bash/interactive/shortcuts.bash"
}
