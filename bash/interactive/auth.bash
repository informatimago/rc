#!/bin/bash
#
# Authentication helpers.
#
# The generic secret lookup now lives in bash/lib/authinfo.bash, which
# provides get_apikey and get_authinfo_password.  The functions below are
# kept as DEPRECATED compatibility wrappers around it -- callers should be
# upgraded to the new API.

source "${PJB_BASH_RC_ROOT:-$HOME/rc}/bash/lib/authinfo.bash"

# --------------------------------------------------------------------------
# DEPRECATED: use  get_authinfo_password machine HOST [port PORT] [login LOGIN]
# Old positional API:  authinfo_password HOST [PORT] [LOGIN]
# --------------------------------------------------------------------------
authinfo_password() {
    printf 'authinfo_password is DEPRECATED; use: get_authinfo_password machine HOST [port PORT] [login LOGIN]\n' >&2
    local host="$1" port="$2" login="$3"
    local args=(machine "$host")
    [ -n "$port" ]  && args+=(port "$port")
    [ -n "$login" ] && args+=(login "$login")
    get_authinfo_password "${args[@]}"
}

# --------------------------------------------------------------------------
# DEPRECATED: use  get_authinfo_password machine HOST [login USER] [port PORT] [protocol PROTO]
# Old flag API:  auth_pick_first_password --host HOST [--user U] [--port P] [--protocol PROTO] [--file F]
# --------------------------------------------------------------------------
auth_pick_first_password() {
    local file="" host="" user="" port="" protocol=""
    while [ $# -gt 0 ]; do
        case "$1" in
            --file)     file="$2"; shift 2 ;;
            --host)     host="$2"; shift 2 ;;
            --user)     user="$2"; shift 2 ;;
            --port|--service) port="$2"; shift 2 ;;
            --protocol) protocol="$2"; shift 2 ;;
            -h|--help)
                cat <<'EOF'
Usage: auth_pick_first_password --host HOST [--user USER] [--port PORT] [--protocol PROTO] [--file FILE]
DEPRECATED -- use: get_authinfo_password machine HOST [login USER] [port PORT] [protocol PROTO]
EOF
                return 0
                ;;
            *) echo "Option inconnue: $1" >&2; return 2 ;;
        esac
    done

    printf 'auth_pick_first_password is DEPRECATED; use: get_authinfo_password machine HOST [login USER] [port PORT] [protocol PROTO]\n' >&2

    [ -n "$host" ] || { echo "Erreur: --host est obligatoire." >&2; return 2; }

    local args=(machine "$host")
    [ -n "$user" ]     && args+=(login "$user")
    [ -n "$port" ]     && args+=(port "$port")
    [ -n "$protocol" ] && args+=(protocol "$protocol")

    if [ -n "$file" ]; then
        AUTHINFO_FILE="$file" get_authinfo_password "${args[@]}"
    else
        get_authinfo_password "${args[@]}"
    fi
}

function pjb_bash_load_auth(){
    if [ -z "${LLM_API_KEY:-}" ] ; then
        LLM_API_KEY="$(get_authinfo_password machine api.openai.com port script 2>/dev/null || true)"
        export LLM_API_KEY
    fi
}
