#!/bin/bash
# authinfo.bash -- generic secret lookup for ~/.apikeys and ~/.authinfo
#
# Pascal J. Bourguignon's bashrc library.
#
# Both files are token streams of "key value" pairs.  A new *record*
# begins at a "lead" keyword:
#
#   ~/.authinfo : lead keyword "machine" (plus bare keyword "default")
#                 netrc-style, one record per line:
#                     machine HOST login LOGIN password SECRET port PORT
#
#   ~/.apikeys  : lead keyword "name", one-line OR multi-line records:
#                     name binance label prod apikey K secret S password P
#                 -- or --
#                     name binance
#                       label prod
#                       apikey K
#                       secret S
#                       password P
#
# Lines whose first non-blank character is '#' are comments.  Blank
# lines are ignored (a multi-line record is terminated by the next lead
# keyword, not by blank lines).
#
# A record is selected by giving "key value" pairs for any NON-SECRET
# field (machine, name, port, login, label, ... whatever the record has).
# The secret fields (apikey, secret, password) may never be used as a
# selection key.  When the selection is too lax and several records
# match, the FIRST one is returned and a warning is printed on stderr.
#
# Public API:
#
#   get_apikey WHAT [KEY VALUE]...        WHAT in {apikey,secret,password}
#       get_apikey apikey name binance label test   # -> some-api-key
#       get_apikey secret name binance label test   # -> some-secret
#
#   get_authinfo_password [KEY VALUE]...
#       get_authinfo_password machine fabrik.sncf.fr port gitlab
#       get_authinfo_password machine news.individual.net login bourguignon
#
# Exit status: 0 and the secret on stdout when found, non-zero and
# nothing on stdout when no record matches.
#
# File overrides: $APIKEYS_FILE and $AUTHINFO_FILE (a *.gpg path is
# transparently decrypted with gpg).

# --------------------------------------------------------------------------
# Internal engine.
#
#   __authinfo_query FILE LEADKEYS BAREKEYS SECRETKEYS RESULTKEY [KEY VALUE]...
#
# LEADKEYS   space separated keywords that start a record and take a value.
# BAREKEYS   space separated keywords that start a record with no value.
# SECRETKEYS space separated field names forbidden as selection keys.
# RESULTKEY  field name to return.
# --------------------------------------------------------------------------
__authinfo_query() {
    local file="$1" leadkeys="$2" barekeys="$3" secretkeys="$4" resultkey="$5"
    shift 5

    if [ ! -r "$file" ]; then
        printf 'authinfo: cannot read %s\n' "$file" >&2
        return 2
    fi

    # Collect "key value" selection pairs, rejecting secret fields.
    local sep=$'\037' selpacked='' querydesc='' k v
    while [ $# -gt 0 ]; do
        if [ $# -lt 2 ]; then
            printf 'authinfo: dangling selection key %q (need KEY VALUE pairs)\n' "$1" >&2
            return 2
        fi
        k="$1"; v="$2"; shift 2
        case " $secretkeys " in
            *" $k "*)
                printf 'authinfo: %q is a secret field and cannot be a selection key\n' "$k" >&2
                return 2 ;;
        esac
        selpacked+="$k$sep$v$sep"
        querydesc+="$k=$v "
    done
    querydesc="${querydesc% }"

    # Transparent gpg decryption.
    local catcmd
    if [[ "$file" == *.gpg ]]; then
        catcmd() { gpg --quiet --batch --decrypt -- "$file" 2>/dev/null; }
    else
        catcmd() { cat -- "$file"; }
    fi

    local result
    result="$(catcmd | awk \
        -v leadkeys="$leadkeys" \
        -v barekeys="$barekeys" \
        -v resultkey="$resultkey" \
        -v sep="$sep" \
        -v selpacked="$selpacked" \
        -v querydesc="$querydesc" '
      BEGIN {
        n = split(leadkeys, a, " "); for (i = 1; i <= n; i++) lead[a[i]] = 1
        m = split(barekeys, b, " "); for (i = 1; i <= m; i++) bare[b[i]] = 1
        nsel = 0
        ns = split(selpacked, s, sep)
        for (i = 1; i + 1 <= ns; i += 2) { nsel++; selk[nsel] = s[i]; selv[nsel] = s[i + 1] }
        have = 0; count = 0
      }
      function reset() { delete rec; have = 0 }
      function matches(   i, k) {
        if (!have) return 0
        if (!(resultkey in rec)) return 0
        for (i = 1; i <= nsel; i++) {
          k = selk[i]
          if (!(k in rec)) return 0
          if (rec[k] != selv[i]) return 0
        }
        return 1
      }
      function flush() {
        if (matches()) { count++; if (count == 1) result = rec[resultkey] }
      }
      {
        line = $0
        sub(/\r$/, "", line)
        if (line ~ /^[ \t]*#/) next
        nf = split(line, f, " ")
        i = 1
        while (i <= nf) {
          tok = f[i]
          if (tok in bare)      { flush(); reset(); have = 1; i++ }
          else if (tok in lead) { flush(); reset(); have = 1
                                  if (i + 1 <= nf) { rec[tok] = f[i + 1]; i += 2 } else i++ }
          else if (have)        { if (i + 1 <= nf) { rec[tok] = f[i + 1]; i += 2 } else i++ }
          else                  i++
        }
      }
      END {
        flush()
        if (count > 1)
          printf "authinfo: ambiguous query {%s} matched %d records; using the first\n", querydesc, count > "/dev/stderr"
        if (count >= 1) { print result; exit 0 }
        exit 1
      }
    ')"
    local status=$?
    unset -f catcmd
    if [ $status -eq 0 ]; then
        printf '%s\n' "$result"
    fi
    return $status
}

# --------------------------------------------------------------------------
# get_apikey WHAT [KEY VALUE]...
# --------------------------------------------------------------------------
get_apikey() {
    if [ $# -eq 0 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        cat <<'EOF' >&2
Usage: get_apikey {apikey|secret|password} [KEY VALUE]...
  Selects a record in ~/.apikeys (override with $APIKEYS_FILE) by the given
  non-secret KEY VALUE pairs and prints the requested secret field.
    get_apikey apikey name binance label test
    get_apikey secret name binance label test
EOF
        [ $# -eq 0 ] && return 2 || return 0
    fi

    local what="$1"; shift
    case "$what" in
        apikey|secret|password) ;;
        *) printf 'get_apikey: WHAT must be one of apikey|secret|password, got %q\n' "$what" >&2
           return 2 ;;
    esac

    __authinfo_query "${APIKEYS_FILE:-$HOME/.apikeys}" \
                     "name" "" "apikey secret password" "$what" "$@"
}

# --------------------------------------------------------------------------
# get_authinfo_password [KEY VALUE]...
# --------------------------------------------------------------------------
get_authinfo_password() {
    if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
        cat <<'EOF' >&2
Usage: get_authinfo_password [KEY VALUE]...
  Selects a record in ~/.authinfo (override with $AUTHINFO_FILE) by the given
  non-secret KEY VALUE pairs and prints its password.
    get_authinfo_password machine fabrik.sncf.fr port gitlab
    get_authinfo_password machine news.individual.net login bourguignon
EOF
        return 0
    fi

    __authinfo_query "${AUTHINFO_FILE:-$HOME/.authinfo}" \
                     "machine" "default" "password" "password" "$@"
}
