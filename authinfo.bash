authinfo_password() {
  local host="$1"
  local port="$2"
  local login="$3"

  awk -v host="$host" -v port="$port" -v login="$login" '
  $1=="machine" {
      h=""; p=""; l=""; pw=""
      for(i=1;i<=NF;i++){
          if($i=="machine") h=$(i+1)
          if($i=="login")   l=$(i+1)
          if($i=="password") pw=$(i+1)
          if($i=="port")    p=$(i+1)
      }

      if(h==host &&
         (port=="" || p==port) &&
         (login=="" || l==login)) {
           print pw
           exit
      }
  }
  ' ~/.authinfo
}


# Usage:
#   auth_pick_first_password --host HOST [--user USER] [--port PORT|--service PORT] [--protocol PROTO] [--file FILE]
# Retour:
#   imprime le password du premier enregistrement netrc qui matche TOUS les critères.
auth_pick_first_password() {
  local file="$HOME/.authinfo"
  local host="" user="" port="" protocol=""

  # Parse options
  while [ $# -gt 0 ]; do
    case "$1" in
      --file)     file="$2"; shift 2 ;;
      --host)     host="$2"; shift 2 ;;
      --user)     user="$2"; shift 2 ;;
      --port|--service) port="$2"; shift 2 ;;  # alias --service -> --port
      --protocol) protocol="$2"; shift 2 ;;
      -h|--help)
        cat <<EOF
Usage: auth_pick_first_password --host HOST [--user USER] [--port PORT] [--protocol PROTO] [--file FILE]
Lit FILE (~/.authinfo par défaut, .gpg accepté) et imprime le premier password correspondant.
EOF
        return 0
        ;;
      *) echo "Option inconnue: $1" >&2; return 2 ;;
    esac
  done

  [ -n "$host" ] || { echo "Erreur: --host est obligatoire." >&2; return 2; }

  # Choisir la commande de lecture (gpg si .gpg)
  local catcmd=(cat --)
  if [[ "$file" == *.gpg ]]; then
    catcmd=(gpg --quiet --decrypt --)
  fi

  "${catcmd[@]}" "$file" 2>/dev/null | awk -v want_host="$host" -v want_user="$user" -v want_port="$port" -v want_proto="$protocol" '
    BEGIN {
      found = 0
      reset()
    }
    function reset() {
      delete rec
      order = 0
    }
    # Tokenisation simple: paires clé valeur (netrc)
    {
      for (i=1; i<=NF; i++) {
        tok = $i
        if (tok == "machine" || tok == "default") {
          # Avant d’ouvrir un nouvel enregistrement, valider l’ancien
          if (order > 0 && match_rec()) {
            print rec["password"]
            found = 1
            exit 0
          }
          reset()
          if (tok == "default") {
            rec["machine"] = "default"
          } else if (++i <= NF) {
            rec["machine"] = $i
          }
        } else {
          key = tok
          if (++i <= NF) {
            val = $i
            # enlever un éventuel \r (fichiers CRLF)
            sub(/\r$/, "", val)
            rec[key] = val
          }
        }
        order++
      }
    }
    END {
      # Si on a déjà imprimé, ne rien refaire
      if (found) exit 0
      # Sinon, vérifier le dernier enregistrement lu
      if (order > 0 && match_rec()) {
        print rec["password"]
        exit 0
      }
      exit 1
    }
    # Tous les critères fournis doivent matcher (ET logique)
    function match_rec(    ok) {
      ok = 1
      if (!("password" in rec)) ok = 0
      if (ok && want_host  != "" && !(("machine"  in rec) && rec["machine"]  == want_host))  ok = 0
      if (ok && want_user  != "" && !(("login"    in rec) && rec["login"]    == want_user))  ok = 0
      if (ok && want_port  != "" && !(("port"     in rec) && rec["port"]     == want_port))  ok = 0
      if (ok && want_proto != "" && !(("protocol" in rec) && rec["protocol"] == want_proto)) ok = 0
      return ok
    }
  '
}

