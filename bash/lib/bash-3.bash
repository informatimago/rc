
# Bash 3 compatible subset of bash-4 mapfile/readarray.
# Supports: -t -n count -s count -O origin -u fd
# Does not support: -d -C -c
mapfile() {
    local OPTIND opt
    local trim_newline=false
    local max_lines=0
    local skip_lines=0
    local origin=0
    local fd=0

    while getopts ":tn:s:O:u:" opt; do
        case "$opt" in
            t) trim_newline=true ;;
            n) max_lines=$OPTARG ;;
            s) skip_lines=$OPTARG ;;
            O) origin=$OPTARG ;;
            u) fd=$OPTARG ;;
            *)
                echo "mapfile: invalid option -- $OPTARG" >&2
                return 2
                ;;
        esac
    done
    shift $((OPTIND - 1))

    local array_name=${1:-MAPFILE}

    # Basic validation for array variable name.
    case "$array_name" in
        ''|[0-9]*|*[!a-zA-Z0-9_]*)
            echo "mapfile: invalid array name: $array_name" >&2
            return 2
            ;;
    esac

    local line
    local index=$origin
    local read_count=0
    local skipped=0

    # If origin is 0, mimic bash mapfile behavior: clear the array.
    if [ "$origin" -eq 0 ]; then
        eval "$array_name=()"
    fi

    # Skip initial lines.
    while [ "$skipped" -lt "$skip_lines" ]; do
        IFS= read -r -u "$fd" line || return 0
        skipped=$((skipped + 1))
    done

    # Read lines.
    while :; do
        if [ "$max_lines" -ne 0 ] && [ "$read_count" -ge "$max_lines" ]; then
            break
        fi

        IFS= read -r -u "$fd" line
        local status=$?

        # Preserve final unterminated line.
        [ $status -ne 0 ] && [ -z "$line" ] && break

        if ! $trim_newline; then
            line="${line}"$'\n'
        fi

        # Store safely, preserving spaces, quotes, backslashes, etc.
        printf -v "__mapfile_q" "%q" "$line"
        eval "$array_name[$index]=$__mapfile_q"

        index=$((index + 1))
        read_count=$((read_count + 1))

        [ $status -ne 0 ] && break
    done
}

readarray() {
    mapfile "$@"
}
