#!/bin/bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd -P)"
test_home="${TMPDIR:-/tmp}/codex-bash-startup.$$"

cleanup() {
    rm -rf "$test_home"
}
trap cleanup EXIT

mkdir -p "$test_home/rc" "$test_home/bin"

ln -s "$repo_root/bashrc" "$test_home/.bashrc"
ln -s "$repo_root/bash_profile" "$test_home/.bash_profile"
ln -s "$repo_root/profile" "$test_home/.profile"

cp -R "$repo_root/bash" "$test_home/rc/bash"

for file in bashrc-pjb bashrc-mts bashrc-span bashrc-harman bashrc-trustonic bashrc-nvidia bashrc-google-cloud; do
    if [ -r "$repo_root/$file" ] ; then
        cp "$repo_root/$file" "$test_home/rc/$file"
    fi
done

if [ -r "$repo_root/bashrc-keys" ] ; then
    cp "$repo_root/bashrc-keys" "$test_home/rc/bashrc-keys"
fi

cat > "$test_home/bin/distribution" <<'EOF'
#!/bin/sh
echo unknown
EOF
chmod +x "$test_home/bin/distribution"

run_clean_bash() {
    env -u BASH_ENV -u ENV HOME="$test_home" bash --noprofile --norc -c "$1"
}

printf 'syntax: '
for file in \
    "$repo_root/bashrc" \
    "$repo_root/bash_profile" \
    "$repo_root/profile" \
    "$repo_root"/bash/lib/*.bash \
    "$repo_root"/bash/env/*.bash \
    "$repo_root"/bash/interactive/*.bash \
    "$repo_root"/bash/profiles/*.bash \
    "$repo_root"/bash/legacy/monolith.bash
do
    bash -n "$file"
done
printf 'ok\n'

printf 'non-interactive: '
run_clean_bash 'bash -c "printf ok\n"' >/tmp/verify-bash-noninteractive.$$
cat /tmp/verify-bash-noninteractive.$$
rm -f /tmp/verify-bash-noninteractive.$$

printf 'interactive: '
run_clean_bash 'bash --rcfile "$HOME/.bashrc" -i -c "printf ok\n"' >/tmp/verify-bash-interactive.$$
cat /tmp/verify-bash-interactive.$$
rm -f /tmp/verify-bash-interactive.$$

printf 'login: '
run_clean_bash 'bash --login -c "printf ok\n"' >/tmp/verify-bash-login.$$
cat /tmp/verify-bash-login.$$
rm -f /tmp/verify-bash-login.$$
