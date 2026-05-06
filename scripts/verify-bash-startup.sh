#!/bin/bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd -P)"
test_home="${TMPDIR:-/tmp}/codex-bash-startup.$$"

cleanup() {
    rm -rf "$test_home"
}
trap cleanup EXIT

mkdir -p "$test_home/rc" "$test_home/bin"

cp "$repo_root/bashrc" "$test_home/rc/bashrc"
ln -s "rc/bashrc" "$test_home/.bashrc"
ln -s "$repo_root/bash_profile" "$test_home/.bash_profile"
ln -s "$repo_root/profile" "$test_home/.profile"
ln -s "$repo_root/bashrc-engine" "$test_home/rc/bashrc-engine"

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
    env \
        -u BASH_ENV \
        -u ENV \
        -u PJB_BASH_RC_ROOT \
        -u PJB_BASH_OS \
        -u PJB_BASH_HOSTNAME \
        -u PJB_BASH_CACHE_DIR \
        -u PJB_BASH_ENV_FILE \
        -u PJB_BASH_ENV_CACHE_FILE \
        HOME="$test_home" \
        bash --noprofile --norc -c "$1"
}

printf 'syntax: '
for file in \
    "$repo_root/bashrc" \
    "$repo_root/bashrc-engine" \
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

printf 'interactive copied bashrc: '
rm -f "$test_home/.bashrc"
cp "$repo_root/bashrc" "$test_home/.bashrc"
run_clean_bash 'bash --rcfile "$HOME/.bashrc" -i -c "printf ok\n"' >/tmp/verify-bash-interactive-copied.$$
cat /tmp/verify-bash-interactive-copied.$$
rm -f /tmp/verify-bash-interactive-copied.$$

printf 'interactive copied engine: '
rm -f "$test_home/.bashrc"
cp "$repo_root/bashrc-engine" "$test_home/.bashrc"
run_clean_bash 'bash --rcfile "$HOME/.bashrc" -i -c "printf ok\n"' >/tmp/verify-bash-interactive-copied-engine.$$
cat /tmp/verify-bash-interactive-copied-engine.$$
rm -f /tmp/verify-bash-interactive-copied-engine.$$

printf 'interactive empty readlink: '
rm -f "$test_home/.bashrc"
ln -s "rc/bashrc" "$test_home/.bashrc"
run_clean_bash 'function readlink(){ : ; } ; bash --rcfile "$HOME/.bashrc" -i -c "printf ok\n"' >/tmp/verify-bash-interactive-empty-readlink.$$
cat /tmp/verify-bash-interactive-empty-readlink.$$
rm -f /tmp/verify-bash-interactive-empty-readlink.$$

printf 'login: '
run_clean_bash 'bash --login -c "printf ok\n"' >/tmp/verify-bash-login.$$
cat /tmp/verify-bash-login.$$
rm -f /tmp/verify-bash-login.$$

printf 'login copied profile and engine: '
rm -f "$test_home/.bash_profile" "$test_home/.bashrc"
cp "$repo_root/bash_profile" "$test_home/.bash_profile"
cp "$repo_root/bashrc-engine" "$test_home/.bashrc"
run_clean_bash 'bash --login -c "printf ok\n"' >/tmp/verify-bash-login-copied-profile.$$
cat /tmp/verify-bash-login-copied-profile.$$
rm -f /tmp/verify-bash-login-copied-profile.$$
