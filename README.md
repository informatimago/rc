# RC Notes

This repository contains personal startup files and app configuration.

## Bash startup status

First-pass fixes applied on 2026-03-19:

- `bash_profile` now loads terminal setup and `~/.bashrc` only for interactive shells.
- `profile` is now quiet, POSIX-safe, and only loads `~/.bashrc` for interactive shells.
- `bashrc-engine` now keeps prompt, completion, aliases, and other interactive-only features out of non-interactive startup.
- `bashrc-engine` no longer assumes the rc tree is always at `~/rc`; host-specific files are loaded relative to the current engine file.
- `bashrc-engine` now skips `.bash_env` regeneration when the target files are not writable instead of failing noisily.
- the macOS `defaults write ... X11 ...` side effect is now limited to interactive TTY shells.

Second-pass refactor applied on 2026-03-19:

- `bashrc` is now a small symlink target that sources `~/rc/bashrc-engine`.
- `bashrc-engine` is now a dispatcher that loads context, profile selection, env cache, and then a compatibility interactive layer.
- the old monolithic startup file is preserved in `bash/legacy/monolith.bash`.
- host/environment selection now follows the Emacs pattern: a small entrypoint chooses a profile, and profiles compose common layers explicitly.
- `BASH_ENV` now points to `bash/env/noninteractive.bash`, which loads a host-specific cached environment file from `${XDG_CACHE_HOME:-$HOME/.cache}/bash/`.

## Remaining issues worth cleaning up

- `bashrc-keys` is a plain sourced file and should be treated as local secret material, not as a normal tracked config fragment.
- `.bash_env` generation still happens during shell startup and still mixes cache generation with config loading.
- host-specific files such as `bashrc-pjb`, `bashrc-mts`, `bashrc-trustonic`, `bashrc-harman` combine unrelated concerns and should be split further.
- `bashrc-engine` still contains a large amount of legacy aliases/functions that are useful interactively but make the file hard to audit.

## Proposed reorganization

Keep the startup chain small and explicit:

1. `bash_profile`
   Load login-only setup, then source `bashrc` only for interactive shells.
2. `profile`
   Stay POSIX and quiet; do not carry Bash-specific behavior except a guarded `.` of `~/.bashrc` for interactive Bash sessions if still needed.
3. `bashrc`
   Stay as the `~/.bashrc` symlink target and source `~/rc/bashrc-engine`.
4. `bashrc-engine`
   Act as a dispatcher only.
5. `bash/env/noninteractive.bash`
   Stable `BASH_ENV` entrypoint.
6. `bash/profiles/*.bash`
   Environment and host composition.
7. `bash/interactive/*.bash`
   Prompt, completion, aliases, readline behavior, interactive helpers.
8. `bash/hosts/*.bash`
   Host or employer specific additions.
9. `bash/secrets.local.bash`
   Untracked secrets and tokens.

Recommended follow-up:

- move generated `.bash_env*` files under `${XDG_CACHE_HOME:-$HOME/.cache}/bash/`.
- make `BASH_ENV` point to a stable generated file that never emits output and never spawns expensive subprocesses.
- move legacy topic blocks into opt-in files, for example `bash/optional/flightgear.bash`, `bash/optional/gnustep.bash`, `bash/optional/ruby.bash`.
- add a small verification script that runs interactive and non-interactive startup probes under a clean environment.

Implemented:

- generated non-interactive env is now cached under `${XDG_CACHE_HOME:-$HOME/.cache}/bash/`.
- `BASH_ENV` now points to a stable loader at `bash/env/noninteractive.bash`.
- the first optional interactive blocks were extracted to `bash/interactive/optionals.bash` and `bash/interactive/flightgear.bash`.
- startup verification is available via `scripts/verify-bash-startup.sh`.
