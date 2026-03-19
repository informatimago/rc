# Agent Notes

- Treat `bashrc-keys` and any other sourced secret-bearing files as sensitive local material. Do not print secret values in responses.
- Keep `profile` POSIX-compatible and quiet.
- Keep `bash_profile` login-focused and interactive-gated.
- Keep `bashrc` as a dispatcher; prefer moving environment, interactive features, host-specific logic, and secrets into separate files.
- Any file intended for `BASH_ENV` must be safe for non-interactive shells: no prompts, no `echo`, no terminal control, no unconditional subprocess-heavy logic, no required writes on startup.
