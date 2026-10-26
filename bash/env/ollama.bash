#!/bin/bash
#
# Ollama server settings for thalassa (M3 Pro, 36 GB shared by apps and GPU).
# Sourced by bash/profiles/host-thalassa.bash for shells, and by the LaunchAgent
# ~/Library/LaunchAgents/com.informatimago.ollama-env.plist, which copies every
# OLLAMA_* variable below into the launchd environment for the Ollama app.
# Safe for non-interactive shells: exports only.

# Unload an idle model sooner than the default 5m, and never keep two loaded,
# so the ~19 GB qwen3-coder:30b doesn't push the other apps out to swap.
export OLLAMA_KEEP_ALIVE=2m
export OLLAMA_MAX_LOADED_MODELS=1

# q8_0 KV cache (needs flash attention): half the f16 size, near-lossless.
export OLLAMA_FLASH_ATTENTION=1
export OLLAMA_KV_CACHE_TYPE=q8_0

# 64k tokens: room for Claude Code's system prompt and tools; for qwen3-coder
# the q8_0 KV cache is then ~3.4 GB.
export OLLAMA_CONTEXT_LENGTH=65536

# Tell Claude Code the real window of local models it doesn't recognize
# (otherwise it assumes 200k and never compacts before Ollama truncates).
# Ignored for recognized claude-* models.
export CLAUDE_CODE_MAX_CONTEXT_TOKENS=$OLLAMA_CONTEXT_LENGTH
