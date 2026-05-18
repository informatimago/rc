#!/bin/bash
# 95-windows.bash -- native-Windows host tools, lowest priority.
#
# These are the PATH entries from
#   c:/Users/$USER/AppData/Roaming/.bashrc
# which gets sourced when bash is launched on Windows with
# HOME = AppData/Roaming (typically by Emacs-on-Windows or by
# Windows-side tooling that does not go through the MSYS2 launcher).
#
# Numbered 95 because they are fallbacks: under MSYS2 the pacman-
# installed versions in /ucrt64/bin and /usr/bin already win via
# the msystem (40) and system (90) layers, so these only become
# visible when the better-versioned dirs are absent.  path_add is
# existence-filtered, so on Linux/macOS this whole layer is a no-op.

# For stuff like robocopy:

path_add "/c/windows/System32"
path_add "/c/Windows/System32/WindowsPowerShell/v1.0"
path_add "/C/Users/PPBN02261/AppData/Local/Microsoft/WinGet/Packages/astral-sh.uv_Microsoft.Winget.Source_8wekyb3d8bbwe"

# Git for Windows
path_add "/c/Program Files/Git/bin"
path_add "/c/Program Files/Git/usr/bin"

# GNU tools built for Windows
path_add "/c/Program Files/make-4.4.1-with-guile-w32/bin"
path_add "/c/Program Files/binutils-2.46-w32/bin"
path_add "/c/Program Files/findutils-4.2.30-5-w64/bin"

# Steel Bank Common Lisp (Windows installer drops sbcl.exe here)
path_add "/c/Program Files/Steel Bank Common Lisp"

# Astral uv, installed via WinGet.  The trailing package-id segment
# is Microsoft.Winget.Source_8wekyb3d8bbwe, which is a stable
# WinGet-source hash, not a version, so this path is stable across
# uv updates.
_pjb_winget="/c/Users/${USER:-$USERNAME}/AppData/Local/Microsoft/WinGet/Packages"
path_add "$_pjb_winget/astral-sh.uv_Microsoft.Winget.Source_8wekyb3d8bbwe"
unset _pjb_winget

path_add "/c/Program Files/dotnet"

