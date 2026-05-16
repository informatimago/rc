#!/bin/bash
# 40-msystem.bash -- MSYS2 toolchain layer.
#
# Each MSYSTEM (MSYS, MINGW32, MINGW64, UCRT64, CLANG64, CLANGARM64)
# has its own prefix.  Add only the active prefix's bin dirs so a
# UCRT64 shell does not see /mingw64/bin and vice-versa.
#
# Whole layer is a no-op when MSYSTEM is unset (i.e. Linux/macOS).

case "${MSYSTEM:-}" in
    MSYS)
        path_add /usr/bin
        manpath_add /usr/share/man
        infopath_add /usr/share/info
        ;;
    MINGW32|MINGW64|UCRT64|CLANG64|CLANGARM64)
        # $_pjb_prefix scoped to this layer; cleared at the end.
        case "$MSYSTEM" in
            MINGW32)    _pjb_prefix=/mingw32 ;;
            MINGW64)    _pjb_prefix=/mingw64 ;;
            UCRT64)     _pjb_prefix=/ucrt64 ;;
            CLANG64)    _pjb_prefix=/clang64 ;;
            CLANGARM64) _pjb_prefix=/clangarm64 ;;
        esac
        path_add     "$_pjb_prefix/bin"
        manpath_add  "$_pjb_prefix/share/man" "$_pjb_prefix/local/man"
        infopath_add "$_pjb_prefix/share/info" "$_pjb_prefix/local/info"
        env_set MINGW_PREFIX "$_pjb_prefix"
        unset _pjb_prefix
        # MINGW_CHOST, MINGW_PACKAGE_PREFIX, PKG_CONFIG_PATH,
        # ACLOCAL_PATH, CMAKE_PREFIX_PATH would be set here too.
        ;;
    "")
        : # Linux / macOS -- nothing to add at this layer.
        ;;
esac
