# ~/.bash_logout
#
# bash login shell termination file.
#

case "$TERM" in
(xterm*) set-xterm-title "xterm" ;;
esac

case "$TERM" in
(emacs) true ;;
(*)     clear ;;
esac

