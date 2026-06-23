#!/usr/bin/perl
# Replace the body of pjb-terminal-key-bindings in emacs-common.el
# with a comprehensive CSI/SS3 input-decode-map registration.
# Preserves the file's original byte content outside the defun.

use strict;
use warnings;

my $path = '/c/msys64/home/PPBN02261/rc/emacs-common.el';
open my $fh, '<:raw', $path or die "open $path: $!";
local $/;  # slurp
my $data = <$fh>;
close $fh;

my $start_marker = "(defun pjb-terminal-key-bindings ()\n";
my $end_marker   = "  nil)\n";

my $start = index($data, $start_marker);
die "start marker not found" if $start < 0;

my $end = index($data, $end_marker, $start + length($start_marker));
die "end marker not found" if $end < 0;
$end += length($end_marker);

my $old = substr($data, $start, $end - $start);

# Build the replacement. Embedded raw bytes:
#   \033 = ESC, \177 = DEL.
my $ESC = chr(0o33);
my $DEL = chr(0o177);

my $new = <<"EOF";
(defun pjb-terminal-key-bindings ()
  "Decode CSI/SS3 escape sequences sent by function and navigation keys.

On terminal Emacs (notably on MS-Windows where escape sequences may
arrive too slowly to be timed by `read-key-sequence' on their own),
registering the full xterm CSI/SS3 prefix set in `input-decode-map'
does two useful things at once:
  - it covers every modifier/key combination the terminal actually
    sends, instead of leaving most of them as raw M-[..., M-O...;
  - because `read-key-sequence' keeps waiting as long as the partial
    input matches some prefix in `input-decode-map', registering the
    prefixes is what keeps a lone ESC from being interpreted as a
    bare Meta prefix while the rest of the sequence is still in flight."
  (interactive)
  ;; http://paste.lisp.org/display/131216
  ;; Legacy literal-byte bindings (kept in case the sequences arrive
  ;; raw because something else swallowed ESC upstream).
  (global-set-key "${ESC}OF"    'end-of-buffer)
  (global-set-key "${ESC}OH"    'beginning-of-buffer)
  (global-set-key "${DEL}"      'backward-delete-char-untabify)
  (global-unset-key "${ESC}[")
  (global-set-key "${ESC}[15~"  'set-justification-left)   ; <f5>
  (global-set-key "${ESC}[17~"  'set-justification-center) ; <f6>
  (global-set-key "${ESC}[18~"  'set-justification-right)  ; <f7>
  (global-set-key "${ESC}[19~"  'disabled)                 ; <f8>
  (global-set-key "${ESC}[20~"  'disabled)                 ; <f9>
  (global-set-key "${ESC}[21~"  'disabled)                 ; <f10>
  (global-set-key "${ESC}[23~"  'disabled)                 ; <f11>
  (global-set-key "${ESC}[24~"  #'copilot-accept-completion-by-paragraph) ; <f12>

  ;; Symbolic bindings that the input-decode-map block below will
  ;; translate the corresponding escape sequences into.
  (global-set-key (kbd "<f5>")   'set-justification-left)
  (global-set-key (kbd "<f6>")   'set-justification-center)
  (global-set-key (kbd "<f7>")   'set-justification-right)
  (global-set-key (kbd "<f8>")   'disabled)
  (global-set-key (kbd "<f9>")   'disabled)
  (global-set-key (kbd "<f10>")  'disabled)
  (global-set-key (kbd "<f11>")  'disabled)
  (global-set-key (kbd "<f12>")  #'copilot-accept-completion-by-paragraph)
  (global-set-key (kbd "<home>") 'beginning-of-buffer)
  (global-set-key (kbd "<end>")  'end-of-buffer)

  ;; xterm modifier suffix encoding:
  ;;   2 = S    3 = M    4 = M-S
  ;;   5 = C    6 = C-S  7 = C-M    8 = C-M-S
  (let ((modifiers '((2 . "S-")
                     (3 . "M-")
                     (4 . "M-S-")
                     (5 . "C-")
                     (6 . "C-S-")
                     (7 . "C-M-")
                     (8 . "C-M-S-")))
        ;; ESC [ <letter>          and   ESC [ 1 ; <mod> <letter>
        (csi-letter-keys '(("A" . up)
                           ("B" . down)
                           ("C" . right)
                           ("D" . left)
                           ("H" . home)
                           ("F" . end)))
        ;; ESC O <letter>          and   ESC O 1 ; <mod> <letter>
        ;; SS3 form: DECCKM application-cursor mode and F1-F4.
        (ss3-letter-keys '(("P" . f1)
                           ("Q" . f2)
                           ("R" . f3)
                           ("S" . f4)
                           ("A" . up)
                           ("B" . down)
                           ("C" . right)
                           ("D" . left)
                           ("H" . home)
                           ("F" . end)))
        ;; ESC [ <n> ~             and   ESC [ <n> ; <mod> ~
        (tilde-keys      '(( 1 . home)
                           ( 2 . insert)
                           ( 3 . delete)
                           ( 4 . end)
                           ( 5 . prior)
                           ( 6 . next)
                           ( 7 . home)
                           ( 8 . end)
                           (11 . f1)
                           (12 . f2)
                           (13 . f3)
                           (14 . f4)
                           (15 . f5)
                           (17 . f6)
                           (18 . f7)
                           (19 . f8)
                           (20 . f9)
                           (21 . f10)
                           (23 . f11)
                           (24 . f12)
                           (25 . f13)
                           (26 . f14)
                           (28 . f15)
                           (29 . f16)
                           (31 . f17)
                           (32 . f18)
                           (33 . f19)
                           (34 . f20)))
        sym)
    (dolist (k csi-letter-keys)
      (define-key input-decode-map (concat "\\M-[" (car k))
                  (vector (cdr k)))
      (dolist (m modifiers)
        (setq sym (intern (format "%s%s" (cdr m) (cdr k))))
        (define-key input-decode-map
                    (format "\\M-[1;%d%s" (car m) (car k))
                    (vector sym))))
    (dolist (k ss3-letter-keys)
      (define-key input-decode-map (concat "\\M-O" (car k))
                  (vector (cdr k)))
      (dolist (m modifiers)
        (setq sym (intern (format "%s%s" (cdr m) (cdr k))))
        (define-key input-decode-map
                    (format "\\M-O1;%d%s" (car m) (car k))
                    (vector sym))))
    (dolist (k tilde-keys)
      (define-key input-decode-map
                  (format "\\M-[%d~" (car k))
                  (vector (cdr k)))
      (dolist (m modifiers)
        (setq sym (intern (format "%s%s" (cdr m) (cdr k))))
        (define-key input-decode-map
                    (format "\\M-[%d;%d~" (car k) (car m))
                    (vector sym)))))
  nil)
EOF

# Preserve any trailing newline structure: $old ends with "  nil)\n",
# replacement also ends with "  nil)\n" -- good.

my $new_data = substr($data, 0, $start) . $new . substr($data, $end);

open my $out, '>:raw', $path or die "write $path: $!";
print $out $new_data;
close $out;

printf "rewrote %d bytes -> %d bytes (delta %+d)\n",
    length($data), length($new_data), length($new_data) - length($data);
