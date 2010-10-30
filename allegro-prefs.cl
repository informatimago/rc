;; Allegro CL Options

;; -----------------------------------------------------------
;; The following three forms need to appear first in this file

:has-version-check

(common-lisp:in-package :common-lisp-user) 

(ide.base::verify-implementation-version
  "8.0 [Linux (x86)] (Jun 6, 2006 16:01)" "1.80.2.21")

;; -----------------------------------------------------------
;; Configuration
(ide.base::restore-configuration
    'ide:allow-component-overlap common-lisp:t
    'ide:ask-before-auto-saving common-lisp:t
    'ide:ask-for-action-at-ide-startup common-lisp:t
    'ide:backtrace-print-circle common-lisp:t
    'ide:backtrace-print-length 12
    'ide:backtrace-print-level 3
    'ide:cg-tree-font common-lisp:nil
    'ide:class-graph-font common-lisp:nil
    'ide:class-graph-initial-depth common-lisp:nil
    'ide:close-inactive-listeners common-lisp:nil
    'ide:close-project-close-editor-buffers common-lisp:nil
    'ide:code-file-pretty-print-columns 72
    'ide:comment-indent 55
    'ide:context-sensitive-default-path common-lisp:t
    'ide:debug-font common-lisp:nil
    'ide:debug-history-limit 100
    'ide:directory-dialog-avoids-network common-lisp:nil
    'ide:display-console common-lisp:nil
    'ide:display-extended-toolbar common-lisp:nil
    'ide:display-form-grid common-lisp:t
    'ide:display-status-bar common-lisp:t
    'ide:display-toolbars common-lisp:t
    'ide:distribution-directories common-lisp:nil
    'ide:editor-font common-lisp:nil
    'ide:editor-mode :host
    'ide:file-dialog-source-types
       (common-lisp:list (common-lisp:cons "Lisp files"
                                           "*.cl;*.lsp;*.lisp;*.jil")
                         (common-lisp:cons "All files" "*"))
    'ide:find-again-after-replace common-lisp:nil
    'ide:grid-x-spacing 8
    'ide:grid-y-spacing 8
    'ide:highlight-selected-editor-tab common-lisp:t
    'ide:ide-exterior
       (cg.base:make-box 0 0 924 618)
    'ide:ide-html-history common-lisp:nil
    'ide:ide-prompt "~&~
~@[[Current process: ~a]~%~]~
~@[[Current process focus: ~a]~%~]~
~:[~
     ~:[~
        ~2*~
     ~;~
        [~:*~d~:[~;c~]~:[~;i~]] ~
     ~]~
~;~
     [~:*~a] ~3*~
~]~
~a(~d): "
    'ide:incremental-search common-lisp:nil
    'ide:initial-package :cg-user
    'ide:initial-search-directories common-lisp:nil
    'ide:inspector-name-font common-lisp:nil
    'ide:inspector-style :single-click-without-underscores
    'ide:inspector-value-font common-lisp:nil
    'ide:lisp-message-print-length 20
    'ide:lisp-message-print-level 4
    'ide:maximize-ide common-lisp:t
    'ide:maximize-ide-background-window common-lisp:nil
    'ide:maximum-symbol-completion-choices 25
    'ide:min-pixels-between-widgets 4
    'ide.base:mozilla-library-path "/opt/mozilla/lib/"
    'ide:new-project-create-form common-lisp:nil
    'ide:new-project-show-editor common-lisp:t
    'ide:new-project-show-form common-lisp:nil
    'ide:new-project-show-project-manager common-lisp:t
    'ide:open-files-in-gnu-emacs common-lisp:nil
    'ide:open-project-action :compile
    'ide:open-project-show-files-in-editor common-lisp:nil
    'ide:open-project-show-project-manager common-lisp:t
    'ide:patch-reminder-interval 30
    'ide:patch-reminder-previous-time common-lisp:nil
    'ide:pretty-printer :reindent
    'ide:printer-font common-lisp:nil
    'ide:profiler-included-node-types
       '(:self-hits :multiple-children :external-user-symbols
         :internal-user-symbols :external-lisp-symbols
         :external-cg-symbols :winapi-symbols)
    'ide:project-parent-directory common-lisp:nil
    'ide:query-exit common-lisp:t
    'ide:query-os-exit common-lisp:nil
    'ide:recent
       '((:open-project) (:load-project) (:open-file) (:load-file) (:edit-definition))
    'ide:recent-limit 18
    'ide:run-project-action :save-compile
    'ide:save-options-on-exit common-lisp:t
    'ide:save-options-to-user-specific-file common-lisp:t
    'ide:scroll-while-tracing common-lisp:nil
    'ide:show-dialog-on-compiler-warnings common-lisp:t
    'ide:snap-to-components common-lisp:t
    'ide:snap-to-grid common-lisp:t
    'ide:standard-toolbar-icons
       '(:new :save :open :save-all :gap :incremental-compile :load
         :gap :new-project :run-project :new-form :stop)
    'ide:start-in-allegro-directory common-lisp:nil
    'ide:symbol-completion-searches-all-packages common-lisp:t
    'ide:use-cg-html-browser common-lisp:nil
    'ide:use-ide-background-window common-lisp:nil
    'ide:use-ide-parent-window common-lisp:t
    'ide:use-private-html-browser common-lisp:t
    'ide:warn-on-no-action-taken common-lisp:t
    'ide:window-configurations common-lisp:nil
    'cg:center-all-modal-dialogs-on-screen :on-owner
    'cg:cg-timer-interval 500
    'cg:clipboard-history-limit 40
    'cg:color-for-characters
       (cg.base:make-rgb :red 190 :green 0 :blue 0)
    'cg:color-for-comments cg.base:dark-green
    'cg:color-for-external-allegro-symbols cg.base:dark-magenta
    'cg:color-for-external-cg-symbols
       (cg.base:make-rgb :red 200 :green 100 :blue 0)
    'cg:color-for-external-cl-symbols
       (cg.base:make-rgb :red 0 :green 0 :blue 180)
    'cg:color-for-global-variables
       (cg.base:make-rgb :red 240 :green 0 :blue 0)
    'cg:color-for-strings
       (cg.base:make-rgb :red 190 :green 0 :blue 0)
    'cg:color-for-user-functions common-lisp:nil
    'cg:colorize-on-load-file common-lisp:nil
    'cg:colorize-on-typing common-lisp:t
    'cg:conserve-indentation common-lisp:nil
    'cg:custom-status-bar-font common-lisp:nil
    'cg:custom-tooltip-font common-lisp:nil
    'cg:default-height common-lisp:nil
    'cg:default-height-factor 0.5
    'cg:default-tab-height 30
    'cg:default-tab-width 96
    'cg:default-width common-lisp:nil
    'cg:default-width-factor 0.75
    'cg:drag-images common-lisp:nil
    'cg:file-selection-buffer-size 120000
    'cg:fixed-font
       (cg.base:make-font-ex common-lisp:nil "Adobe Courier" 12.5)
    'cg:menu-tooltip-delay 1500
    'cg:modal-dialog-margin 12
    'cg:multi-picture-button-scroll-interval 200
    'cg:offset-from-selected-window common-lisp:t
    'cg:parenthesis-matching-color cg.base:green
    'cg:parenthesis-matching-style :color-block
    'cg:pprint-plist-definers
       '((cg.pixmap:make-texture-info 1) (cg.menu:open-menu 4)
         (common-lisp:make-instance 2) (cg.base:make-window 2))
    'cg:pprint-plist-pairs-on-separate-lines common-lisp:nil
    'cg:private-html-browser-handle common-lisp:nil
    'cg:proportional-font
       (cg.base:make-font-ex common-lisp:nil "Sans" 10 '(:bold))
    'cg:show-tooltips common-lisp:t
    'cg:tooltip-delay 1000
    'cg:tooltip-vertical-offset 16
    'cg:use-cg-timer common-lisp:nil
    'cg:use-pixmap-handles common-lisp:t)

;; End of file
