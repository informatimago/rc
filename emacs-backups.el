(setq backup-by-copying   t             ; don't clobber symlinks
      backup-directory-alist '(("/" . "~/.backups"))
                                        ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions   5
      kept-old-versions   3
      version-control     t)


