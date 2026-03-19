#!/bin/bash

export PATH="/opt/anaconda3/bin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"

if [ -x /opt/homebrew/bin/brew ] ; then
    eval "$(/opt/homebrew/bin/brew shellenv bash)"
fi

test -r /Users/pjb/.opam/opam-init/init.sh && . /Users/pjb/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
