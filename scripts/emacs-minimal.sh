#!/bin/sh
exec emacs --init-directory $HOME/.emacs.d/test/ "$@"
