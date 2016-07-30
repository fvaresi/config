alias info='info --vi-keys'
alias svndiff='svn diff --diff-cmd=svn-diff-meld'
alias gitdiff='git diff --ext-diff'
alias memcache_stat='watch "echo stats | nc 127.0.0.1 11211"'

alias disable_webcam='sudo modprobe -r uvcvideo'
alias enable_webcam='sudo modprobe uvcvideo'

alias grep='grep --color=auto --exclude-dir=.svn'
alias fgrep='fgrep --color=auto --exclude-dir=.svn'
alias egrep='egrep --color=auto --exclude-dir=.svn'

alias e='emacsclient --no-wait'
alias E='sudo -e'

alias t="tree -L 1"
alias tt="tree -L 2"
alias ttt="tree -L 3"
alias tttt="tree -L 4"

alias tm="tmux attach"

alias update-org="cd $HOME/projects/org-mode && make update"