alias info='info --vi-keys'
alias svndiff='svn diff --diff-cmd=svn-diff-meld'
alias gitdiff='git diff --ext-diff'
alias memcache_stat='watch "echo stats | nc 127.0.0.1 11211"'

alias grep='grep --color=auto --exclude-dir=.svn'
alias fgrep='fgrep --color=auto --exclude-dir=.svn'
alias egrep='egrep --color=auto --exclude-dir=.svn'

alias e='nohup emacs . >/dev/null &'
alias E='sudo -e'

alias t="i3-sensible-terminal"
alias tt="tree -L 2"
alias ttt="tree -L 3"
alias tttt="tree -L 4"

alias tm="tmux attach"

alias update-org="cd $HOME/projects/org-mode && make update"