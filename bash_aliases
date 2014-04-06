alias info='info --vi-keys'
alias backup_rename="rename -v 's/(IMG|VID)_(\d{8})_(\d{6})\.(jpg|3gp)/$2$3\.$4/' *"
alias svndiff='svn diff --diff-cmd=svn-diff-meld'
alias gitdiff='git diff --ext-diff'
alias memcache_stat='watch "echo stats | nc 127.0.0.1 11211"'

alias kb_us='setxkbmap us -variant intl -option "ctrl:nocaps"'
alias kb_es='setxkbmap es -option "ctrl:nocaps"'

alias disable_webcam='sudo modprobe -r uvcvideo'
alias enable_webcam='sudo modprobe uvcvideo'

alias grep='grep --color=auto --exclude-dir=.svn --exclude="*.sql"'
alias fgrep='fgrep --color=auto --exclude-dir=.svn --exclude="*.sql"'
alias egrep='egrep --color=auto --exclude-dir=.svn --exclude="*.sql"'

alias e='emacsclient -nw'
alias E='sudo -e'
