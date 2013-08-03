alias info='info --vi-keys'
alias backup_rename="rename -v 's/(IMG|VID)_(\d{8})_(\d{6})\.(jpg|3gp)/$2$3\.$4/' *"
alias svndiff='svn diff --diff-cmd=svn-diff-meld'
alias gitdiff='git diff --ext-diff'
alias memcache_stat='watch "echo stats | nc 127.0.0.1 11211"'
