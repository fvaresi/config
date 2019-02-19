#!/usr/bin/env zsh

RPROMPT='%{$fg[yellow]%}%@%{$reset_color%}'

bgnotify_end() {
  didexit=$?
  elapsed=$(( EPOCHSECONDS - bgnotify_timestamp ))
  past_threshold=$(( elapsed >= bgnotify_threshold ))
  if (( bgnotify_timestamp > 0 )) && (( past_threshold )); then
      print -n "\a"
      bgnotify_formatted "$didexit" "$bgnotify_lastcmd" "$elapsed"
  fi
  bgnotify_timestamp=0 #reset it to 0!
}
