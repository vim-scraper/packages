" Twitter with Vim
" Language: Vim Script
" Maintainer: Travis Jeffery
" Created: 14 January 2008
" Last Change: 18 February 2008

function s:Twitter()

let s:tweet = getline('.')

 call system("curl -u USER:PASS -d status=\"" . s:tweet . "\" http://twitter.com/statuses/update.xml")

 echo "Tweet successfully sent."

endfunction

command! PosttoTwitter :call <SID>Twitter()

