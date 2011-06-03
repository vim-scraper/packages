" Twitter with Vim
" Language: Vim Script
" Maintainer: Travis Jeffery
" Created: 14 January 2008
" Last Change: 19 February 2008

function s:Twitter()

    %s/"/\\"/ge
    let lines = getline(1, "$")
    let s:tweet = join(lines)
    
    if strlen(s:tweet) > 140
    
        echo "Your Tweet is too long and was not sent. It has" strlen(s:tweet) - 140 "too many characters."
    
    elseif strlen(s:tweet) <= 140
    
        call system("curl -u travisjeffery:n1c3guy -d status=\"" . s:tweet . "\" http://twitter.com/statuses/update.xml")
        echo "The Tweet successfully sent. You used" strlen(s:tweet) "characters."
    
    endif
endfunction

command! PosttoTwitter :call <SID>Twitter()
