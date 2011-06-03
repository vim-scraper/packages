" Twitter with Vim
" Language: Vim Script
" Maintainer: Travis Jeffery <eatsleepgolf@gmail.com>
" Created: 14 January 2008
" Last Change: 26 Mar 2008
" GetLatestVimScripts: 2124 1 [:AutoInstall:] vimtwitter.vim 
" ==============================================================

function! s:Twitter()

    %s/"/\\"/ge                 " to replace quotes with \" so that Twitter will get them
    %s/&/\\&/ge                 " to replace ampersand characters
    let lines = getline(1, "$") " get the entire buffer
    let s:tweet = join(lines)   " put the lines together so that it's a string and not a list
    
    if strlen(s:tweet) > 140 
    
        echo "Your Tweet is too long and was not sent. It has" strlen(s:tweet) - 140 "too many characters."
    
    elseif strlen(s:tweet) <= 140
    
        call system("curl -u USER:PASS -d status=\"" . s:tweet . "\" http://twitter.com/statuses/update.xml?source=vim")
        echo "The Tweet successfully sent. You used" strlen(s:tweet) "characters."
    
    endif
endfunction

function! s:CurrentLine_Twitter()

let s:currentline = getline('.')

    if strlen(s:currentline) > 140 
    
        echo "Your Tweet is too long and was not sent. It has" strlen(s:currentline) - 140 "too many characters."
    
    elseif strlen(s:currentline) <= 140
    
        call system("curl -u USER:PASS -d status=\"" . s:currentline . "\" http://twitter.com/statuses/update.xml?source=vim")
        echo "The Tweet successfully sent. You used" strlen(s:currentline) "characters."
    
    endif
endfunction

function! s:CmdLine_Twitter()
    call inputsave()
    let s:cmdline_twitter = input("Your Twitter: ")
    call inputrestore()

    if strlen(s:cmdline_twitter) > 140 
    
        echo "Your Tweet is too long and was not sent. It has" strlen(s:cmdline_twitter) - 140 "too many characters."
    
    elseif strlen(s:cmdline_twitter) <= 140
    
        call system("curl -u USER:PASS -d status=\"" . s:cmdline_twitter . "\" http://twitter.com/statuses/update.xml?source=vim")
        echo "The Tweet successfully sent. You used" strlen(s:cmdline_twitter) "characters."
    
    endif
endfunction

command! PosttoTwitter :call <SID>CmdLine_Twitter()
command! CPosttoTwitter :call <SID>CurrentLine_Twitter()
command! BPosttoTwitter :call <SID>Twitter()
vmap T y:tabnew<CR>p:BPosttoTwitter<CR>:tabclose<CR>
