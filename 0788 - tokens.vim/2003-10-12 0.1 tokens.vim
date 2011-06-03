" tokens.vim:   an extension of the built in abbreviation
"               function to allow input at specified 
"               characters in a string.
" Last Change:  Sun 12 Oct 2003
" Author:       Mark Woodward <markwoodward@bigpond.com>
" Version:      0.1              
" Requires:     vimlist.vim

" Description:  This file allows the use of token characters
"               within an abbreviation. These are based on digraphs
"               and are as follows:
               
"           ¤   replaced each time with new input   

"           •   prompted the first time then replaced with
"               previous •                          digraph <C-K>MW                

"           Þ  the place where the cursor will end up.
"              Doesn't work when this is the last char for
"              some reason?                        digraph <C-K>TH
              
"          « » the prompt...**MUST be 15 chars INCLUDING the « »**. 
"              If not included you are prompted with a 
"              generic 'replace ¤ with what?' prompt.
              
"           ¸  Used to represent commas in a quote. Vimlist
"              doesn't like them.                  digraph <C-K>',   

" TODO:     Problems:   1. Why cant Þ be the last character?
"                       2. how to find Þ 
"                          ie looking across lines?


if exists("loaded_tokens")
    finish
endif

let loaded_tokens = 1
let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>TokensExp_Token')
    imap <unique> <Leader>et  <Plug>TokensExp_Token
endif

inoremap <unique> <script> <Plug>TokensExp_Token  <SID>Exp_Token
" inoremenu <script> Tokens.Expand          <SID>Exp_Token
inoremap <SID>Exp_Token        <c-o>:call <SID>Exp_Token()<CR>

function s:Exp_Token()

    " Initializations  
    let s:vartoken = "¤" 
    let s:str = expand("<cword>")   " current word
    let s:len = strlen(s:str)       " current word length
    let s:cpos = 0                  " switch for Þ
    let s:repstr = ""               " our replacement string
    let s:i = 0                     " a simple counter    
    let s:promptstr = ""            " prompt for ¤
    let s:mw_str = ""               " prompt for •
    let s:mlines = 0                " ? multiple lines
    
    " you can either set up a string like this and then
    " use s:table for eg in vimlist, or type the string 
    " directly in the array..
    let s:table =           "tbz <table width=\"¤« table width »\" cellspacing=\"¤« cellspacing »\"" 
    let s:table = s:table . " cellpadding=\"¤« cellpadding »\">Þ</table>"
    
    let s:head  =           "html <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
    let s:head  = s:head .  "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    let s:head  = s:head .  "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
    let s:head  = s:head .  "<head>"
    let s:head  = s:head .  "<title>¤« page title  »</title>"
    let s:head  = s:head .  "<meta name=\"keywords\" content=\"¤« keywords    »\" />"
    let s:head  = s:head .  "<meta name=\"description\" content=\"¤« description »\" />"
    let s:head  = s:head .  "<meta name=\"author\" content=\"¤« author      »\" />"
    let s:head  = s:head .  "<meta name=\"generator\" content=\"¤« generator   »\" />"
    let s:head  = s:head .  "</head>"
    let s:head  = s:head .  "<body>Þ</body>"
    let s:head  = s:head .  "</html>"

    " Replacement strings -- 
    " Using liblist.vim to add the replacement strings. 
    " The first word will act as the 'trigger' so *has* to be
    " unique..(like :ab trigger replacement)
    let instr = ""
    let instr = AddListItem(instr, 'this is the ¤ string', 0)
    let instr = AddListItem(instr, 'dog dog day afternoon', 1)
    let instr = AddListItem(instr, 'cat cat on a Þ hot ¤«roof material» roof', 2)
    let instr = AddListItem(instr, 'dic <div class=\"¤« class name  »\">Þ</div>', 3)
    let instr = AddListItem(instr, 'if if(¤«  condition  »){Þ}', 4)
    let instr = AddListItem(instr, 'for for(¤«expression 1 »; ¤« condition   »; ¤«expression 2 »){Þ}', 5)
    let instr = AddListItem(instr, 'dii <div id=\"¤«  id name    »\">Þ</div>', 6)
    let instr = AddListItem(instr, 'spc <span class=\"¤« class name  »\">Þ</span>', 7)
    let instr = AddListItem(instr, 'tag <•«  tag name   »>Þ</•>', 8)
    let instr = AddListItem(instr, 'img <img src=\"¤« image src   »\" width=\"¤« img width   »\" height=\"¤« img height  »\" alt=\"¤«  alt text   »\" />', 9)
    let instr = AddListItem(instr, 'h1c <h1 class=\"¤« class name  »\">Þ</h1>', 10)
    let instr = AddListItem(instr, 'hd <h•«header level?»>Þ</h•>', 11)
    let instr = AddListItem(instr, 'hdc <h•«header level?» class=\"¤« class name  »\">Þ</h•>', 12)
    let instr = AddListItem(instr, "hpv $HTTP_POST_VARS['$¤'];", 13)
    let instr = AddListItem(instr, "cdb @ $db = mysql_pconnect('¤«  host name  »'¸ '¤«  user name  »'¸ '¤«  user pwd   »');", 14)
    let instr = AddListItem(instr, "sdb mysql_select_db('¤« dbase name  »');", 15)
    let instr = AddListItem(instr, "mqdb mysql_query($query);", 16)
    let instr = AddListItem(instr, "mqr $result = mysql_query($query);", 17)
    let instr = AddListItem(instr, "ech echo \"¤«echo what?   »\";", 18)
    let instr = AddListItem(instr, s:table, 19)
    let instr = AddListItem(instr, s:head, 20)

    " check to see if s:str matches anywhere..
    let s:matchString = GetListMatchItem(instr, s:str)

    " if theres a match..
    if (s:matchString != "")

        " check if cword is exactly like the first 
        " word of our matched string..
        if(matchstr(s:matchString, '^\w\+') == s:str) 

            " how long is the liblist listing?
            let s:mstrlen=strlen(s:matchString)

            " while there's more characters in the string, build 
            " up the replacement string (repstr).
            while(s:i < (s:mstrlen))        
                " if the current character is the ¤ token..
                if(strpart(s:matchString,s:i,1) == s:vartoken)
                    if (strpart(s:matchString,s:i+1,1) == "«")
                        let s:prompt = strpart(s:matchString, s:i+1, 15)
                        let s:promptstr = input(s:prompt)
                        let s:repstr = s:repstr . s:promptstr
                        let s:i = s:i + 15
                    else
                        let s:promptstr = input("replace token with?")
                        let s:repstr = s:repstr . s:promptstr
                    endif  

                    " this defeats the need for '\\/' in the quotes above 
                elseif(strpart(s:matchString,s:i,1) == "/")
                    let s:repstr = s:repstr . "\\" . strpart(s:matchString,s:i,1)
                
                    " liblist doesn't like commas so use <C-k>', digraph '¸'
                    " in the quotes above instead. This is replaced with a ','
                elseif(strpart(s:matchString,s:i,1) == "¸")
                    let s:repstr = s:repstr . ","
                elseif(strpart(s:matchString,s:i,1) == "")
                    let s:repstr = s:repstr . strpart(s:matchString,s:i,1)
                    let s:mlines = 1
                    " if the cursor positioning Þ is in the string..
                elseif(strpart(s:matchString,s:i,1) == "Þ")
                    " add it to the repstr but set s:cpos so we can delete
                    " it and move to the spot it occurred once finished..
                    let s:repstr = s:repstr . strpart(s:matchString,s:i,1)
                    let s:cpos = 1
                elseif(strpart(s:matchString,s:i,1) == "•")
                    if(s:mw_str == "")
                        if(strpart(s:matchString,s:i+1,1) == "«")
                            let s:prompt = strpart(s:matchString, s:i+1, 15)
                            let s:mw_str = input(s:prompt)
                            let s:i = s:i + 15
                        else
                            let s:mw_str = input("replace • with? ") 
                        endif
                    endif
                    let s:repstr = s:repstr . s:mw_str
                    
                else
                    " if it's some other char, just use it as is in repstr
                    let s:repstr = s:repstr . strpart(s:matchString,s:i,1)
                endif
                let s:i = s:i + 1
            endwhile

            " we now have the replacement string but it starts with 
            " the 'trigger' text. So we need to get rid of this.
            let s:repstr = strpart(s:repstr, s:len + 1)

            " replace the trigger with the replacement string
            execute 's/' . s:str . '/' . s:repstr . '/'

            " Cursor placement afterwards
            " if the cursor positioning Þ was present..
            " -- this would be good if it worked across lines ;-) --
            if(s:cpos==1)
                if(s:mlines == 1)
                    normal gg
                    execute '%s/Þ//'
                else
                    normal fÞxa
                endif
            " or if it wasn't go to a new line below?
            else
                norm o
            endif
        endif
    endif
endfunction

let &cpo = s:save_cpo
