" Vim utility script "YAPosting"
" 
" Plugin providing extended, reusable, comprehensive and smart USENET/E-Mail 
" support. 
"
" Version    : 0.2
" Author     : Pratz Bernard <Guyzmo@m0g.net>
" Target     : Vim
" Tested     : VIM - Vi IMproved 7.1
" Last update: Mon Dec 20 17:10:12 CET 2010
" TODO       : * Add a line cutter for OE-Quotefix when increasing the
"                QuoteLevel
"              * Improve the quote rectification by adding the space where it
"                belongs (`XX> ')
"                             ^
"              * Adding selection for justification to right and left
"              * Improve YAPConfPerl by checking if the package is not already
"                present
"              * Reformat quoted lines
"              * Reformat lines automatically line by line
"              * if you have any other good ideas, e-mail at <bernard@pratz.net>
"              * Make the OE Quoted text go where at the level of Quote it
"                belongs
"              * make a function who moves the selected text to the beginning
"                of the quote.
" License    : LGPL (which can be seen at <http://www.gnu.org/licenses/lgpl.txt>)
"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                                                             "
"       The following lines are folders, in order to use them (`:he fold') :  "
"   zo to unfold, zc to fold.                                                 "
"                                                                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""
"
"   DOCUMENTATION
"
"
" YAPosting was designed in order to have a simple, but reliable set of
"mappings and functions, to accelerate the Mailing and Posting processus. I did
"see on <http://www.vim.org> if there was any scripts already done, and except
"for Posting.vim (which is dedicated to GVim, not vim), I didn't find any good
"ones...

" NOTE
"
"...So here I come, with this script, and with some ideas I had, like reformatting
"Microsoft's Outlook Express quotes, and put them at the right place, like
"formatting E-Mails with justification, and margins (unlike this text is written :)
"and of course some other nice functions.
"
"Of course, It needs lots of improvement, and please, give me feedback, or
"patches in order to do this -- My E-Mail is <bernard@pratz.net>. As you can
"see the TODO list is still quite big, and there's still a lot of work to do,
"but this script works as is, and the priority would be to find the bugs.
"
"                   ``We've got to fulfill the book...''
"
"                                                 - Bob Marley
"
" I did want this set of function easily reusable to make it possible for
" other people to use this set of script, where I did a lot of work for small
" but useful functions. So do not hesitate to look at the source below, I
" tried to document it as much as possible, and adapt them to your needs.
"
" So, I said all, now, please take the time to look at the documentation 
" below, where it is explained how to install, use and set up this script.
"
" May the posting side of the Force be with you :)
"
""
"   INSTALLATION
"
"        To use this script, copy it  in  your  ~/.vim/plugins/,  install
"    Perl v5.8.3 and then type in your editor `:call YAPConfPerl()',  which
"    follows :

function! YAPConfPerl()
    exe "perl use CPAN;"
    exe "perl install Text::Format"
endfunction

"    or at least simply install the package `Text::Format' from the CPAN.
" WHAT DOES THIS SCRIPT ?

"  Actually, this script is a set of functions, easy to use and re-use :
"  
"  - Take Out Function :                                           (default: tt)
"
" This function takes out the paragraph where the cursor is and removes it.
" It does work on quoted and not quoted paragraphs.
"
"  - Reformat Function :                                           (default: tr)
"
" This function justifies a paragraph respecting the current Paging Setup of
" the script (see PAGE SETUP below).
"
"  - Clean Reply :                                  (defaults: tr|t<UP>|t<DOWN>)
"
" This function removes the quoted signature, the quoted empty lines, condense
" the quote signs and place the cursor where the user want it to be :
"
"   tr      : let the cursor at the same position
"   t<UP>   : move the cursor at the top of the citation
"   t<DOWN> : move the cursor at the end of the citation
"
" To know more about the functions, know what are the bugs and what are the
" feature I'm working on, have a look at the functions' headers.

"
""
" CHANGES
"
"* April, 8th 2004 * Version 0.1 ***********************************************
"
"  This is a little hack, more in the todo list than in the Code...
"
"*******************************************************************************
"
"   CONFIGURATION
"
"   You have to Configure variables in CONFIGURATION -> VARIABLES folder
"   below.
"
"
" PAGE SETUP 
"
"        The following shows the rules  that  I  choosed  to  format  the
"    e-mails. You can change any of those values to personnalize it.
"
"                                 80 [textwidth]
"<----------------------------------------------------------------------------->
"   <--> 4 [alinea]
"       Hee hee heh. Stop! What... is your name? It is 'Arthur', King of
"   the Britons. What... is your quest? To seek the Holy Grail.  What...
"   is the air-speed velocity of an unladen swallow? What do  you  mean?
"   An African or European swallow? Huh? I-- I don't know that.
"<->                                                                    <------>
" 4 [marginleft]                                            [marginright] 8
"
"

" KEY MAPPINGS
"
"       I choosed  simple  keymappings...  I  don't  know  any  mappings
"   beginning with 't', if it's already taken, please let me know,  I'll
"   choose something better for the defaults. 
"   
"   So here are the default keymaps :
"
"   - `tt' will Take Out the current paragraph (calling TakeOut() below)
"   - `tr' will reformat the current parapraph (calling Format() below)
"   - `tm' will reformat a quoted OE mail (calling OEQuoteFix() below)
"   - `tc' will do a complete cleaning of the mail (calling CleanReply()
"      below)
"   - `t<UP>' will clean the mail, and put the cursor at the top of the
"      citation
"   - `t<DOWN>' will clean the mail, and put the cursor at the end of the
"      citation
"   - `th' will toggle improved highlights
"
"   You can of course change them, by changing the values in the folder :
"   CONFIGURATION -> KEY MAPPINGS. To know more about the called functions
"   read the previous chapter about them.

""
" CONFIGURATION
""

" VARIABLES

" Some standard strings' pattern

" These are the headers we can find in a mail (still not used)
let s:GoodHeaders = 'Newsgroups:\|From:\|To:\|Cc:\|Bcc:\|Reply-To:\|Subject:\|Return-Path:\|Received:\|Date:\|Replied:'

" This is for testing the hostname. (still not used)
let s:host        = '%(%(\[%(\d{1,3}\.){3})|%(%([a-zA-Z0-9-]+\.)+))%(\a{2,4}|\d{1,3})\]?'

" This is a test for the e-mail address. (still not used)
let s:eml_add     = '\v[< (]@<=[0-9A-Za-z_-]+%(\.[0-9A-Za-z_-]+)*\@' . s:host . '\_[> )]@=\m'

" This is one of the standards verbs used
let s:verb        = '\%(wrote\|writes\|said\|says\|stated\|states\|typed\|opinion\|schrieb\|schreibt\|geschrieben\|a écrit\|dixit\|racont\|a dit\|disait\)'

" This is a MS quote intro :
let s:OEQuoteIntro = '-----\s*Original Message\s*-----'

let s:CutHereBeg = '--------8<----------------8<----------------8<---------------8<--------'
let s:CutHereEnd = '-------->8---------------->8---------------->8--------------->8--------'

""""
" You'd better configure this variables to fit your needs.

" If you want improved text highlight change the value to 1
let s:highlights = 0

" This is a more precise quote intro :
" This one does fit my needs...
let s:IntroQuote  = '^On.*'.s:verb.' :$'

" This is the quote intro which will replace MS OE's quote :
" Just move the FROM and DATE strings in the variable, in order to get no
" errors.
let s:QuoteIntro = 'On DATE, FROM said :'

"""
" These ones can be kept the same way.

" This is the quote regexp pattern which match a common quote sign.
"let g:QuoteRegexp = '[A-Za-z0-9]*[|>%]'
let g:QuoteRegexp = '>'

" This is the signature footer pattern.
let s:SignPattern = '-- '



""
" PAGE SETUP

let s:marginleft  = 0
let s:marginright = 8
let s:textwidth   = 80
let s:alinea      = 4

""
" KEY MAPPINGS

" Here I am defining here the mappings to use this script quickly and easily :)

" Call the Take Out function
"
let s:takeout  = "tt"

" Call the Reformat Function
"
let s:reformat = "tr"
let s:oequotef = "tm"
let s:cleanrep = "tc"
let s:cleanreu = "t<UP>"
let s:cleanred = "t<DOWN>"
let s:highligh = "th"
let s:cuthere = "tn"

""
" HIGHLIGHTS
"
function! HighLightenment()
    if s:highlights == 0
        let s:highlights = 1
    elseif s:highlights == 1
        let s:highlights = 0
        setl syn=none " TODO : put the same syntax file as it was before the
                      "        keystroke if there was one..
        match none
        return 0
    endif
    "let s:hi_notquoted = '/^\([\[A-Za-z0-9\]*\[|%>\]\s^V|]\)\@!\%(\%(.\%<'.s:textwidth.'v\)*\)\@>.\%(\n\zs\|\zs.*\)/'
    let s:hi_first     = '/\%>'. ( s:textwidth - s:marginright + 1 ) .'v.\+/'
    let s:hi_second     = '/\%>'. ( s:textwidth + 1 ) .'v.\+/'
    setl syn=mail
    exe ':syn match Search '.s:hi_first
    exe ':syn match Error '.s:hi_second
endfunction

"
"
""

"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" `` It was a dark and stormy night... ''                 "
"                               - Snoopy                  "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" CODE
"
" Some useful function for the latter ones...
"
"
" MISC functions
"

function! s:DoMappings()
    " It does just make the mappings
    exe "nmap ".s:takeout."  :call TakeOut()<CR>"
    exe "nmap ".s:reformat." :call FormatText()<CR>"
    exe "nmap ".s:oequotef." :call OEQuoteFix()<CR>"
    exe 'nmap '.s:cleanrep.' :call CleanReply("clean")<CR>'
    exe 'nmap '.s:cleanreu.' :call CleanReply("clean","cursatbeg")<CR>'
    exe 'nmap '.s:cleanred.' :call CleanReply("clean","cursatend")<CR>'
    exe 'nmap '.s:highligh.' :call HighLightenment()<CR>'
    exe 'nmap '.s:cuthere.'  O'.s:CutHereBeg.'<CR>'.s:CutHereEnd.'<ESC>^O'
    exe 'vmap '.s:cuthere.'  :s/\(\_.*\)/'.s:CutHereBeg.'\1'.s:CutHereEnd.'<CR>'
endfunction
function! s:EndOfQuotation()
    " Returns the linenumber following the quotation
    let i = 1
    while i <= line('$')
        if getline (i) =~ '^'.g:QuoteRegexp.'.*$' && getline (i + 1) !~ '^'.g:QuoteRegexp.'.*$'
            break
        endif
        let i = i + 1
    endwhile
    return i
endfunction

function! s:BegOfQuotation()
    " Returns the first line number of the quotation
    let i = 1
    while i <= line('$') && getline (i) !~ '^'.g:QuoteRegexp
        let i = i + 1
    endwhile
    return i
endfunction
    
" Activate Highlights...
if s:highlights == 1
    sil call HighLightenment()
endif

" Function to ask questions
function! s:Input(prom, def)
    " Returns the answer
    echohl Question
    let str = input(a:prom . ' [' . a:def . ']? ')
    echohl None
    return substitute(str, '^$', a:def, 'e')
endfunction

function! s:QuoteLevel(line)
    " Function who returns the quoted level of the line
    let i = 30
    let result = -1
    while i > 0
        sil exe a:line.'g/^\('.g:QuoteRegexp.'\s\+\)\{'.i.','.i.'\}/let result='.i
        if result >= 0
            return result
            break
        endif
        let i = i - 1
    endwhile
endfunction

" TODO : Reformat the quoted text to fit the width.             " TODO
function! s:IncreaseQuoteLevel(beg,end)
    " Increase Quotation Level '> >' => '> > >'
    exe a:beg.','.a:end.'s/\(\('.g:QuoteRegexp.'\s*\)\+\)/\1> /'
    " remove all spaces found before a quotesign
    " > >> > > >>> > becomes >>>>>>>>>
    exe '%s/\s\+\('.g:QuoteRegexp.'\)/>/g'
    " add the first space :
    " >>>>>>>>> becomes > >>>>>>>>
    exe '%s/^\('.g:QuoteRegexp.'\)>/\1 >/g'
endfunction
"
" ...Here begins the serious work ;)
"
"

"---------------------------------------------------------------------------
" Function:     CLeanReply()                                      TODO
" Purpose:      Delete signatures at the end of e-mail replies. But keep
"               your sig intact. (if mutt (or other MUA) had added it)
" Features:     * Condense quote signs (> >> >>> >> > => >>>>>>>>> )
"               * Clean blanck quotted lines
"               * Remove quoted signature blocks
"               * Put the cursor at the beginning of the quoted text
" Usage:        CleanReply() does a complete cleaning of the mail/post
"               CleanReply(condense) condenses the quote signs
"               CleanReply(blank) cleans the blank quoted lines
"               CleanReply(sig) removes the signature
"               CleanReply(all) does all of the above
" Author:       Bernard PRATZ aka Guyzmo <bernard@pratz.net> 
"               based on Yann Kerhervé's <yk@cyberion.net> work
"               based on Luc Hermitte <hermitte@free.fr> work
" TODO:         * remove already quoted signatures ('>> -- ')
"               * make the function who removes signatures longer than 
"                 4 lines
"               * reformat cutted lines
"---------------------------------------------------------------------------
function! CleanReply(...)
    " Save cursor's position
    let s:curpos = line (".")
    exe 'norm m`'
    function! s:CleanBlankQuotedLines()
        " let's loop into the whole mail
        let i = 1
        while i <= line('$')
            " If the quoted line is empty
            while getline(i) =~ '^'.g:QuoteRegexp.'\s*$'
                " buuuuuurn !
                exe 'normal '.i.'GddG' 
            endwhile
            " If a quoted quoted (...) line is empty
            while getline(i) =~ '^\('.g:QuoteRegexp.'\s*\)\+$'
                " buuuurn !
                exe 'normal '.i.'GddG' 
            endwhile
            let i = i + 1
        endwhile
    endfunction   
    function! s:CondenseQuotesSign()
        " remove all spaces found before a quotesign
        " > >> > > >>> > becomes >>>>>>>>>
        exe '%s/\s\+\('.g:QuoteRegexp.'\)/>/g'
        " add the first space :
        " >>>>>>>>> becomes > >>>>>>>>
        exe '%s/^\('.g:QuoteRegexp.'\)>/\1 >/g'
    endfunction
    " If a signature is too long, this function'll be called
    function! s:TooLongSignatureDetected(line)                    " TODO
    " Make a variable to activate or not the radical mode..
    " Select the end of the signature
    " ask for destruction
    " destroy
    endfunction
    function! s:CleanSignature()
        " seek and destroy the signature
        " Firstable, seek the signature regexp
        let i = 1
        while i <= line('$')
            " if a signature has begun
            if getline(i) =~ '^\('.g:QuoteRegexp.'\s*\)\+\s*'.s:SignPattern.'$'
                let l:xx = 0
                " DESTROY IT RIGHT AWAY !! niark
                while getline(i) =~ '^\('.g:QuoteRegexp.'\s*\)\+.*$' && l:xx <= 4
                    exe 'normal '.i.'GddG'
                    let l:xx = l:xx + 1
                endwhile
                " If a signature is longer than 4 chars, ask the behaviour to
                " adopt...
                if l:xx == 42 && getline(i) =~ '^\('.g:QuoteRegexp.'\)\+.*$'
                    let behave = s:Input("Is Quoted signature longer than four character long ? ","no")
                    if l:behave =~ '[yY]'
                        let l:xx = 0
                        while getline(i) =~ '^\('.g:QuoteRegexp.'\)\+.*$' && i < line('$')
                            exe 'normal '.i.'GddG'
                            let l:xx = l:xx + 1 
                        endwhile
                    else
                        echohl "Quoted signature may be longer, and has not been erased."
                    endif
                endif
            endif
            let i = i + 1
        endwhile
    endfunction
    " Define the different behaviour
    let i = 1
    if a:0 >= 1
        while i <= a:0
            if a:{i} =~ "condense"
                sil call s:CondenseQuotesSign()
            elseif a:{i} =~ "blank"
                sil call s:CleanBlankQuotedLines()
            elseif a:{i} =~ "sig"
                sil call s:CleanSignature()
            elseif a:{i} =~ "clean"
                sil call s:CondenseQuotesSign()
                sil call s:CleanBlankQuotedLines()
                sil call s:CleanSignature()
            elseif a:{i} =~ "cursatend"
                let s:curpos = s:EndOfQuotation()
            elseif a:{i} =~ "cursatbeg"
                let s:curpos = s:BegOfQuotation()
            endif
            let i = i + 1
        endwhile
    else
        sil call s:CondenseQuotesSign()
        sil call s:CleanBlankQuotedLines()
        sil call s:CleanSignature()
    endif
    " Move the cursor
    call cursor (s:curpos,0)
    exe 'norm zz'
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---------------------------------------------------------------------------
" Function:     TakeOut()                                         TODO
" Purpose:      Takes out a paragraph (quoted or not)
"               
" Features:     * Ask a question and put the taking out reason in brakets
"               * finds out paragraphs in quoted context
"               * support up to 30 level of quotation
" TODO:         * change the QuoteLevel() algorithm, which is not
"                 optimum...
"               * make the selection visible to the user when selecting
"                 in a quote or selecting the first sentence of the paragraph
" Author:       Bernard PRATZ <bernard@pratz.net>
"---------------------------------------------------------------------------
function! TakeOut()
    function! s:BegQuotedParagraph()
    " Function to find the beginning of a quoted parapraph
        let curpos = line('.')
        let i = line('.')
        while i > 2
            if s:QuoteLevel(i-1) != s:QuoteLevel(i) || getline(i - 1) =~ '^\('.g:QuoteRegexp.'\)*\s*$'
                sil call cursor(curpos,0)
                return i
            endif
            let i = i - 1
        endwhile
    endfunction
    function! s:EndQuotedParagraph()
    " Function to find the end of a quoted paragraph
        let curpos = line('.')
        let i = line('.')
        while i < line('$')
            if s:QuoteLevel(i+1) != s:QuoteLevel(i) || getline(i + 1) =~ '^\('.g:QuoteRegexp.'\)*\s*$'
                sil call cursor(curpos,0)
                return i
            endif
            let i = i + 1
        endwhile
    endfunction
    " Function to remove a paragraph
    if getline('.') =~ '^\('.g:QuoteRegexp.'\)*\s*$'
        return -1
    elseif s:QuoteLevel(line('.')) == 0
        exe 'norm vip'
    else
        exe 'norm '.s:BegQuotedParagraph().'GV'.s:EndQuotedParagraph().'G'
    endif
    redraw
    let snippedtext = s:Input("Reason","snip")
    exe 'norm dO['.snippedtext.']'
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---------------------------------------------------------------------------
" Function:     OEQuoteFix()                                      TODO
" Purpose:      Requote MS OE malformed quotes...
" Features:     * Does Remove OE dark side of the force's quote header :
"# -----Original Message-----
"# From: Mr Foo Bar [foobar@barfoo.com]
"# Sent: Date
"# Subject:
"               and replaces it with :
"# On Date, Mr Foo Bar wrote:
"               * Does put the answer at the correct position (before the
"               answer)
"               * Does increment the quote level
" Usage:        OEQuotefix() and it will do everything :)
"
" BUG:          If the quotation is not regular and rigorous, it may
"               do a lot of crap...
"
" TODO:         Make a test to be sure if there's an OE quoted mail or not...
"               Apparently, sometimes OE Quoted text may already has a quote
"               level... so I don't have to add one more... has to be checked.
"
" Author:       Bernard PRATZ <bernard@pratz.net>
"---------------------------------------------------------------------------
function! OEQuoteFix()
    "" 
    " let's make the toys...

    " Firstable set some variables :
    " DebQuote : Beginning of the MS/OE citation
    " From     : Name of the quoted sender
    " Sent     : Date of the quoted mail
    " EndHead  : Ending of the MS/OE Citation header
    " EndQuote : Ending of the MS/OE citation
    let i = 1
    while i < line('$')
        if getline(i) =~ s:OEQuoteIntro
            let DebQuote = i
            let Quote = matchstr(getline(i),'^\('.g:QuoteRegexp.'\)')
            let j = i + 1
            while j < line('$')
                if getline(j) =~ '^\([A-Za-z0-9]*[>|%]\s*\)\+.*[A-Za-z0-9]\+:.*$'
                    if getline(j+1) =~ '^\('.g:QuoteRegexp.'\s*\)\+\s*$'
                        break
                    endif
                endif
                let j = j + 1
            endwhile
            let EndHead = j
        endif
        if getline(i) =~ 'From:'
            " Keep the name (has to be improved...)
            let From = substitute(getline(i),'^.*From:\s\+\(.*\)\s\+[<|]\?.*','\1','')
            " Remove trailing spaces
            let From = substitute(From,'\s*$','',"")
        endif
        if getline(i) =~ 'Sent:'
            let Date = matchstr(getline(i), '\([A-Za-z0-9,]\+\s\+\)\+') " <-'
            let Date = substitute(Date,'\s*$','',"")
        endif
        if s:QuoteLevel(i) != s:QuoteLevel(i + 1)
            let EndQuote = i
        endif
        let i = i + 1
    endwhile

    ""
    " ...let's play
    " TODO
    " I don't know how many joints did I smoke when I did 
    " it, but this function is bogus and has to be re-written
    "call s:IncreaseQuoteLevel(EndHead + 1,EndQuote)              TODO

    " Select the entire quotation
    exe 'norm '.DebQuote.'GV'.EndQuote.'Gd'
    " Move it where it belongs
    exe 'norm '.s:BegOfQuotation().'GP'
    " Change the variables
    let HeadLength = EndHead - DebQuote
    let DebQuote = s:BegOfQuotation()
    let EndHead = DebQuote + HeadLength
    " Replace Header
    exe 'norm '.DebQuote.'GV'.EndHead.'G'
    redraw
    exe 'norm dO'.Quote.' On '.Date.', '.From.' wrote :'
    " And now, your mail's soul has been saved... thanks who ? :)
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---------------------------------------------------------------------------
" Function:     FormatText()                                      TODO
" Purpose:      Reformat the typed text
"               
" Features:     * Add a tabulation at the beginning of every paragraph
"               * Add a margin to the right of the text
"               * Justify the text to the post's width with the Force's help.
"
" Usage:        FormatText()
" Author:       Bernard PRATZ <bernard@pratz.net>
"---------------------------------------------------------------------------
function! FormatText()
    function! BegParagraph(line)
        let i = a:line
        let intro = 0
        " If current line is empty, is a quote or is the introduction exit with error
        if getline(i) =~ g:QuoteRegexp || getline(i) =~ "^\s*$" || getline(i) =~ s:IntroQuote
            return 0
        endif
        " if the preceding line is a quote, or an empty line it's not a paragraph anymore
        while i > 1
            if getline(i - 1) =~ g:QuoteRegexp || getline(i - 1) =~ "^\s*$"
                break
            endif
            let i = i - 1
        endwhile
        " So we can return the value
        return i
    endfunction
    function! EndParagraph(line)
        let i = a:line
        let intro = 0
        " If current line is empty, is a quote or is the introduction exit with error
        if getline(i) =~ g:QuoteRegexp || getline(i) =~ "^\s*$" || getline(i) =~ s:IntroQuote
            return 0
        endif
        while i < line('$')
        " if the following line is a quote or an empty line, we're done...
            if getline(i + 1) =~ g:QuoteRegexp || getline(i + 1) =~ "^\s*$"
                break
            endif
            let i = i + 1
        endwhile
        " so we can return this value
        return i 
    endfunction
    function! s:IsInAParagraph(line)
    " Returns true if I'm in a paragraph
        if BegParagraph(a:line) == EndParagraph(a:line) || getline(BegParagraph(a:line)) =~ s:SignPattern
            return 0
        endif
        return 1
    endfunction
    function! Justify(beg,end)
        if a:beg == 0 || a:end == 0 || a:beg > a:end
            return 0
        endif

    "                                                             TODO
    " There's a bug in the output in some conditions... I still haven't found
    " where it is from... But I hope that Jah'll show me the way %)
    " Anyway, I'm on it, I want it clean
    " Of course, any help is welcome :)
    " Justifies the current paragraph respecting the first tabulation
    " and the left and right margins
        exe "perl use Text::Iconv;"
        exe "perl use Text::Format;"
        exe "perl my @line, $text, $i, $j;"
        exe "perl $text = new Text::Format;"
        exe "perl $text->columns(".s:textwidth.");"
        exe "perl $text->leftMargin(".s:marginleft.");"
        exe "perl $text->rightMargin(".s:marginright.");"
        exe "perl $text->firstIndent(".s:alinea.");"
        exe "perl $text->bodyIndent(0);"
        exe "perl $text->justify(1);"
        exe "perl @line = $curbuf->Get(".a:beg."..".a:end.");"
        exe "perl $converter = Text::Iconv->new('utf-8','iso8859-15');"
        exe "perl $_ = $converter->convert($_) for @line;"
        exe "norm ".a:beg."GV".a:end."Gd"
        exe "perl @line = $text->format(@line)"
        exe "perl $converter = Text::Iconv->new('iso8859-15','utf-8');"
        exe "perl $_ = $converter->convert($_) for @line;"
        exe "perl $curbuf->Append(".a:beg."-1,@line)"
        exe "perl undef @line, @lnum, $text, $i, $j;"
        exe "norm ".a:beg."Gvip"
        exe "%s/".nr2char(10)."//g"
        exe "norm "
    endfunction
    if s:IsInAParagraph(line('.'))
        sil call Justify(BegParagraph(line('.')),EndParagraph(line('.')))
    endif
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
fun! CompleteAddressBook(findstart, base)
    if a:findstart
        " locate the start of the word
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start - 1] =~ '\a'
            let start -= 1
        endwhile
        return start
    else
        " find months matching with "a:base"
        let res = []
        for m in sort(copy(eval(system('python /home/guyzmo/bin/list_ab.py /home/guyzmo/.abook/addressbook'))))
            if m =~ '^' . a:base
                call add(res, m)
            endif
        endfor
        return res
    endif
endfun
set completefunc=CompleteAddressBook
set omnifunc=CompleteAddressBook
inoremap <Nul> <C-x><C-o>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Let's init the mappings...
sil call s:DoMappings()

"
" `` This is the end... My only friend, the end... ''
"                               - Jim Morrison, The Doors
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
