" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/vimwiki.vim	[[[1
185
" Vim filetype plugin file
" Language:     Wiki
" Maintainer:   Maxim Kim (habamax at gmail dot com)
" Home:         http://code.google.com/p/vimwiki/
" Author:       Maxim Kim
" Filenames:    *.wiki
" Last Change:  (04.05.2008 17:45)
" Version:      0.1

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1  " Don't load another plugin for this buffer

" Reset the following options to undo this plugin.
let b:undo_ftplugin = "setl tw< wrap< lbr< fenc< ff< sua< isf< awa<"

setlocal textwidth=0
setlocal wrap
setlocal linebreak
setlocal fileencoding=utf-8
setlocal fileformat=unix
setlocal autowriteall
" for gf
setlocal suffixesadd=.wiki
setlocal isfname-=[,]


"" Defaults
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:default(varname,value)
  if !exists('g:vimwiki_'.a:varname)
    let g:vimwiki_{a:varname} = a:value
  endif
endfunction

call s:default('index',"")
call s:default('home',"")
call s:default('upper','A-ZА-Я')
call s:default('lower','a-zа-я')
call s:default('other','0-9_')
call s:default('ext','.wiki')
call s:default('history',[])

let upp = g:vimwiki_upper
let low = g:vimwiki_lower
let oth = g:vimwiki_other
let nup = low.oth
let nlo = upp.oth
let any = upp.nup

let g:vimwiki_word1 = '['.upp.']['.nlo.']*['.low.']['.nup.']*['.upp.']['.any.']*'
let g:vimwiki_word2 = '\[\[['.upp.low.oth.'[:punct:][:space:]]\+\]\]'

let s:wiki_word = '\C\<'.g:vimwiki_word1.'\>\|'.g:vimwiki_word2


"" Functions {{{
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:SearchWord(wikiRx,cmd)
    let hl = &hls
    let lasts = @/
    let @/ = a:wikiRx
    set nohls
    try
        :silent exe 'normal ' a:cmd
    catch /Pattern not found/
        echoh WarningMsg
        echo "No WikiWord found."
        echoh None
    endt
    let @/ = lasts
    let &hls = hl
endfunction

function! s:WikiNextWord()
    call s:SearchWord(s:wiki_word, 'n')
endfunction

function! s:WikiPrevWord()
    call s:SearchWord(s:wiki_word, 'N')
endfunction

function! s:WikiGetWordAtCursor(wikiRX)
    let col = col('.') - 1
    let line = getline('.')
    let ebeg = -1
    let cont = match(line, a:wikiRX, 0)
    while (ebeg >= 0 || (0 <= cont) && (cont <= col))
        let contn = matchend(line, a:wikiRX, cont)
        if (cont <= col) && (col < contn)
            let ebeg = match(line, a:wikiRX, cont)
            let elen = contn - ebeg
            break
        else
            let cont = match(line, a:wikiRX, contn)
        endif
    endwh
    if ebeg >= 0
        return strpart(line, ebeg, elen)
    else
        return ""
    endif
endf

function! s:WikiStripWord(word)
    let result = a:word
    if strpart(a:word, 0, 2) == "[["
        let result = strpart(a:word, 2, strlen(a:word)-4)
    endif
    return result
endfunction


if !exists('*s:WikiFollowWord')
    function! s:WikiFollowWord()
        let word = s:WikiStripWord(s:WikiGetWordAtCursor(s:wiki_word))
        " insert doesn't work properly inside :if. Check :help :if.
        if word == ""
            execute "normal! \n"
            return
        endif
        " history is [['WikiWord.wiki', 11], ['AnotherWikiWord', 3] ... etc]
        " where numbers are column positions we should return when coming back.
        call insert(g:vimwiki_history, [expand('%:p'), col('.')])
        execute ":e ".g:vimwiki_home.word.g:vimwiki_ext
    endfunction

    function! s:WikiGoBackWord()
        if len(g:vimwiki_history) > 0
            let word = remove(g:vimwiki_history, 0)
            " go back to saved WikiWord
            execute ":e ".get(word, 0)
            call cursor(line('.'), get(word,1))
        endif
    endfunction
endif

function! s:WikiNewLine()
    let prevline = getline(line('.')-1)

    if prevline =~ '^\s*\*'
        let curline = substitute(getline('.'),'^\s\+',"","g")
        if prevline =~ '^\s*\*\s*$'
            " there should be easier way ...
            execute 'normal kA '."\<ESC>".'"_dF*JX'
            return
        endif
        call setline(line('.'), '* '.curline)
        execute "normal =="
        let ind = indent(line('.')) + 3
        call cursor(line('.'), ind)
        return
    endif

    " delete <space>
    execute 'normal X'
endfunction


" Functions }}}

"" Keybindings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <buffer> <Up>   gk
nmap <buffer> k      gk
vmap <buffer> <Up>   gk
vmap <buffer> k      gk

nmap <buffer> <Down> gj
nmap <buffer> j      gj
vmap <buffer> <Down> gj
vmap <buffer> j      gj

imap <buffer> <Down>   <C-o>gj
imap <buffer> <Up>     <C-o>gk

nmap <buffer> <CR> :call <SID>WikiFollowWord()<CR>
nmap <buffer> <BS> :call <SID>WikiGoBackWord()<CR>

nmap <buffer> <TAB> :call <SID>WikiNextWord()<CR>
nmap <buffer> <S-TAB> :call <SID>WikiPrevWord()<CR>

inoremap <CR> <CR> <C-O>:call <SID>WikiNewLine()<CR>
syntax/vimwiki.vim	[[[1
129
" Vim syntax file
" Language:     Wiki
" Maintainer:   Maxim Kim (habamax at gmail dot com)
" Home:         http://code.google.com/p/vimwiki/
" Author:       Maxim Kim
" Filenames:    *.wiki
" Last Change:  (04.05.2008 17:45)
" Version:      0.1
" Based on FlexWiki

" Quit if syntax file is already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" A WikiWord (unqualifiedWikiName)
execute 'syntax match wikiWord /'.g:vimwiki_word1.'/'
" A [bracketed wiki word]
execute 'syntax match wikiWord /'.g:vimwiki_word2.'/'

" text: "this is a link (optional tooltip)":http://www.microsoft.com
" TODO: check URL syntax against RFC
syntax match wikiLink           `\("[^"(]\+\((\([^)]\+\))\)\?":\)\?\(https\?\|ftp\|gopher\|telnet\|file\|notes\|ms-help\):\(\(\(//\)\|\(\\\\\)\)\+[A-Za-z0-9:#@%/;$~_?+-=.&\-\\\\]*\)`

" text: *strong*
syntax match wikiBold           /\(^\|\W\)\zs\*\([^ ].\{-}\)\*/
" '''bold'''
syntax match wikiBold           /'''\([^'].\{-}\)'''/

" text: _emphasis_
syntax match wikiItalic         /\(^\|\W\)\zs_\([^ ].\{-}\)_/
" ''italic''
syntax match wikiItalic         /''\([^'].\{-}\)''/

" ``deemphasis``
syntax match wikiDeEmphasis     /``\([^`].\{-}\)``/

" text: @code@ 
syntax match wikiCode           /\(^\|\s\|(\|\[\)\zs@\([^@]\+\)@/

"   text: -deleted text-
syntax match wikiDelText        /\(^\|\s\+\)\zs-\([^ <a ]\|[^ <img ]\|[^ -].*\)-/

"   text: +inserted text+
syntax match wikiInsText        /\(^\|\W\)\zs+\([^ ].\{-}\)+/

"   text: ^superscript^
syntax match wikiSuperScript    /\(^\|\W\)\zs^\([^ ].\{-}\)^/

"   text: ~subscript~
syntax match wikiSubScript      /\(^\|\W\)\zs\~\([^ ].\{-}\)\~/

"   text: ??citation??
syntax match wikiCitation       /\(^\|\W\)\zs??\([^ ].\{-}\)??/

" Emoticons: must come after the Textilisms, as later rules take precedence
" over earlier ones. This match is an approximation for the ~70 distinct
" patterns that FlexWiki knows.
syntax match wikiEmoticons      /\((.)\|:[()|$@]\|:-[DOPS()\]|$@]\|;)\|:'(\)/

" Aggregate all the regular text highlighting into flexwikiText
syntax cluster wikiText contains=wikiItalic,wikiBold,wikiCode,wikiDeEmphasis,wikiDelText,wikiInsText,wikiSuperScript,wikiSubScript,wikiCitation,wikiLink,wikiWord,wikiEmoticons

" single-line WikiPropertys
syntax match wikiSingleLineProperty /^:\?[A-Z_][_a-zA-Z0-9]\+:/

" Header levels, 1-6
syntax match wikiH1             /^!.*$/
syntax match wikiH2             /^!!.*$/
syntax match wikiH3             /^!!!.*$/
syntax match wikiH4             /^!!!!.*$/
syntax match wikiH5             /^!!!!!.*$/
syntax match wikiH6             /^!!!!!!.*$/

" <hr>, horizontal rule
syntax match wikiHR             /^----.*$/

" Formatting can be turned off by ""enclosing it in pairs of double quotes""
syntax match wikiEscape         /"".\{-}""/

" Tables. Each line starts and ends with '||'; each cell is separated by '||'
syntax match wikiTable          /||/

" Treat all other lines that start with spaces as PRE-formatted text.
syntax match wikiPre            /^[ \t]\+.*$/

" Bulleted list items start with whitespace(s), then '*'
" syntax match wikiList           /^\s\+\(\*\|[1-9]\+0*\.\).*$/   contains=@wikiText
" highlight only bullets and digits.
syntax match wikiList           /^\s\+\(\*\|[1-9]\+0*\.\)/




" Link FlexWiki syntax items to colors
hi def link wikiH1                    Title
hi def link wikiH2                    wikiH1
hi def link wikiH3                    wikiH2
hi def link wikiH4                    wikiH3
hi def link wikiH5                    wikiH4
hi def link wikiH6                    wikiH5
hi def link wikiHR                    wikiH6
    
hi def wikiBold                       term=bold cterm=bold gui=bold
hi def wikiItalic                     term=italic cterm=italic gui=italic

hi def link wikiCode                  Statement
hi def link wikiWord                  Underlined

hi def link wikiEscape                Todo
hi def link wikiPre                   PreProc
hi def link wikiLink                  Underlined
hi def link wikiList                  Type
hi def link wikiTable                 Type
hi def link wikiEmoticons             Constant
hi def link wikiDelText               Comment
hi def link wikiDeEmphasis            Comment
hi def link wikiInsText               Constant
hi def link wikiSuperScript           Constant
hi def link wikiSubScript             Constant
hi def link wikiCitation              Constant

hi def link wikiSingleLineProperty    Identifier

let b:current_syntax="VimWiki"

" vim:tw=0:
