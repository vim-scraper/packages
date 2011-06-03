" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/vimwiki.vim	[[[1
59
" VimWiki plugin file
" Language:     Wiki
" Author:       Maxim Kim (habamax at gmail dot com)
" Home:         http://code.google.com/p/vimwiki/
" Filenames:    *.wiki
" Last Change:  (02.06.2008 12:57)
" Version:      0.4


if exists("loaded_vimwiki") || &cp
  finish
endif
let loaded_vimwiki = 1

let s:save_cpo = &cpo
set cpo&vim


function! s:default(varname,value)
  if !exists('g:vimwiki_'.a:varname)
    let g:vimwiki_{a:varname} = a:value
  endif
endfunction

"" Could be redefined by users
call s:default('home',"")
call s:default('index',"index")
call s:default('ext','.wiki')
call s:default('upper','A-ZА-Я')
call s:default('lower','a-zа-я')
call s:default('maxhi','1')
call s:default('other','0-9_')
call s:default('smartCR',1)
call s:default('stripsym','_')
call s:default('home_html',g:vimwiki_home."html/")
" call s:default('addheading','1')

call s:default('history',[])

let upp = g:vimwiki_upper
let low = g:vimwiki_lower
let oth = g:vimwiki_other
let nup = low.oth
let nlo = upp.oth
let any = upp.nup

let g:vimwiki_word1 = '\C\<['.upp.']['.nlo.']*['.low.']['.nup.']*['.upp.']['.any.']*\>'
let g:vimwiki_word2 = '\[\[['.upp.low.oth.'[:punct:][:space:]]\{-}\]\]'

"" TODO: common regexps for syntax hiliting
"" regexps
call s:default('rxWeblink', '\("[^"(]\+\((\([^)]\+\))\)\?":\)\?\(https\?\|ftp\|gopher\|telnet\|file\|notes\|ms-help\):\(\(\(//\)\|\(\\\\\)\)\+[A-Za-z0-9:#@%/;$~_?+-=.&\-\\\\]*\)')
call s:default('rxWikiWord', g:vimwiki_word1.'\|'.g:vimwiki_word2)
call s:default('rxCode', '`.\{-}`')

execute 'autocmd! BufNewFile,BufReadPost,BufEnter *'.g:vimwiki_ext.' set ft=vimwiki'

nmap <silent><unique> <Leader>ww :call vimwiki#WikiGoHome()<CR>
nmap <silent><unique> <Leader>wh :execute "edit ".g:vimwiki_home."."<CR>
autoload/vimwiki.vim	[[[1
923
" VimWiki plugin file
" Language:     Wiki
" Author:       Maxim Kim (habamax at gmail dot com)
" Home:         http://code.google.com/p/vimwiki/
" Filenames:    *.wiki
" Last Change:  (02.06.2008 12:57)
" Version:      0.4

if exists("g:loaded_vimwiki_auto") || &cp
 finish
endif
let g:loaded_vimwiki_auto = 1

let s:wiki_badsymbols = '[<>|?*/\:"]'

"" vimwiki functions {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:msg(message) "{{{
    echohl WarningMsg
    echomsg 'vimwiki: '.a:message
    echohl None
endfunction "}}}

function! s:getFileNameOnly(filename) "{{{
    let word = substitute(a:filename, '\'.g:vimwiki_ext, "", "g")
    let word = substitute(word, '.*[/\\]', "", "g")
    return word
endfunction "}}}

function! s:editfile(command, filename) "{{{
    let fname = escape(a:filename, '% ')
    execute a:command.' '.fname

    " if fname is new
    " if g:vimwiki_addheading!=0 && glob(fname) == ''
    " execute 'normal I! '.s:getfilename(fname)
    " update
    " endif
endfunction "}}}

function! s:SearchWord(wikiRx,cmd) "{{{
    let hl = &hls
    let lasts = @/
    let @/ = a:wikiRx
    set nohls
    try
        :silent exe 'normal ' a:cmd
    catch /Pattern not found/
        call s:msg('WikiWord not found')
    endt
    let @/ = lasts
    let &hls = hl
endfunction "}}}

function! s:WikiGetWordAtCursor(wikiRX) "{{{
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
endf "}}}

function! s:WikiStripWord(word, sym) "{{{
    function! s:WikiStripWordHelper(word, sym)
        return substitute(a:word, s:wiki_badsymbols, a:sym, 'g')
    endfunction

    let result = a:word
    if strpart(a:word, 0, 2) == "[["
        let result = s:WikiStripWordHelper(strpart(a:word, 2, strlen(a:word)-4), a:sym)
    endif
    return result
endfunction "}}}

function! s:WikiIsLinkToNonWikiFile(word) "{{{
    " Check if word is link to a non-wiki file.
    " The easiest way is to check if it has extension like .txt or .html
    if a:word =~ '\.\w\{1,4}$'
        return 1
    endif
    return 0
endfunction "}}}

"" WikiWord history helper functions {{{
" history is [['WikiWord.wiki', 11], ['AnotherWikiWord', 3] ... etc]
" where numbers are column positions we should return when coming back.
function! s:GetHistoryWord(historyItem)
    return get(a:historyItem, 0)
endfunction
function! s:GetHistoryColumn(historyItem)
    return get(a:historyItem, 1)
endfunction
"}}}

function! vimwiki#WikiNextWord() "{{{
    call s:SearchWord(g:vimwiki_rxWikiWord, 'n')
endfunction "}}}

function! vimwiki#WikiPrevWord() "{{{
    call s:SearchWord(g:vimwiki_rxWikiWord, 'N')
endfunction "}}}

function! vimwiki#WikiFollowWord(split) "{{{
    if a:split == "split"
        let cmd = ":split "
    elseif a:split == "vsplit"
        let cmd = ":vsplit "
    else
        let cmd = ":e "
    endif
    let word = s:WikiStripWord(s:WikiGetWordAtCursor(g:vimwiki_rxWikiWord), g:vimwiki_stripsym)
    " insert doesn't work properly inside :if. Check :help :if.
    if word == ""
        execute "normal! \n"
        return
    endif
    if s:WikiIsLinkToNonWikiFile(word)
        call s:editfile(cmd, word)
    else
        call insert(g:vimwiki_history, [expand('%:p'), col('.')])
        call s:editfile(cmd, g:vimwiki_home.word.g:vimwiki_ext)
    endif
endfunction "}}}

function! vimwiki#WikiGoBackWord() "{{{
    if !empty(g:vimwiki_history)
        let word = remove(g:vimwiki_history, 0)
        " go back to saved WikiWord
        execute ":e ".s:GetHistoryWord(word)
        call cursor(line('.'), s:GetHistoryColumn(word))
    endif
endfunction "}}}

function! vimwiki#WikiNewLine(direction) "{{{
    "" direction == checkup - use previous line for checking
    "" direction == checkdown - use next line for checking
    function! s:WikiAutoListItemInsert(listSym, dir)
        let sym = escape(a:listSym, '*')
        if a:dir=='checkup'
            let linenum = line('.')-1
        else
            let linenum = line('.')+1
        end
        let prevline = getline(linenum)
        if prevline =~ '^\s\+'.sym
            let curline = substitute(getline('.'),'^\s\+',"","g")
            if prevline =~ '^\s*'.sym.'\s*$'
                " there should be easier way ...
                execute 'normal kA '."\<ESC>".'"_dF'.a:listSym.'JX'
                return 1
            endif
            let ind = indent(linenum)
            call setline(line('.'), strpart(prevline, 0, ind).a:listSym.' '.curline)
            call cursor(line('.'), ind+3)
            return 1
        endif
        return 0
    endfunction

    if s:WikiAutoListItemInsert('*', a:direction)
        return
    endif

    if s:WikiAutoListItemInsert('#', a:direction)
        return
    endif

    " delete <space>
    if getline('.') =~ '^\s\+$'
        execute 'normal x'
    else
        execute 'normal X'
    endif
endfunction "}}}

function! vimwiki#WikiHighlightWords() "{{{
    let wikies = glob(g:vimwiki_home.'*')
    "" remove .wiki extensions
    let wikies = substitute(wikies, '\'.g:vimwiki_ext, "", "g")
    let g:vimwiki_wikiwords = split(wikies, '\n')
    "" remove paths
    call map(g:vimwiki_wikiwords, 'substitute(v:val, ''.*[/\\]'', "", "g")')
    "" remove backup files (.wiki~)
    call filter(g:vimwiki_wikiwords, 'v:val !~ ''.*\~$''')

    for word in g:vimwiki_wikiwords
        if word =~ g:vimwiki_word1 && !s:WikiIsLinkToNonWikiFile(word)
            execute 'syntax match wikiWord /\<'.word.'\>/'
        else
            execute 'syntax match wikiWord /\[\['.substitute(word,  g:vimwiki_stripsym, s:wiki_badsymbols, "g").'\]\]/'
        endif
    endfor
endfunction "}}}

function! vimwiki#WikiGoHome()"{{{
    execute ':e '.g:vimwiki_home.g:vimwiki_index.g:vimwiki_ext
    let g:vimwiki_history = []
endfunction"}}}

function! vimwiki#WikiDeleteWord() "{{{
    "" file system funcs
    "" Delete WikiWord you are in from filesystem
    let val = input('Delete ['.expand('%').'] (y/n)? ', "")
    if val!='y'
        return
    endif
    let fname = expand('%:p')
    " call WikiGoBackWord()
    try
        call delete(fname)
    catch /.*/
        call s:msg('Cannot delete "'.expand('%:r').'"!')
        return
    endtry
    execute "bdelete! ".escape(fname, " ")

    " delete from g:vimwiki_history list
    call filter (g:vimwiki_history, 's:GetHistoryWord(v:val) != fname')
    " as we got back to previous WikiWord - delete it from history - as much
    " as possible
    let hword = s:GetHistoryWord(remove(g:vimwiki_history, 0))
    while !empty(g:vimwiki_history) && hword == s:GetHistoryWord(g:vimwiki_history[0])
        let hword = s:GetHistoryWord(remove(g:vimwiki_history, 0))
    endwhile

    " reread buffer => deleted WikiWord should appear as non-existent
    execute "e"
endfunction "}}}

function! vimwiki#WikiRenameWord() "{{{
    "" Rename WikiWord, update all links to renamed WikiWord
    let wwtorename = expand('%:r')
    let isOldWordComplex = 0
    if wwtorename !~ g:vimwiki_word1
        let wwtorename = substitute(wwtorename,  g:vimwiki_stripsym, s:wiki_badsymbols, "g")
        let isOldWordComplex = 1
    endif

    " there is no file (new one maybe)
    if glob(g:vimwiki_home.expand('%')) == ''
        call s:msg('Cannot rename "'.expand('%').'". It does not exist!')
        return
    endif

    let val = input('Rename "'.expand('%:r').'" (y/n)? ', "")
    if val!='y'
        return
    endif
    let newWord = input('Enter new name: ', "")
    " check newWord - it should be 'good', not empty
    if substitute(newWord, '\s', '', 'g') == ''
        call s:msg('Cannot rename to an empty filename!')
        return
    endif
    if s:WikiIsLinkToNonWikiFile(newWord)
        call s:msg('Cannot rename to a filename with extension (ie .txt .html)!')
        return
    endif

    if newWord !~ g:vimwiki_word1
        " if newWord is 'complex wiki word' then add [[]]
        let newWord = '[['.newWord.']]'
    endif
    let newFileName = s:WikiStripWord(newWord, g:vimwiki_stripsym).g:vimwiki_ext

    " do not rename if word with such name exists
    let fname = glob(g:vimwiki_home.newFileName)
    if fname != ''
        call s:msg('Cannot rename to "'.newFileName.'". File with that name exist!')
        return
    endif
    " rename WikiWord file
    try
        call rename(expand('%'), newFileName)
        bd
        "function call doesn't work
        call s:editfile('e', newFileName)
    catch /.*/
        call s:msg('Cannot rename "'.expand('%:r').'" to "'.newFileName.'"')
        return
    endtry

    " save open buffers
    let openbuffers = []
    let bcount = 1
    while bcount<=bufnr("$")
        if bufexists(bcount)
            call add(openbuffers, bufname(bcount))
        endif
        let bcount = bcount + 1
    endwhile

    " update links
    execute ':args '.g:vimwiki_home.'*'.g:vimwiki_ext
    if isOldWordComplex
        execute ':silent argdo %s/\[\['.wwtorename.'\]\]/'.newWord.'/geI | update'
    else
        execute ':silent argdo %s/\<'.wwtorename.'\>/'.newWord.'/geI | update'
    endif
    execute ':argd *'.g:vimwiki_ext

    " restore open buffers
    let bcount = 1
    while bcount<=bufnr("$")
        if bufexists(bcount)
            if index(openbuffers, bufname(bcount)) == -1
                execute 'silent bdelete '.escape(bufname(bcount), " ")
            end
        endif
        let bcount = bcount + 1
    endwhile

    "" DONE: after renaming GUI caption is a bit corrupted?
    "" FIXED: buffers menu is also not in the "normal" state, howto Refresh menu?
    execute "emenu Buffers.Refresh\ menu"

endfunction "}}}

" Functions 2}}}

"" vimwiki html functions {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:WikiCreateDefaultCSS(path) " {{{
    if glob(a:path.'style.css') == ""
        let lines = ['body { margin: 1em 5em 1em 5em; font-size: 100%; }']
        call add(lines, 'h1 {font-size: 2.0em;}')
        call add(lines, 'h2 {font-size: 1.4em;}')
        call add(lines, 'h3 {font-size: 1.0em;}')
        call add(lines, 'h4 {font-size: 0.8em;}')
        call add(lines, 'h5 {font-size: 0.7em;}')
        call add(lines, 'h6 {font-size: 0.6em;}')
        call add(lines, 'h1 { border-bottom: 1px solid #3366cc; text-align: right; padding: 0.3em 1em 0.3em 1em; }')
        call add(lines, 'h3 { background: #e5ecf9; border-top: 1px solid #3366cc; padding: 0.1em 0.3em 0.1em 0.5em; }')
        call add(lines, 'ul { margin-left: 2em; padding-left: 0.5em; }')
        call add(lines, 'pre { border-left: 0.2em solid #ccc; margin-left: 2em; padding-left: 0.5em; }')
        call add(lines, 'td { border: 1px solid #ccc; padding: 0.3em; }')
        call add(lines, 'hr { border: none; border-top: 1px solid #ccc; }')

        call writefile(lines, a:path.'style.css')
        echomsg "Default style.css created."
    endif
endfunction "}}}

function! vimwiki#WikiAll2HTML(path) "{{{
    if !isdirectory(a:path)
        call s:msg('Please create '.a:path.' directory first!')
        return
    endif
    let wikifiles = split(glob(g:vimwiki_home.'*'.g:vimwiki_ext), '\n')
    for wikifile in wikifiles
        echo 'Processing '.wikifile
        call vimwiki#Wiki2HTML(a:path, wikifile)
    endfor
    call s:WikiCreateDefaultCSS(g:vimwiki_home_html)
    echomsg 'Wikifiles converted.'
endfunction "}}}

function! vimwiki#Wiki2HTML(path, wikifile) "{{{
    if !isdirectory(a:path)
        call s:msg('Please create '.a:path.' directory first!')
        return
    endif

    "" helper funcs
    function! s:isLinkToPic(lnk) "{{{
        if a:lnk =~ '.\(png\|jpg\|gif\|jpeg\)$'
            return 1
        endif
        return 0
    endfunction "}}}

    function! s:HTMLHeader(title, charset) "{{{
        let lines=[]
        call add(lines, "")
        call add(lines, '<html>')
        call add(lines, '<head>')
        call add(lines, '<link rel="Stylesheet" type="text/css" href="style.css" />')
        call add(lines, '<title>'.a:title.'</title>')
        call add(lines, '<meta http-equiv="Content-Type" content="text/html; charset='.a:charset.'" />')
        call add(lines, '</head>')
        call add(lines, '<body>')
        return lines
    endfunction "}}}

    function! s:HTMLFooter() "{{{
        let lines=[]
        call add(lines, "")
        call add(lines, '</body>')
        call add(lines, '</html>')
        return lines
    endfunction "}}}

    function! s:closeCode(code, ldest) "{{{
        if a:code
            call add(a:ldest, "</pre></code>")
            return 0
        endif
        return a:code
    endfunction "}}}

    function! s:closePre(pre, ldest) "{{{
        if a:pre
            call add(a:ldest, "</pre>")
            return 0
        endif
        return a:pre
    endfunction "}}}

    function! s:closeTable(table, ldest) "{{{
        if a:table
            call add(a:ldest, "</table>")
            return 0
        endif
        return a:table
    endfunction "}}}

    function! s:closeList(lists, ldest) "{{{
        while len(a:lists)
            let item = remove(a:lists, -1)
            call add(a:ldest, item[0])
        endwhile
    endfunction! "}}}

    function! s:processCode(line, code) "{{{
        let lines = []
        let code = a:code
        let processed = 0
        if !code && a:line =~ '^{{{\s*$'
            let code = 1
            call add(lines, "<code><pre>")
            let processed = 1
        elseif code && a:line =~ '^}}}\s*$'
            let code = 0
            call add(lines, "</pre></code>")
            let processed = 1
        elseif code
            let processed = 1
            call add(lines, a:line)
        endif
        return [processed, lines, code]
    endfunction "}}}

    function! s:processPre(line, pre) "{{{
        let lines = []
        let pre = a:pre
        let processed = 0
        if a:line =~ '^\s\+[^[:blank:]*#]'
            if !pre
                call add(lines, "<pre>")
                let pre = 1
            endif
            let processed = 1
            call add(lines, a:line)
        elseif pre && a:line =~ '^\s*$'
            let processed = 1
            call add(lines, a:line)
        elseif pre 
            call add(lines, "</pre>")
            let pre = 0
        endif
        return [processed, lines, pre]
    endfunction "}}}

    function! s:processList(line, lists) "{{{
        let lines = []
        let lstSym = ''
        let lstTagOpen = ''
        let lstTagClose = ''
        let lstRegExp = ''
        let processed = 0
        if a:line =~ '^\s\+\*'
            let lstSym = '*'
            let lstTagOpen = '<ul>'
            let lstTagClose = '</ul>'
            let lstRegExp = '^\s\+\*'
            let processed = 1
        elseif a:line =~ '^\s\+#' 
            let lstSym = '#'
            let lstTagOpen = '<ol>'
            let lstTagClose = '</ol>'
            let lstRegExp = '^\s\+#'
            let processed = 1
        endif
        if lstSym != ''
            let indent = stridx(a:line, lstSym)
            let cnt = len(a:lists)
            if !cnt || (cnt && indent > a:lists[-1][1])
                call add(a:lists, [lstTagClose, indent])
                call add(lines, lstTagOpen)
            elseif (cnt && indent < a:lists[-1][1])
                while indent < a:lists[-1][1]
                    let item = remove(a:lists, -1)
                    call add(lines, item[0])
                endwhile
            endif
            call add(lines, '<li>'.substitute(a:line, lstRegExp, '', '').'</li>')
        else
            while len(a:lists)
                let item = remove(a:lists, -1)
                call add(lines, item[0])
            endwhile
        endif
        return [processed, lines]
    endfunction "}}}

    function! s:processP(line) "{{{
        let lines = []
        if a:line =~ '^\S'
            call add(lines, '<p>'.a:line.'</p>')
            return [1, lines]
        endif
        return [0, lines]
    endfunction "}}}

    function! s:processHeading(line) "{{{
        let line = a:line
        let processed = 0
        if a:line =~ '^!!!!!!.*$'
            let line = '<h6>'.strpart(a:line, 6).'</h6>'
            let processed = 1
        elseif a:line =~ '^!!!!!.*$'
            let line = '<h5>'.strpart(a:line, 5).'</h5>'
            let processed = 1
        elseif a:line =~ '^!!!!.*$'
            let line = '<h4>'.strpart(a:line, 4).'</h4>'
            let processed = 1
        elseif a:line =~ '^!!!.*$'
            let line = '<h3>'.strpart(a:line, 3).'</h3>'
            let processed = 1
        elseif a:line =~ '^!!.*$'
            let line = '<h2>'.strpart(a:line, 2).'</h2>'
            let processed = 1
        elseif a:line =~ '^!.*$'
            let line = '<h1>'.strpart(a:line, 1).'</h1>'
            let processed = 1
        endif
        if processed
            return [processed, line]
        endif

        "" dumb test if Tagit has made changes to line
        let lenbefore = len(line)
        let line = s:Tagit(a:line, '^=\{6}.*=\{6}\s*$', '<h6>', '</h6>', 6)
        if len(line)!=lenbefore
            let processed = 1
        endif

        if !processed
            let lenbefore = len(line)
            let line = s:Tagit(a:line, '^=\{5}.*=\{5}\s*$', '<h5>', '</h5>', 5)
            if len(line)!=lenbefore
                let processed = 1
            endif
        endif
        if !processed
            let lenbefore = len(line)
            let line = s:Tagit(a:line, '^=\{4}.*=\{4}\s*$', '<h4>', '</h4>', 4)
            if len(line)!=lenbefore
                let processed = 1
            endif
        endif
        if !processed
            let lenbefore = len(line)
            let line = s:Tagit(a:line, '^=\{3}.*=\{3}\s*$', '<h3>', '</h3>', 3)
            if len(line)!=lenbefore
                let processed = 1
            endif
        endif
        if !processed
            let lenbefore = len(line)
            let line = s:Tagit(a:line, '^=\{2}.*=\{2}\s*$', '<h2>', '</h2>', 2)
            if len(line)!=lenbefore
                let processed = 1
            endif
        endif
        if !processed
            let lenbefore = len(line)
            let line = s:Tagit(a:line, '^=\{1}.*=\{1}\s*$', '<h1>', '</h1>', 1)
            if len(line)!=lenbefore
                let processed = 1
            endif
        endif

        return [processed, line]
    endfunction "}}}

    function! s:processHR(line) "{{{
        let line = a:line
        let processed = 0
        if a:line =~ '^-----*$'
            let line = '<hr />'
            let processed = 1
        endif
        return [processed, line]
    endfunction "}}}

    function! s:processTable(line, table) "{{{
        let table = a:table
        let lines = []
        let processed = 0
        if a:line =~ '^||.\+||.*'
            if !table
                call add(lines, "<table>")
                let table = 1
            endif
            let processed = 1

            call add(lines, "<tr>")
            let pos1 = 0
            let pos2 = 0
            let done = 0
            while !done
                let pos1 = stridx(a:line, '||', pos2)
                let pos2 = stridx(a:line, '||', pos1+2)
                if pos1==-1 || pos2==-1
                    let done = 1
                    let pos2 = len(a:line)
                endif
                let line = strpart(a:line, pos1+2, pos2-pos1-2)
                if line != ''
                    call add(lines, "<td>".line."</td>")
                endif
            endwhile
            call add(lines, "</tr>")

        elseif table
            call add(lines, "</table>")
            let table = 0
        endif
        return [processed, lines, table]
    endfunction "}}}

    "" yes I know it is dumb to name variables like that ;)
    function! s:Linkit(line, regexp, regexp2) "{{{
        let line = a:line
        let done = 0
        let posend = -1
        let posbeg = 0
        let posbeg2 = 0
        while !done
            let posbeg = match(a:line, a:regexp, posend+1)
            let posbeg2 = match(a:line, a:regexp2, posend+1)
            if (posbeg < posbeg2 && posbeg!=-1) || (posbeg2==-1 && posbeg!=-1)
                let str = matchstr(a:line, a:regexp, posbeg)
                if str != ''
                    let word = s:WikiStripWord(str, g:vimwiki_stripsym)

                    let ext = '.html'
                    let path = ''
                    if s:WikiIsLinkToNonWikiFile(word)
                        let ext = ''
                        " let path = g:vimwiki_home
                    endif

                    if s:isLinkToPic(word)
                        let strrep = '<img src="'.path.word.'" />'
                    else
                        let strrep = '<a href="'.path.word.ext.'">'.word.'</a>'
                    endif
                    let line = substitute(line, escape(str,'[]*?\'), strrep, "")
                    let posend = posbeg+len(str)
                else
                    let done = 1
                endif
            elseif posbeg2 != -1
                let str = matchstr(a:line, a:regexp2, posbeg2)
                if str != ''
                    if s:isLinkToPic(str)
                        let strrep = '<img src="'.str.'" />'
                    else
                        let strrep = '<a href="'.str.'">'.str.'</a>'
                    endif
                    let line = substitute(line, str, strrep, "")
                    let posend = posbeg2+len(str)
                else
                    let done = 1
                endif
            else
                let done = 1
            endif
        endwhile
        return line
    endfunction "}}}

    function! s:processLink(line) "{{{
        let line = ''
        let done = 0

        let pos0 = 0
        let pos1 = 0
        let pos2 = 0
        while !done
            let pos1 = match(a:line, g:vimwiki_rxCode, pos2)
            let pos2 = matchend(a:line, g:vimwiki_rxCode, pos1)
            if pos1!=-1 && pos2!=-1
                let line = line.s:Linkit(strpart(a:line, pos0, pos1-pos0), g:vimwiki_rxWikiWord, g:vimwiki_rxWeblink)
                let line = line.strpart(a:line, pos1, pos2-pos1)
                let pos0 = pos2
            else
                let line = line.s:Linkit(strpart(a:line, pos0, len(a:line)-pos0), g:vimwiki_rxWikiWord, g:vimwiki_rxWeblink)
                let done = 1
            endif
        endwhile

        return line
    endfunction "}}}

    "" TODO: processTODO(line)
    
    "" change dangerous html symbols - < > & (line)
    function! s:safeHTML(line) "{{{
        let line = substitute(a:line, '&', '\&amp;', 'g')
        let line = substitute(line, '<', '\&lt;', 'g')
        let line = substitute(line, '>', '\&gt;', 'g')
        return line
    endfunction "}}}

    function! s:Tagit(line, regexp, tagOpen, tagClose, cSymRemove) "{{{
        let line = a:line
        let done = 0
        while !done
            let str = matchstr(line, a:regexp)
            if str != ''
                let strrep = a:tagOpen.strpart(str, a:cSymRemove, len(str)-2*a:cSymRemove).a:tagClose
                let line = substitute(line, a:regexp, escape(strrep, '[]*.?&\'), "")
            else
                let done = 1
            endif
        endwhile
        return line
    endfunction "}}}

    function! s:replacePairs(line, regexp, tagOpen, tagClose) "{{{
        let line = ''
        let done = 0

        if a:regexp == g:vimwiki_rxCode
            let line = s:Tagit(a:line, a:regexp, a:tagOpen, a:tagClose, 1)
        else
            let pos0 = 0
            let pos1 = 0
            let pos2 = 0
            while !done
                "" TODO: improve it, improve it, improve it...
                let pos1 = match(a:line, g:vimwiki_rxCode, pos2)
                let tpos1 = match(a:line, g:vimwiki_rxWeblink, pos2)
                let ttpos1 = match(a:line, g:vimwiki_rxWikiWord, pos2)
                let pos2 = matchend(a:line, g:vimwiki_rxCode, pos1)
                let tpos2 = matchend(a:line, g:vimwiki_rxWeblink, tpos1)
                let ttpos2 = matchend(a:line, g:vimwiki_rxWikiWord, ttpos1)
                if (pos1 < tpos1)
                    let pos1 = tpos1
                endif
                if (pos1 < ttpos1)
                    let pos1 = ttpos1
                endif
                let pos2 = max([pos2, tpos2, ttpos2])

                if pos1!=-1 && pos2!=-1
                    let line = line.s:Tagit(strpart(a:line, pos0, pos1-pos0), a:regexp, a:tagOpen, a:tagClose, 1)
                    let line = line.strpart(a:line, pos1, pos2-pos1)
                    let pos0 = pos2
                else
                    let line = line.s:Tagit(strpart(a:line, pos0, len(a:line)-pos0), a:regexp, a:tagOpen, a:tagClose, 1)
                    let done = 1
                endif
            endwhile
        endif

        return line
    endfunction "}}}

    let lsource=readfile(a:wikifile)
    let ldest = s:HTMLHeader(s:getFileNameOnly(a:wikifile), &encoding)

    let pre = 0
    let code = 0
    let table = 0
    let lists = []

    for line in lsource
        let processed = 0
        let lines = []

        let line = s:safeHTML(line)

        "" code
        if !processed
            let [processed, lines, code] = s:processCode(line, code)
            if processed && len(lists)
                call s:closeList(lists, ldest)
            endif
            if processed && table
                let table = s:closeTable(table, ldest)
            endif
            if processed && pre
                let pre = s:closePre(pre, ldest)
            endif
            call extend(ldest, lines)
        endif

        "" Pre
        if !processed
            let [processed, lines, pre] = s:processPre(line, pre)
            if processed && len(lists)
                call s:closeList(lists, ldest)
            endif
            if processed && table
                let table = s:closeTable(table, ldest)
            endif
            if processed && code
                let code = s:closeCode(code, ldest)
            endif
            call extend(ldest, lines)
        endif


        "" TODO: `code` do not emphasize or bold it.
        "" list
        if !processed
            let [processed, lines] = s:processList(line, lists)
            if processed && pre
                let pre = s:closePre(pre, ldest)
            endif
            if processed && code
                let code = s:closeCode(code, ldest)
            endif
            if processed && table
                let table = s:closeTable(table, ldest)
            endif
            call map(lines, 's:replacePairs(v:val, ''_.\{-}_'', ''<em>'', ''</em>'')')
            call map(lines, 's:replacePairs(v:val, ''\*.\{-}\*'', ''<strong>'', ''</strong>'')')
            call map(lines, 's:processLink(v:val)')
            call map(lines, 's:replacePairs(v:val, ''`.\{-}`'', ''<code>'', ''</code>'')')
            call extend(ldest, lines)
        endif

        "" table
        if !processed
            let [processed, lines, table] = s:processTable(line, table)
            call map(lines, 's:replacePairs(v:val, ''_.\{-}_'', ''<em>'', ''</em>'')')
            call map(lines, 's:replacePairs(v:val, ''\*.\{-}\*'', ''<strong>'', ''</strong>'')')
            call map(lines, 's:processLink(v:val)')
            call map(lines, 's:replacePairs(v:val, ''`.\{-}`'', ''<code>'', ''</code>'')')
            call extend(ldest, lines)
        endif

        if !processed
            let [processed, line] = s:processHeading(line)
            if processed
                call s:closeList(lists, ldest)
                let table = s:closeTable(table, ldest)
                let code = s:closeCode(code, ldest)
                call add(ldest, line)
            endif
        endif

        if !processed
            let [processed, line] = s:processHR(line)
            if processed
                call s:closeList(lists, ldest)
                let table = s:closeTable(table, ldest)
                let code = s:closeCode(code, ldest)
                call add(ldest, line)
            endif
        endif

        "" P
        if !processed
            let line = s:replacePairs(line, '_.\{-}_', '<em>', '</em>')
            let line = s:replacePairs(line, '\*.\{-}\*', '<strong>', '</strong>')
            let line = s:processLink(line)
            let line = s:replacePairs(line, '`.\{-}`', '<code>', '</code>')
            let [processed, lines] = s:processP(line)
            if processed && pre
                let pre = s:closePre(pre, ldest)
            endif
            if processed && code
                let code = s:closeCode(code, ldest)
            endif
            if processed && table
                let table = s:closeTable(table, ldest)
            endif
            call extend(ldest, lines)
        endif

        "" add the rest
        if !processed
            call add(ldest, line)
        endif
    endfor

    "" process end of file
    "" close opened tags if any
    call s:closePre(pre, ldest)
    call s:closeCode(code, ldest)
    call s:closeList(lists, ldest)
    call s:closeTable(table, ldest)


    call extend(ldest, s:HTMLFooter())

    "" make html file.
    "" TODO: add html headings, css, etc.
    let wwFileNameOnly = s:getFileNameOnly(a:wikifile)
    call writefile(ldest, a:path.wwFileNameOnly.'.html')
endfunction "}}}

" 2}}}
ftplugin/vimwiki.vim	[[[1
86
" Vim filetype plugin file
" Language:     Wiki
" Author:       Maxim Kim (habamax at gmail dot com)
" Home:         http://code.google.com/p/vimwiki/
" Filenames:    *.wiki
" Last Change:  (02.06.2008 12:58)
" Version:      0.4

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1  " Don't load another plugin for this buffer


"" Defaults
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Reset the following options to undo this plugin.
let b:undo_ftplugin = "setl tw< wrap< lbr< fenc< ff< sua< isf< awa< com< fo<"

setlocal textwidth=0
setlocal wrap
setlocal linebreak
setlocal fileencoding=utf-8
setlocal fileformat=unix
setlocal autowriteall
" for gf
execute 'setlocal suffixesadd='.g:vimwiki_ext
setlocal isfname-=[,]

if g:vimwiki_smartCR>=2
    setlocal comments=b:*,b:#
    setlocal formatoptions=ctnqro
endif

"" keybindings {{{
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

nmap <silent><buffer> <CR> :call vimwiki#WikiFollowWord('nosplit')<CR>
nmap <silent><buffer> <S-CR> :call vimwiki#WikiFollowWord('split')<CR>
nmap <silent><buffer> <C-CR> :call vimwiki#WikiFollowWord('vsplit')<CR>

nmap <buffer> <S-LeftMouse> <NOP>
nmap <buffer> <C-LeftMouse> <NOP>
noremap <silent><buffer> <2-LeftMouse> :call vimwiki#WikiFollowWord('nosplit')<CR>
noremap <silent><buffer> <S-2-LeftMouse> <LeftMouse>:call vimwiki#WikiFollowWord('split')<CR>
noremap <silent><buffer> <C-2-LeftMouse> <LeftMouse>:call vimwiki#WikiFollowWord('vsplit')<CR>

nmap <silent><buffer> <BS> :call vimwiki#WikiGoBackWord()<CR>
"<BS> mapping doesn't work in vim console
nmap <silent><buffer> <C-h> :call vimwiki#WikiGoBackWord()<CR>
nmap <silent><buffer> <RightMouse><LeftMouse> :call vimwiki#WikiGoBackWord()<CR>

nmap <silent><buffer> <TAB> :call vimwiki#WikiNextWord()<CR>
nmap <silent><buffer> <S-TAB> :call vimwiki#WikiPrevWord()<CR>

nmap <silent><buffer> <Leader>wd :call vimwiki#WikiDeleteWord()<CR>
nmap <silent><buffer> <Leader>wr :call vimwiki#WikiRenameWord()<CR>

if g:vimwiki_smartCR==1
    inoremap <silent><buffer><CR> <CR><Space><C-O>:call vimwiki#WikiNewLine('checkup')<CR>
    noremap <silent><buffer>o o<Space><C-O>:call vimwiki#WikiNewLine('checkup')<CR>
    noremap <silent><buffer>O O<Space><C-O>:call vimwiki#WikiNewLine('checkdown')<CR>
endif
" keybindings }}}

"" commands {{{2
" command! -nargs=1 Wiki2HTML call WikiExportHTML(expand(<f-args>))
command! Wiki2HTML call vimwiki#Wiki2HTML(g:vimwiki_home_html, expand('%'))
command! WikiAll2HTML call vimwiki#WikiAll2HTML(g:vimwiki_home_html)

"" commands 2}}}

syntax/vimwiki.vim	[[[1
121
" Vim syntax file
" Language:     Wiki
" Author:       Maxim Kim (habamax at gmail dot com)
" Home:         http://code.google.com/p/vimwiki/
" Filenames:    *.wiki
" Last Change:  (02.06.2008 12:58)
" Version:      0.4

" Quit if syntax file is already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

"" use max highlighting - could be quite slow if there are too many wikifiles
if g:vimwiki_maxhi
    " Every WikiWord is nonexistent
    execute 'syntax match wikiNoExistsWord /'.g:vimwiki_word1.'/'
    execute 'syntax match wikiNoExistsWord /'.g:vimwiki_word2.'/'
    " till we find them in g:vimwiki_home
    call vimwiki#WikiHighlightWords()
else
    " A WikiWord (unqualifiedWikiName)
    execute 'syntax match wikiWord /'.g:vimwiki_word1.'/'
    " A [[bracketed wiki word]]
    execute 'syntax match wikiWord /'.g:vimwiki_word2.'/'
endif


" text: "this is a link (optional tooltip)":http://www.microsoft.com
" TODO: check URL syntax against RFC
syntax match wikiLink           `\("[^"(]\+\((\([^)]\+\))\)\?":\)\?\(https\?\|ftp\|gopher\|telnet\|file\|notes\|ms-help\):\(\(\(//\)\|\(\\\\\)\)\+[A-Za-z0-9:#@%/;$~_?+-=.&\-\\\\]*\)`

" text: *strong*
" syntax match wikiBold           /\(^\|\W\)\zs\*\([^ ].\{-}\)\*/
" syntax match wikiBold           /\(^\|\W\)\zs\*.\{-}\*/
syntax match wikiBold           /\*.\{-}\*/

" text: _emphasis_
syntax match wikiItalic         /_.\{-}_/

" text: `code`
syntax match wikiCode           /`.\{-}`/

"   text: ~~deleted text~~
syntax match wikiDelText        /\~\{2}.\{-}\~\{2}/

"   text: ^superscript^
syntax match wikiSuperScript    /\^.\{-}\^/

"   text: ,,subscript,,
syntax match wikiSubScript      /,,.\{-},,/

" Emoticons: must come after the Textilisms, as later rules take precedence
" over earlier ones. This match is an approximation for the ~70 distinct
" patterns that FlexWiki knows.
syntax match wikiEmoticons      /\((.)\|:[()|$@]\|:-[DOPS()\]|$@]\|;)\|:'(\)/

" Aggregate all the regular text highlighting into wikiText
syntax cluster wikiText contains=wikiItalic,wikiBold,wikiCode,wikiDelText,wikiSuperScript,wikiSubScript,wikiLink,wikiWord,wikiEmoticons

" Header levels, 1-6
syntax match wikiH1             /\(^!\{1}.*$\|^\s*=\{1}.*=\{1}\s*$\)/
syntax match wikiH2             /\(^!\{2}.*$\|^\s*=\{2}.*=\{2}\s*$\)/
syntax match wikiH3             /\(^!\{3}.*$\|^\s*=\{3}.*=\{3}\s*$\)/
syntax match wikiH4             /\(^!\{4}.*$\|^\s*=\{4}.*=\{4}\s*$\)/
syntax match wikiH5             /\(^!\{5}.*$\|^\s*=\{5}.*=\{5}\s*$\)/
syntax match wikiH6             /\(^!\{6}.*$\|^\s*=\{6}.*=\{6}\s*$\)/

" <hr>, horizontal rule
syntax match wikiHR             /^----.*$/

" Tables. Each line starts and ends with '||'; each cell is separated by '||'
syntax match wikiTable          /||/

" Bulleted list items start with whitespace(s), then '*'
" syntax match wikiList           /^\s\+\(\*\|[1-9]\+0*\.\).*$/   contains=@wikiText
" highlight only bullets and digits.
syntax match wikiList           /^\s\+\(\*\|[1-9]\+0*\.\|#\)/

syntax match wikiTodo           /\(TODO:\|DONE:\|FIXME:\|FIXED:\)/ 

" Treat all other lines that start with spaces as PRE-formatted text.
syntax match wikiPre            /^\s\+[^[:blank:]*#].*$/

syntax region wikiPre start=/^{{{\s*$/ end=/^}}}\s*$/
syntax sync match wikiPreSync grouphere wikiPre /^{{{\s*$/

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

hi def link wikiCode                  PreProc
hi def link wikiWord                  Underlined
hi def link wikiNoExistsWord          Error

hi def link wikiEscape                Todo
hi def link wikiPre                   PreProc
hi def link wikiLink                  Underlined
hi def link wikiList                  Type
hi def link wikiTable                 Type
hi def link wikiEmoticons             Constant
hi def link wikiDelText               Comment
hi def link wikiInsText               Constant
hi def link wikiSuperScript           Constant
hi def link wikiSubScript             Constant
hi def link wikiCitation              Constant
hi def link wikiTodo                  Todo

let b:current_syntax="vimwiki"

" vim:tw=0:
