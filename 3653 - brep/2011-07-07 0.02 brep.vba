" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/brep.txt	[[[1
76
*brep.txt*          Scan certain buffers for a regexp
                    Author: Tom Link, micathom at gmail com

Scan certain buffers for a regexp and save the result as |quickfix| list.


-----------------------------------------------------------------------
Install~

In order to install the vba, open the vba file in VIM and type: >

    :so %

See :help vimball for details.

Optional enhancement: trag (vimscript #2033)

Also available via git: http://github.com/tomtom/brep_vim/


========================================================================
Contents~

        :Brep ....................... |:Brep|
        g:brep#view_qfl ............. |g:brep#view_qfl|
        g:brep#ignore_buftypes ...... |g:brep#ignore_buftypes|
        g:brep#ignore_bufnames_rx ... |g:brep#ignore_bufnames_rx|
        g:brep#ignore_filetype ...... |g:brep#ignore_filetype|
        g:brep#use_bufdo ............ |g:brep#use_bufdo|
        brep#Grep ................... |brep#Grep()|


========================================================================
plugin/brep.vim~

                                                    *:Brep*
Brep[!] REGEXP
    Scan buffers for REGEXP by means of |brep#Grep()|.
    
    With the optional bang '!', scan unlisted buffers too.


========================================================================
autoload/brep.vim~

                                                    *g:brep#view_qfl*
g:brep#view_qfl                (default: exists('g:loaded_trag') ? 'Tragcw' : 'cwindow')
    Command used to display the |quickfixlist|.

                                                    *g:brep#ignore_buftypes*
g:brep#ignore_buftypes         (default: ['quickfix'])
    Ignore buffer types (see 'buftype').

                                                    *g:brep#ignore_bufnames_rx*
g:brep#ignore_bufnames_rx      (default: '')
    Ignore buffers matchings this |regexp|.

                                                    *g:brep#ignore_filetype*
g:brep#ignore_filetype         (default: [])
    Ignore these filetypes.

                                                    *g:brep#use_bufdo*
g:brep#use_bufdo               (default: 0)
    By default brep does single line scans on the buffer contents. If 
    you want to search for multi-line patterns, you have to use |:bufdo| 
    instead. You won't be able to scan unlisted buffers this way. 
    Using bufdo is slightly slower.
    If the pattern contains '\n', |:Brep| always uses :bufdo anyway.

                                                    *brep#Grep()*
brep#Grep(regexp, ?buffers=[], ?show_hidden=0)
    Scan buffers vor a |regexp|.



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/brep.vim	[[[1
29
" brep.vim -- Scan certain buffers for a regexp and save as quickfix
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2011-07-04.
" @Last Change: 2011-07-07.
" @Revision:    12
" GetLatestVimScripts: 3653 0 :AutoInstall: brep.vim
" 

if &cp || exists("loaded_brep")
    finish
endif
let loaded_brep = 2

let s:save_cpo = &cpo
set cpo&vim


" :display: Brep[!] REGEXP
" Scan buffers for REGEXP by means of |brep#Grep()|.
"
" With the optional bang '!', scan unlisted buffers too.
command! -nargs=1 -bang Brep call brep#Grep(<q-args>, [], !empty('<bang>'))


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/brep.vim	[[[1
104
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2011-07-04.
" @Last Change: 2011-07-07.
" @Revision:    73


if !exists('g:brep#view_qfl')
    " Command used to display the |quickfixlist|.
    let g:brep#view_qfl = exists('g:loaded_trag') ? 'Tragcw' : 'cwindow'   "{{{2
endif

if !exists('g:brep#ignore_buftypes')
    " Ignore buffer types (see 'buftype').
    let g:brep#ignore_buftypes = ['quickfix']   "{{{2
endif

if !exists('g:brep#ignore_bufnames_rx')
    " Ignore buffers matchings this |regexp|.
    let g:brep#ignore_bufnames_rx = ''   "{{{2
endif

if !exists('g:brep#ignore_filetype')
    " Ignore these filetypes.
    let g:brep#ignore_filetype = []   "{{{2
endif

if !exists('g:brep#use_bufdo')
    " By default brep does single line scans on the buffer contents. If 
    " you want to search for multi-line patterns, you have to use |:bufdo| 
    " instead. You won't be able to scan unlisted buffers this way. 
    " Using bufdo is slightly slower.
    " If the pattern contains '\n', |:Brep| always uses :bufdo anyway.
    let g:brep#use_bufdo = 0   "{{{2
endif


" :display: brep#Grep(regexp, ?buffers=[], ?show_hidden=0)
" Scan buffers vor a |regexp|.
function! brep#Grep(regexp, ...) "{{{3
    let buffers = a:0 >= 1 ? a:1 : []
    let show_hidden = a:0 >= 2 ? a:2 : 0
    if empty(buffers)
        redir => bufferss
        if show_hidden
            silent ls!
        else
            silent ls
        endif
        redir END
        let buffers = split(bufferss, '\n')
        let buffers = map(buffers, 'str2nr(matchstr(v:val, ''^\s*\zs\d\+''))')
    endif
    let qfl = []
    if g:brep#use_bufdo || match(a:regexp, '\(^\|[^\\]\|\(^\|[^\\]\)\(\\\\\)\+\)\\n') != -1
        let cbufnr = bufnr('%')
        let lazyredraw = &lazyredraw
        let eventignore = &eventignore
        set lazyredraw
        set eventignore=all
        try
            exec 'silent keepalt bufdo! g/'. escape(a:regexp, '/') .'/call add(qfl, {"bufnr": bufnr("%"), "lnum": line("."), "text": getline("."), "type": "G"})'
        finally
            exec 'keepalt buffer' cbufnr
            let &lazyredraw = lazyredraw
            let &eventignore = eventignore
        endtry
    else
        for bufnr in buffers
            if index(g:brep#ignore_buftypes, getbufvar(bufnr, '&buftype')) == -1
                        \ && index(g:brep#ignore_filetype, getbufvar(bufnr, '&filetype')) == -1
                        \ && (empty(g:brep#ignore_bufnames_rx) || bufname(bufnr) !~ g:brep#ignore_bufnames_rx)
                let buffer_text = getbufline(bufnr, 1, '$')
                let s:lnum = 0
                let buffer_text = map(buffer_text, 's:LineDef(bufnr, v:val)')
                unlet s:lnum
                if &smartcase && a:regexp =~ '\u'
                    let ic = &ignorecase
                    let &l:ic = 0
                endif
                let buffer_text = filter(buffer_text, 'v:val.text =~ a:regexp')
                if &smartcase && a:regexp =~ '\u'
                    let &l:ic = ic
                endif
                if !empty(buffer_text)
                    let qfl = extend(qfl, buffer_text)
                endif
            endif
        endfor
    endif
    if !empty(qfl)
        call setqflist(qfl)
        if !empty(g:brep#view_qfl)
            exec g:brep#view_qfl
        endif
    endif
endf


function! s:LineDef(bufnr, text) "{{{3
    let s:lnum += 1
    return {'bufnr': a:bufnr, 'lnum': s:lnum, 'text': a:text, 'type': 'G'}
endf

