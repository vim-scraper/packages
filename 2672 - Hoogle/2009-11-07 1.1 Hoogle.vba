" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/hoogle.vim	[[[1
116
"=============================================================================
" What Is This: Perform a search on local hoogle and display the results on
"               a scratch buffer.
" File: hoogle.vim
" Author: Vincent B <twinside@gmail.com>
" Last Change: 2009 june 11
" Version: 1.0
" Thanks:
" Usage:
"       :Hoogle <search>
"       :HoogleClose
"       :HoogleLine
"
" Additional:
"     * g:hoogle_search_count
"           Define it with the number of max results to get,
"           default is 10
"
"     * g:hoogle_search_buf_name
"           Name of the search buffer, default HoogleSearch
"
" ChangeLog:
"       1.1: resize un function of line count
"       1.0: initial version
if exists("g:__HOOGLE_VIM__")
    finish
endif
let g:__HOOGLE_VIM__ = 1

if !exists("g:hoogle_search_count")
    let g:hoogle_search_count = 10
endif

if !exists("g:hoogle_search_buf_name")
    let g:hoogle_search_buf_name = 'HoogleSearch'
endif

" ScratchMarkBuffer
" Mark a buffer as scratch
function! s:ScratchMarkBuffer()
    setlocal buftype=nofile
    " make sure buffer is deleted when view is closed
    setlocal bufhidden=wipe
    setlocal noswapfile
    setlocal buflisted
    setlocal nonumber
    setlocal statusline=%F
endfunction

" return -1 if no windows was open
"        >= 0 if cursor is now in the window
fun! s:HoogleGotoWin() "{{{
    let bufnum = bufnr( g:hoogle_search_buf_name )
    if bufnum >= 0
        let win_num = bufwinnr( bufnum )
        " We must get a real window number, or
        " else the buffer would have been deleted
        " already
        exe win_num . "wincmd w"
        return 0
    endif
    return -1
endfunction "}}}

" Close hoogle search
fun! HoogleCloseSearch() "{{{
    let last_buffer = bufnr("%")
    if s:HoogleGotoWin() >= 0
        close
    endif
    let win_num = bufwinnr( last_buffer )
    " We must get a real window number, or
    " else the buffer would have been deleted
    " already
    exe win_num . "wincmd w"
endfunction "}}}

" Open a scratch buffer or reuse the previous one
fun! HoogleLookup( search ) "{{{
    " Ok, previous buffer to jump to it at final
    let last_buffer = bufnr("%")

    if s:HoogleGotoWin() < 0
        new
        exe 'file ' . g:hoogle_search_buf_name
        setl modifiable
    else
        setl modifiable
        normal ggVGd
    endif

    call s:ScratchMarkBuffer()

    execute '.!hoogle --n=' . g:hoogle_search_count  . ' "' . a:search . '"'
    setl nomodifiable
    
    execute 'resize ' . line( '$' )

    let win_num = bufwinnr( last_buffer )
    " We must get a real window number, or
    " else the buffer would have been deleted
    " already
    exe win_num . "wincmd w"
endfunction "}}}

" Search the current line and delete it
fun! HoogleSearchLine() "{{{
    let search = getline( '.' )
    normal dd
    call HoogleLookup( search )
endfunction "}}}

command! -nargs=* Hoogle call HoogleLookup( '<args>' )
command! HoogleClose call HoogleCloseSearch()
command! HoogleLine call HoogleSearchLine()

doc/Hoogle.txt	[[[1
81
*hoogle.txt*    Plugin for local hoogle search

                            Hoogle.Vim Plugin ~

    |hoogle|
            What is Hoogle
    |hooglecommands|
            |:Hoogle|
            |:HoogleClose|
            |:HoogleLine|

    |hoogleconfiguration|
            |g:hoogle_search_count|
            |g:hoogle_search_buf_name|

    |hooglesuggestions|

==============================================================================
*hoogle*
What is Hoogle ?~
    Hoogle is a search engine dedicated to haskell's libraries. It can work
    online using http://www.haskell.org/hoogle, in the #haskell irc channel
    on freenode. It can also be used offline using this package :
    http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hoogle

    This plugin is intented to be used with the offline version of hoogle
    and provide a quick windows for hoogle lookup.

==============================================================================
*hooglecommands*   List of hoogle related command
                                                             *:Hoogle* *Hoogle*
 :Hoogle <search>
            Search with hoogle installed on the local machine. Results are
        stored in a scratch buffer of 10 lines by default.

        Examples : >
        ==========
            :Hoogle foldr
<       Will search all foldr named function on hoogle. >

            :Hoogle (a -> b) -> [a] -> [b]
<       The type will be searched with hoogle, and normally, the map function
        should arrise
                                                   *:HoogleClose* *HoogleClose*
 :HoogleClose
        Quickly close the hoogle search windows. Cursor stay at it's current
        position

                                                     *:HoogleLine* *HoogleLine*
 :HoogleLine
        Search on hoogle the line on which the cursor is currently into, then
        it delete the current line.
==============================================================================
*hoogleconfiguration*  Variables controlling hoogle configuration


                                  *hoogle_search_count* *g:hoogle_search_count*
 g:hoogle_search_count                                      
        Define it with the number of max results to get,
        default is 10.
        To override this number, put the following line in your .vimrc: >
    	:let g:hoogle_search_count = 20 " set number to 20
<
                                        
                            *hoogle_search_buf_name* *g:hoogle_search_buf_name*
g:hoogle_search_buf_name
        Let you customize the name of the scratch buffer used to display
        Hoogle's results. Default name is 'HoogleSearch'. If you want to
        change this name, add following line in your .vimrc : >
        :let g:hoogle_search_buf_name = 'MyFavoriteHoogleName'
<
==============================================================================
*hooglesuggestions*
  To be used efficiently, you may consider add some mapping, like this : >
    au BufNewFile,BufRead *.hs map <buffer> <F1> :Hoogle 
    au BufNewFile,BufRead *.hs map <buffer> <C-F1> :HoogleClose<CR>
    au BufNewFile,BufRead *.hs map <buffer> <S-F1> :HoogleLine<CR>
<
==============================================================================
 vim:tw=78:ts=8:ft=help:norl:

