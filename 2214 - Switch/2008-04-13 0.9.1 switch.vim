"        name: Switch
"     summary: Quickly toggle boolean options between 'on' and 'off'
"     version: 0.9.1
"     license: GPL
"    requires: Vim 7+
"   script id: 2214
" last change: 14 April 2008
"   developer: Tomas RC
"     project: Univrc
"       email: univrc@gmail.com
"        site: http://univrc.org
"
"         use: # list of switch keys : <C-Q><Space>
"              # switch action       : <C-Q>{key}

" {{{1 [loaded]                   
"-------------------------------------------------------------------------->
:if exists('g:loaded_switch')
    :finish
:endif
:let g:loaded_switch = 1
"--------------------------------------------------------------------------<
 
" {{{1 [config]         *         
"-------------------------------------------------------------------------->
:if  !exists("g:prev_{'Switch'}_{'Dict'}")
:let g:prev_{'Switch'}_{'Dict'} =  { 'A'  : 'autochdir'
                                 \, 'B'  : 'scrollbind'
                                 \, 'C'  : 'cursorcolumn'
                                 \, 'D'  : 'digraph'
                                 \, 'E'  : 'expandtab'
                                 \, 'F'  : 'rightleft'
                                 \, 'G'  : 'magic'
                                 \, 'H'  : 'hidden'
                                 \, 'I'  : 'infercase'
                                 \, 'K'  : 'writebackup'
                                 \, 'L'  : 'cursorline'
                                 \, 'M'  : 'showmode'
                                 \, 'N'  : 'icon'
                                 \, 'O'  : 'confirm'
                                 \, 'P'  : 'wrapscan'
                                 \, 'R'  : 'autoread'
                                 \, 'S'  : 'showcmd'
                                 \, 'U'  : 'autoindent'
                                 \, 'V'  : 'revins'
                                 \, 'W'  : 'autowrite'
                                 \, 'a'  : 'showmatch'
                                 \, 'b'  : 'linebreak'
                                 \, 'c'  : 'cindent'
                                 \, 'd'  : 'diff'
                                 \, 'e'  : 'errorbells'
                                 \, 'f'  : 'foldenable'
                                 \, 'g'  : 'ignorecase'
                                 \, 'h'  : 'hlsearch'
                                 \, 'i'  : 'incsearch'
                                 \, 'j'  : 'joinspaces'
                                 \, 'k'  : 'backup'
                                 \, 'l'  : 'list'
                                 \, 'm'  : 'modifiable'
                                 \, 'n'  : 'number'
                                 \, 'o'  : 'readonly'
                                 \, 'p'  : 'paste'
                                 \, 'r'  : 'ruler'
                                 \, 's'  : 'spell'
                                 \, 't'  : 'title'
                                 \, 'u'  : 'secure'
                                 \, 'v'  : 'visualbell'
                                 \, 'w'  : 'wrap'
                                 \, 'z'  : 'lazyredraw'
                                   \}
:endif
"--------------------------------------------------------------------------<
" }}}1
" {{{1 [system]                   
:function! <SID>Load_SwitchSys()
    " {{{2 :let
"-------------------------------------------------------------------------->
:let s:sys = g:prev_{'Switch'}_{'Dict'}
"--------------------------------------------------------------------------<
  
    " {{{2 :fun
"-------------------------------------------------------------------------->
:function! <SID>Switch()
    :echo ":switch "
    :let l:list = []
    :let l:flag = nr2char(getchar())
    :if   l:flag == ' '
        :echo "[switch.vim]"
        :echo "==== Dictionary of boolean options ===="
        :echo " "
        :for l:entry in items(s:sys)
            :call add(l:list, [l:entry[1], l:entry[0]])
        :endfor
        :call sort(l:list)
        :for l:entry in l:list
            :echo "          " l:entry[1] " : " l:entry[0]
        :endfor
        :echo " "
        :echo " "
        :call getchar()
    :else
        :if exists("s:sys[l:flag]")
            :let l:setting = s:sys[l:flag]
        :else
            :echo "Given key not found in the boolean options dictionary!"
            :return
        :endif
        :execute ":let l:value=&".l:setting
        :if exists("l:value")
            :execute ":set ".l:setting."!" 
            :if l:value == 0 | :echon l:setting."! [ON]" 
            :else            | :echon l:setting."! [OFF]" 
            :endif 
        :else
            :echo 'Unknown boolean option:' l:setting
            :return 1
        :endif
    :endif
:endfunction
"--------------------------------------------------------------------------<
   
    " {{{2 :map
"-------------------------------------------------------------------------->
:nnoremap <silent> <C-Q> :call <SID>Switch()  <CR>
"--------------------------------------------------------------------------<
    " }}}2
            :endfunction

" {{{1 [handle]                   
"-------------------------------------------------------------------------->
:call <SID>Load_SwitchSys()
:unlet g:prev_{'Switch'}_{'Dict'}
"--------------------------------------------------------------------------<
" }}}1

" vim:fen:fdm=marker:fdl=0
