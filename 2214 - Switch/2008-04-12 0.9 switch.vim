"        name: switch
"     summary: quickly toggle boolean options
"     version: 0.9
"     license: GPL
"    requires: vim 7+
" last change: 12 April 2008
"   developer: Tomas RC
"     project: univrc <org>
"       email: univrc@gmail.com
"
"         use: # list of switch keys : <C-Q><Space>
"              # switch action       : <C-Q>{key}

" {{{1 [loaded]                   
"-------------------------------------------------------------------------->
:if exists('g:loaded_switch')
    :finish
:endif
:let g:loaded_switch = 1
:let s:toggle_switch = 0
"--------------------------------------------------------------------------<
 
" {{{1 [config]         *         
"-------------------------------------------------------------------------->
:if  !exists("g:pre{'Switch'}{'SwitchDict'}")
:let g:pre{'Switch'}{'SwitchDict'} =  { 'C'  : 'cursorcolumn'
                                     \, 'F'  : 'rightleft'
                                     \, 'L'  : 'cursorline'
                                     \, 'M'  : 'showmode'
                                     \, 'R'  : 'revins'
                                     \, 'S'  : 'showcmd'
                                     \, 'W'  : 'wrapscan'
                                     \, 'a'  : 'showmatch'
                                     \, 'b'  : 'linebreak'
                                     \, 'e'  : 'errorbells'
                                     \, 'f'  : 'foldenable'
                                     \, 'h'  : 'hlsearch'
                                     \, 'i'  : 'incsearch'
                                     \, 'l'  : 'list'
                                     \, 'm'  : 'modifiable'
                                     \, 'n'  : 'number'
                                     \, 'o'  : 'readonly'
                                     \, 'p'  : 'paste'
                                     \, 'r'  : 'ruler'
                                     \, 't'  : 'title'
                                     \, 'v'  : 'visualbell'
                                     \, 'w'  : 'wrap'
                                   \}
:endif
"--------------------------------------------------------------------------<
" }}}1
" {{{1 [system]                   
:function! <SID>Load_SwitchSys()
    " {{{2 :let
"-------------------------------------------------------------------------->
:let s:sys = g:pre{'Switch'}{'SwitchDict'}
"--------------------------------------------------------------------------<
  
    " {{{2 :fun
"-------------------------------------------------------------------------->
:function! <SID>Switch()
    :echo ":switch "
    :let l:flag = nr2char(getchar())
    :if   l:flag == ' '
        :echo "[switch.vim] Dictionary of Toggle Options: "
        :for l:entry in items(s:sys)
            :echo l:entry[0] " : " l:entry[1]
        :endfor
        :call getchar()
    :else
        :if exists("s:sys[l:flag]")
            :let l:setting = s:sys[l:flag]
        :else
            :echo "Given key not found in flag settings dictionary!"
            :return
        :endif
        :execute ":let l:value=&".l:setting
        :if exists("l:value")
            :execute ":set ".l:setting."!" 
            :if l:value == 0 | :echon l:setting."! [ON]" 
            :else            | :echon l:setting."! [OFF]" 
            :endif 
        :else
            :echo 'Unknown Toggle Option:' l:setting
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
:unlet g:pre{'Switch'}{'SwitchDict'}
"--------------------------------------------------------------------------<
" }}}1

" vim:fen:fdm=marker:fdl=0
