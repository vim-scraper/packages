" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/checksyntax.txt	[[[1
98
*checksyntax.txt*   checksyntax -- Check syntax when saving a file (php, ruby, tex ...)
                    Author: Tom Link, micathom at gmail com

By default, |:CheckSyntax| is mapped to <F5>. The |:CheckSyntax| command 
takes one optional argument: the mode (default: &filetype). The 
following modes are pre-defined (and maybe some more):

    c, cpp :: Check syntax via splint
    html :: Check syntax via tidy
    javascript :: Check syntax via jsl or gjslint
    java :: Check syntax via jlint or javaCheckstyle
    lua :: Parse file (luac -p)
    php  :: Check syntax (php -l)
    phpp :: Parse a file (php -f)
    ruby :: Check syntax (ruby -c; no auto-checks)
    tex, latex :: Parse file (chktex -q -v0; no auto-checks)
    xml, docbk :: Check syntax via xmllint

See |g:checksyntax| for how to define your own syntax checkers.

This plugin was originally based on Klaus Horsten's php_console 
(vimscript #779) and it is the successor of php_check_syntax.vim 
(vimscript #1272). In opposition to php_console, it is not a php 
ftplugin but a general solution for checking the syntax when saving a 
file (using the appropriate interpreter, lint or whatsoever).


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball (vimscript 
#1502) installed or update your runtime.


========================================================================
Contents~

        :CheckSyntax ............... |:CheckSyntax|
        <F5> ....................... |<F5>|
        i_<F5> ..................... |i_<F5>|
        g:checksyntax .............. |g:checksyntax|
        checksyntax#Check .......... |checksyntax#Check()|


========================================================================
plugin/checksyntax.vim~

                                                    *:CheckSyntax*
CheckSyntax[!] [NAME]
    Check the current buffer's syntax (type defaults to &filetype).
    Or use NAME instead of &filetype.
    
    With the bang !, use the alternative syntax checker (see 
    |g:checksyntax|).

                                                    *<F5>*
<F5> ... :CheckSyntax<cr>

                                                    *i_<F5>*
i_<F5> ... <c-o>:CheckSyntax<cr>


========================================================================
autoload/checksyntax.vim~

                                                    *g:checksyntax*
g:checksyntax                  (default: {})
    A dictionary {name/filetype => definition} of syntax checkers, where 
    definition is a dictionary with the following fields:
    
    Mandatory (either one of the following):
      cmd  ... A shell command used as 'makeprg' to check the file.
      exec ... A vim command used to check the file.
      compiler ... A vim compiler that is used to check the file.
    
    Optional:
      auto ... Run automatically when saving a file.
      efm  ... An 'errorformat' string.
      okrx ... A |regexp| matching the command output if no error were 
               found.
      failrx ... A |regexp| matching the command output if an error 
               was found.
      alt  ... The name of an alternative syntax checker (see 
               |:CheckSyntax|).
      prepare ... An ex command that is run before doing anything.
      ignore_nr ... A list of error numbers that should be ignored.

                                                    *checksyntax#Check()*
checksyntax#Check(manually, ?bang='', ?type=&ft)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/checksyntax.vim	[[[1
75
" checksyntax.vim -- Check syntax when saving a file (php, ruby, tex ...)
" @Author:      Tom Link (micathom AT gmail com)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     04-Mai-2005.
" @Last Change: 2010-09-12.
" @Revision:    390

if exists('g:loaded_checksyntax')
    finish
endif
let g:loaded_checksyntax = 100


" @TPluginInclude
if !exists('g:checksyntax_auto')
    " If non-null, enable automatic syntax checks after saving a file.
    let g:checksyntax_auto = 1
endif


" @TPluginInclude
augroup CheckSyntax
    autocmd!
    if g:checksyntax_auto
        autocmd CheckSyntax BufWritePost * call checksyntax#Check(0)
    endif
augroup END


" :display: CheckSyntax[!] [NAME]
" Check the current buffer's syntax (type defaults to &filetype).
" Or use NAME instead of &filetype.
"
" With the bang !, use the alternative syntax checker (see 
" |g:checksyntax|).
command! -bang -nargs=? CheckSyntax call checksyntax#Check(1, "<bang>", <f-args>)


" @TPluginInclude
if !hasmapto(':CheckSyntax')
    noremap <F5> :CheckSyntax<cr>
    inoremap <F5> <c-o>:CheckSyntax<cr>
endif


finish
0.2
php specific

0.3
generalized plugin; modes; support for ruby, phpp, tex (chktex)

0.4
use vim compilers if available (e.g., tidy, xmllint ...); makeprg was 
restored in the wrong window

0.5
- Support for jsl (javascript lint).
- Support for jlint.
- Don't automatically check php files if eclim is installed.
- Allow auto_* parameters to be buffer local.
- FIX: Unlet current_compiler, use g:current_compiler
- FIX: garbled screen: use redraw! (thanks to Vincent de Lau)
- Support for lua (thanks to norman)

0.6
- checksyntax_compiler_{&ft} & checksyntax_cmd_{&ft} variables can be 
buffer local

1.0
- The info maintained as g:checksyntax_* variables is now kept in a 
dictionary named g:checksyntax
- Support for gjslint
- Some bug fixes (e.g. tidy)

autoload/checksyntax.vim	[[[1
347
" checksyntax.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-01-03.
" @Last Change: 2010-10-18.
" @Revision:    214


if !exists('g:checksyntax#failrx')
    let g:checksyntax#failrx = '\ *\(\d \f\{-}:\)\?\d\{-}:'
endif

if !exists('g:checksyntax#okrx')
    let g:checksyntax#okrx = ''
endif

if !exists('g:checksyntax')
    " A dictionary {name/filetype => definition} of syntax checkers, where 
    " definition is a dictionary with the following fields:
    " 
    " Mandatory (either one of the following):
    "   cmd  ... A shell command used as 'makeprg' to check the file.
    "   exec ... A vim command used to check the file.
    "   compiler ... A vim compiler that is used to check the file.
    " 
    " Optional:
    "   auto ... Run automatically when saving a file.
    "   efm  ... An 'errorformat' string.
    "   okrx ... A |regexp| matching the command output if no error were 
    "            found.
    "   failrx ... A |regexp| matching the command output if an error 
    "            was found.
    "   alt  ... The name of an alternative syntax checker (see 
    "            |:CheckSyntax|).
    "   prepare ... An ex command that is run before doing anything.
    "   ignore_nr ... A list of error numbers that should be ignored.
    let g:checksyntax = {}   "{{{2
endif


""" Php
if !exists('g:checksyntax.php')
    let g:checksyntax['php'] = {
                \ 'auto': 1,
                \ 'cmd': 'php -l',
                \ 'efm': '%*[^:]: %m in %f on line %l',
                \ 'okrx': 'No syntax errors detected in ',
                \ 'alt': 'phpp'
                \ }
endif

"""""" Parse php
if !exists('g:checksyntax.phpp')
    let g:checksyntax['phpp'] = {
                \ 'cmd': 'php -f',
                \ 'efm': g:checksyntax.php.efm,
                \ 'okrx': g:checksyntax.php.okrx
                \ }
endif

autocmd CheckSyntax BufReadPost *.php if exists(':EclimValidate') && !empty(eclim#project#util#GetCurrentProjectName()) | let b:checksyntax.php.auto = 0 | endif


""" JavaScript
if !exists('g:checksyntax.javascript')
    if exists('g:checksyntax_javascript') ? (g:checksyntax_javascript == 'gjslint') : executable('gjslint')
        let g:checksyntax['javascript'] = {
                    \ 'cmd': 'gjslint',
                    \ 'ignore_nr': [1, 110],
                    \ 'efm': '%P%*[^F]FILE%*[^:]: %f %*[-],Line %l%\, %t:%n: %m,%Q',
                    \ }
    elseif exists('g:checksyntax_javascript') ? (g:checksyntax_javascript == 'jsl') : executable('jsl')
        let g:checksyntax['javascript'] = {
                    \ 'cmd': 'jsl -nofilelisting -nocontext -nosummary -nologo -process',
                    \ 'okrx': '0 error(s), 0 warning(s)',
                    \ }
    endif
endif


""" Ruby
if !exists('g:checksyntax.ruby')
    let g:checksyntax['ruby'] = {
                \ 'prepare': 'compiler ruby',
                \ 'cmd': 'ruby -c',
                \ 'okrx': 'Syntax OK\|No Errors'
                \ }
endif


""" Viki
if !exists('g:checksyntax.viki')
    let g:checksyntax['viki'] = {
                \ 'cmd': 'deplate -f null',
                \ }
endif


""" chktex (LaTeX)
if !exists('g:checksyntax.tex')
    if executable('chktex')
        let g:checksyntax['tex'] = {
                    \ 'cmd': 'chktex -q -v0',
                    \ 'efm': '%f:%l:%m',
                    \ }
    endif
endif


""" c, cpp
if !exists('g:checksyntax.c')
    if executable('splint')
        let g:checksyntax['c'] = {
                    \ 'compiler': 'splint',
                    \ }
    endif
endif

if !exists('g:checksyntax.cpp') && exists('g:checksyntax.c')
    let g:checksyntax['cpp'] = copy(g:checksyntax.c)
endif


""" java
if !exists('g:checksyntax.java')
    if executable('jlint')
        let g:checksyntax['java'] = {
                    \ 'exec': 'call checksyntax#Jlint()',
                    \ 'alt': 'javaCheckstyle'
                    \ }

        " :nodoc:
        function! checksyntax#Jlint() "{{{3
            let filename = expand('%:r') .'.class'
            " TLogVAR filename
            " echom '! jlint -done '. shellescape(filename)
            exec '! jlint -done '. shellescape(filename)
        endf
    endif
endif

if !exists('g:checksyntax.javaCheckstyle')
    if executable('checkstyle')
        let g:checksyntax['javaCheckstyle'] = {
                    \ 'compiler': 'checkstyle',
                    \ }
    endif
endif


""" lua
if !exists('g:checksyntax.lua')
    " efm: File:Line:Column:Warning number:Warning message
    let g:checksyntax['lua'] = {
                \ 'auto': 1,
                \ 'cmd': 'luac -p',
                \ 'efm': 'luac\:\ %f:%l:\ %m'
                \ }
endif


""" tidy (HTML)
if !exists('g:checksyntax.html')
    let g:checksyntax['html'] = {
                \ 'cmd': 'tidy -eq',
                \ 'efm': 'line %l column %c - %m'
                \ }
endif
if !exists('g:checksyntax.xhtml')
    let g:checksyntax['xhtml'] = copy(g:checksyntax.html)
endif


""" XML
if !exists('g:checksyntax.xml')
    let g:checksyntax['xml'] = {
                \ 'compiler': 'xmllint'
                \ }
endif
if !exists('g:checksyntax.docbk')
    let g:checksyntax['docbk'] = copy(g:checksyntax.xml)
endif


if !exists('*CheckSyntaxSucceed')
    " :nodoc:
    function! CheckSyntaxSucceed(manually)
        cclose
        if a:manually
            echo
            echo 'Syntax ok.'
        endif
    endf
endif


if !exists('*CheckSyntaxFail')
    " :nodoc:
    function! CheckSyntaxFail(manually)
        copen
    endf
endif


function! s:Make(def)
    let bufnr = bufnr('%')
    let pos = getpos('.')
    try
        if has_key(a:def, 'compiler')

            if exists('g:current_compiler')
                let cc = g:current_compiler
            else
                let cc = ''
            endif
            try
                exec 'compiler '. a:def.compiler
                silent make
                return 1
            finally
                if cc != ''
                    let g:current_compiler = cc
                    exec 'compiler '. cc
                endif
            endtry

        else

            let makeprg = &makeprg
            let shellpipe = &shellpipe
            let errorformat = &errorformat
            if has_key(a:def, 'shellpipe')
                let &l:shellpipe = get(a:def, 'shellpipe')
            endif
            if has_key(a:def, 'efm')
                let &l:errorformat = get(a:def, 'efm')
            endif
            try
                if has_key(a:def, 'cmd')
                    let &l:makeprg = a:def.cmd
                    " TLogVAR &l:makeprg, &l:errorformat
                    silent make %
                    return 1
                elseif has_key(a:def, 'exec')
                    exec a:def.exec
                    return 1
                endif
            finally
                if &l:makeprg != makeprg
                    let &l:makeprg = makeprg
                endif
                if &l:shellpipe != shellpipe
                    let &l:shellpipe = shellpipe
                endif
                if &l:errorformat != errorformat
                    let &l:errorformat = errorformat
                endif
            endtry

        endif
    catch
        echohl Error
        echom v:errmsg
        echohl NONE
    finally
        if bufnr == bufnr('%')
            call setpos('.', pos)
        endif
    endtry
    return 0
endf


function! s:GetDef(ft) "{{{3
    if exists('b:checksyntax') && has_key(b:checksyntax, a:ft)
        return b:checksyntax[a:ft]
    elseif has_key(g:checksyntax, a:ft)
        return g:checksyntax[a:ft]
    else
        return {}
    endif
endf


" :def: function! checksyntax#Check(manually, ?bang='', ?type=&ft)
function! checksyntax#Check(manually, ...)
    let bang = a:0 >= 1 && a:1 != '' ? 1 : 0
    let ft   = a:0 >= 2 && a:2 != '' ? a:2 : &filetype
    let def = a:manually ? {} : s:GetDef(ft .',auto')
    if empty(def)
        let def  = s:GetDef(ft)
    endif
    if bang && has_key(def, 'alt')
        let def = s:GetDef(def.alt)
    endif
    " TLogVAR def
    if empty(def)
        return
    endif
    let auto = get(def, 'auto', 0)
    " TLogVAR auto
    if !(a:manually || auto)
        return
    endif
    if &modified
        echom "Buffer was modified. Please save it before calling :CheckSyntax."
        return
    end
    " TLogVAR &makeprg, &l:makeprg, &g:makeprg, &errorformat
    exec get(def, 'prepare', '')
    if s:Make(def)
        let failrx = get(def, 'failrx', g:checksyntax#failrx)
        let okrx   = get(def, 'okrx', g:checksyntax#okrx)
        let qfl = getqflist()
        let bnr = bufnr('%')
        call filter(qfl, 's:FilterItem(def, v:val)')
        call map(qfl, 's:CompleteItem(def, v:val)')
        call setqflist(qfl)
        " echom "DBG 1" string(qfl)
        if len(qfl) == 0
            call CheckSyntaxSucceed(a:manually)
        else
            call CheckSyntaxFail(a:manually)
        endif
        redraw!
    endif
endf


function! s:CompleteItem(def, val) "{{{3
    if get(a:val, 'bufnr', 0) == 0
        let a:val.bufnr = bufnr('%')
    endif
    return a:val
endf


function! s:FilterItem(def, val) "{{{3
    if a:val.lnum == 0 && a:val.pattern == ''
        return 0
    elseif has_key(a:val, 'nr') && has_key(a:def, 'ignore_nr') && index(a:def.ignore_nr, a:val.nr) != -1
        return 0
    endif
    return 1
endf

