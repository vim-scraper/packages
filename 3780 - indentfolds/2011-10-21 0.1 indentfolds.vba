" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/indentfolds.txt	[[[1
115
*indentfolds.txt*   Indentation-level based folds with a twist
                    Author: Tom Link, micathom at gmail com

When I write a longer text, I tend to use a top-down approach. I begin 
writing down the headings. I then outline each section. Next, I write 
down keywords for each paragraph etc. At each step, I increase the 
indentation level (multiples of 'shiftwidth', see also 'fold-indent').

This plugin help maintaining an overview of the document by setting a 
paragraph's 'foldlevel' based on its indentation level -- see 
|:Indentfolds|. Other than vim's own 'fold-indent' method, any 
indentation level can be assigned as the top fold level. E.g. text with 
a prefix of 16 whitespace characters can be defined as level 1, 8 and 24 
whitespace characters would then be at level 2 etc.

This plugin also provides a helper-command to comment out or delete all 
lines at higher fold levels -- see |:IndentfoldsComment|.

Users can also use key maps (default: <tab>, <s-tab>) to cycle between 
indentation levels.


-----------------------------------------------------------------------
Install~

In order to install the vba, open the vba file in VIM and type: >

    :so %

See :help vimball for details.

Also available via git: http://github.com/tomtom/indentfolds_vim/


========================================================================
Contents~

        :Indentfolds .................... |:Indentfolds|
        :IndentfoldsComment ............. |:IndentfoldsComment|
        g:indentfolds#cycleplus_map ..... |g:indentfolds#cycleplus_map|
        g:indentfolds#cycleminus_map .... |g:indentfolds#cycleminus_map|
        g:indentfolds#foldlevel ......... |g:indentfolds#foldlevel|
        g:indentfolds#comment_command ... |g:indentfolds#comment_command|
        indentfolds#Fold ................ |indentfolds#Fold()|
        indentfolds#Comment ............. |indentfolds#Comment()|
        indentfolds#Expr ................ |indentfolds#Expr()|
        indentfolds#Cycle ............... |indentfolds#Cycle()|


========================================================================
plugin/indentfolds.vim~

                                                    *:Indentfolds*
Indentfolds LEVEL [FOLDLEVEL]
    See |indentfolds#Fold()| for help on LEVEL.

                                                    *:IndentfoldsComment*
IndentfoldsComment [LEVEL=2]
    Comment out all lines with fold level LEVEL or greater.
    Requires |g:indentfolds#comment_command| to be set.


========================================================================
autoload/indentfolds.vim~

                                                    *g:indentfolds#cycleplus_map*
g:indentfolds#cycleplus_map    (default: '<tab>')
    Use this map for cycling through indentation levels (up).

                                                    *g:indentfolds#cycleminus_map*
g:indentfolds#cycleminus_map   (default: '<s-tab>')
    Use this map for cycling through indentation levels (down).

                                                    *g:indentfolds#foldlevel*
g:indentfolds#foldlevel        (default: 2)
    If >= 0, |indentfolds#Fold()| sets 'foldlevel' to this value.

                                                    *g:indentfolds#comment_command*
g:indentfolds#comment_command  (default: ...)
    An ex-command to evaluate on a range of lines in order to comment 
    that lines out.
    
    If the tcomment plugin is installed, this defaults to:
      TComment!
    Otherwise, commenting is disabled.
    
    You can set this variable to "delete" to delete lines instead of 
    commenting them out.

                                                    *indentfolds#Fold()*
indentfolds#Fold(LEVELS, foldlevel=g:indentfolds#foldlevel)
    Set the indentation levels (see also |fold-indent|) that should be 
    considered a top level fold. The fold level is the smallest difference 
    between the current line's indentation level and the levels set with 
    this function.
    
    The LEVELS argument can have the following forms:
      LEVEL     ... Indentation LEVEL is fold level 1
      LOW-HIGH  ... Indentation levels from LOW to HIGH are fold level 1
      L1,L2,... ... Indentation levels L1, L2 etc. are fold level 1

                                                    *indentfolds#Comment()*
indentfolds#Comment(...)
    Comment out lines whose fold level is greater or equal the LEVEL 
    argument.

                                                    *indentfolds#Expr()*
indentfolds#Expr(lnum)

                                                    *indentfolds#Cycle()*
indentfolds#Cycle(delta)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/indentfolds.vim	[[[1
32
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2011-10-21.
" @Last Change: 2011-10-21.
" @Revision:    9
" GetLatestVimScripts: 0 0 :AutoInstall: indentfolds.vim
" Folds specific indentation levels

if &cp || exists("loaded_indentfolds")
    finish
endif
let loaded_indentfolds = 1

let s:save_cpo = &cpo
set cpo&vim


" :display: Indentfolds LEVEL [FOLDLEVEL]
" See |indentfolds#Fold()| for help on LEVEL.
command! -nargs=1 Indentfolds call indentfolds#Fold(<f-args>)


" :display: IndentfoldsComment [LEVEL=2]
" Comment out all lines with fold level LEVEL or greater.
" Requires |g:indentfolds#comment_command| to be set.
command! -nargs=? IndentfoldsComment call indentfolds#Comment(<f-args>)


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/indentfolds.vim	[[[1
132
" indentfolds.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2011-10-21.
" @Last Change: 2011-10-21.
" @Revision:    86


if !exists('g:indentfolds#cycleplus_map')
    " Use this map for cycling through indentation levels (up).
    let g:indentfolds#cycleplus_map = '<tab>'   "{{{2
endif

if !exists('g:indentfolds#cycleminus_map')
    " Use this map for cycling through indentation levels (down).
    let g:indentfolds#cycleminus_map = '<s-tab>'   "{{{2
endif

if !exists('g:indentfolds#foldlevel')
    " If >= 0, |indentfolds#Fold()| sets 'foldlevel' to this value.
    let g:indentfolds#foldlevel = 2   "{{{2
endif

if !exists('g:indentfolds#comment_command')
    " An ex-command to evaluate on a range of lines in order to comment 
    " that lines out.
    "
    " If the tcomment plugin is installed, this defaults to:
    "   TComment!
    " Otherwise, commenting is disabled.
    "
    " You can set this variable to "delete" to delete lines instead of 
    " commenting them out.
    " :read: let g:indentfolds#comment_command = ...   "{{{2
    if exists('g:loaded_tcomment')
        let g:indentfolds#comment_command = 'TComment!'
    else
        let g:indentfolds#comment_command = ''
    endif
endif

let s:levels = [0]


" :display: indentfolds#Fold(LEVELS, foldlevel=g:indentfolds#foldlevel)
" Set the indentation levels (see also |fold-indent|) that should be 
" considered a top level fold. The fold level is the smallest difference 
" between the current line's indentation level and the levels set with 
" this function.
"
" The LEVELS argument can have the following forms:
"   LEVEL     ... Indentation LEVEL is fold level 1
"   LOW-HIGH  ... Indentation levels from LOW to HIGH are fold level 1
"   L1,L2,... ... Indentation levels L1, L2 etc. are fold level 1
function! indentfolds#Fold(...) "{{{3
    if a:0 > 0 && a:1 =~ '^\d\+\(,\d\+\)*$'
        let s:levels = split(a:0, ',')
    elseif a:0 > 0 && a:1 =~ '^\d\+-\d\+$'
        let s:levels = call('range', split(a:0, '-'))
    else
        throw "IndentFolds: Argument format (one of): Level L1-L2 L1,L2"
    endif
    setlocal foldmethod=expr
    setlocal foldexpr=indentfolds#Expr(v:lnum)
    let foldlevel = a:0 >= 2 ? a:2 : g:indentfolds#foldlevel
    if foldlevel > 0 && &l:foldlevel != foldlevel
        let &l:foldlevel = foldlevel
    endif
    if !empty('g:indentfolds#cycleplus_map')
        exec 'noremap <buffer>' g:indentfolds#cycleplus_map ':call indentfolds#Cycle(1)<cr>'
    endif
    if !empty('g:indentfolds#cycleminus_map')
        exec 'noremap <buffer>' g:indentfolds#cycleminus_map ':call indentfolds#Cycle(-1)<cr>'
    endif
endf


" Comment out lines whose fold level is greater or equal the LEVEL 
" argument.
function! indentfolds#Comment(...) "{{{3
    if empty(g:indentfolds#comment_command)
        throw 'IndentFolds: Commenting not supported! Please see :help g:indentfolds#comment_command'
    else
        let pos = getpos('.')
        try
            let level = a:0 >= 1 ? a:1 : 2
            let lbeg = 0
            for lnum in range(1, line('$'))
                if foldlevel(lnum) >= level
                    if lbeg == 0
                        let lbeg = lnum
                    endif
                elseif lbeg > 0
                    let lend = lnum - 1
                    " echom "DBG" lbeg .",". lend . g:indentfolds#comment_command
                    silent exec lbeg .",". lend . g:indentfolds#comment_command
                    let lbeg = 0
                endif
            endfor
            if lbeg > 0
                " echom "DBG" lbeg .",$" . g:indentfolds#comment_command
                silent exec lbeg .",$" . g:indentfolds#comment_command
            endif
        finally
            call setpos('.', pos)
        endtry
    endif
endf


function! indentfolds#Expr(lnum) "{{{3
    let indent = indent(a:lnum)
    let level = indent / &shiftwidth
    if index(s:levels, level) != -1
        return 1
    else
        let val = min(map(copy(s:levels), 'abs(v:val - level)')) + 1
        return val
    endif
endf


function! indentfolds#Cycle(delta) "{{{3
    let s:levels = map(s:levels, 'v:val + a:delta')
    echom "IndentFolds: Set top indentation levels to:" join(s:levels, ', ')
    norm! zx
    if g:indentfolds#foldlevel > 0
        let &l:foldlevel = g:indentfolds#foldlevel
        norm! zv
    endif
endf

