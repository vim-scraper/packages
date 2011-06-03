" tEchoPair.vim
" @Author:      Thomas Link (mailto:samul AT web de?subject=vim-tEchoPair)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-03-24.
" @Last Change: 2007-03-27.
" @Revision:    0.1.175

if &cp || exists("loaded_techopair")
    finish
endif
let loaded_techopair = 1

" if !exists('g:tEchoPairStyle') | let g:tEchoPairStyle = 'inner' | endif
if !exists('g:tEchoPairStyle')   | let g:tEchoPairStyle = 'indicate' | endif
if !exists('g:tEchoPairInstall') | let g:tEchoPairInstall = [] | endif

if !exists('g:tEchoPairIndicateOpen')   | let g:tEchoPairIndicateOpen = ' <<<&'       | endif
if !exists('g:tEchoPairIndicateClose')  | let g:tEchoPairIndicateClose = '&>>> '      | endif
if !exists('g:tEchoPairIndicateCursor') | let g:tEchoPairIndicateCursor = ' <<<&>>> ' | endif

if !exists('g:tEchoPairs')
                " \ 'viki': [
                "     \ 'fold;-1',
                " \ ], 
    let g:tEchoPairs = {
                \ 'ruby': [
                    \ ['(', ')'],
                    \ ['{', '}'],
                    \ ['[', ']'],
                    \ ['\<\(module\|class\|def\|begin\|do\|if\|unless\)\>', '', '\<end\>', 'TEchoSkipRuby()'],
                \ ],
                \ 'vim': [
                    \ ['(', ')'],
                    \ ['{', '}'],
                    \ ['[', ']'],
                    \ ['\<for\>', '\<endfor\?\>'],
                    \ ['\<wh\(i\(l\(e\)\?\)\?\)\?\>', '\<endw\(h\(i\(l\(e\)\?\)\?\)\?\)\?\>'],
                    \ ['\<if\>', '\<end\(i\(f\)\?\)\?\>'],
                    \ ['\<try\>', '\<endt\(r\(y\)\?\)\?\>'],
                    \ ['\<fu\(n\(c\(tion\)\?\)\?\)\?\>', '\<endf\(u\(n\(c\(tion\)\?\)\?\)\?\)\?\>'],
                \ ]
            \ }
endif

if !exists('g:tEchoPairStyle_inner')
    let g:tEchoPairStyle_inner = ['lisp', 'scheme']
endif

if !exists('g:tEchoPairStyle_indicate')
    let g:tEchoPairStyle_indicate = []
endif

if !exists('g:tEchoPairStyles')
endif

let s:tEchoPairStyles = []

fun! TEchoCollectStyles()
    redir => vars
    silent let
    redir END
    let s:tEchoPairStyles = split(vars, '\n')
    call filter(s:tEchoPairStyles, 'v:val =~ ''^tEchoPairStyle_''')
    call map(s:tEchoPairStyles, 'matchstr(v:val, ''^tEchoPairStyle_\zs\S\+'')')
endf

" fun! TEchoPair(backwards, type, ?what, ?[args])
fun! TEchoPair(backwards, type, ...)
    let lz = &lazyredraw
    set lazyredraw
    try
        let w0 = line('w0')
        let l0 = line('.')
        let c0 = col('.')
        " let c0 = col['.']
        let pos = getpos('.')
        let what = a:0 >= 1 ? a:1 : '.'
        if a:type == 'rx'
            let args0 = a:0 >= 2 ? a:2 : []
            let args = []
            " call add(args, '\V'. escape(get(args0, 0, '('), '\'))
            call add(args, '\V'. get(args0, 0, '('))
            " let m = escape(get(args0, 1, ''), '\')
            let m = get(args0, 1, '')
            call add(args, empty(m) ? m : '\V'.m)
            " call add(args, '\V'. escape(get(args0, 2, ')'), '\'))
            call add(args, '\V'. get(args0, 2, ')'))
            call add(args, a:backwards ? 'bW' : 'W')
            let args += args0[3:-1]
            echom "DBG searchpairs: ". string(args)
            " echom "DBG TEchoPair: ". string(args)
            let what = a:backwards ? args[0] : args[2]
            let this = a:backwards ? args[2] : args[0]
            " echom 'DBG '. what .' '. a:backwards .' '. expand('<cword>') .' '. (expand('<cword>')=~ '\V\^'. what .'\$')
            if a:backwards
                if expand('<cword>') =~ '\V\^'. this .'\$'
                    call search('\V'. this, 'cb', l0)
                endif
            else
                if expand('<cword>') =~ '\V\^'. this .'\$'
                    call search('\V'. this, 'c', l0)
                endif
            endif
            call call('searchpair', args)
        elseif a:type =~ '^fold'
            let fold_args = split(a:type, ';')
            if a:backwards
                call s:Normal('[z', 'silent!')
            else
                call s:Normal(']z', 'silent!')
            endif
            let lineshift = get(fold_args, a:backwards ? 1 : 2, 0)
            if lineshift > 0
                call s:Normal(lineshift .'j', 'silent!')
            elseif lineshift < 0
                call s:Normal((-lineshift) .'k', 'silent!')
            endif
        endif
        " else
        "     if a:backwards
        "         exec 'norm! ['. a:what
        "     else
        "         exec 'norm! ]'. a:what
        "     endif
        " endif
        let l1 = line('.')
        let c1 = col('.')
        if l1 != l0
            if a:backwards
                let c0 = col('$')
            else
                " let c0 = matchend(getline(l1), '^\s*') - 1
                let c0 = matchend(getline(l1), '^\s*')
            endif
        endif
        let text  = getline(l1)
        if empty(s:tEchoPairStyles)
            call TEchoCollectStyles()
        endif
        if exists('b:tEchoPairStyle')
            let style = b:tEchoPairStyle
        else
            let style = g:tEchoPairStyle
            for s in s:tEchoPairStyles
                if index(g:tEchoPairStyle_{s}, &filetype) != -1
                    let style = s
                endif
            endfor
        endif
        " if l1 < l0 || c1 < c0
        if a:backwards
            let text = TEchoPair_open_{style}(what, text, c0, l0, c1, l1)
        else
            let text = TEchoPair_close_{style}(what, text, c0, l0, c1, l1)
        endif
        if &debug != ''
            echo text . ' '. a:backwards .':'. c0.'x'.l0.'-'.c1.'x'.l1
        else
            echo text
        endif
        let b:tEchoPair = text
        call s:Normal(w0 .'zt')
        call setpos('.', pos)
        " return a:what
    finally
        let &lz = lz
    endtry
endf

fun! s:Normal(cmd, ...)
    let p = a:0 >= 1 ? a:1 : ''
    let m = mode()
    if m ==? 's' || m == ''
        exec p .' norm! '. a:cmd
    else
        exec p .'norm! '. a:cmd
    endif
endf

fun! TEchoPair_open_inner(what, text, c0, l0, c1, l1)
    return strpart(a:text, a:c1 - 1, a:c0 - a:c1)
endf

fun! TEchoPair_close_inner(what, text, c0, l0, c1, l1)
    return strpart(a:text, a:c0, a:c1 - a:c0)
endf

fun! TEchoPair_open_indicate(what, text, c0, l0, c1, l1)
    let text = s:IndicateCursor(a:text, a:c0, a:l0, a:c1, a:l1)
    " let text = a:l1.': '. substitute(text, '\V\%'. a:c1 .'c'. a:what, ' <<<&<<< ', '')
    let text = a:l1.': '. substitute(text, '\V\%'. a:c1 .'c'. a:what, g:tEchoPairIndicateOpen, '')
    let cmdh = s:GetCmdHeight()
    if cmdh > 1
        let acc = [text]
        for i in range(a:l1 + 1, a:l1 + cmdh - 1)
            if i > a:l0
                break
            endif
            " call add(acc, i.': '. s:IndicateCursor(getline(i), a:c0, a:l0, a:c1, i))
            call add(acc, i.': '. getline(i))
        endfor
        let text = join(acc, "\<c-j>")
    endif
    return text
endf

fun! TEchoPair_close_indicate(what, text, c0, l0, c1, l1)
    " let text = substitute(a:text, '\V\%'. a:c1 .'c'. a:what, ' >>>&>>> ', '')
    let text = substitute(a:text, '\V\%'. a:c1 .'c'. a:what, g:tEchoPairIndicateClose, '')
    let text = a:l1.': '. s:IndicateCursor(text, a:c0, a:l0, a:c1, a:l1)
    let cmdh = s:GetCmdHeight()
    if cmdh > 1
        let acc = [text]
        for i in range(a:l1 - 1, a:l1 - cmdh + 1, -1)
            if i < a:l0
                break
            endif
            " call insert(acc, i.': '. s:IndicateCursor(getline(i), a:c0, a:l0, a:c1, i))
            call insert(acc, i.': '. getline(i))
        endfor
        let text = join(acc, "\<c-j>")
    endif
    return text
endf

fun! s:IndicateCursor(text, c0, l0, c1, l1)
    if a:l0 == a:l1
        return substitute(a:text, '\%'. a:c0 .'c.', g:tEchoPairIndicateCursor, '')
    else
        return a:text
    endif
endf

fun! s:GetCmdHeight()
    let ch = &cmdheight
    if mode() == 'i' && &showmode
        let ch -= 1
    endif
    return ch
endf

fun! TEchoSkipRuby()
    let n = synIDattr(synID(line('.'), col('.'), 1), 'name')
    return (n =~ '^ruby\(Comment\|String\)$')
endf

fun! TEchoPairReset()
    augroup TEchoPair
        au!
    augroup END
endf
" call TEchoPairReset()

fun! TEchoPairInstall(pattern, ...)
    augroup TEchoPair
        if a:0 >= 1
            let list = a:1
        elseif has_key(g:tEchoPairs, &filetype)
            let list = g:tEchoPairs[&filetype]
        else
            " [a b]
            " [['(', ')'], ['{', '}']]
            let list = split(&matchpairs, ',')
            call map(list, 'split(v:val, ":")')
        endif
        for i in list
            if type(i) == 1
                if i =~ '^fold'
                    exec 'au CursorMoved '. a:pattern .' if foldclosed(line(".")) == -1 '.
                                \ ' | keepjumps call TEchoPair(1, '. string(i) .') | endif'
                endif
            elseif type(i) == 3
                if len(i) == 2
                    let [io, ie] = i
                    call insert(i, '', 1)
                    let im = ''
                else
                    let [io, im, ie; rest] = i
                endif
                if len(io) == 1
                    let condo = 'getline(".")[col(".") - 1] == '. string(io)
                else
                    " let condo = 'strpart(getline("."), col(".") - 1) =~ ''\V\^'''. string(io)
                    let condo = 'expand("<cword>") =~ ''\V\^'. io .'\$'''
                endif
                if len(ie) == 1
                    let conde = 'getline(".")[col(".") - 1] == '. string(ie)
                else
                    " let conde = 'strpart(getline("."), col(".") - 1) =~ ''\V\^'''. string(io)
                    let conde = 'expand("<cword>") =~ ''\V\^'. ie .'\$'''
                endif
                exec 'au CursorMoved '. a:pattern .' if '. condo . 
                            \ ' | keepjumps call TEchoPair(0, "rx", '. string(ie) .', '. string(i) .') | endif'
                exec 'au CursorMoved '. a:pattern .' if '. conde . 
                            \ ' | keepjumps call TEchoPair(1, "rx", '. string(io) .', '. string(i) .') | endif'
                exec 'au CursorMovedI '. a:pattern .' if '. condo . 
                            \ ' | keepjumps call TEchoPair(0, "rx", '. string(ie) .', '. string(i) .') | endif'
                exec 'au CursorMovedI '. a:pattern .' if '. conde . 
                            \ ' | keepjumps call TEchoPair(1, "rx", '. string(io) .', '. string(i) .') | endif'
            endif
        endfor
    augroup END
endf

" command! TEchoPairInstallGlobal call TEchoPairInstall('*')
command! TEchoPairInstallBuffer call TEchoPairInstall(expand('%:p'))

for ft in g:tEchoPairInstall
    exec 'au Filetype '. ft .' TEchoPairInstallBuffer'
endfor


finish
____________________________________________________________________

tEchoPair.vim -- Display matching parenthesis in the echo area


VIM is an excellent editor but in comparison too e.g. emacs it lacks a 
minor feature that makes editing lisp code somewhat cumbersome. While 
VIM can highlight the matching parenthesis, this doesn't help much with 
long functions when the matching parenthesis is off the screen. 
Emacs users are better off in such a situation, since emace displays the 
matching line in the echo area.

This plugin tries to mimic Emacs's behaviour.


In order to enable this plugin, you choose between the following  
options:
    TEchoPairInstallBuffer ... enable for the current buffer

    call TEchoPairInstall('*') ... enable globally

    let g:tEchoPairInstall = ['lisp', 'scheme'] ... enable for certain 
      filetypes


Currently, there are the following display modes:
    indicate ... Display the whole line and highlight the matching 
      parenthesis. If 'cmdheight' is greater than 1, additional lines 
      are display.

    inner ... Display the inner text Emacs-style.

In order to see the matching parenthesis when 'showmode' is on, set 
'cmdheight' to something greater than 1.

You can select the preferred display mode on a filetype basis, by 
setting g:tEchoPairStyle_{STYLE}.

Example:
    let g:tEchoPairStyle_inner = ['lisp', 'scheme']
    let g:tEchoPairStyle_indicate = ['java']


The pairs are usually deduced from the value of 'matchpairs' unless 
there is an entry for the current buffer's filetype in g:tEchoPairs. For 
the following filetypes custom pairs are pre-defined:
    - ruby
    - vim


CHANGES

0.1:
Initial release

