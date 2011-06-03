" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/stakeholders.txt	[[[1
124
*stakeholders.txt*  Charismatic, overachieving, dynamically expansive place-holders
                    Author: Tom Link, micathom at gmail com

Older templating plugins for VIM often used simple markup for placeholders 
(e.g. <+VARIABLE_NAME+>). A placeholder was simply a pattern you could easily 
search for. Some newer templating plugins support dynamic replacement of 
placeholders with the same name.

The stakeholders plugin turns your old templating system into a high 
potential overachiever. It provides dynamic replacement of placeholders 
for older templating plugins. By default, it assumes placeholders match 
/<+\([[:alpha:]_]\+\)+>/ since this pattern is used in several 
templating systems. You can easily change that pattern by setting 
|g:stakeholders#def| or b:stakeholders_def.


Usage~

The tskeleton templating plugin provides a parameter 
(|g:tskeleton#enable_stakeholders|) that enables integration with the 
stakeholders plugin. For other templating plugins, you have to enable 
stakeholders by calling either |stakeholders#EnableBuffer()| or 
|stakeholders#EnableInRange()|.

Demo: 
http://vimsomnia.blogspot.com/2010/11/tskeleton-and-stakeholders-vim-plugins.html


Known issues~

    - Replacing a visual selection by pasting text (using paste#Paste()) 
      doesn't work. You have to delete the selection first and paste the 
      text afterwards.


Also available via git: http://github.com/tomtom/stakeholders_vim/

-----------------------------------------------------------------------
Install~

In order to install the vba, open the vba file in VIM and type: >

    :so %

See :help vimball for details.


========================================================================
Contents~

        g:stakeholders#def .................... |g:stakeholders#def|
        g:stakeholders#expansion .............. |g:stakeholders#expansion|
        g:stakeholders#exclude_rx ............. |g:stakeholders#exclude_rx|
        g:stakeholders#undo_breaks ............ |g:stakeholders#undo_breaks|
        stakeholders#Disable .................. |stakeholders#Disable()|
        stakeholders#Enable ................... |stakeholders#Enable()|
        stakeholders#EnableInRange ............ |stakeholders#EnableInRange()|
        stakeholders#EnableBuffer ............. |stakeholders#EnableBuffer()|
        stakeholders#DisableBuffer ............ |stakeholders#DisableBuffer()|
        stakeholders#immediate#Init ........... |stakeholders#immediate#Init()|
        stakeholders#delayed#Init ............. |stakeholders#delayed#Init()|


========================================================================
autoload/stakeholders.vim~

                                                    *g:stakeholders#def*
g:stakeholders#def             (default: {'rx': '<+\([[:alpha:]_]\+\)+>'})
    The placeholder definition. A dictionary with the fields:
      rx ....... A |regexp| that matches placeholders.

                                                    *g:stakeholders#expansion*
g:stakeholders#expansion       (default: 'immediate')
    The type of placeholder expansion. Possible values:
      - delayed (see |stakeholders#delayed#Init()|)
      - immediate (see |stakeholders#immediate#Init()|)

                                                    *g:stakeholders#exclude_rx*
g:stakeholders#exclude_rx      (default: '^\(TODO\|\)$')
    Ignore placeholders with labels matching this |regexp|.

                                                    *g:stakeholders#undo_breaks*
g:stakeholders#undo_breaks     (default: 0)
    If non-null, break the undo sequence (see |i_CTRL-G_u|) before 
    updating the replacement string.

                                                    *stakeholders#Disable()*
stakeholders#Disable()

                                                    *stakeholders#Enable()*
stakeholders#Enable()

                                                    *stakeholders#EnableInRange()*
stakeholders#EnableInRange(line1, line2)
    Enable stakeholders for a range of lines.

                                                    *stakeholders#EnableBuffer()*
stakeholders#EnableBuffer()
    Enable stakeholders for the current buffer.

                                                    *stakeholders#DisableBuffer()*
stakeholders#DisableBuffer()
    Disable stakeholders for the current buffer.


========================================================================
autoload/stakeholders/immediate.vim~

                                                    *stakeholders#immediate#Init()*
stakeholders#immediate#Init(ph_def)
    Expand placeholders as the user types.


========================================================================
autoload/stakeholders/delayed.vim~

                                                    *stakeholders#delayed#Init()*
stakeholders#delayed#Init(ph_def)
    Replace placeholders after the cursor leaves the pre-defined place or 
    the moves to another line.



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/stakeholders.vim	[[[1
42
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/stakeholders_vim
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-11-19.
" @Last Change: 2010-11-22.
" @Revision:    10
" GetLatestVimScripts: 3326 0 :AutoInstall: stakeholders.vim

if &cp || exists("loaded_stakeholders")
    finish
endif
let loaded_stakeholders = 2

let s:save_cpo = &cpo
set cpo&vim


" :display: :StakeholdersEnable[!]
" Enable stakeholders for the current buffer.
" With the optional '!', enable stakeholders globally.
command! -bang StakeholdersEnable
            \ if empty("<bang>")
            \ | call stakeholders#EnableBuffer()
            \ | else
            \ | call stakeholders#Enable()
            \ | endif


" :display: :StakeholdersDisable[!]
" Disable stakeholders for the current buffer.
" With the optional '!', disable stakeholders globally.
command! -bang StakeholdersDisable
            \ if empty("<bang>")
            \ | call stakeholders#DisableBuffer()
            \ | else
            \ | call stakeholders#Disable()
            \ | endif


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/stakeholders.vim	[[[1
319
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-11-02.
" @Last Change: 2010-11-20.
" @Revision:    770


if !exists('g:stakeholders#def')
    " The placeholder definition. A dictionary with the fields:
    "   rx ....... A |regexp| that matches placeholders.
    let g:stakeholders#def = {'rx': '<+\([[:alpha:]_]\+\)+>'}   "{{{2
endif


if !exists('g:stakeholders#expansion')
    " The type of placeholder expansion. Possible values:
    "   - delayed (see |stakeholders#delayed#Init()|)
    "   - immediate (see |stakeholders#immediate#Init()|)
    let g:stakeholders#expansion = 'immediate'   "{{{2
endif


if !exists('g:stakeholders#exclude_rx')
    " Ignore placeholders with labels matching this |regexp|.
    let g:stakeholders#exclude_rx = '^\(TODO\|\)$'   "{{{2
endif


if !exists('g:stakeholders#undo_breaks')
    " If non-null, break the undo sequence (see |i_CTRL-G_u|) before 
    " updating the replacement string.
    let g:stakeholders#undo_breaks = 0   "{{{2
endif




if !has_key(g:stakeholders#def, 'Replace')
    " :nodoc:
    function! g:stakeholders#def.Replace(text) dict "{{{3
        return substitute(a:text, self.placeholder_rx, escape(self.replacement, '\&~'), 'g')
    endf
endif


if !has_key(g:stakeholders#def, 'ReplacePlaceholderInCurrentLine')
    " :nodoc:
    function! g:stakeholders#def.ReplacePlaceholderInCurrentLine(pos, line, rline) dict "{{{3
        let m = matchlist(a:rline, printf(self.prepost_rx_fmt, self.replacement))
        " TLogVAR m
        if empty(m)
            let n1 = len(self.pre)
            echom "Internal error stakeholders#def.ReplacePlaceholderInCurrentLine:" a:rline self.prepost_rx_fmt
        else
            let n1 = len(m[1])
        endif
        let pre = self.Replace(self.pre)
        let n2 = len(pre)
        let post = self.Replace(self.post)
        let line1 = pre . self.replacement . post
        " echom "DBG End" n1 n2 pre
        let pos = copy(a:pos)
        if n1 != n2
            let delta = - n1 + n2
            let pos[2] += delta
        endif
        return [pos, line1]
    endf
endif


let s:enable_globally = 0

function! stakeholders#Disable() "{{{3
    if exists('#stakeholders')
        au! stakeholders
        aug! stakeholders
        let view = winsaveview()
        try
            silent windo unlet! w:stakeholders
            silent bufdo unlet! b:stakeholders
        finally
            call winrestview(view)
        endtry
    endif
    let s:enable_globally = 0
endf


function! stakeholders#Enable() "{{{3
    if !s:enable_globally
        if !exists('#stakeholders')
            augroup stakeholders
                autocmd!
            augroup END
        endif
        autocmd stakeholders BufNewFile,BufReadPost * call stakeholders#EnableBuffer()
        let view = winsaveview()
        try
            silent bufdo call stakeholders#EnableBuffer()
        finally
            call winrestview(view)
        endtry
        let s:enable_globally = 1
    endif
endf


" Enable stakeholders for a range of lines.
function! stakeholders#EnableInRange(line1, line2) "{{{3
    if !exists('b:stakeholders')
        let b:stakeholders_range = [a:line1, a:line2]
        " echom "DBG stakeholders#EnableInRange" string(b:stakeholders_range)
        call stakeholders#EnableBuffer()
    endif
endf


" Enable stakeholders for the current buffer.
function! stakeholders#EnableBuffer() "{{{3
    if !exists('#stakeholders')
        augroup stakeholders
            autocmd!
        augroup END
    endif
    if !exists('b:stakeholders')
        let b:stakeholders = exists('b:stakeholders_def') ? 
                    \ b:stakeholders_def : g:stakeholders#def
        " echom "DBG stakeholders#EnableBuffer" b:stakeholders
        autocmd stakeholders CursorMoved,CursorMovedI <buffer> call s:CursorMoved(mode())
        " autocmd stakeholders InsertEnter,InsertLeave <buffer> call s:CursorMoved(mode())
        call s:CursorMoved('n')
    endif
endf


" Disable stakeholders for the current buffer.
function! stakeholders#DisableBuffer() "{{{3
    if exists('b:stakeholders')
        unlet! b:stakeholders b:stakeholders_range w:stakeholders
        autocmd! stakeholders CursorMoved,CursorMovedI <buffer>
    endif
endf


function! s:SetContext(pos, mode) "{{{3
    if !exists('b:stakeholders')
        return a:pos
    endif
    let pos = a:pos
    if exists('w:stakeholders.End')
        let pos = w:stakeholders.End(pos)
    endif
    " TLogVAR pos
    let lnum = pos[1]
    if exists('b:stakeholders_range')
                \ && (lnum < b:stakeholders_range[0]
                \ || lnum > b:stakeholders_range[1])
        call stakeholders#DisableBuffer()
    else
        let w:stakeholders = copy(b:stakeholders)
        let w:stakeholders.lnum = lnum
        let line = getline(lnum)
        if line !~ w:stakeholders.rx
            let w:stakeholders.line = ''
        else
            let w:stakeholders.line = line
            " TLogVAR a:mode, mode()
            let col = s:Col(pos[2], a:mode)
            call s:SetParts(w:stakeholders, line, col)
        endif
        " TLogVAR w:stakeholders
    endif
    return pos
endf


function! s:SetParts(ph_def, line, col) "{{{3
    " function! stakeholders#SetParts(ph_def, line, col) "{{{3
    " TLogVAR a:col
    let a:ph_def.pre = ''
    let parts = split(a:line, w:stakeholders.rx .'\zs')
    let c = 0
    for i in range(len(parts))
        let part = parts[i]
        let plen = c + len(part)
        " TLogVAR plen
        if plen < a:col
            let a:ph_def.pre .= part
            let c = plen
        else
            let phbeg = match(part, w:stakeholders.rx .'$')
            if phbeg != -1
                let pre = strpart(part, 0, phbeg)
                let prelen = c + len(pre)
                " TLogVAR prelen
                if prelen <= a:col
                    let a:ph_def.pre .= pre
                    let placeholder = strpart(part, phbeg)
                    let a:ph_def.placeholder = placeholder
                    let a:ph_def.post = join(parts[i + 1 : -1], '')
                    " TLogVAR a:ph_def
                    break
                endif
            endif
            " let a:ph_def.pre .= join(parts[i : -1], '')
            " TLogVAR a:ph_def
            break
        endif
    endfor
    return a:ph_def
endf


function! s:Col(col, mode) "{{{3
    " TLogVAR a:col, a:mode
    let col = a:col
    if a:mode == 'n' " && col < len(getline(a:pos[1]))
        let col -= 1
    elseif a:mode =~ '^[sv]' && &selection[0] == 'e'
        let col -= 1
    endif
    " TLogVAR col
    return col
endf


function! s:CursorMoved(mode) "{{{3
    let pos = getpos('.')
    " TLogVAR a:mode, pos
    try
        let lnum = pos[1]
        if exists('w:stakeholders.placeholder') && !empty(w:stakeholders.line) && w:stakeholders.lnum == lnum
            " TLogVAR w:stakeholders.placeholder
            let line = getline(lnum)
            " TLogVAR line
            if line != w:stakeholders.line
                let pre0 = w:stakeholders.pre
                let post0 = w:stakeholders.post
                let init = !has_key(w:stakeholders, 'replacement')
                if !init
                    let pre0 = w:stakeholders.Replace(pre0)
                    let post0 = w:stakeholders.Replace(post0)
                endif
                " TLogVAR pre0, post0
                let lpre = len(pre0)
                let lpost = len(line) - len(post0)
                let cpre = s:Col(lpre, a:mode) + 1
                let cpost = s:Col(lpost, a:mode) + 1
                let col = s:Col(pos[2], a:mode)
                " TLogVAR col, cpre, cpost
                if col >= cpre && col <= cpost
                    let spre = strpart(line, 0, lpre)
                    let spost = line[lpost : -1]
                    " TLogVAR pre0, post0
                    " TLogVAR spre, spost
                    if spre == pre0 && (empty(spost) || spost == post0)
                        let replacement = line[lpre : lpost - 1]
                        let placeholder = replacement[-len(w:stakeholders.placeholder) : -1]
                        " TLogVAR replacement, placeholder, w:stakeholders.placeholder
                        if !init || placeholder != w:stakeholders.placeholder
                            if init
                                call s:Init(w:stakeholders, pos)
                            endif
                            let w:stakeholders.replacement = replacement
                            if g:stakeholders#undo_breaks && a:mode == 'i'
                                call feedkeys("\<c-g>u")
                            endif
                            " TLogVAR w:stakeholders.replacement
                            if exists('w:stakeholders.Update')
                                let pos = w:stakeholders.Update(pos)
                            endif
                            return
                        endif
                    endif
                endif
            endif
        endif
        let pos = s:SetContext(pos, a:mode)
    finally
        call setpos('.', pos)
    endtry
endf


function! s:Init(ph_def, pos) "{{{3
    let a:ph_def.lnum = a:pos[1]
    " TLogVAR getline(a:ph_def.lnum)
    let a:ph_def.lines = {}
    let a:ph_def.placeholder_rx = '\V'. escape(a:ph_def.placeholder, '\')
    let pre_fmt = substitute(a:ph_def.pre, '%', '%%', 'g')
    let post_fmt = substitute(a:ph_def.post, '%', '%%', 'g')
    let a:ph_def.prepost_rx_fmt = '\V\^\('
                \ . substitute(pre_fmt, a:ph_def.placeholder_rx, '\\(\\.\\{-}\\)', 'g')
                \ .'\)%s\('
                \ . substitute(post_fmt, a:ph_def.placeholder_rx, '\\(\\.\\{-}\\)', 'g')
                \ .'\)\$'
    " TLogVAR a:ph_def
    if exists('b:stakeholders_range')
        let range = join(b:stakeholders_range, ',')
    else
        let range = ''
    endif
    try
        exec 'keepjumps' range .'g/'. escape(a:ph_def.placeholder_rx, '/') .'/let a:ph_def.lines[line(".")] = getline(".")'
    finally
        keepjumps call setpos('.', a:pos)
    endtry
    call stakeholders#{g:stakeholders#expansion}#Init(a:ph_def)
endf


finish

n1: foo <+FOO+> bar
n2: foo <+FOO+> bar bla <+FOO+> bla
<+FOO+> bar bla <+FOO+>
foo <+FOO+> bar bla <+FOO+>

autoload/stakeholders/delayed.vim	[[[1
40
" delayed.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-11-05.
" @Last Change: 2010-11-19.
" @Revision:    59


let s:prototype = {} "{{{2

" Replace placeholders after the cursor leaves the pre-defined place or 
" the moves to another line.
function! stakeholders#delayed#Init(ph_def) "{{{3
    call extend(a:ph_def, s:prototype)
    return a:ph_def
endf


" " :nodoc:
" function! s:prototype.Update(pos) dict "{{{3
"     return a:pos
" endf


" :nodoc:
function! s:prototype.End(pos) dict "{{{3
    echom "DBG delayed.End" self.placeholder
    let pos = a:pos
    for [lnum, line] in items(self.lines)
        if lnum == self.lnum
            let [pos, line1] = self.ReplacePlaceholderInCurrentLine(pos, line, getline('.'))
        else
            let line1 = self.Replace(line)
        endif
        keepjumps call setline(lnum, line1)
    endfor
    return pos
endf


autoload/stakeholders/immediate.vim	[[[1
41
" immediate.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-11-05.
" @Last Change: 2010-11-19.
" @Revision:    37


let s:prototype = {} "{{{2

" Expand placeholders as the user types.
function! stakeholders#immediate#Init(ph_def) "{{{3
    call extend(a:ph_def, s:prototype)
    return a:ph_def
endf


" :nodoc:
function! s:prototype.Update(pos) dict "{{{3
    if a:pos[1] == line('.')
        let pos = a:pos
        for [lnum, line] in items(self.lines)
            if lnum == self.lnum
                let [pos, line1] = self.ReplacePlaceholderInCurrentLine(pos, self.line, getline(lnum))
            else
                let line1 = self.Replace(line)
            endif
            keepjumps call setline(lnum, line1)
        endfor
    endif
    return pos
endf


" " :nodoc:
" function! s:prototype.End(pos) dict "{{{3
"     unlet self.placeholder
"     return a:pos
" endf


