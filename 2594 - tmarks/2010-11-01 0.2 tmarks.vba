" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/tmarks.txt	[[[1
91
*tmarks.txt*        Browse & manipulate marks
                    Author: Tom Link, micathom at gmail com

This plugin provides commands to browse and to manipulate (place & 
delete) marks.

Features:

    - Browse marks
    - Delete all (lower-case) marks in the current buffer
    - Delete all (lower-case) marks in a range
    - Set the next available (lower-case) mark at a specified line

Key maps in the list view:

    <c-d> ... Delete mark
    <cr>  ... Jump to mark

If g:tmarks_key is set in your |vimrc| file, this plugin also provides the 
following maps:

    <c-KEY> ... Toggle the mark at the current line
    <KEY>   ... Jump to the next mark
    <s-KEY> ... Jump to the previous mark


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.

This script requires tlib (vimscript #1863) to be installed.

Also available via git: http://github.com/tomtom/tmarks_vim/


========================================================================
Contents~

        :TMarks ................... |:TMarks|
        :TMarkstoggle ............. |:TMarkstoggle|
        :TMarksdelete ............. |:TMarksdelete|
        :TMarksdeleteall .......... |:TMarksdeleteall|
        :TMarksnext ............... |:TMarksnext|
        tmarks#ToggleMarkAtLine ... |tmarks#ToggleMarkAtLine()|
        tmarks#Next ............... |tmarks#Next()|


========================================================================
plugin/tmarks.vim~

                                                    *:TMarks*
:TMarks
    Browse all marks.

                                                    *:TMarkstoggle*
:{range}TMarkstoggle
    Place the next available a-z mark at the specified line.

                                                    *:TMarksdelete*
:{range}TMarksdelete
    Delete all a-z marks in range.

                                                    *:TMarksdeleteall*
:TMarksdeleteall
    Delete all a-z marks in the current buffer.

                                                    *:TMarksnext*
:TMarksnext
    Jump to the nth prev/next mark.


========================================================================
autoload/tmarks.vim~

                                                    *tmarks#ToggleMarkAtLine()*
tmarks#ToggleMarkAtLine(?line='.')
    Toggle the mark at line.

                                                    *tmarks#Next()*
tmarks#Next(count)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
autoload/tmarks.vim	[[[1
143
" tmarks.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-29.
" @Last Change: 2010-10-23.
" @Revision:    0.0.123


if !exists('g:tmarks_handlers') "{{{2
    let g:tmarks_handlers = [
            \ {'key':  4, 'agent': 'tmarks#AgentDeleteMark', 'key_name': '<c-d>', 'help': 'Delete mark'},
            \ ]
            " \ {'pick_last_item': 0},
endif


" :nodoc:
function! tmarks#AgentDeleteMark(world, selected) "{{{3
    for l in a:selected
        call s:DelMark(s:GetMark(l))
    endfor
    let a:world.base  = s:GetList()
    let a:world.state = 'display'
    return a:world
endf


function! s:DelMark(m) "{{{3
    exec 'delmarks '. escape(a:m, '"\')
endf


function! s:GetList() "{{{3
    return tlib#cmd#OutputAsList('marks')[1:-1]
endf


function! s:GetLocalMarks(key_is_mark) "{{{3
    let local_marks = s:GetList()
    call filter(local_marks, 'v:val =~ '' \l ''')
    let marks = {}
    for line in local_marks
        let ml = matchlist(line, '^\s\(\l\)\s\+\(\d\+\)')[1:2]
        if !empty(ml)
            let marks[ml[!a:key_is_mark]] = ml[!!a:key_is_mark]
        endif
    endfor
    return marks
endf


function! s:GetMark(line) "{{{3
    return matchstr(a:line, '^ \+\zs\S')
endf


" :nodoc:
function! tmarks#List() "{{{3
    keepjumps let m = tlib#input#List('s', 'Marks', s:GetList(), g:tmarks_handlers)
    if !empty(m)
        exec 'norm! `'. s:GetMark(m)
    endif
endf


" Delete all (lower-case) marks at the specified line.
" :display: tmarks#DeleteInRange(?line1=line('.'), ?line2=line('.'))
" :nodoc:
function! tmarks#DeleteInRange(...) "{{{3
    TVarArg ['line1', line('.')], ['line2', line('.')]
    for [line, mark] in items(s:GetLocalMarks(0))
        if line >= line1 && line <= line2
            call s:DelMark(mark)
        endif
    endfor
endf


" Delete all (lower-case) marks for the current buffer.
" :nodoc:
function! tmarks#DeleteAllMarks() "{{{3
    for mark in keys(s:GetLocalMarks(1))
        call s:DelMark(mark)
    endfor
endf


let s:local_marks = split('abcdefghijklmnopqrstuvwxyz', '\zs')


" :display: tmarks#ToggleMarkAtLine(?line='.')
" Toggle the mark at line.
function! tmarks#ToggleMarkAtLine(...) "{{{3
    TVarArg ['line', line('.')]
    if line =~ '^\.\?$'
        let line = line('.')
    endif
    let lines = s:GetLocalMarks(0)
    " TLogVAR lines, marks
    if has_key(lines, line)
        let mark = lines[line]
        call s:DelMark(mark)
        echom 'Remove mark' mark 'at line' line
        return
    else
        let marks = s:GetLocalMarks(1)
        for mark in s:local_marks
            if !has_key(marks, mark)
                exec line .'mark '. mark
                echom 'Set mark' mark 'at line' line
                return
            endif
        endfor
    endif
    echohl Error
    echom 'TMarks: No mark available'
    echohl None
endf


function! tmarks#Next(count) "{{{3
    let cul = line('.')
    " TLogVAR a:count, cul
    let lines = keys(s:GetLocalMarks(0))
    if !empty(lines)
        call map(lines, 'printf("%99s", v:val)')
        call sort(lines)
        call map(lines, '0 + matchstr(v:val, ''\d\+'')')
        " TLogVAR lines
        if a:count > 0
            let lines = filter(copy(lines), 'v:val > cul')
                        \ + filter(copy(lines), 'v:val <= cul')
            exec lines[(a:count - 1) % len(lines)]
        elseif a:count < 0
            let lines = reverse(filter(copy(lines), 'v:val < cul'))
                        \ + reverse(filter(copy(lines), 'v:val >= cul'))
            exec lines[(-a:count - 1) % len(lines)]
        endif
    endif
endf


plugin/tmarks.vim	[[[1
68
" tmarks.vim -- Browse & manipulate marks
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-08-23.
" @Last Change: 2010-11-01.
" @Revision:    69
" GetLatestVimScripts: 2594 0 tmarks.vim

if &cp || exists("loaded_tmarks")
    finish
endif
if !exists('g:loaded_tlib') || g:loaded_tlib < 11
    runtime plugin/02tlib.vim
    if !exists('g:loaded_tlib') || g:loaded_tlib < 11
        echoerr 'tlib >= 0.11 is required'
        finish
    endif
endif
let loaded_tmarks = 2
let s:save_cpo = &cpo
set cpo&vim


" Browse all marks.
command! -bar TMarks call tmarks#List()


" Place the next available a-z mark at the specified line.
" :display: :{range}TMarkstoggle
command! -range -nargs=? -bar TMarkstoggle call tmarks#ToggleMarkAtLine(<line1>)

" Delete all a-z marks in range.
" :display: :{range}TMarksdelete
command! -range -nargs=? -bar TMarksdelete call tmarks#DeleteInRange(<line1>, <line2>)

" Delete all a-z marks in the current buffer.
" :display: :TMarksdeleteall
command! -bar TMarksdeleteall call tmarks#DeleteAllMarks()

" Jump to the nth prev/next mark.
" :display: :TMarksnext
command! -count=1 -bar TMarksnext call tmarks#Next(<count>)


" @TPluginInclude
if exists('g:tmarks_key')
    exec 'map <silent> <'. g:tmarks_key .'> :<c-u>call tmarks#Next(v:count1)<cr>'
    exec 'map <silent> <s-'. g:tmarks_key .'> :<c-u>call tmarks#Next(-v:count1)<cr>'
    exec 'map <silent> <c-'. g:tmarks_key .'> :call tmarks#ToggleMarkAtLine()<cr>'
endif


let &cpo = s:save_cpo
unlet s:save_cpo


finish

0.1
Initial release

0.2
- Moved the definition of some variables from plugin/tmarks.vim to 
autoload/tmarks.vim
- Removed some commands & related functions (there are better plugins for 
that): TMarksPlace, TMarksDelete, TMarksDeleteAll

