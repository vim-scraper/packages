" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/tcommand.txt	[[[1
74
*tcommand.txt*      Select and execute a command or menu item from a list
                    Author: Tom Link, micathom at gmail com

VIM plugins are cool but with time you forget about even useful 
commands. This plugin provides a command (|:TCommand|) that lets you 
select a command from a list (you can optionally display help on the 
currently hightlighted command) and then run it from the command line.


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.

This script requires tlib (vimscript #1863) to be installed.

Also available via git: http://github.com/tomtom/vimtlib/


Post-Install~

Suggested maps (to be set in your |vimrc| file): >

    noremap <Leader>: :TCommand<cr>


========================================================================
Contents~

        :TCommand ............... |:TCommand|
        g:tcommand#world ........ |g:tcommand#world|
        g:tcommand#hide_rx ...... |g:tcommand#hide_rx|
        g:tcommand#what ......... |g:tcommand#what|
        tcommand#Select ......... |tcommand#Select()|
        tcommand#Info ........... |tcommand#Info()|


========================================================================
plugin/tcommand.vim~

                                                    *:TCommand*
TCommand[!] [INITIAL_FILTER]
    With a '!', reset the list of known commands and menu items.


========================================================================
autoload/tcommand.vim~

                                                    *g:tcommand#world*
g:tcommand#world

                                                    *g:tcommand#hide_rx*
g:tcommand#hide_rx             (default: '\(^\(Toolbar\|Popup\|Hilfe\|Help\)\>\)')
    Hide entries matching this rx.

                                                    *g:tcommand#what*
g:tcommand#what                (default: {'command': 's:CollectCommands', 'menu': 's:CollectMenuItems'})
    The items that should be displayed.

                                                    *tcommand#Select()*
tcommand#Select(reset, filter)

                                                    *tcommand#Info()*
tcommand#Info(world, selected)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
autoload/tcommand.vim	[[[1
167
" tcommand.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-03-12.
" @Last Change: 2010-03-13.
" @Revision:    140

let s:save_cpo = &cpo
set cpo&vim


" :nodefault:
TLet g:tcommand#world = {
            \ 'type': 's',
            \ 'query': 'Select command',
            \ 'pick_last_item': 1,
            \ 'scratch_pos': 'leftabove',
            \ 'scratch_vertical': -1,
            \ 'resize_vertical': 34,
            \ 'key_handlers': [
                \ {'key':  15, 'agent': 'tcommand#Info', 'key_name': '<c-o>', 'help': 'Show info'},
                \ ]
            \ }


" Hide entries matching this rx.
TLet g:tcommand#hide_rx = '\(^\(Toolbar\|Popup\|Hilfe\|Help\)\>\)'


" The items that should be displayed.
TLet g:tcommand#what = {'command': 's:CollectCommands', 'menu': 's:CollectMenuItems'}


function! g:tcommand#world.SetStatusline(query) dict "{{{3
    echo
    echo self.DisplayFilter() .': '. matchstr(self.CurrentItem(), '^\S\+')
endf


let s:commands = []


function! tcommand#Select(reset, filter) "{{{3
    let w = copy(g:tcommand#world)
    if !empty(a:filter)
        let w.initial_filter = [[a:filter]]
    endif
    if empty(s:commands) || a:reset
        let s:commands = []
        for [what, fn] in items(g:tcommand#what)
            call call(fn, [s:commands])
        endfor
        if !empty(g:tcommand#hide_rx)
            call filter(s:commands, 'v:val !~ g:tcommand#hide_rx')
        endif
    endif
    let w.base = s:commands
    let v = winsaveview()
    let help = 0
    windo if &ft == 'help' | let help = 1 | endif
    try
        let item = tlib#input#ListD(w)
    finally
        if !help
            silent! windo if &ft == 'help' | exec 'wincmd c' | endif
        endif
        call winrestview(v)
    endtry
    if !empty(item)
        let [item, type, modifier, nargs] = split(item, '\t')
        let item = substitute(item, '\s\+$', '', '')
        if type == 'C'
            let feed = ':'. item
            if nargs == '0'
                let feed .= "\<cr>"
            else
                if modifier != '!'
                    let feed .= ' '
                endif
            endif
            call feedkeys(feed)
        elseif type == 'M'
            exec 'emenu '. item
        else
            echoerr 'TCommand: Internal error: '. item
        endif
    endif
endf


function! tcommand#Info(world, selected) "{{{3
    " TLogVAR a:selected
    let bufnr = bufnr('%')
    try
        let [item, type, modifier, nargs] = split(a:selected[0], '\t')
        if !empty(item)
            exec 'help '. item
        endif
    finally
        exec bufwinnr(bufnr) .'wincmd w'
    endtry
    let a:world.state = 'redisplay'
    return a:world
endf


function! s:CollectCommands(acc) "{{{3
    let commands = tlib#cmd#OutputAsList('command')
    call remove(commands, 0)
    call map(commands, 's:FormatCommand(v:val)')
    call extend(a:acc, commands)
endf


function! s:MatchCommand(string) "{{{3
    let match = matchlist(a:string, '\([!"b ]\)\s\+\(\S\+\)\s\+\(.\)')
    return match
endf


function! s:FormatCommand(cmd0) "{{{3
    let match = s:MatchCommand(a:cmd0)
    return printf("%-30s\tC\t%s\t%s", match[2], match[1], match[3])
endf


function! s:CollectMenuItems(acc) "{{{3
    let items = tlib#cmd#OutputAsList('menu')
    let menu = {0: ['']}
    let formattedmenuitem = ''
    for item in items
        let match = matchlist(item, '^\(\s*\)\(\d\+\)\s\+\([^-].\{-}\)\ze\(\^I\|$\)')
        if !empty(match)
            " TLogVAR item, match
            let level = len(match[1])
            let parentlevel = -1
            for prevlevel in keys(menu)
                if prevlevel > level
                    call remove(menu, prevlevel)
                elseif prevlevel > parentlevel && prevlevel < level
                    let parentlevel = prevlevel
                endif
            endfor
            let cleanitem = substitute(match[3], '&', '', 'g')
            " let cleanitem = substitute(cleanitem, '\\\+\.', '.', 'g')
            if parentlevel >= 0
                let parent = menu[parentlevel]
                let menuitem = parent + [cleanitem]
            else
                let menuitem = [cleanitem]
            endif
            let menu[level] = menuitem
            let formattedmenuitem = printf("%-30s\tM\t \t0", join(map(copy(menuitem), 'escape(v:val, ''\.'')'), '.'))
        elseif !empty(formattedmenuitem)
            if match(item, '^\s\+\l\*\s') != -1
                " TLogVAR formattedmenuitem
                call add(a:acc, formattedmenuitem)
            endif
            let formattedmenuitem = ''
        endif
    endfor
endf


let &cpo = s:save_cpo
unlet s:save_cpo
plugin/tcommand.vim	[[[1
39
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-03-12.
" @Last Change: 2010-03-13.
" @Revision:    7
" GetLatestVimScripts: 0 0 :AutoInstall: tcommand.vim
" Select and execute a command or menu item from a list

if &cp || exists("loaded_tcommand")
    finish
endif
if !exists('g:loaded_tlib') || g:loaded_tlib < 36
    runtime plugin/02tlib.vim
    if !exists('g:loaded_tlib') || g:loaded_tlib < 36
        echoerr 'tlib >= 0.36 is required'
        finish
    endif
endif
let loaded_tcommand = 1

let s:save_cpo = &cpo
set cpo&vim


" :display: TCommand[!] [INITIAL_FILTER]
" With a '!', reset the list of known commands and menu items.
command! -bang -nargs=? TCommand call tcommand#Select(!empty("<bang>"), <q-args>)


let &cpo = s:save_cpo
unlet s:save_cpo
finish

CHANGES:
0.1
- Initial release

