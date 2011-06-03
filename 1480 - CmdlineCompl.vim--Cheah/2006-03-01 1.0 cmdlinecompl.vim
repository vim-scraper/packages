" Vim global plugin for command-line completion
" Last Change: March 1, 2006
" Maintainer: Po Shan Cheah (vim@cheah.endjunk.com)
" Version: 1.0
"
" See also:
" 
" http://vim.sourceforge.net/scripts/script.php?script_id=474
" http://vim.sourceforge.net/scripts/script.php?script_id=147
"
" Description:
" 
" Based on ideas from the above two plugins, but also adds the ability to
" word-complete in a regular (non-search) command line and to word-complete
" in the middle of the command line. To use, hit F12 while in the command
" line. Hit F12 again to get a different completion. Repeated invocations
" will cycle through all possible completions. Word completion will be done
" in the context of the current buffer.
"
" Installation:
"
" Drop this file into your plugin directory.
"
" Customization:
"
" To inhibit loading of this plugin, define the loaded_cmdline_compl
" variable.
"
" To change the key mapping, cmap the desired key to
" <Plug>CmdlinecomplComplete. For example:
"
"   cmap <F11> <Plug>CmdlinecomplComplete
"

if exists("loaded_cmdline_compl")
    finish
endif
let loaded_cmdline_compl = 1

let s:save_cpo = &cpoptions
set cpoptions&vim

let s:lastcmdline = ""
let s:lastcmdpos = ""

function! <SID>CmdlineComplete()
    let cmdline = getcmdline()
    let cmdpos = getcmdpos()
    if cmdline == s:lastcmdline && cmdpos == s:lastcmdpos
	" User invoked completion again without moving cursor or editing
	let cmdline = s:origcmdline
	let cmdpos = s:origcmdpos
	let s:completeDepth = s:completeDepth . "\<C-N>"
    else
	" Starting new completion
	let s:origcmdline = cmdline
	let s:origcmdpos = cmdpos
	let s:completeDepth = "\<C-N>"
    endif

    " Set paste option to disable indent
    let paste = &l:paste
    setlocal paste

    let savecol = col(".")

    execute "normal! o" . cmdline . "\<C-O>" . cmdpos . "|" . s:completeDepth

    " Update the command line and position.
    let s:lastcmdline = getline(".")
    let s:lastcmdpos = col(".") + 1

    " Check if we have come full circle and reset the search depth. This
    " needs to be done because Vim behaves in a weird way if too many
    " Ctrl-Ns are used in a macro.
    if s:lastcmdline == s:origcmdline
	let s:completeDepth = ""
    endif

    " Undo changes and restore cursor position
    execute "normal! u" . savecol . "|"

    " Restore paste option
    let &l:paste = paste

    let tmp = setcmdpos(s:lastcmdpos)
    return s:lastcmdline
endfunction

if !hasmapto('<Plug>CmdlinecomplComplete', 'c')
    cmap <unique> <F12> <Plug>CmdlinecomplComplete
endif
cnoremap <unique> <script> <Plug>CmdlinecomplComplete  <SID>CmdlineComplete

cnoremap <SID>CmdlineComplete <C-\>e<SID>CmdlineComplete()<cr>


let &cpoptions = s:save_cpo

" vim:fo=cqro tw=75 com=\:\" sw=4
