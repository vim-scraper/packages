" IndentConsistencyCopAutoCmds.vim: autocmds for IndentConsistencyCop.vim
"
" DESCRIPTION:
"   The autocmds in this script automatically trigger the IndentConsistencyCop
"   for certain, configurable filetypes (such as c, cpp, html, xml, which
"   typically contain lots of indented lines) once when you load the file in
"   VIM. The entire buffer will be checked for inconsistent indentation, and you
"   will receive a report on its findings. With this automatic background check,
"   you'll become aware of indentation problems before you start editing. 
"
" USAGE:
"   Triggering happens automatically; of cause, you can still start the
"   IndentConsistencyCop ex command to re-check the buffer after changes. 
"
"   For very large files, the check may take a couple of seconds. You can abort
"   the script run with CTRL-C, like any other VIM command. 
"
"   You can disable/re-enable the autocommands with 
"   :IndentConsistencyCopAutoCmdsOff and :IndentConsistencyCopAutoCmdsOff,
"   respectively. 
"
" INSTALLATION:
"   Put the script into your user or system VIM plugin directory (e.g.
"   ~/.vim/plugin). 
"
" DEPENDENCIES:
"   - Requires VIM 7.0 or higher. 
"   - Requires IndentConsistencyCop.vim (vimscript #1690). 
"
" CONFIGURATION:
"   If you don't like the default filetypes that are inspected, modify the
"   comma-separated list of filetypes in g:indentconsistencycop_filetypes. 
"
" Copyright: (C) 2006 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
"   1.10.003	21-Feb-2008	Avoiding multiple invocations of the
"				IndentConsistencyCop when reloading or switching
"				buffers. Now there's only one check per file and
"				VIM session. 
"   1.00.002	25-Nov-2006	Added commands :IndentConsistencyCopAutoCmdsOn
"				and :IndentConsistencyCopAutoCmdsOff
"				to re-enable/disable autocommands. 
"	0.01	16-Oct-2006	file creation

" Avoid installing twice or when in compatible mode
if exists("loaded_indentconsistencycopautocmds") || (v:version < 700)
    finish
endif
let loaded_indentconsistencycopautocmds = 1

if ! exists('g:indentconsistencycop_filetypes')
    let g:indentconsistencycop_filetypes = 'ant,c,cpp,cs,csh,css,dosbatch,html,java,javascript,jsp,lisp,pascal,perl,php,python,ruby,scheme,sh,sql,tcsh,vb,vbs,vim,wsh,xhtml,xml,xsd,xslt,zsh'
endif

"- functions ------------------------------------------------------------------
function! s:StartCopOnce()
    " The straightforward way to ensure that the Cop is called only once per
    " file is to hook into the BufRead event. We cannot do this, because at that
    " point modelines haven't been set yet and the filetype hasn't been
    " determined. 
    " Although the BufWinEnter hook removes itself after execution, it may still
    " be triggered multiple times in a VIM session, e.g. when switching buffers
    " (alternate file, or :next, ...) or when a plugin (like FencView) reloads
    " the buffer with changed settings.
    " Thus, we set a buffer-local flag. This ensures that the Cop is really only
    " called once per file in a VIM session, even when the buffer is reloaded
    " via :e!. (Only :bd and :e <file> will create a fresh buffer and cause a
    " new Cop run.) 
    if ! exists('b:indentconsistencycop_is_checked')
	let b:indentconsistencycop_is_checked = 1
	execute 'IndentConsistencyCop'
    endif
endfunction

function! s:StartCopBasedOnFiletype( filetype )
    let l:activeFiletypes = split( g:indentconsistencycop_filetypes, ', *' )
    if count( l:activeFiletypes, a:filetype ) > 0
	" Modelines have not been processed yet, but we need them because they
	" very likely change the buffer indent settings. So we set up a second
	" autocmd BufWinEnter (which is processed after the modelines), that
	" will trigger the IndentConsistencyCop and remove itself (i.e. a "run
	" once" autocmd). 
	augroup IndentConsistencyCopBufferCmds
	    autocmd!
	    " When a buffer is loaded, the FileType event will fire before the
	    " BufWinEnter event, so that the IndentConsistencyCop is triggered. 
	    autocmd BufWinEnter <buffer> call s:StartCopOnce() |  autocmd! IndentConsistencyCopBufferCmds * <buffer>
	    " When the filetype changes in an existing buffer, the BufWinEnter
	    " event is not fired. We use the CursorHold event to trigger the
	    " IndentConsistencyCop when the user pauses for a brief period.
	    " (There's no better event for that.)
	    autocmd CursorHold <buffer> call s:StartCopOnce() |  autocmd! IndentConsistencyCopBufferCmds * <buffer>
	augroup END
    endif
endfunction

function! s:IndentConsistencyCopAutoCmds(isOn)
    augroup IndentConsistencyCopAutoCmds
	autocmd!
	if a:isOn
	    autocmd FileType * call <SID>StartCopBasedOnFiletype( expand('<amatch>') )
	endif
    augroup END
endfunction

" Enable the autocommands. 
call s:IndentConsistencyCopAutoCmds(1)

"- commands -------------------------------------------------------------------
command! -nargs=0 IndentConsistencyCopAutoCmdsOn call <SID>IndentConsistencyCopAutoCmds(1)
command! -nargs=0 IndentConsistencyCopAutoCmdsOff call <SID>IndentConsistencyCopAutoCmds(0)

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
