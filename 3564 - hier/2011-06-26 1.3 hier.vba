" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/hier.txt	[[[1
79
*hier.txt*		For Vim version 7.3	       Last change: 2011 June 26
				    Copyright (c) 2011 Jan Christoph Ebersbach

Hier                                                                      *hier*

DESCRIPTION                    |hier-description|
USAGE                          |hier-usage|
CUSTOMIZATION                  |hier-customization|
INSTALLATION                   |hier-installation|
CHANGELOG                      |hier-changelog|

==============================================================================
DESCRIPTION                                                   *hier-description*

Highlight quickfix errors and location list entries in buffer. This plugin
was designed to support the editqf vim script
(http://www.vim.org/scripts/script.php?script_id=3557) but it also works
very well stand alone.

This script can be downloaded from
http://www.vim.org/scripts/script.php?script_id=3564. The latest development
version is available at https://github.com/jceb/vim-hier.

==============================================================================
USAGE                                                               *hier-usage*

The following commands are provided:
	:HierStart		" enable hier highlighting
	:HierStop		" disable hier highlighting
	:HierUpdate		" update error highlighting for current buffer
	:HierClear		" remove highlighting - it will be displayed
				" again when :HierUpdate is called

==============================================================================
CUSTOMIZATION                                               *hier-customization*

The highlight group can be customized by setting the following variables.
Setting a variable to the string "" will disable highlighting of that
group. Every type can be highlighted differently (error, warning, info):
	let g:hier_highlight_group_qf   = 'SpellBad'
	let g:hier_highlight_group_qfw  = 'SpellLocal'
	let g:hier_highlight_group_qfi  = 'SpellRare'

	let g:hier_highlight_group_loc  = 'SpellBad'
	let g:hier_highlight_group_locw = 'SpellLocal'
	let g:hier_highlight_group_loci = 'SpellRare'

Enable/disable highlighting highlighting by default:
	let g:hier_enabled              = 1

==============================================================================
INSTALLATION                                                 *hier-installation*

1. Download hier.vba.gz
2. Open file in vim and run :so % to install plugin
3. Restart vim

==============================================================================
CHANGLOG                                                        *hier-changelog*

1.3
- fix problem when disabling the highlighting by setting the
  hier_highlight_group variables to the empty string ""

1.2
- add highlighting groups for warning and info entries
- make clearing of highlighting behave more graceful towards other
  plugins
- add function s:Getlist to remove duplicated code

1.1
- add commands :HierStart and :HierStop
- add support for highlighting location list entries
- add support for highlighting pattern entries

1.0
- inital release

 vim:tw=78:ts=8:ft=help:norl:
plugin/hier.vim	[[[1
95
" hier.vim:		Highlight quickfix errors
" Last Modified: Tue 03. May 2011 10:55:27 +0900 JST
" Author:		Jan Christoph Ebersbach <jceb@e-jc.de>
" Version:		1.3

if (exists("g:loaded_hier") && g:loaded_hier) || &cp
    finish
endif
let g:loaded_hier = 1

let g:hier_enabled = ! exists('g:hier_enabled') ? 1 : g:hier_enabled

let g:hier_highlight_group_qf = ! exists('g:hier_highlight_group_qf') ? 'SpellBad' : g:hier_highlight_group_qf
let g:hier_highlight_group_qfw = ! exists('g:hier_highlight_group_qfw') ? 'SpellLocal' : g:hier_highlight_group_qfw
let g:hier_highlight_group_qfi = ! exists('g:hier_highlight_group_qfi') ? 'SpellRare' : g:hier_highlight_group_qfi

let g:hier_highlight_group_loc = ! exists('g:hier_highlight_group_loc') ? 'SpellBad' : g:hier_highlight_group_loc
let g:hier_highlight_group_locw = ! exists('g:hier_highlight_group_locw') ? 'SpellLocal' : g:hier_highlight_group_locw
let g:hier_highlight_group_loci = ! exists('g:hier_highlight_group_loci') ? 'SpellRare' : g:hier_highlight_group_loci

if eval('g:hier_highlight_group_qf') != ''
	exec "hi! link QFError    ".g:hier_highlight_group_qf
endif
if eval('g:hier_highlight_group_qfw') != ''
	exec "hi! link QFWarning  ".g:hier_highlight_group_qfw
endif
if eval('g:hier_highlight_group_qfi') != ''
	exec "hi! link QFInfo     ".g:hier_highlight_group_qfi
endif

if eval('g:hier_highlight_group_loc') != ''
	exec "hi! link LocError   ".g:hier_highlight_group_loc
endif
if eval('g:hier_highlight_group_locw') != ''
	exec "hi! link LocWarning ".g:hier_highlight_group_locw
endif
if eval('g:hier_highlight_group_loci') != ''
	exec "hi! link LocInfo    ".g:hier_highlight_group_loci
endif

function! s:Getlist(winnr, type)
	if a:type == 'qf'
		return getqflist()
	else
		return getloclist(a:winnr)
	endif
endfunction

function! s:Hier(clearonly)
	for m in getmatches()
		for h in ['QFError', 'QFWarning', 'QFInfo', 'LocError', 'LocWarning', 'LocInfo']
			if m.group == h
				call matchdelete(m.id)
			endif
		endfor
	endfor

	if g:hier_enabled == 0 || a:clearonly == 1
		return
	endif

	let bufnr = bufnr('%')

	for type in ['qf', 'loc']
		for i in s:Getlist(0, type)
			if i.bufnr == bufnr
				let hi_group = 'QFError'
				if i.type == 'I' || i.type == 'info'
					let hi_group = 'QFInfo'
				elseif i.type == 'W' || i.type == 'warning'
					let hi_group = 'QFWarning'
				elseif eval('g:hier_highlight_group_'.type) == ""
					continue
				endif

				if i.lnum > 0
					call matchadd(hi_group, '\%'.i.lnum.'l')
				elseif i.pattern != ''
					call matchadd(hi_group, i.pattern)
				endif
			endif
		endfor
	endfor
endfunction

command! -nargs=0 HierUpdate call s:Hier(0)
command! -nargs=0 HierClear call s:Hier(1)

command! -nargs=0 HierStart let g:hier_enabled = 1 | HierUpdate
command! -nargs=0 HierStop let g:hier_enabled = 0 | HierClear

augroup Hier
	au!
	au QuickFixCmdPost,BufEnter,WinEnter * :HierUpdate
augroup END
