" quiltdiff (C) 2007, Bernhard Walle <bernhard.walle@gmx.de>
"                     Heavily based on svndiff by Ico Doornekamp
"
" This program is free software; you can redistribute it and/or modify it  {{{1
" under the terms of the GNU General Public License as published by the Free
" Software Foundation; either version 2 of the License, or (at your option)
" any later version.
"
" This program is distributed in the hope that it will be useful, but WITHOUT
" ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
" FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
" more details.
"
" Introduction
" ------------
"
" NOTE: This plugin is unix-only!
"
" An small vim 7.0 plugin for showing quilt diff information in a file while
" editing. This plugin runs a diff between the current buffer and the original
" subversion file, and shows coloured signs indicating where the buffer
" differs from the original file from the subversion repository. The original
" text is not shown, only signs are used to indicate where changes were made.
"
" The following symbols and syntax highlight groups are used for the signs:
"
"   > DiffAdd:    Newly added lines. (default=blue)
"
"   ! DiffChange: Lines which are changed from the original. (default=cyan)
"
"   < DiffDel:    Applied to the lines directly above and below a deleted block
"                 (default=magenta) 
" Usage
" -----
"
" The plugin defines one function: Quiltdiff(). This function figures out the
" difference between the current buffer and it's subversion original, and adds
" the signs at the places where the buffer differs from the original file from
" subversion. You'll need to call this function after making changes to update
" the highlighting.
"
" The function takes one argument specifying an additional action to perform:
"
"   "prev"  : jump to the previous different block 
"   "next"  : jump to the next different block
"   "clear" : clean up all signs
"
" You might want to map some keys to run the Svndiff function. For
" example, add to your .vimrc:
"
"   noremap <F3> :call quiltdiff("prev")<CR> 
"   noremap <F4> :call quiltdiff("next")<CR>
"   noremap <F5> :call quiltdiff("clear")<CR>
"
"
" Configuration
" -------------
"
" The following configuration variables are availabe:
" 
" * g:quiltdiff_autoupdate
"
"   If this variable is defined, quiltdiff will automatically update the signs
"   when the user stops typing for a short while, and when leaving insert
"   mode. This might slow things down on large files, so use with caution.
"   The vim variable 'updatetime' can be used to set the auto-update intervar,
"   but not that changing this variable other effects as well. (refer to the 
"   vim docs for more info) 
"   To use, add to your .vimrc:
"
"   let g:quiltndiff_autoupdate = 1
"
" * g:quiltdiff_one_sign_delete
"
"   Normally, two 'delete' signs are placed around the location where
"   text was deleted. When this variable is defined, only one sign is
"   placed, above the location of the deleted text.
"   To use, add to your .vimrc:
"
"   let g:quiltdiff_one_sign_delete = 1
"
" Colors
" ------
"
" Personally, I find the following colours more intuitive for diff colours:
" red=deleted, green=added, yellow=changed. If you want to use these colours,
" try adding the following lines to your .vimrc
"
" hi DiffAdd      ctermfg=0 ctermbg=2 guibg='green'
" hi DiffDelete   ctermfg=0 ctermbg=1 guibg='red'
" hi DiffChange   ctermfg=0 ctermbg=3 guibg='yellow'
"
" Changelog
" ---------
"
" 1.0 2008-02-06	Initial version, split from svndiff 3.1
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" global variables                                                          {{{1

let s:sign_base = 200000  " Base for our sign id's, hoping to avoid colisions
let s:is_active = {}      " dictionary with buffer names that have svndiff active
let s:diff_signs = {}     " dict with list of ids of all signs, per file
let s:diff_blocks = {}    " dict with list of ids of first line of each diff block, per file
let s:changedtick = {}    " dict with changedticks of each buffer since last invocation

"
" Do the diff and update signs.
"

function s:Quiltdiff_update(...)                                          " {{{1

    let fname = bufname("%")

    if ! exists("s:is_active[fname]")
        return 0
	end

	" Check if this file is managed by subversion, exit otherwise
	
    let info = system("quilt files | grep '" . fname . "'")
    let patchname = substitute(system("quilt top"), "\n", "", "")
	if match(info, fname) == -1
		echom "Quiltdiff: Warning, file " . fname . " is not managed by quilt, or error running quilt."
		unlet s:is_active[fname]
		return 0
	end

	" Check if the changedticks changed since the last invocation of this
	" function. If nothing changed, there's no need to update the signs.

	if exists("s:changedtick[fname]") && s:changedtick[fname] == b:changedtick
		return 1
	end
	let s:changedtick[fname] = b:changedtick

	" The diff has changed since the last time, so we need to update the signs.
	" This is where the magic happens: pipe the current buffer contents to a
	" shell command calculating the diff in a friendly parsable format.

	let contents = join(getbufline("%", 1, "$"), "\n")
	let diff = system("diff -U0 <(cat .pc/" . patchname . "/" . fname . ") <(cat;echo)", contents)

	" clear the old signs

	call s:Quiltdiff_clear()

	" Parse the output of the diff command and put signs at changed, added and
	" removed lines

	for line in split(diff, '\n')
		
    let part = matchlist(line, '@@ -\([0-9]*\),*\([0-9]*\) +\([0-9]*\),*\([0-9]*\) @@')

		if ! empty(part)
			let old_from  = part[1]
			let old_count = part[2] == '' ? 1 : part[2]
			let new_from  = part[3]
			let new_count = part[4] == '' ? 1 : part[4]

			" Figure out if text was added, removed or changed.
			
			if old_count == 0
				let from  = new_from
				let to    = new_from + new_count - 1
				let name  = 'quiltdiff_add'
				let info  = new_count . " lines added"
			elseif new_count == 0
				let from  = new_from
				let to    = new_from 
				let name  = 'quiltdiff_delete'
				let info  = old_count . " lines deleted"
				if ! exists("g:quiltdiff_one_sign_delete")
					let to += 1
				endif
			else
				let from  = new_from
				let to    = new_from + new_count - 1
				let name  = 'quiltdiff_change'
				let info  = new_count . " lines changed"
			endif

			let id = from + s:sign_base	
			let s:diff_blocks[fname] += [{ 'id': id, 'info': info }]

			" Add signs to mark the changed lines 
			
			let line = from
			while line <= to
				let id = line + s:sign_base
				exec 'sign place ' . id . ' line=' . line . ' name=' . name . ' file=' . expand("%:p")
				let s:diff_signs[fname] += [id]
				let line = line + 1
			endwhile

		endif
	endfor

endfunction



"
" Remove all signs we placed earlier 
"

function s:Quiltdiff_clear(...)                                           " {{{1
	let fname = bufname("%")
	if exists("s:diff_signs[fname]") 
		for id in s:diff_signs[fname]
			exec 'sign unplace ' . id . ' file=' . expand("%:p")
		endfor
	end
	let s:diff_blocks[fname] = []
	let s:diff_signs[fname] = []
endfunction


"
" Jump to previous diff block sign above the current line
"

function s:Quiltdiff_prev(...)                                            " {{{1
	let fname = bufname("%")
	let diff_blocks_reversed = reverse(copy(s:diff_blocks[fname]))
	for block in diff_blocks_reversed
		let line = block.id - s:sign_base
		if line < line(".") 
			call setpos(".", [ 0, line, 1, 0 ])
			echom 'quiltdiff: ' . block.info
			return
		endif
	endfor
	echom 'quiltdiff: no more diff blocks above cursor'
endfunction


"
" Jump to next diff block sign below the current line
"

function s:Quiltdiff_next(...)                                            " {{{1
	let fname = bufname("%")
	for block in s:diff_blocks[fname]
		let line = block.id - s:sign_base
		if line > line(".") 
			call setpos(".", [ 0, line, 1, 0 ])
			echom 'quiltdiff: ' . block.info
			return
		endif
	endfor
	echom 'quiltdiff: no more diff blocks below cursor'
endfunction


"
" Wrapper function: Takes one argument, which is the action to perform:
" {next|prev|clear}
"

function Quiltdiff(...)                                                   " {{{1

    let cmd = exists("a:1") ? a:1 : ''
    let fname = bufname("%")
    if fname == ""
        echom "Buffer has no file name, can not do a svn diff"
        return
	endif

	if cmd == 'clear'
		let s:changedtick[fname] = 0
		if exists("s:is_active[fname]") 
			unlet s:is_active[fname]
		endif
		call s:Quiltdiff_clear()
	end
	
	if cmd == 'prev'
		let s:is_active[fname] = 1
		let ok = s:Quiltdiff_update()
		if ok
			call s:Quiltdiff_prev()
		endif
	endif

	if cmd == 'next'
		let s:is_active[fname] = 1
		let ok = s:Quiltdiff_update()
		if ok
			call s:Quiltdiff_next()
		endif
	endif

endfunction


" Define sign characters and colors                                         {{{1

sign define quiltdiff_add    text=> texthl=diffAdd
sign define quiltdiff_delete text=< texthl=diffDelete
sign define quiltdiff_change text=! texthl=diffChange

" add commands as shortcut                                                  {{{1
command QDiffPrev :call Quiltdiff("prev")
command QDiffNext :call Quiltdiff("next")
command QDiffClear :call Quiltdiff("clear")


" Define autocmds if autoupdate is enabled

if exists("g:quiltdiff_autoupdate")
    autocmd CursorHold,CursorHoldI * call s:Quiltdiff_update()
    if version >= 700
        autocmd InsertLeave * call s:Quiltdiff_update()
    end
endif

" vim: ts=4 sw=4 et tw=0 fdm=marker:
