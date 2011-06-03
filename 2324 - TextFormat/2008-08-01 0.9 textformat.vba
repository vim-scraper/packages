" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/textformat.txt	[[[1
236
*textformat.txt*	Vim Text Formatter  (version 0.9)	    2008-08-01


Description	This plugin provides commands and key mappings to quickly
		align and format text. Text can be aligned to either left or
		right margin or justified to both margins or centered. The
		text formatting commands in this plugin are a bit different
		from those integrated to Vim.

Author		Teemu Likonen <tlikonen@iki.fi>




Contents

	1. Quick start				|textformat-start|
	2. Commands				|textformat-commands|
	3. Default key mappings			|textformat-keymap-default|
	4. User key mappings			|textformat-keymap-user|

==============================================================================
1. Quick start						    *textformat-start*

The impatient ones are always with us so here is the list of (probably) the
most commonly used key mappings for formatting text:

	<Leader>al	Reformat current paragraph and align lines to the
			left.
	<Leader>ar	Align lines of current paragraph to the right.
	<Leader>aj	Reformat current paragraph and justify it to both
			margins. The last line in the paragraph is aligned to
			the left.
	<Leader>ac	Center lines of current paragraph.

By default, <Leader> is the backslash key (\) so by default the mappings are
actually \al, \ar, \aj and \ac. If you have changed the g:mapleader variable
in your .vimrc file <Leader> may be something else.

==============================================================================
2. Commands						 *textformat-commands*

Let's start with the basic components of this plugin. These are the ex
commands. You probably don't need these very often but they can be handy if
you want to have text formatting and aligning as a part of a macro or function
or something. The "daily tools" are explained later.

:[range]AlignLeft [indent]					  *:AlignLeft*
			Align to left all the lines in [range] (default is
			current line) and truncate all extra whitespace
			characters. That is, if there are more than one space
			between words they are reduced to just one. If
			'joinspaces' is set then two spaces are inserted after
			every sentence ending with character ".", "?" or "!".

			If optional numeric argument [indent] is given then
			that is used as the left margin. If [indent] is not
			given the indent of the first line in the [range] is
			used to define indent for the rest of the lines. There
			is one exception: if 'formatoptions' contains "2" then
			the second line in the [range] defines the indent for
			the rest of the lines.

			Note: This is very similar to |:left| command except
			that this also truncates whitespaces and that without
			[indent] the first line's indent is used.

			Note: There is a possible unexpected behaviour: If
			command is run without [range] (i.e., it's just the
			current line) and [indent] is not given then this
			command just "aligns" to the current indent position
			and truncates whitespaces. You might see nothing
			happening if there weren't any extra whitespaces. Use
			[indent] (or |:left| command) to align to desired
			column.

:[range]AlignRight [width]					 *:AlignRight*
			Align to right all the lines in [range] (default is
			current line) and truncate all extra whitespace
			characters (honor 'joinspaces', as in :AlignLeft).
			[width] is used as the right margin. If [width] is not
			given 'textwidth' option is used instead. If
			'textwidth' is zero then the value of 80 is used.

			Note: This is very similar to |:right| command except
			that this also truncates whitespaces.

:[range]AlignJustify [width]				       *:AlignJustify*
			Left-right justify lines in [range] (default is
			current line). This means adjusting spaces between
			words so that the lines fit. Unlike the previous
			commands this does not use 'joinspaces' option.
			Instead, extra spaces are always first added after
			sentences ending with ".", "?" or "!". Then to other
			available positions.

			The first line in the [range] defines the indent for
			the rest of the lines, except if 'formatoptions'
			contains "2" then it's the second line.

			Numeric argument [width] is used as the right margin.
			If [width] is not given 'textwidth' is used instead.
			If 'textwidth' is zero then the value of 80 is used.

			Also see the Discussion below.

:[range]AlignCenter [width]					*:AlignCenter*
			Center lines in [range] (default is current line)
			horizontally between the first column and [width]. All
			extra whitespace characters are truncated but honor
			'joinspaces', just like in :AlignLeft. If [width] is
			not given 'textwidth' option is used instead. If
			'textwidth' is zero the value of 80 is used.

			Note: This is very similar to |:center| except that
			this also truncates whitespaces.

Discussion ~

All previous commands operate on single lines only. They do not wrap lines nor
do other kind of formatting. If [width] (or 'textwidth') is too narrow for the
line then some characters will go beyond the right margin. This is similar to
Vim's own |:left|, |:right| and |:center| commands. You might first want to
adjust line lengths with other editing commands and then run one of the
previous text-formatting commands.

Usually when paragraphs are justified the last line of paragraph is aligned to
left. However, :AlignJustify command (see above) does not do this. The purpose
of this command is to do exactly what was asked for: left-right justify
a range of lines. The general-purpose justification tools is <Leader>aj which
reformats the paragraph (like |gw|), justifies lines and aligns the last line
to left.

Why do these commands truncate whitespaces? Well, this is a text-formatting
tool and in plain text paragraphs any extra spaces are considered
a "typographical flaw." These formatting commands can be used to fix
whitespace errors in plain text. Another reason is that left-right justify
commands need to add extra spaces to make lines fit. If you later want to
reformat such previously justified paragraph and align it to left, for
example, it's convenient that the tool automatically handles this and removes
extra spaces. If you want to align text without truncating whitespaces use
Vim's own align commands: |:left|, |:right| and |:center|.

==============================================================================
3. Default key mappings				   *textformat-keymap-default*

By default this plugin provides a couple of key mappings for convenient text
formatting. If some of the mappings have already been defined by user (or are
taken by some other plugin) then some of the following mappings may not be
automatically available. See the next section of this manual for information
on how to change the default mappings.

There are key mappings available for normal mode and visual mode. As usual,
<Leader> is the backslash key by default but it can be changed with
g:mapleader variable. Consult the Vim manual for more information on <Leader>.

Normal mode (current paragraph) ~

<Leader>al		Left-align current paragraph, truncate all whitespace
			characters (but honor 'joinspaces', as in :AlignLeft)
			and reformat the paragraph so that it fits to
			'textwidth'. The first line in the paragraph defines
			the indent for the rest of the lines, except if
			'formatoptions' contains "2" then it's the second
			line.

<Leader>ar		Right-align current paragraph (use 'textwidth') and
			truncate all whitespace characters (but honor
			'joinspaces', as in :AlignLeft). This does not
			reformat lines because with right-aligned text user
			usually wants to decide exactly what goes to what
			line.

<Leader>aj		Left-right justify. Truncate all whitespace characters
			(but honor 'joinspaces', as in :AlignLeft), reformat
			the paragraph so that it fits to 'textwidth' and
			finally left-right justify lines. Extra spaces may be
			added between words so that lines fill the text area.
			Paragraph's last line is aligned to left. The first
			line in the paragraph defines the indent for the rest
			of the lines, except if 'formatoptions' contains "2"
			then it's the second line.

<Leader>ac		Center lines of current paragraph horizontally between
			the first column and 'textwidth'. All whitespace
			characters are truncated, except if 'joinspaces' is
			set then an extra space is added after full sentences
			(see :AlignLeft). This does not reformat the
			paragraph. With centered text user usually wants to
			decide exactly what goes to what line. Reformatting
			would destroy it.

Visual mode (range of lines) ~

{Visual}<Leader>al	Left-align {Visual} lines.
{Visual}<Leader>ar	Right-align {Visual} lines.
{Visual}<Leader>aj	Left-right justify {Visual} lines.
{Visual}<Leader>ac	Center {Visual} lines.

Visual mode mappings do not wrap lines nor otherwise reformat text. They work
line-by-line, just like their command line equivalents: :AlignLeft,
:AlignRight, :AlignJustify and :AlignCenter.

==============================================================================
4. User key mappings				      *textformat-keymap-user*

The key mappings can be configured freely by user. This plugin uses the
default ones only if they are free and not mapped to some other things. We try
to be non-intrusive because this is a general-purpose tool which is likely
loaded automatically when Vim starts.

Here's an example of lines you could put in your .vimrc file: >

	nmap <F5> <Plug>Quick_Align_Paragraph_Left
	nmap <F6> <Plug>Quick_Align_Paragraph_Right
	nmap <F7> <Plug>Quick_Align_Paragraph_Justify
	nmap <F8> <Plug>Quick_Align_Paragraph_Center

	vmap <F5> <Plug>Align_Range_Left
	vmap <F6> <Plug>Align_Range_Right
	vmap <F7> <Plug>Align_Range_Justify
	vmap <F8> <Plug>Align_Range_Center

That is, |:nmap| command defines mappings for normal mode and |:vmap| for
visual mode. Function keys from <F5> to <F8> are used in this example. The
rest of the line is a code word for this plugin; they tell Vim what to do.
I think the code words are pretty much self-descriptive.

Don't use |:nnoremap| and |:vnoremap| commands here; they don't work because
the right-hand side (<Plug>...) must be remappable. See Vim's manual for more
information about mapping commands.

Happy formatting!

==============================================================================
vim: ft=help tw=78 ts=8 norl
plugin/textformat.vim	[[[1
68
" Text formatter plugin for Vim text editor
"
" This plugin provides commands and key mappings to quickly align and format
" text. Text can be aligned to either left or right margin or justified to
" both margins or centered. The text formatting commands in this plugin are
" a bit different from those integrated to Vim.
"
" Version:    0.9
" Maintainer: Teemu Likonen <tlikonen@iki.fi>
" GetLatestVimScripts: 0 0 :AutoInstall: textformat.vim
"
" {{{ Copyright and license
"
" Copyright (C) 2008 Teemu Likonen <tlikonen@iki.fi>
"
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
"
" The License text in full:
" 	http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
"
" }}}

command! -nargs=? -range AlignLeft <line1>,<line2>call textformat#Align_Command('left',<args>)
command! -nargs=? -range AlignRight <line1>,<line2>call textformat#Align_Command('right',<args>)
command! -nargs=? -range AlignJustify <line1>,<line2>call textformat#Align_Command('justify',<args>)
command! -nargs=? -range AlignCenter <line1>,<line2>call textformat#Align_Command('center',<args>)

nnoremap <silent> <Plug>Quick_Align_Paragraph_Left :call textformat#Quick_Align_Left()<CR>
nnoremap <silent> <Plug>Quick_Align_Paragraph_Right :call textformat#Quick_Align_Right()<CR>
nnoremap <silent> <Plug>Quick_Align_Paragraph_Justify :call textformat#Quick_Align_Justify()<CR>
nnoremap <silent> <Plug>Quick_Align_Paragraph_Center :call textformat#Quick_Align_Center()<CR>

vnoremap <silent> <Plug>Align_Range_Left :call textformat#Align_Command('left')<CR>
vnoremap <silent> <Plug>Align_Range_Right :call textformat#Align_Command('right')<CR>
vnoremap <silent> <Plug>Align_Range_Justify :call textformat#Align_Command('justify')<CR>
vnoremap <silent> <Plug>Align_Range_Center :call textformat#Align_Command('center')<CR>

function! s:Add_Mapping(mode, lhs, rhs)
	if maparg(a:lhs, a:mode) == '' && !hasmapto(a:rhs, a:mode)
		execute a:mode.'map '.a:lhs.' '.a:rhs
	endif
endfunction

call s:Add_Mapping('n', '<Leader>al', '<Plug>Quick_Align_Paragraph_Left')
call s:Add_Mapping('n', '<Leader>ar', '<Plug>Quick_Align_Paragraph_Right')
call s:Add_Mapping('n', '<Leader>aj', '<Plug>Quick_Align_Paragraph_Justify')
call s:Add_Mapping('n', '<Leader>ac', '<Plug>Quick_Align_Paragraph_Center')

call s:Add_Mapping('v', '<Leader>al', '<Plug>Align_Range_Left')
call s:Add_Mapping('v', '<Leader>ar', '<Plug>Align_Range_Right')
call s:Add_Mapping('v', '<Leader>aj', '<Plug>Align_Range_Justify')
call s:Add_Mapping('v', '<Leader>ac', '<Plug>Align_Range_Center')

delfunction s:Add_Mapping

" vim600: fdm=marker
autoload/textformat.vim	[[[1
453
" Text formatter plugin for Vim text editor
"
" This plugin provides commands and key mappings to quickly align and format
" text. Text can be aligned to either left or right margin or justified to
" both margins or centered. The text formatting commands in this plugin are
" a bit different from those integrated to Vim.
"
" Version:    0.9
" Maintainer: Teemu Likonen <tlikonen@iki.fi>
" GetLatestVimScripts: 0 0 :AutoInstall: textformat.vim
"
" {{{ Copyright and license
"
" Copyright (C) 2008 Teemu Likonen <tlikonen@iki.fi>
"
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
"
" The License text in full:
" 	http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
"
" }}}

" Constant variables(s) {{{1
let s:default_width = 80

function! s:Align_Range_Left(...) range "{{{1
	if a:0 > 0 && a:1 >= 0
		" If [indent] is given align the first line accordingly
		let l:start_ws = repeat(' ',a:1)
		let l:line_replace = s:Align_String_Left(getline(a:firstline))
		call setline(a:firstline,l:start_ws.l:line_replace)
	else
		" If [indent] is not given get the indent of the first line
		" (and possibly the second too in case fo+=2).
		let l:start_ws = substitute(getline(a:firstline),'\m\S.*','','')
		let l:line_replace = s:Align_String_Left(getline(a:firstline))
		call setline(a:firstline,l:start_ws.l:line_replace)
		if match(&formatoptions,'2') >= 0 && a:lastline > a:firstline
			let l:start_ws = substitute(getline(a:firstline+1),'\m\S.*','','')
		endif
	endif
	" Align the rest of the lines
	for l:i in range(a:lastline-a:firstline)
		let l:line = a:firstline + 1 + l:i
		let l:line_replace = s:Align_String_Left(getline(l:line))
		call setline(l:line,l:start_ws.l:line_replace)
	endfor
endfunction

function! s:Align_Range_Right(width) "{{{1
	let l:line_replace = s:Align_String_Right(getline('.'),a:width)
	if l:line_replace =~ '\v^ *$'
		" If line would be full of spaces just print empty line.
		call setline(line('.'),'')
	else
		call setline(line('.'),l:line_replace)
	endif
endfunction

function! s:Align_Range_Justify(width, ...) range "{{{1
	" Get the indent of the first line.
	let l:start_ws = substitute(getline(a:firstline),'\m\S.*','','')
	" 'textwidth' minus indent to get the actual text area width
	normal! ^
	let l:width = a:width-virtcol('.')+1
	let l:line_replace = substitute(l:start_ws.s:Align_String_Justify(getline(a:firstline),l:width),'\m\s*$','','')
	call setline(a:firstline,l:line_replace)
	" If fo+=2 and range is more than one line get the indent of the
	" second line.
	if match(&formatoptions,'2') >= 0 && a:lastline > a:firstline
		let l:start_ws = substitute(getline(a:firstline+1),'\m\S.*','','')
		execute a:firstline+1
		normal! ^
		let l:width = a:width-virtcol('.')+1
	endif
	" Justify all the lines in range
	for l:i in range(a:lastline-a:firstline)
		let l:line = a:firstline + 1 + l:i
		if l:line == a:lastline && a:0
			" Align the last line to left
			call setline(l:line,l:start_ws.s:Truncate_Spaces(getline(l:line)))
		else
			" Other lines left-right justified
			let l:line_replace = substitute(l:start_ws.s:Align_String_Justify(getline(l:line),l:width),'\m\s*$','','')
			call setline(l:line,l:line_replace)
		endif
	endfor
endfunction

function! s:Align_Range_Center(width) "{{{1
	let l:line_replace = s:Truncate_Spaces(getline('.'))
	let l:line_replace = s:Add_Double_Spacing(l:line_replace)
	call setline(line('.'),l:line_replace)
	execute 'center '.a:width
endfunction

function! s:Align_String_Left(string, ...) "{{{1
	let l:string_replace = s:Truncate_Spaces(a:string)
	let l:string_replace = s:Add_Double_Spacing(l:string_replace)
	if a:0 && a:1
		" If optional width argument is given (and is non-zero) we pad
		" the rest of string with spaces. Currently this code path is
		" never needed.
		let l:string_width = s:String_Width(l:string_replace)
		let l:more_spaces = a:1-l:string_width
		return l:string_replace.repeat(' ',l:more_spaces)
	else
		return l:string_replace
	endif
endfunction

function! s:Align_String_Right(string, width) "{{{1
	let l:string_replace = s:Truncate_Spaces(a:string)
	let l:string_replace = s:Add_Double_Spacing(l:string_replace)
	let l:string_width = s:String_Width(l:string_replace)
	let l:more_spaces = a:width-l:string_width
	return repeat(' ',l:more_spaces).l:string_replace
endfunction

function! s:Align_String_Justify(string, width) "{{{1
	let l:string = s:Truncate_Spaces(a:string)
	" If the parameter string is empty we can just return a line full of
	" spaces. No need to go further.
	if l:string =~ '\v^ *$'
		return repeat(' ',a:width)
	endif
	let l:string_width = s:String_Width(l:string)
	if l:string_width >= a:width
		" The original string is longer than width so we can just
		" return the string. No need to go further.
		return l:string
	endif

	" This many extra spaces we need.
	let l:more_spaces = a:width-l:string_width
	" Convert the string to a list of words.
	let l:word_list = split(l:string)
	" This is the amount of spaces available in the original string (word
	" count minus one).
	let l:string_spaces = len(l:word_list)-1
	" If there are no spaces there is only one word. We can just return
	" the string with padded spaces. No need to go further.
	if l:string_spaces == 0
		return l:string.repeat(' ',l:more_spaces)
	endif
	" Ok, there are more than one word in the string so we get to do some
	" real work...

	" Make a list which each item represent a space available in the
	" string. The value means how many spaces there are. At the moment set
	" every list item to one: [1, 1, 1, 1, ...]
	let l:space_list = []
	for l:item in range(l:string_spaces)
		let l:space_list += [1]
	endfor

	" Repeat while there are no more need to add any spaces.
	while l:more_spaces > 0
		if l:more_spaces >= l:string_spaces
			" More extra spaces are needed than there are spaces
			" available in the string so we add one more space to
			" after every word (add 1 to items of space list).
			for l:i in range(l:string_spaces)
				let l:space_list[l:i] += 1
			endfor
			let l:more_spaces -= l:string_spaces
			" And then another round... and a check if more spaces
			" are needed.
		else " l:more_spaces < l:string_spaces
			" This list tells where .?! characters are.
			let l:space_sentence_full = []
			" This list tells where ,:; characters are.
			let l:space_sentence_semi = []
			" And this is for the rest of spaces.
			let l:space_other = []
			" Now, find those things:
			for l:i in range(l:string_spaces)
				if match(l:word_list[l:i],'\m\S[.?!]$') >= 0
					let l:space_sentence_full += [l:i]
				elseif match(l:word_list[l:i],'\m\S[,:;]$') >= 0
					let l:space_sentence_semi += [l:i]
				else
					let l:space_other += [l:i]
				endif
			endfor

			" First distribute spaces after .?!
			if l:more_spaces >= len(l:space_sentence_full)
				" If we need more extra spaces than there are
				" .?! spaces, just add one after every item.
				for l:i in l:space_sentence_full
					let l:space_list[l:i] += 1
				endfor
				let l:more_spaces -= len(l:space_sentence_full)
				if l:more_spaces == 0 | break | endif
			else
				" Distribute the rest of spaces evenly and
				" break the loop. All the spaces are added.
				for l:i in s:Distributed_Selection(l:space_sentence_full,l:more_spaces)
					let l:space_list[l:i] +=1
				endfor
				break
			endif

			" Then distribute spaces after ,:;
			if l:more_spaces >= len(l:space_sentence_semi)
				" If we need more extra spaces than there are
				" ,:; spaces available, just add one after
				" every item.
				for l:i in l:space_sentence_semi
					let l:space_list[l:i] += 1
				endfor
				let l:more_spaces -= len(l:space_sentence_semi)
				if l:more_spaces == 0 | break | endif
			else
				" Distribute the rest of spaces evenly and
				" break the loop. All the spaces are added.
				for l:i in s:Distributed_Selection(l:space_sentence_semi,l:more_spaces)
					let l:space_list[l:i] +=1
				endfor
				break
			endif

			" Finally distribute spaces to other available
			" positions and exit the loop.
			for l:i in s:Distributed_Selection(l:space_other,l:more_spaces)
				let l:space_list[l:i] +=1
			endfor
			break
		endif
	endwhile

	" Now we now where all the extra spaces will go. We have to construct
	" the string again.
	let l:string = ''
	for l:item in range(l:string_spaces)
		let l:string .= l:word_list[l:item].repeat(' ',l:space_list[l:item])
	endfor
	" Add the last word to the and and return the string.
	return l:string.l:word_list[-1]
endfunction

function! s:Truncate_Spaces(string) "{{{1
	let l:string = substitute(a:string,'\v\s+',' ','g')
	let l:string = substitute(l:string,'\m^\s*','','')
	let l:string = substitute(l:string,'\m\s*$','','')
	return l:string
endfunction

function! s:String_Width(string) "{{{1
	" This counts the string width in characters. Combining diacritical
	" marks do not count so the base character with all the combined
	" diacritics is just one character (which is good for our purposes).
	" Double-wide characters will not get double width so unfortunately
	" they don't work in our algorithm.
	return strlen(substitute(a:string,'\m.','x','g'))
endfunction

function! s:Add_Double_Spacing(string) "{{{1
	if &joinspaces
		return substitute(a:string,'\m\S[.?!] ','& ','g')
	else
		return a:string
	endif
endfunction

function! s:Distributed_Selection(list, pick) "{{{1
	" 'list' is a list-type variable [ item1, item2, ... ]
	" 'pick' is a number how many of the list's items we want to choose
	"
	" This function returns a list which has 'pick' number of items from
	" the original list. Items are choosed in distributed manner. For
	" example, if 'pick' is 1 then the algorithm chooses an item near the
	" center of the 'list'. If 'pick' is 2 then the first one is about 1/3
	" from the begining and the another one about 2/3 from the begining.

	" l:pick_list is a list of 0's and 1's and its length will be the
	" same as original list's. Number 1 means that this list item will be
	" picked and 0 means that the item will be dropped. Finally
	" l:pick_list could look like this: [0, 1, 0, 1, 0]
	" (i.e., two items evenly picked from a list of five items)
	let l:pick_list = []

	" First pick items evenly from the begining of the list. This also
	" actually constructs the list.
	let l:div1 = len(a:list) / a:pick
	let l:mod1 = len(a:list) % a:pick
	for l:i in range(len(a:list)-l:mod1)
		if !eval(l:i%l:div1)
			let l:pick_list += [1]
		else
			let l:pick_list += [0]
		endif
	endfor

	if l:mod1 > 0
		" The division wasn't even so we get the remaining items and
		" distribute them evenly again to the list.
		let l:div2 = len(l:pick_list) / l:mod1
		let l:mod2 = len(l:pick_list) % l:mod1
		for l:i in range(len(l:pick_list)-l:mod2)
			if !eval(l:i%l:div2)
				call insert(l:pick_list,0,l:i)
			endif
		endfor
	endif

	" There may be very different number of zeros in the begining and end
	" of the list. We count them.
	let l:zeros_begin = 0
	for l:i in l:pick_list
		if l:i == 0
			let l:zeros_begin += 1
		else
			break
		endif
	endfor
	let l:zeros_end = 0
	for l:i in reverse(copy(l:pick_list))
		if l:i == 0
			let l:zeros_end += 1
		else
			break
		endif
	endfor

	" Then we remove them.
	if l:zeros_end
		" Remove 0 items from the end. We need to remove them first
		" from the end because list items' index number will change
		" when items are removed from the begining. Then it would make
		" a bit more difficult to remove ending spaces.
		call remove(l:pick_list,len(l:pick_list)-l:zeros_end,-1)
	endif
	if l:zeros_begin
		" Remove 0 items from the begining.
		call remove(l:pick_list,0,l:zeros_begin-1)
	endif
	let l:zeros_both = l:zeros_begin + l:zeros_end

	" Put even amount of zeros to begining and end
	for l:i in range(l:zeros_both/2)
		call insert(l:pick_list,0,0)
	endfor
	for l:i in range((l:zeros_both/2)+(l:zeros_both%2))
		call add(l:pick_list,0)
	endfor

	" Finally construct and return a new list which has only the items we
	" have chosen.
	let l:new_list = []
	for l:i in range(len(l:pick_list))
		if l:pick_list[l:i] == 1
			let l:new_list += [a:list[l:i]]
		endif
	endfor
	return l:new_list
endfunction

function! textformat#Quick_Align_Left() "{{{1
	let l:pos = getpos('.')
	let l:autoindent = &autoindent
	let l:formatoptions = &formatoptions
	setlocal autoindent formatoptions-=w
	silent normal! vip:call s:Align_Range_Left()
	silent normal! gwip
	call setpos('.',l:pos)
	let &l:formatoptions = l:formatoptions
	let &l:autoindent = l:autoindent
endfunction

function! textformat#Quick_Align_Right() "{{{1
	let l:width = &textwidth
	if l:width == 0 | let l:width = s:default_width | endif
	let l:pos = getpos('.')
	silent normal! vip:call s:Align_Range_Right(l:width)
	call setpos('.',l:pos)
endfunction

function! textformat#Quick_Align_Justify() "{{{1
	let l:width = &textwidth
	if l:width == 0 | let l:width = s:default_width  | endif
	let l:pos = getpos('.')
	let l:joinspaces = &joinspaces
	setlocal nojoinspaces
	call textformat#Quick_Align_Left()
	let &l:joinspaces = l:joinspaces
	silent normal! vip:call s:Align_Range_Justify(l:width,1)
	call setpos('.',l:pos)
endfunction

function! textformat#Quick_Align_Center() "{{{1
	let l:width = &textwidth
	" It's not good idea to use tabs in text area which has very different
	" number of spaces before the lines. Plain spaces are more reliable so
	" we set 'expandtab' variable before the operation.
	let l:expandtab = &expandtab
	setlocal expandtab
	if l:width == 0 | let l:width = s:default_width  | endif
	let l:pos = getpos('.')
	silent normal! vip:call s:Align_Range_Center(l:width)
	call setpos('.',l:pos)
	let &l:expandtab = l:expandtab
endfunction

function! textformat#Align_Command(align, ...) range "{{{1
	" For the left align the optional parameter a:1 is [indent]. For
	" others it's [width].
	if a:align == 'left'
		if a:0 && a:1 >= 0
			execute a:firstline.','.a:lastline.'call s:Align_Range_Left('.a:1.')'
		else
			execute a:firstline.','.a:lastline.'call s:Align_Range_Left()'
		endif
	else
		if a:0 && a:1 > 0
			let l:width = a:1
		elseif &textwidth
			let l:width = &textwidth
		else
			let l:width = s:default_width
		endif

		if a:align == 'right'
			execute a:firstline.','.a:lastline.'call s:Align_Range_Right('.l:width.')'
		elseif a:align == 'justify'
			execute a:firstline.','.a:lastline.'call s:Align_Range_Justify('.l:width.')'
		elseif a:align == 'center'
			" It's not good idea to use tabs in text area which
			" has very different number of spaces before the
			" lines. Plain spaces are more reliable so we set
			" 'expandtab' variable before the operation.
			let l:expandtab = &expandtab
			setlocal expandtab
			execute a:firstline.','.a:lastline.'call s:Align_Range_Center('.l:width.')'
			let &l:expandtab = l:expandtab
		endif
	endif
endfunction

" vim600: fdm=marker
