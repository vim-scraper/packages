" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/textformat.vim	[[[1
610
" Text formatter plugin for Vim text editor
"
" Version:              2.1
" Last Change:          2008-09-13
" Maintainer:           Teemu Likonen <tlikonen@iki.fi>
" License:              This file is placed in the public domain.
" GetLatestVimScripts:  2324 1 :AutoInstall: TextFormat

"{{{1 The beginning stuff
if &compatible
	finish
endif
let s:save_cpo = &cpo
set cpo&vim

" Constant variables(s) {{{1
let s:default_width = 80

function! s:Align_Range_Left(...) range "{{{1
	" The optional parameter is the left indent. If it is not given we
	" detect the indent used in the buffer.
	if a:0 && a:1 >= 0
		" The parameter was given so we just use that as the left
		" indent.
		let l:leading_ws = s:Retab_Indent(a:1)
		for l:line in range(a:firstline,a:lastline)
			let l:line_string = getline(l:line)
			let l:line_replace = s:Align_String_Left(l:line_string)
			if &formatoptions =~ 'w' && l:line_string =~ '\m\s$'
				" Preserve trailing whitespace because fo=~w
				let l:line_replace .= ' '
			endif
			if l:line_replace =~ '\m^\s*$'
				call setline(l:line,'')
			else
				call setline(l:line,l:leading_ws.l:line_replace)
			endif
		endfor
	else
		" The parameter was not given so we detect each paragraph's
		" indent.
		let l:line = a:firstline
		while l:line <= a:lastline
			let l:line_string = getline(l:line)
			if l:line_string =~ '\m^\s*$'
				" The line is empty or contains only
				" whitespaces so print empty line and
				" continue.
				call setline(l:line,'')
				let l:line += 1
				continue
			endif

			" Paragraph (or the whole line range) begins here so
			" get the indent of the first line and print the line.
			let l:leading_ws = s:Retab_Indent(indent(l:line))
			let l:line_replace = s:Align_String_Left(l:line_string)
			if &formatoptions =~ 'w' && l:line_string =~ '\m\s$'
				let l:line_replace .= ' '
			endif
			call setline(l:line,l:leading_ws.l:line_replace)
			let l:line += 1

			" If fo=~w, does the paragraph end here? If yes,
			" continue to next round and find a new first line.
			if &formatoptions =~ 'w' && l:line_string =~ '\m\S$'
				continue
			endif

			" If fo=~2 get the indent of the second line
			if &formatoptions =~ '2'
				let l:leading_ws = s:Retab_Indent(indent(l:line))
			endif

			" This loop will go through all the lines in the
			" paragraph (or till the a:lastline) - starting from
			" the second line.
			while l:line <= a:lastline && getline(l:line) !~ '\m^\s*$'
				let l:line_string = getline(l:line)
				let l:line_replace = s:Align_String_Left(l:line_string)
				if &formatoptions =~ 'w'
					if l:line_string =~ '\m\s$'
						call setline(l:line,l:leading_ws.l:line_replace.' ')
						let l:line += 1
						continue
					else
						call setline(l:line,l:leading_ws.l:line_replace)
						let l:line += 1
						" fo=~w and paragraph ends
						" here so we break the loop.
						" The next line is new first
						" line.
						break
					endif
				else
					call setline(l:line,l:leading_ws.l:line_replace)
					let l:line += 1
				endif
			endwhile
		endwhile
	endif
endfunction

function! s:Align_Range_Right(width) "{{{1
	let l:line_replace = s:Align_String_Right(getline('.'),a:width)
	if &formatoptions =~ 'w' && getline('.') =~ '\m\s$'
		let l:line_replace .= ' '
	endif
	if l:line_replace =~ '\m^\s*$'
		" If line would be full of spaces just print empty line.
		call setline(line('.'),'')
	else
		" Retab leading whitespaces
		let l:leading_ws = s:Retab_Indent(strlen(substitute(l:line_replace,'\v^( *).*$','\1','')))
		" Get the rest of the line
		let l:line_replace = substitute(l:line_replace,'^ *','','')
		call setline(line('.'),l:leading_ws.l:line_replace)
	endif
endfunction

function! s:Align_Range_Justify(width, ...) range "{{{1
	" If the optional second argument is given (and is non-zero) each
	" paragraph's last line and range's last line is left-aligned.
	if a:0 && a:1
		let l:paragraph = 1
	else
		let l:paragraph = 0
	endif
	let l:line = a:firstline
	while l:line <= a:lastline
		let l:line_string = getline(l:line)
		if l:line_string =~ '\m^\s*$'
			" The line is empty or contains only
			" whitespaces so print empty line and
			" continue.
			call setline(l:line,'')
			let l:line += 1
			continue
		endif

		" Paragraph (or the whole line range) begins here so
		" get the indent of the first line and print the line.
		let l:indent = indent(l:line)
		let l:width = a:width - l:indent
		let l:leading_ws = s:Retab_Indent(l:indent)

		if l:paragraph && (l:line == a:lastline || getline(l:line+1) =~ '\m^\s*$' || (&formatoptions =~ 'w' && l:line_string =~ '\m\S$'))
			let l:line_replace = s:Align_String_Left(l:line_string)
		else
			let l:line_replace = s:Align_String_Justify(l:line_string,l:width)
		endif
		if &formatoptions =~ 'w' && l:line_string =~ '\m\s$'
			let l:line_replace .= ' '
		endif
		call setline(l:line,l:leading_ws.l:line_replace)
		let l:line += 1

		" If fo=~w, does the paragraph end here? If yes,
		" continue to next round and find a new first line.
		if &formatoptions =~ 'w' && l:line_string =~ '\m\S$'
			continue
		endif

		" If fo=~2 get the indent of the second line
		if &formatoptions =~ '2'
			let l:indent = indent(l:line)
			let l:width = a:width - l:indent
			let l:leading_ws = s:Retab_Indent(l:indent)
		endif

		" This loop will go through all the lines in the
		" paragraph (or till the a:lastline) - starting from
		" paragraph's second line.
		while l:line <= a:lastline && getline(l:line) !~ '\m^\s*$'
			let l:line_string = getline(l:line)
			if l:paragraph && (l:line == a:lastline || getline(l:line+1) =~ '\m^\s*$' || (&formatoptions =~ 'w' && l:line_string =~ '\m\S$'))
				let l:line_replace = s:Align_String_Left(l:line_string)
			else
				let l:line_replace = s:Align_String_Justify(l:line_string,l:width)
			endif
			if &formatoptions =~ 'w'
				if l:line_string =~ '\m\s$'
					call setline(l:line,l:leading_ws.l:line_replace.' ')
					let l:line += 1
					continue
				else
					call setline(l:line,l:leading_ws.l:line_replace)
					let l:line += 1
					" fo=~w and paragraph ends
					" here so we break the loop.
					" The next line is new first
					" line.
					break
				endif
			else
				call setline(l:line,l:leading_ws.l:line_replace)
				let l:line += 1
			endif
		endwhile
	endwhile
endfunction

function! s:Align_Range_Center(width) "{{{1
	let l:line_replace = s:Truncate_Spaces(getline('.'))
	let l:line_replace = s:Add_Double_Spacing(l:line_replace)
	if &formatoptions =~ 'w' && getline('.') =~ '\m\s$'
		let l:line_replace .= ' '
	endif
	call setline(line('.'),l:line_replace)
	execute '.center '.a:width
endfunction

function! s:Align_String_Left(string) "{{{1
	let l:string_replace = s:Truncate_Spaces(a:string)
	let l:string_replace = s:Add_Double_Spacing(l:string_replace)
	return l:string_replace
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
	if l:string =~ '\m^ *$'
		return repeat(' ',a:width)
	endif
	if s:String_Width(s:Add_Double_Spacing(l:string)) >= a:width
		" The original string is longer than width so we can just
		" return the string. No need to go further.
		return s:Add_Double_Spacing(l:string)
	endif
	let l:string_width = s:String_Width(l:string)

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

	" Make a list of which each item represent a space available in the
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
			" available in the string so we add one more space 
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
				if l:word_list[l:i] =~ '\m\S[.?!]$'
					let l:space_sentence_full += [l:i]
				elseif l:word_list[l:i] =~ '\m\S[,:;]$'
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
				" break the loop. All the spaces have been
				" added.
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
				" break the loop. All the spaces have been
				" added.
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
	" Add the last word to the end and return the string.
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
	" the original list. Items are chosen in distributed manner. For
	" example, if 'pick' is 1 then the algorithm chooses an item near the
	" center of the 'list'. If 'pick' is 2 then the first one is about 1/3
	" from the beginning and the another one about 2/3 from the beginning.

	" l:pick_list is a list of 0's and 1's and its length will be the
	" same as original list's. Number 1 means that this list item will be
	" picked and 0 means that the item will be dropped. Finally
	" l:pick_list could look like this: [0, 1, 0, 1, 0]
	" (i.e., two items evenly picked from a list of five items)
	let l:pick_list = []

	" First pick items evenly from the beginning of the list. This also
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

	" There may be very different number of zeros in the beginning and the
	" end of the list. We count them.
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
		" Remove "0" items from the end. We need to remove them first
		" from the end because list items' index number will change
		" when items are removed from the beginning. Then it would be
		" more difficult to remove trailing zeros.
		call remove(l:pick_list,len(l:pick_list)-l:zeros_end,-1)
	endif
	if l:zeros_begin
		" Remove zero items from the beginning.
		call remove(l:pick_list,0,l:zeros_begin-1)
	endif
	let l:zeros_both = l:zeros_begin + l:zeros_end

	" Put even amount of zeros to beginning and end
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

function! s:Retab_Indent(column) "{{{1
	" column = the left indent column starting from 0 Function returns
	" a string of whitespaces, a mixture of tabs and spaces depending on
	" the 'expandtab' and 'tabstop' options.
	if &expandtab
		" Only spaces
		return repeat(' ',a:column)
	else
		" Tabs and spaces
		let l:tabs = a:column / &tabstop
		let l:spaces = a:column % &tabstop
		return repeat("\<Tab>",l:tabs).repeat(' ',l:spaces)
	endif
endfunction

function! s:Reformat_Range(...) range "{{{1
	if a:0 == 2
		let l:first = a:1
		let l:last = a:2
	else
		let l:first = a:firstline
		let l:last = a:lastline
	endif
	let l:autoindent = &autoindent
	setlocal autoindent
	execute l:first
	normal! 0
	execute 'normal! V'.l:last.'G$gw'
	let &l:autoindent = l:autoindent
	" The formatting may change the last line of the range so we return
	" it.
	return line("'>")
endfunction

function! textformat#Visual_Align_Left() range "{{{1
	execute a:firstline.','.a:lastline.'call s:Align_Range_Left()'
	call s:Reformat_Range(a:firstline,a:lastline)
endfunction

function! textformat#Visual_Align_Right() range "{{{1
	let l:width = &textwidth
	if l:width == 0 | let l:width = s:default_width | endif

	execute a:firstline.','.a:lastline.'call s:Align_Range_Right('.l:width.')'
	normal! '>$
endfunction

function! textformat#Visual_Align_Justify() range "{{{1
	let l:width = &textwidth
	if l:width == 0 | let l:width = s:default_width | endif

	execute a:firstline.','.a:lastline.'call s:Align_Range_Left()'

	let l:last = s:Reformat_Range(a:firstline,a:lastline)
	let l:pos = getpos('.')
	execute a:firstline.','.l:last.'call s:Align_Range_Justify('.l:width.',1)'
	call setpos('.',l:pos)
endfunction

function! textformat#Visual_Align_Center() range "{{{1
	let l:width = &textwidth
	if l:width == 0 | let l:width = s:default_width | endif

	execute a:firstline.','.a:lastline.'call s:Align_Range_Center('.l:width.')'
	normal! '>$
endfunction

function! textformat#Quick_Align_Left() "{{{1
	let l:autoindent = &autoindent
	setlocal autoindent
	let l:pos = getpos('.')
	silent normal! vip:call s:Align_Range_Left()
	call setpos('.',l:pos)
	silent normal! gwip
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
	let l:autoindent = &autoindent
	setlocal autoindent
	let l:pos = getpos('.')
	silent normal! vip:call s:Align_Range_Left()
	call setpos('.',l:pos)
	silent normal! gwip
	let l:pos = getpos('.')
	silent normal! vip:call s:Align_Range_Justify(l:width,1)
	call setpos('.',l:pos)
	let &l:autoindent = l:autoindent
endfunction

function! textformat#Quick_Align_Center() "{{{1
	let l:width = &textwidth
	if l:width == 0 | let l:width = s:default_width  | endif
	let l:pos = getpos('.')
	silent normal! vip:call s:Align_Range_Center(l:width)
	call setpos('.',l:pos)
endfunction

function! textformat#Align_Command(align, ...) range "{{{1
	" For left align the optional parameter a:1 is [indent]. For others
	" it's [width].
	let l:pos = getpos('.')
	if a:align ==? 'left'
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

		if a:align ==? 'right'
			execute a:firstline.','.a:lastline.'call s:Align_Range_Right('.l:width.')'
		elseif a:align ==? 'justify'
			execute a:firstline.','.a:lastline.'call s:Align_Range_Justify('.l:width.')'
		elseif a:align ==? 'center'
			execute a:firstline.','.a:lastline.'call s:Align_Range_Center('.l:width.')'
		endif
	endif
	call setpos('.',l:pos)
endfunction

"{{{1 The ending stuff
let &cpo = s:save_cpo

" vim600: fdm=marker
doc/textformat.txt	[[[1
301
*textformat.txt*        Vim Text Formatter  (version 2.1)           2008-09-13


Description     This plugin provides commands and key mappings to quickly 
                align and format text. Text can be easily reformatted and 
                aligned to either left or right margin or justified to both 
                margins or centered. The text formatting commands provided by 
                this plugin are a bit more automatic and more intelligent than 
                those integrated to Vim. Together they make more powerful 
                command set for formatting text. 

Author          Teemu Likonen <tlikonen@iki.fi>




Contents

        1. Quick start                          |textformat-start|
        2. Commands                             |textformat-commands|
        3. Default key mappings                 |textformat-keymap|
        4. Configuration                        |textformat-config|
        5. Version history                      |textformat-history|

==============================================================================
1. Quick start                                              *textformat-start*

The impatient are always with us so below is a short info on (probably) the 
most important tools provided by this plugin. See the following sections of 
this manual for more detailed instructions.

<Leader>al              Left-align and reformat
<Leader>ar              Right-align
<Leader>aj              Left-right justify and reformat
<Leader>ac              Center lines

In normal mode the commands operate on current paragraph; in visual mode they 
operate on the selected lines. By default, <Leader> is the backslash key, so 
the mappings are actually \al, \ar, \aj and \ac, by default. If you have 
changed the g:mapleader variable in your .vimrc file <Leader> may be something 
else.

==============================================================================
2. Commands                                              *textformat-commands*

Let's start with the basic components of this plugin. These are the ex 
commands. You probably don't need these very often but they can be handy if 
you want to have text formatting and aligning as a part of a macro or function 
or something. The "daily tools" are explained later.

:[range]AlignLeft [indent]                                        *:AlignLeft*
                        Align to left all the lines in [range] (default is 
                        current line) and truncate all extra whitespace 
                        characters. That is, if there are more than one space 
                        between words they are reduced to just one. If 
                        'joinspaces' is set then two spaces are added after 
                        every sentence ending with character ".", "?" or "!".

                        If optional numeric argument [indent] is given then 
                        that is used as the left margin. If [indent] is not 
                        given the indent of the first line in the [range] (and 
                        the first line of each paragraph within the [range]) 
                        is used to define indent for the rest of the lines in  
                        the paragraph. There is one exception: if 
                        'formatoptions' contains "2" then the second line 
                        defines the indent for the rest of the lines in the 
                        paragraph.

                        Note: This is very similar to |:left| command except 
                        that this also truncates whitespaces and that without 
                        [indent] each paragraph's indent is detected and used.

                        Note: There is a possible unexpected behaviour: If 
                        command is run without [range] (i.e., it's just the 
                        current line) and [indent] is not given then this 
                        command just "aligns" to the current indent position 
                        and truncates whitespaces. You might see nothing 
                        happening if there weren't any extra whitespaces. Use 
                        [indent] (or |:left| command) to align to desired 
                        column.

:[range]AlignRight [width]                                       *:AlignRight*
                        Align to right all the lines in [range] (default is 
                        current line) and truncate all extra whitespace 
                        characters (honor 'joinspaces', as in :AlignLeft). 
                        [width] is used as the right margin. If [width] is not 
                        given the value of 'textwidth' option is used instead. 
                        If 'textwidth' is zero then the value of 80 is used.

                        Note: This is very similar to |:right| command except 
                        that this also truncates whitespaces.

:[range]AlignJustify [width]                                   *:AlignJustify*
                        Left-right justify lines in [range] (default is 
                        current line). This means adjusting spaces between 
                        words so that the lines fit. If 'joinspaces' is set 
                        then at least two spaces are printed after every 
                        sentence ending with a ".", "?" or "!". The first line 
                        in [range] (and the first line in each paragraph 
                        within the [range]) defines the indent for the rest of 
                        the lines in the paragraph, except if 'formatoptions' 
                        contains "2" then it's the second line.

                        Numeric argument [width] is used as the right margin. 
                        If [width] is not given the value of 'textwidth' is 
                        used instead. If 'textwidth' is zero then the value of 
                        80 is used.

                        Also see the Discussion below.

:[range]AlignCenter [width]                                     *:AlignCenter*
                        Center lines in [range] (default is current line) 
                        between the first column and [width]. All extra 
                        whitespace characters are truncated (but honor 
                        'joinspaces', just like in :AlignLeft). If [width] is 
                        not given the value of 'textwidth' option is used 
                        instead. If 'textwidth' is zero the value of 80 is 
                        used.

                        Note: This is very similar to |:center| except that 
                        this also truncates whitespaces.


Discussion ~

All the previous ex commands are rather "stupid" and operate on single lines 
only. They do not wrap lines nor do other kind of formatting. If [width] (or 
'textwidth') is too narrow for the line then some characters will go beyond 
the right margin. This is similar to Vim's own |:left|, |:right| and |:center| 
commands. More sophisticated formatting tools are provided as key mappings 
(see below).

Usually when paragraphs are justified the last line of each paragraph is 
aligned to left. However, :AlignJustify command does not do this. The purpose 
of this command is to do exactly what was asked for: left-right justify 
a range of lines. More sophisticated justify tools is <Leader>aj which 
reformats the paragraph (like |gw|), justifies lines and aligns each 
paragraph's last line to left.

All the commands truncate extra whitespaces which makes them work well 
together. This is particularly because the left-right justify needs to add 
extra spaces to make lines fill the text area. If you later want to reformat 
such previously justified paragraph and align it to left, for example, it's 
convenient that the tool automatically handles this and removes extra spaces. 
If you want to align text without truncating whitespaces use Vim's own align 
commands: |:left|, |:right| and |:center|.

==============================================================================
3. Default key mappings                                    *textformat-keymap*

By default this plugin provides a couple of key mappings for convenient text 
formatting. If the mappings have already been defined by user (or are taken by 
other plugins) then some of the following mappings may not be automatically 
available. See the next section of this manual for information on how to 
change the default mappings.

There are key mappings available for normal mode and visual mode. As usual, 
<Leader> is the backslash key by default but it can be changed with 
g:mapleader variable. Consult the Vim manual for more information on <Leader>.


Normal mode (current paragraph) ~

<Leader>al              Left-align the current "inner paragraph" (see |ip|) 
                        and reformat it according to 'textwidth'.

<Leader>ar              Right-align the current "inner paragraph" (see |ip|) 
                        to margin at 'textwidth'. This does not reformat the 
                        paragraph because with right-aligned text user usually 
                        wants to decide exactly what goes to what line.

<Leader>aj              Left-right justify the current "inner paragraph" (see 
                        |ip|). Technically each line's whitespaces are first 
                        truncated, then the text is reformatted according to 
                        'textwidth' and finally lines are justified. The last 
                        line (of each text paragraph) is aligned to left.

<Leader>ac              Center lines of current "inner paragraph" (see |ip|) 
                        between the first column and 'textwidth'. This does 
                        not reformat the paragraph because with centered text 
                        user usually wants to decide exactly what goes to what 
                        line.


Visual mode (range of lines) ~

{Visual}<Leader>al      Left-align and reformat {Visual} lines so that they 
                        fill 'textwidth'.

{Visual}<Leader>ar      Right-align {Visual} lines.

{Visual}<Leader>aj      Left-right justify {Visual} lines. First truncate all 
                        extra whitespace characters, then reformat lines so 
                        that they fill 'textwidth' and finally left-right 
                        justify. The last line of each paragraph as well as 
                        the last line in {Visual} range is aligned to left.

{Visual}<Leader>ac      Center {Visual} lines.


Both normal mode and visual mode commands truncate extra whitespace 
characters. If 'joinspaces' is set then an extra space is added after every 
sentence ending with a ".", "?" or "!". The first line in each paragraph 
inside the range defines the indent for the rest of the lines in the 
paragraph, except if 'formatoptions' contains "2" then it's the second line.


Paragraph definitions ~

The normal mode commands operate on the concept of "inner paragraph" (see 
|ip|). The effect is almost the same as selecting current paragraph with Vim's 
"vip" command and then executing the equivalent visual mode command. Such 
inner paragraph may contain several text paragraphs if 'formatoptions' 
contains "w". Each of them is reformatted separately with <Leader>al and 
<Leader>aj commands.

New paragraph always begins after a blank line. If 'formatoptions' contains 
"w" then new paragraph also begins after a line which ends with 
a non-whitespace character. That is, with "w" in 'formatoptions' every line 
which ends with a non-whitespace also ends a paragraph. In left-right justify 
(<Leader>aj) such line is aligned to left. You need to ensure that there is 
a trailing whitespace in every consecutive line which belongs to same 
paragraph (the whitespace is preserved after formatting). If 'formatoptions' 
does not contain "w" then all consecutive non-blank lines belong to same 
paragraph.

==============================================================================
4. Configuration                                           *textformat-config*

The key mappings can be configured freely by user. This plugin uses the 
default ones only if they are free and not used for other purposes. Here's an 
example of lines you could put to your .vimrc file:
>
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
rest of the line is a code word for Vim and this plugin. They must be written 
exactly as shown in the example. I think the code words are pretty much 
self-descriptive.

Don't use |:nnoremap| and |:vnoremap| commands here; they don't work because 
the right-hand side (<Plug>...) must be remappable. See Vim's manual for more 
information about the key map commands.

Most part of this plugin is loaded into memory when the text-formatting 
commands or key maps are used for the first time. Only the very minimum is 
automatically loaded when Vim is started. If you want to completely avoid 
loading this plugin put the following line to your .vimrc file:
>
        let g:loaded_textformat = 1

Happy formatting!

==============================================================================
5. Version history                                        *textformat-history*

        v2.1                                                        2008-09-13

              - Minor internal cleanup.

        v2.0                                                        2008-08-10

              - \al and \aj now reformat text also in visual mode.
              - \al and \aj honor "w" in 'formatoptions' and detect paragraph 
                boundaries accordingly.
              - :AlignLeft, :AlignJustify, \al and \aj recognize several 
                paragraphs within the range and detect indent for each 
                paragraph separately.
              - Add logic to load the plugin script only once.

        v1.1                                                        2008-08-04

              - Keep cursor position more accurately when formatting 
                a paragraph with \al and \aj.
              - When 'joinspaces' is set insert two spaces after .?! 
                punctuation with left-right justify. This is now similar to 
                other commands.

        v1.0                                                        2008-08-03

              - All the commands now follow user's 'expandtab' setting and 
                print leading whitespaces accordingly. Now this works just 
                like :left, :right and :center commands.
              - The left-aligned last line in justified paragraph did not 
                honor 'joinspaces'. Fixed.

        v0.9                                                        2008-08-01

              - Initial upload to http://www.vim.org .

==============================================================================
vim: ft=help tw=78 ts=8 et norl fo+=2aw
plugin/textformat.vim	[[[1
66
" Text formatter plugin for Vim text editor
"
" Version:              2.1
" Last Change:          2008-09-13
" Maintainer:           Teemu Likonen <tlikonen@iki.fi>
" License:              This file is placed in the public domain.
" GetLatestVimScripts:  2324 1 :AutoInstall: TextFormat

"{{{1 The beginning stuff
if &compatible || exists('g:loaded_textformat')
	finish
endif
let s:save_cpo = &cpo
set cpo&vim
"}}}1

if v:version < 700
	echohl ErrorMsg
	echomsg 'TextFormat plugin needs Vim version 7.0 or later. Sorry.'
	echohl None
	finish
endif

if !exists(':AlignLeft')
	command -nargs=? -range AlignLeft <line1>,<line2>call textformat#Align_Command('left',<args>)
endif
if !exists(':AlignRight')
	command -nargs=? -range AlignRight <line1>,<line2>call textformat#Align_Command('right',<args>)
endif
if !exists(':AlignJustify')
	command -nargs=? -range AlignJustify <line1>,<line2>call textformat#Align_Command('justify',<args>)
endif
if !exists(':AlignCenter')
	command -nargs=? -range AlignCenter <line1>,<line2>call textformat#Align_Command('center',<args>)
endif

nnoremap <silent> <Plug>Quick_Align_Paragraph_Left :call textformat#Quick_Align_Left()<CR>
nnoremap <silent> <Plug>Quick_Align_Paragraph_Right :call textformat#Quick_Align_Right()<CR>
nnoremap <silent> <Plug>Quick_Align_Paragraph_Justify :call textformat#Quick_Align_Justify()<CR>
nnoremap <silent> <Plug>Quick_Align_Paragraph_Center :call textformat#Quick_Align_Center()<CR>

vnoremap <silent> <Plug>Align_Range_Left :call textformat#Visual_Align_Left()<CR>
vnoremap <silent> <Plug>Align_Range_Right :call textformat#Visual_Align_Right()<CR>
vnoremap <silent> <Plug>Align_Range_Justify :call textformat#Visual_Align_Justify()<CR>
vnoremap <silent> <Plug>Align_Range_Center :call textformat#Visual_Align_Center()<CR>

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
let g:loaded_textformat = 1
let &cpo = s:save_cpo
" vim600: fdm=marker
