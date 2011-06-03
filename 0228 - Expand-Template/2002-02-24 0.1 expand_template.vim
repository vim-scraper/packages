" File:        expand_template.vim
" Scirpt Name: Expand Template
" Version:     0.1
" Author:      Little Dragon
" Email:       littledragon@altern.org
"
" This script is intended as a way to define and use templates while
" editing files with ViM.
" 
" Key Bindings:
" Normal and Insert Mode
" ----------------------
"  C-_     expand template
"
" Usage Examples:
" You can define a template by the name of "func1", for example, using
" the following command:
"   let g:template{"func1"} = 'do_something("|")'
"
" To call the template, wether you're in insert or normal mode, you will
" have to type
"   func1<C-_>
"
" After you press C-_, "func1" will be replaced with do_something(""),
" and the cursor will be placed between the quotes. So "func1" will
" become:
"   do_something("|")
"
" where | is the position of the cursor.
"
" ---------------------------------------------------------------------
"
" To specify a multiline template, you would use, for example:
"   let g:template{"fmain"} = 'void main(void) {~	~}'
"
" The ~ characters will be replaced with a newline. This behaviour can
" be modified by specifying a g:template_newline character. So, for
" example, if you would like to use @ as a newline character, you will
" have to execute:
"   let g:template_newline = '@'
"   let g:template_newline_escaped = '\@'
"
" (if the character is special, it has to be escaped, else you would
" specify the same character in g:template_newline_escaped).
" Now, if you type:
"    vfunc<C-_>
"
" you will get instead of "vfunc":
"    void main(void) {
"    	
"    }|
"
" where | is the position of the cursor.
"
" NOTE:
" Cursor positioning is not yet supported in multiline templates.
"
" Configuration:
" The configurable variables are:
"
"   g:template_curpos            the character designating the cursor
"                                position (default: |)
"   g:template_newline           the character designating a new line
"                                (default: ~)
"   g:template_newline_escaped   the escaped newline character
"                                (default: \~)
"
" Defining a new template:
"
"   g:template{"name"} = "value"


" Default values
let g:template_curpos = '|'
let g:template_newline = '~'
let g:template_newline_escaped = '\~'
let g:template{"qqq"} = 'function |()~	firstline~	second("") "comment~	third~endfunction'
let g:template{"q"} = 'test("|")'

" Main function
function! ExpandTemplate()
	let s:bc = strpart(getline("."), 0, col("."))
	let s:ac = strpart(getline("."), col("."), strlen(getline("."))-col("."))
	let s:bcword = substitute(s:bc, '^.*\W\(\w*\)$', '\1', '')
	let s:acword = substitute(s:ac, '^\(\w\{-}\)\W.*$', '\1', '')
	let s:rem    = substitute(s:ac, '^\w\{-}\(\W.*\)$', '\1', '')
	let s:word   = s:bcword . s:acword
	let s:wstart = match(s:bc, '\w*$')
	let s:wend   = s:wstart+strlen(s:word)
	if(exists("g:template".s:word))
		let s:line   = getline(".")
		let s:nline  = strpart(s:line, 0, s:wstart) . g:template{s:word} . strpart(s:line, s:wend, strlen(s:line)-s:wend)
		echo s:nline
		if stridx(g:template{s:word}, g:template_newline) > -1
			let s:curpos_line = -1
			let s:last_nlmatch = match(g:template{s:word}, g:template_newline_escaped)
			let s:first_part = strpart(s:nline, 0, match(s:nline, g:template_newline_escaped, s:wstart))
			let s:lead = substitute(s:nline, '^\(\W\{-}\)\w.*$', '\1', '')
			let s:first_line = line(".")
			let s:last_line = line(".")
			call setline(s:last_line, s:first_part)
			while s:last_nlmatch > -1
				let s:str = strpart(g:template{s:word}, s:last_nlmatch+1, strlen(g:template{s:word})-s:last_nlmatch)
				let s:str = substitute(s:str, '^\(.\{-}\)' . g:template_newline_escaped . '.*$', '\1', '')
				call append(s:last_line, s:lead . s:str)
				let s:last_line = s:last_line + 1
				let s:last_nlmatch = match(g:template{s:word}, g:template_newline_escaped, s:last_nlmatch+1)
			endwhile
			let s:last_pos = strlen(getline(s:last_line))
			call setline(s:last_line, getline(s:last_line) . s:rem)
			if s:curpos_line > -1
				execute s:curpos_line
				execute "normal " . s:curpos_last . "|"
				execute "normal s"
			else
				execute s:last_line
				execute "normal 0"
				execute "normal " . (s:last_pos-1) . "l"
			endif
		else
			call setline(line("."), s:nline)
			let s:pos = stridx(g:template{s:word}, g:template_curpos)
			let s:pos = s:pos + s:wstart + 1
			execute "normal " . s:pos . "|"
			execute "normal s"
		endif
	endif
endfunction

" Key bindings
imap <C-_> <Esc>:call ExpandTemplate()<CR>a
map  <C-_>      :call ExpandTemplate()<CR>a
