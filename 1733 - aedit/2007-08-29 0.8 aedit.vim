" Author: Radu Dineiu <radu.dineiu@gmail.com>
" Version: 0.8
" URL: http://ld.yi.org/vim/aedit/aedit.vim
" Changelog: http://ld.yi.org/vim/aedit/changelog.txt

" Settings
set tabstop=4 shiftwidth=4 autoindent noexpandtab
set backspace=indent,eol,start
set backupext=.bak
if has('win32')
	set backupdir=c:\windows\temp directory=c:\windows\temp
	set tags=c:\.vimtags
else
	set backupdir=/tmp directory=/tmp
	set tags=/tmp/.vimtags
endif
set hlsearch history=10000 incsearch
set ignorecase smartcase
set undolevels=10000
set display="lastline,uhex"
set splitbelow
set whichwrap=<,>,h,l,b,s,~,[,] wildcharm=<Tab> wildmenu
set wildmode=list:longest,full
set laststatus=2 linebreak
set statusline=%<%f%(\ [%M%R%H%W]%)%=%(%<\ %o\ %02c%02V\ [%l/%L:%03p]%)
set printheader=%<%t\ %y%=%02p%%\ %02N

" Syntax
syn on
filetype plugin on
source $VIMRUNTIME/macros/matchit.vim

" Convenience shortcuts
command! CD cd %:p:h

" Write / Quit
nnoremap <silent> <M-w> :update<CR>
inoremap <silent> <M-w> <C-o>:update<CR>
nnoremap <M-W> :write!<CR>
inoremap <silent> <M-W> <C-o>:write!<CR>
nnoremap <M-q> :quit<CR>
inoremap <silent> <M-q> <C-o>:quit<CR>
nnoremap <M-Q> :quit!<CR>
inoremap <silent> <M-Q> <C-o>:quit!<CR>

" Home key
inoremap <silent> <Home> <C-o>^
nnoremap <silent> <Home> ^

" Utility functions
function! GetSynUnder()
	return synIDattr(synID(line('.'), col('.'), 1), 'name')
endfunction

function! GetCharUnder()
	return strpart(getline('.'), col('.') - 1, 1)
endfunction

function! GetCharBefore(offset)
	return strpart(getline('.'), col('.') - (a:offset + 1), 1)
endfunction

function! GetCharAfter()
	return strpart(getline('.'), col('.'), 1)
endfunction

function! AtEnd()
	return col('$') == col('.')
endfunction

function! GetStringBeforeCursor(offset)
	return strpart(getline('.'), 0, col('.') - a:offset)
endfunction

function! GetStringAfterCursor()
	return strpart(getline('.'), col('.'))
endfunction

function! GetWordBeforeCursor(keep_spaces)
	let regexp = '^.*\(\<\w\+\)'
	if !a:keep_spaces
		let regexp .= '\s*'
	endif
	let regexp .= '$'
	return substitute(GetStringBeforeCursor(0), regexp, '\1', '')
endfunction

function! GetExactWordBeforeCursor(offset)
	return substitute(GetStringBeforeCursor(a:offset), '^.*\(\<\w\+\)\s*$', '\1', '')
endfunction

function! GetFirstWord()
	return substitute(getline('.'), '^\W*\(\<\w\+\).*$', '\1', '')
endfunction

function! CountOccurances(haystack, needle)
	let occurances = 0
	let lastpos = 0
	let firstiter = 1
	while lastpos > -1
		if firstiter
			let lastpos = match(a:haystack, a:needle, lastpos)
		else
			let lastpos = match(a:haystack, a:needle, lastpos + 1)
		endif
		let firstiter = 0
		if lastpos > -1
			let occurances = occurances + 1
		endif
	endwhile
	return occurances
endfunction

function! InsideTag()
	let str = GetStringBeforeCursor(0) . GetCharUnder()
	return str =~ '^.*<[^/>]*$'
endfunction

function! InsideQuote(char)
	let str = GetStringBeforeCursor(0) . GetCharUnder()
	if !InsideTag()
		let tags_complete = CountOccurances(str, '<[^/>]*>')
		let tags_incomplete = CountOccurances(str, '<\w')
		let tags = tags_incomplete - tags_complete
		return (CountOccurances(str, a:char) - tags) % 2 != 0
	else
		return CountOccurances(str, a:char) % 2 != 0
	endif
endfunction

" PHP script template shortcut
function! InsertPHP()
	if &ft == 'php'
		if GetSynUnder() =~ '^php'
			call search('?>')
			call cursor(line('.'), col('.') + 2)
			if GetCharUnder() == '"'
				return "\<Right>"
			endif
			return ''
		else
			return "<?php\<CR>?>\<Up>\<End>\<CR>\<CR>\<CR>\<Up>\<Tab>"
		endif
	endif
	return '.'
endfunction
inoremap <silent> <M-.> <C-R>=InsertPHP()<CR>

" Insert ending characters
function! InsertAtEnd(char)
	let line = getline('.')
	if line =~ a:char . '$'
		return "\<Right>\<Left>"
	else
		let extra = ''
		if AtEnd()
			let extra = "\<Right>"
		endif
		if line =~ ';\s*$'
			return "\<C-o>mk\<End>\<Left>i" . a:char . "\<C-o>`ki" . extra
		else
			return "\<C-o>mk\<End>i" . a:char . "\<C-o>`ki" . extra
		endif
	endif
endfunction
inoremap <silent> <M-;> <C-R>=InsertAtEnd(';')<CR>
inoremap <silent> <M-,> <C-R>=InsertAtEnd(',')<CR>
inoremap <silent> <M-)> <C-R>=InsertAtEnd(')')<CR>
inoremap <silent> <M-]> <C-R>=InsertAtEnd(']')<CR>
inoremap <silent> <M-}> <C-R>=InsertAtEnd('}')<CR>

" Use <M-d> to delete the rest of the word
inoremap <silent> <M-d> <C-o>cw<Esc><Right>

" Use <C-a> as an alias to <Home>
inoremap <silent> <C-a> <C-o>^

" Use <C-e> as an alias to <End>
inoremap <silent> <C-e> <End>

" Use <F2> to toggle hlsearch
nnoremap <silent> <F2> :let &hlsearch = !&hlsearch<CR>
inoremap <silent> <F2> <C-o>:let &hlsearch = !&hlsearch<CR>

" Use <Tab> to switch between windows and indent in visual mode
nnoremap <Tab> <C-w>w
nnoremap <S-Tab> <C-w>W
vnoremap <silent> <Tab> >gv
vnoremap <silent> <S-Tab> <gv

" Execute last command
inoremap <M-:> <C-o>:echo ':' . @: \| execute @:<CR>
nnoremap <M-:> :echo ':' . @: \| execute @:<CR>

" Lines
function! IsBlockStart(offset)
	if &ft == 'vim'
		if getline(line('.') + a:offset) =~ '^\s*endfun\w\+$'
			return 1
		endif
	else
		if getline(line('.') + a:offset) =~ '^\s*{$'
			return 1
		endif
	endif
	return 0
endfunction

function! IsFuncStart()
	if &ft == 'vim'
		return getline(line('.')) =~ '^\s*fun' && getline(line('.') + 1) =~ '^\s*endfun'
	else
		return getline(line('.')) =~ '^\s*function\>'
	endif
	return 0
endfunction

function! JumpNext(tab)
	let line = getline('.')
	if line =~ ':$'
		return "\<End>\<CR>\<Tab>"
	endif
	if IsBlockStart(1)
		if &ft == 'vim'
			return "\<End>\<CR>\<Tab>"
		else
			return "\<Down>\<Down>\<End>"
		endif
	endif
	let retval = "\<End>\<CR>"
	if a:tab
		let retval .= "\<Tab>"
	endif
	return retval
endfunction
inoremap <C-CR> <C-R>=JumpNext(0)<CR>
inoremap <S-CR> <C-o>O
inoremap <C-S-CR> <C-R>=JumpNext(1)<CR>

function! JumpTab()
	if pumvisible()
		return "\<C-y>"
	endif
	let b = GetCharBefore(1)
	if b == '{' || b == '(' || b == '['
		if IsBlockStart(0)
			let line = getline(line('.') + 1)
			if (b == '{' && line =~ '^\s*}$') || (b == '(' && line =~ '^\s*)[;,]*$') || (b == '[' && line =~ '^\s*\][;,]*$')
				return "\<CR>\<Tab>"
			else
				return "\<Down>\<End>"
			endif
		else
			let next_line = getline(line('.') + 1)
			if b == '{' && &ft == 'javascript'
				return "\<CR>\<Up>\<End>\<CR>\<Tab>"
			else
				return "\<CR>\<Tab>"
			endif
		endif
	endif
	return "\<CR>"
endfunction
inoremap <CR> <C-R>=JumpTab()<CR>

" Braces
function! InsertBrace(mode)
	if (&ft == 'vim' && IsFuncStart()) || (&ft != 'vim' && IsBlockStart(1))
		return JumpNext(0)
	else
		let line = getline('.')
		let next_line = getline(line('.') + 1)
		if line =~ '{\s*$'
			if next_line =~ '^$'
				return "\<Down>\<Tab>"
			elseif next_line =~ '^\s\+$'
				return "\<Down>\<End>"
			elseif next_line =~ '^}\s*$' || next_line =~ '^\s*};\?\s*$'
				return "\<End>\<CR>\<Tab>"
			endif
		endif

		if &ft == 'vim'
			let end_word = 'end' . substitute(GetFirstWord(), 'else', '', '')
			return "\<End>\<CR>" . end_word . "\<Up>\<End>\<CR>\<Tab>"
		elseif &ft == 'python'
			if next_line =~ '^\s*pass\s*$'
				return "\<Down>\<End>\<C-w>"
			else
				return "\<End>\<CR>\<Tab>"
			endif
		else
			if &ft == 'php'
				if line =~ '^\s*<?.*?>$'
					let end_word = 'end' . substitute(GetFirstWord(), 'else', '', '')
					let retval = ''
					if line !~ ':\s*?>$'
						let retval = "\<End>\<Left>\<Left>\<Left>:"
					endif
					let end_word_match = '\<' . end_word . '\>'
					if next_line =~ end_word_match || getline(line('.') + 2) =~ end_word_match
						if next_line =~ '^\s*$'
							return retval . "\<Down>"
						else
							return retval . "\<End>\<CR>\<Tab>"
						endif
					else
						return retval . "\<End>\<CR><? " . end_word . "; ?>\<Up>\<End>\<CR>\<Tab>"
					endif
				elseif line =~ '^\s*case\s\+'
					let retval = ''
					if line !~ ':\s*$'
						let retval .= "\<End>:"
					endif
					if getline(line('.') + 1) =~ '^\s*break\>'
						return retval . JumpNext(1)
					else
						return retval . "\<End>\<CR>\<Tab>break;\<Up>\<End>\<CR>\<Tab>"
					endif
				endif
			endif
			if a:mode == '/'
				return "\<End>\<CR>{\<CR>}\<Up>\<CR>\<Tab>"
			elseif a:mode == '?'
				return "\<End> {\<CR>}\<Up>\<End>\<CR>\<Tab>"
			endif
		endif
	endif
endfunction
inoremap <silent> <M-/> <C-R>=InsertBrace('/')<CR>
inoremap <silent> <M-?> <C-R>=InsertBrace('?')<CR>

" Assignment
function! CheckAssign()
	let line = getline('.')
	if &ft != 'vim'
		let m_php = '^\s*\(var\|public\|private\|protected\|static\)*\s*\$[^ ]\+\s*$'
		let m_js = '^\s*\(var\)*\s*[a-zA-Z0-9_\.]\+\s*$'
		let m_as = '^\s*\(var\)*\s*[a-zA-Z0-9_\.:]\+\s*$'
		if (&ft == 'php' && match(line, m_php) == 0) || (&ft == 'javascript' && match(line, m_js) == 0) || (&ft == 'actionscript' && match(line, m_as) == 0)
			if line =~ '\s$'
				return "=;\<Left>"
			else
				return " = ;\<Left>"
			endif
		endif
	endif
	return '='
endfunction
inoremap <silent> = <C-R>=CheckAssign()<CR>

" Parens
function! CheckClose(char)
	if GetCharUnder() != a:char
		return a:char
	else
		if AtEnd()
			return a:char
		else
			return "\<Right>"
		endif
	endif
endfunction
inoremap <silent> ) <C-R>=CheckClose(')')<CR>
inoremap <silent> ] <C-R>=CheckClose(']')<CR>
inoremap <silent> } <C-R>=CheckClose('}')<CR>
" ; and , can also be detected here
inoremap <silent> ; <C-R>=CheckClose(';')<CR>
inoremap <silent> , <C-R>=CheckClose(',')<CR>

function! CheckOpen(char)
	if GetCharUnder() == a:char
		return "\<Right>"
	else
		let cword = GetWordBeforeCursor(1)
		let cunder = strpart(getline('.'), col('.') - 1, 1)
		if !AtEnd() && (a:char == '(' && cunder =~ '[a-zA-Z0-9_!&$]')
			return '('
		endif
		let repl = a:char
		if cword == 'if' || cword == 'for' || cword == 'while' || cword == 'switch'
			let repl = ' ' . a:char
		endif
		if a:char == '(' | let repl .= ')'
		elseif a:char == '[' | let repl .= ']'
		elseif a:char == '{'
			let line = getline('.')
			if line =~ '^\s*$'
				return '{'
			endif
			let repl .= '}'
			if &ft == 'javascript' && line =~ '=\s*;'
				return repl . "\<End>\<BS>\<Left>"
			endif
		endif
		return repl . "\<Left>"
	endif
endfunction
inoremap <silent> ( <C-R>=CheckOpen('(')<CR>
inoremap <silent> [ <C-R>=CheckOpen('[')<CR>
inoremap <silent> { <C-R>=CheckOpen('{')<CR>

" Quotes
let g:last_for = ''
function! CheckOpenQuote(char)
	if &ft == 'vim' && getline('.') =~ '^\s*$'
		return '" '
	else
		if GetCharUnder() == a:char
			" Check for HTML attributes
			if GetStringBeforeCursor(0) =~ '^\s*<label for="\w\+"'
				let word = substitute(getline('.'), '^\s*<label for="\(\w\)\(\w*\)".*$', '\U\1\E\2', '')
				let g:last_for = tolower(word)
				return "\<Right>\<Right>" . word . ':'
			endif
			return "\<Right>"
		elseif a:char == "'" && InsideQuote('"')
			return "'"
		else
			if (a:char == "'" && (&ft == 'php' || &ft == 'javascript' || &ft == 'python' || &ft == 'vim')) || a:char == '"'
				return a:char . a:char . "\<Left>"
			endif
		endif
	endif
	return a:char
endfunction
inoremap <silent> " <C-R>=CheckOpenQuote('"')<CR>
inoremap <silent> ' <C-R>=CheckOpenQuote("'")<CR>

function! AppendQuote(char)
	if &ft == 'php' || &ft == 'vim'
		return a:char . ' .  . ' . a:char . repeat("\<Left>", 4)
	elseif &ft == 'javascript'
		return a:char . ' +  + ' . a:char . repeat("\<Left>", 4)
	endif
	return a:char
endfunction
inoremap <silent> <M-'> <C-R>=AppendQuote("'")<CR>
inoremap <silent> <M-"> <C-R>=AppendQuote('"')<CR>

" Delete / Backspace
" use <C-Del> / <C-BS> to force normal behaviour
function! CheckDelete(dir)
	let b = GetCharBefore(1)
	let u = GetCharUnder()
	if (b == '(' && u == ')') || (b == '[' && u == ']') || (b == '{' && u == '}') || (b == '"' && u == '"') || (b == "'" && u == "'")
		return "\<BS>\<Del>"
	endif
	if a:dir == -1
		return "\<BS>"
	elseif a:dir == 1
		return "\<Del>"
	endif
endfunction
inoremap <silent> <BS> <C-R>=CheckDelete(-1)<CR>
" to map <Del> too:
" inoremap <silent> <Del> <C-R>=CheckDelete(1)<CR>

" Line numbers
set nonumber
inoremap <silent> <M-n> <C-o>:let &number = !&number<CR>
nnoremap <silent> <M-n> :let &number = !&number<CR>

" Text wrapping
set nowrap
inoremap <silent> <M-a> <C-o>:let &wrap = !&wrap<CR>
nnoremap <silent> <M-a> :let &wrap = !&wrap<CR>

" Move lines
function! SwapUp()
	if line('.') > 1
		let cur_col = virtcol('.')
		if line('.') == line('$')
			normal ddP
		else
			normal ddkP
		endif
		execute 'normal ' . cur_col . '|'
	endif
endfunction
nnoremap <silent> <M-Up> :call SwapUp()<CR>
inoremap <silent> <M-Up> <C-o>:call SwapUp()<CR>

function! SwapDown()
	if line('.') < line('$')
		let cur_col = virtcol('.')
		normal ddp
		execute 'normal ' . cur_col . '|'
	endif
endfunction
nnoremap <silent> <M-Down> :call SwapDown()<CR>
inoremap <silent> <M-Down> <C-o>:call SwapDown()<CR>

" Tab key
function! InsertTab(tab)
	if strpart(getline('.'), 0, col('.')-1) =~ '^\s*$'
		return "\<Tab>"
	else
		if a:tab == 'n'
			if pumvisible()
				return "\<C-n>"
			else
				return "\<C-p>"
			endif
		elseif a:tab == 'o'
			return "\<C-x>\<C-o>"
		endif
	endif
endfunction
inoremap <silent> <Tab> <C-R>=InsertTab('n')<CR>
inoremap <silent> <S-Tab> <C-R>=InsertTab('o')<CR>

" PHP syntax configuration
let php_special_functions = 1
let php_alt_comparisons = 1
let php_alt_properties = 1
let php_sql_query = 0
let php_htmlInStrings = 0
let php_folding = 3
let php_fold_arrays = 1

" Template and tag expansion
function! ExpandTemplate(ignore_quote)
	if a:ignore_quote || GetSynUnder() == 'htmlString' || (!InsideQuote("'") && !InsideQuote('"'))
		let cword = GetExactWordBeforeCursor(1)
		if exists('g:template' . &ft . cword)
			return "\<C-W>" . g:template{&ft}{cword}
		elseif exists('g:template_' . cword)
			return "\<C-W>" . g:template_{cword}
		endif
	endif
	return ExpandTag(' ')
endfunction
inoremap <silent> <Space> <C-R>=ExpandTemplate(0)<CR>

function! ExpandTag(char)
	if a:char == '>'
		if GetCharUnder() == '>'
			return "\<Right>"
		elseif GetCharBefore(1) == '>' && (&ft == 'php' || &ft == 'html')
			if &ft == 'php' && GetCharBefore(2) == '?'
				return '>'
			endif
			return "\<CR>\<CR>\<Up>\<Tab>"
		endif
	endif
	let sbefore = GetStringBeforeCursor(0)
	if GetCharUnder() == '>'
		return a:char
	endif
	if sbefore =~ '^.*<\w\+\S*$' && (&ft == 'php' || &ft == 'html')
		let cword = GetExactWordBeforeCursor(1)
		let sbefore1 = strpart(sbefore, 0, strlen(sbefore) - 1)
		if cword !~ '>' && CountOccurances(sbefore1, '<') > CountOccurances(sbefore1, '>')
			let cleft = repeat("\<Left>", len(cword) + 4)
			let retval = ''
			let close_tag = '></' . cword . '>' . cleft
			if cword == 'input' || cword == 'label' || cword == 'br' || cword == 'hr'
				let retval .= ">\<Left>"
			else
				let retval .= close_tag
			endif
			if a:char == ' '
				let retval = ' ' . retval
			else
				let retval .= "\<Right>"
			endif
			return retval
		endif
		return a:char
	endif
	return a:char
endfunction
inoremap <silent> > <C-R>=ExpandTag('>')<CR>

" <Up> and <Down> in word wrapping mode
nnoremap <Down> gj
nnoremap <Up> gk
function! MoveCursor(cmd)
	if a:cmd == 'down'
		if pumvisible() | return "\<Down>" | endif
		execute 'normal! gj'
		return ''
	else
		if pumvisible() | return "\<Up>" | endif
		execute 'normal! gk'
		return ''
	endif
endfunction
inoremap <silent> <Down> <C-R>=MoveCursor('down')<CR>
inoremap <silent> <Up> <C-R>=MoveCursor('up')<CR>

" Visual mode functions
function! Enclose(mode, indent)
	if a:mode == '{'
		let start = '{'
		let end = '}'
	elseif a:mode == '/'
		let start = '/**'
		let end = '/**/'
	endif
	let extra = ''
	if a:indent
		let extra = "\<BS>"
	endif
	call cursor(line("'<"), col("'<"))
	execute "normal! O" . extra . start
	call cursor(line("'>"), col("'>"))
	execute "normal! o" . extra . end
endfunction
vnoremap <silent> <M-{> >gv:<C-u>call Enclose('{', 1)<CR>
vnoremap <silent> <M-/> :<C-u>call Enclose('/', 0)<CR>

" NERDTree
nnoremap <silent> <C-Q> :NERDTreeToggle<CR>
inoremap <silent> <C-Q> <C-O>:NERDTreeToggle<CR>

" Template utility functions
function! InsideShortPHP()
	return GetStringBeforeCursor(0) =~ '^.*<?[^p]'
endfunction

function! InsertPHPBlock()
	if InsideShortPHP()
		let cword = substitute(GetStringBeforeCursor(0), '^.*\(\<\w\+\).*$', '\1', '')
		return ":\<End>\<CR><? end" . cword . "; ?>\<Up>\<End>" . repeat("\<Left>", 4)
	else
		return "\<CR>{\<CR>}\<Up>\<CR>\<Tab>\<Up>\<Up>\<End>"
	endif
endfunction

" PHP templates
let g:template{'php'}{'cl'} = "class \<CR>{\<CR>}\<Up>\<CR>\<Tab>\<Up>\<Up>\<End>"
let g:template{'php'}{'e'} = "echo "
let g:template{'php'}{'ex'} = "extends "
let g:template{'php'}{'st'} = "static "
let g:template{'php'}{'pb'} = "public "
let g:template{'php'}{'pv'} = "private "
let g:template{'php'}{'pt'} = "protected "
let g:template{'php'}{'n'} = "new ()\<Left>\<Left>"
let g:template{'php'}{'t'} = "$this->"
let g:template{'php'}{'v'} = "var "
let g:template{'php'}{'gl'} = "global $;\<Left>"
let g:template{'php'}{'fe'} = "foreach ()\<C-R>=InsertPHPBlock()\<CR>\<Left>"
let g:template{'php'}{'wh'} = "while ()\<C-R>=InsertPHPBlock()\<CR>\<Left>"
let g:template{'php'}{'sw'} = "switch ()\<C-R>=InsertPHPBlock()\<CR>\<Left>"
let g:template{'php'}{'cs'} = "case :\<CR>\<Tab>break;\<Up>\<End>\<Left>"
let g:template{'php'}{'csd'} = "default:\<CR>\<Tab>break;\<Up>\<End>\<CR>\<Tab>"
let g:template{'php'}{'ar'} = "array()\<Left>"
let g:template{'php'}{'rq'} = "require ;\<Left>"
let g:template{'php'}{'rqo'} = "require_once ;\<Left>"
let g:template{'php'}{'dn'} = "dirname(__FILE__) . ''\<Left>"
let g:template{'php'}{'p'} = "<?  ?>\<Left>\<Left>\<Left>"
let g:template{'php'}{'pe'} = "<?=  ?>\<Left>\<Left>\<Left>"
let g:template{'php'}{'srv'} = "$_SERVER['']\<Left>\<Left>"
let g:template{'php'}{'def'} = "define('');\<Left>\<Left>\<Left>"

let g:template{'php'}{'lab'} = "<label for=\"\"></label>" . repeat("\<Left>", 10)
function! InsertLastFor()
	if len(g:last_for) > 0
		let html = ' name="' . g:last_for . '"'
		let g:last_for = ''
		return html
	endif
	return ''
endfunction
let g:template{'php'}{'inp'} = "<input\<C-R>=InsertLastFor()\<CR> type=\"\">" . repeat("\<Left>", 2)
let g:template{'php'}{'sel'} = "<select\<C-R>=InsertLastFor()\<CR> >\<CR></select>\<Up>\<End>\<Left>"

" JavaScript templates
function! InsertJavaScriptFunction(mode)
	let result = ''
	let line = getline('.')
	let anon = 1
	if line =~ '^\s*$'
		let anon = 0
	elseif line =~ ';\s*$'
		let result = "\<End>\<BS>"
	endif
	let result .= "function" . (anon ? '()' : ' ()')
	if a:mode == 'ff'
		let result .= " {\<CR>}\<Up>\<End>\<CR>\<Tab>\<Up>\<End>" . repeat("\<Left>", (anon ? 3 : 4))
	else
		let result .= "\<CR>{\<CR>}\<Up>\<End>\<CR>\<Tab>\<Up>\<Up>\<End>" . repeat("\<Left>", (anon ? 1 : 2))
	endif
	if anon && line !~ ';\s*$'
		call setline('.', getline('.') . ';')
	endif
	return result
endfunction
let g:template{'javascript'}{'f'} = "\<C-R>=InsertJavaScriptFunction('f')\<CR>"
let g:template{'javascript'}{'ff'} = "\<C-R>=InsertJavaScriptFunction('ff')\<CR>"
let g:template{'javascript'}{'m'} = ": function()\<CR>{\<CR>}\<Up>\<CR>\<Tab>\<Up>\<Up>\<C-o>I"
let g:template{'javascript'}{'mm'} = ": function() {\<CR>}\<Up>\<End>\<CR>\<Tab>\<Up>\<C-o>I"
let g:template{'javascript'}{'cl'} = "var  = new function()\<CR>{\<CR>\<CR>}\<Up>\<Tab>\<Up>\<Up>\<C-o>^" . repeat("\<Right>", 4)
let g:template{'javascript'}{'t'} = "this."
let g:template{'javascript'}{'d'} = "document."
let g:template{'javascript'}{'v'} = "var "

" Vim templates
let g:template{'vim'}{'f'} = "function! ()\<CR>endfunction\<Up>\<End>\<Left>\<Left>"
let g:template{'vim'}{'r'} = "return "

" Python
let g:template{'python'}{'f'} = "def ():\<CR>\<Tab>pass\<Up>\<End>" . repeat("\<Left>", 3)
let g:template{'python'}{'fi'} = "def __init__(self):\<CR>\<Tab>pass\<Up>\<End>" . repeat("\<Left>", 2)
let g:template{'python'}{'cl'} = "class ():\<CR>\<Tab>pass\<Up>\<End>" . repeat("\<Left>", 3)
let g:template{'python'}{'p'} = 'pass'
let g:template{'python'}{'s'} = 'self.'

" JSP
let g:template{'jsp'}{'inc'} = '<%@ include file="" %>' . repeat("\<Left>", 4)

" General templates
let g:template{'_'}{'f'} = "function ()\<CR>{\<CR>}\<Up>\<CR>\<Tab>\<Up>\<Up>\<End>\<Left>\<Left>"
let g:template{'_'}{'ff'} = "function () {\<CR>}\<Up>\<End>" . repeat("\<Left>", 4)
let g:template{'_'}{'r'} = "return ;\<Left>"

" General HTML templates
let g:template{'_'}{'thtml'} = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">' . "\<CR><html>\<CR>\<Tab><head>\<CR>\<Tab><title></title>\<CR>" . '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' . "\<CR>\<BS></head>\<CR><body>\<CR></body>\<CR>\<BS></html>\<Up>\<Up>\<Up>\<Up>\<Up>\<End>\<Left>\<Left>\<Left>\<Left>\<Left>\<Left>\<Left>\<Left>"
let g:template{'_'}{'tht'} = g:template{'_'}{'thtml'}

let g:template{'_'}{'txhtml'} = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' . "\<CR><html>\<CR>\<Tab><head>\<CR>\<Tab><title></title>\<CR>" . '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' . "\<CR>\<BS></head>\<CR><body>\<CR></body>\<CR>\<BS></html>\<Up>\<Up>\<Up>\<Up>\<Up>\<End>\<Left>\<Left>\<Left>\<Left>\<Left>\<Left>\<Left>\<Left>"
let g:template{'_'}{'thtt'} = g:template{'_'}{'txhtml'}

let g:template{'_'}{'css'} = '<link rel="stylesheet" type="text/css" href="">' . repeat("\<Left>", 2)
let g:template{'_'}{'js'} = '<script type="text/javascript" src=""></script>' . repeat("\<Left>", 11)
