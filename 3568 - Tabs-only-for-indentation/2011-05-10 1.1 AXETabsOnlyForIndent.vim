"" AXETabsOnlyForIndent.vim
"" Description:    Using tabs only for indentation while editing.
""                 If left part of the cursor position in current line has only
""                 tabs or spaces, then they will be replaced by tabs and one
""                 more tab will be added. In other cases, only spaces will be
""                 added.
"" Maintainer:     Makoto Nokata <nokatamakoto@gmail.com>
"" Licence:        No Warranties. Do whatever you want with this.
"" Last Change:    2011-05-10
"" Version:        1.1
"" Usage:          Only two things:
""                 1) Put this file to the plugin directory;
""                 2) Add the folowing line to your ~/.vimrc config:
""                     imap <Tab> <Plug>AXEIndentTabs
""                 That's all!
"" BTW:            Yes, you can use another keybind ;)

if exists( 'loaded_AXETabsOnlyForIndent' )
	finish
endif

let loaded_AXETabsOnlyForIndent = 1

function! s:AXETabsOnlyForIndent()
	if mode() != 'i' | return | endif
	let cbefore = strpart( getline('.'), 0, col('.') - 1 )
	let tab = "\t"
	if substitute( cbefore, '[\t ]*', '', '' ) != ''
		let tab = printf( '%'.(&ts - ((virtcol('.') - 1) % &ts)).'s', '' )
	endif
	return tab
endfunction

inoremap <script> <silent>
	\ <Plug>AXEIndentTabs <C-R>=<SID>AXETabsOnlyForIndent()<CR>

