" Simple snippet storage and retrieval separated by filetype
" Last Modified: Thu 2008-02-14 10:11:46 (Eastern Standard Time)
" Maintainer:    Jeremy Cantrell <jmcantrell@gmail.com>

if exists('loaded_snippets')
	finish
endif

let loaded_snippets = 1

let s:snippet_directory = split(&rtp,',')[0].'/snippets'
let s:snippet_filetype = ""

command -bar -range AddSnippet :<line1>,<line2>call AddSnippet()
command -bar -range PutSnippet :<line1>,<line2>call PutSnippet()
command -bar EditSnippet :call EditSnippet()
command -bar ListSnippets :call ListSnippets()
command -bar DeleteSnippet :call DeleteSnippet()

xmap <silent> <leader>sa :AddSnippet<cr>
nmap <silent> <leader>sa :%AddSnippet<cr>
nmap <silent> <leader>sp :PutSnippet<cr>
nmap <silent> <leader>sd :DeleteSnippet<cr>
nmap <silent> <leader>se :EditSnippet<cr>
nmap <silent> <leader>sl :ListSnippets<cr>

xmenu &Plugin.&Snippets.&Add<tab><leader>sa :AddSnippet<cr>
nmenu &Plugin.&Snippets.&Add<tab><leader>sa :%AddSnippet<cr>
menu &Plugin.&Snippets.&Edit<tab><leader>se :EditSnippet<cr>
menu &Plugin.&Snippets.&Delete<tab><leader>sd :DeleteSnippet<cr>
menu &Plugin.&Snippets.&List<tab><leader>sl :ListSnippets<cr>
menu &Plugin.&Snippets.&Put<tab><leader>sp :PutSnippet<cr>

function InitSnippets() "{{{
	if !isdirectory(s:snippet_directory)
		if GetChoice("Create snippet directory '".s:snippet_directory."'?")
			call mkdir(s:snippet_directory, "p")
		else
			return 0
		endif
	endif
	return 1
endfunction "}}}
function ListSnippets() "{{{
	if !InitSnippets()
		return
	endif
	let filetype = GetFiletype()
	if len(filetype) == 0
		call Warn("No filetype entered")
		return
	endif
	if !HasFiletype(filetype)
		call Warn("Filetype '".filetype."' does not exist")
		return
	endif
	let snippet_files = GetSnippetFiles(filetype, "")
	if len(snippet_files) == 0
		call Warn("No snippets for filetype '".filetype."'")
		return
	endif
	echo join(GetSnippetNames(snippet_files), "\n")
endfunction "}}}
function PutSnippet() range "{{{
	if !InitSnippets()
		return
	endif
	let filetype = GetFiletype()
	if len(filetype) == 0
		call Warn("No filetype entered")
		return
	endif
	if !HasFiletype(filetype)
		call Warn("Filetype '".filetype."' does not exist")
		return
	endif
	let snippet_files = GetSnippetFiles(filetype, "")
	if len(snippet_files) == 0
		call Warn("No snippets for filetype '".filetype."'")
		return
	endif
	let snippet_names = GetSnippetNames(snippet_files)
	let name = GetSnippet(filetype)
	if len(name) == 0
		call Warn("No snippet name entered")
		return
	endif
	if count(snippet_names, name) == 0
		call Warn("Snippet '".name."' does not exist")
		return
	endif
	let snippet_file = snippet_files[index(snippet_names, name)]
	if strlen(snippet_file) == 0
		return
	endif
	let lines = readfile(snippet_file)
	call append(a:firstline, lines)
endfunction "}}}
function AddSnippet() range "{{{
	if !InitSnippets()
		return
	endif
	let filetype = GetFiletype()
	if !HasFiletype(filetype)
		if GetChoice("Create filetype directory for '".filetype."'?")
			mkdir(s:snippet_directory.'/'.filetype)
		else
			call Warn("Directory for filetype '".filetype."' does not exist")
			return
		endif
	endif
	let name = StripString(input("Snippet: "))
	if len(name) == 0
		call Warn("No snippet name entered")
		return
	endif
	let ext = GetSnippetExtension(filetype)
	if len(ext) == 0
		call Warn("Extension could not be determined for filetype '".filetype."'")
		return
	endif
	let filename = s:snippet_directory.'/'.filetype.'/'.name.'.'.ext
	call writefile(getline(a:firstline, a:lastline), filename)
	echo "Snippet '".name."' added for filetype '".filetype."'"
endfunction "}}}
function EditSnippet() "{{{
	if !InitSnippets()
		return
	endif
	let filetype = GetFiletype()
	if len(filetype) == 0
		call Warn("No filetype entered")
		return
	endif
	if !HasFiletype(filetype)
		call Warn("Filetype '".filetype."' does not exist")
		return
	endif
	let snippet_files = GetSnippetFiles(filetype, "")
	if len(snippet_files) == 0
		call Warn("No snippets for filetype '".filetype."'")
		return
	endif
	let snippet_names = GetSnippetNames(snippet_files)
	let name = GetSnippet(filetype)
	if len(name) == 0
		call Warn("No snippet name entered")
		return
	endif
	if count(snippet_names, name) == 0
		call Warn("Snippet '".name."' does not exist")
		return
	endif
	let snippet_file = snippet_files[index(snippet_names, name)]
	if strlen(snippet_file) == 0
		return
	endif
	execute "tabedit ".snippet_file
endfunction "}}}
function DeleteSnippet() "{{{
	if !InitSnippets()
		return
	endif
	let filetype = GetFiletype()
	if len(filetype) == 0
		call Warn("No filetype entered")
		return
	endif
	if !HasFiletype(filetype)
		call Warn("Filetype '".filetype."' does not exist")
		return
	endif
	let snippet_files = GetSnippetFiles(filetype, "")
	if len(snippet_files) == 0
		call Warn("No snippets for filetype '".filetype."'")
		return
	endif
	let snippet_names = GetSnippetNames(snippet_files)
	let name = GetSnippet(filetype)
	if len(name) == 0
		call Warn("No snippet name entered")
		return
	endif
	if count(snippet_names, name) == 0
		call Warn("Snippet '".name."' does not exist")
		return
	endif
	let snippet_file = snippet_files[index(snippet_names, name)]
	if strlen(snippet_file) == 0
		return
	endif
	if !GetChoice("Delete snippet '".name."'?")
		return
	endif
	call delete(snippet_file)
	call Warn("Snippet '".name."' for filetype '".filetype."' deleted")
endfunction "}}}
function GetSnippetExtension(filetype) "{{{
	let files = GetSnippetFiles(a:filetype, "")
	if len(files) == 0
		return StringStrip(input("Enter extension for filetype '".filetype."': "))
	endif
	let tokens = split(GetSnippetFiles(a:filetype, "")[0], '\.')
	if len(tokens) < 2
		return ""
	endif
	return StripString(tokens[len(tokens)-1])
endfunction "}}}
function HasFiletype(filetype) "{{{
	if isdirectory(s:snippet_directory.'/'.a:filetype)
		return 1
	endif
	return 0
endfunction "}}}
function StripString(str) "{{{
	return substitute(substitute(a:str, '\s*$', '', 'g'), '^\s*', '', 'g')
endfunction "}}}
function Warn(message) "{{{
	echohl WarningMsg | echo a:message | echohl None
endfunction "}}}
function Error(message) "{{{
	echohl ErrorMsg | echo a:message | echohl None
endfunction "}}}
function GetChoice(prompt) "{{{
	let reply = StripString(tolower(input(a:prompt.' (y/n): ')))
	if reply == 'y'
		return 1
	else
		return 0
	endif
endfunction "}}}
function GetSnippet(filetype) "{{{
	let s:snippet_filetype = a:filetype
	let snippet = input("Snippet: ", "", "customlist,CompleteSnippetName")
	unlet! s:snippet_filetype
	return StripString(snippet)
endfunction "}}}
function GetFiletype() "{{{
	if len(&filetype) == 0
		return StripString(input("Filetype: ", "", "customlist,CompleteSnippetFiletype"))
	else
		return &filetype
	endif
endfunction "}}}
function CompleteSnippetName(arg_lead, cmd_line, cursor_pos) "{{{
	if len(s:snippet_filetype) == 0
		return
	endif
	return GetSnippetNames(GetSnippetFiles(s:snippet_filetype, a:arg_lead))
endfunction "}}}
function CompleteSnippetFiletype(arg_lead, cmd_line, cursor_pos) "{{{
	return GetSnippetFiletypes(GetSnippetDirs(a:arg_lead))
endfunction "}}}
function GetSnippetNames(snippet_files) "{{{
	let snippet_names = []
	for snippet_file in a:snippet_files
		let tokens = split(snippet_file, '/')
		call add(snippet_names, split(tokens[len(tokens)-1], '\.')[0])
	endfor
	return snippet_names
endfunction "}}}
function GetSnippetFiletypes(snippet_dirs) "{{{
	let snippet_filetypes = []
	for snippet_dir in a:snippet_dirs
		let tokens = split(snippet_dir, '/')
		call add(snippet_filetypes, tokens[len(tokens)-1])
	endfor
	return snippet_filetypes
endfunction "}}}
function GetSnippetFiles(filetype, arg_lead) "{{{
	return split(glob(s:snippet_directory.'/'.a:filetype.'/'.a:arg_lead.'*'),"\n")
endfunction "}}}
function GetSnippetDirs(arg_lead) "{{{
	return split(glob(s:snippet_directory.'/'.a:arg_lead.'*'), "\n")
endfunction "}}}

