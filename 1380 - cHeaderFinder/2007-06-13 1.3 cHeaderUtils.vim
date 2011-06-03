"$Id: cHeaderUtils.vim,v 1.1 2005/10/31 15:57:51 penz Exp $
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Name:         cHeaderUtils
" Description:  Usefull functions for C headers.
" Author:       Leandro Penz <lpenz AT terra DOT com DOT br>
" Maintainer:   Leandro Penz <lpenz AT terra DOT com DOT br>
" Url:          http://www.vim.org/scripts/script.php?script_id=?
" Licence:      This program is free software; you can redistribute it
"                   and/or modify it under the terms of the GNU General Public
"                   License.  See http://www.gnu.org/copyleft/gpl.txt
" Credits:      Mathieu Clabaut <mathieu.clabaut@free.fr>, the author of
"                    vimspell, from where I got how to autogenerate the 
"                    help from within the script.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Documentation 
"
" Documentation should be available by ":help cHeaderUtils" command, once the
" script has been copied in you .vim/plugin directory.
"
" If you do not want the documentation to be installed, just put
" let b:cHeaderUtils_install_doc=0
" in your .vimrc, or uncomment the line above.
"
" The documentation is still available at the end of the script.
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Code
"

fu! <SID>WordUnderCursor()
	let iskbk=&isk
	setlocal isk-=(,)
	let myline = getline(line('.'))

    let nonSpaceStrInd1 = col('.') - 1
	while myline[nonSpaceStrInd1] =~ '\k' && nonSpaceStrInd1 > 1
		let nonSpaceStrInd1 = nonSpaceStrInd1 - 1
	endwhile
	while ! (myline[nonSpaceStrInd1] =~ '\k')
		let nonSpaceStrInd1 = nonSpaceStrInd1 + 1
	endwhile

    let nonSpaceStrInd2 = col('.')
	while myline[nonSpaceStrInd2] =~ '\k' && nonSpaceStrInd2 < strlen(myline)
		let nonSpaceStrInd2 = nonSpaceStrInd2 + 1
	endwhile
	while ! (myline[nonSpaceStrInd2] =~ '\k')
		let nonSpaceStrInd2 = nonSpaceStrInd2 - 1
	endwhile

	let &isk=iskbk
	return strpart(myline, nonSpaceStrInd1, nonSpaceStrInd2 - nonSpaceStrInd1 + 1)
endf

fu! <SID>IncludeLineGenerate(header)
	silent let tmp = system("basename '".a:header."'")
	return "#include \"".strpart(tmp, 0, strlen(tmp) - 1)."\"\n"
endf

fu! <SID>HeaderFromTag(word)
	keepjumps exe 'tag '.a:word.' | let filename=@% | pop'
	" If not in the header:
	if match(filename, "\.h$") < 0
		return substitute(filename, "\.c$", ".h", "")
	end
	" Else, filename already contains the header name:
	return filename
endf

fu! <SID>Prototype(word)
	exe 'keepjumps tag '.a:word." | let l=getline(line('.')) | keepjumps pop"
	return l
endf

fu! <SID>IncludeLines(word)
	let manline = system("man -P cat 3 ".a:word." 2>/dev/null | grep '#include'")
	if v:shell_error != 0
		let manline = system("man -P cat 2 ".a:word." 2>/dev/null | grep '#include'")
	end
	if v:shell_error == 0
		let manline = system("echo \"".manline."\" | sed -r -e 's! +#!#!' -e '/^$/d' | sort -u")
		return manline
	end
	let header = <SID>HeaderFromTag(a:word)
	return <SID>IncludeLineGenerate(header)
endf

fu! <SID>GotoHeaderFromTag(word)
	let header = <SID>HeaderFromTag(a:word)
	exe 'edit '.header.' | call search("\\<'.a:word.'\\>", "w")'
endf

fu! <SID>InstallDocumentation(full_name, revision)
	" Name of the document path based on the system we use:
	if (has("unix"))
		" On UNIX like system, using forward slash:
		let l:slash_char = '/'
		let l:mkdir_cmd  = ':silent !mkdir -p '
	else
		" On M$ system, use backslash. Also mkdir syntax is different.
		" This should only work on W2K and up.
		let l:slash_char = '\'
		let l:mkdir_cmd  = ':silent !mkdir '
	endif

	let l:doc_path = l:slash_char . 'doc'
	let l:doc_home = l:slash_char . '.vim' . l:slash_char . 'doc'

	" Figure out document path based on full name of this script:
	let l:vim_plugin_path = fnamemodify(a:full_name, ':h')
	let l:vim_doc_path    = fnamemodify(a:full_name, ':h:h') . l:doc_path
	if (!(filewritable(l:vim_doc_path) == 2))
		echomsg "Doc path: " . l:vim_doc_path
		execute l:mkdir_cmd . l:vim_doc_path
		if (!(filewritable(l:vim_doc_path) == 2))
			" Try a default configuration in user home:
			let l:vim_doc_path = expand("~") . l:doc_home
			if (!(filewritable(l:vim_doc_path) == 2))
				execute l:mkdir_cmd . l:vim_doc_path
				if (!(filewritable(l:vim_doc_path) == 2))
					" Put a warning:
					echomsg "Unable to open documentation directory"
					echomsg " type :help add-local-help for more informations."
					return 0
				endif
			endif
		endif
	endif

	" Exit if we have problem to access the document directory:
	if (!isdirectory(l:vim_plugin_path)
				\ || !isdirectory(l:vim_doc_path)
				\ || filewritable(l:vim_doc_path) != 2)
		return 0
	endif

	" Full name of script and documentation file:
	let l:script_name = fnamemodify(a:full_name, ':t')
	let l:doc_name    = fnamemodify(a:full_name, ':t:r') . '.txt'
	let l:plugin_file = l:vim_plugin_path . l:slash_char . l:script_name
	let l:doc_file    = l:vim_doc_path    . l:slash_char . l:doc_name

	" Bail out if document file is still up to date:
	if (filereadable(l:doc_file)  &&
				\ getftime(l:plugin_file) < getftime(l:doc_file))
		return 0
	endif

	" Prepare window position restoring command:
	if (strlen(@%))
		let l:go_back = 'b ' . bufnr("%")
	else
		let l:go_back = 'enew!'
	endif

	" Create a new buffer & read in the plugin file (me):
	setl nomodeline
	exe 'enew!'
	exe 'r ' . l:plugin_file

	setl modeline
	let l:buf = bufnr("%")
	setl noswapfile modifiable

	norm! zR
	norm! gg

	" Delete from first line to a line starts with
	" === START_DOC
	1,/^=\{3,}\s\+START_DOC\C/ d

	" Delete from a line starts with
	" === END_DOC
	" to the end of the documents:
	/^=\{3,}\s\+END_DOC\C/,$ d

	" Remove fold marks:
	% s/{\{3}[1-9]/    /

	" Add modeline for help doc: the modeline string is mangled intentionally
	" to avoid it be recognized by VIM:
	call append(line('$'), '')
	call append(line('$'), ' v' . 'im:tw=78:ts=8:ft=help:norl:')

	" Replace revision:
	exe "norm! :1s/#version#/ v" . a:revision . "/\<CR>"

	" Save the help document:
	exe 'w! ' . l:doc_file
	exe l:go_back
	exe 'bw ' . l:buf

	" Build help tags:
	exe 'helptags ' . l:vim_doc_path

	return 1
endf

" Autodoc install:

if exists("b:cHeaderUtils_install_doc") && b:cHeaderUtils_install_doc==0
	finish
end

let s:revision= substitute("$Revision: 1.1 $",'\$\S*: \([.0-9]\+\) \$','\1','')
silent! let s:install_status = <SID>InstallDocumentation(expand('<sfile>:p'), s:revision)
if (s:install_status == 1)
	echom expand("<sfile>:t:r") . ' v' . s:revision . ': Help-documentation installed.'
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Interface

fu! CHeaderUtils_PrototypeFromFuncUnderCursor()
	redraw | echon <SID>Prototype(<SID>WordUnderCursor())
endf

fu! CHeaderUtils_HeadersFromFuncUnderCursor()
	let @"=<SID>IncludeLines(<SID>WordUnderCursor())
endf

fu! CHeaderUtils_GotoHeaderFromFuncUnderCursor()
	call <SID>GotoHeaderFromTag(<SID>WordUnderCursor())
endf

finish

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Documentation Contents
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

=== START_DOC
*cHeaderUtils.txt*   utilities for C headers.                       #version#


                        CHEADERUTILS REFERENCE MANUAL~


Utilities for C headers.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License.  See
    http://www.gnu.org/copyleft/gpl.txt

==============================================================================
CONTENT                                                *cHeaderUtils-contents* 

    Installation                       : |cHeaderUtils-install|
    Intro                              : |cHeaderUtils|
    Keys                               : |cHeaderUtils-keys|
    Todo list                          : |cHeaderUtils-todo|
    Links                              : |cHeaderUtils-links|

==============================================================================
1. cHeaderUtils Installation                            *cHeaderUtils-install*

    In order to install the plugin, place the cHeaderUtils.vim file into a plugin
    directory in your runtime path (please see |add-global-plugin| and
    |'runtimepath'|).

    A key-map should also be made. Put in your |.vimrc| something like: >
        nnoremap <leader>fh :call CHeaderUtils_HeadersFromFuncUnderCursor()<CR>
        nnoremap <leader>a :call CHeaderUtils_GotoHeaderFromFuncUnderCursor()<CR>
<

==============================================================================
2. cHeaderUtils Intro                                           *cHeaderUtils*

    This is cHeaderUtils, a collection of utilities to ease the maintenance of
    C code.


2.1 List of Features:                                  *cHeaderUtils-features*
---------------------
    
    - Go to any header file corresponding to any function.
    - Fetch the list of header files required for a function.

==============================================================================
3. cHeaderUtils Options                                    *cHeaderUtils-opts*

    There are no options for cHeaderUtils.

==============================================================================
4. cHeaderUtils Todo                                       *cHeaderUtils-todo*

    - Improve documentation.
    - More features!

==============================================================================
5. cHeaderUtils Links                                     *cHeaderUtils-links*

    http://www.vim.org/scripts/script.php?script_id=?
        Home page of cHeaderUtils.

=== END_DOC

" vim: fdm=marker foldmarker=fu!,endf

