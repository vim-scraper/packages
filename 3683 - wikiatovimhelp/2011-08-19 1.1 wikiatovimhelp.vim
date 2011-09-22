" VimWikia Html Article to Vim help file converter {{{1
"
" Version 1.0 : initial upload
" Version 1.1 : add those features
"               - generates help files into $tmp dir
"               - add function to search into help files generated and add
"               tags
"
"
"
"
" Instructions : 
"
"               1. Download one or all vimwikia articles or tips from 
"               http://vim.wikia.com/wiki/Vim_Tips_Wiki into a directory of 
"               your choice (on windows i use httrack)
"
"               2. Open all html files into vim and call the converter cmd on
"               all buffers
"               :bufdo! VIMHELP
"
"               3. Consult wikiaArticles into help format within Vim
"               all converted articles are in $TMP/wikiaArticles
"
"               -> grep, vimgrep command facilities you to find  the key word you
"               search for
"
"
" Author: niva
" mail: nivaemail@gmail.com
"}}}1
"=============================================================================
" Func InitVars {{{1
fun! InitVars()
	let g:wikiarepo=expand("$TMP").'\wikiaArticles'
endfunction
" }}}1
"=============================================================================
" Func VimWikia Creating Local Repository {{{1
fun! CreateLocalRepo()

	" 0-make repository directory if needed
	if !isdirectory(g:wikiarepo)
		silent! call mkdir(g:wikiarepo)
	endif

endfunction
" }}}1
"=============================================================================
" Func VimWikia Html to Vim help file converter {{{1
function! ConvertVimWikiaToHelpFile()

	syn off
	set noswf

	call InitVars()
	call CreateLocalRepo()

	"1-save the current file name
	let s:name=expand("%:p:t:r")

	"2-search for the article and copy it to a new buffer
	0
	call search("WikiaPageHeader")
	call search("<h1>")
	norm V
	call search('id..Comments\|NewPP limit report')
	norm k"ay

	enew!
	put=@a

	"4-format paragraph
	%s/<p>/ o /

	"5-cleaning begin and end tags
	%s/<\/*\(h2\|header\|p\|big\|ol\|div\|b\|ul\|i\|li\|dt\|tr\|td\|table\|strong\|''\|dl\|dd\|tt\)>//g
	%s/\(<hr\s*\/>\)//g
	%s/\(<\/*tt>\|''\)/'/g

	"3-format title to help format
	%s/^\s\+<h1>\([^<]\+\)<.*$/\1\~/

	"6-replace some specials html codes
	let s:listOfHtmlCodes=['&lt;', '&gt;' ,'&nbsp;', '&#58;', '&quot;' ,'&amp;' , '§', '&middot' ,'&#32;']
	let s:listOfAsciiChar=['<'   , '>'    ,' '     , ':'    , '"'      ,'\&'     , '#', ''         ,'']
	let s:idx=0
	for item in s:listOfHtmlCodes
		let s:cmd='silent! %s/'.item.'/'.s:listOfAsciiChar[s:idx].'/g'
		exe s:cmd
		let s:idx+=1
	endfor

	"8-delete unuseful lines and then blank lines
	let s:list=['accesskey', 'wikia-menu-button', 'chevron', 'WikiaArticle', 'class=', 'data-id', 'id="News', 'showTocToggle', '<div.*>' ,'span style' ]
	for item in s:list
		if item==''
			continue
		endif
		let s:cmd='silent! g/'.item.'/d'
		exe s:cmd
	endfor

	0g/^\s*$/d
	%s/\(^\(\s*\)\n\)\{2,}//g
	%s/^\n\(>\n\)\n/\1/


	"9-reformat link to others html pages
	%s/<a\s\+href="\([^"]\+\)[^>]\+>\([^<]\+\)<\/a>/\2 |\1|/g

	"10-footer part for vim recognition
	norm Go vim:tw=78:sw=4:ts=8:ft=help:norl:fdm=marker:

	"11-indent paragraph
	2,$s/^\(\w\)/ \1/

	"12-wrap paragraph text
	0
	while(search('o\s\w\+','W')!=0)
		norm Vgw
	endwhile
	%s/^\s\{2,}/ /

	"7-format vimscript code block to help file quoted text
	%s/<pre\(>\)/\1/g
	%s/\(<\)\/pre>/\1/g

	set ft=vim

	0
	while(search("^>$",'W')!=0)
		exe "norm jV" 
		call search("<$") 
		exe "norm k" 
		exe "norm ="
		exe "norm gv>"
	endwhile



	exe "w! ".g:wikiarepo."/".s:name.'.txt"'
	exe "bd!"
	exe "bd!"

	unlet! s:name
	unlet! s:listOfHtmlCodes
	unlet! s:listOfAsciiChar
	unlet! s:cmd
	unlet! s:idx
	unlet! s:list

	set ft=help
	syn on

endfunc
command! -nargs=0 -complete=file VIMHELP   :silent! call ConvertVimWikiaToHelpFile()
"}}}1
"=============================================================================
"{{{1 Func VimWikia Inception
fun! VimWikiaInception() 

	call InitVars()

	let keyword=input("Keyword to search for?")

	" search for keyword
	echo "Please Wait while searching for keyword '".keyword."'..."


	" vimgrep has good response on unix system but not on windows os
	" if exists("$windir")
	" 	" windows search is faster than vimgrep in this case
	" 	let g:Findstr_Default_Filelist = '*.txt'
	" 	let Findstr_Default_Options = '/i'
	" 	let cmd= 'Findpattern '.keyword
	" else
		" vimgrep search too slow on windows
		let cmd= 'vimgrep /'.keyword.'/ '.escape(g:wikiarepo, ' ').'\**/*.*'
		exe cmd
	" endif
	"
	return 0

	" add the keyword to the file where it was found
	exe "cd ".g:wikiarepo
	let maxBufferToTeat=len(getqflist())
	let treatedBuffers={}
	if maxBufferToTeat>0
		call setqflist(getqflist())
		let list=getqflist()
		for d in list

			let keyExists=0
			for key in keys(treatedBuffers)
				if key==d.bufnr
					let keyExists=1
				endif
			endfor
			if keyExists==0
				if d.bufnr!=0
					"
					let treatedBuffers[d.bufnr]='v'

					"open buffer
					exe "silent! buf ".d.bufnr

					" add keyword
					" let newkeyword="*".keyword."*"
					" if stridx(getline(1),'~')>-1
					" 	call append(0,newkeyword)
					" else
					" 	if stridx(getline(1),''.keyword)==-1
					" 		call setline(1, getline(1).' '.newkeyword)
					" 	endif
					" endif
					" silent! exe "w!"
					"
					silent! exe "bd!"
				endif
			endif
		endfor                                         
	endif

	copen

	" build vim helptags
	" exe "silent! helptags ++t ".g:wikiarepo

endfunction 
command! -nargs=0 -complete=file VIMWIKIAINCEPTION  :call VimWikiaInception()
" amenu icon=$HOME/vimfiles/icons/demarrage.bmp       ToolBar.VIMWIKIAINCEPTION :call VimWikiaInception()<CR>
" tmenu ToolBar.VIMWIKIAINCEPTION						Modify local vimwikia database files
"}}}1
"}}}1

" vim: set ft=vim ff=unix fdm=marker ts=4 :expandtab:
