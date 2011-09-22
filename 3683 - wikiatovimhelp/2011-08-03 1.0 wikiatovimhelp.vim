" VimWikia Html Article to Vim help file converter {{{1
"
" Version 1.0
" Author: niva
" mail: nivaemail@gmail.com
"
function! ConvertVimWikiaToHelpFile()

	syn off

	"1-save the current file name
	let name=expand("%:p:t:r")

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
	%s/<\/*\(header\|p\|big\|ol\|div\|b\|ul\|i\|li\|dt\|tr\|td\|table\|strong\|''\|dl\|dd\)>//g
	%s/\(<hr\s*\/>\)//g
	%s/\(<\/*tt>\|''\)/'/g

    "return 0
	"3-format title to help format
	%s/^\s\+<h1>\([^<]\+\)<.*$/\1\~/

	"6-replace some specials html codes
	let listOfHtmlCodes=['&lt;', '&gt;' ,'&nbsp;', '&#58;', '&quot;' ,'&amp;' , '§', '&middot' ,'&#32;']
	let listOfAsciiChar=['<'   , '>'    ,' '     , ':'    , '"'      ,'&'     , '#', ''         ,'']
	let idx=0
	for item in listOfHtmlCodes
		let cmd='silent! %s/'.item.'/'.listOfAsciiChar[idx].'/g'
		exe cmd
		let idx+=1
	endfor

	"7-format code block to help file quoted text
	%s/<pre\(>\)/\1/g
	%s/\(<\)\/pre>/\1/g

	0
	while(search("^>$",'W')!=0)
		exe "norm jV" 
		call search("<$") 
		exe "norm k" 
		exe "norm >>"
	endwhile

	"8-delete unuseful lines and then blank lines
	let list=['accesskey', 'wikia-menu-button', 'chevron', 'WikiaArticle', 'class=', 'data-id', 'id="News', 'showTocToggle', '<div.*>' ,'<span style' ]
	for item in list
		if item==''
			continue
		endif
		let cmd='silent! g/'.item.'/d'
		exe cmd
	endfor

	0g/^\s*$/d
	%s/\(^\(\s*\)\n\)\{2,}//g
	%s/^\n\(>\n\)\n/\1/


	"9-reformat link to others html pages
	%s/<a\s\+href="\([^"]\+\)[^>]\+>\([^<]\+\)<\/a>/\2 |\1|/g

	"10-footer part for vim recognition
	norm Go"vim: set ft=help ff=unix fdm=marker ts=4 :expandtab:

	"11-indent paragraph
	2,$s/^\(\w\)/ \1/

	"12-wrap paragraph text
	0
	while(search('o\s\w\+','W')!=0)
		norm Vgw
	endwhile
	%s/^\s\{2,}/ /

	set ft=help
	syn on
	0
endfunc
command! -nargs=0 -complete=file TOVIMHELP   :silent! call ConvertVimWikiaToHelpFile()
"}}}1

" vim: set ft=vim ff=unix fdm=marker ts=4 :expandtab:
