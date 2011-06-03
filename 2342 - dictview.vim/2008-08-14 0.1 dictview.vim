" Vim plugin -- print (= echo) contents of a variable
" File:		dictview.vim
" Created:	2008 May 25
" Last Change:	2008 Aug 14
" Rev Days:	4
" Author:	Andy Wokula <anwoku@yahoo.de>
" Version:	0.1

" Examples:
"   :let mylist = ["foo","bar"]
"   :DictView mylist
"   * [
"	'foo'
"	'bar'
"   * ]
"   :DictView {1: mylist}
"   * {
"   *	1:
"	[
"	  'foo'
"	  'bar'
"	]
"   * }

" Notes:
" - output: leading stars mark top level entries (esp. dictionary keys)
" - careful: cannot detect recursive entries
" - alternatively stop endless loops:
"	:h maxfuncdepth
" - Vim7.2: float entries are ignored! (untested)

if exists("loaded_dictview")
    finish
endif
let loaded_dictview = 1

if v:version < 700
    echomsg "dictview: You need at least Vim7.0"
    finish
endif

let s:cpo_sav = &cpo
set cpo&vim

if !exists("g:dictview_shiftwidth")
    let g:dictview_shiftwidth = 2
endif

if !exists("g:dictview_maxlevel")
    let g:dictview_maxlevel = 10
endif

" return lines as a list
func! DictView_PrintList(value, ...)
    let level =  a:0>=1 ? a:1 : 0
    let oneind = repeat(" ", g:dictview_shiftwidth)
    let indstr = a:0>=2 ? a:2 : repeat(oneind, level)
    let type = type(a:value)
    if level > g:dictview_maxlevel
	return [indstr. "..."]
    endif
    if type == 0
	return [indstr. a:value]
    elseif type == 1
	return [indstr. "'". substitute(a:value, "'", "''", 'g'). "'"]
    elseif type == 2
	return [indstr. string(a:value)]
    elseif type == 3
	let sofar = [ indstr. "[" ]
	try
	    for item in a:value
		call extend(sofar, DictView_PrintList(item, level+1))
		unlet item
	    endfor
	catch
	    call add(sofar, indstr.oneind. "...!")
	endtry
	call add(sofar, indstr. "]")
	" let sofar[-1] .= " ]"
	return sofar
    elseif type == 4
	let sofar = [ indstr. "{" ]
	try
	    for key in sort(keys(a:value))
		if type(a:value[key]) <= 2
		    call add(sofar, indstr. oneind. key. ": ".
			\ DictView_PrintList(a:value[key], level+1, "")[0])
		else
		    call add(sofar, indstr. oneind. key. ":")
		    call extend(sofar, DictView_PrintList(a:value[key], level+1))
		endif
	    endfor
	catch
	    call add(sofar, indstr.oneind. "...!")
	endtry
	call add(sofar, indstr. "}")
	return sofar
    endif
endfunc

func! DictView_Print(value)
    echo "\n". join(DictView_PrintList(a:value, 1, "* "), "\n")
endfunc

com! -nargs=1 -complete=expression DictView call DictView_Print(<args>)

let &cpo = s:cpo_sav
unlet s:cpo_sav

"vim:ts=8:noet:sw=4:sts=4
