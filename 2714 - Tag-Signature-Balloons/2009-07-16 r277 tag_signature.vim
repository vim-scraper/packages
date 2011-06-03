" Tag Signature Balloon
"   Author: A. S. Budden
"   Date:   16th July 2009
"   Version: r277

if &cp || exists("g:loaded_tag_signature") || ! has('balloon_eval')
	finish
endif
let g:loaded_tag_signature = 1

function! GetTagSignature()
	let TagList = taglist(v:beval_text)
	let s = ""

	if len(TagList) > 0
		let s = substitute(TagList[0]['cmd'], '^/\^\(.*\)\$/$', '\1', '')
	endif
	return s
endfunction

set bexpr=GetTagSignature()
set ballooneval
