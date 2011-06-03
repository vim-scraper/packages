" Tag Signature Balloon
"   Author: A. S. Budden
"   Date:   21st July 2009
"   Version: r284

if &cp || exists("g:loaded_tag_signature") || ! has('balloon_eval')
	finish
endif
let g:loaded_tag_signature = 1

if !exists('g:TagSignatureAllowFileLoading')
	let g:TagSignatureAllowFileLoading = 1
endif

function! GetTagSignature()
	let TagList = taglist('\<' . v:beval_text . '\>')
	let s = ""

	if len(TagList) > 0
		if TagList[0]['cmd'] =~ '^\d\+$'
			let LineNum = str2nr(TagList[0]['cmd'])
			let FileName = TagList[0]['filename']
			if bufexists(FileName)
				let s = getbufline(FileName, LineNum)[0]
			elseif g:TagSignatureAllowFileLoading == 1
				let s = readfile(FileName, '', LineNum)[LineNum-1]
			endif
		else
			let s = substitute(TagList[0]['cmd'], '^/\^\(.*\)\$/$', '\1', '')
		endif
	endif
	let s = substitute(s, '^\s*\(.\{-}\)\s*$/', '\1', '')
	return s
endfunction

set bexpr=GetTagSignature()
set ballooneval
