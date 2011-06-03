"""""""""""""""""""""""""""""""""""""""""""
" Vim script for enhancing the vcscommand.vim script. You can easily diff between two
" revisions. It also add coloring to the log buffer. Right now it supports svn and hg.
"
" Maintainer: wenzhi.liang AT gmail.com
"
" USAGE:
" - Call VCSLog
" - Go to revision a, press 's'
" - Go to revision b, press 's'
" - Press 'd'
"
" NEED:
" - diffuse
" - Vim compile with +sign
"
" PLATFORM:
" - Only for Linux right now.
"
" BUG:
"
" LIMITATION:
" - The VCSCommandDeleteOnHide must be set to true
" - User cannot use customized Log buffer name
"
" HISTORY:
" Thu Dec 23 Added mercurial support
"
""""""""""""""""""""""""""""""""""""""""""""

let s:index = 0 " array accessing index
let s:diff_revision_array = ['none', 'none']
let s:log_lines = ['none', 'none']
let s:filename = ''
if has( "signs" ) == 0
	echoe 'Vim not compiled with +signs support.'
	finish
endif
if ! executable('diffuse') 
	echoe 'diffuse not installed.'
	finish
endif

sign define w_vcsdiff text=>< texthl=Search 

func! <SID>Init()
	let s:diff_revision_array[0] = 'none'
	let s:diff_revision_array[1] = 'none'
	let s:log_lines[0] = 'none'
	let s:log_lines[1] = 'none'
	let s:filename = ''
endfunc

function! <SID>Debug()
	echo s:diff_revision_array[0]
	echo s:diff_revision_array[1]
	echo s:log_lines[0]
	echo s:log_lines[1]
	echo s:filename 
endfunc

func! <SID>OnBufferExit()
	call <SID>Init()
	bwipeout
endfunc

func! <SID>AddRevisionToArray( r )
	"This is always called on the current line
	if s:diff_revision_array[0] != a:r && s:diff_revision_array[1] != a:r
		let id=s:index+100
		let s:diff_revision_array[s:index] = a:r
		exe "sign unplace " . id
		"echo "sign place " . id . " name=w_vcsdiff line=" . line('.') . ' buffer=' . winbufnr('%')
		" the sign id cannot be 0
		exe "sign place " . id . " name=w_vcsdiff line=" . line('.') . ' buffer=' . winbufnr('%')
	else
		return
	endif
	let s:index += 1
	if s:index >= 2
		let s:index = 0
	endif
endfunc

func! <SID>DoRevisionDiff()
	if s:diff_revision_array[0] == 'none' || s:diff_revision_array[1] == 'none'
		return
	else
		if str2nr( s:diff_revision_array[0] ) > str2nr( s:diff_revision_array[1] )
			let r_left = 1
			let r_right = 0
		else
			let r_left = 0
			let r_right = 1
		endif

		silent exec '!diffuse -r ' . s:diff_revision_array[r_left] . ' -r ' . s:diff_revision_array[r_right] . ' ' . s:filename . ' &'
	endif
endfunc

func! <SID>DisplaySelection()
	exe 'sign place 1 name=w_svndiff  line=' . winline() . ' buffer=' . winbufnr('%')
	return
	if s:diff_revision_array[0] != 'none'
		echon s:diff_revision_array[0]
		if s:diff_revision_array[1] != 'none'
			echon ' and ' . s:diff_revision_array[1]
		endif
		echo ' selected for diffing'
	else
		return
	endif
endfunc

func! <SID>OnSpacePressedSvn()
	let l = getline('.')
	if l !~ '^r\(\d\+\)'
		return
	else
		let revision = substitute(l, '^r\(\d\+\).*', '\1', "g")
		call <SID>AddRevisionToArray(revision)
		"call <SID>DisplaySelection()
		let s:filename = substitute(bufname('%'), '^SVN log \(\S\+\).*', '\1', 'g')
		"echo s:filename
	endif
endfunction

func! <SID>OnSpacePressedHg()
	let l = getline('.')
	let pat = '^changeset:\s\+\([0-9]\+\):.*'
	if l !~ pat
		return
	else
		let revision = substitute(l, pat, '\1', "g" )
		call <SID>AddRevisionToArray(revision)
		let s:filename = substitute(bufname('%'), '^HG log \(\S\+\).*', '\1', 'g')
	endif
endfunction

func! <SID>VCSLogPlug()
	if bufname('%') =~ '^SVN log'
		setlocal shiftwidth=3
		setlocal foldmethod=indent
		normal zm
		syn match W_NewLogLine '------------------------------------------------------------------------'
		hi link W_NewLogLine Comment
		syn match W_RevisionNumber '^r\d\+'
		hi link W_RevisionNumber Keyword
		silent! nmap <unique> <buffer> s :call <SID>OnSpacePressedSvn()<cr>
		return
	endif
	if bufname('%') =~ '^HG log'
		syn match W_RevisionNumber '\d\+:[0-9a-f]\+$'
		hi link W_RevisionNumber Keyword
		silent! nmap <unique> <buffer> s :call <SID>OnSpacePressedHg()<cr>
		return
	endif
endfunc

augroup VCSCommand
 au VCSCommand User VCSBufferCreated call <SID>VCSLogPlug()
 au VCSCommand User VCSBufferCreated silent! nmap <unique> <buffer> q :call <SID>OnBufferExit()<cr>
 au VCSCommand User VCSBufferCreated silent! nmap <unique> <buffer> d :call <SID>DoRevisionDiff()<cr>
 au VCSCommand User VCSBufferCreated silent! nmap <unique> <buffer> g :call <SID>Debug()<cr>
augroup END


" EoF
