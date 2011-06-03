set foldcolumn=3
" 
" syntax for aegis rpt.

" using c will disable comments split over chunks.
" runtime! syntax/c.vim
"
syn region nwChunkFold start="<<.*>>=$" end="^@" transparent fold

"syn match nwChunk "<<.*>>" 
syn sync fromstart
set foldmethod=syntax
" folding without mouse is not so funny
set mouse=a

delfunction NwFoldText
function NwFoldText()
	let line = (v:foldend-v:foldstart)." lines: "
	let startln = getline(v:foldstart)
	let startln = strpart(startln,0,strlen(startln)-1)
	let endln = getline(v:foldend)
	if strlen(endln)>2
		let endln = " declares: " . strpart(endln,2)
	else
		let endln = ""
	endif
	return line . startln . endln
endfunction
set foldtext=NwFoldText()

if version >= 508 || !exists("did_abaqus_syn_inits")
    if version < 508
        let did_abaqus_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif
  
	HiLink nwChunk PreProc

	delcommand HiLink
endif
" to syncronize highlighting when scrolling
syntax sync minlines=100

let b:current_syntax = "noweb"  
