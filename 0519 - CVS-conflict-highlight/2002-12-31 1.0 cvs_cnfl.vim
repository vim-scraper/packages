" Vim syntax file
" Language:	CVS conflict file
" Maintainer:	Alex Jakushev (Alex.Jakushev@kemek.lt)
" Last Change:	2002.12.30

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn match cvsStart "<<<*"
syn match cvsStartLineErr "^<<<<<<< .*" contained
syn match cvsStartLine "^<<<<<<< .*" contains=cvsStart contained

syn match cvsEnd ">>>*"
syn match cvsEndLineErr "^>>>>>>> .*"
syn match cvsEndLine "^>>>>>>> .*" contains=cvsEnd contained

syn match cvsMiddleErr "^=======$" contained
syn match cvsMiddle "^=======$" containedin=cvsSrvVer

syn region cvsThisVer start="^<<<<<<< " end="^=======$"me=s-2 contains=cvsStartLine,cvsEndLineErr
syn sync match SyncThisVer grouphere cvsThisVer "^<<<<<<< "

syn region cvsSrvVer start="^=======$" end="^>>>>>>> .*" contains=cvsMiddle,cvsEndLine,cvsStartLineErr keepend
syn sync match SyncThisVer grouphere cvsSrvVer "^=======$"


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_cvs_cnfl_syn_inits")
	if version < 508
		let did_cvs_cnfl_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink cvsStartLine	Special
	HiLink cvsEndLine	Label
	HiLink cvsStart		WarningMsg
	HiLink cvsEnd		WarningMsg
	HiLink cvsMiddle	WarningMsg
	HiLink cvsThisVer	DiffAdd
	HiLink cvsSrvVer	DiffChange
	HiLink cvsEndLineErr	Error
	HiLink cvsStartLineErr	Error
	HiLink cvsMiddleErr	Error

	delcommand HiLink
endif

let b:current_syntax = "cvs_cnfl"
