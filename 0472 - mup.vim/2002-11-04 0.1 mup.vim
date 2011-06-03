" Vim syntax file
" Language:	MUP
" Maintainer:	CHUA Soon Wah <soonwah_yh@yahoo.com>
" Last Change:	Tues 05 Nov 2002
" Remark:	Music publication format by Arkkra Enterprise <http://www.arkkra.com> 
" 		This syntax file is not created/maintained by Arkkra Enterprise

" $Id:$
" remove old syntax
syn clear

syn keyword MUPContext		score staff voice music grids
syn keyword MUPContext		header header2
syn keyword MUPContext		footer footer2
syn keyword MUPParameter	title print
syn keyword MUPParameter	aboveorder addtranspose
syn keyword MUPParameter	barstyle beamslope beamstyle
syn keyword MUPParameter	beloworder betweenorder botmargin brace bracket
syn keyword MUPParameter	cancelkey chorddist clef defoct dist division
syn keyword MUPParameter	dyndist endingstyle firstpage
syn keyword MUPParameter	font fontfamily gridfret gridsatend
syn keyword MUPParameter	gridswhereused gridscale key label label2 leftmargin
syn keyword MUPParameter	lyricsalign lyricsfont lyricsfontfamily lyricssize
syn keyword MUPParameter	measnum numbermrpt ontheline packexp packfact pad pagewidth
syn keyword MUPParameter	panelsperpage pedstyle printmultnum
syn keyword MUPParameter	rehstyle release restcombine rightmargin
syn keyword MUPParameter	scale scorepad scoresep size
syn keyword MUPParameter	stafflines staffpad staffs staffscale staffsep
syn keyword MUPParameter	sylposition tabwhitebox time timeunit
syn keyword MUPParameter	topmargin transpose units visible vscheme warn

syn keyword MUPStatement	define undef include fontfile

syn keyword MUPValue		mussym octave dyn othertext chord lyrics ending reh
syn keyword MUPValue		up down y n
syn keyword MUPValue		treble treble8 8treble frenchviolin soprano
syn keyword MUPValue		mezzosoprano alto tenor baritone bass
syn keyword MUPValue		top barred grouped
syn keyword MUPValue		rom bold ital boldital
syn keyword MUPValue		avantgarde bookman courier helvetica newcentury palatino times
syn match MUPValue		"= *[0-9]\+"hs=s+1
syn match MUPValue		"[0-9]\=[0-9]/[1\|3\|6]\=[1\|2\|4\|8\|6]"
syn region MUPValue		start=/= *[0-9]\+.*[0-9]*[, *[0-9]\+]*/hs=s+1 end=/$/ contains=MUPComment keepend
syn match MUPValue		"= *[0-7][#\|&]\+ major\|= *[0-7][#\|&]\+ minor"hs=s+1
syn keyword MUPValue		line pedstar alt pedstar
syn keyword MUPValue		boxed circled plain
syn keyword MUPValue		com cut
syn keyword MUPValue		inches cm
syn keyword MUPValue		bar dblbar repeatstart repeatend repeatboth
syn keyword MUPValue		endbar invisbar restart
syn keyword MUPValue		full grace cue xnote diam with pad slash
syn keyword MUPValue		len dist
syn keyword MUPValue		above below
syn keyword MUPValue		left right center print
syn keyword MUPValue		midi phrase pedal
syn keyword MUPConditional	ifdef ifndef else endif

syn match MUPComment		"//.*$"
syn match MUPStaff		"[[0-9] ]*[0-9]*[\-|\,]*[0-9]\+\:"
syn match MUPStaff		"[0-9]*,*[0-9]*\-*[0-9]\+\:"

syn region  MUPString		matchgroup=Normal start=+"+  end=+"+ skip=+\\\\\|\\'+

hi link MUPContext		Statement
hi link MUPStatement		Statement
hi link MUPComment		Comment
hi link MUPString		String
hi link MUPConditional		Conditional
hi link MUPParameter		Identifier
hi link MUPValue		Special
hi link MUPStaff		Label
hi Value gui=italic
hi Staff gui=bold

let b:current_syntax = "MUP"

" vim: ts=8
