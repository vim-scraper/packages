" Vim syntax file
" Language:	Nexus file format with some reserved words for MrBayes
" Maintainer:	Luis Carvalho <lexcarvalho@hotmail.com>
" Last Change:	2004 Apr 14

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Main reserved words
syn keyword	nBlock		begin end
syn keyword	nBatch		dimensions format matrix translate tree
" Commands
syn keyword	nCommand	charset charstat comparetree ctype
syn keyword	nCommand	databreaks delete deroot exclude
syn keyword	nCommand	execute include link log
syn keyword	nCommand	lset mcmc mcmcp outgroup
syn keyword	nCommand	pairs partition plot prset
syn keyword	nCommand	props report restore root
syn keyword	nCommand	set showmatrix showmodel showtree
syn keyword	nCommand	sump sumt taxastat taxset
syn keyword	nCommand	unlink usertree
" Parameters
syn keyword	nParameter	unordered ordered irreversible
syn keyword	nParameter	tratio revmat omega statefreq
syn keyword	nParameter	shape pinvar correlation switchrates
syn keyword	nParameter	brlens topology speciationrates
syn keyword	nParameter	extinctionrates theta
syn keyword	nParameter	start stop append replace
syn keyword	nParameter	nucmodel nst code rates
syn keyword	nParameter	ngammacat nbetacat omegavar covarion
syn keyword	nParameter	coding parsmodel
syn keyword	nParameter	seed ngen samplefreq swapfreq
syn keyword	nParameter	printfreq nchains temp reweight
syn keyword	nParameter	burnin startingtree nperts savebrlens
syn keyword	nParameter	parameter match
syn keyword	nParameter	tratiopr revmatpr aamodelpr omegapr
syn keyword	nParameter	ny98omega1pr ny98omega3pr m3omegapr
syn keyword	nParameter	codoncatfreqs statefreqpr ratepr shapepr
syn keyword	nParameter	ratecorrpr pinvarpr covswitchpr
syn keyword	nParameter	symmetricbetapr topologypr brlenspr
syn keyword	nParameter	speciationpr extinctionpr sampleprob thetapr
syn keyword	nParameter	ratemult autoclose nowarnings
syn keyword	nParameter	displaygeq contype showtreeprobs
" Options
syn keyword	nOptionsR	4by4 doublet codon
syn keyword	nOptionsR	universal vertmt mycoplasma
syn keyword	nOptionsR	yeast ciliates metmt
syn keyword	nOptionsR	equal gamma propinv invgamma adgamma
syn keyword	nOptionsR	noabsencesites nopresencesites
syn keyword	nOptionsR	random user perfect consistentwith
syn keyword	nOptionsR	scaled ratio dir
syn keyword	nOptionsR	halfcompat allcompat
syn keyword	nOptionsD	fixed variable beta dirichlet
syn keyword	nOptionsD	uniform exponential constraints
syn keyword	nOptionsD	unconstrained clock
syn keyword	nOptionsD	dna rna protein restriction standard
syn keyword	nOptionsD	continuous mixed
syn keyword	nOptionsB	ntax nchar datatype interleave
syn keyword	nOptionsB	gap missing matchchar
" Categorical
syn keyword	nCategorical	yes no all

syn keyword	nTodo		contained TODO FIXME XXX

" nCommentGroup allows adding matches for special things in comments
syn cluster	nCommentGroup	contains=nTodo


"catch errors caused by wrong parenthesis and brackets
syn cluster	nParenGroup	contains=nParenError,@nCommentGroup,nCommentStartError,nNumber,nFloat
syn region	nParen		transparent start='(' end=')' contains=ALLBUT,@nParenGroup,nErrInBracket
syn match	nParenError	display "[\])]"
syn match	nErrInParen	display contained "[\]{}]"
syn region	nBracket	transparent start='\[' end=']' contains=ALLBUT,@nParenGroup,nErrInParen
syn match	nErrInBracket	display contained "[);{}]"

"integer or floating point number
syn case ignore
syn match	nNumbersCom	display transparent "\<\d\|\.\d" contains=nNumber,nFloat
syn match	nNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
syn match	nNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
"floating point number, with dot, optional exponent
syn match	nFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	nFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	nFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"

"comments
syn region	nComment	start="\[" end="\]" contains=@nCommentGroup,nCommentStartError
syntax match	nCommentError	display "\]"
syntax match	nCommentStartError display "\["me=e-1 contained


" Define the default highlighting
if version >= 508 || !exists("did_nexus_syn_inits")
  if version < 508
    let did_nexus_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

	HiLink nBlock			Repeat
	HiLink nBatch			Conditional
	HiLink nCommand			Statement
	HiLink nParameter		Type
	HiLink nOptionsR		String
	HiLink nOptionsD		Character
	HiLink nOptionsB		Include
	HiLink nCategorical		Constant

	HiLink nNumber			Number
	HiLink nNumber			Number
	HiLink nFloat			Float
	HiLink nParenError		nError
	HiLink nErrInParen		nError
	HiLink nErrInBracket		nError
	HiLink nCommentError		nError
	HiLink nCommentStartError	nError
	HiLink nError			Error
	HiLink nCommentStart		nComment
	HiLink nComment			Comment
	HiLink nTodo			Todo

	delcommand HiLink
endif

let b:current_syntax = "nexus"

" vim: ts=8
