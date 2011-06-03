" Vim syntax file
" Language:	LINDO
" Maintainer:	Juan M. Cataldo <jcataldo@inf.utfsm.cl>
" Last change:	2004 Oct 27

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn match	lindoComment	"^\s*!.*$"
syn match	lindoNumber	"\<\d\+\>"
syn match	lindoIdentifier "^\s*[a-zA-z_0-9]*)"
syn keyword	lindoStatement	ALT Alt alt
syn keyword	lindoStatement	APPC Appc appc
syn keyword	lindoStatement	BAT Bat bat
syn keyword	lindoStatement	BIP Bip bip
syn keyword	lindoStatement	BPIC Bpic bpic
syn keyword	lindoStatement	BUG Bug bug
syn keyword	lindoStatement	CAT Cat cat
syn keyword	lindoStatement	COM Com com
syn keyword	lindoStatement	CPRI Cpri cpri
syn keyword	lindoStatement	DATE Date date
syn keyword	lindoStatement	DEB Deb deb
syn keyword	lindoStatement	DEL Del del
syn keyword	lindoStatement	DIVE Dive dive
syn keyword	lindoStatement	DMPS Dmps dmps
syn keyword	lindoStatement	END End end
syn keyword	lindoStatement	ENGR Engr engr
syn keyword	lindoStatement	EXT Ext ext
syn keyword	lindoStatement	FBR Fbr fbr
syn keyword	lindoStatement	FBS Fbs fbs
syn keyword	lindoStatement	FINS Fins fins
syn keyword	lindoStatement	FPUN Fpun fpun
syn keyword	lindoStatement	FREE Free free
syn keyword	lindoStatement	GIN Gin gin
syn keyword	lindoStatement	GLEX Glex glex
syn keyword	lindoStatement	GO Go go
syn keyword	lindoStatement	HELP Help help
syn keyword	lindoStatement	INT Int int
syn keyword	lindoStatement	INV Inv inv
syn keyword	lindoStatement	IPTOL Iptol iptol
syn keyword	lindoStatement	LEAV Leav leav
syn keyword	lindoStatement	LKLG Lklg lklg
syn keyword	lindoStatement	LOCAL Local local
syn keyword	lindoStatement	LOOK Look look
syn keyword	lindoStatement	MAX Max max
syn keyword	lindoStatement	MIN Min min
syn keyword	lindoStatement	NONZ Nonz nonz
syn keyword	lindoStatement	PAGE Page page
syn keyword	lindoStatement	PARA Para para
syn keyword	lindoStatement	PAUS Paus paus
syn keyword	lindoStatement	PIC Pic pic
syn keyword	lindoStatement	PIV Piv piv
syn keyword	lindoStatement	POSD Posd posd
syn keyword	lindoStatement	PPIC Ppic ppic
syn keyword	lindoStatement	QCP Qcp qcp
syn keyword	lindoStatement	QUIT Quit quit
syn keyword	lindoStatement	RANGE Range range
syn keyword	lindoStatement	RDBC Rdbc rdbc
syn keyword	lindoStatement	RETR Retr retr
syn keyword	lindoStatement	RMPS Rmps rmps
syn keyword	lindoStatement	RPRI Rpri rpri
syn keyword	lindoStatement	RVRT Rvrt rvrt
syn keyword	lindoStatement	SAVE Save save
syn keyword	lindoStatement	SDBC Sdbc sdbc
syn keyword	lindoStatement	SET Set set
syn keyword	lindoStatement	SHOC Shoc shoc
syn keyword	lindoStatement	SLB Slb slb
syn keyword	lindoStatement	SMPN Smpn smpn
syn keyword	lindoStatement	SMPS Smps smps
syn keyword	lindoStatement	SOLU Solu solu
syn keyword	lindoStatement	ST St st
syn keyword	lindoStatement	STAT Stat stat
syn keyword	lindoStatement	SUB Sub sub
syn keyword	lindoStatement	TABL Tabl tabl
syn keyword	lindoStatement	TAKE Take take
syn keyword	lindoStatement	TERS Ters ters
syn keyword	lindoStatement	TIME Time time
syn keyword	lindoStatement	TITAN Titan titan
syn keyword	lindoStatement	TITL Titl titl
syn keyword	lindoStatement	USER User user
syn keyword	lindoStatement	VERB Verb verb
syn keyword	lindoStatement	WIDTH Width width

hi def link lindoComment Comment
hi def link lindoNumber Number
hi def link lindoIdentifier Identifier
hi def link lindoStatement Statement

let b:current_syntax = "lindo"

" vim: ts=8: noexpandtab
