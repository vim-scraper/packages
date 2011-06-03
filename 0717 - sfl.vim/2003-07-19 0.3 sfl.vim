" Vim syntax file
" Language:	SFL
" Maintainer:	Yoshihiro Iida <yiida@fw.ipsj.or.jp>
" Last Change:	Jul 20, 2003
" URL:          http://shimizu-lab.dt.u-tokai.ac.jp/
" Filenames:    *.sfl
" Version:	0.3

syn case ignore


" storage types
syn keyword	sflType		module declare par any alt stage state instruct task else circuit
syn keyword	sflInst		if state_name first_state input output instrin bidirect segment
syn keyword	sflInst		instrout instr_arg instrself reg reg_ws reg_wr sel mem sel_v bus bus_v
syn keyword	sflInst		stage_name goto call return generate relay finish
syn match	sflBracket	"[(){}=:;\.]"
syn match	sflInclude	"%i"
syn match	sflInclude	"%d"
syn match	sflLogic	"\^"
syn match	sflLogic	"&"
syn match	sflLogic	"/&"
syn match	sflLogic	"|"
syn match	sflLogic	"/|"
syn match	sflLogic	"@"
syn match	sflLogic	"/@"
syn match	sflType		"#"
syn match	sflInst		"=="
syn keyword	sflTodo		contained TODO

syn match	sflComment	"//.*" oneline contains=sflTodo
syntax region	sflComment	start="/\*" end="\*/" contains=sflTodo
syntax region	sflLogic	start="\"" end="\""

"syn match sflAny		"\s*[a-z_][a-z0-9_]*\s*:"he=e-1
"syn match sflAny		"\.*\s*[a-z_][a-z0-9_<>]*\s*:"he=e-1

syn match	sflIdentifier	"[a-z_][a-z0-9_]*"
syn match	sflSubmodule	"[A-Z_][A-Z0-9_]*\."he=e-1
syn match	sflLabel	"\<[A-Z][A-Z0-9_]*\>\C"

syn match	decNumber	"0\+[1-7]\=[\t\n$,; ]"
syn match	decNumber	"[0-9]\d*"
syn match	octNumber	"0[oO][0-7]\+" "hs=s+2
syn match	hexNumber	"0[xX][0-9a-fA-F]\+" "hs=s+2
syn match	binNumber	"0[bB][0-1]*" "hs=s+2

" Following contents are reserved name for SFL and Verilog
" Reserved name for Verilog
syn keyword	sflReserved	always and assign
syn keyword	sflReserved	begin buf bufif0 bufif1
syn keyword	sflReserved	case casex casez cmos
syn keyword	sflReserved	deassign default defparam disable
syn keyword	sflReserved	edge end endcase endmodule endfunction endprimitive endspecify endtable endtask event
syn keyword	sflReserved	for force forever function
syn keyword	sflReserved	highz0 highz1
syn keyword	sflReserved	ifnone initial inout integer
syn keyword	sflReserved	join
syn keyword	sflReserved	large
syn keyword	sflReserved	macromodule medium
syn keyword	sflReserved	nand negedge nmos nor not notif0 notif1
syn keyword	sflReserved	or
syn keyword	sflReserved	parameter pmos posedge primitive pull0 pull1 pullup pulldown
syn keyword	sflReserved	rcmos real realtime release repeat rnmos rpmos rtran rtranif0 rtranif1
syn keyword	sflReserved	scalared small specify specparam strong0 strong1 supply0 supply1
syn keyword	sflReserved	table time tran tranif0 tranif1 tri tri0 tri1 triand trior trireg
syn keyword	sflReserved	vectored
syn keyword	sflReserved	wait wand weak0 weak1 while wire wor
syn keyword	sflReserved	xnor xor
" Reserved name for SFL
syn keyword	sflReserved	p_reset m_clock s_clock b_clock VDD VSS scan_in scan_out sca_enb scan_lock t_adrs_

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_sfl_syn_inits")
  if version < 508
    let did_sfl_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
" The default methods for highlighting.  Can be overridden later
" Comment:	deep blue
" Identifier:	blue
" Number:	deep red
" Type:		green
" Repeat:	yellow
" Special:	violet
" Include:	violet
" Error:	red background
  HiLink sflComment	Comment
  HiLink sflInclude	Include

  HiLink hexNumber	Number
  HiLink decNumber	Number
  HiLink octNumber	Number
  HiLink binNumber	Number
  HiLink sflLogic	Number

  HiLink sflIdentifier	Identifier
  HiLink sflSubmodule	Special
  HiLink sflBracket	Default
  HiLink sflLabel	Special

"  HiLink sflSubmodule	Identifier
"  HiLink sflBracket	Special

  HiLink sflTodo	Todo
  HiLink sflType	Type
  HiLink sflInst	Repeat
  HiLink sflReserved	Error
  HiLink sflError	Error
"  HiLink sflAny		Repeat

" My default color overrides:
" hi sflSpecialComment ctermfg=red
" hi sflIdentifier ctermfg=lightcyan
" hi sflType ctermbg=black ctermfg=brown

  delcommand HiLink
endif

let b:current_syntax = "sfl"

" vim: ts=8
