" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
syntax/sv.vim	[[[1
177
" Vim syntax file
" Language:	SV
" Maintainer:  Aditya Kher http://kher.org

" Last Update: 6th Nov 2003


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" A bunch of useful SV keywords
 syn keyword SVStatement  alias always always_comb 
 syn keyword SVStatement  always_ff always_latch 
 syn keyword SVStatement  and assert 
 syn keyword SVStatement  assert_strobe assign 
 syn keyword SVStatement  automatic before 
 syn keyword SVStatement  begin bind 
 syn keyword SVStatement  bit break 
 syn keyword SVStatement  buf bufif0 
 syn keyword SVStatement  bufif1 byte 
 syn keyword SVStatement  case casex 
 syn keyword SVStatement  casez cell 
 syn keyword SVStatement  chandle class 
 syn keyword SVStatement  clocking cmos 
 syn keyword SVStatement  config const 
 syn keyword SVStatement  constraint context 
 syn keyword SVStatement  continue cover 
 syn keyword SVStatement  deassign default 
 syn keyword SVStatement  defparam design 
 syn keyword SVStatement  disable dist 
 syn keyword SVStatement  do edge 
 syn keyword SVStatement  else end 
 syn keyword SVStatement  endcase endclass 
 syn keyword SVStatement  endclocking endconfig 
 syn keyword SVStatement  endfunction endgenerate 
 syn keyword SVStatement  endinterface endmodule 
 syn keyword SVStatement  endprimitive endprogram 
 syn keyword SVStatement  endproperty endspecify 
 syn keyword SVStatement  endsequence endtable 
 syn keyword SVStatement  endtask enum 
 syn keyword SVStatement  event export 
 syn keyword SVStatement  extends extern 
 syn keyword SVStatement  final first_match 
 syn keyword SVStatement  for force 
 syn keyword SVStatement  forever fork 
 syn keyword SVStatement  fork join function 
 syn keyword SVStatement  generate genvar 
 syn keyword SVStatement  highz0 highz1 
 syn keyword SVStatement  if iff 
 syn keyword SVStatement  ifnone import 
 syn keyword SVStatement  incdir include 
 syn keyword SVStatement  initial inout 
 syn keyword SVStatement  input inside 
 syn keyword SVStatement  instance int 
 syn keyword SVStatement  integer interface 
 syn keyword SVStatement  intersect join 
 syn keyword SVStatement  join_any join_none 
 syn keyword SVStatement  large liblist 
 syn keyword SVStatement  library local 
 syn keyword SVStatement  localparam logic 
 syn keyword SVStatement  longint macromodule 
 syn keyword SVStatement  medium modport 
 syn keyword SVStatement  module nand 
 syn keyword SVStatement  negedge new 
 syn keyword SVStatement  nmos nor 
 syn keyword SVStatement  noshowcancelled not 
 syn keyword SVStatement  notif0 notif1 
 syn keyword SVStatement  null or 
 syn keyword SVStatement  output packed 
 syn keyword SVStatement  parameter pmos 
 syn keyword SVStatement  posedge primitive 
 syn keyword SVStatement  priority program 
 syn keyword SVStatement  property protected 
 syn keyword SVStatement  pull0 pull1 
 syn keyword SVStatement  pulldown pullup 
 syn keyword SVStatement  pulsestyle_onevent pulsestyle_ondetect 
 syn keyword SVStatement  pure rand 
 syn keyword SVStatement  randc rcmos 
 syn keyword SVStatement  ref real 
 syn keyword SVStatement  realtime reg 
 syn keyword SVStatement  release repeat 
 syn keyword SVStatement  return rnmos 
 syn keyword SVStatement  rpmos rtran 
 syn keyword SVStatement  rtranif0 rtranif1 
 syn keyword SVStatement  scalared sequence 
 syn keyword SVStatement  shortint shortreal 
 syn keyword SVStatement  showcancelled signed 
 syn keyword SVStatement  small solve 
 syn keyword SVStatement  specify specparam 
 syn keyword SVStatement  static string 
 syn keyword SVStatement  strong0 strong1 
 syn keyword SVStatement  struct super 
 syn keyword SVStatement  supply0 supply1 
 syn keyword SVStatement  table task 
 syn keyword SVStatement  this throughout 
 syn keyword SVStatement  time timeprecision 
 syn keyword SVStatement  timeunit tran 
 syn keyword SVStatement  tranif0 tranif1 
 syn keyword SVStatement  tri tri0 
 syn keyword SVStatement  tri1 triand 
 syn keyword SVStatement  trior trireg 
 syn keyword SVStatement  type typedef 
 syn keyword SVStatement  union unique 
 syn keyword SVStatement  unsigned use 
 syn keyword SVStatement  var vectored 
 syn keyword SVStatement  virtual void 
 syn keyword SVStatement  wait wait_order 
 syn keyword SVStatement  wand weak0 
 syn keyword SVStatement  weak1 while 
 syn keyword SVStatement  wire with 
 syn keyword SVStatement  within wor 
 syn keyword SVStatement  xnor xor

" syn keyword SVLabel       begin end fork join
" syn keyword SVConditional if else case casex casez default endcase
" syn keyword SVRepeat      forever repeat while for

syn keyword SVTodo contained TODO

syn match   SVOperator "[&|~><!)(*#%@+/=?:;}{,.\^\-\[\]]"

syn region  SVComment start="/\*" end="\*/" contains=SVTodo
syn match   SVComment "//.*" oneline

syn match   SVGlobal "`[a-zA-Z0-9_]\+\>"
syn match   SVGlobal "$[a-zA-Z0-9_]\+\>"

syn match   SVConstant "\<[A-Z][A-Z0-9_]\+\>"

syn match   SVNumber "\(\<\d\+\|\)'[bB]\s*[0-1_xXzZ?]\+\>"
syn match   SVNumber "\(\<\d\+\|\)'[oO]\s*[0-7_xXzZ?]\+\>"
syn match   SVNumber "\(\<\d\+\|\)'[dD]\s*[0-9_xXzZ?]\+\>"
syn match   SVNumber "\(\<\d\+\|\)'[hH]\s*[0-9a-fA-F_xXzZ?]\+\>"
syn match   SVNumber "\<[+-]\=[0-9_]\+\(\.[0-9_]*\|\)\(e[0-9_]*\|\)\>"

syn region  SVString start=+"+  end=+"+

" Directives
syn match   SVDirective   "//\s*synopsys\>.*$"
syn region  SVDirective   start="/\*\s*synopsys\>" end="\*/"
syn region  SVDirective   start="//\s*synopsys dc_script_begin\>" end="//\s*synopsys dc_script_end\>"

syn match   SVDirective   "//\s*\$s\>.*$"
syn region  SVDirective   start="/\*\s*\$s\>" end="\*/"
syn region  SVDirective   start="//\s*\$s dc_script_begin\>" end="//\s*\$s dc_script_end\>"

"Modify the following as needed.  The trade-off is performance versus
"functionality.
syn sync lines=50

if !exists("did_SV_syntax_inits")
  let did_SV_syntax_inits = 1
 " The default methods for highlighting.  Can be overridden later

  hi link SVCharacter       Character
  hi link SVConditional     Conditional
  hi link SVRepeat          Repeat
  hi link SVString          String
  hi link SVTodo            Todo
  hi link SVComment         Comment
  hi link SVConstant        Constant
  hi link SVLabel           Label
  hi link SVNumber          Number
  hi link SVOperator        Special
  hi link SVStatement       Statement
  hi link SVGlobal          Define
  hi link SVDirective       SpecialComment
endif

let b:current_syntax = "SV"

" vim: ts=8
