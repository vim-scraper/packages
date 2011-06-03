" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
syntax/upf.vim	[[[1
229
" Vim syntax file
" Language:     Unified Power Format (UPF) standard version 1.0
" Maintainer:   
" Last Update:  May 20 2008
" Filenames:    *.upf *.mv
" URL:          
"
" UPF provides the ability for electronic systems to be designed with power as a key consideration early in the process 
" of chip design
"
" History:
"       Aditya Kher created version for UPF 1.0
"

syn clear

" A bunch of useful Vera keywords
syn keyword upfCommand add_domain_elements 
syn keyword upfCommand add_port_state 
syn keyword upfCommand add_pst_state 
syn keyword upfCommand bind_checker
syn keyword upfCommand connect_supply_net  
syn keyword upfCommand create_hdl2upf_vct
syn keyword upfCommand create_power_domain
syn keyword upfCommand create_power_switch
syn keyword upfCommand create_pst
syn keyword upfCommand create_supply_net
syn keyword upfCommand create_supply_port
syn keyword upfCommand create_upf2hdl
syn keyword upfCommand get_supply_net
syn keyword upfCommand map_isolation_cell
syn keyword upfCommand map_level_shifter_cell
syn keyword upfCommand map_power_switch
syn keyword upfCommand map_retention_cell
syn keyword upfCommand merge_power_domains
syn keyword upfCommand name_format
syn keyword upfCommand new_domain_name
syn keyword upfCommand save_upf
syn keyword upfCommand set_domain_supply_net
syn keyword upfCommand set_isolation
syn keyword upfCommand set_isolation_control
syn keyword upfCommand set_pin_related_supply 
syn keyword upfCommand set_power_switch
syn keyword upfCommand set_retention
syn keyword upfCommand set_retention_control
syn keyword upfCommand upf_version 

syn keyword upfParameter -ack_delay
syn keyword upfParameter -ack_port
syn keyword upfParameter -all_equivalent
syn keyword upfParameter -applies_to
syn keyword upfParameter -assert_r_mutex
syn keyword upfParameter -assert_rs_mutex
syn keyword upfParameter -assert_s_mutex
syn keyword upfParameter -cells
syn keyword upfParameter	-cells 
syn keyword upfParameter -clamp_value
syn keyword upfParameter -control_port
syn keyword upfParameter -direction
syn keyword upfParameter -domain
syn keyword upfParameter -elements
syn keyword upfParameter -error_state
syn keyword upfParameter -hdl_type
syn keyword upfParameter -include_scope
syn keyword upfParameter -input_supply_port
syn keyword upfParameter -isolation_ground_net
syn keyword upfParameter -isolation_power_net
syn keyword upfParameter -isolation_prefix
syn keyword upfParameter -isolation_sense
syn keyword upfParameter -isolation_signal
syn keyword upfParameter -isolation_suffix
syn keyword upfParameter -level_shift_prefix
syn keyword upfParameter -level_shift_suffix
syn keyword upfParameter -lib_cells
syn keyword upfParameter -lib_cell_type
syn keyword upfParameter -lib_model_name
syn keyword upfParameter -location
syn keyword upfParameter -module
syn keyword upfParameter -module
syn keyword upfParameter -no_isolation
syn keyword upfParameter -no_shift
syn keyword upfParameter -off_state
syn keyword upfParameter -on_partial_state
syn keyword upfParameter -on_state
syn keyword upfParameter -output_supply_port
syn keyword upfParameter -pg_type
syn keyword upfParameter -pg_type 
syn keyword upfParameter -pins
syn keyword upfParameter -pins 
syn keyword upfParameter -port
syn keyword upfParameter -ports
syn keyword upfParameter -power_domains
syn keyword upfParameter -primary_ground_net
syn keyword upfParameter -primary_power_net
syn keyword upfParameter -pst
syn keyword upfParameter -rail_connection
syn keyword upfParameter -rail_connection 
syn keyword upfParameter -related_ground_pin
syn keyword upfParameter -related_power_pin
syn keyword upfParameter -resolve
syn keyword upfParameter -restore_signal
syn keyword upfParameter -retention_ground_net
syn keyword upfParameter -retention_power_net
syn keyword upfParameter -reuse
syn keyword upfParameter -rule
syn keyword upfParameter -save_signal
syn keyword upfParameter -scope
syn keyword upfParameter -state
syn keyword upfParameter -supplies
syn keyword upfParameter -table
syn keyword upfParameter -threshold
syn keyword upfParameter -vct
syn keyword upfParameter -version

syn keyword upfStatement		automatic    
syn keyword upfStatement		both  
syn keyword upfStatement  fanout
syn keyword upfStatement  high
syn keyword upfStatement		high_to_low
syn keyword upfStatement  in
syn keyword upfStatement  inputs
syn keyword upfStatement		latch 
syn keyword upfStatement		low 
syn keyword upfStatement  low_to_high
syn keyword upfStatement		max 
syn keyword upfStatement		min 
syn keyword upfStatement  name
syn keyword upfStatement		negedge   
syn keyword upfStatement		nom 
syn keyword upfStatement		off 
syn keyword upfStatement		one_hot
syn keyword upfStatement		out  
syn keyword upfStatement		outputs
syn keyword upfStatement		parallel  
syn keyword upfStatement		parent 
syn keyword upfStatement		posedge 
syn keyword upfStatement  self 
syn keyword upfStatement		sibling 
syn keyword upfStatement		SV 
syn keyword upfStatement		typename  
syn keyword upfStatement  unresolved
syn keyword upfStatement  vhdl 
syn keyword upfStatement		vlog 

"syn keyword upfTodo contained TODO

"syn match upfCommand "-" contains=upfCommand
setlocal iskeyword+=-
syn keyword upfCommand when-not


syn match   upfOperator "[&|~><!)(*#%@+/=?:;}{,.\^\-\[\]]"

"syn region  upfComment start="/\*" end="\*/" contains=upfTodo
"syn match   upfComment "#.*" contains=upfTodo

"syn match   upfGlobal "`[a-zA-Z0-9_]\+\>"
"syn match   upfGlobal "$[a-zA-Z0-9_]\+\>"

syn match   upfConstant "\<[A-Z][A-Z0-9_]\+\>"

syn match   upfNumber "\(\<[0-9]\+\|\)'[bdh][0-9a-fxzA-FXZ_]\+\>"
syn match   upfNumber "\<[+-]\=[0-9]\+\>"

syn region  upfString start=+"+  end=+"+


"copied these from the c.vim file and modified
syn region upfPreCondit start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\|elif\>\|else\>\|endif\>\)" skip="\\$" end="$" contains=upfComment,upfString,upfCharacter,upfNumber
syn region upfIncluded contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match upfIncluded contained "<[^>]*>"
syn match upfInclude           "^\s*#\s*include\>\s*["<]" contains=upfIncluded
syn region upfDefine           start="^\s*#\s*\(define\>\|undef\>\)" skip="\\$" end="$" contains=ALLBUT,upfPreCondit,upfIncluded,upfInclude,upfDefine,upfOperator
syn region upfPreProc start="^\s*#\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" contains=ALLBUT,upfPreCondit,upfIncluded,upfInclude,upfDefine


""catch errors caused by wrong parenthesis and brackets
"" also accept <% for {, %> for }, <: for [ and :> for ] (C99)
"" But avoid matching <::.
syn cluster	upfParenGroup	contains=upfParenError
if exists("upf_no_bracket_error")
  syn region	upfParen		transparent start='(' end=')' contains=ALLBUT,@upfParenGroup,upfBracket,@Spell
  syn match	upfParenError	display ")"
  syn match	upfErrInParen	display contained "[{}]\|<%\|%>"
else
  syn region	upfParen		transparent start='(' end=')' contains=ALLBUT,@upfParenGroup,upfErrInBracket,@Spell
  syn match	upfParenError	display "[\])]"
  syn match	upfErrInParen	display contained "[\]{}]\|<%\|%>"
  syn region	upfBracket	transparent start='\[\|<::\@!' end=']\|:>' contains=ALLBUT,@upfParenGroup,upfErrInParen,@Spell
  syn match	upfErrInBracket	display contained "[);{}]\|<%\|%>"
endif

:runtime! syntax/tcl.vim

"Modify the following as needed.  The trade-off is performance versus
"functionality.
syn sync lines=75

if !exists("did_upf_syntax_inits")
  let did_upf_syntax_inits = 1
 " The default methods for highlighting.  Can be overridden later

		hi link upfCommand           Type 
		hi link upfParameter         String
  hi link upfStatement         String
		
  hi link upfRepeat            Repeat
  hi link upfString            String
  hi link upfTodo              Todo
  hi link upfCharacter         Character
  hi link upfConditional       Conditional

  hi link upfDefine            Macro
  hi link upfInclude           Include
  hi link upfConstant          Constant
  hi link upfLabel             PreCondit
  hi link upfPreCondit         PreCondit
  hi link upfNumber            Special
"  hi link upfComment           Comment
		
  hi link upfParenError		      Error
  hi link upfErrInParen		      Error
  hi link upfErrInBracket     	Error
		
endif

let b:current_syntax = "upf"


