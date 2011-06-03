" Vim syntax file
" Language:	Vera, updated for 5.0
" Maintainer:	Beth Leonard	(work)<beth_leonard@agilent.com>
"				(home)<beth@slimy.com> 
" Last Update:  June 14, 2002
" Filenames:	*.vr *.vrh
" URL:		http://www.slimy.com/~beth/vim/syntax/vera.vim
"
" Vera is a Hardware Verification Language sold by Synopsys.
"
" History:
" 	Mark Madsen leveraged this from the Verilog syntax file 
"	Beth Leonard added some enhancements
"	Dave Eggum added some keywords and updates
"		The most notable change is that veraStatement is now linked
"		to 'Type' instead of 'Statement'.  To override, modify your
"		.vimrc to include:
"			hi link veraStatement Statement
"	Beth Leonard updated it for Vera 5:
"		Added the keywords/functions that were deprecated ages ago, in
"		addition to adding them to the deprecated list.

syn clear

" A bunch of useful Vera keywords
syn keyword veraStatement	all any assoc_index
syn keyword veraStatement	assoc_size async bad_state
syn keyword veraStatement	bad_trans begin big_endian
syn keyword veraStatement	bind bind_var bit
syn keyword veraStatement	bit_normal bit_reverse break
syn keyword veraStatement	breakpoint case casex
syn keyword veraStatement	casez class CLOCK continue
syn keyword veraStatement	coverage_block coverage_def coverage_depth
syn keyword veraStatement	coverage_goal coverage_option coverage_val
syn keyword veraStatement	default depth dist
syn keyword veraStatement	else end enum
syn keyword veraStatement	event export extends
syn keyword veraStatement	extern for fork
syn keyword veraStatement	function hdl_node hdl_task
syn keyword veraStatement	if illegal_self_transition illegal_state
syn keyword veraStatement	illegal_transation in !in
syn keyword veraStatement	inout input integer
syn keyword veraStatement	interface join little_endian
syn keyword veraStatement	local m_bad_state m_bad_trans
syn keyword veraStatement	m_state m_trans negedge
syn keyword veraStatement	newcov non_rand none
syn keyword veraStatement	not null or
syn keyword veraStatement	output packed port
syn keyword veraStatement	posedge prod prodget
syn keyword veraStatement	prodset program protected
syn keyword veraStatement	public rand randc
syn keyword veraStatement	randcase randseq reg
syn keyword veraStatement	repeat return shadow
syn keyword veraStatement	soft state static
syn keyword veraStatement	string super task
syn keyword veraStatement	terminate this trans
syn keyword veraStatement	typedef unpacked var
syn keyword veraStatement	vca vector verilog_node
syn keyword veraStatement	verilog_task vhdl_node vhdl_task
syn keyword veraStatement	virtual void while
syn keyword veraStatement	wildcard with 

" bein/end and fork/join stand out a little more than normal statements
syn keyword veraLabel           begin end fork join
syn keyword veraConditional     if else case default 
syn keyword veraRepeat          repeat while for

" predefined vera tasks
" String methods and coverage methods are not listed in the vera.vim file
syn keyword veraTask		alloc assoc_index boundary
syn keyword veraTask		call_func call_task cast_assign
syn keyword veraTask		close_conn constraint constraint_mode
syn keyword veraTask		delay error error_mode
syn keyword veraTask		exit fclose feof
syn keyword veraTask		ferror fflush flag
syn keyword veraTask		fopen fprintf freadb
syn keyword veraTask		freadh freadstr get_bind
syn keyword veraTask		get_bind_id get_conn_err get_cycle
syn keyword veraTask		get_memsize get_plus_arg get_systime
syn keyword veraTask		get_time lock_file mailbox_get
syn keyword veraTask		mailbox_put mailbox_receive mailbox_send
syn keyword veraTask		make_client make_server new
syn keyword veraTask		os_command pack post_boundary
syn keyword veraTask		post_pack post_randomize post_unpack
syn keyword veraTask		pre_boundary pre_pack pre_randomize
syn keyword veraTask		pre_unpack printf psprintf
syn keyword veraTask		query query_str query_x
syn keyword veraTask		rand48 rand_mode random
syn keyword veraTask		randomize region_enter region_exit
syn keyword veraTask		rewind save semaphore_get
syn keyword veraTask		semaphore_put signal_connect simwave_plot
syn keyword veraTask		sprintf sscanf stop
syn keyword veraTask		suspend_thread sync timeout
syn keyword veraTask		trace trigger unit_delay
syn keyword veraTask		unit_delay unlock_file unpack
syn keyword veraTask		up_connections urand48 urandom
syn keyword veraTask		vca vsv_call_func vsv_call_task
syn keyword veraTask		vsv_close_conn vsv_get_conn_err vsv_make_client
syn keyword veraTask		vsv_make_server vsv_up_connections 
syn keyword veraTask		vsv_wait_for_done vsv_wait_for_input
syn keyword veraTask		wait_child wait_var

syn keyword veraTodo contained TODO

syn match   veraOperator "[&|~><!)(*#%@+/=?:;}{,.\^\-\[\]]"

syn region  veraComment start="/\*" end="\*/" contains=veraTodo
syn match   veraComment "//.*" contains=veraTodo

syn match   veraGlobal "`[a-zA-Z0-9_]\+\>"
syn match   veraGlobal "$[a-zA-Z0-9_]\+\>"

syn match   veraConstant "\<[A-Z][A-Z0-9_]\+\>"

syn match   veraNumber "\(\<[0-9]\+\|\)'[bdh][0-9a-fxzA-FXZ_]\+\>"
syn match   veraNumber "\<[+-]\=[0-9]\+\>"

syn region  veraString start=+"+  end=+"+

" predefined vera constants
syn keyword veraConst	stderr stdin stdout
syn keyword veraConst	ALL ANY BAD_STATE
syn keyword veraConst	BAD_TRANS CALL CHECK
syn keyword veraConst	CHGEDGE CLEAR COPY_NO_WAIT
syn keyword veraConst	COPY_WAIT CROSS CROSS_TRANS
syn keyword veraConst	DEBUG DELETE EC_ARRAYX
syn keyword veraConst	EC_CODE_END EC_CONFLICT EC_EVNTIMOUT
syn keyword veraConst	EC_EXPECT EC_FULLEXPECT EC_MBXTMOUT
syn keyword veraConst	EC_NEXPECT EC_RETURN EC_RGNTMOUT
syn keyword veraConst	EC_RHNTMOUT EC_SCONFLICT EC_SEMTMOUT
syn keyword veraConst	EC_SEXPECT EC_SFULLEXPECT EC_SNEXTPECT
syn keyword veraConst	EC_USERSET EQ EVENT
syn keyword veraConst	FAIL FIRST FORK
syn keyword veraConst	GE GOAL GT
syn keyword veraConst	HAND_SHAKE HI HIGH
syn keyword veraConst	HNUM LE LIC_EXIT
syn keyword veraConst	LIC_PRERR LIC_PRWARN LIC_WAIT
syn keyword veraConst	LO LOAD LOW
syn keyword veraConst	LT MAILBOX MAX_COM
syn keyword veraConst	NAME NE NEGEDGE
syn keyword veraConst	NEXT NO_OVERLAP NO_OVERLAP_TRANS
syn keyword veraConst	NO_VARS NO_WAIT NUM
syn keyword veraConst	NUM_BIN NUM_DET OFF
syn keyword veraConst	OK OK_LAST ON
syn keyword veraConst	ONE_BLAST ONE_SHOT ORDER
syn keyword veraConst	PAST_IT PERCENT POSEDGE
syn keyword veraConst	PROGRAM RAWIN REGION
syn keyword veraConst	REPORT SAMPLE SAVE
syn keyword veraConst	SEMAPHORE SET SILENT
syn keyword veraConst	STATE STR STR_ERR_OUT_OF_RANGE
syn keyword veraConst	STR_ERR_REGEXP_SYNTAX SUM TRANS
syn keyword veraConst	VERBOSE WAIT 


"copied these from the c.vim file and modified
syn region veraPreCondit start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\|elif\>\|else\>\|endif\>\)" skip="\\$" end="$" contains=veraComment,veraString,veraCharacter,veraNumber
syn region veraIncluded contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match veraIncluded contained "<[^>]*>"
syn match veraInclude		"^\s*#\s*include\>\s*["<]" contains=veraIncluded
syn region veraDefine		start="^\s*#\s*\(define\>\|undef\>\)" skip="\\$" end="$" contains=ALLBUT,veraPreCondit,veraIncluded,veraInclude,veraDefine,veraOperator
syn region veraPreProc start="^\s*#\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" contains=ALLBUT,veraPreCondit,veraIncluded,veraInclude,veraDefine


" Add deprecated constructs here.
syn keyword veraDeprecated simwave_plot mailbox_send mailbox_receive
syn keyword veraDeprecated vhdl_node verilog_node 
syn keyword veraDeprecated bind_var get_bind get_bind_id
syn keyword veraDeprecated call_func call_task close_conn get_conn_err
syn keyword veraDeprecated make_client make_server up_connections

"Modify the following as needed.  The trade-off is performance versus
"functionality.
syn sync lines=75

if !exists("did_vera_syntax_inits")
  let did_vera_syntax_inits = 1
 " The default methods for highlighting.  Can be overridden later

  hi link veraCharacter       Character
  hi link veraConditional     Conditional
  hi link veraRepeat          Repeat
  hi link veraString          String
  hi link veraTodo            Todo

  hi link veraDefine	Macro
  hi link veraInclude	Include
  hi link veraIncluded	String
  hi link veraComment   Comment
  hi link veraConstant  Constant
  hi link veraLabel     PreCondit
  hi link veraPreCondit PreCondit
  hi link veraNumber    Special
  " hi link veraOperator  Type
  " hi link veraOperator  Operator
  " hi link veraStatement Statement
  hi link veraStatement Type
  hi link veraGlobal    String
  hi link veraTask	Statement
  hi link veraConst	Statement

  hi link veraDeprecated	Error
endif

let b:current_syntax = "vera"

" vim: ts=8

