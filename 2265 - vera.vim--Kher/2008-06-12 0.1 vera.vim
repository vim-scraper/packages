" Vim syntax file
" Language:     Vera, updated for 6.3.0
" Maintainer:   
" Last Update:  October 20, 2004
" Filenames:    *.vr *.vrh
" URL:          
"
" Vera is a Hardware Verification Language sold by Synopsys.
"
" History:
"       Mark Madsen leveraged this from the Verilog syntax file 
"       Beth Leonard added some enhancements
"       Dave Eggum added some keywords and updates
"               The most notable change is that veraStatement is now linked
"               to 'Type' instead of 'Statement'.  To override, modify your
"               .vimrc to include:
"                       hi link veraStatement Statement
"       Beth Leonard updated it for Vera 5:
"               Added the keywords/functions that were deprecated ages ago, in
"               addition to adding them to the deprecated list.
"       Aditya Kher updated it for Vera 6.3.0, RVM 8.0
"               added RVM specific keywords,tasks.

syn clear

" A bunch of useful Vera keywords
syn keyword veraStatement       all any assoc_index
syn keyword veraStatement       assoc_size async bad_state
syn keyword veraStatement       bad_trans begin big_endian
syn keyword veraStatement       bind bind_var bit
syn keyword veraStatement       bit_normal bit_reverse break
syn keyword veraStatement       breakpoint case casex
syn keyword veraStatement       casez class CLOCK continue
syn keyword veraStatement       coverage_block coverage_def coverage_depth
syn keyword veraStatement       coverage_goal coverage_option coverage_val
syn keyword veraStatement       default depth dist
syn keyword veraStatement       else end enum
syn keyword veraStatement       event export extends
syn keyword veraStatement       extern for fork
syn keyword veraStatement       function hdl_node hdl_task
syn keyword veraStatement       if illegal_self_transition illegal_state
syn keyword veraStatement       illegal_transation in !in
syn keyword veraStatement       inout input integer
syn keyword veraStatement       interface join little_endian
syn keyword veraStatement       local m_bad_state m_bad_trans
syn keyword veraStatement       m_state m_trans negedge
syn keyword veraStatement       newcov non_rand none
syn keyword veraStatement       not null or
syn keyword veraStatement       output packed port
syn keyword veraStatement       posedge prod prodget
syn keyword veraStatement       prodset program protected
syn keyword veraStatement       public rand randc
syn keyword veraStatement       randcase randseq reg
syn keyword veraStatement       repeat return shadow
syn keyword veraStatement       soft state static
syn keyword veraStatement       string super task
syn keyword veraStatement       terminate this trans
syn keyword veraStatement       typedef unpacked var
syn keyword veraStatement       vca vector verilog_node
syn keyword veraStatement       verilog_task vhdl_node vhdl_task
syn keyword veraStatement       virtual void while
syn keyword veraStatement       wildcard with 
syn keyword veraStatement       after
syn keyword veraStatement       coverage_group
syn keyword veraStatement       around rules
syn keyword veraStatement       do
syn keyword veraStatement       before
syn keyword veraStatement       proceed
syn keyword veraStatement       hide
syn keyword veraStatement       illegal_transition
syn keyword veraStatement       virtuals
syn keyword veraStatement       vera_bit_reverse
syn keyword veraStatement       vera_crc
syn keyword veraStatement       get_env
syn keyword veraStatement       vera_pack
syn keyword veraStatement       vera_pack_big_endian
syn keyword veraStatement       vera_plot
syn keyword veraStatement       getstate
syn keyword veraStatement       vera_report_profile
syn keyword veraStatement       cm_coverage
syn keyword veraStatement       vera_unpack
syn keyword veraStatement       cm_get_coverage
syn keyword veraStatement       setstate
syn keyword veraStatement       vera_unpack_big_endian
syn keyword veraStatement       cm_get_limit get_time_unit
syn keyword veraStatement       initstate
syn keyword veraStatement       urandom_range
syn keyword veraStatement       atohex
syn keyword veraStatement       icompare
syn keyword veraStatement       atoi insert
syn keyword veraStatement       atooct
syn keyword veraStatement       inst_get_at_least
syn keyword veraStatement       prematch
syn keyword veraStatement       atobin
syn keyword veraStatement       inst_get_auto_bin_max
syn keyword veraStatement       backref inst_get_collect
syn keyword veraStatement       bittostr inst_get_cov_weight
syn keyword veraStatement       capacity inst_get_coverage_goal
syn keyword veraStatement       compare inst_getcross_bin_max putc
syn keyword veraStatement       Configure inst_query
syn keyword veraStatement       inst_set_at_least
syn keyword veraStatement       delete inst_set_auto_bin_max
syn keyword veraStatement       DisableTrigger inst_set_bin_activiation
syn keyword veraStatement       DoAction inst_set_collect reserve
syn keyword veraStatement       empty inst_set_cov_weight reverse
syn keyword veraStatement       EnableCount inst_set_coverage_goal rsort
syn keyword veraStatement       EnableTrigger inst_set_cross_bin_max search
syn keyword veraStatement       Event itoa set_at_least
syn keyword veraStatement       find last set_auto_bin_max
syn keyword veraStatement       find_index last_index set_bin_activiation
syn keyword veraStatement       first len set_cov_weight
syn keyword veraStatement       first_index load set_coverage_goal
syn keyword veraStatement       GetAssert match set_cross_bin_max
syn keyword veraStatement       getc max set_name
syn keyword veraStatement       GetCount max_index size
syn keyword veraStatement       GetFirstAssert min sort
syn keyword veraStatement       GetName min_index substr
syn keyword veraStatement       GetNextAssert object_compare sum
syn keyword veraStatement       get_at_least object_copy thismatch
syn keyword veraStatement       get_auto_bin object_print tolower
syn keyword veraStatement       get_cov_weight toupper
syn keyword veraStatement       get_coverage_goal pick_index
syn keyword veraStatement       get_cross_bin_max
syn keyword veraStatement       get_status unique_index
syn keyword veraStatement       get_status_msg postmatch Wait
syn keyword veraStatement       object_compare

" bein/end and fork/join stand out a little more than normal statements
syn keyword veraLabel           begin end fork join
syn keyword veraConditional     if else case default 
syn keyword veraRepeat          repeat while for

" predefined vera tasks
" String methods and coverage methods are not listed in the vera.vim file
syn keyword veraTask            alloc assoc_index boundary
syn keyword veraTask            call_func call_task cast_assign
syn keyword veraTask            close_conn constraint constraint_mode
syn keyword veraTask            delay error error_mode
syn keyword veraTask            exit fclose feof
syn keyword veraTask            ferror fflush flag
syn keyword veraTask            fopen fprintf freadb
syn keyword veraTask            freadh freadstr get_bind
syn keyword veraTask            get_bind_id get_conn_err get_cycle
syn keyword veraTask            get_memsize get_plus_arg get_systime
syn keyword veraTask            get_time lock_file mailbox_get
syn keyword veraTask            mailbox_put mailbox_receive mailbox_send
syn keyword veraTask            make_client make_server new
syn keyword veraTask            os_command pack post_boundary
syn keyword veraTask            post_pack post_randomize post_unpack
syn keyword veraTask            pre_boundary pre_pack pre_randomize
syn keyword veraTask            pre_unpack printf psprintf
syn keyword veraTask            query query_str query_x
syn keyword veraTask            rand48 rand_mode random
syn keyword veraTask            randomize region_enter region_exit
syn keyword veraTask            rewind save semaphore_get
syn keyword veraTask            semaphore_put signal_connect simwave_plot
syn keyword veraTask            sprintf sscanf stop
syn keyword veraTask            suspend_thread sync timeout
syn keyword veraTask            trace trigger unit_delay
syn keyword veraTask            unit_delay unlock_file unpack
syn keyword veraTask            up_connections urand48 urandom
syn keyword veraTask            vca vsv_call_func vsv_call_task
syn keyword veraTask            vsv_close_conn vsv_get_conn_err vsv_make_client
syn keyword veraTask            vsv_make_server vsv_up_connections 
syn keyword veraTask            vsv_wait_for_done vsv_wait_for_input
syn keyword veraTask            wait_child wait_var

"RVM Vera Keywords 
syn keyword rvmStatement       rvm_watchdog_port
syn keyword rvmStatement       rvm_log
syn keyword rvmStatement							rvm_data
syn keyword rvmStatement							rvm_notify_event 
syn keyword rvmStatement							rvm_notify
syn keyword rvmStatement							rvm_xactor
syn keyword rvmStatement							rvm_log_watchpoint 
syn keyword rvmStatement							rvm_log_msg
syn keyword rvmStatement							rvm_xactor_callbacks
syn keyword rvmStatement							rvm_broadcast
syn keyword rvmStatement							rvm_scheduler_election
syn keyword rvmStatement							rvm_scheduler
syn keyword rvmStatement							rvm_watchdog 
syn keyword rvmStatement							rvm_env 
syn keyword rvmStatement							rvm_fatal 
syn keyword rvmStatement							rvm_error
syn keyword rvmStatement							rvm_warning
syn keyword rvmStatement							rvm_note 
syn keyword rvmStatement							rvm_trace
syn keyword rvmStatement							rvm_debug
syn keyword rvmStatement							rvm_verbose
syn keyword rvmStatement							rvm_report 
syn keyword rvmStatement							rvm_command
syn keyword rvmStatement							rvm_protocol 
syn keyword rvmStatement							rvm_transaction
syn keyword rvmStatement							rvm_cycle
syn keyword rvmStatement							rvm_user 

"RVM Vera functions
syn keyword rvmTask     rvm_channel_decl
syn keyword rvmTask     extern_rvm_channel
syn keyword rvmTask	 			rvm_channel
syn keyword rvmTask	 			rvm_warning 
syn keyword rvmTask	 			rvm_error
syn keyword rvmTask	 			rvm_fatal
syn keyword rvmTask	 			rvm_trace
syn keyword rvmTask	 			rvm_debug
syn keyword rvmTask	 			rvm_verbose   
syn keyword rvmTask	 			rvm_note   
syn keyword rvmTask	 			rvm_report   
syn keyword rvmTask	 			rvm_command 
syn keyword rvmTask	 			rvm_protocol  
syn keyword rvmTask	 			rvm_cycle
syn keyword rvmTask	 			rvm_trace
syn keyword rvmTask	 			rvm_debug 
syn keyword rvmTask	 			rvm_transaction 
syn keyword rvmTask	 			rvm_verbose
syn keyword rvmTask	 			rvm_note
syn keyword rvmTask	 			rvm_report
syn keyword rvmTask	 			rvm_command
syn keyword rvmTask	 			rvm_protocol
syn keyword rvmTask	 			rvm_transaction
syn keyword rvmTask	 			rvm_cycle
syn keyword rvmTask	 			rvm_user
syn keyword rvmTask	 			rvm_OO_callback
syn keyword rvmTask	 			rvm_atomic_gen_callbacks_decl 
syn keyword rvmTask	 			rvm_atomic_gen_decl 
syn keyword rvmTask	 			extern_rvm_atomic_gen 
syn keyword rvmTask	 			rvm_atomic_gen
syn keyword rvmTask	 			rvm_scenario_decl
syn keyword rvmTask	 			rvm_atomic_scenario_decl 
syn keyword rvmTask	 			rvm_scenario_election_decl
syn keyword rvmTask	 			rvm_scenario_gen_callbacks_decl   
syn keyword rvmTask	 			rvm_scenario_gen_decl 
syn keyword rvmTask	 			extern_rvm_scenario_gen   
syn keyword rvmTask	 			rvm_scenario_gen   
syn keyword rvmTask	 			rvm_channel_ 

"RVM Vera constants
syn keyword rvmConst    RVM_NUMERIC_VERSION_MACROS   
syn keyword rvmConst    RVM_VERSION   
syn keyword rvmConst				RVM_MINOR   
syn keyword rvmConst				RVM_PATCH
syn keyword rvmConst				rvm_channel__SOURCE   
syn keyword rvmConst				rvm_channel__SINK   
syn keyword rvmConst				rvm_channel__NO_ACTIVE   
syn keyword rvmConst				rvm_channel__ACT_PENDING 
syn keyword rvmConst				rvm_channel__ACT_STARTED
syn keyword rvmConst				rvm_channel__ACT_COMPLETED  
syn keyword rvmConst				rvm_channel__FULL
syn keyword rvmConst				rvm_channel__EMPTY 
syn keyword rvmConst				rvm_channel__PUT
syn keyword rvmConst				rvm_channel__GOT
syn keyword rvmConst				rvm_channel__PEEKED
syn keyword rvmConst				rvm_channel__ACTIVATED 
syn keyword rvmConst				rvm_channel__STARTED 
syn keyword rvmConst				rvm_channel__COMPLETED 
syn keyword rvmConst				rvm_channel__REMOVED
syn keyword rvmConst				rvm_channel__LOCKED 
syn keyword rvmConst				rvm_channel__UNLOCKED
syn keyword rvmConst				rvm_data__EXECUTE 
syn keyword rvmConst				rvm_data__STARTED
syn keyword rvmConst				rvm_data__ENDED 
syn keyword rvmConst				rvm_env__CFG_GENED
syn keyword rvmConst				rvm_env__BUILT
syn keyword rvmConst				rvm_env__DUT_CFGED
syn keyword rvmConst				rvm_env__STARTED 
syn keyword rvmConst				rvm_env__RESTARTED  
syn keyword rvmConst				rvm_env__ENDED   
syn keyword rvmConst				rvm_env__STOPPED
syn keyword rvmConst				rvm_env__CLEANED 
syn keyword rvmConst				rvm_env__DONE
syn keyword rvmConst				rvm_log__DEFAULT
syn keyword rvmConst				rvm_log__UNCHANGED
syn keyword rvmConst				rvm_log__FAILURE_TYP 
syn keyword rvmConst				rvm_log__NOTE_TYP
syn keyword rvmConst				rvm_log__DEBUG_TYP
syn keyword rvmConst				rvm_log__REPORT_TYP
syn keyword rvmConst				rvm_log__NOTIFY_TYP		
syn keyword rvmConst				rvm_log__TIMING_TYP
syn keyword rvmConst				rvm_log__XHANDLING_TYP 
syn keyword rvmConst				rvm_log__PROTOCOL_TYP
syn keyword rvmConst				rvm_log__TRANSACTION_TYP 
syn keyword rvmConst				rvm_log__COMMAND_TYP 
syn keyword rvmConst				rvm_log__CYCLE_TYP
syn keyword rvmConst				rvm_log__USER_TYP_0
syn keyword rvmConst				rvm_log__USER_TYP_1 
syn keyword rvmConst				rvm_log__USER_TYP_2 
syn keyword rvmConst				rvm_log__USER_TYP_3 
syn keyword rvmConst				rvm_log__DEFAULT_TYP
syn keyword rvmConst				rvm_log__ALL_TYPES 
syn keyword rvmConst				rvm_log__FATAL_SEV
syn keyword rvmConst				rvm_log__ERROR_SEV 
syn keyword rvmConst				rvm_log__WARNING_SEV 
syn keyword rvmConst				rvm_log__NORMAL_SEV 
syn keyword rvmConst				rvm_log__TRACE_SEV
syn keyword rvmConst				rvm_log__DEBUG_SEV
syn keyword rvmConst				rvm_log__VERBOSE_SEV 
syn keyword rvmConst				rvm_log__HIDDEN_SEV 
syn keyword rvmConst				rvm_log__IGNORE_SEV 
syn keyword rvmConst				rvm_log__DEFAULT_SEV   
syn keyword rvmConst				rvm_log__ALL_SEVERITIES 
syn keyword rvmConst				rvm_log__CONTINUE 
syn keyword rvmConst				rvm_log__COUNT_AS_ERROR 
syn keyword rvmConst				rvm_log__DEBUGGER 
syn keyword rvmConst				rvm_log__DUMP
syn keyword rvmConst				rvm_log__STOP 
syn keyword rvmConst				rvm_log__ABORT 
syn keyword rvmConst				rvm_notify__ONE_SHOT_TRIGGER 
syn keyword rvmConst				rvm_notify__ONE_BLAST_TRIGGER  
syn keyword rvmConst				rvm_notify__HAND_SHAKE_TRIGGER
syn keyword rvmConst				rvm_notify__ON_OFF_TRIGGER
syn keyword rvmConst				rvm_xactor__XACTOR_IDLE  
syn keyword rvmConst				rvm_xactor__XACTOR_BUSY 
syn keyword rvmConst				rvm_xactor__XACTOR_STARTED
syn keyword rvmConst				rvm_xactor__XACTOR_STOPPED
syn keyword rvmConst				rvm_xactor__XACTOR_RESET 
syn keyword rvmConst				rvm_xactor__XACTOR_SOFT_RST 
syn keyword rvmConst				rvm_xactor__XACTOR_FIRM_RST
syn keyword rvmConst				rvm_xactor__XACTOR_HARD_RST 
syn keyword rvmConst				rvm_xactor__XACTOR_PROTOCOL_RST 
syn keyword rvmConst				rvm_broadcast__AFAP
syn keyword rvmConst				rvm_broadcast__ALAP 
syn keyword rvmConst				rvm_watchdog__TIMEOUT 

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
syn keyword veraConst   stderr stdin stdout
syn keyword veraConst   ALL ANY BAD_STATE
syn keyword veraConst   BAD_TRANS CALL CHECK
syn keyword veraConst   CHGEDGE CLEAR COPY_NO_WAIT
syn keyword veraConst   COPY_WAIT CROSS CROSS_TRANS
syn keyword veraConst   DEBUG DELETE EC_ARRAYX
syn keyword veraConst   EC_CODE_END EC_CONFLICT EC_EVNTIMOUT
syn keyword veraConst   EC_EXPECT EC_FULLEXPECT EC_MBXTMOUT
syn keyword veraConst   EC_NEXPECT EC_RETURN EC_RGNTMOUT
syn keyword veraConst   EC_RHNTMOUT EC_SCONFLICT EC_SEMTMOUT
syn keyword veraConst   EC_SEXPECT EC_SFULLEXPECT EC_SNEXTPECT
syn keyword veraConst   EC_USERSET EQ EVENT
syn keyword veraConst   FAIL FIRST FORK
syn keyword veraConst   GE GOAL GT
syn keyword veraConst   HAND_SHAKE HI HIGH
syn keyword veraConst   HNUM LE LIC_EXIT
syn keyword veraConst   LIC_PRERR LIC_PRWARN LIC_WAIT
syn keyword veraConst   LO LOAD LOW
syn keyword veraConst   LT MAILBOX MAX_COM
syn keyword veraConst   NAME NE NEGEDGE
syn keyword veraConst   NEXT NO_OVERLAP NO_OVERLAP_TRANS
syn keyword veraConst   NO_VARS NO_WAIT NUM
syn keyword veraConst   NUM_BIN NUM_DET OFF
syn keyword veraConst   OK OK_LAST ON
syn keyword veraConst   ONE_BLAST ONE_SHOT ORDER
syn keyword veraConst   PAST_IT PERCENT POSEDGE
syn keyword veraConst   PROGRAM RAWIN REGION
syn keyword veraConst   REPORT SAMPLE SAVE
syn keyword veraConst   SEMAPHORE SET SILENT
syn keyword veraConst   STATE STR STR_ERR_OUT_OF_RANGE
syn keyword veraConst   STR_ERR_REGEXP_SYNTAX SUM TRANS
syn keyword veraConst   VERBOSE WAIT 


"copied these from the c.vim file and modified
syn region veraPreCondit start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\|elif\>\|else\>\|endif\>\)" skip="\\$" end="$" contains=veraComment,veraString,veraCharacter,veraNumber
syn region veraIncluded contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match veraIncluded contained "<[^>]*>"
syn match veraInclude           "^\s*#\s*include\>\s*["<]" contains=veraIncluded
syn region veraDefine           start="^\s*#\s*\(define\>\|undef\>\)" skip="\\$" end="$" contains=ALLBUT,veraPreCondit,veraIncluded,veraInclude,veraDefine,veraOperator
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

  hi link veraDefine    Macro
  hi link veraInclude   Include
  hi link veraIncluded  String
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
  hi link veraTask      Statement
  hi link veraConst     Statement

  hi link veraDeprecated        Error
		
"RVM specific additions :
		"hi link rvmStatement statement
		hi link rvmStatement Type
		hi link rvmConst     Constant
		hi link rvmTask      statement 

endif

let b:current_syntax = "vera"


