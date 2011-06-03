" Vim syntax file
" Language: Otter (Theorm prover) input files
" Author: Steven VanHorn (theepicsnail@gmail.com)
" Version: 4.12.2010


if exists("b:current_syntax")
  finish
endif
												


			
syn keyword valFlags stats_level max_given parameter name max_seconds max_levels report max_seconds max_gen max_kept max_given max_levels max_mem max_literals max_weight max_distinct_vars max_answers fpa_literals fpa_terms pick_given_ratio age_factor distinct_vars_factor interrupt_given demod_limit max_proofs min_bit_width neg_weight pretty_print_indent stats_level dynamic_demod_depth dynamic_demod_rhs new_symbol_lex_position pick_given_ratio equiv_hint_wt equiv_hint_add_wt fsub_hint_wt fsub_hint_add_wt bsub_hint_wt bsub_hint_add_wt bsub_hint_add_wt bsub_hint_wt bsub_hint_add_wt param-name change_limit_after new_max_weight debug_first debug_first verbose_demod_skip geo_given_ratio max_ur_depth max_ur_deduction_size split_depth split_given split_seconds split_depth 
hi def link valFlags Type

syn keyword boolFlags sos_queue sos_stack input_sos_first interactive_given print_given print_lists_at_end binary_res hyper_res neg_hyper_res ur_res para_into para_from demod_inf order_hyper unit_res ur_last para_from_left para_from_right para_into_left para_into_right para_from_vars para_into_vars para_from_units_only para_into_units_only para_skip_skolem para_ones_rule para_all detailed_history order_history unit_deletion back_unit_deletion delete_identical_nested_skolem sort_literals for_sub back_sub factor demod_history order_eq eq_units_both_ways demod_linear demod_out_in dynamic_demod dynamic_demod_all dynamic_demod_lex_dep back_demod anl_eq knuth_bendix lrpo lex_order_vars symbol_elim rewriter check_arity prolog_style_variables echo_included_files simplify_fol process_input tptp_eq very_verbose print_kept print_proofs build_proof_object_1 build_proof_object_2 print_new_demod print_back_demod print_back_sub display_terms pretty_print formula_history index_for_back_demod for_sub_fpa no_fapl no_fanl control_memory propositional really_delete_clauses atom_wt_max_args term_wt_max_args free_all_mem sigint_interact keep_hint_subsumers keep_hint_equivalents keep_hint_subsumers degrade_hints2 print_proof_as_hints ancestor_subsume input_sequent output_sequent geometric_rule geometric_rewrite_before geometric_rewrite_after gl_demod linked_ur_res linked_ur_trace linked_sub_unit_usable linked_sub_unit_sos linked_unit_del linked_target_all	
hi def link boolFlags Type

syn match funcs /\$\(SUM\|PROD\|DIFF\|DIV\|MOD\|EQ\|NE\|LT\|LE\|GT\|GE\||FSUM\|FPROD\|FDIFF\|FDIV\|FEQ\|FNE\|FLT\|FLE\|FGT\|FGE\|BIT_AND\|BIT_OR\|BIT_XOR\|SHIFT_LEFT\|SHIFT_RIGHT\|BIT_NOT\|INT_TO_BITS\|BITS_TO_INT\|T\|F\|AND\|OR\|TRUE\|NOT\|IF\|ID\|LNE\|LLT\|LLE\|LGT\|LGE\|OCCURS\|VOCCURS\|VFREE\|RENAME\|ATOMIC\|INT\|BITS\|VAR\|GROUND\|NEXT_CL_NUM\|UNIQUE_NUM\)/
hi def link funcs Function

syn match test /a\(a\|b\)/
hi def link test Function
syn keyword commands include op make_evaluable set clear assign list formula_list weight_list lex skolem lrpo_multiset_status end_of_list
hi def link commands Function

syn keyword listTypes usable sos demodulators passive
hi def link listTypes Constant 

syn keyword limits max_seconds max_gen max_kept max_given max_levels max_mem max_literals max_weight max_distinct_vars max_answers
hi def link limits Type
syn match cmnt  		"%.*$" 
hi def link cmnt 		Comment

syn match grouping 		"[\.|,\[\]\(\){}-]"
hi def link grouping		Operator

syn match number		"\(-\?\d\+\)"
hi def link number		Number

let b:current_syntax = "otter"










"
"syn keyword inference_rules 	binary_res 	hyper_res 	neg_hyper_res 	ur_res 
"syn keyword inference_rules	para_into	para_from 	demod_inf
"hi def link inference_rules	Type
"
"syn keyword resolution_flags 	order_hyper 	unit_res ur_last
"hi def link resolution_flags	Type
"
"syn keyword paramodulation_flags para_from_left para_from_right para_into_left 	para_into_right 
"syn keyword paramodulation_flags para_from_vars para_from_vars	para_into_vars	para_from_units_only
"syn keyword paramodulation_flags para_into_units_only	para_skip_skolem	para_ones_rule	para_all
"hi def link paramodulation_flags Type
"
"syn keyword gen_clause_flags	detailed_history	order_history	unit_deletion back_unit_deletion
"syn keyword gen_clause_flags	delete_idential_nested_skolem	sort_literals	for_sub	back_sub	factor
"hi def link gen_clause_flags	Type
"
"
"syn keyword demod_flags		demod_history	order_eq	eq_units_both_ways	demod_linear	demod_out_in
"syn keyword demod_flags		dynamic_demod	dynamic_demod_all dynamic_demod_lex_dep back_demod anl_eq knuth_bendix
"syn keyword demod_flags		lrpo	lex_order_vars symbol_elim rewriter
"hi def link demod_flags		Type
"
"syn keyword input_flags		check_arity prolog_style_variables echo_included_files simplify_fol process_input tptp_eq
"hi def link input_flags		Type
"
"syn keyword output_flags	very_verbose print_ept print_proofs build_proof_object_1 build_proof_object_2 print_new_demod
"syn keyword output_flags	print_back_demod print_back_sub display_terms pretty_print bird_print formula_history 
"hi def link output_flags	Type
"
"
"syn keyword index_flags		index_for_back_demod for_sub_fpa no_fapl no_fanl 
"hi def link index_flags		Type
"
"syn keyword misc_flags		control_memory propositional really_delete_clauses atom_wt_max_args term_wt_max_args free_all_mem sig_interact
"hi def link misc_flags		Type
"
"syn keyword list_name 		usable sos demodulators passive
"hi def link list_name		Constant
"
"syn keyword commands 		include op make_evaluable set clear assign list formula_list weight_list lex skolem lrpo_multiset_status end_of_list
"hi def link commands		Preproc	
"
"syn keyword assign_params	report max_seconds max_gen max_kept max_given max_levels max_mem max_literals max_weight max_distinct_vars max_answers
"syn keyword assign_params	fpa_literals fpa_terms pick_given_ratio age_factor distinct_vars_factor interrupt_given demod_limit max_proofs
"syn keyword assign_params	min_bit_width neg_weight pretty_print_indent stats_level dynamic_demod_depth dynamic_demod_rhs new_symbol_lex_position
"hi def link assign_params	Type

