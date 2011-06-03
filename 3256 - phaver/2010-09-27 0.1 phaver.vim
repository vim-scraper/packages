" Vim syntax file
" Language: Phaver
" Maintainer: huang chongdi <huangcd.thu@gmail.com>
" Last Change: 2010-09-27
" A very simple syntax highlighting description file for PHAVER(Polyhedral Hybrid
" Automaton Verifyer)
 
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword phaKeyword          contr_var input_var parameter initially state_var sys
syn keyword phaStructure        automaton end
syn keyword phaRepeat           while wait
syn keyword phaConditional      when do goto sync
syn keyword phaLabel            loc synclabs
syn keyword phaBoolean          true false
syn keyword phaFunction         print reachable is_reachable is_reachable_fb get_sim
syn keyword phaFunction         is_sim is_bisim ag_sim set_partition_constraints is_empty
syn keyword phaFunction         inital_states get_invariants rename remove project_to get_parameters
syn keyword phaFunction         loc_union loc_intersection intersection_assign difference_assign
syn keyword phaFunction         inverse project_to_first add_label reverse initial_states 
syn keyword phaFunction         invariant_assign set_refine_constraints CHEAP_CONTAIN_RETURN_OTHERS
syn keyword phaFunction         REACH_MAX_ITER USE_CONVEX_HULL REACH_STOP_USE_CONVEX_HULL_ITER
syn keyword phaFunction         REACH_USE_BBOX REACH_USE_BBOX_ITER REACH_ONLY_EXPLORE SEARCH_METHOD
syn keyword phaFunction         SNAPSHOT_INTERVAL CONSTRAINT_BITSIZE REACH_BITSIZE_TRIGGER
syn keyword phaFunction         REACH_STOP_USE_BITSIZE LIMIT_CONSTRAINTS_METHOD REACH_CONSTRAINT_LIMIT
syn keyword phaFunction         REACH_CONSTRAINT_TRIGGER TP_CONSTRAINT_LIMIT PRIME_R_WITH_REACH
syn keyword phaFunction         USE_CONVEX_HULL_FOR_PRIMING PRIME_R_WITH_DISCRETE_REACH
syn keyword phaFunction         STOP_AT_BAD_STATES SHOW_BAD_STATES SIM_SIMPLIFY_R TIME_POST_ITER
syn keyword phaFunction         PARTITION_CHECK_TIME_RELEVANCE PARTITION_CHECK_TIME_RELEVANCE_DURING
syn keyword phaFunction         PARTITION_CHECK_TIME_RELEVANCE_FINAL REFINE_DERIVATIVE_METHOD
syn keyword phaFunction         PARTITION_PRIORITIZE_REACH_SPLIT PARTITION_PRIORITIZE_ANGLE
syn keyword phaFunction         PARTITION_SMALLEST_FIRST PARTITION_DERIV_MAXANGLE REACH_USE_CONVEX_HULL
syn keyword phaFunction         REFINE_CHECK_TIME_RELEVANCE_DURING REFINE_CHECK_TIME_RELEVANCE_FINAL  
syn keyword phaFunction         who echo
syn region  phaBlockComment     start="/\*" end="\*/" contains=phaBlockComment
syn match   phaComment          /--/
syn match   phaComment          /\/\/.*/
syn region  phaString           start=/"/ skip=/\\"/ end=/"/
syn match   phaNumber           /[+-]?[0-9]*(\.[0-9]+)?((e|E)[+-]?[0-9]+)?/

hi  link    phaKeyword          Keyword
hi  link    phaComment          Comment
hi  link    phaString           String
hi  link    phaBlockComment     SpecialComment
hi  link    phaNumber           Float
hi  link    phaRepeat           SpecialComment
hi  link    phaLabel            Label
hi  link    phaConditional      SpecialComment
hi  link    phaStructure        Structure
hi  link    phaBoolean          Boolean
hi  link    phaFunction         Function

let b:current_syntax = "pha"
