" Vim filetype plugin file
" Language:	tex
" Maintainer:	Marcin Szamotulski
" Last Changed: June 20
" URL:		
" Email:	mszamot [AT] gmail [DOT] com

" {{{1 maps
if mapcheck("\\swp",'n') =~ "SaveWinPos"
   unmap \swp
endif
let this_file=globpath(&rtp,'**/shapley.vim')
map \s :w \| so %<CR>
map \l :w \| so %<CR> @:
"}}}1
" Examples of games: {{{1
"{{{2 Coalitional Games /CGames/
let Game=[[[],0], [['a'],1], [['b'],1], [['c'],0], [['a', 'b'],2], [['a', 'c'],3], [['b', 'c'],2], [['a', 'b', 'c'],0]]

let GameB=[[[],0], [['a'],1], [['b'],1], [['c'],0], [['d'],2], 
	    \ [['a', 'b'],2], [['a', 'c'],3], [['a', 'd'],4], 
	    \ [['b', 'c'],2], [['b', 'd'],-2], [['c', 'd'],4],
	    \ [['a', 'b', 'c'],0], [['a', 'b', 'd'],2], [['a', 'c', 'd'],4], [['b', 'c', 'd'],5],
	    \ [['a', 'b', 'c', 'd'],0] ]

let GameD=[[[['a']], 1.0], [[['b']], 1.0], [[['c', 'd']], 2.0], [[['a'], ['b']], 2.5], [[['a'], ['c', 'd']], 3.0], [[['b'], ['c', 'd']], 3.0], [[['a'], ['b'], ['c', 'd']], 4.5], [[], 0]]

let GameDa=[[['a'], 1.0], [['b'], 1.0], [['c'], 2.0], [['a', 'b'], 2.5], [['a', 'c'], 3.0], [['b', 'c'], 3.0], [['a', 'b', 'c'], 4.5], [[], 0]]
"}}}2
"{{{2 Partition Form Games /PFGames/
" game in partition function form:
" [ [coalition_structure], ... ]  coalition structure is a list of elements of
" the form [coalition,value] coalitions must form a coalition structure (i.e.
" a partition of the set of players). Example: 
" (values are scaled 10 times! to avoid floats).

" Tomeks Example (1)
let GameC=[ [[['a'], 1.0], [['b'], 1.0], [['c'], 2.0], [['d'], 0]],
	    \ [[['a', 'b'], 3.5], [['c'], 4.0], [['d'],1.0]],
	    \ [[['a', 'b'], 2.5], [['c', 'd'], 2.0]],
	    \ [[['a', 'c'], 3.0], [['b'],1.0], [['d'], 0]], 
	    \ [[['a', 'c'], 3.5], [['b', 'd'], 0]],
	    \ [[['a', 'd'], 1.0], [['b'], 1.0], [['c'], 2.0]],
	    \ [[['a', 'd'], 1.0], [['b', 'c'], 3.0]],
	    \ [[['b', 'c'], 3.0], [['a'],1.0], [['d'], 0]],
	    \ [[['b', 'd'], 1.0], [['a'],1.0], [['c'], 2.0]],
	    \ [[['c', 'd'], 2.0], [['a'],1.0], [['b'], 1.0]],
	    \ [[['a', 'b', 'c'], 4.5], [['d'], 0]],
	    \ [[['a', 'b', 'd'], 2.5], [['c'], 2.0]],
	    \ [[['a', 'c', 'd'], 3.0], [['b'], 1.0]],
	    \ [[['b', 'c', 'd'], 3.0], [['a'], 1.0]],
	    \ [[['a', 'b', 'c', 'd'], 4.5]] ]

" Tomeks Example (2)
let GameE=[ [[['a'], 1.0], [['b'], 1.0], [['c'], 2.0], [['d'], 0]],
	    \ [[['a', 'b'], 3.5], [['c'], 2.0], [['d'], 1.0]],
	    \ [[['a', 'b'], 3.5], [['c', 'd'], 3.0]],
	    \ [[['a', 'c'], 3.0], [['b'], 1.0], [['d'], 0.0]], 
	    \ [[['a', 'c'], 3.5], [['b', 'd'], 0.0]],
	    \ [[['a', 'd'], 1.0], [['b'], 1.0], [['c'], 2.0]],
	    \ [[['a', 'd'], 1.0], [['b', 'c'], 3.0]],
	    \ [[['b', 'c'], 3.0], [['a'], 1.0], [['d'], 0.0]],
	    \ [[['b', 'd'], 1.0], [['a'], 1.0], [['c'], 2.0]],
	    \ [[['c', 'd'], 2.0], [['a'], 1.0], [['b'], 1.0]],
	    \ [[['a', 'b', 'c'], 7.5], [['d'], 1.0]],
	    \ [[['a', 'b', 'd'], 4.5], [['c'], 4.0]],
	    \ [[['a', 'c', 'd'], 3.0], [['b'], 1.0]],
	    \ [[['b', 'c', 'd'], 3.0], [['a'], 1.0]],
	    \ [[['a', 'b', 'c', 'd'], 8.0]]
	    \ ]

" let GameE2=[ [[['a'], 1.0], [['b'], 1.0], [['c'], 2.0], [['d'], 0]],
" 	    \ [[['a', 'b'], 3.5], [['c'], 4.0], [['d'],1.0]],
" 	    \ [[['a', 'b'], 2.5], [['c', 'd'], 2.0]],
" 	    \ [[['a', 'c'], 3.0], [['b'],1.0], [['d'], 0]], 
" 	    \ [[['a', 'c'], 3.5], [['b', 'd'], 0]],
" 	    \ [[['a', 'd'], 1.0], [['b'], 1.0], [['c'], 2.0]],
" 	    \ [[['a', 'd'], 1.0], [['b', 'c'], 3.0]],
" 	    \ [[['b', 'c'], 3.0], [['a'],1.0], [['d'], 0]],
" 	    \ [[['b', 'd'], 1.0], [['a'],1.0], [['c'], 2.0]],
" 	    \ [[['c', 'd'], 2.0], [['a'],1.0], [['b'], 1.0]],
" 	    \ [[['a', 'b', 'c'], 6.0], [['d'], 1.0]],
" 	    \ [[['a', 'b', 'd'], 4.5], [['c'], 4.0]],
" 	    \ [[['a', 'c', 'd'], 3.0], [['b'], 1.0]],
" 	    \ [[['b', 'c', 'd'], 3.0], [['a'], 1.0]],
" 	    \ [[['a', 'b', 'c', 'd'], 4.5]]
" 	    \ ]
"}}}2
"{{{2 Ieong Shoham Rules with negative literals
" IeongShohamRules With negative literals
let ISRule=[
	    \ [10, ['a','c'],['b']]
	    \ ]
"}}}2
"{{{2 Weighted Rules 
" weighted rules for GameC and GameE
" each rule is of the form
" 	paper version (pattern1,value1)(pattern2,value2)...|...|(patternX,valueX) ... 
" 	we use only positive litteral thus pattern == a coalition
" 	and we write the above in the form:
" 	[[[value,coalition_in_1,coalition_out_1],[value2_caolitin_in_2,coalition_out_2],...],...,[[valueX,coalition_in_X,,coalition_out_X],...,]]
" 	the second coaltion (of negative literals) is optional, while the first two
" 	are obsolete.

" Tomek Example (1) /this is the same game as above but written using weighted rules with | /
let WeightedRulesC=[ 
	    \ [[[1,['a']]]],
	    \ [[[1,['b']]]],
	    \ [[[2,['c']]]],
	    \ [[[0.5, ['a', 'b']]]],
	    \ [[[0.5,['a', 'c']]], [[-1,['b', 'd']]]],
	    \ [[[1,['a', 'b']]], [[2,['c']]],[[1,['d']]]]
	    \ ]

" Tomeks Example (2) /this is the same game as above but written using weighted rules without | /
let WeightedRulesE=[
	    \ [[[1,['a']]]],
	    \ [[[1,['b']]]],
	    \ [[[2,['c']]]],
	    \ [[[0.5, ['a', 'b']]]],
	    \ [[[0.5,['a', 'c']], [-1,['b', 'd']]]],
	    \ [[[1,['a', 'b']], [2,['c']], [1,['d']]]]
	    \ ]
let WeightedRulesG=[
	    \ [[[10,['a'],['c']],[15,['b'], ['a']]], [[20, ['x'],['y']]]],
	    \ ]
" 	    \ [[[10,['a'],['c']],[15,['b'], ['a']]]],
" 	    \ [[[10,['a']],[15,['b']]]],
" 	    \ [[[10,['a']]], [[20,['x']]]],
" 	    \ [[[10,['a', 'b']], [20,['c'],['e']], [30,['d'],['f']]], [[11, ['x'], ['y']], [12, ['z']]]],
" 	    \ [[[10,['b']]]],
" 	    \ [[[20,['c']]]],
" 	    \ [[[5,['a', 'c']], [-10,['b', 'd']]],[[-10,['e', 'f']]]],
" 	    \ [[[10,['a', 'b']], [20,['c']], [10,['d']], [15,['e', 'f']]]],
let WeightedRulesH=[
	    \ [[[10,['a']]]],
	    \ [[[10,['b']]]],
	    \ [[[20,['c']]]],
	    \ [[[5,['a', 'c']], [-10,['b', 'd']]],[[-10,['e', 'f']]]],
	    \ [[[10,['a', 'b']], [20,['c'],['e']], [10,['d'],['f']]], [[-10,['p', 'q']], [-20,['r']]]],
	    \ [[[10,['a'], ['f']], [20,['c']], [10,['d', 'b']], [15,['e', 'g'],['f']]],[[-10,['x'],['z']],[-20,['w']]]],
	    \ ]
"}}}2
"}}}1
" {{{1 McQUILLIN SHAPLEY VALUE
" compute the McQuillin Shapley Value of a partition form game:
function! McQuillinShapleyValue(game,players) 
    let game=deepcopy(a:game)
    let players=deepcopy(a:players)

    let cs_games=[]
    let coalition_structures=CoalitionStructures(players)
    for cs in coalition_structures
	" we compute the game w_\pi here we denote it by cs_game!
	
	let cs_game=[]
	let cs_players=cs
	let powerset=Minus(PowerSet(cs_players),[[]])
	for cs_coalition in powerset

	    " this variable stores the leading term of an embedded coalition
	    " actually the list of players which constitute the coalitions
	    " which belongs to it!  what I denote by: \lfoot T \rfoot
	    let cs_embedded=[]
	    for cs_player in cs_coalition
		for player in cs_player
		    call add(cs_embedded,player)
		endfor
	    endfor
	    let cs_rest=Minus(players,cs_embedded)
	    if cs_rest != []
		let partition=[cs_embedded,cs_rest]
	    else
		let partition=[cs_embedded]
	    endif
	    call sort(partition,"SortCS")
	    let embedded_coalition=[cs_embedded, partition]

" 	    echo "cs ".string(cs)
" 	    echo "     ec ".string(embedded_coalition[0])
" 	    echo "  partition ". string(embedded_coalition[1]) 
	    	" cs_u ".string(cs_embedded)." cs_r ".string(cs_rest)
	    
	    " Now we have to take the value from the game a:game
	    let cs_value=ValueCS(a:game,embedded_coalition)
" 	    echo string(embedded_coalition)." value ".value
	    call add(cs_game,[cs_coalition,cs_value])
	endfor

	call add(cs_game,[[],0])
	call add(cs_games,cs_game)
" 	echo cs_game
    endfor
    let g:games=cs_games

    " and now we compute the Shapley values of w_\pi
    let mcq_game=[]
    for cs_game in cs_games
	" set of players
	" this is actually [ [player], value ] we want to get [player,...]
	let cs_players=filter(deepcopy(cs_game),'len(v:val[0]) == 1')
	call map(cs_players,'v:val[0][0]')
" 	echo cs_players
" 	let b:cs_players=[]
" 	call add(b:cs_players,cs_players)

	let cs_ShapleyValue=ShapleyValue(cs_game)
	" embedded coalition = [ coalition, cs_players ]

	for coalition in cs_ShapleyValue
	    call add(mcq_game,[[coalition[0], cs_players], coalition[1]])
	endfor
    endfor
    return mcq_game
endfunction
command! -nargs=* -complete=var McQuillinShapleyValue	:echo McQuillinShapleyValue(<args>)
"}}}1
"{{{1 SHAPLEY VALUE
" For a game we find Ieong Shoham rules with positive literals and the compute the IeongShohamShapleyValue 
" the translation is done by the functio IeongShohamRules below.
" {{{2 ShapleyValue
" This function computes the Shapley value from a function form representation.
function! ShapleyValue(game)
    let game=deepcopy(a:game)
    
    " get the list of players
    let players=[]
    for player in filter(deepcopy(game),'len(v:val[0]) == 1')
	call add(l:players,player[0][0])
    endfor
    unlet player

"   make the list of rules (without negative once)	 	
    let rules=IeongShohamRules(game)
    let b:rules=rules 

    " dictionary of Shapley values { player : value }
    return IeongShohamShapleyValue(rules,players)
endfunction
command! -nargs=1 -complete=var ShapleyValue	:echo ShapleyValue(<args>)
"}}}2
"{{{2 IeonShohamRules /with only positive rules/
function! IeongShohamRules(game,...)
    " count number of players
    let game=deepcopy(a:game)
  
    " get the list of players
    let players=[]
    for player in filter(deepcopy(game),'len(v:val[0]) == 1')
	call add(players,player[0][0])
    endfor
    let g:players=players

    " rules= [ [coalition,value], ... ]
    let rules=[]
    let powerset=PowerSet(players)
    let g:powerset=powerset
    for coalition in powerset
	let value=Value(game,coalition)
" 	echo "coalition ".string(coalition)." Minus ".string(Minus(SubPowerSet(powerset,coalition),[coalition]))
" 	echo "coalition ".string(coalition)." Minus ".string(Minus(PowerSet(coalition),[coalition]))
	let rule_value=0
" 	for subset in Minus(SubPowerSet(powerset,coalition),[coalition])
	for subset in Minus(PowerSet(coalition),[coalition])
	    let rule_value+=Value(rules,subset)
	endfor
	let rule_value=(value-rule_value)
	call add(rules,[coalition,rule_value])
    endfor
    return rules
endfunction
command! -nargs=* -complete=var IeongShohamRules	:echo IeongShohamRules(<args>)
"}}}2
"}}}1
" {{{1 IEONG & SHOHAM SHAPLEY VALUE
" {{{2 IeongShohamShapleyValue /with only positive literals/
" this function takes as an argument the set of rules
" of Ieong and Shoham and computes the Shapley value.
" rules = [ [coalition,value], ... ] where coalition is a list representing
" /this I have to change into [ [coalition_pos, coalition_neg], value ] /
" a positive pattern, players is a list of players (it should contain all the
" players for which we want to compute the Shapley values). The players which
" do not appear in rules have value 0 so they do not count.
function! IeongShohamShapleyValue(rules,players)
    let rules=deepcopy(a:rules)
    let players=deepcopy(a:players)

    let values=[]
    for player in players
	let player_value=0
	for rule in rules
	    if SetBelongs(rule[0],player)
		let player_value+=rule[1]/len(rule[0])
	    endif
	endfor
	call add(values, [ player, player_value ])
    endfor
    return values
endfunction
command! -nargs=* -complete=var IeongShohamShapleyValue	:echo IeongShohamShapleyValue(<args>)
"{{{2 IeongShohamShapleyValue_wn /with negative literals/
" and this works with negative rules each rule must be of the form:
" [value, positive pattern, negative pattern] 
" negative pattern can be not present.
function! IeongShohamShapleyValue_wn(rules,...)

    if a:0 == 0
	let players=[]
	for rule in deepcopy(a:rules)
	    let new_players=Sum(copy(rule[1]),get(copy(rule),2,[])
	    let players=Sum(copy(players),copy(new_players))
	endfor
    else
	let players=deepcopy(a:1) 
    endif

"     echo "PLAYERS ISSV: ".string(sort(players))

    let rules=deepcopy(a:rules)

    let values=[]
    for player in players
	let player_value=0
	for rule in rules
	    let p=len(rule[1])
	    let n=len(get(rule,2,[]))
	    " we make a simple check, if player belongs to both positive and negative part we issue an error!
	    if SetBelongs(rule[1],player) && !SetBelongs(get(rule,2,[]),player)
		let player_value+=rule[0]/(p*Binomial(p+n,n))
	    elseif SetBelongs(get(rule,2,[]),player) && !SetBelongs(rule[1],player)
		let player_value-=rule[0]/(n*Binomial(p+n,p))
	    elseif SetBelongs(get(rule,2,[]),player) && SetBelongs(rule[1],player)
		echoerr "Rule ".string(rule)." is wrong."
	    endif
	endfor
	call add(values, [ player, player_value ])
    endfor
"     for player in values
" 	echo string(player)
"     endfor
    return values
endfunction
command! -nargs=* -complete=var IeongShohamShapleyValue		:echo IeongShohamShapleyValue(<args>)
"}}}3
"}}}2
"}}}1
"{{{1 WEIGHTED RULES TOOLS /to be written - weighted representation from a game/
"{{{2 WeightRulesToPFGame /not done/
" this function computes the game from weighted rule representation
" each weighted rule has to be of the form
" [bracket1,bracket2,...]
" where each bracket is of the form
" [pattern1,pattern2,...]
" where each pattern is of the form
" [value, coalition1,coaltion2]
" coalition2 might not be sepcified
" 	coalition1 = positive pattern
" 	coalition2 = negative pattern
" the result is of the form
" [ [coalition1, value],[coalition2, value ],...]
" where coalitions form a partition of players.
 
" the possible argument is a list of names of players, if not given is
" computed from the rules.  
" TO BE DONE:
" function! WeightedRulestoPFGame(WeightedRules,...)
" 
"     let rules=deepcopy(a:weightedRules)
" 
"     if a:0 == 1
" 	let players=copy(a:1)
"     else
" 	let players=[]
" 	for rule in w_rules
" 	    for bracket in rule
" 		for pattern in bracket
" 		    let players=Sum(copy(players),pattern[1])
" 		    let players=Sum(copy(players),get(pattern,2,[]))
" 		endfor
" 	    endfor
" 	endfor
" 	call sort(players)
"     endif
"     echo "PLAYERS ".string(players)
" 
"     let pfgame=[]
" 
"     let CoalitionStructures=CoalitionStructures(copy(players))
"     for cs in CoalitionStructures
" 	for rule in rules
" 	    " ther must exist a partition of cs
" 	    " such that every bracket is met by seprate cs.
" 	    let coal_str=RewriteCS(copy(players), copy())
" 	endfor
"     endfor
" endfunction

"}}}2
"}}}1
" {{{1 MYERSON VALUE /from weighted representation with negative literals!/
" the game must be represented by weighted rules without | (WeightedRulesE but
" not WeightedRulesC) i.e. each element of a weighted rule can have at most 1 element.
" players for which the value is computed are thos which appear in any rule.

" weighted_rules 	= list of rules
" weighted_rules[0] 	= a rule /should contain only one element!/
" weighted_rules[0][0]  = the rule that we will actually use 
"			  It is of the form: [[coalition], value ], thus:
" weighted_rules[0][0][0] = value
" weighted_rules[0][0][1] = coalition in (players which must be present)
" weighted_rules[0][0][2] = coalition out (players which must be omitted)
function! MyersonValue(weighted_rules)

    let w_rules=deepcopy(a:weighted_rules)

    if max(map(deepcopy(w_rules),'len(v:val)')) > 1
	echoerr "Wrong kind of rules, see desription of the function."
	return "Error"
    endif
    
    " Description:
    " w_rules[i] 	  - i-th rule
    " w_rules[i][0][j][0] - value of j-th pattern in i-th rule (coalition)
    " w_rules[i][0][j][1] - j-th pattern of i-th rule (coalition)

    let players=[]
    for w_rule in w_rules
	for pattern_wv in w_rule[0]
	    let players=Sum(players,pattern_wv[1])
	    let players=Sum(players,get(pattern_wv,2,[]))
	endfor
    endfor

    " compute tha Myerson values for each player
    let MyersonValues={}
    for player in players
	call extend(MyersonValues, { player : 0 } )
    endfor
    for w_rule in w_rules
" 	echo string(w_rule)
	" pattern_wv = [ coalition, value ] !
	for pattern_wv in w_rule[0]
	    let p=len(pattern_wv[1])
	    let n=len(get(pattern_wv,2,[]))
	    let p_value=FunctionF(p,n)*pattern_wv[0]
	    let n_value=(-1)*(p/n)*FunctionF(p,n)*pattern_wv[0]
	    " make a list of all patterns except the current one:
	    let w_rule_w=deepcopy(w_rule[0])
	    call remove(w_rule_w,index(w_rule_w,pattern_wv))

	    " and sum up all ngative patterns:
	    let pattern_n=[]
	    for pattern_wv_r in w_rule_w
		let pattern_n=Sum(copy(pattern_n),get(pattern_wv_r,2,[]))
	    endfor

	    " lines 3,4,5 and 7 (ECAI paper) are in different order here
	    " 4,3,5,7.
	    if pattern_n == []
		for player in pattern_wv[1]
		    " line 6 of Algorithm 3 in ECAI paper:
		    let MyersonValues[player]+=p_value
		endfor
		for player in get(pattern_wv,2,[])
		    " line 8 of Algorithm 3 in ECAI paper:
		    let MyersonValues[player]+=n_value
		endfor
	    endif
	endfor
    endfor
    echo MyersonValues
    return MyersonValues
endfunction
command! -nargs=1 -complete=var Myerson	:echo Myerson(<args>)
" }}}1
" {{{1 PHAM DO & NORDE 
"
" a:weighted_rules is a list of weighted rules without the restriction made
" for computing the Myerson Value
" it reutrns a list of Ieong Shoham rules in the form
" [value, positive pattern, negative pattern]
function! PhamDoNordeValue(weighted_rules)
    let w_rules=deepcopy(a:weighted_rules)

    echo "RULES: "
    for rule in w_rules 
	echo string(rule)
    endfor


    " Description:
    " w_rules[i] 	  - i-th rule
    " w_ruels[i][k]	  - k-th bracket ( | ) in i-th rule
    " w_rules[i][k][j][0] - value of j-th pattern of k-th bracket of i-th rule (coalition)
    " w_rules[i][k][j][1] - j-th (positive) pattern of k-th bracket of i-th rule (coalition)
    " w_rules[i][k][j][2] - j-th (negative) pattern of k-th bracket of i-th rule (coalition)

    let players=[]
    for rule in w_rules
	for bracket in rule
	    for pattern in bracket
		let players=Sum(copy(players),pattern[1])
		let players=Sum(copy(players),get(pattern,2,[]))
	    endfor
	endfor
    endfor
    call sort(players)
    echo "PLAYERS ".string(players)

    " (i) if two brackets have pattern longer (>) than 1
    " the is not counting.

    " These two lines
"     	let b:w_rules_ab=deepcopy(w_rules)
"     	call filter(b:w_rules_ab,'len(filter(v:val,"len(v:val) > 1")) <= 1')
    " are the same as the next for:endfor pair :) but vim seems to see the
    " second v:val eaual to the first one and that's why it it not possible to
    " make it that short.

    let w_rules_a=[[],[]]
    let g:w_rules_a=w_rules_a
    " w_rules_a[0] rules which all brackets are shortten ( <= ) than 1
    " w_rules_a[1] for which there exists only one bracket with pattern of lenght > 1 
    for rule in w_rules
	let check=0
	for bracket in rule 
	    let long_bracket=min([1,max(map(deepcopy(bracket),'len(v:val[1])'))-1])
	    let check+=long_bracket
	endfor 
	if check == 0
" 	    echo "passed 0   ".string(rule)
	    call add(w_rules_a[0],rule) 
	elseif check == 1
" 	    echo "passed 1   ".string(rule)
	    call add(w_rules_a[1],rule)
" 	else
" 	    echo "not passed ".string(rule)
	endif
    endfor
    let b:w_rules_ac=copy(w_rules_a)

    " ii a) /check compatibility, after Lemma 5.1 of our the ECAI paper:
    " 				'Computational Aspects of Extending the Shapley
    " 				Value to Coalitional Games with Externalities'
    for rule in w_rules_a[1]
" 	echo "RULE ".string(rule)
	for bracket in rule
	    let p_pattern=[]
	    let n_pattern=[]
	    for pos_pattern in bracket
		if len(pos_pattern[1]) > 1
		    let p_pattern=Sum(p_pattern,pos_pattern[1])
		    let n_pattern=Sum(n_pattern,get(pos_pattern,2,[]))
		endif
		" ii a)
		if Intersection(p_pattern,n_pattern) != []
" 		    echo "RULE REMOVED"
		    call remove(w_rules_a[1],index(w_rules_a[1],rule))
		endif
" 	    echo "p_pattern	".string(p_pattern)
" 	    echo "n_pattern	".string(n_pattern)
	    endfor
	endfor
	unlet p_pattern
	unlet n_pattern
    endfor

    " We compute the value in two setps
    " first we transform the rules into simple rules
    "  ieong_shoham_rules has as entries list: [value, pattern_pos, pattern_neg ]
    "  pattern_pos = the coalition that must be present
    "  pattern_neg = the coalition that must be absent
    "  value 	   = value of the rule
    let ieong_shoham_rules=[]
"     echo len(w_rules_a[1])." rule(s) in w_rules_a[1]"
    for rule in w_rules_a[1]
" 	echo "IS_RULE ".string(rule)
	"whole pattern:
	let is_pattern_pos=[]
	let is_pattern_neg=[]
	" only coming from long patterns (this is needed to check if it is
	" consistent (\oplus in the ECAI papaer notation))
	let is_pattern_l_pos=[]
	let is_pattern_l_neg=[]
	let is_value=0
	for bracket in rule
" 	    echo "IS_BRACKET " .string(bracket)
	    " check if this bracket has a pattern of length > 1
	    let long_bracket=min([1,max(map(deepcopy(bracket),'len(v:val[1])'))-1])
" 	    echo "LONG BRACKET ".string(long_bracket)
	    if long_bracket 
		" we must remember the long bracket for later:
		" it is unique /we filtered out already some rules .../ so it
		" won't be overwritten.
		let bracket_l=copy(bracket)
		for pattern in bracket
" 		    echo "IS PATTERN ".string(pattern)
		    if len(pattern[1]) > 1
			" Keep in mind that pattern[1] is the positive part and
			" pattern[2] is the negative /this might not be
			" defined/
			let is_pattern_l_pos=Sum(copy(is_pattern_l_pos),pattern[1]) 
			let is_pattern_l_neg=Sum(copy(is_pattern_l_neg),get(pattern,2,[]))
			let is_value+=pattern[0]
		    endif
		endfor
	    else
		for pattern in bracket
		    let is_pattern_neg=Sum(copy(is_pattern_neg),pattern[1])
" 		    echo "is_pattern_neg=".string(is_pattern_neg)
		endfor
	    endif
	endfor
	" line 3 in Algorithm 1:
	if Intersection(copy(is_pattern_l_pos), copy(is_pattern_l_neg)) == []
	    " note that we have already filtered out the rules which have more
	    " than (>) 1 bracket with at least one positive pattern of length>1.
	    let b_pattern=[ is_value, is_pattern_l_pos, Sum(copy(is_pattern_l_neg),copy(is_pattern_neg))]
	    " line 10 in Algorithm 1:
	    call add(ieong_shoham_rules,copy(b_pattern)) 
	    let b_pattern[0]=0
	    " lines 11-13 in Algorithm 1:
	    call filter(bracket_l,'len(v:val[1]) <= 1')
	    let b_pattern[0]=0 
	    for pattern in bracket_l 
		if Consistent([copy(b_pattern)[1], copy(b_pattern)[2]], [pattern[1], get(pattern,2,[])])
		    let b_pattern_copy=deepcopy(b_pattern)
		    " the value:
		    let b_pattern_copy[0]=pattern[0]
		    " the positive part:
		    let b_pattern_copy[1]=Sum(copy(b_pattern_copy)[1],copy(pattern)[1])
		    " and the negative part:
		    let b_pattern_copy[2]=Sum(copy(b_pattern_copy)[2],get(copy(pattern),2,[]))
		endif
	    " 13 of Algorithm 1:
	    call add(ieong_shoham_rules,b_pattern_copy) 
	    endfor
	endif
    endfor
    " lines 14-22 of Algorithm 1.
    for rule in w_rules_a[0]
	for bracket in rule
	    for pattern in bracket
		" line 16 of Algorithm 1:
		let b_pattern=[copy(pattern[1]), get(copy(pattern),2,[])]
		" lines 17,18 of Algorithm 1:
" 		echo "RULE  ".string(rule)." index ".index(copy(rule),copy(bracket))
		let rule_r=deepcopy(rule)
		call remove(rule_r,index(copy(rule_r),copy(bracket)))
" 		echo "RULE_R ".string(rule_r)
		for bracket_r in rule_r
		    for pattern_r in bracket_r
			" line 18 of Algorithm 1:
" 			echo string(b_pattern)."  ".string(pattern_r)
			let b_pattern[1]=Sum(deepcopy(b_pattern)[1], copy(pattern_r)[1])
		    endfor
		endfor
		" lines 19-21 of Algorithm 1: 
" 		echo "BRACKET   ".string(bracket)
		let bracket_r=deepcopy(bracket)
		call remove(bracket_r,index(copy(bracket_r),copy(pattern)))
" 		echo "BRACKET_R ".string(bracket_r)
		" Note: in the line 20 of Algorithm 1 it is not written precisly that
		" the condition is taken with a copy of b_pattern (not a shallow copy).
		let b_pattern_copy=b_pattern
		for pattern_r in bracket_r
		    " line 20 of Algorithm 1:
" 		    echo "PATTERN_R ".string(pattern_r)
		    if !Consistent(copy(b_pattern_copy),[copy(pattern_r)[1],get(copy(pattern_r),2,[])])
			" line 21 of ALgorithm 1:
			let b_pattern[1]=Sum(copy(b_pattern)[1],copy(pattern_r)[1])
		    endif
		endfor
		" line 22 of Algorithm 1:
		call add(ieong_shoham_rules,[pattern[0], b_pattern[0], b_pattern[1]])
	    endfor
	endfor
    endfor
    echo " Ieong Shoham Rules for this game:"
    for rule in ieong_shoham_rules
	echo rule
    endfor

"     compute the values
    let values=IeongShohamShapleyValue_wn(ieong_shoham_rules,players)
    echo "VALUES:"
    for value in values
	echo string(value)
    endfor
    return ieong_shoham_rules
endfunction
" }}}1
" {{{1 Consistent
" this function checks if the two patterns are consistent or not, i.e. if there
" exist a coalition which is simultaneously met by both of them.
function! Consistent(A,B)
    let A=deepcopy(a:A)
    let B=deepcopy(a:B)
    let int=Intersection(Sum(copy(A)[0],copy(B)[0]),Sum(copy(A)[1],copy(B)[1]))
    return int == []
endfunction
" }}}1
" {{{1 Mathematical Functions
" a and b are integers, returns a float.
function! FunctionF(a,b)
    let f=str2float(0)
    let j=0

    while j <= a:b
    let a=(float2nr(j/2)*2)
	if a == j
	    let f+=Binomial(a:b,j)/str2float((a:a+j))
" 	    echo "+ ".float2nr(f*1000) ."  ". float2nr(Binomial(a:b,j)) ."  ".(a:a+j)."  ".float2nr(Binomial(a:b,j)/str2float((a:a+j))*1000) 
	else
	    let f-=Binomial(a:b,j)/str2float((a:a+j))
" 	    echo "- ".float2nr(f*1000) ."  ". float2nr(Binomial(a:b,j)) ."  ".(a:a+j)."  ".float2nr(Binomial(a:b,j)/str2float((a:a+j))*1000)
	endif
	let j+=1
    endwhile
    return f
endfunction
" works up to Factorial(19)
function! Factorial(a)
    let i=1
    let f=1
    while  i <= a:a
	let f=f*i
	let i+=1
    endwhile
    return f
endfunction
" a>b, works up to Binomial(12,x) but the value might be wrong?
function! Binomial(a,b) 
    return str2float(Factorial(a:a))/str2float((Factorial(a:b)*Factorial(a:a-a:b)))
endfunction
"}}}1
"{{{1 Tool Box
" {{{2 ShowCGame
" show the values of a Caolitional Game with externalities written in a condensed form (like GameC).
function! ShowCGame(game)
    let game=deepcopy(a:game)
    let new_game=ExpandGame(game)

    let show=[]

    for coalition in new_game
	let len=len(string(coalition[0]))
	let i=1
	let space=""
	while i <= 60-len
	    let space.=" "
	    let i+=1
	endwhile

	" SHOW GAME
	echo string(coalition[0]).space." ".coalition[1])
	call add(show,coalition[0]).space." ".coalition[1])
    endfor
    return show
endfunction
command! -nargs=1 -complete=var ShowCGame	:call ShowGame(<args>)
" }}}2
" {{{2 ShowPFGame
" show a game with externalities (partition form game = pgame)
function! ShowPFGame(game) 
    let game=deepcopy(a:game)
    let show=[]

    for coalition in game
	let len=len(string(coalition[0][0]))
	let i=1
	let space_a=""
	while i <= 30-len
	    let space_a.=" "
	    let i+=1
	endwhile
	let len=len(string(coalition[0][1]))
	let i=1
	let space_b=""
	while i <= 30-len
	    let space_b.=" "
	    let i+=1
	endwhile

	" SHOW GAME
	echo string(coalition[0][0]).space_a." ".string(coalition[0][1]).space_b." ".string(coalition[1])
	call add(show,string(coalition[0][0]).space_a." ".string(coalition[0][1]).space_b." ".string(coalition[1]))
    endfor
    return show
endfunction
command! -nargs=1 -complete=var ShowPFGame	:call ShowPFGame(<args>)
"}}}2
" {{{2 ExpandGame
" rewrite a game with externalities from the compact form to 
" [[embedded_coalition,value],...
function! ExpandGame(game) 
    let game=deepcopy(a:game)
    let new_game=[]

    " wv = with values
    for coalition_structure_wv in game
	" make the partition which will be used for all coalition_wv's
	" belonging to this coalition_structure_wv
	let partition=[]
	for coalition_wv in coalition_structure_wv
	    call add(partition,coalition_wv[0])
	endfor
	call sort(partition,"SortCS")
	call map(partition,'sort(v:val)')
" 	echo partition

	for coalition_wv in coalition_structure_wv
	    let embedded_coalition=[coalition_wv[0],partition]
	    let value=coalition_wv[1]
	    call add(new_game,[embedded_coalition,value])
	endfor
    endfor
    return new_game
endfunction
command! -nargs=1 -complete=var ExpandGame	:echo ExpandGame(<args>)
"}}}2
"{{{2 RewriteCS
" Rewrite a coalition structure with new names
" a:cs_names=[coalition_structure,names]  
" 	names list of names (labels) of agents
" a:new_names = list of new names
function! RewriteCS(cs_names,new_names)
    let names=deepcopy(a:cs_names[1])
    let old_coalitionstructures=deepcopy(a:cs_names[0])
    let new_names=deepcopy(a:new_names)
    if len(names) != len(new_names)
	echoerr "Rewrite Lenght Error"
	return 1
    endif

    let new

    " make a dictionary
    let dict={}
    let i=1 
    for name in names 
	call extend(dict, name : new_names[i])
	let i+=1
    endfor

    let new_coalitionstructures=[]
    " map the old_cs to new one 
    for cs in old_coalitionstructures
	let new_cs=map(copy(cs),'dict[v:val]') 
	call extend(new_coalitionstructures,new_cs)
    endfor

    " return value
    return new_coalitionstructures
endfunction
"}}}2
"{{{2 ValueCS
" the argument a:embedded_coalition is usually written in mathematical
" notation as:
"  		(coalition,partition), coalition \in partition
" here it is a list of two lists:
"  		[coalition,partition]
function! ValueCS(game,embedded_coalition)
    " we have to rewrite the game to take the value.
    let embedded=deepcopy(a:embedded_coalition)
    let game=deepcopy(a:game)
    let new_game=ExpandGame(game)
    unlet game

    let embedded[1]=sort(embedded[1],"SortCS")
    let embedded[0]=sort(embedded[0])
    call map(embedded[1],'sort(v:val)') 
"     echo string(embedded[0])." ".string(embedded[1])

    for em_coalition_wv in new_game
	if em_coalition_wv[0] == embedded
	    return em_coalition_wv[1]
	endif
    endfor
    echoerr "no value for coalition ".string(embedded) 
    return 0
endfunction
"}}}2
"{{{2 Check
function! Check(coalition_structures)
    for coalition_structure in a:coalition_structures
	let l:count=count(a:coalition_structures,coalition_structure)
	if l:count != 1
	    echoerr "coalition_structure ".coalition_structure." is multiple: ".l:count 
	    return 1
	endif
    endfor
    return 0
endfunction
command! -nargs=1 -complete=var Check	:call Check(<args>)
"}}}2
"{{{2 CoalitionStructures
" make a list of all coalition structures of the set of players.
" players is a list of player names for example: [ 'Marcin', 'Dorota', 'Tomek' ]
" the function goes inductively
function! CoalitionStructures(players,...)

    let players=deepcopy(a:players)

    " 1st step of induction
    if len(players) == 1
	return [[players]]
    endif
    
    " make power set of the players.
    if a:0 >= 1
	let powerset=a:1
    else
	let powerset=PowerSet(players)
    endif

    " a dictionary of things done { length :  [ CoalitionStructure, players] }
    let dict={}

    " induction step 
    let s:i=1
    for coalition in Minus(powerset,[[]])
	let rest=Minus(players,coalition)
	if rest != []
	    if index(keys(dict),len(rest)) != -1
		let coalition_structure_rest=RewriteCS(dict[len(rest)],rest)
	    else
		let coalition_structures_rest=CoalitionStructures(rest,SubPowerSet(deepcopy(powerset),rest))
		call extend(dict, { len(rest) : [deepcopy(coalition_structures_rest), deepcopy(rest)]})
	    endif
	    if exists("coalition_structures")
		let coalition_structures_rest=map(deepcopy(coalition_structures_rest),'add(v:val,deepcopy(coalition))')
		let coalition_structures=extend(coalition_structures,coalition_structures_rest)
		call map(coalition_structures,'SortFunction(v:val)')
	    else
		let coalition_structures=map(deepcopy(coalition_structures_rest),'add(v:val,deepcopy(coalition))')
	    endif
	    let s:i+=1
	else
	    let coalition_structures=add(coalition_structures,[coalition])
	endif
    endfor
    call map(coalition_structures,'SortFunction(v:val)')
    return RemoveDoubleEntries(coalition_structures)
endfunction
"}}}2
"{{{2 PowerSet
" make a list of all coalitions on the set of n players 1,2,...,n
" players is a list of player names for example: [ 'Marcin', 'Dorota', 'Tomek' ]
function! PowerSet(players)
    let n=len(a:players)
    let power_set=[[]]
    let i=1
    while i<=n
	for coalition in power_set 
	    for player in Minus(a:players,coalition)
		let coalition_extended=RemoveDoubleEntries(add(deepcopy(coalition),player))
		if !Belongs(power_set,deepcopy(coalition_extended))
		    call add(power_set,deepcopy(sort(coalition_extended)))
		endif
		unlet coalition_extended
	    endfor
	endfor
	let i+=1
    endwhile
    return power_set
endfunction
"}}}2
"{{{2 RemoveDoubleEntries
function! RemoveDoubleEntries(list)
    let list=deepcopy(a:list)
    return sort(filter(list,'count(list,v:val) == 1'))
endfunction
" }}}2
" {{{2 RemoveDoubleEntriesCS
" this removes double entries form coalition structure list which is of the
" form:
" [ [ [set1], [set2], ... , [setn] ], ... ]  
function! RemoveDoubleEntriesCS(list) 
    let list=deepcopy(a:list)
    let clean_list=[]
    for coalition_structure in list
	let check=0
	for cs in clean_list
	    if CompareSetsCS(cs,coalition_structure) == 1
		let check = 1
		break
	    endif
	endfor
	if check
	    call add(clean_list,coalition_structure)
	endif
    endfor
endfunction
"}}}2
"{{{2 CompareSetsCS /not implemented/
" compares two list of coalition structures as they were sets and their
" element were also sets.
" two list are eaqal if in add(A,B) every element is counted twice!
" function! CompareSetsCS(A,B)
"     let A=deepcopy(a:A)
"     let B=deepcopy(a:B)
" endfunction
"}}}2
"{{{2 CompareSets
" compares two list as they were sets
" two list are eaqal if in add(A,B) every element is counted twice!
function! CompareSets(A,B)
    let C=deepcopy(extend(deepcopy(a:A),deepcopy(a:B)))
    let D=deepcopy(C)
    call filter(C,'count(D,v:val) != 2')
    if C == []
	return 1
    else
	return 0
    endif
" the first algorithm is two times faster :)
"     let A=deepcopy(a:A)
"     let B=deepcopy(a:B)
"     let C=len(Minus(A,B))
"     let D=len(Minus(B,A))
"     return !(C || D)
endfunction
"}}}2 
"{{{2 Belongs
" A is a set of coalitions (represented by a list) 
" B is a coalition (represented by a list)
" return 1 iff B is present in A
function! Belongs(A,B)
    for coalition in a:A
	if CompareSets(deepcopy(coalition),deepcopy(a:B))
	    return 1
	endif
    endfor
    return 0
endfunction
"}}}2
"{{{2 SetBelongs
" A is a set (represented by a list) B is an element (string)
" return 1 if B belongs to A
function! SetBelongs(A,B)
    let A=deepcopy(a:A)
    return len(filter(A,'v:val == a:B'))
endfunction
"}}}2
"{{{2 Sorting Functions
" {{{3 Sort
" A is sorted after B if len(A) > len(B)
" as required by sort() function ':h sort()'
function! Sort(A,B)
    let a=len(a:A)
    let b=len(a:B)
"     echo string(a:A)."   ".string(a:B)
    if a == b
" 	echo 0
	return 0
    elseif a > b
" 	echo 1
	return 1
    else
" 	echo -1
	return -1
    endif
endfunction
"}}}3 
"{{{3 SortCS /not used anymore, to report a possible bug in vim/
" sort entries of a coalition structure, for example
" [['a'], ['b', 'c']]
" A and B are two entries of a coalition structures
" Note: entries of both list should be sorted!
" we can not use sorting function recursively :(.
"{{{4
function! SortCS(A,B)
    let A=deepcopy(a:A)
    let B=deepcopy(a:B)
    let A=sort(deepcopy(A))
    let B=sort(deepcopy(B))
    let a=len(A)
    let b=len(B)
"     echomsg string(A)."   ".string(B)
    if a == b
	if A == B
" 	    echo 0
	    return 0
	elseif sort([join(A,''),join(B,'')]) == [join(A,''),join(B,'')]
" 	    echo -1 
	    return -1
	else
" 	    echo 1
	    return 1
	endif
    elseif a > b
" 	echo 1
	return 1
    else
" 	echo -1
	return -1
    endif
endfunction
"}}}4
"{{{4 THIS IS WERE VIM GOES DOWN !!! a possible BUG !!! /to be reported/.
" this is a sorting function:
function! SORTCS(A,B)
    let A=copy(a:A)
    let B=copy(a:B)
"     let A=sort(deepcopy(A))
"     let B=sort(deepcopy(B))
    let a=len(A)
    let b=len(B)
"     echomsg string(A)."   ".string(B)
    if a == b
	if A == B
" 	    echo 0
	    return 0
	elseif sort([join(A,''),join(B,'')]) == [join(A,''),join(B,'')]
" 	    echo -1 
	    return -1
	else
" 	    echo 1
	    return 1
	endif
    elseif a > b
" 	echo 1
	return 1
    else
" 	echo -1
	return -1
    endif
endfunction
"}}}4
"}}}3
"{{{3 SortFunction
" A is a list of list of strings
" For a b\in A a > b if join(sort(a)) > join(sort(b)) in lexicographical order.
" Note: this works if A doesn't have two identical elememnts (in the above
" sense) which is the case I use. 
function! SortFunction(A)
    let A=deepcopy(a:A)

    " first make a dictionary of labels = join(sort(a))
    let labels_dict={}
    for a in A
	" if I add deepcopy to here then the returns a's are not sorted!
	let label=join(sort(a))
	" issue an error message if two elements have the same label
	call extend(labels_dict,{ label : a }, "error")
    endfor
    
    " make a list of labels and sort it
    let sorted_list=sort(keys(labels_dict))

    " translate labels
    let sorted_list=map(sorted_list,'labels_dict[v:val]') 
    return sorted_list
endfunction
"}}}3
"{{{3 SortLabels
" sort labels as described above
" I assume that A and B are labels already in alphabetical order!
function! SortLabels(A,B) 
    let A=a:A
    let B=a:B
    if len(A) > len(B) 
	return 1
    elseif len(A) < len(B)
	return -1
    elseif len(A) == len(B)
	let i=0
	let true = 1
	while true
	for char_A in split(A)
	    let char_B=B[i]
	    let true = sort(['char_A', 'char_B']) == ['char_A', 'char_B']
	    let i+=1
	endfor
	if true 
	    return 1
	elseif true && i == len(A)
	    return 0
	else
	    return -1
	endif
    endif
endfunction
"}}}3
"}}}2   
"{{{2 Value
" game = [ [coalition,value], ... ] coalition = a list (in both places!)
function! Value(game,coalition)
    let game=deepcopy(a:game)
    return get(filter(game,'CompareSets(v:val[0],a:coalition)'),0,'[0,0]')[1]
endfunction
"}}}2
"{{{2 Minus (set theoretic)
" Set A-B A,B are lists but treated as sets
function! Minus(A,B)
    let A=deepcopy(a:A)
    let B=deepcopy(a:B)
    return filter(A,'count(B,v:val) == 0')
endfunction
"}}}2
"{{{2 Sum (set theoretic)
function! Sum(A,B) 
    let A=deepcopy(a:A)
    let B=deepcopy(a:B)
    return extend(A,Minus(B,A))
endfunction
"}}}2
"{{{2 Intersection (set theoretic)
function! Intersection(A,B)
    let A=deepcopy(a:A)
    let B=deepcopy(a:B)
    let I=[]
    for a in A
	if index(B,a) != '-1'
	    call add(I,a)
	endif
    endfor
    return I
endfunction
"}}}2
"{{{2 SubPowerSet
" players list of players but this doesn't work if players are coalitions! ad
" in McQuillin ShapleyValue
function! SubPowerSet(powerset,players)
    let pattern='^['.join(a:players,'').']*$'
    let powerset=deepcopy(a:powerset)
    return filter(powerset,'join(v:val,"") =~ pattern')
endfunction
"}}}2
"}}}1
"{{{1 Coalition Structures for 7 players
" CoalitionStructure generated for 7 players (there are 877 of them): 
" 	for 2 players  - there are 	3
" 	for 3				5
" 	for 4				15
" 	for 5				52	
" 	for 6				203
"	for 7				877 
"	for 8				4140 	/it took ~ 100 minutes/

let bigcoalition_structure=[
		\ [['a' , 'b' , 'c' , 'e' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'c' , 'e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'b' , 'c' , 'e' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'c' , 'e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'f' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'b' , 'c' , 'f' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'f' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'f' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'b' , 'c' , 'f' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'b' , 'c' , 'f' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'f' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'f' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'c' , 'f' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'f' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'g' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'b' , 'c' , 'g' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'b' , 'c' , 'g' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'g' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'g' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'c' , 'g' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'c' , 'g' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'c' , 'h' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'b' , 'c' , 'h' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'b' , 'c' , 'h' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'b' , 'c' , 'h' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'c' , 'h' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'c' ], ['e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'b' , 'c' ], ['e' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'c' ], ['e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'c' ], ['e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'c' ], ['e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'c' ], ['e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'e' , 'f' , 'g' , 'h' ], ['c' ]],
		\ [['a' , 'b' , 'e' , 'f' , 'g' ], ['c' , 'h' ]],
		\ [['a' , 'b' , 'e' , 'f' , 'g' ], ['c' ], ['h' ]],
		\ [['a' , 'b' , 'e' , 'f' , 'h' ], ['c' , 'g' ]],
		\ [['a' , 'b' , 'e' , 'f' , 'h' ], ['c' ], ['g' ]],
		\ [['a' , 'b' , 'e' , 'f' ], ['c' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'e' , 'f' ], ['c' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'e' , 'f' ], ['c' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'e' , 'f' ], ['c' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'e' , 'f' ], ['c' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'e' , 'g' , 'h' ], ['c' , 'f' ]],
		\ [['a' , 'b' , 'e' , 'g' , 'h' ], ['c' ], ['f' ]],
		\ [['a' , 'b' , 'e' , 'g' ], ['c' , 'f' , 'h' ]],
		\ [['a' , 'b' , 'e' , 'g' ], ['c' , 'f' ], ['h' ]],
		\ [['a' , 'b' , 'e' , 'g' ], ['c' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'e' , 'g' ], ['c' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'e' , 'g' ], ['c' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'e' , 'h' ], ['c' , 'f' , 'g' ]],
		\ [['a' , 'b' , 'e' , 'h' ], ['c' , 'f' ], ['g' ]],
		\ [['a' , 'b' , 'e' , 'h' ], ['c' , 'g' ], ['f' ]],
		\ [['a' , 'b' , 'e' , 'h' ], ['c' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'e' , 'h' ], ['c' ], ['f' ], ['g' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'e' ], ['c' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'b' , 'e' ], ['c' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'e' ], ['c' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'e' ], ['c' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'e' ], ['c' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'e' ], ['c' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'f' , 'g' , 'h' ], ['c' , 'e' ]],
		\ [['a' , 'b' , 'f' , 'g' , 'h' ], ['c' ], ['e' ]],
		\ [['a' , 'b' , 'f' , 'g' ], ['c' , 'e' , 'h' ]],
		\ [['a' , 'b' , 'f' , 'g' ], ['c' , 'e' ], ['h' ]],
		\ [['a' , 'b' , 'f' , 'g' ], ['c' , 'h' ], ['e' ]],
		\ [['a' , 'b' , 'f' , 'g' ], ['c' ], ['e' , 'h' ]],
		\ [['a' , 'b' , 'f' , 'g' ], ['c' ], ['e' ], ['h' ]],
		\ [['a' , 'b' , 'f' , 'h' ], ['c' , 'e' , 'g' ]],
		\ [['a' , 'b' , 'f' , 'h' ], ['c' , 'e' ], ['g' ]],
		\ [['a' , 'b' , 'f' , 'h' ], ['c' , 'g' ], ['e' ]],
		\ [['a' , 'b' , 'f' , 'h' ], ['c' ], ['e' , 'g' ]],
		\ [['a' , 'b' , 'f' , 'h' ], ['c' ], ['e' ], ['g' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'e' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'e' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'e' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'e' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'e' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'b' , 'f' ], ['c' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'b' , 'f' ], ['c' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'b' , 'f' ], ['c' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'b' , 'f' ], ['c' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'b' , 'f' ], ['c' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'b' , 'f' ], ['c' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'b' , 'g' , 'h' ], ['c' , 'e' , 'f' ]],
		\ [['a' , 'b' , 'g' , 'h' ], ['c' , 'e' ], ['f' ]],
		\ [['a' , 'b' , 'g' , 'h' ], ['c' , 'f' ], ['e' ]],
		\ [['a' , 'b' , 'g' , 'h' ], ['c' ], ['e' , 'f' ]],
		\ [['a' , 'b' , 'g' , 'h' ], ['c' ], ['e' ], ['f' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'e' , 'f' , 'h' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'e' , 'f' ], ['h' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'e' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'e' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'e' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'f' , 'h' ], ['e' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'f' ], ['e' , 'h' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'f' ], ['e' ], ['h' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'b' , 'g' ], ['c' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'b' , 'g' ], ['c' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'b' , 'g' ], ['c' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'b' , 'g' ], ['c' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'b' , 'g' ], ['c' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'b' , 'g' ], ['c' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'e' , 'f' , 'g' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'e' , 'f' ], ['g' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'e' , 'g' ], ['f' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'e' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'e' ], ['f' ], ['g' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'f' , 'g' ], ['e' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'f' ], ['e' , 'g' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'f' ], ['e' ], ['g' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'g' ], ['e' , 'f' ]],
		\ [['a' , 'b' , 'h' ], ['c' , 'g' ], ['e' ], ['f' ]],
		\ [['a' , 'b' , 'h' ], ['c' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'b' , 'h' ], ['c' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'b' , 'h' ], ['c' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'b' , 'h' ], ['c' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'b' , 'h' ], ['c' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'b' ], ['c' , 'e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'e' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'f' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'b' ], ['c' , 'f' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'f' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'f' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'b' ], ['c' , 'f' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'f' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'f' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'f' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'f' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'f' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'g' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'b' ], ['c' , 'g' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'b' ], ['c' , 'g' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'g' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'g' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'b' ], ['c' , 'g' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'b' ], ['c' , 'g' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'b' ], ['c' , 'h' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'b' ], ['c' , 'h' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'b' ], ['c' , 'h' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'b' ], ['c' , 'h' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'b' ], ['c' , 'h' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'b' ], ['c' ], ['e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'b' ], ['c' ], ['e' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'b' ], ['c' ], ['e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'b' ], ['c' ], ['e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'c' , 'e' , 'f' , 'g' , 'h' ], ['b' ]],
		\ [['a' , 'c' , 'e' , 'f' , 'g' ], ['b' , 'h' ]],
		\ [['a' , 'c' , 'e' , 'f' , 'g' ], ['b' ], ['h' ]],
		\ [['a' , 'c' , 'e' , 'f' , 'h' ], ['b' , 'g' ]],
		\ [['a' , 'c' , 'e' , 'f' , 'h' ], ['b' ], ['g' ]],
		\ [['a' , 'c' , 'e' , 'f' ], ['b' , 'g' , 'h' ]],
		\ [['a' , 'c' , 'e' , 'f' ], ['b' , 'g' ], ['h' ]],
		\ [['a' , 'c' , 'e' , 'f' ], ['b' , 'h' ], ['g' ]],
		\ [['a' , 'c' , 'e' , 'f' ], ['b' ], ['g' , 'h' ]],
		\ [['a' , 'c' , 'e' , 'f' ], ['b' ], ['g' ], ['h' ]],
		\ [['a' , 'c' , 'e' , 'g' , 'h' ], ['b' , 'f' ]],
		\ [['a' , 'c' , 'e' , 'g' , 'h' ], ['b' ], ['f' ]],
		\ [['a' , 'c' , 'e' , 'g' ], ['b' , 'f' , 'h' ]],
		\ [['a' , 'c' , 'e' , 'g' ], ['b' , 'f' ], ['h' ]],
		\ [['a' , 'c' , 'e' , 'g' ], ['b' , 'h' ], ['f' ]],
		\ [['a' , 'c' , 'e' , 'g' ], ['b' ], ['f' , 'h' ]],
		\ [['a' , 'c' , 'e' , 'g' ], ['b' ], ['f' ], ['h' ]],
		\ [['a' , 'c' , 'e' , 'h' ], ['b' , 'f' , 'g' ]],
		\ [['a' , 'c' , 'e' , 'h' ], ['b' , 'f' ], ['g' ]],
		\ [['a' , 'c' , 'e' , 'h' ], ['b' , 'g' ], ['f' ]],
		\ [['a' , 'c' , 'e' , 'h' ], ['b' ], ['f' , 'g' ]],
		\ [['a' , 'c' , 'e' , 'h' ], ['b' ], ['f' ], ['g' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'c' , 'e' ], ['b' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'c' , 'e' ], ['b' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'c' , 'e' ], ['b' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'c' , 'e' ], ['b' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'c' , 'e' ], ['b' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'c' , 'e' ], ['b' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'c' , 'f' , 'g' , 'h' ], ['b' , 'e' ]],
		\ [['a' , 'c' , 'f' , 'g' , 'h' ], ['b' ], ['e' ]],
		\ [['a' , 'c' , 'f' , 'g' ], ['b' , 'e' , 'h' ]],
		\ [['a' , 'c' , 'f' , 'g' ], ['b' , 'e' ], ['h' ]],
		\ [['a' , 'c' , 'f' , 'g' ], ['b' , 'h' ], ['e' ]],
		\ [['a' , 'c' , 'f' , 'g' ], ['b' ], ['e' , 'h' ]],
		\ [['a' , 'c' , 'f' , 'g' ], ['b' ], ['e' ], ['h' ]],
		\ [['a' , 'c' , 'f' , 'h' ], ['b' , 'e' , 'g' ]],
		\ [['a' , 'c' , 'f' , 'h' ], ['b' , 'e' ], ['g' ]],
		\ [['a' , 'c' , 'f' , 'h' ], ['b' , 'g' ], ['e' ]],
		\ [['a' , 'c' , 'f' , 'h' ], ['b' ], ['e' , 'g' ]],
		\ [['a' , 'c' , 'f' , 'h' ], ['b' ], ['e' ], ['g' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'e' , 'g' , 'h' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'e' , 'g' ], ['h' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'e' , 'h' ], ['g' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'e' ], ['g' , 'h' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'e' ], ['g' ], ['h' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'c' , 'f' ], ['b' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'c' , 'f' ], ['b' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'c' , 'f' ], ['b' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'c' , 'f' ], ['b' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'c' , 'f' ], ['b' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'c' , 'f' ], ['b' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'c' , 'g' , 'h' ], ['b' , 'e' , 'f' ]],
		\ [['a' , 'c' , 'g' , 'h' ], ['b' , 'e' ], ['f' ]],
		\ [['a' , 'c' , 'g' , 'h' ], ['b' , 'f' ], ['e' ]],
		\ [['a' , 'c' , 'g' , 'h' ], ['b' ], ['e' , 'f' ]],
		\ [['a' , 'c' , 'g' , 'h' ], ['b' ], ['e' ], ['f' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'e' , 'f' , 'h' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'e' , 'f' ], ['h' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'e' , 'h' ], ['f' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'e' ], ['f' , 'h' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'e' ], ['f' ], ['h' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'f' , 'h' ], ['e' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'f' ], ['e' , 'h' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'f' ], ['e' ], ['h' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'c' , 'g' ], ['b' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'c' , 'g' ], ['b' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'c' , 'g' ], ['b' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'c' , 'g' ], ['b' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'c' , 'g' ], ['b' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'c' , 'g' ], ['b' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'e' , 'f' , 'g' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'e' , 'f' ], ['g' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'e' , 'g' ], ['f' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'e' ], ['f' , 'g' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'e' ], ['f' ], ['g' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'f' , 'g' ], ['e' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'f' ], ['e' , 'g' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'f' ], ['e' ], ['g' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'g' ], ['e' , 'f' ]],
		\ [['a' , 'c' , 'h' ], ['b' , 'g' ], ['e' ], ['f' ]],
		\ [['a' , 'c' , 'h' ], ['b' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'c' , 'h' ], ['b' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'c' , 'h' ], ['b' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'c' , 'h' ], ['b' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'c' , 'h' ], ['b' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'c' ], ['b' , 'e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'e' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'f' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'c' ], ['b' , 'f' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'f' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'f' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'c' ], ['b' , 'f' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'f' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'f' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'f' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'f' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'f' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'g' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'c' ], ['b' , 'g' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'c' ], ['b' , 'g' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'g' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'g' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'c' ], ['b' , 'g' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'c' ], ['b' , 'g' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'c' ], ['b' , 'h' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'c' ], ['b' , 'h' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'c' ], ['b' , 'h' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'c' ], ['b' , 'h' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'c' ], ['b' , 'h' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'c' ], ['b' ], ['e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'c' ], ['b' ], ['e' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'c' ], ['b' ], ['e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'c' ], ['b' ], ['e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'e' , 'f' , 'g' , 'h' ], ['b' , 'c' ]],
		\ [['a' , 'e' , 'f' , 'g' , 'h' ], ['b' ], ['c' ]],
		\ [['a' , 'e' , 'f' , 'g' ], ['b' , 'c' , 'h' ]],
		\ [['a' , 'e' , 'f' , 'g' ], ['b' , 'c' ], ['h' ]],
		\ [['a' , 'e' , 'f' , 'g' ], ['b' , 'h' ], ['c' ]],
		\ [['a' , 'e' , 'f' , 'g' ], ['b' ], ['c' , 'h' ]],
		\ [['a' , 'e' , 'f' , 'g' ], ['b' ], ['c' ], ['h' ]],
		\ [['a' , 'e' , 'f' , 'h' ], ['b' , 'c' , 'g' ]],
		\ [['a' , 'e' , 'f' , 'h' ], ['b' , 'c' ], ['g' ]],
		\ [['a' , 'e' , 'f' , 'h' ], ['b' , 'g' ], ['c' ]],
		\ [['a' , 'e' , 'f' , 'h' ], ['b' ], ['c' , 'g' ]],
		\ [['a' , 'e' , 'f' , 'h' ], ['b' ], ['c' ], ['g' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'c' , 'g' , 'h' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'c' , 'g' ], ['h' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'c' , 'h' ], ['g' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'c' ], ['g' , 'h' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'c' ], ['g' ], ['h' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'g' , 'h' ], ['c' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'g' ], ['c' , 'h' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'g' ], ['c' ], ['h' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'h' ], ['c' , 'g' ]],
		\ [['a' , 'e' , 'f' ], ['b' , 'h' ], ['c' ], ['g' ]],
		\ [['a' , 'e' , 'f' ], ['b' ], ['c' , 'g' , 'h' ]],
		\ [['a' , 'e' , 'f' ], ['b' ], ['c' , 'g' ], ['h' ]],
		\ [['a' , 'e' , 'f' ], ['b' ], ['c' , 'h' ], ['g' ]],
		\ [['a' , 'e' , 'f' ], ['b' ], ['c' ], ['g' , 'h' ]],
		\ [['a' , 'e' , 'f' ], ['b' ], ['c' ], ['g' ], ['h' ]],
		\ [['a' , 'e' , 'g' , 'h' ], ['b' , 'c' , 'f' ]],
		\ [['a' , 'e' , 'g' , 'h' ], ['b' , 'c' ], ['f' ]],
		\ [['a' , 'e' , 'g' , 'h' ], ['b' , 'f' ], ['c' ]],
		\ [['a' , 'e' , 'g' , 'h' ], ['b' ], ['c' , 'f' ]],
		\ [['a' , 'e' , 'g' , 'h' ], ['b' ], ['c' ], ['f' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'c' , 'f' , 'h' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'c' , 'f' ], ['h' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'c' , 'h' ], ['f' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'c' ], ['f' , 'h' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'c' ], ['f' ], ['h' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'f' , 'h' ], ['c' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'f' ], ['c' , 'h' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'f' ], ['c' ], ['h' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'h' ], ['c' , 'f' ]],
		\ [['a' , 'e' , 'g' ], ['b' , 'h' ], ['c' ], ['f' ]],
		\ [['a' , 'e' , 'g' ], ['b' ], ['c' , 'f' , 'h' ]],
		\ [['a' , 'e' , 'g' ], ['b' ], ['c' , 'f' ], ['h' ]],
		\ [['a' , 'e' , 'g' ], ['b' ], ['c' , 'h' ], ['f' ]],
		\ [['a' , 'e' , 'g' ], ['b' ], ['c' ], ['f' , 'h' ]],
		\ [['a' , 'e' , 'g' ], ['b' ], ['c' ], ['f' ], ['h' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'c' , 'f' , 'g' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'c' , 'f' ], ['g' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'c' , 'g' ], ['f' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'c' ], ['f' , 'g' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'c' ], ['f' ], ['g' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'f' , 'g' ], ['c' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'f' ], ['c' , 'g' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'f' ], ['c' ], ['g' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'g' ], ['c' , 'f' ]],
		\ [['a' , 'e' , 'h' ], ['b' , 'g' ], ['c' ], ['f' ]],
		\ [['a' , 'e' , 'h' ], ['b' ], ['c' , 'f' , 'g' ]],
		\ [['a' , 'e' , 'h' ], ['b' ], ['c' , 'f' ], ['g' ]],
		\ [['a' , 'e' , 'h' ], ['b' ], ['c' , 'g' ], ['f' ]],
		\ [['a' , 'e' , 'h' ], ['b' ], ['c' ], ['f' , 'g' ]],
		\ [['a' , 'e' , 'h' ], ['b' ], ['c' ], ['f' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'e' ], ['b' , 'c' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'c' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'c' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'c' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'c' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'c' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'f' , 'g' , 'h' ], ['c' ]],
		\ [['a' , 'e' ], ['b' , 'f' , 'g' ], ['c' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'f' , 'g' ], ['c' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'f' , 'h' ], ['c' , 'g' ]],
		\ [['a' , 'e' ], ['b' , 'f' , 'h' ], ['c' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'f' ], ['c' , 'g' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'f' ], ['c' , 'g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'f' ], ['c' , 'h' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'f' ], ['c' ], ['g' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'f' ], ['c' ], ['g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'g' , 'h' ], ['c' , 'f' ]],
		\ [['a' , 'e' ], ['b' , 'g' , 'h' ], ['c' ], ['f' ]],
		\ [['a' , 'e' ], ['b' , 'g' ], ['c' , 'f' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'g' ], ['c' , 'f' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'g' ], ['c' , 'h' ], ['f' ]],
		\ [['a' , 'e' ], ['b' , 'g' ], ['c' ], ['f' , 'h' ]],
		\ [['a' , 'e' ], ['b' , 'g' ], ['c' ], ['f' ], ['h' ]],
		\ [['a' , 'e' ], ['b' , 'h' ], ['c' , 'f' , 'g' ]],
		\ [['a' , 'e' ], ['b' , 'h' ], ['c' , 'f' ], ['g' ]],
		\ [['a' , 'e' ], ['b' , 'h' ], ['c' , 'g' ], ['f' ]],
		\ [['a' , 'e' ], ['b' , 'h' ], ['c' ], ['f' , 'g' ]],
		\ [['a' , 'e' ], ['b' , 'h' ], ['c' ], ['f' ], ['g' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'f' , 'g' , 'h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'f' , 'g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'f' , 'h' ], ['g' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'f' ], ['g' , 'h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'f' ], ['g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'g' , 'h' ], ['f' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'g' ], ['f' , 'h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'g' ], ['f' ], ['h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'h' ], ['f' , 'g' ]],
		\ [['a' , 'e' ], ['b' ], ['c' , 'h' ], ['f' ], ['g' ]],
		\ [['a' , 'e' ], ['b' ], ['c' ], ['f' , 'g' , 'h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' ], ['f' , 'g' ], ['h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' ], ['f' , 'h' ], ['g' ]],
		\ [['a' , 'e' ], ['b' ], ['c' ], ['f' ], ['g' , 'h' ]],
		\ [['a' , 'e' ], ['b' ], ['c' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' , 'f' , 'g' , 'h' ], ['b' , 'c' , 'e' ]],
		\ [['a' , 'f' , 'g' , 'h' ], ['b' , 'c' ], ['e' ]],
		\ [['a' , 'f' , 'g' , 'h' ], ['b' , 'e' ], ['c' ]],
		\ [['a' , 'f' , 'g' , 'h' ], ['b' ], ['c' , 'e' ]],
		\ [['a' , 'f' , 'g' , 'h' ], ['b' ], ['c' ], ['e' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'c' , 'e' , 'h' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'c' , 'e' ], ['h' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'c' , 'h' ], ['e' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'c' ], ['e' , 'h' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'c' ], ['e' ], ['h' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'e' , 'h' ], ['c' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'e' ], ['c' , 'h' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'e' ], ['c' ], ['h' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'h' ], ['c' , 'e' ]],
		\ [['a' , 'f' , 'g' ], ['b' , 'h' ], ['c' ], ['e' ]],
		\ [['a' , 'f' , 'g' ], ['b' ], ['c' , 'e' , 'h' ]],
		\ [['a' , 'f' , 'g' ], ['b' ], ['c' , 'e' ], ['h' ]],
		\ [['a' , 'f' , 'g' ], ['b' ], ['c' , 'h' ], ['e' ]],
		\ [['a' , 'f' , 'g' ], ['b' ], ['c' ], ['e' , 'h' ]],
		\ [['a' , 'f' , 'g' ], ['b' ], ['c' ], ['e' ], ['h' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'c' , 'e' , 'g' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'c' , 'e' ], ['g' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'c' , 'g' ], ['e' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'c' ], ['e' , 'g' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'c' ], ['e' ], ['g' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'e' , 'g' ], ['c' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'e' ], ['c' , 'g' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'e' ], ['c' ], ['g' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'g' ], ['c' , 'e' ]],
		\ [['a' , 'f' , 'h' ], ['b' , 'g' ], ['c' ], ['e' ]],
		\ [['a' , 'f' , 'h' ], ['b' ], ['c' , 'e' , 'g' ]],
		\ [['a' , 'f' , 'h' ], ['b' ], ['c' , 'e' ], ['g' ]],
		\ [['a' , 'f' , 'h' ], ['b' ], ['c' , 'g' ], ['e' ]],
		\ [['a' , 'f' , 'h' ], ['b' ], ['c' ], ['e' , 'g' ]],
		\ [['a' , 'f' , 'h' ], ['b' ], ['c' ], ['e' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'e' , 'g' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'e' , 'g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'e' , 'h' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'e' ], ['g' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'e' ], ['g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'f' ], ['b' , 'c' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'c' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'c' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'c' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'c' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'c' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'e' , 'g' , 'h' ], ['c' ]],
		\ [['a' , 'f' ], ['b' , 'e' , 'g' ], ['c' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'e' , 'g' ], ['c' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'e' , 'h' ], ['c' , 'g' ]],
		\ [['a' , 'f' ], ['b' , 'e' , 'h' ], ['c' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'e' ], ['c' , 'g' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'e' ], ['c' , 'g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'e' ], ['c' , 'h' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'e' ], ['c' ], ['g' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'e' ], ['c' ], ['g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'g' , 'h' ], ['c' , 'e' ]],
		\ [['a' , 'f' ], ['b' , 'g' , 'h' ], ['c' ], ['e' ]],
		\ [['a' , 'f' ], ['b' , 'g' ], ['c' , 'e' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'g' ], ['c' , 'e' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'g' ], ['c' , 'h' ], ['e' ]],
		\ [['a' , 'f' ], ['b' , 'g' ], ['c' ], ['e' , 'h' ]],
		\ [['a' , 'f' ], ['b' , 'g' ], ['c' ], ['e' ], ['h' ]],
		\ [['a' , 'f' ], ['b' , 'h' ], ['c' , 'e' , 'g' ]],
		\ [['a' , 'f' ], ['b' , 'h' ], ['c' , 'e' ], ['g' ]],
		\ [['a' , 'f' ], ['b' , 'h' ], ['c' , 'g' ], ['e' ]],
		\ [['a' , 'f' ], ['b' , 'h' ], ['c' ], ['e' , 'g' ]],
		\ [['a' , 'f' ], ['b' , 'h' ], ['c' ], ['e' ], ['g' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'e' , 'g' , 'h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'e' , 'g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'e' , 'h' ], ['g' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'e' ], ['g' , 'h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'e' ], ['g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'g' , 'h' ], ['e' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'g' ], ['e' , 'h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'g' ], ['e' ], ['h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'h' ], ['e' , 'g' ]],
		\ [['a' , 'f' ], ['b' ], ['c' , 'h' ], ['e' ], ['g' ]],
		\ [['a' , 'f' ], ['b' ], ['c' ], ['e' , 'g' , 'h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' ], ['e' , 'g' ], ['h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' ], ['e' , 'h' ], ['g' ]],
		\ [['a' , 'f' ], ['b' ], ['c' ], ['e' ], ['g' , 'h' ]],
		\ [['a' , 'f' ], ['b' ], ['c' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'c' , 'e' , 'f' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'c' , 'e' ], ['f' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'c' , 'f' ], ['e' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'c' ], ['e' , 'f' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'c' ], ['e' ], ['f' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'e' , 'f' ], ['c' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'e' ], ['c' , 'f' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'e' ], ['c' ], ['f' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'f' ], ['c' , 'e' ]],
		\ [['a' , 'g' , 'h' ], ['b' , 'f' ], ['c' ], ['e' ]],
		\ [['a' , 'g' , 'h' ], ['b' ], ['c' , 'e' , 'f' ]],
		\ [['a' , 'g' , 'h' ], ['b' ], ['c' , 'e' ], ['f' ]],
		\ [['a' , 'g' , 'h' ], ['b' ], ['c' , 'f' ], ['e' ]],
		\ [['a' , 'g' , 'h' ], ['b' ], ['c' ], ['e' , 'f' ]],
		\ [['a' , 'g' , 'h' ], ['b' ], ['c' ], ['e' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'e' , 'f' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'e' , 'f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'e' , 'h' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'e' ], ['f' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'e' ], ['f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'f' , 'h' ], ['e' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'f' ], ['e' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'f' ], ['e' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'g' ], ['b' , 'c' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'c' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'c' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'c' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'c' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'c' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'e' , 'f' , 'h' ], ['c' ]],
		\ [['a' , 'g' ], ['b' , 'e' , 'f' ], ['c' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'e' , 'f' ], ['c' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'e' , 'h' ], ['c' , 'f' ]],
		\ [['a' , 'g' ], ['b' , 'e' , 'h' ], ['c' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'e' ], ['c' , 'f' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'e' ], ['c' , 'f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'e' ], ['c' , 'h' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'e' ], ['c' ], ['f' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'e' ], ['c' ], ['f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'f' , 'h' ], ['c' , 'e' ]],
		\ [['a' , 'g' ], ['b' , 'f' , 'h' ], ['c' ], ['e' ]],
		\ [['a' , 'g' ], ['b' , 'f' ], ['c' , 'e' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'f' ], ['c' , 'e' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'f' ], ['c' , 'h' ], ['e' ]],
		\ [['a' , 'g' ], ['b' , 'f' ], ['c' ], ['e' , 'h' ]],
		\ [['a' , 'g' ], ['b' , 'f' ], ['c' ], ['e' ], ['h' ]],
		\ [['a' , 'g' ], ['b' , 'h' ], ['c' , 'e' , 'f' ]],
		\ [['a' , 'g' ], ['b' , 'h' ], ['c' , 'e' ], ['f' ]],
		\ [['a' , 'g' ], ['b' , 'h' ], ['c' , 'f' ], ['e' ]],
		\ [['a' , 'g' ], ['b' , 'h' ], ['c' ], ['e' , 'f' ]],
		\ [['a' , 'g' ], ['b' , 'h' ], ['c' ], ['e' ], ['f' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'e' , 'f' , 'h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'e' , 'f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'e' , 'h' ], ['f' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'e' ], ['f' , 'h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'e' ], ['f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'f' , 'h' ], ['e' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'f' ], ['e' , 'h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'f' ], ['e' ], ['h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'h' ], ['e' , 'f' ]],
		\ [['a' , 'g' ], ['b' ], ['c' , 'h' ], ['e' ], ['f' ]],
		\ [['a' , 'g' ], ['b' ], ['c' ], ['e' , 'f' , 'h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' ], ['e' , 'f' ], ['h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' ], ['e' , 'h' ], ['f' ]],
		\ [['a' , 'g' ], ['b' ], ['c' ], ['e' ], ['f' , 'h' ]],
		\ [['a' , 'g' ], ['b' ], ['c' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'e' , 'f' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'e' , 'f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'e' , 'g' ], ['f' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'e' ], ['f' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'e' ], ['f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'f' , 'g' ], ['e' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'f' ], ['e' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'f' ], ['e' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'g' ], ['e' , 'f' ]],
		\ [['a' , 'h' ], ['b' , 'c' , 'g' ], ['e' ], ['f' ]],
		\ [['a' , 'h' ], ['b' , 'c' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'c' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'c' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'h' ], ['b' , 'c' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'c' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'e' , 'f' , 'g' ], ['c' ]],
		\ [['a' , 'h' ], ['b' , 'e' , 'f' ], ['c' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'e' , 'f' ], ['c' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'e' , 'g' ], ['c' , 'f' ]],
		\ [['a' , 'h' ], ['b' , 'e' , 'g' ], ['c' ], ['f' ]],
		\ [['a' , 'h' ], ['b' , 'e' ], ['c' , 'f' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'e' ], ['c' , 'f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'e' ], ['c' , 'g' ], ['f' ]],
		\ [['a' , 'h' ], ['b' , 'e' ], ['c' ], ['f' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'e' ], ['c' ], ['f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'f' , 'g' ], ['c' , 'e' ]],
		\ [['a' , 'h' ], ['b' , 'f' , 'g' ], ['c' ], ['e' ]],
		\ [['a' , 'h' ], ['b' , 'f' ], ['c' , 'e' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'f' ], ['c' , 'e' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'f' ], ['c' , 'g' ], ['e' ]],
		\ [['a' , 'h' ], ['b' , 'f' ], ['c' ], ['e' , 'g' ]],
		\ [['a' , 'h' ], ['b' , 'f' ], ['c' ], ['e' ], ['g' ]],
		\ [['a' , 'h' ], ['b' , 'g' ], ['c' , 'e' , 'f' ]],
		\ [['a' , 'h' ], ['b' , 'g' ], ['c' , 'e' ], ['f' ]],
		\ [['a' , 'h' ], ['b' , 'g' ], ['c' , 'f' ], ['e' ]],
		\ [['a' , 'h' ], ['b' , 'g' ], ['c' ], ['e' , 'f' ]],
		\ [['a' , 'h' ], ['b' , 'g' ], ['c' ], ['e' ], ['f' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'e' , 'f' , 'g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'e' , 'f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'e' , 'g' ], ['f' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'e' ], ['f' , 'g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'e' ], ['f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'f' , 'g' ], ['e' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'f' ], ['e' , 'g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'f' ], ['e' ], ['g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'g' ], ['e' , 'f' ]],
		\ [['a' , 'h' ], ['b' ], ['c' , 'g' ], ['e' ], ['f' ]],
		\ [['a' , 'h' ], ['b' ], ['c' ], ['e' , 'f' , 'g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' ], ['e' , 'f' ], ['g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' ], ['e' , 'g' ], ['f' ]],
		\ [['a' , 'h' ], ['b' ], ['c' ], ['e' ], ['f' , 'g' ]],
		\ [['a' , 'h' ], ['b' ], ['c' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'f' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'g' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'g' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'h' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'c' , 'e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'e' ], ['f' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'f' , 'g' , 'h' ], ['e' ]],
		\ [['a' ], ['b' , 'c' , 'f' , 'g' ], ['e' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'f' , 'g' ], ['e' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'f' , 'h' ], ['e' , 'g' ]],
		\ [['a' ], ['b' , 'c' , 'f' , 'h' ], ['e' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'f' ], ['e' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'f' ], ['e' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'f' ], ['e' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'f' ], ['e' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'f' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'g' , 'h' ], ['e' , 'f' ]],
		\ [['a' ], ['b' , 'c' , 'g' , 'h' ], ['e' ], ['f' ]],
		\ [['a' ], ['b' , 'c' , 'g' ], ['e' , 'f' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'g' ], ['e' , 'f' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'g' ], ['e' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'c' , 'g' ], ['e' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'c' , 'g' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'c' , 'h' ], ['e' , 'f' , 'g' ]],
		\ [['a' ], ['b' , 'c' , 'h' ], ['e' , 'f' ], ['g' ]],
		\ [['a' ], ['b' , 'c' , 'h' ], ['e' , 'g' ], ['f' ]],
		\ [['a' ], ['b' , 'c' , 'h' ], ['e' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'c' , 'h' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'f' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'g' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'g' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'h' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'c' ], ['e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'c' ], ['e' ], ['f' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'c' ], ['e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'c' ], ['e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'e' , 'f' , 'g' , 'h' ], ['c' ]],
		\ [['a' ], ['b' , 'e' , 'f' , 'g' ], ['c' , 'h' ]],
		\ [['a' ], ['b' , 'e' , 'f' , 'g' ], ['c' ], ['h' ]],
		\ [['a' ], ['b' , 'e' , 'f' , 'h' ], ['c' , 'g' ]],
		\ [['a' ], ['b' , 'e' , 'f' , 'h' ], ['c' ], ['g' ]],
		\ [['a' ], ['b' , 'e' , 'f' ], ['c' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'e' , 'f' ], ['c' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'e' , 'f' ], ['c' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'e' , 'f' ], ['c' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'e' , 'f' ], ['c' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'e' , 'g' , 'h' ], ['c' , 'f' ]],
		\ [['a' ], ['b' , 'e' , 'g' , 'h' ], ['c' ], ['f' ]],
		\ [['a' ], ['b' , 'e' , 'g' ], ['c' , 'f' , 'h' ]],
		\ [['a' ], ['b' , 'e' , 'g' ], ['c' , 'f' ], ['h' ]],
		\ [['a' ], ['b' , 'e' , 'g' ], ['c' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'e' , 'g' ], ['c' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'e' , 'g' ], ['c' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'e' , 'h' ], ['c' , 'f' , 'g' ]],
		\ [['a' ], ['b' , 'e' , 'h' ], ['c' , 'f' ], ['g' ]],
		\ [['a' ], ['b' , 'e' , 'h' ], ['c' , 'g' ], ['f' ]],
		\ [['a' ], ['b' , 'e' , 'h' ], ['c' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'e' , 'h' ], ['c' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'f' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'g' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'g' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'g' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'h' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'e' ], ['c' , 'h' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'e' ], ['c' ], ['f' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' ], ['f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' ], ['f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'e' ], ['c' ], ['f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'e' ], ['c' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'f' , 'g' , 'h' ], ['c' , 'e' ]],
		\ [['a' ], ['b' , 'f' , 'g' , 'h' ], ['c' ], ['e' ]],
		\ [['a' ], ['b' , 'f' , 'g' ], ['c' , 'e' , 'h' ]],
		\ [['a' ], ['b' , 'f' , 'g' ], ['c' , 'e' ], ['h' ]],
		\ [['a' ], ['b' , 'f' , 'g' ], ['c' , 'h' ], ['e' ]],
		\ [['a' ], ['b' , 'f' , 'g' ], ['c' ], ['e' , 'h' ]],
		\ [['a' ], ['b' , 'f' , 'g' ], ['c' ], ['e' ], ['h' ]],
		\ [['a' ], ['b' , 'f' , 'h' ], ['c' , 'e' , 'g' ]],
		\ [['a' ], ['b' , 'f' , 'h' ], ['c' , 'e' ], ['g' ]],
		\ [['a' ], ['b' , 'f' , 'h' ], ['c' , 'g' ], ['e' ]],
		\ [['a' ], ['b' , 'f' , 'h' ], ['c' ], ['e' , 'g' ]],
		\ [['a' ], ['b' , 'f' , 'h' ], ['c' ], ['e' ], ['g' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'e' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'e' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'e' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'e' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'e' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'g' , 'h' ], ['e' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'g' ], ['e' , 'h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'g' ], ['e' ], ['h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'h' ], ['e' , 'g' ]],
		\ [['a' ], ['b' , 'f' ], ['c' , 'h' ], ['e' ], ['g' ]],
		\ [['a' ], ['b' , 'f' ], ['c' ], ['e' , 'g' , 'h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' ], ['e' , 'g' ], ['h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' ], ['e' , 'h' ], ['g' ]],
		\ [['a' ], ['b' , 'f' ], ['c' ], ['e' ], ['g' , 'h' ]],
		\ [['a' ], ['b' , 'f' ], ['c' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' , 'g' , 'h' ], ['c' , 'e' , 'f' ]],
		\ [['a' ], ['b' , 'g' , 'h' ], ['c' , 'e' ], ['f' ]],
		\ [['a' ], ['b' , 'g' , 'h' ], ['c' , 'f' ], ['e' ]],
		\ [['a' ], ['b' , 'g' , 'h' ], ['c' ], ['e' , 'f' ]],
		\ [['a' ], ['b' , 'g' , 'h' ], ['c' ], ['e' ], ['f' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'e' , 'f' , 'h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'e' , 'f' ], ['h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'e' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'e' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'e' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'f' , 'h' ], ['e' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'f' ], ['e' , 'h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'f' ], ['e' ], ['h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'h' ], ['e' , 'f' ]],
		\ [['a' ], ['b' , 'g' ], ['c' , 'h' ], ['e' ], ['f' ]],
		\ [['a' ], ['b' , 'g' ], ['c' ], ['e' , 'f' , 'h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' ], ['e' , 'f' ], ['h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' ], ['e' , 'h' ], ['f' ]],
		\ [['a' ], ['b' , 'g' ], ['c' ], ['e' ], ['f' , 'h' ]],
		\ [['a' ], ['b' , 'g' ], ['c' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'e' , 'f' , 'g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'e' , 'f' ], ['g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'e' , 'g' ], ['f' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'e' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'e' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'f' , 'g' ], ['e' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'f' ], ['e' , 'g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'f' ], ['e' ], ['g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'g' ], ['e' , 'f' ]],
		\ [['a' ], ['b' , 'h' ], ['c' , 'g' ], ['e' ], ['f' ]],
		\ [['a' ], ['b' , 'h' ], ['c' ], ['e' , 'f' , 'g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' ], ['e' , 'f' ], ['g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' ], ['e' , 'g' ], ['f' ]],
		\ [['a' ], ['b' , 'h' ], ['c' ], ['e' ], ['f' , 'g' ]],
		\ [['a' ], ['b' , 'h' ], ['c' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'f' , 'g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'g' , 'h' ], ['f' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'g' ], ['f' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'h' ], ['f' , 'g' ]],
		\ [['a' ], ['b' ], ['c' , 'e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'e' ], ['f' , 'g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'e' ], ['f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'f' , 'g' , 'h' ], ['e' ]],
		\ [['a' ], ['b' ], ['c' , 'f' , 'g' ], ['e' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'f' , 'g' ], ['e' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'f' , 'h' ], ['e' , 'g' ]],
		\ [['a' ], ['b' ], ['c' , 'f' , 'h' ], ['e' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'f' ], ['e' , 'g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'f' ], ['e' , 'g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'f' ], ['e' , 'h' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'f' ], ['e' ], ['g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'f' ], ['e' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'g' , 'h' ], ['e' , 'f' ]],
		\ [['a' ], ['b' ], ['c' , 'g' , 'h' ], ['e' ], ['f' ]],
		\ [['a' ], ['b' ], ['c' , 'g' ], ['e' , 'f' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'g' ], ['e' , 'f' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'g' ], ['e' , 'h' ], ['f' ]],
		\ [['a' ], ['b' ], ['c' , 'g' ], ['e' ], ['f' , 'h' ]],
		\ [['a' ], ['b' ], ['c' , 'g' ], ['e' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' , 'h' ], ['e' , 'f' , 'g' ]],
		\ [['a' ], ['b' ], ['c' , 'h' ], ['e' , 'f' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' , 'h' ], ['e' , 'g' ], ['f' ]],
		\ [['a' ], ['b' ], ['c' , 'h' ], ['e' ], ['f' , 'g' ]],
		\ [['a' ], ['b' ], ['c' , 'h' ], ['e' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'f' , 'g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'f' ], ['g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'g' , 'h' ], ['f' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'g' ], ['f' , 'h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'g' ], ['f' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'h' ], ['f' , 'g' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' , 'h' ], ['f' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' ], ['f' , 'g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' ], ['f' , 'g' ], ['h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' ], ['f' , 'h' ], ['g' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' ], ['f' ], ['g' , 'h' ]],
		\ [['a' ], ['b' ], ['c' ], ['e' ], ['f' ], ['g' ], ['h' ]] ]
"}}}1
