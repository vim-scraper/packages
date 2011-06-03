"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Parameter order changing

" stack based storing of cursor and mark positions
let s:v_fst = []
let s:v_lst = []
let s:c_pos = []
function! SavePositions()
	let s:v_fst = [ getpos("'<") ] + s:v_fst
	let s:v_snd = [ getpos("'>") ] + s:v_snd
	let s:c_pos = [ getpos(".") ] + s:c_pos
endfunction
function! RestorePositions()
	call setpos( "'<", s:v_fst[0] )
	call setpos( "'>", s:v_snd[0] )
	call setpos( ".",  s:c_pos[0] )
	let s:v_fst = s:v_fst[1:]
	let s:v_snd = s:v_snd[1:]
	let s:c_pos = s:c_pos[1:]
endfunction

" This function is NOT foolproof:
" example:
"   aap( noot, "mies", "boom" )
" Now put the cursor at the second comma and do va"v.
" Only use this function when you already suspect to be inside a string.
function! InsideQuotedString()
	call SavePositions()
	let l:cur = col(".") " the next line moves the cursor
	normal va"v
	let l:retval = col("'<") <= l:cur && l:cur <= col("'>")
	call RestorePositions()
	return l:retval
endfunction

" Function to delive a list of [begin, Begin, End, end] sequences for
" comma separated argument lists.
" Regular expressions are not particularly useful here, we need something like
" a recursive descent parser. But VIM provides most of what's needed.
" begin : the virtcol at the begining of the parameter
" Begin : the first non-white space virtcol of the parameter
" End   : last non-white space character of the parameter
" end   : end of parameter, one column before the comma or closing brackets
function! ParamRangeList()
	call SavePositions()

	" introduced behaviour: if starting on a closing bracket, assume it's part
	" of the outer scope. without this explicit case, a closing bracket is
	" part of the inner scope, but not of any parameter in that inner scope.
	if getline(".")[col(".")-1] == ")"
		normal [(
	endif

	" investigate out outermost scope, i.e. the pair of brackets we're in
	let l:starting = virtcol(".")
	normal [(
	let l:leftmost = virtcol(".")
	if l:leftmost == l:starting
		" could be we're inside a string. there the [( operator
		" doesn't work for some reason
		if InsideQuotedString() 
			" get us out of this mess: this doesn't leave ( ) scope.
			normal va"vl[(
			let l:leftmost = virtcol(".")
		else
			echo "Not inside a properly nested ( bracket expression )"
			call RestorePositions()
			return
		endif
	endif
	normal ])
	let l:rightmost = virtcol(".")
	if l:rightmost == l:leftmost
		echo "No matching closing bracket ) found. Balance your brackets first."
		call RestorePositions()
		return
	endif

	" parse the inside of the ( bracket expression )
	exec "normal ".l:leftmost."|"
	let l:retval = []
	while virtcol(".") < l:rightmost
		normal l
		" store begin virtcol in return value
		let l:this = [ virtcol(".") ] " begin new param bound set
		let l:current = getline(".")[ col(".")-1 ]
		" skip whitespace and add to retval how much whitespace we skipped
		if l:current == " " || l:current == "	" " skip white-space
			normal w
		endif
		let l:this += [ virtcol(".") ] " begin of non-white space
		let l:this += [ virtcol(".") ] " last non-white (will be updated)
		while l:current != ',' && virtcol(".") < l:rightmost
			" some characters to pay extra attention to:
			" \": jump to matching quote, use the quoted text object
			" ' : jump to matching quote, trivially found with f'
			" ( : a nested group: use the ]) command again
			if l:current == "\""
				normal va"v
				" this is a bit messy: sometimes the trick above jumps past
				" the matching quote, and sometimes it doesn't, depending on
				" whether there's WS after it. make canonical again.
				if getline(".")[ col(".")-1 ] != "\""
					normal F"
				endif
			elseif l:current == "'"
				" this will keep the cursor on NWS: safe
				normal f'
			elseif l:current == "("
				" this will keep the cursor on NWS: safe
				normal ])
			endif
			" ... in all other cases we haven't moved yet
			if l:current != ' ' && l:current != "	"
				let l:this[2] = virtcol(".")
			endif
			normal l
			let l:current = getline(".")[ col(".")-1 ]
		endwhile
		" store End and end virtcol in retval
		let l:this += [ virtcol(".") - 1 ] " exclude close bracket or ,
		let l:retval += [ l:this ]
	endwhile

	" done
	call RestorePositions()
	return l:retval
endfunction

function! CurrentPosition( bounds )
	if empty( a:bounds ) " silent pre-condition
		return -1
	endif
	let l:here = virtcol(".")
	let l:i = 0
	let l:ourpos = -1
	while l:i < len( a:bounds ) && l:ourpos == -1
		" of course check *outer* bounds of each param
		if (a:bounds[ l:i ][0] <= l:here) && (l:here <= a:bounds[ l:i ][3])
			let l:ourpos = l:i
		endif
		let l:i += 1
	endwhile
	if l:ourpos == -1
		if getline(".")[col(".")-1] == ","
			echo "Ambiguous cursor position: on a comma. Move the cursor."
		elseif getline(".")[col(".")-1] == ")"
			echo "Ambiguous cursor position: on a closing bracket. Move the cursor."
		else
			echo "Couldn't find current position in parameter list."
		endif
	endif
	return l:ourpos
endfunction

function! ShiftCurrentParam( shift )
	let l:abs_shift = a:shift > 0 ? a:shift : ( 0 - a:shift )
	let l:direction = a:shift > 0 ? "RIGHT" : "LEFT"
	let l:bounds = ParamRangeList()
	let l:ourpos = CurrentPosition( l:bounds )
	if l:ourpos == -1
		return
	endif
	" check if we're instructed to shift to an existing neighbour
	let l:neigh = l:ourpos + a:shift
	if g:param_shift_round_robin
		let l:rr_neigh = ( l:neigh < 0 ? ( l:neigh + len(l:bounds) ) : l:neigh ) % len(l:bounds)
		if l:ourpos == l:rr_neigh
			" we end up on the same position. let's call it an early day then
			return
		else
			let l:neigh = l:rr_neigh
		endif
	elseif l:neigh < 0 || l:neigh + 1 >= len( l:bounds )
		" neighbour out of bounds, print a nice error message and quit
		echo "Cannot shift current parameter " . l:abs_shift . " position(s) to the " . l:direction . "."
		return
	endif
	let l:offset = virtcol(".") - l:bounds[ l:ourpos ][0]
	" do the actual substitution
	" leave the white-space as it is, and switch only the non-white space
	let l:first = min( [l:ourpos, l:neigh ] )
	let l:last  = max( [l:ourpos, l:neigh ] )
	let l:expr  = "s/\\%" . l:bounds[ l:first ][1] . "v\\(.*\\)\\%" . (l:bounds[ l:first ][2] + 1) . "v" " first parameter
	let l:expr .= "\\(.*\\)" " stuff in between
	let l:expr .= "\\%" . l:bounds[ l:last ][1] . "v\\(.*\\)\\%" . (l:bounds[ l:last ][2] + 1) . "v" " last parameter
	let l:expr .= "/\\3\\2\\1/" " reverse order of matching
	execute l:expr
	" set the cursor at the same offset in the param, which is now shifted
	exec "normal " . ( l:bounds[ l:neigh ][0] + l:offset ) . "|"
	echo "Shifted " . (g:param_shift_round_robin ? "(round-robin) " : "" )  . "parameter " . (l:ourpos + 1) . " to the " . l:direction . ( l:abs_shift > 1 ? " (".l:abs_shift." positions)." : "." )
endfunction

function! VisualizeParameter( outer )
	let l:bounds = ParamRangeList()
	let l:ourpos = CurrentPosition( l:bounds )
	if l:ourpos == -1
		return
	endif
	let l:left  = l:bounds[ l:ourpos ][ (a:outer ? 0 : 1) ]
	let l:right = l:bounds[ l:ourpos ][ (a:outer ? 2 : 3) ]
	exec "normal " . l:left . "|v" . l:right . "|"
endfunction

