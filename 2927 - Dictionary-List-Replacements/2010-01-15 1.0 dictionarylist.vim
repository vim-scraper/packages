
",dict	: (dic)tionary on (t)ab
"---------------------------------------------------------------------
:map ,dict :let @a=@/


",rict	: (r)eplace (dict)ionary words in searched pattern
"---------------------------------------------------------------------
:map ,rict :let @a=@/
:vmap ,rict :let @a=@/


",list	: remember sequential (list)
"---------------------------------------------------------------------
:map ,list :let @b=@/


",rist	: (r)eplace sequential l(ist)
"---------------------------------------------------------------------
:map ,rist :let @b=@/
:vmap ,rist :let @b=@/

							   "----------
							   " functions
							   "----------
"Push	> Push(key, value)	add value to g:hash stack
" --------------------------------------------------------------------
function! Push(key, value)	
	if a:key!=""
		if !exists("g:hash")	
			let g:hash={}
		endif
		let g:hash[a:key]=a:value
		return a:key."=".a:value
	endif
endfunction


"Pop	> Pop(key)		pop value from g:hash stack
" --------------------------------------------------------------------
function! Pop(key)	
	if exists("g:hash[a:key]")	
		return g:hash[a:key]
	endif
	return ""
endfunction


"Add	> Add(key, step) 	increment by given step
" --------------------------------------------------------------------
function! Add(key, step)
	if !exists("g:counters")	
		let g:counters={}
	endif
	if !exists("g:counters[a:key]")	
		let g:counters[a:key] = 0
	endif
	let g:counters[a:key] = g:counters[a:key] + a:step
	return g:counters[a:key]
endfunction


"Clear	> Clear(key) 		clear counters. empty key clears all
" --------------------------------------------------------------------
function! Clear(key)
	if (a:key == "") 
		let g:counters={}
	else
		if exists("g:counters[a:key]")	
			let g:counters[a:key] = 0
		endif
	endif
endfunction


"Inc	> Inc(key) 		let g:counters={} to reset
" --------------------------------------------------------------------
function! Inc(key)
	return Add(a:key, 1)
endfunction


"Set	> Set(key, value) 	Set to value
" --------------------------------------------------------------------
function! Set(key, value)
        call Clear(a:key)
	return Add(a:key, a:value)
endfunction


"Get	> Get(key) 		Get value
" --------------------------------------------------------------------
function! Get(key)
	return Add(a:key, 0)
endfunction