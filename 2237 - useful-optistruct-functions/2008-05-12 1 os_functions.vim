"============================================================================= 
" Author: Sam Patten
" Help: 
" examples:
" :%g /^PCOMP/ :call INC(2,1)	increments integers in field 2 on lines starting with PCOMP card by 1
" :'m,.g /^PLY/ :call INC(2,10)	increments integers in field 2 on lines from mark m to here starting with PLY card by 10
" :4,. :call INC(2,100)		increment integers in field 2 from line 4 to this line
" 
" Instalation: save this in your plugins directory 
"========================================================================= 
function INC(field,increment) 

	" get start position
	:let spos = (a:field-1)*8
	:let spos2 = spos+1
	" get end position
	:let epos = a:field*8-1
	" get field data
	:let data = strpart(getline('.'), spos, 8)
	:if data != "        "
		" convert to number
		:let n = str2nr(data)
		" increment number
		:let newn = n + a:increment
	:else
		:let newn = ""
	:endif

	:let retval = EightField(newn) 
	
	" substitute
	:let line = getline(".")
	:let repl = substitute(line, '\%' . spos2 . 'c........', retval , "")
	:call setline(".", repl)
	"return retval
endfunction 

" function to convert string to 8 chars with spaces
function EightField(newn)
	" get string length
	:let l = strlen(a:newn)	
	:if l == 0
	:let endpart = "        "
	:elseif l == 1
	:let endpart = "       "
	:elseif l == 2
	:let endpart = "      "
	:elseif l == 3
	:let endpart = "     "
	:elseif l == 4
	:let endpart = "    "
	:elseif l == 5
	:let endpart = "   "
	:elseif l == 6
	:let endpart = "  "
	:elseif l == 7
	:let endpart = " "
	:elseif l == 8
	:let endpart = ""
	:endif
	:let retval = endpart . a:newn
	return retval
endfunction

"============================================================================= 
" Help: 
" examples:
" :let g:autonumstart = 1
" :%g /^PLY/ :call AUTONUM(2,1)	replace field 2 on lines starting with PLY card with auto number; start 1, incr 1
" :'m,.g /^PLY/ :call AUTONUM(2,1)      replace field 2 on lines starting with PLY card with auto number from mark m to here; start 1, incr 1
"========================================================================= 
let g:autonumstart = 1
function AUTONUM(field,increment) 

	" get start position
	:let spos = (a:field-1)*8
	:let spos2 = spos+1
	" increment number
	let g:autonumstart = g:autonumstart + a:increment
	:let newn = g:autonumstart - 1
	" get string length
	:let l = strlen(newn)
	" create final string
	
	:let retval = EightField(newn)
	
	" substitute
	:let line = getline(".")
	:let repl = substitute(line, '\%' . spos2 . 'c........', retval , "")
	:call setline(".", repl)
	"return retval
endfunction 
"============================================================================= 
" Help: 
" examples:
" :let g:autonumstart = 1
" :'m,.call AUTONUM2(3,7,1)      replace fields 3-10 on lines from mark m to here; start 1, incr 1
"========================================================================= 
function AUTONUM2(sfield,nfields,increment) 

        :for n in range(1,a:nfields)
                :let field = a:sfield-1+n

	        " get start position
	        :let spos = (field-1)*8
	        :let spos2 = spos+1

                " get initial number
                :let data = strpart(getline('.'), spos, 8)
                :let initdata = str2nr(data)
	        :if initdata != 0
		
	                " increment number
	                let g:autonumstart = g:autonumstart + a:increment
	                :let newn = g:autonumstart - 1
	                " get string length
	                :let l = strlen(newn)
	                " create final string
	                
	                :let retval = EightField(newn)
	                
	                " substitute
	                :let line = getline(".")
	                :let repl = substitute(line, '\%' . spos2 . 'c........', retval , "")
	                :call setline(".", repl)

                :endif

                :endfor
	"return retval
endfunction 

"============================================================================= 
" Author: Sam Patten
" Help: 
" examples:
" :%s/\%9c../\=INCR(1)/	increments 2 digit integers in fields 9-11 by 1
"========================================================================= 
function INCR(increment) 

	" get start column
	:let cnow = col(".")
	" get field position
	:let field = (cnow-1)/8+1
	" get start position
	:let spos = (field-1)*8
	" get end position
	:let epos = field*8-1
	" get field data
	:let data = strpart(getline('.'), spos, 8)
	:if data != "        "
		" convert to number
		:let n = str2nr(data)
		" increment number
		:let newn = n + a:increment
	:else
		:let newn = ""
	:endif
	
	:let retval = newn
	
	return retval
endfunction 


"============================================================================= 
" Author: Sam Patten
" Help: 
" examples:
" :%g /^PCOMP/ :call FIELD(2,1)	replaces field 2 with "       1" on lines starting with PCOMP card
" :'m,. :call FIELD(2,1)	replaces field 2 with "       1" on lines from mark m to here
" 
" Instalation: save this in your plugins directory 
"========================================================================= 
function FIELD(field,Value) 
	" get start position
	:let spos = (a:field-1)*8
	:let spos2 = spos+1
	" get field data
	:let retval = EightField(a:Value) 
	
	" substitute
	:let line = getline(".")
	:let repl = substitute(line, '\%' . spos2 . 'c........', retval , "")
	:call setline(".", repl)
	"return retval
endfunction 

"============================================================================= 
" Author: Sam Patten
" Help: 
" examples:
" 
"========================================================================= 
function Maths(data) 
	:let retval = exec(!"echo a:data | bc -l -q")
	:return retval
endfunction 
