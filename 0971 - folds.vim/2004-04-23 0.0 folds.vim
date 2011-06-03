"============================================================================= 
" File:         Foldutl.vim
" Author:       Mutoh Yasuoki <mutoh@sepia.ocn.ne.jp>
" Requirements: Vim version 6.1 and later.
" Commands:     {range}Fold0, 
"               {range}Fold1 st_col end_col, {range}fold2 st_col end_col
" Description:  Fold the continuous same lines more than 4 to 2 lines  
"               and one folded line.
"               Useful for cheking or debugging long log files.
" Last Change:	2004 April 24
"============================================================================= 
"Has this already been loaded?
if exists('loaded_Foldutl')
    finish
endif
let loaded_Foldutl = 1

" Commands:     {range}Fold0
" Description:  Fold the same lines. No columns arguments. 
" Last Change:	2004 April 24
function! Fold0(...) range
	" use arguments if two passed
	if a:0 == 2
		let a = a:1
		let z = a:2
	" use range
	else
		let a = a:firstline
		let z = a:lastline
	endif
	while (a <= z)
		let str1 = getline(a)
		let str2 = getline(a+1)
		if (str1 == str2)
			let st = a + 1
			let a = a + 2
			while (a <= z)
				let str3 = getline(a)
				if (str1 == str3) && a < z
					let a = a + 1
				elseif (str1 == str3) && a == z
					if st < a-1
						execute st.","a-1 "fold"
					endif
					break
				else
					if st < a-2
						execute st.","a-2 "fold"
					endif
					break
				endif
			endwhile
		else
			let a = a + 1
		endif
	endwhile
endfunction
"Create command
command! -nargs=0 -range=% Fold0 <line1>,<line2>call Fold0()

" Commands:     {range}Fold1 start_col end_col
" Description:  Fold the same lines of columns b/n start_col and end_col.
" Last Change:	2004 April 24
function! Fold1(col_start, col_end, ...) range
	let c1 = a:col_start
	let c2 = a:col_end
	" use arguments if two passed
	if a:0 == 2
		let a = a:1
		let z = a:2
	" use range
	else
		let a = a:firstline
		let z = a:lastline
	endif
	while (a <= z)
		let str1 = strpart(getline(a), c1, c2-c1+1)
		let str2 = strpart(getline(a+1), c1, c2-c1+1)
		if (str1 == str2) && (str1 != "")
			let st = a + 1
			let a = a + 2
			while (a <= z)
				let str3 = strpart(getline(a), c1, c2-c1+1)
				if (str1 == str3) && a < z
					let a = a + 1
				elseif (str1 == str3) && a == z
					if st < a-1
						execute st.","a-1 "fold"
					endif
					break
				else
					if st < a-2
						execute st.","a-2 "fold"
					endif
					break
				endif
			endwhile
		else
			let a = a + 1
		endif
	endwhile
endfunction
command! -nargs=* -range=% Fold1 <line1>,<line2>call Fold1(<f-args>)

" Commands:     {range}Fold2 start_col end_col
" Description:  Fold the same lines ignoring columns b/n start_col and end_col.
" Last Change:	2004 April 24
function! Fold2(col_start, col_end, ...) range
	let c1 = a:col_start
	let c2 = a:col_end
	" use arguments if two passed
	if a:0 == 2
		let a = a:1
		let z = a:2
	" use range
	else
		let a = a:firstline
		let z = a:lastline
	endif
	while (a <= z)
		let str1 = strpart(getline(a), 0, c1) . strpart(getline(a), c2+1)
		let str2 = strpart(getline(a+1), 0, c1) . strpart(getline(a+1), c2+1)
		if (str1 == str2) && (str1 != "")
			let st = a + 1
			let a = a + 2
			while (a <= z)
				let str3 = strpart(getline(a), 0, c1) . strpart(getline(a), c2+1)
				if (str1 == str3) && a < z
					let a = a + 1
				elseif (str1 == str3) && a == z
					if st < a-1
						execute st.","a-1 "fold"
					endif
					break
				else
					if st < a-2
						execute st.","a-2 "fold"
					endif
					break
				endif
			endwhile
		else
			let a = a + 1
		endif
	endwhile
endfunction
command! -nargs=* -range=% Fold2 <line1>,<line2>call Fold2(<f-args>)


