"Veritag Info file explorer
" Version:1.0
" A script to brow through info.txt file made by veratags
"This will automatically show the hierarchy of the module from top
"you may use 
"set statusline=%{Verilog_get_hierarchy()} in the .vimrc or whenever u need it
"Vertags is a cool utility which creates tag for ur verilog desing and creates a ascii hierarchy file called info.txt
"u can use this file to browse through the design
"hierarchy.vim allows u to know the current scope in the info.txt file
"created by: Pushkar Sareen
function! Verilog_get_hierarchy()
	if expand("%") != "info.txt"
		return "kewl"
	endif
	let linestart =  search("{","nbW")
	let coloumnstart =  searchpos("[{}]","nbW")
        if getline(line(coloumnstart))[col(coloumnstart) - 1] !=  "{"
		return "Not in hierarchy"
	endif
	let orig_save_cursor = getpos(".")
	let end_orig_save_cursor = getpos(".")
	let end_orig_save_cursor[2] = "\$"
	call setpos('.',end_orig_save_cursor)
	let hier = ""
	"let t_buf = expand("<cword>")
	let lineend = line(".")
	let coloumnend = col(".")
        "echo lineend
	echo coloumnstart
"	let text = getline(linestart,lineend)
	let hier = ""
	let n = 0
	let currline = lineend
	let save_curr_pos = getpos(".")
"	while n <= (lineend - linestart)
	while currline != linestart
	"	let hier = expand("<cword>") . "." . hier                   Uncomment for full hierarchy
		let curr_pos = getpos(".")
		let new_pos = searchpos("[\\|]___","bW")
		if (line(new_pos) <= linestart) || (curr_pos == new_pos)
			break
		endif
		let onebelow = col(new_pos)
		let currline = line(new_pos)
		while (getline(currline)[onebelow -1] == "|") || (getline(currline)[onebelow - 1] == "\\")
			let currline = currline - 1 
		endwhile
		let save_cursor = getpos(".")
		let save_cursor[1] = currline
		call setpos('.',save_cursor)
		let hier = expand("<cword>") . "." . hier
	endwhile
"	let hier = expand("<cword>") . "." . hier
	
	call setpos('.',save_curr_pos)
	let hier = substitute(hier,"|___",".","g")
	let hier = substitute(hier,"|","","g")
	let hier = substitute(hier," ","","g")
        "echo hier
	call setpos('.',orig_save_cursor)
	return hier
endfunction
