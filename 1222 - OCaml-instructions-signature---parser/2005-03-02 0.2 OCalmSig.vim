" Vim plugin for OCaml instruction signature ver. 0.2
" Last Change: 2005 Mar 03
" Maintainer: Grzegorz Dymarek <gdymarek@idea.net.pl>

if exists("ocaml_signature")
  finish
endif
let ocaml_signature = 1

function s:ParseInstr()
	
	let buf = getline(line("."))
	let num = match(buf,";;")
	let end = 0
	let i = 0	
	while (num<1 && end==0)
		let i = i + 1
		let next_line = getline(line(".")+i)
		if (next_line == "") 
			let end = 1
		else
			let num = match(next_line,";;")	
			let buf = buf . next_line
		endif
	endwhile

	if (end==1)
		echo "Can't find the end of instruction (;;)"
	else
		let s:com = "echo \"".buf."\" | ocaml"
		let ret = system(s:com)		
		let ret = strpart(ret,match(ret,"#")+1,strlen(ret))
		let ret = "#".strpart(ret,0,match(ret,"#")-1)
		echo ret	
	endif
endfunction

if !exists(":OCamlParseInstruction")
	command -nargs=0 OCamlParseInstruction :call s:ParseInstr()
endif
