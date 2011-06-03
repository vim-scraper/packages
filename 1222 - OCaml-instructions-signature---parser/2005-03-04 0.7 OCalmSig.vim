" Vim plugin for OCaml instruction signature ver. 0.7
" Last Change: 2005 Mar 04
" Maintainer: Grzegorz Dymarek <125783@student.pwr.wroc.pl>" 
" Tested on OCaml 3.08.2 and VIM 6.3.20

if exists("ocaml_signature")
  finish
endif
	
let ocaml_signature = 1
let s:PreviewBuf = 0
let s:Buf = ""

function s:GetPreviewBuf()
    let i = 0
    while (i<10 && !getbufvar(i,"OCamlPreview"))
	let i = i + 1
    endwhile
    if (i==10) 
	return -1
    else 
	return i
    endif
endfunction

function StatusWindow()
    if (s:PreviewBuf==-1)
	enew!
	set hidden
	call setbufvar(bufnr("%"),"OCamlPreview",1)
	"prevBuf"call append
	let s:PreviewBuf = bufnr("%")
	bp!
    endif
    split
"    echo s:PreviewBuf
	let abc = 2
    call {b {s:PreviewBuf}}
    wincmd J
    res 4
    wincmd p
endfunction

function s:ResetBuf()
	let s:Buf = ""
endfunction

function s:ShowBuf()
	if (s:Buf=="")
		echo "OCaml buffer is empty!"
	else
		echo s:Buf
	endif
endfunction

function Transformate(str)
	let t = match(a:str,"\"")
	if (t==-1) 
		return a:str
	else
		return strpart(a:str,0,t)."\\\"".Transformate(strpart(a:str,t+1,strlen(a:str)-t))
	endif
endfunction

function s:ParseInstr(l,si)
	
	if (s:PreviewBuf==-1)
		call StatusWindow()
	endif	
	let buf = getline(a:l)
	let num = match(buf,";;")
	let end = 0
	let error = 0
	let i = 0
	let last_line = line("$")	
	while (num<1 && end==0)
		let i = i + 1
		if (i>last_line)
			let end = 1
		else
			let next_line = getline(a:l+i)
			let num = match(next_line,";;")	
			let buf = buf ."\n". next_line
		endif
	endwhile
	let buf = Transformate(buf)
	let prog = s:Buf."\n".buf
	let s:com = "echo \"".prog."\" | ocaml"
	let ret = system(s:com)		
	let last = strridx(ret, "#")-1
	let b_last = strridx( strpart(ret,0,strlen(ret)-3), "#")+2
	let ret = strpart(ret, b_last, last-b_last)
	
	if match(ret,"[24m")>0
	    let ret = strpart(ret,match(ret,"[24m")+4,strlen(ret))
	    let ret = strpart(ret,match(ret,"[24m")+4,strlen(ret))
	    if match(ret,"Warning: ")==-1
	    	let error = 1
	    endif
    	endif 

	if (error==0)
	    let s:Buf = s:Buf."\n".buf
	endif
		
	if (a:si==1)
		echo "# ".buf
		echo ret
	else
		echo ret
	endif
	if (error==0) 
		return a:l + i 
	else
		return last_line
	endif
endfunction

function s:ParseAll()
	call s:ResetBuf()
	let i = 1
	let last = line("$")
	while (i<=last)		
		let i = s:ParseInstr(i,1) + 1
	endwhile
endfunction

function s:AddMenu()
	menu 80.310 &OCamlSig.Parse\ &Instruction<Tab>:OCamlSigParseInstruction :OCamlSigParseInstruction<CR>
	menu 80.320 OCamlSig.Parse\ &All<Tab>:OCamlSigParseAll :OCamlSigParseAll<CR>
	menu 80.330 OCamlSig.Show\ Buffer<Tab>:OCamlSigShowOCamlBuffer :OCamlSigShowOCamlBuffer<CR>
	menu 80.340 OCamlSig.&Clear\ Buffer<Tab>:OCamlSigClearOCamlBuffer :OCamlSigClearOCamlBuffer<CR>
endfunction

if !executable("ocaml")
	echo "OCaml instruction signature"
	echo "Can't find OCaml!!!"
elseif !exists(":OCamlParseInstruction")
	command -nargs=0 OCamlSigMenu :call s:AddMenu()
	command -nargs=0 OCamlSigParseAll :call s:ParseAll()
	command -nargs=0 OCamlSigParseInstruction :call s:ParseInstr(line("."),0)
	command -nargs=0 OCamlSigClearOCamlBuffer :call s:ResetBuf()
	command -nargs=0 OCamlSigShowOCamlBuffer :call s:ShowBuf()
endif


