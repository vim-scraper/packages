" Vim plugin for OCaml instruction signature ver. 0.71
" Last Change: 2005 Mar 04
" Maintainer: Grzegorz Dymarek <125783@student.pwr.wroc.pl>" 
" Tested on OCaml 3.08.2 and VIM 6.3.20

if exists("ocaml_signature")
  finish
endif
	
let ocaml_signature = 1
let s:PreviewBuf = 0
let s:Buf = " "
let s:LastFun = ""
let s:LastFunL = -1

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
	let s:LastBuf = " "
endfunction

function s:ShowBuf()
	if (s:Buf=="")
		echo "OCaml buffer is empty!"
	else
		echo s:Buf
	endif
endfunction

function NumOfWChars(str)
	let a = match(a:str," ")
	let b = match(a:str,"\n")
	if (a==0 || b==0)
		return 1 + NumOfWChars(strpart(a:str,1,strlen(a:str)-1))
	else 
		return 0
	endif
endfunction

function GetFunName(str)
	let semi = match(a:str," :")
	let val = match(a:str,"val ")
	let ret = strpart(a:str,val+4,semi-val-4)
	return ret
endfunction

function Declaration(str)
	let semi = match(a:str," :")
	let val = match(a:str,"val ")
	if (val>=0 && val<semi)
		return 1
	else
		return 0
	endif
endfunction

function DeleteLastFun()
	let s:Buf = strpart(s:Buf,0,s:LastFunL)
endfunction

function ToBuf(instr,ret)
	if (Declaration(a:ret)==1)
		if (match(s:LastFun," ".GetFunName(a:ret))!=-1)
			call DeleteLastFun()
		else
			let s:LastFun = " ".GetFunName(a:ret)
		endif
		let s:LastFunL = strlen(s:Buf)
		let s:Buf = s:Buf."\n".a:instr
	endif
endfunction

function Transformate(str,a,b)
	let t = match(a:str,a:a)
	if (t==-1) 
		return a:str
	else
		return strpart(a:str,0,t).a:b.Transformate(strpart(a:str,t+strlen(a:a),strlen(a:str)-t-strlen(a:a)),a:a,a:b)
	endif
endfunction

function FindFLine(l)
	let t = a:l - 1
	if (t<0)
		return 0
	else
		if (match(getline(t),";;")!=-1)
			return t + 1
		else
			return FindFLine(t)
		endif
	endif
endfunction

function s:ParseInstr(line,si)
	
	if (s:PreviewBuf==-1)
		call StatusWindow()
	endif	
	
	let l = FindFLine(a:line)	
	let buf = getline(l)
	let num = match(buf,";;")
	let end = 0
	let error = 0
	let i = 0
	let last_line = line("$")	
	while (num<0 && end==0)
		let i = i + 1
		if (i>last_line)
			let end = 1
		else
			let next_line = getline(l+i)
			let num = match(next_line,";;")	
			if (next_line!="")
				let buf = buf ."\n". next_line
			endif
		endif
	endwhile
	let buf = Transformate(buf,"\"","\\\"")
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
"		call ParseAll(0)
"		call ParseInstr(l,si)
	    	let error = 1
	    endif
    	endif 

	if (error==0)
		let s = NumOfWChars(ret)
		let ret = strpart(ret,s,strlen(ret))
		call ToBuf(buf,ret)
	endif
		
	if (a:si==1)
		echo "\n# ".buf
		echo ret
	else
		echo ret
	endif
	if (error==0) 
		return l + i 
	else
		return last_line
	endif
endfunction

function s:ParseAll(d)
	call s:ResetBuf()
	let i = 1
	let last = line("$")
	while (i<=last)		
		let i = s:ParseInstr(i,a:d) + 1
	endwhile
endfunction

function s:AddMenu()
	menu 80.310 &OCamlSig.Parse\ &Instruction<Tab>:OCamlSigParseInstruction :OCamlSigParseInstruction<CR>
	menu 80.320 OCamlSig.Parse\ &All<Tab>:OCamlSigParseAll :OCamlSigParseAll<CR>
	menu 80.320 OCamlSig.Parse\ All-&Short<Tab>:OCamlSigParseAllF :OCamlSigParseAllF<CR>	
	menu 80.330 OCamlSig.Show\ Buffer<Tab>:OCamlSigShowOCamlBuffer :OCamlSigShowOCamlBuffer<CR>
	menu 80.340 OCamlSig.&Clear\ Buffer<Tab>:OCamlSigClearOCamlBuffer :OCamlSigClearOCamlBuffer<CR>
endfunction

if !executable("ocaml")
	echo "OCaml instruction signature"
	echo "Can't find OCaml!!!"
elseif !exists(":OCamlParseInstruction")
	command -nargs=0 OCamlSigMenu :call s:AddMenu()
	command -nargs=0 OCamlSigParseAll :call s:ParseAll(0)
	command -nargs=0 OCamlSigParseAllF :call s:ParseAll(1)
	command -nargs=0 OCamlSigParseInstruction :call s:ParseInstr(line("."),0)
	command -nargs=0 OCamlSigClearOCamlBuffer :call s:ResetBuf()
	command -nargs=0 OCamlSigShowOCamlBuffer :call s:ShowBuf()
endif
