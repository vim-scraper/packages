" Vim plugin for OCaml instruction signature ver. 0.80-beta2
" Last Change: 2005 Mar 31
" Maintainer: Grzegorz Dymarek <125783@student.pwr.wroc.pl>" 
" Tested on OCaml 3.08.2 and VIM 6.3.20

if exists("ocaml_signature")
  finish
endif

let s:OCamlPath = ""

let ocaml_signature = 1
let s:WithPreview = 1
let s:PreviewBuf = -1 
let s:Buf = " "
let s:LastFun = ""
let s:LastFunL = -1

function Transformate(str,a,b,d)
	let t = match(a:str,a:a)
	if (t==-1)
		return a:str
	else
		return strpart(a:str,0,t).a:b.Transformate(strpart(a:str,t+a:d,strlen(a:str)-t-a:d),a:a,a:b,a:d)
	endif
endfunction

function BufWinExists()
	 return getbufvar(s:PreviewBuf,"OCamlPreview")
endfunction

function Go2B()
	let r = getbufvar(s:PreviewBuf,"OCamlPreview")
	if (r==1)
		set hidden
		let instr = "buffer ".s:PreviewBuf
		execute instr
		return 1
	endif
	return 0
endfunction

function ReturnFromB()
	bp!
	set nohidden
endfunction

function AddText(str)
	if (Go2B()==1)
		let r = match(a:str,"\n")
		let k = r + 1 
		let s = strpart(a:str,0,r)
		if (r==-1) 
			let s = a:str
		endif
		call append(line("$"),s)
		while (r>=0)
			let s = strpart(a:str,k,strlen(a:str))
			let r = match(s,"\n")
			let k = k + r + 1
			let s = strpart(s,0,r)
			if (s!="")
				call append(line("$"),"  ".s)
			endif
			if (r<0) 
				let r = -1
				call append(line("$"),"  ".strpart(a:str,k,strlen(a:str)))
			endif
		endwhile
		bp!
		exe "normal ".bufwinnr("#")."\<c-w>\<c-w>"
		norm G
		exe "normal\<c-w>\<c-p>"
		set nohidden
	else 
		let s:PreviewBuf = -1
	endif
endfunction

function s:StatusWindow()
    let r = getbufvar(s:PreviewBuf,"OCamlPreview")
    if (r==-1)
	    let s:PreviewBuf = -1
    endif
    if (s:PreviewBuf==-1)
	let s:WithPreview = 1
	let wh = winheight(winnr())
	set hidden
	enew!
"	let instr = "setfiletype ocaml"
"	execute instr
	set hidden
"	set nomodified
"	set nobuflisted
	call setbufvar(bufnr("%"),"OCamlPreview",1)
	let s:PreviewBuf = bufnr("%")
	bp!
	set nohidden
    	split
    	let instr = "buffer ".s:PreviewBuf
    	execute instr
    	wincmd J
	let wh = wh/4
    	let instr = "res ".wh
	execute instr
"	set number
	set number!
    	wincmd p
    endif
endfunction

function s:CloseStatus()
	let r = getbufvar(s:PreviewBuf,"OCamlPreview")
	if (r==1)
		let instr = "bdelete! ".s:PreviewBuf
		execute instr
	endif
	let s:PreviewBuf = -1
	let s:WithPreview = 0
endfunction
			
function AddNumber()
"	let odp = 
"execute ":l"
"	echo odp
"	if (:l!=
endfunction 

function s:ResetBuf()
	let s:Buf = ""
	let s:LastBuf = " "
	if (s:WithPreview==1)
		call s:CloseStatus()
		call s:StatusWindow()
	endif
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

function TypeDeclaration(str)
	if (stridx(a:str,"type ")==0)
	    return 1
	else
	    return 0
	endif
endfunction

function Declaration(str)
"	let h = strridx(a:str,"\n")
"	let ret = strpart(a:str,h+1,strlen(a:str)) chyba niepotrzebne

	let ret = a:str
	let val = match(ret,"val ")
	if (val==0)
		return 1
	else
		return 0
	endif
endfunction

function DeleteLastFun()
	let s:Buf = strpart(s:Buf,0,s:LastFunL)
endfunction

function Function(str)
"	let h = strridx(a:str,"\n")
"	let ret = strpart(a:str,h+1,strlen(a:str)) chyba niepotrzebne

	let ret = a:str
	let l = strlen(ret)
	let f = strpart(ret,l-5,5)
	if (f=="<fun>")
		return 1
	else
		return 0
	endif
endfunction

function Structure(str)
"	let h = strridx(a:str,"\n")
"	let ret = strpart(a:str,h+1,strlen(a:str)) chyba niepotrzebne

	let ret = a:str
	let l = strlen(ret)
	let f = strpart(ret,l-5,5)
	if (f=="<fun>")
		return 1
	else
		return 0
	endif
endfunction

function Simplify(str)
"	let h = strridx(a:str,"\n")
"	let rets = strpart(a:str,h+1,strlen(a:str))

	let rets = a:str
	let semi = match(rets,":")
	let ret = strpart(rets,0,semi-1)
	let ret = strpart(ret,4,strlen(ret)-4)
	let ret = "let ".ret." = "
	let m = stridx(a:str," =")
	let val = strpart(rets,m+2,strlen(rets))
	let ret = ret.val.";;"
	let ret = Transformate(ret,"\"","\\\"",1)
	let ret = Transformate(ret,"\\$","\\$",1)	
	return ret
endfunction

function ContainsWarning(str)
	let i = strridx(a:str,"[24mWarning")
	if (i!=-1)
	    return 1
	endif
	return 0
endfunction

function Sig(str)
	let i = strridx(a:str,"\n")
	let ret = strpart(a:str,i+1,strlen(a:str))
	return ret
endfunction

function ToBuf(instr,rett,warning)
	let ret = a:rett
	
	if (a:warning==1)
	    let ret = Sig(ret)
"	    call AddText(ret)
	endif
		
	if (TypeDeclaration(ret)==1)
"		    call AddText("type")
	    let s:LastFunL = -1
	    let s:LastFun = ""
	    let s:Buf = s:Buf."\n".a:instr
	endif
	if (Declaration(ret)==1)
"		    call AddText("dec")
		if (match(s:LastFun," ".GetFunName(ret)." ")!=-1)
			call DeleteLastFun()
		else
			let s:LastFun = " ".GetFunName(ret)." "
		endif
		if (Function(ret)==1)
"			    call AddText("fun")
			let s:LastFunL = strlen(s:Buf)
			let s:Buf = s:Buf."\n".a:instr
"		else if (Structure(ret))
"			call AddText("struct")
"			let s:LastFunL = strlen(s:Buf)
"			let s:Buf = s:Buf."\n".a:instr			
		else
"			call AddText(Simplify(ret))
			let s:LastFunL = -1
			let s:LastFun = ""
			let s:Buf = s:Buf."\n".Simplify(ret)
		endif
	endif
"	    call AddText("end")
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


function ContainsError(str)
	let h = strridx(a:str,"\n")
	let r = strpart(a:str,h+1,strlen(a:str))
	if (stridx(r,"Exception: ")==0)
		return 1
	else 
		if (match(r,"[24m")==1)
			return 1
		endif
	endif
	return 0
endfunction

function MarkError(str)
	exe "normal ".bufwinnr("#")."\<c-w>\<c-w>"
	highlight Err ctermbg=red guibg=red
	let instr = "match Err /".a:str
	execute instr
	exe "normal\<c-w>\<c-p>"
endfunction

function ParseError(str)
	if (BufWinExists()==0)
		return
	endif
	let a = stridx(a:str,"[4m")
	let b = stridx(a:str,"[24m")
	let r = strpart(a:str,0,a).strpart(a:str,a+2,b-a).strpart(a:str,b+3,strlen(a:str))

"	exe "normal gg4j"
"	execute "normal 9l"
"	let r = Transformate(a:str,"[4m","")
"	let r = Transformate(r,"[24m","")
"	let err = strpart(r,0,a)
	"strpart(r,a,b-a-4)
"	call MarkError(err)
endfunction

function ParseError1(str)
	let a = stridx(a:str,"[4m")
        let b = stridx(a:str,"[24m")	
	let s = strpart(a:str,0,a)
	let la = strridx(s,"\n")
	let s = strpart(a:str,0,b)
	let lb = strridx(s,"\n")
	echo a-la
	echo b-lb
	strpart(a:str,0)
endfunction

function FormatERet(str)
	let ret = strpart(a:str,match(a:str,"#")+1,strlen(a:str))
    	let ret = strpart(ret,match(ret,"#")+1,strlen(ret))
	return ret
endfunction

function s:ParseInstr(line,si)
	
	if (s:WithPreview==1)
		call s:StatusWindow()
	endif	
	
	let l = FindFLine(a:line)	
	let buf = getline(l)
	let num = match(buf,";;")
	let end = 0
	let error = 0
	let warning = 0
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
	let buf = strpart(buf,NumOfWChars(buf),strlen(buf))
	let buf = Transformate(buf,"\"","\\\"",1)
"	let buf = TransformateD(buf)
	let buf = Transformate(buf,"\\$","\\$",1)	
	let prog = s:Buf."\n".buf
	let s:com = "echo \"".prog."\" | ".s:OCamlPath."ocaml"
	let ret = system(s:com)	
		let last = strridx(ret, "#")-1
		let b_last = strridx( strpart(ret,0,strlen(ret)-3), "#")+2
		let ret = strpart(ret, b_last, last-b_last)

	if (ContainsError(ret)==1)
		let ret = FormatERet(ret)
		call ParseError(ret)
		let error = 1
	else
	endif
	
	if (ContainsWarning(ret)==1)
		let warning = 1
	else
	endif	
	let ret = Transformate(ret,"[4m","",3)
"	call AddNumber()
	let ret = Transformate(ret,"[24m","",4)
	if (error==0)
		let s = NumOfWChars(ret)
		let ret = strpart(ret,s,strlen(ret))
		call ToBuf(buf,ret,warning)
	endif
	if (a:si==1)
		if (s:PreviewBuf==-1)
			echo "\n# ".buf
			echo ret
		else	
			call AddText("# ".buf)
			call AddText(ret)
			call AddText("\n")
		endif
	else
		if (s:PreviewBuf==-1)
			echo ret
		else
			call AddText(ret)
			call AddText("\n")
		endif
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
	while (i<last)		
		let i = s:ParseInstr(i,a:d) + 1
	endwhile
endfunction

function s:AddMenu()
	menu 80.310 &OCamlSig.Parse.&Instruction<Tab>:OCamlSigParseInstruction :OCamlSigParseInstruction<CR>
	menu 80.320 OCamlSig.Parse.&All<Tab>:OCamlSigParseAll :OCamlSigParseAll<CR>
	menu 80.320 OCamlSig.Parse.All-&Full<Tab>:OCamlSigParseAllF :OCamlSigParseAllF<CR>	
	menu 80.340 OCamlSig.&Status\ Window.&Show<Tab>:OCamlSigStatusWinShow :OCamlSigStatusWinShow<CR>
	menu 80.350 OCamlSig.&Status\ Window.&Close<Tab>:OCamlSigStatusWinClose :OCamlSigStatusWinClose<CR>
	menu 80.370 OCamlSig.&Buffer.&Show<Tab>:OCamlSigShowOCamlBuffer :OCamlSigShowOCamlBuffer<CR>
	menu 80.380 OCamlSig.&Buffer.&Clear<Tab>OCamlSigClearOCamlBuffer :OCamlSigClearOCamlBuffer<CR>
"	menu 80.330 OCamlSig.Show\ Buffer<Tab>:OCamlSigShowOCamlBuffer :OCamlSigShowOCamlBuffer<CR>
"	menu 80.340 OCamlSig.&Clear\ Buffer<Tab>:OCamlSigClearOCamlBuffer :OCamlSigClearOCamlBuffer<CR>
endfunction

if !executable(s:OCamlPath."ocaml")
	echo "OCaml instruction signature"
	echo "Can't find OCaml!!!"
elseif !exists(":OCamlParseInstruction")
	set number
	command -nargs=0 OCamlSigMenu :call s:AddMenu()
	command -nargs=0 OCamlSigParseAll :call s:ParseAll(0)
	command -nargs=0 OCamlSigParseAllF :call s:ParseAll(1)
	command -nargs=0 OCamlSigParseInstruction :call s:ParseInstr(line("."),0)
	command -nargs=0 OCamlSigStatusWinShow :call s:StatusWindow()
	command -nargs=0 OCamlSigStatusWinClose :call s:CloseStatus()
	
	command -nargs=0 OCamlSigClearOCamlBuffer :call s:ResetBuf()
	command -nargs=0 OCamlSigShowOCamlBuffer :call s:ShowBuf()
endif
