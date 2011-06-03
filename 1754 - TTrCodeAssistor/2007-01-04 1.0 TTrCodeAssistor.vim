"        File: TTrCodeAssistor.vim
"     Authors: winterTTr <winterTTr.vim@gmail.com>

" Description: Sometimes we do not need a tool which is so complex to
"              manage it. We just need a tool which is easy to used even
"              without learning it , although the tool is just a little
"              poor. So i write this. Hope to give you just a littel
"              help during your coding process. :-)
"
"     Porpose: This script is mainly used to help you input you coding.
"
"       Usage:                                                     {{{1
"              1 . map key
"                 <tab>     or Tab        --- move forward
"                 <s-tab>   or Shift+tab  --- move backward
"                 <c-space> or ctrl+space --- expand the 'key' word
"
"              2 . 'key' word table
"
"      Edit File type(&ft)       'key' word        expand result
"     --------------------------------------------------------------------
"        cpp/c/h                  for              <for> statement
"                                 switch           <switch> statement
"                                 ife              <if> statement (with else)
"                                 if               <if> statement (no else)
"                                 df               #ifndef __FILENAME__ 
"                                                  #define __FILENAME__
"                                                  #endif /*__FILENAME__*/
"                                 c                /* <cur> */
"                                 d                #define ...
"                                 i                #include \"<cur>\"
"                                 is               #include  < <cur> >
"        any file                 xt               insert the time (now!)
"     --------------------------------------------------------------------
                
"              3 . All the 'key' word should be input under the <insert>
"                  mode with <c-space> RIGHT AFTER it to expand.
"                                                                    }}}1
"
"   Bug Report: This is the first time i wrote vim scrips, so there must
"               be a lot of bugs now , if you find some or you just have
"               some good suggestions , mail to me . Thanks to your
"               ideas.
"       Thanks: thands to my best friend -Ming Bai- who gives many help 
"               and good ideas.
"               Also i have learn many thing from a.vim and latex-suite, so
"               i must say that i stand on the should of gaint . ^_^

" Make sure the vim version is 7.0 ,and don't load twice{{{1
if v:version < 700
	finish
endif

if exists("g:TTrCodeAssistor")
	finish
endif

let g:TTrCodeAssistor = 1
"-------------------------------------------------- }}}1

" --- TTrCodeAssistor --- {{{1

" Global option variable {{{2
let g:TTrCodeAssistor_St="<+"         " Used to pack the word for Assistor
let g:TTrCodeAssistor_En="+>"         " You can change it to the key which willn't appear during you edit
let g:TTrCodeAssistor_RemeberUserInput = 0
let g:TTrCodeAssistor_UserInputVar = ""
let g:TTrCodeAssistor_CursorPos = []     "used to remeber the cursor's positon
let g:TTrCodeAssistor_CallingMoveFirst = "\<c-r>=TTrCodeAssistor_MoveFirst()\<CR>"
" }}}2

" maps {{{2
inoremap <silent> <c-space>  <c-r>=TTr_ExpandTemplate()<cr>
inoremap <silent> <tab> <c-r>=TTrCodeAssistor_Action("")<CR>
snoremap <silent> <tab> <left>a<c-r>=TTrCodeAssistor_Action("")<CR>
snoremap <silent> <s-tab> <left>F<i<c-r>=TTrCodeAssistor_Action("b")<CR>
" }}}2

" functions {{{2
function! s:TTrCodeAssistor_GetScope(if_return,direction)
	" remember user input
	if g:TTrCodeAssistor_RemeberUserInput == 1
		let g:TTrCodeAssistor_UserInputVar = substitute(getline(".")[:col(".")-2],'\zs.*\W\ze\w*$','','g')
		let g:TTrCodeAssistor_RemeberUserInput = 0
	endif

	let ReturnString = ""
	" search the _St and move the cursor to there(begin)
	let SelectLineNum = search(g:TTrCodeAssistor_St,'w'.a:direction)

	"if can't find , return NULL
	if SelectLineNum == 0 
		return ""
	else
		" get the word between _St and _En
		let ReturnString = matchstr(getline(".")[col(".")-1:],g:TTrCodeAssistor_St.'\zs.\{-}\ze'.g:TTrCodeAssistor_En)
		normal v
		let SelectLineNum = search(g:TTrCodeAssistor_En,'eW')
		if a:if_return == 0
			return ""
		else
			return ReturnString
		endif
	endif
endfunction

function! TTrCodeAssistor_Action(direction)
	let UserInputString = "\<C-\>\<C-N>gv\<C-G>"
	let PreInputString = "\<C-\>\<C-N>gvc"
	let PositionContent = s:TTrCodeAssistor_GetScope(1,a:direction)

	"if can't find _St and _En pattern
	if PositionContent == ""
		return "\<tab>"
	endif

	" On the case for -->for<--
	if PositionContent == "var_b"
		let g:TTrCodeAssistor_RemeberUserInput = 1
		return UserInputString
	elseif PositionContent == "var_c"
		if g:TTrCodeAssistor_UserInputVar == ""
			return UserInputString
		else
			return PreInputString.g:TTrCodeAssistor_UserInputVar."\<C-R>=TTrCodeAssistor_Action(\"\")\<CR>"
		endif
	endif

	return UserInputString
endfunction

function! TTrCodeAssistor_MoveFirst()
	call setpos('.',g:TTrCodeAssistor_CursorPos)
	return "\<c-r>=TTrCodeAssistor_Action(\"\")\<CR>"
endfunction
function! TTr_ExpandTemplate()
	let g:TTrCodeAssistor_CursorPos = getpos(".")
    let cword = substitute(getline('.')[:(col('.')-2)],'\zs.*\W\ze\w*$','','g')
    if exists('g:TTr_template' . &ft . cword)
        return "\<C-W>" . g:TTr_template{&ft}{cword}
    elseif exists('g:TTr_template_' . cword)
        return "\<C-W>" . g:TTr_template_{cword}
    endif
	return ""
endfunction
function! GetFileNameForDefine()
    let filename=expand("%:t")
    let filename=toupper(filename)
    let _name=substitute(filename,'\.','_',"g")
    let _name="__"._name."__"
    return _name
endfunction
" }}}2

" TTrCodeAssistor libs {{{2
let g:TTrCodeAssistor_Libs{'for'} = "for( ".
										\g:TTrCodeAssistor_St."var_b".g:TTrCodeAssistor_En." = ".
										\g:TTrCodeAssistor_St."start".g:TTrCodeAssistor_En." ; ".
										\g:TTrCodeAssistor_St."var_c".g:TTrCodeAssistor_En." ".
										\g:TTrCodeAssistor_St."op".g:TTrCodeAssistor_En." ".
										\g:TTrCodeAssistor_St."end".g:TTrCodeAssistor_En." ; ".
										\g:TTrCodeAssistor_St."var_c".g:TTrCodeAssistor_En." += ".
										\g:TTrCodeAssistor_St."inc".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
										\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}\<CR>".
										\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
										\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'while'} = "while ( ".
									\g:TTrCodeAssistor_St."expression".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
									\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}\<CR>".
									\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
									\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'if'} = "if ( ".
								\g:TTrCodeAssistor_St."expression".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}\<CR>".
								\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'ife'} = "if ( ".
								\g:TTrCodeAssistor_St."expression".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}\<CR>else\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}\<CR>".
								\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'switch'} = "switch ( ".
									\g:TTrCodeAssistor_St."expr".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
									\repeat("case ".g:TTrCodeAssistor_St."case".g:TTrCodeAssistor_En." :\<CR>".g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.";\<CR>break;\<CR>",3).
									\"default :\<CR>".
									\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.";\<CR>break;\<CR>}\<CR>".
									\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
									\g:TTrCodeAssistor_CallingMoveFirst
" }}}2

" --- ExpandTemplate Libs --- {{{2
" common templates
let g:TTr_template{'_'}{'xt'} = "\<c-r>=strftime(\"%Y-%m-%d %H:%M:%S\")\<cr>"
let g:ExpandTemplateCommonLibs{'df'} = "#ifndef  \<C-R>=GetFileNameForDefine()".repeat("\<CR>",2).
                                 \"#define  \<C-R>=GetFileNameForDefine()".repeat("\<cr>",6).
                                 \"#endif  /*\<C-R>=GetFileNameForDefine()\<CR>*/".repeat("\<up>",3)
" C TTr_templates
let g:TTr_template{'c'}{'c'} = "/*  */\<left>\<left>\<left>"
let g:TTr_template{'c'}{'d'} = "#define  "
let g:TTr_template{'c'}{'i'} = "#include  \"\"\<left>"
let g:TTr_template{'c'}{'is'} = "#include  <>\<left>"
let g:TTr_template{'c'}{'df'} = g:ExpandTemplateCommonLibs{'df'}
let g:TTr_template{'c'}{'for'} = g:TTrCodeAssistor_Libs{'for'}
let g:TTr_template{'c'}{'while'} = g:TTrCodeAssistor_Libs{'while'}
let g:TTr_template{'c'}{'if'} = g:TTrCodeAssistor_Libs{'if'}
let g:TTr_template{'c'}{'switch'} = g:TTrCodeAssistor_Libs{'switch'}
let g:TTr_template{'c'}{'ife'} = g:TTrCodeAssistor_Libs{'ife'}
" C++ TTr_templates
let g:TTr_template{'cpp'}{'c'} = "/*  */\<left>\<left>\<left>"
let g:TTr_template{'cpp'}{'d'} = "#define  "
let g:TTr_template{'cpp'}{'i'} = "#include  \"\"\<left>"
let g:TTr_template{'cpp'}{'is'} = "#include  <>\<left>"
let g:TTr_template{'cpp'}{'df'} = g:ExpandTemplateCommonLibs{'df'}
let g:TTr_template{'cpp'}{'for'} = g:TTrCodeAssistor_Libs{'for'}
let g:TTr_template{'cpp'}{'while'} = g:TTrCodeAssistor_Libs{'while'}
let g:TTr_template{'cpp'}{'if'} = g:TTrCodeAssistor_Libs{'if'}
let g:TTr_template{'cpp'}{'switch'} = g:TTrCodeAssistor_Libs{'switch'}
let g:TTr_template{'cpp'}{'ife'} = g:TTrCodeAssistor_Libs{'ife'}
" --------------------------- }}}2

"}}}1


" vim: set ft=vim ff=unix foldmethod=marker :
