"        File: TTrCodeAssistor.vim
"     Authors: winterTTr <winterTTr.vim@gmail.com>
"  UpdateDate: 2007-01-26

" Description: Help you to accelerate your inputing coding . ;-)
"              What you need to do is just see the 'key' tables in the 
"              Usage sectoin and then enjoy it by yourself.
"              If you want the ability to complete the function prototype
"              read the Usage more seriously :-)
"              Hope you can get even just a little convenience from this .
"              
"
"     Porpose: This script is mainly used to help you to accelerate your
"              input as easy as possible.
"
"       Usage:                                                     {{{1
"              1 . map key
"                 <tab>     or Tab        --- move forward
"                 <s-tab>   or Shift+tab  --- move backward
"                 <c-cr> or ctrl+enter    --- expand the 'key' word
"                 (                       --- Auto complete the function 
"                                         prototype
"                 )                       --- move or insert automantically
"                 <BS> or <backspace>     --- it will auto delete the )
"                                         while the ) is leaded by ( if
"                                         you just input like (<cur_pos>)
"
"              2 . You can use the '(' in the insert mode to complete
"                  the function prototype according to your tagfile. And the 
"                  '(' is intelligentized , it will decide how it action by
"                  itself.
"                  Note:
"                      You MUST use the Exuberant ctags and MUST execute the
"                  command with arguments '--fields=+S' , if not , the '('
"                  will not have his ability :-). Make sure !!!!
"                      After that don't forget to tell the vim you tag files
"                  by the command :set tags+=/your/tags/file
"
"              3 . The key ')' will decide how to acton automatically.
"                  Ex :
"                      When you input:
"                          function_name (  <cursor_pos> )
"                      and input the ) , it will just move without input ')'
"                      , so the result would be like this
"                          function_name (   )<cursor_pos>
"              4 . Auto Start
"                  You can let the plugin to start automatically.
"                  put it into your vimrc:
"                      let g:TTrCodeAssistor_AutoStart=1
"
"              5 . All the 'key' word should be input under the <insert>
"                  mode with <c-cr> RIGHT AFTER it to expand.
"
"              6 . 'key' word table
"
"       File type(&ft)  'key'shortcut/full/help remember    expand result
"     --------------------------------------------------------------------
"        cpp/c/h          fo/for/[fo]r                    <for> statement
"                         sw/switch/[sw]itch              <switch> statement
"                         wh/while/[wh]ile                <while> statement
"                         ife/ife/[if] with [e]lse        <if> statement (with else)
"                         if/if/if                        <if> statement (no else)
"                         df/df/[d]efine the [f]ile       #ifndef __FILENAME__ 
"                                                         #define __FILENAME__
"                                                         #endif /*__FILENAME__*/
"                         c/c/[c]omments                  /* <cur> */
"                         d/d/[d]efine                    #define ...
"                         dn/dn/[d]efine with [n]o        #ifndef ...
"                                                         #define ... 
"                                                         #endif  /* ... */
"                         i/i/[i]clude                    #include \"<cur>\"
"                         is/is/[i]clude [s]ystem file    #include  < <cur> >
"                         main/main/[main] process        int main(...)...
"        any file         it/it/[i]nsert [t]ime           insert the time (now!)
"     --------------------------------------------------------------------
"
"              7 . move the cursor to the line and call TTrCodeAssistor_ToggleComment
"                  to add or remove the comment.
"                  You can also visual select a area and call the function.
"                  To make a convienint way , you can give it to a map like:
"                  nnoremap <F7> :call TTrCodeAssistor_ToggleComments()<CR>
"                  vnoremap <F7> :call TTrCodeAssistor_ToggleComments()<CR>
"                  
                
"                                                                    }}}1
"
"   Bug Report:                                                      {{{1
"               1.0 This is the first time i wrote vim scrips, so there must
"                   be a lot of bugs now , if you find some or you just have
"                   some good suggestions , mail to me . Thanks to your
"                   ideas.
"               2.0 When the vim option [selecton] is set to 'exlusive' ,
"                   the selected area is not totally selected.So add some 
"                   solution to this( thanks to Nikolay Golubev)        
"               3.1 When the function name is start like a.b , the b can't
"                   be recognized correctly , so i change the regular exp
"                   to make it can find it out.
"               3.2 The tags can't match exactly .  The menu doesn't display
"                   right .
"                                                                    }}}1
" 
"Modify Record:                                                      {{{1
"               2.0 
"                   -- Modify some the structure of the main process to make it 
"                      more clearly. Add  expand [nd].
"                   -- Change the action to the for , when user input += 1 or
"                      += -1 , change it to ++ and -- . 
"                   -- Auto make the word upercase when user input the define.
"                   -- According to the user report that the map <c-space> has 
"                      conflict to  the hot key of changing input method ,so i 
"                      change it to <c-cr>
"                   -- Add new 'key' word called 'nd' , see the table
"                   -- All the key word is shortened to just TWO charactors
"                   -- Change the key 'xt' to 'it' to make it easy to be remebered
"               3.0 
"                   -- Add the function which let the '(' key in the insert mode
"                      to have the ability to complete the function prototype 
"                      according to the tag files , make sure that , you make the 
"                      tag files via the correct command and the correct arguments.
"                      See usage!!
"                   -- Add the expand 'main'
"               3.1
"                   -- Add the ability for key ')' , make it more intelligent. It
"                      will decide if you need to input ')' or just move to the next
"                      ')' , and will warning you when the ')' is not match.
"               3.2
"                   -- Add the Menu for this plugin which let the user can                   
"                      start or stop all the functions.
"                   -- Add TTrCodeAssistor_AutoStart variable which can let
"                      this plugin start automaticlly.
"                   -- Fix the problem when the Function name is followed
"                      by its owner ,such as , the member function of a class.
"
"               3.3
"                   -- Let the menu to display rightly
"                   -- Add the function for <bs> to make it delete the redundant
"                      ) while the ) is close to (
"                   -- Modify the code to match the function name exactly
"               3.4 
"                   -- Add function TTrCodeAssistor_ToggleComment to add or
"                      remove comments of the current line or v selected area.
"
"                                                                    }}}1
"
"       Thanks:                                                      {{{1
"               Thands to my best friend -Ming Bai- who gives many help 
"               and good ideas.
"               Also i have learn many thing from a.vim and latex-suite, so
"               i must say that i stand on the shoulder of gaint . ^_^
"                                                                    }}}1


" ==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
" Make sure the vim version is 7.0 ,and don't load again {{{1
if v:version < 700
	echoe "TTrCodeAssistor.vim requires the version 7.0 or higher."
	finish
endif

if exists("g:TTrCodeAssistor")
	finish
endif

let g:TTrCodeAssistor = 1
"======================================================= }}}1

" === TTrCodeAssistor === {{{1

" ***  Functions *** {{{2

" Functions: TTrCodeAssistor_ExpandTemplates() ( without return )
"     Usage: Use to expand the 'key' word to the predefine templates
function! TTrCodeAssistor_ExpandTemplates() "{{{3
	let g:TTrCodeAssistor_CursorPos = getpos(".")
    let cword = substitute(getline('.')[:(col('.')-2)],'\zs.*\W\ze\w*$','','g')
    if exists('g:TTrCodeAssistor_Templates' . &ft . cword)
        return "\<C-W>" . g:TTrCodeAssistor_Templates{&ft}{cword}
    elseif exists('g:TTrCodeAssistor_Templates_' . cword)
        return "\<C-W>" . g:TTrCodeAssistor_Templates_{cword}
    endif
	return ""
endfunction "}}}3

" Functions: GetFileNameForDefine()( without return )
"     Usage: Get the file name and make it like __FILENAME_EXP__
function! GetFileNameForDefine() "{{{3
    let filename=expand("%:t")
    let filename=toupper(filename)
    let _name=substitute(filename,'\.','_',"g")
    let _name="__"._name."__"
    return _name
endfunction "}}}3

" Functions: TTrCodeAssistor_MoveFirst()( return calling the Action function )
"     Usage: Used to move the cursor to the beginning of the where the key
"            word is expanded , and then , begin the Action
function! TTrCodeAssistor_MoveFirst() "{{{3
	call setpos('.',g:TTrCodeAssistor_CursorPos)
	return "\<c-r>=TTrCodeAssistor_Action(\"\")\<CR>"
endfunction "}}}3

" Functions: TTrCodeAssistor_Action(direction)( return the string about how to action )
"     Usage: MAIN process of the selection and relative actions
function! TTrCodeAssistor_Action(direction) "{{{3
	if g:TTrCodeAssistor_GotoBeginning == 1
		call setpos('.',g:TTrCodeAssistor_CursorPos)
		let g:TTrCodeAssistor_GotoBeginning = 0 
	endif
	call s:GetUserInputBeforeMove()
    call s:ActionBeforeGetArea()
	let PositionContent = s:GetArea(a:direction)
	return s:MarkAndReturn(PositionContent)
endfunction "}}}3

" Functions: s:GetUserInputBeforeMove( without return )
"     Usage: Used to get the string about what user inputed in to TTrCodeAssistor_UserInput
function! s:GetUserInputBeforeMove() "{{{3
	" Just jump , don't need to remeber
	if g:TTrCodeAssistor_ActionType == "J" 
		return 
	elseif g:TTrCodeAssistor_ActionType == "RJ"
		let g:TTrCodeAssistor_UserInput = substitute(getline(".")[:col(".")-2],'\zs.*\s\+\ze[-]\{0,1}1$','','g') 
	elseif g:TTrCodeAssistor_ActionType == "SJ"
		let g:TTrCodeAssistor_UserInput = substitute(getline(".")[:col(".")-2],'\zs.*\W\ze\w*$','','g')
	elseif g:TTrCodeAssistor_ActionType == "UJ"
		let g:TTrCodeAssistor_UserInput = substitute(getline(".")[:col(".")-2],'\zs.*\W\ze\w*$','','g')
		let g:TTrCodeAssistor_UserInput = toupper(g:TTrCodeAssistor_UserInput)
	endif
	if g:TTrCodeAssistor_UserInput =~ '[A-Za-z0-9_()]*\('.g:TTrCodeAssistor_St.'\)\+' || g:TTrCodeAssistor_UserInput =~ '[A-Za-z0-9_()]*\('.g:TTrCodeAssistor_En.'\)\+'
		g:TTrCodeAssistor_UserInput = ""
	endif
endfunction "}}}3

" Functions: s:ActionBeforeGetArea() ( without return )
"     Usage: Take action before the next move
function! s:ActionBeforeGetArea() "{{{3
	if g:TTrCodeAssistor_ActionType == "RJ"
		if g:TTrCodeAssistor_UserInput == "1" || g:TTrCodeAssistor_UserInput == "-1"
			call setline(".",substitute(getline("."),'+=\s\+[-]\{0,1}1',g:TTrCodeAssistor_UserInput=="1" ? '++' : '--',''))
		endif
	elseif g:TTrCodeAssistor_ActionType == "UJ"
		if g:TTrCodeAssistor_UserInput == ""
			return
		else
			call setline(".",substitute(getline("."),g:TTrCodeAssistor_UserInput,g:TTrCodeAssistor_UserInput,'gi'))
		endif
	endif
endfunction "}}}3

" Functions: s:GetArea(direction) (return the word in the area)
"     Usage: Take action before the next move
function! s:GetArea(direction) "{{{3
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
		if SelectLineNum == 0 
			return ""
		else
			return ReturnString
		endif
	endif
endfunction "}}}3

" Functions: s:MarkAndReturn(content) (return the string how to action)
"     Usage: return the action string and mark how to actiontype for next time
function s:MarkAndReturn(content) "{{{3

	let selectionString = ""
	if &selection == 'exclusive'
		let selectionString = "gvl"
	else
		let selectionString = "gv"
	endif

	let UserInputString = "\<C-\>\<C-N>".selectionString."\<C-G>"
	let PreInputString = "\<C-\>\<C-N>".selectionString."c"
	
	"if can't find _St and _En pattern
	if a:content == ""
		return "\<tab>"
	endif

	let g:TTrCodeAssistor_ActionType = "J"

	"Set action type according to the a:content
	if a:content == "var_b"
		let g:TTrCodeAssistor_ActionType  = "SJ"
		return UserInputString
	elseif a:content == "var_c"
		let g:TTrCodeAssistor_ActionType = "J"
		if g:TTrCodeAssistor_UserInput == "" 
			return UserInputString
		else
			return PreInputString.g:TTrCodeAssistor_UserInput."\<C-R>=TTrCodeAssistor_Action(\"\")\<CR>"
		endif
	elseif a:content == "inc_r"
		let g:TTrCodeAssistor_ActionType = "RJ"
		return UserInputString
	elseif a:content == "var_u"
		let g:TTrCodeAssistor_ActionType = "UJ"
		return UserInputString
	endif

	return UserInputString
endfunction "}}}3

" Functions: TTrCodeAssistor_CompleteFuncPrototyoeFromTags() (return the string '' or '()' )
"     Usage: using tag files to complete the funtion declaration
function! TTrCodeAssistor_CompleteFuncPrototypeFromTags() "{{{3
	"Remeber the position
	let g:TTrCodeAssistor_CursorPos = getpos('.')

	" Check whether the ( need to call complete
	let LBracketNum = strlen( substitute( getline('.') , '[^(]','','g' ) )
	let RBracketNum = strlen( substitute( getline('.') , '[^)]','','g' ) )

	if LBracketNum < RBracketNum
		return "("
	endif

	" Get the function Name
	let FuncName = substitute(getline('.')[:col('.')-2],'\m\zs.\{-}\ze\w\+\s*$','','')
	let FuncName = substitute(FuncName,'\m\s\+','','g')

	if FuncName !~ '\m^\w\+$'
		return '()'."\<Left>"
	endif
	
	"Get the prototyoe from tags
	let FuncInfo = []
	let FuncInfo = taglist('^'.FuncName.'$')

	if FuncInfo == [] "Can't find one
		return '()'."\<Left>"
	endif

	"Get complete infomations
	let CompleteInfo = []
	for item in FuncInfo
		if has_key(item,'name') && has_key(item,'kind') && has_key(item,'signature')
			if item.kind == 'p' || item.kind == 'f'
				let DealedSig = substitute( item.signature , '(', '( '.g:TTrCodeAssistor_St,'')
				let DealedSig = substitute( DealedSig , ',' , g:TTrCodeAssistor_En.' , '.g:TTrCodeAssistor_St , 'g')
				let DealedSig = substitute( DealedSig , ')' , ' '.g:TTrCodeAssistor_En.' )','')
				call add( CompleteInfo , DealedSig )
			endif
		endif
	endfor

	if len( CompleteInfo ) == 0
		return "()"."\<Left>"
	elseif len( CompleteInfo ) == 1
		return CompleteInfo[0].g:TTrCodeAssistor_CallingMoveFirst
	endif

	let g:TTrCodeAssistor_GotoBeginning = 1
	call complete(col('.'),CompleteInfo)

	return ""
endfunction "}}}3

" Functions: TTrCodeAssistor_IntelligentRightBracket() (return the string '' or ')' )
"     Usage: Let the right bracket have the ability to insert or just move itself
function! TTrCodeAssistor_IntelligentRightBracket() "{{{3
	"Calculate the number of the left/right bracket 
	let LBracketNum = strlen( substitute( getline('.') , '[^(]','','g' ) )
	let RBracketNum = strlen( substitute( getline('.') , '[^)]','','g' ) )

	if RBracketNum == LBracketNum
		let RightCharactor = substitute( getline('.')[col('.')-1:] , '\m^\s*)\zs.*\ze$','','')
		let RightCharactor = substitute( RightCharactor , '\m\s*','','g')
		if strlen( RightCharactor ) == 1 && RightCharactor == ')'
			call search(')','c')
			let CursorPos = getpos('.')
			normal l
			let NewCursorPos = getpos('.')
			if NewCursorPos[2] == CursorPos[2]
				call cursor( line('.'),strlen(getline('.'))+1 )
			endif
			return ''
		else
			setlocal noshowmode
			echohl WarningMsg | echo 'Odd Right Bracket!' | echohl None
			return ')'
		endif
	elseif  RBracketNum > LBracketNum
		setlocal noshowmode
		echohl WarningMsg | echo "Odd Right Bracket!" | echohl None
		return ')'
	else
		return ')'
	endif

	return ''
endfunction "}}}3

" Functions: TTrCodeAssistor_IntelligentBackspace() (return the string '' or '<BS>' )
"     Usage: Let the right bracket have the ability to insert or just move itself
function! TTrCodeAssistor_IntelligentBackspace() "{{{3
	let LeftChar = getline('.')[col('.')-2]
	if LeftChar =='('
		let RightChar = getline('.')[col('.')-1]
		if RightChar == ')'
			"Calculate the number of the left/right bracket 
			let LBracketNum = strlen( substitute( getline('.') , '[^(]','','g' ) )
			let RBracketNum = strlen( substitute( getline('.') , '[^)]','','g' ) )
			if LBracketNum == RBracketNum
				let CurPos = getpos('.')
				normal x
				let newCurPos = getpos('.')
				if newCurPos[2] != CurPos[2]
					call cursor(line('.'),strlen(getline('.'))+1 )
				endif
				return ''
			else
				return "\<BS>"
			endif
		endif
	endif
	return "\<BS>"
endfunction "}}}3

" Functions: TTrCodeAssistor_Start()
"     Usage: Used to start all the functions of this plugin
function! TTrCodeAssistor_Start() "{{{3
	if s:TTrCodeAssistor_isStart == 0
		let s:TTrCodeAssistor_isStart = 1

		silent! nnoremenu <silent> &Plugin.&TTrCodeAssistor.*Star&t :call TTrCodeAssistor_Start()<CR>
		silent! nnoremenu <silent> &Plugin.&TTrCodeAssistor.\ Sto&p  :call TTrCodeAssistor_Stop()<CR>

		silent! nunmenu &Plugin.&TTrCodeAssistor.\ Star&t
		silent! nunmenu &Plugin.&TTrCodeAssistor.*Sto&p

		" maps
		inoremap <silent> <c-cr>  <c-r>=TTrCodeAssistor_ExpandTemplates()<CR>
		inoremap <silent> <tab>   <c-r>=TTrCodeAssistor_Action("")<CR>
		inoremap <silent> <s-tab>   <c-r>=TTrCodeAssistor_Action("b")<CR>
		snoremap <silent> <tab>   <left>a<c-r>=TTrCodeAssistor_Action("")<CR>
		snoremap <silent> <s-tab> <left>F`i<c-r>=TTrCodeAssistor_Action("b")<CR>
		inoremap <silent> ( <C-R>=TTrCodeAssistor_CompleteFuncPrototypeFromTags()<CR>
		inoremap <silent> ) <C-R>=TTrCodeAssistor_IntelligentRightBracket()<CR>
		inoremap <silent> <BS> <C-R>=TTrCodeAssistor_IntelligentBackspace()<CR>

		"autocmd
		augroup TTrCodeAssistor
			au!
			autocmd InsertLeave * :echohl WarningMsg |echo "" | echohl None |let &showmode=s:ShowMode_UO
		augroup END
	endif
endfunction "}}}3

" Functions: TTrCodeAssistor_Stop()
"     Usage: Used to stop all the functions of this plugin
function! TTrCodeAssistor_Stop() "{{{3
	if s:TTrCodeAssistor_isStart == 1
		let s:TTrCodeAssistor_isStart = 0

		silent! nnoremenu <silent> &Plugin.&TTrCodeAssistor.\ Star&t :call TTrCodeAssistor_Start()<CR>
		silent! nnoremenu <silent> &Plugin.&TTrCodeAssistor.*Sto&p  :call TTrCodeAssistor_Stop()<CR>

		silent! nunmenu &Plugin.&TTrCodeAssistor.*Star&t
		silent! nunmenu &Plugin.&TTrCodeAssistor.\ Sto&p

		" maps
		iunmap <c-cr>
		iunmap <tab>
		iunmap <s-tab>
		sunmap <tab>
		sunmap <s-tab>
		iunmap (
		iunmap )
		iunmap <BS>

		"autocmd
		augroup TTrCodeAssistor
			au!
		augroup END
	endif
endfunction "}}}3

" Functions:TTrCodeAssistor_ToggleComments()
"     Usage: Visual Select the area and call this function
function! TTrCodeAssistor_ToggleComments() range "{{{3
	if !exists( 'g:TTrCodeAssistor_Templates'. &ft . 'comment_char')
		return
	endif
	let comment_charactors = g:TTrCodeAssistor_Templates{&ft}{'comment_char'}
	let LineContent = getline(a:firstline)
	let pos = match( LineContent , "^" . comment_charactors  )
	if pos == 0
		execute a:firstline . "," . a:lastline . 's/^' . comment_charactors .'//'
	else
		execute a:firstline . "," . a:lastline . 's/^/' . comment_charactors . '/'
	endif
endfunction "}}}3
" ****************** }}}2

" *** Auto Start *** {{{2
" The Flag : whether the Functions is started|0--STOP 1--START
let s:TTrCodeAssistor_isStart = 0

if !exists("g:TTrCodeAssistor_AutoStart")
	let g:TTrCodeAssistor_AutoStart=0
endif

if g:TTrCodeAssistor_AutoStart == 1
	call TTrCodeAssistor_Start()
else
	"make menu
	silent! nnoremenu &Plugin.&TTrCodeAssistor.\ Star&t :call TTrCodeAssistor_Start()<CR>
	silent! nnoremenu &Plugin.&TTrCodeAssistor.*Sto&p  :call TTrCodeAssistor_Stop()<CR>
endif
" ****************** }}}2

" *** Global option variable ***{{{2
let g:TTrCodeAssistor_St='`|'         " Used to wrap the word for Assistor , like `|word|`
let g:TTrCodeAssistor_En='|`'         " Please DON'T change it.
let g:TTrCodeAssistor_UserInput = ""  " Remeber the user input string
let g:TTrCodeAssistor_CursorPos = []  " Used to remeber the cursor's positon
let g:TTrCodeAssistor_CallingMoveFirst = "\<c-r>=TTrCodeAssistor_MoveFirst()\<CR>"
let g:TTrCodeAssistor_GotoBeginning = 0  " Used to decide whether the cursor should 
                                         " goto the beginning before move
let s:ShowMode_UO = &showmode            " Remeber the user option for showmod

" Define the type of action :
"                1 . J   --- Jump ( default : just jump )
"                2 . RJ  --- Replace -> Jump
"                3 . SJ  --- Store(user input) -> jump
"                4 . UJ  --- Uppercase -> Jump
let g:TTrCodeAssistor_ActionType = "J"
"let g:TTrCodeAssistor_RemeberUserInput = 0
"let g:TTrCodeAssistor_NeedModify = 0  "if the use input need to modify 
" ******************************}}}2		

"  *** autocmd  ***{{{2
autocmd InsertLeave * :echohl WarningMsg |echo "" | echohl None |let &showmode=s:ShowMode_UO
"  ****************}}}2

" *** TTrCodeAssistor libs *** {{{2
let g:TTrCodeAssistor_Libs{'for'} = "for( ".
										\g:TTrCodeAssistor_St."var_b".g:TTrCodeAssistor_En." = ".
										\g:TTrCodeAssistor_St."start".g:TTrCodeAssistor_En." ; ".
										\g:TTrCodeAssistor_St."var_c".g:TTrCodeAssistor_En." ".
										\g:TTrCodeAssistor_St."cmp_exp".g:TTrCodeAssistor_En." ; ".
										\g:TTrCodeAssistor_St."var_c".g:TTrCodeAssistor_En." += ".
										\g:TTrCodeAssistor_St."inc_r".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
										\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}".
										\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
										\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'while'} = "while ( ".
									\g:TTrCodeAssistor_St."expression".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
									\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}".
									\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
									\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'if'} = "if ( ".
								\g:TTrCodeAssistor_St."expression".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}".
								\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'ife'} = "if ( ".
								\g:TTrCodeAssistor_St."expression".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}\<CR>else\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>}".
								\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'switch'} = "switch ( ".
									\g:TTrCodeAssistor_St."expr".g:TTrCodeAssistor_En." )\<CR>{\<CR>".
									\repeat("case ".g:TTrCodeAssistor_St."case".g:TTrCodeAssistor_En." :\<CR>".g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.";\<CR>break;\<CR>",3).
									\"default :\<CR>".
									\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.";\<CR>break;\<CR>}".
									\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
									\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'df'} = "#ifndef  \<C-R>=GetFileNameForDefine()".repeat("\<CR>",2).
                                 \"#define  \<C-R>=GetFileNameForDefine()".repeat("\<cr>",6).
                                 \"#endif  /*\<C-R>=GetFileNameForDefine()\<CR>*/".repeat("\<up>",3)
let g:TTrCodeAssistor_Libs{'c'} = "/* ".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.
								\" */".
								\g:TTrCodeAssistor_St."clear".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'d'} = "#define  ".
								\g:TTrCodeAssistor_St."var_u".g:TTrCodeAssistor_En."    ".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'i'} = "#include  \"".
								\g:TTrCodeAssistor_St."filename".g:TTrCodeAssistor_En.
								\"\"".
								\g:TTrCodeAssistor_St."clear".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'is'} = "#include  <".
								\g:TTrCodeAssistor_St."filename".g:TTrCodeAssistor_En.
								\">".
								\g:TTrCodeAssistor_St."clear".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'dn'} = "#ifndef  ".
								\g:TTrCodeAssistor_St."var_u".g:TTrCodeAssistor_En."\<CR>#define  ".
								\g:TTrCodeAssistor_St."var_c".g:TTrCodeAssistor_En."\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En."\<CR>".
								\"#endif  /* ".
								\g:TTrCodeAssistor_St."var_c".g:TTrCodeAssistor_En." */".
								\g:TTrCodeAssistor_St."outside".g:TTrCodeAssistor_En.
								\g:TTrCodeAssistor_CallingMoveFirst
let g:TTrCodeAssistor_Libs{'main'} = "int main ( int argc , char ** argv )\<CR>{\<CR>".
								\g:TTrCodeAssistor_St."content".g:TTrCodeAssistor_En.";\<CR>\<CR>return 0;\<CR>}".
								\g:TTrCodeAssistor_CallingMoveFirst
" *************************************** }}}2

" *** ExpandTemplate Libs *** {{{2
" common templates
let g:TTrCodeAssistor_Templates{'_'}{'it'} = "\<c-r>=strftime(\"%Y-%m-%d %H:%M:%S\")\<cr>"
" C TTrCodeAssistor_Templates
let g:TTrCodeAssistor_Templates{'c'}{'c'} = g:TTrCodeAssistor_Libs{'c'}
let g:TTrCodeAssistor_Templates{'c'}{'d'} = g:TTrCodeAssistor_Libs{'d'}
let g:TTrCodeAssistor_Templates{'c'}{'i'} = g:TTrCodeAssistor_Libs{'i'}
let g:TTrCodeAssistor_Templates{'c'}{'is'} = g:TTrCodeAssistor_Libs{'is'}
let g:TTrCodeAssistor_Templates{'c'}{'df'} = g:TTrCodeAssistor_Libs{'df'}
let g:TTrCodeAssistor_Templates{'c'}{'fo'} = g:TTrCodeAssistor_Libs{'for'}
let g:TTrCodeAssistor_Templates{'c'}{'for'} = g:TTrCodeAssistor_Libs{'for'}
let g:TTrCodeAssistor_Templates{'c'}{'wh'} = g:TTrCodeAssistor_Libs{'while'}
let g:TTrCodeAssistor_Templates{'c'}{'while'} = g:TTrCodeAssistor_Libs{'while'}
let g:TTrCodeAssistor_Templates{'c'}{'if'} = g:TTrCodeAssistor_Libs{'if'}
let g:TTrCodeAssistor_Templates{'c'}{'sw'} = g:TTrCodeAssistor_Libs{'switch'}
let g:TTrCodeAssistor_Templates{'c'}{'switch'} = g:TTrCodeAssistor_Libs{'switch'}
let g:TTrCodeAssistor_Templates{'c'}{'ife'} = g:TTrCodeAssistor_Libs{'ife'}
let g:TTrCodeAssistor_Templates{'c'}{'dn'} = g:TTrCodeAssistor_Libs{'dn'}
let g:TTrCodeAssistor_Templates{'c'}{'main'} = g:TTrCodeAssistor_Libs{'main'}
let g:TTrCodeAssistor_Templates{'c'}{'comment_char'} = '\/\/'
" C++ TTrCodeAssistor_Templates
let g:TTrCodeAssistor_Templates{'cpp'}{'c'} = g:TTrCodeAssistor_Libs{'c'}
let g:TTrCodeAssistor_Templates{'cpp'}{'d'} = g:TTrCodeAssistor_Libs{'d'}
let g:TTrCodeAssistor_Templates{'cpp'}{'i'} = g:TTrCodeAssistor_Libs{'i'}
let g:TTrCodeAssistor_Templates{'cpp'}{'is'} = g:TTrCodeAssistor_Libs{'is'}
let g:TTrCodeAssistor_Templates{'cpp'}{'df'} = g:TTrCodeAssistor_Libs{'df'}
let g:TTrCodeAssistor_Templates{'cpp'}{'fo'} = g:TTrCodeAssistor_Libs{'for'}
let g:TTrCodeAssistor_Templates{'cpp'}{'for'} = g:TTrCodeAssistor_Libs{'for'}
let g:TTrCodeAssistor_Templates{'cpp'}{'wh'} = g:TTrCodeAssistor_Libs{'while'}
let g:TTrCodeAssistor_Templates{'cpp'}{'while'} = g:TTrCodeAssistor_Libs{'while'}
let g:TTrCodeAssistor_Templates{'cpp'}{'if'} = g:TTrCodeAssistor_Libs{'if'}
let g:TTrCodeAssistor_Templates{'cpp'}{'sw'} = g:TTrCodeAssistor_Libs{'switch'}
let g:TTrCodeAssistor_Templates{'cpp'}{'switch'} = g:TTrCodeAssistor_Libs{'switch'}
let g:TTrCodeAssistor_Templates{'cpp'}{'ife'} = g:TTrCodeAssistor_Libs{'ife'}
let g:TTrCodeAssistor_Templates{'cpp'}{'dn'} = g:TTrCodeAssistor_Libs{'dn'}
let g:TTrCodeAssistor_Templates{'cpp'}{'main'} = g:TTrCodeAssistor_Libs{'main'}
let g:TTrCodeAssistor_Templates{'cpp'}{'comment_char'} = '\/\/'
" Vim TTrCodeAssistor_Templates
let g:TTrCodeAssistor_Templates{'vim'}{'comment_char'} = '\"'
" Sh TTrCodeAssistor_Templates
let g:TTrCodeAssistor_Templates{'sh'}{'comment_char'} = '#'
" *************************** }}}2

" ====================== }}}1
" ==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==


" vim: set ft=vim ff=unix foldmethod=marker :

