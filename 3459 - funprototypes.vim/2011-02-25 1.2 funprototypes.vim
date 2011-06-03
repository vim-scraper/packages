" File: funprototypes.vim
" Author: wuhong400@gmail.com
" Last Change:20 Feb 2011
" Version:1.2 
" Introdution:
"   This script is only for C language. You just press the key which you mapped
"   can automatic insert/replace the all funtions' prototypes before the first funtion.
" Preview:
"
"    #include<stdio.h>
"    +-----------------------------------------------------------+
"    |/******************************************************"   |  These three lines 
"    |**               funtion prototypes                 ***"   |  automatic add 
"    |*******************************************************/"  |  by funprototypes.vim!
"    |void fun_1();                                              |
"    +-----------------------------------------------------------+
"    
"    void
"    fun_1(){
"        printf("Hello, VIM!\n");
"    }
"    
"    int main()
"    {
"       fun_1();
"       return 0;
"    }
"
" Configure:
"   1. let g:fun_prototypes_static_first = 0
"
"      if g:fun_prototypes_static_first is 0, then the static functions will
"      insert before the global funtions. <default is 0>
"
"   2. let g:fun_prototypes_skip_fun_list=["main"]
"
"      This list contains the functions which you don't want funprototypes.vim to
"      add. 
"      eg:
"         int main(){}  //skip
"         int _main(){} //doesn't skip
"       
"   3. let g:fun_prototypes_static_fun_start_list=["static"]
"
"      This list contains the functions which are defined as static/LOCAL
"      functions.
"
"   4. let g:fun_prototypes_header = "/******************************************************"
"      let g:fun_prototypes_body   = "**               funtion prototypes                 ***"
"      let g:fun_prototypes_footer = "*******************************************************/"
"
"      These three lines will insert before first function define. If you only
"      want single line, should define like below:
"      eg:
"      let g:fun_prototypes_header = ""
"      let g:fun_prototypes_body   = "/***************funtion prototypes*******************/"
"      let g:fun_prototypes_footer = ""
"      Attention:If these three lines have already inserted in source code,
"      the functions' prototypes will insert after these three lines!
" Usage:
"    Copy this file to ~/.vim/plugin or to /vimfiles/plugins/ (on win32)
"    
"    Add the key map (default is <leader>fd) in your .vimrc or _vimrc (on win32)
"    eg:
"      nnoremap <silent> <leader>fd :call FunProto()<CR>
"
" Attention:
"   You'd better make sure that your code is correct, escepially the start token('{'),
"   close token('}'), and the '( )'. Otherwise, the script will add wrong
"   message in your code! If your code is right but the script insert wrong
"   prototypes, send email or add my gtalk/msn(see below). Thanks for your infomation!
" Contact:
"     Email: wuhong400@gmail.com
"     GTalk: wuhong400@gmail.com
"       MSN: wuhong40@tom.com
" Log:
" 1.2 fix bug:
"     when first function define at line 1, the script go into an infinite
"     loop.
"
" 1.1 fix bug:
"     1. The macro definitiona is in comment will generate bug.
"     like:
"     /*
"     #ifdef MAX 22
"     */
"     2. Above the function definition exist cpp style comment will generate
"     bug.
"     like:
"     //hello 
"     int foo(){}
"     
"     Then script will generate:
"     //hello int foo();
" 1.0 publish funprototypes.vim
              
if !exists("g:fun_prototypes_skip_fun_list")
    let g:fun_prototypes_skip_fun_list=["main"]
endif

if !exists("g:fun_prototypes_static_fun_start_list")
    let g:fun_prototypes_static_fun_start_list=["static"]
endif

if !exists("g:fun_prototypes_static_first")
    let g:fun_prototypes_static_first = 0
endif

if !exists("g:fun_prototypes_header")
    let g:fun_prototypes_header = "/******************************************************"
endif
if !exists("g:fun_prototypes_body")
    let g:fun_prototypes_body   = "**               funtion prototypes                 ***"
endif
if !exists("g:fun_prototypes_footer")
    let g:fun_prototypes_footer = "*******************************************************/"
endif

"default key map
nnoremap <silent> <leader>fd :call FunProto()<CR>

let s:static_fun_list=[]
let s:other_fun_list=[]
let s:function_cnt=0
let s:static_fun_cnt = 0
let s:other_fun_cnt = 0
let s:skip_fun_cnt = 0
let s:first_fun_line=0
let s:first_fun_head_end_line = 0
let s:dbg_str=""
let s:is_prototypes_head_exist = 0
let s:insert_line = 0

function! FunProto()
    let old_pos = [line("."), col(".")]
    
    call FunProtoInit() 
    call FunProtoGetList()
    call FunProtoSkipFuntion()
    call FunProtoRemoveOldprototypes_All()
    call FunProtoInsert()

    call cursor(old_pos)
    "echo s:dbg_str
     echo "Function prototypes Total:".s:function_cnt."(static fun:".s:static_fun_cnt." global fun:".s:other_fun_cnt."), skip ".s:skip_fun_cnt." fun"
endfunction

function! FunProtoInsert()
    call FunProtoGetInsertLine()

    if g:fun_prototypes_static_first == 1
        for item in s:other_fun_list
            call append(s:insert_line,item) 
        endfor

        for item in s:static_fun_list
            call append(s:insert_line,item) 
        endfor
    else
        for item in s:static_fun_list
            call append(s:insert_line,item) 
        endfor

        for item in s:other_fun_list
            call append(s:insert_line,item) 
        endfor
    endif


    if s:is_prototypes_head_exist == 0
        if g:fun_prototypes_footer != ""
            call append(s:insert_line, g:fun_prototypes_footer)
        endif
        if g:fun_prototypes_body != ""
            call append(s:insert_line, g:fun_prototypes_body)
        endif
        if g:fun_prototypes_header != ""
            call append(s:insert_line, g:fun_prototypes_header)
        endif
    endif
endfunction

function! FunProtoInit()
    "clear the list 
    let s:static_fun_list=[]
    let s:other_fun_list=[]

    let s:function_cnt = 0
    let s:static_fun_cnt = 0
    let s:other_fun_cnt = 0
    let s:skip_fun_cnt = 0

    let s:first_fun_line=0
    let s:is_prototypes_head_exist = 0

    let s:dbg_str = ""
    "get the last line number 
    normal!  G
    let s:last_line=line(".")
endfunction

function! FunProtoGetList()
    "jump to the line 1,  and search start from line 1
    normal! gg

    let fun_start_line=0
    let fun_end_line=0
    let pre_fun_end_pos=[0, 0]

    let curr_line = 1
    let is_first_fun = 1
    let fun_body_start_pos = []

    while 1
        let fun_body_start_pos=searchpos("{", "W") 
        if fun_body_start_pos[0] != 0 && fun_body_start_pos[1] != 0
            if 0 == FunProtoIsInComment(fun_body_start_pos)
                "this '{' is likely a function start token, get the other info.
                let fun_head_start_line = fun_body_start_pos[0]
                let fun_head = ""

                "let s:dbg_str= s:dbg_str.",".fun_body_start_pos[0]

                while fun_head_start_line > 0
                    let line_content = getline(fun_head_start_line)
                    let long_line_cnt = 1
                    while fun_head_start_line -1 > 0 
                        let line_up_content = getline(fun_head_start_line - 1)
                        "code example:
                        "   #define AAA \
                        "           aaa
                        "so we need get the pre line, join these two lines
                        if line_up_content =~ ".*\\"
                            let line_content = substitute(line_up_content, "\\", " ", "g")." ". line_content
                            let fun_head_start_line = fun_head_start_line - 1
                            let long_line_cnt = long_line_cnt + 1
                        else
                            break
                        endif
                    endwhile

                    "the line contain the token which means the function is end
                    " ; } #
                    if line_content =~ ".*[;}#].*"
                        "code: fun_1();}/* ; */ int /* } */ fun_2(){ int i;
                        "may be some funtion info after the ';'&'}'
                        let i=0
                        let funhead_end_col = -1
                        let b_is_funhead_end = 0
                        let b_is_macro_define = 0
                        let line_len = strlen(line_content)
                        while i < line_len
                            let i_pos=[fun_head_start_line, i+1]

                            "we search over the '{'
                            if FunProtoComparePos(i_pos, fun_body_start_pos) >=0
                                break
                            endif

                            if line_content[i] == ";" || line_content[i] == "}" || line_content[i] == "#"
                                if FunProtoIsInComment(i_pos) == 0
                                    if FunProtoComparePos(i_pos, fun_body_start_pos) < 0
                                        let funhead_end_col = i
                                        let b_is_funhead_end = 1

                                        "it's a macro define 
                                        if line_content[i] == "#"
                                            let b_is_macro_define = 1
                                            break
                                        endif
                                    endif
                                endif
                            endi
                            let i = i + 1
                        endwhile
                        
                        
                        let temp_info = strpart(line_content, funhead_end_col+1) 
                        let temp_info = FunProtoRemoveComment(temp_info)
                        let temp_info = FunProtoTrim(temp_info)
                        if b_is_funhead_end == 1
                            "There is no info in this line and search
                            "finished or this line is macro define, the pointer go back
                            if b_is_macro_define == 1 || temp_info == ""
                                let fun_head_start_line = fun_head_start_line + long_line_cnt
                            else "there is some info after ; } 
                                let fun_head =temp_info."\n". fun_head
                                let fun_head_start_line = fun_head_start_line - long_line_cnt + 1
                            endif
                            break
                        else
                            let fun_head =temp_info."\n". fun_head
                            let fun_head_start_line = fun_head_start_line - long_line_cnt
                        endif
                    else
                        "remove the comment 
                        let line_content = FunProtoRemoveComment(line_content)
                        "not over , search go on
                        let fun_head = line_content . "\n" . fun_head
                        let fun_head_start_line = fun_head_start_line - long_line_cnt
                    endif
                endwhile

                "Format the fun_head, is it a typedef, struct or function?
                let fun_head = FunProtoFormat(fun_head)
                if fun_head =~ ".*(.*)"
                    if FunProtoIsStaticFun(fun_head) == 1
                        let s:static_fun_cnt = s:static_fun_cnt + 1
                        call insert(s:static_fun_list, fun_head)
                    else
                        let s:other_fun_cnt = s:other_fun_cnt + 1
                        call insert(s:other_fun_list, fun_head)
                    endif

                    " remember the first function position, we will insert the prototypes
                    " before that position
                    if 1 == is_first_fun 
                        let s:first_fun_line = fun_head_start_line
                        let s:first_fun_head_end_line = fun_body_start_pos[0]
                        let is_first_fun = 0
                    endif
                    "let s:dbg_str = s:dbg_str."".fun_head_start_line

                    let s:function_cnt=s:function_cnt+1
                endif

                "move cursor  to the function end by % --> }
                normal! %
                let pre_fun_end_pos = [line("."), col(".")]
            endif
        else 
            "no {, it's over
            break
        endif
    endwhile
endfunction

function! FunProtoRemoveOldprototypes_All()
    for item in s:other_fun_list
        call FunProtoRemoveOldprototypes(item)
    endfor

    for item in s:static_fun_list
        call FunProtoRemoveOldprototypes(item)
    endfor
endfunction

"remove the funtions which are in the skip function list
function! FunProtoSkipFuntion()
    let temp_list=[]
    for item in s:other_fun_list
        "check the function name is in skip_funtion_list
        if FunProtoIsSkipFun(item) == 1
            let s:skip_fun_cnt = s:skip_fun_cnt + 1
        else
            call add(temp_list, item)
        endif
    endfor
    let s:other_fun_list=temp_list

    let temp_list=[]

    for item in s:static_fun_list
        "check the function name is in skip_funtion_list
        if FunProtoIsSkipFun(item) == 1
            let s:skip_fun_cnt = s:skip_fun_cnt + 1
        else
            call add(temp_list, item)
        endif
    endfor

    let s:static_fun_list=temp_list
endfunction

function! FunProtoRemovePrototypes()
    let curr_line = 1
    while curr_line <= s:first_fun_line
        let proto_start_line = curr_line 
        let proto_end_line = curr_line 
        let is_proto = 1
        let curr_line_content = getline(curr_line)
        "typedef #define array{} struct{} comment // /* */

        let curr_line = proto_end_line
    endwhile
endfunction

function! FunProtoRemoveOldprototypes(prototypes_str)
    "get the function name match(str, "[A-Za-z_][A-Za-z0-9_]*[\s\t ]*(")
    let fun_name_start_pos=match(a:prototypes_str, "[A-Za-z_][A-Za-z0-9_]*[\t ]*(")
    if fun_name_start_pos != -1
        let fun_name = strpart(a:prototypes_str, fun_name_start_pos)
        let fun_name_end_pos = match(fun_name, "(")
        let fun_name = strpart(fun_name, 0, fun_name_end_pos)

        "search the function prototypes, check is it already existed?
        let line_i = 1
        while line_i <= s:first_fun_line
            let line_content = getline(line_i)
            if line_content =~ "[^A-Za-z0-9_]".fun_name."[\t ]*(.*)[\t ]*;"
                "is in comment?
                let temp_col = match(line_content, "(")
                if 0 == FunProtoIsInComment([line_i, temp_col])
                    exe "normal! ".line_i."Gdd"
                    let s:first_fun_line = s:first_fun_line - 1
                    break
                endif
            endif
            let line_i = line_i + 1
        endwhile
    endif
endfunction

"get the last static function prototypes , we will only search the prototypes
"between line 1 and the line which the first function define 
function! FunProtoGetInsertLine()
    if s:first_fun_line == 0
        let s:insert_line = 0
    else
        let s:insert_line = s:first_fun_line - 1
    endif
    "search insert line , if exist the function prototypes header just like 
    "/********************
    "** function prototypes 
    "*******************/
    "then insert here! but should make sure that this three lines must be the
    "same , other wise will be 3 lines more!
    "we search it from line 1, and end in the first function start line.
    "if there is no function prototypes, the script will insert it before the
    "first function 

    let curr_line = 1
    let prototypes_line_cnt = 3

    let header_idx = 0
    let body_idx = 1
    let footer_idx = 2
    
    if g:fun_prototypes_header == ""
        let prototypes_line_cnt = prototypes_line_cnt - 1
        let body_idx = body_idx - 1
        let footer_idx = footer_idx - 1 
    endif
    if g:fun_prototypes_body == ""
        let prototypes_line_cnt = prototypes_line_cnt - 1
        let footer_idx = footer_idx - 1
    endif
    if g:fun_prototypes_footer == ""
        let prototypes_line_cnt = prototypes_line_cnt - 1
    endif

    "let s:dbg_str=s:dbg_str."cnt:".prototypes_line_cnt
    while prototypes_line_cnt > 0 && curr_line <= s:first_fun_head_end_line + prototypes_line_cnt 
        let exist_prototypes = 1

        if g:fun_prototypes_header != "" 
            if getline(curr_line + header_idx) != g:fun_prototypes_header
                "let s:dbg_str=s:dbg_str."+"
                let exist_prototypes = 0
                let curr_line = curr_line + 1
                continue
            endif
        endif

        if g:fun_prototypes_body != ""
            if getline(curr_line + body_idx ) != g:fun_prototypes_body
                let exist_prototypes = 0
                let curr_line = curr_line + 1
                continue
            endif
        endif

        if g:fun_prototypes_footer != ""
            if getline(curr_line + footer_idx ) != g:fun_prototypes_footer
                let exist_prototypes = 0
                let curr_line = curr_line + 1
                continue
            endif
        endif

        "exist prototypes header , then let insert line number equal to this
        "prototypes header
        if exist_prototypes == 1
            let s:is_prototypes_head_exist = 1
            let s:insert_line = curr_line + prototypes_line_cnt - 1 
            break
        endif
        let curr_line = curr_line + 1
    endwhile
endfunction

function! FunProtoTrim(str)
    let ret_str = a:str
    "trim
    let ret_str=substitute(ret_str, "^[\t ]*", "", "g")
    let ret_str=substitute(ret_str, "[\t ]*$", "", "g")

    return ret_str
endfunction

function! FunProtoFormat(prototypes_name)
    let l:prototypes_name = a:prototypes_name
    let l:prototypes_name=FunProtoRemoveCPPComment(l:prototypes_name)
    "remove /*.. */
    let l:prototypes_name=FunProtoRemoveCComment(l:prototypes_name)
    "join the multi line to single line 
    let l:prototypes_name=substitute(l:prototypes_name, "\n", " ", "g")
    "let multi blank to single blank
    let l:prototypes_name=substitute(l:prototypes_name, "[\t ][\t ]*", " ", "g")

    "remove the string after '{'
    let l:prototypes_name=substitute(l:prototypes_name,'{.*',"","g")

    let l:prototypes_name =substitute(l:prototypes_name, "^[\t ]*", "", "g")
    let l:prototypes_name=substitute(l:prototypes_name, "[\t ]*$", "", "g") 
    "=FunProtoTrim(l:prototypes_name)
    let l:prototypes_name=l:prototypes_name.";"

    return l:prototypes_name
endfunction

function! FunProtoRemoveComment(str)
    let str_ret = FunProtoRemoveCComment(a:str)
    let str_ret = FunProtoRemoveCPPComment(str_ret)

    return str_ret
endfunction

" why substitute(str, "\/\/.*\n", "\n", "g") didn't work?
" This function remove the str's cpp style comment(//......)
function! FunProtoRemoveCPPComment(str)
    let l:str_ret = a:str 

    let l:start= match(l:str_ret, "\/\/")
    if l:start == -1 
        return l:str_ret
    else
        return strpart(l:str_ret, 0, l:start)
    endif
endfunction

"remove the c style comment, /*....*/
function! FunProtoRemoveCComment(str)
    "return a:str
    let l:str_ret=a:str

    while 1
        let l:start=FunProtoMatch2Char(l:str_ret, "\/", "\*")
        let l:end=FunProtoMatch2Char(l:str_ret, "\*", "\/")
        if l:start == -1 || l:end == -1 || l:start >= l:end
            break
        endif
        let l:str_ret=strpart(l:str_ret, 0, l:start)."".strpart(l:str_ret, l:end +2, strlen(l:str_ret)-l:end-2)
    endwhile

    return l:str_ret
endfunction

" this function is used to match "/*" & "*/" .
" I use match("\/") and match("\*"), both work, but when I use them togther,
" it didn't work! (match ("\/\*"), it always return 0! 
function! FunProtoMatch2Char(str, ch1, ch2)
    let l:i=0
    let l:len= strlen(a:str)

    while l:i < l:len-1
        if a:str[l:i] =~ a:ch1 && a:str[l:i+1] =~ a:ch2
            return l:i
        endif
        let l:i=l:i+1
    endwhile

    return -1
endfunction

function! FunProtoIsInComment(pos)
    let ret = 0
    "is in cpp style comment // ...{...?
    let line_content=getline(".")
    let comment_start =  match(line_content, "\/\/")
    if comment_start != -1 && comment_start <  a:pos[1]
        return 1
    endif

    let curr_pos = [line("."), col(".")]
    call cursor(a:pos)
    
    "is in c style comment /* ...}... */
    "get the /* pos
    normal! [*
    let c_comment_pos_start=[line("."), col(".")]
    if FunProtoComparePos(a:pos, c_comment_pos_start) != 0
        "get the */ pos
        normal! ]*
        let c_comment_pos_end = [line("."), col(".")]

        "judge the char is in the pos between start and end 
        if FunProtoComparePos(c_comment_pos_start, a:pos) == -1 && FunProtoComparePos(c_comment_pos_end, a:pos) == 1
            let ret = 1
        endif
    endif

    call cursor(curr_pos)
    if ret == 1
        return ret
    endif

    "is in #if 0 #endif?
    "get the #if pos
    normal! [#
    let c_comment_pos_start=[line("."), col(".")]
    "the cursor postion is not in macro define
    if FunProtoComparePos(c_comment_pos_start, curr_pos) == 0
        return 0
    endif

    let macro_content = getline(".")
    if macro_content =~ "\#[\t ]*if[\t ]0.*"
        if FunProtoComparePos(a:pos, c_comment_pos_start) != 0
            "get the */ pos
            normal! ]#
            let c_comment_pos_end = [line("."), col(".")]

            "judge the char is in the pos between start and end 
            if FunProtoComparePos(c_comment_pos_start, a:pos) == -1 && FunProtoComparePos(c_comment_pos_end, a:pos) == 1
                let ret = 1
            endif
        endif
    endif

    call cursor(curr_pos)
    return ret
endfunction

" @brief compare  postion 1 to postion 2
"
" @param pos1 : postion 1 [line, col]
" @param pos2
"
" @return 1:pos1 > pos2 
"         0:pos1 = pos2
"        -1:pos1 < pos2
function! FunProtoComparePos(pos1, pos2)
    if a:pos1[0] > a:pos2[0]
        return 1
    endif

    if a:pos1[0] < a:pos2[0] 
        return -1
    endif

    if a:pos1[1] > a:pos2[1]
        return 1
    endif

    if a:pos1[1] < a:pos2[1]
        return -1
    endif

    return 0
endfunction

function! FunProtoIsStaticFun(str)
    for item in g:fun_prototypes_static_fun_start_list
        if a:str =~ item.".*"
            return 1
        endif
    endfor
    return 0
endfunction

function! FunProtoIsSkipFun(str)
    for item in g:fun_prototypes_skip_fun_list
        if a:str =~ ".*[^A-Za-z0-9_]".item."[ \t]*(.*).*"
            return 1
        endif
    endfor
    return 0
endfunction
