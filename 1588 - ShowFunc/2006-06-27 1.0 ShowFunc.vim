" -------------------------------------------------------------------
"  ShowFunc.vim -- Show or jump to current type/function name "{{{
"
"  Author:   Yanbiao Zhao (yanbiao_zhao at yahoo.com)
"  Requires: Vim 7
"  Version:  1.0
"
"  Command:
"      SFunc -- Show Function/Type name 
"      JFunc -- Jump to function/type 
"
"  }}}

"check vim version
if v:version < 700
	echomsg "Vim 7 or higher is required for ShowFunc.vim"
	finish
endif

"commands
command! SFunc :call <SID>ShowClassAndFunctionName()
command! JFunc :call <SID>JumpToFunctionOrClass()

"short cuts
nmap <silent> ,f :call <SID>ShowClassAndFunctionName()<CR>
nmap <silent> \f :call <SID>JumpToFunctionOrClass()<CR>

"global settings
let s:SF_SupportedFileType = ["cs", "java", "cpp"]
let s:SF_TypeDeclare  = ["csStorage",   "javaClassDecl", "cppStructure"]
let s:SF_ScopeDeclare = ["csModifier" , "javaScopeDecl", "cppAccess"]

function! <SID>IsSupportedFileType()
    if index(s:SF_SupportedFileType, &filetype) >= 0
        return 1
    else
        return 0
    endif 
endfunction

function! <SID>ShowClassAndFunctionName()
    if !<SID>IsSupportedFileType()
        return
    endif

    "save current position
    let save_view = winsaveview()

    let funcName = ""
    let typeName = ""

    let result = <SID>JumpToFunctionOrClass() 
    if result == 0
        return
    elseif result == 1
        let typeName = expand("<cword>")
    else
        let funcName = expand("<cword>")

        if result == 3 
            let funcName = "get_".funcName
        elseif result == 4
            let funcName = "set_".funcName
        elseif result == 5
            let funcName = "add_".funcName
        elseif result == 6
            let funcName = "remove_".funcName
        endif

        "Get its type name
        normal! [{
        while 1
            normal! b
            if index(s:SF_TypeDeclare, synIDattr(synID(line("."),col("."),1), "name") ) < 0
                continue
            endif

            normal! w
            let typeName = expand("<cword>") 
            break
        endwhile
    endif

    "restore cursor position
    call winrestview(save_view)

    "display type/func name
    redraw

    if funcName == ""
        echo "Type: ".typeName
    else
        echo "Function: ".typeName.".".funcName."()" 
    endif
endfunction

function! <SID>CheckPositionChange(cmd)
    let lineno = line(".")
    let column = col(".")

    exec a:cmd

    if lineno != line(".") || column != col(".")
        return 1
    else
        return 0
    endif
endfunction

" return values
"   0 -- falied
"   1 -- class
"   2 -- function
"   3 -- getter
"   4 -- setter
function! <SID>JumpToFunctionOrClass()
    if !<SID>IsSupportedFileType()
        return
    endif

    let getSyntaxId = 0

    if expand("<cword>") != '{'
        if <SID>CheckPositionChange("normal! [{") == 0
            return 0
        endif
    endif

    "find class and function name
    while 1 
        let stopline = line(".")
        let stopcol = col(".")
        if search("\[{};]", "bW") == 0
            normal! gg
        endif
        
        normal! w
        while line(".") < stopline || (line(".") == stopline && col(".") < stopcol)
            let idAttr = synIDattr(synID(line("."),col("."),1), "name")
            
            if idAttr != "" && idAttr != "csComment" && idAttr != "xmlTag" && idAttr != "xmlEndTag" && stridx(idAttr, "csXml") != 0
                let getSyntaxId = 1

                "We may not in a function at all
                if index(s:SF_TypeDeclare, idAttr) >= 0
                    normal! w
                    return 1
                endif

                "get a function 
                if index(s:SF_ScopeDeclare,idAttr) >= 0 && search("(", "W", stopline) > 0
                    normal! b

                    if strpart(getline('.'), col('.')-1, 1) == '>' 
                        call search('<', 'bW')
                        normal! b
                    endif
                    return 2 
                endif

                "it maybe a property
                let cword = expand('<cword>')
                if idAttr == 'csContextualStatement' && (cword == 'get' || cword == 'set')
                    normal! [{
                    normal! b

                    "it maybe a indexer
                    if expand('<cword>') == ']'
                        normal! %
                        normal! b
                    endif
                    if cword == 'get'
                        return 3
                    else
                        return 4
                    endif
                endif

                "it maybe an event
                if idAttr == 'csUnsupportedStatement' && (cword == 'add' || cword == 'remove')
                    normal! [{
                    normal! b

                    if cword == 'add'
                        return 5
                    else
                        return 6
                    endif
                endif
            endif

            normal! w
        endwhile

        "some function may not have a modifier like public/private. try to
        "detect here
        if !getSyntaxId
            normal! b
            if expand('<cword>') == ')'
                normal! %
                normal! b

                "to avoid for() statement because it can include ';' in paren
                if synIDattr(synID(line("."),col("."),1), "name") == ""
                    return 2
                endif
            endif
        endif

        "jump to outer scope
        if <SID>CheckPositionChange("normal! [{") == 0
            return 0
        endif
    endwhile
endfunction


