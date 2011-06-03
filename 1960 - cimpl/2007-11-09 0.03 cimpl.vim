" cimpl.vim
" @Author:      Alexander Rodin <rodin.alexander@gmail.com>
" @License:     GPL
" @Created:     2007-07-16
" @Last change: 2007-07-17
"
" This script can generate empty emplementations for specified source file.
" To install the script, copy it script to ~/.vim/plugins. You must have
" installed exuberant-ctags.
" You can call GenImpl function to tSkeleton skeletion.
" Example:
" <+call:eval('findfile(expand("%:r:t").".h", ".;") != "" ? "\n#include \"".findfile(expand("%:r:t").".h", ".;")."\"\n" : ""')+><+call:eval('findfile(expand("%:r:t").".h", ".;") != "" ? "<+CURSOR+>".GenImpl(findfile(expand("%:r:t").".h", ".;")) : "\n<+CURSOR+>\n"')+>
" This string will insert #include directive and function implementations in
" source file if header file exists.

function! ToUnifiedSignature(signature)
    let result = a:signature
    let result = substitute(result, "\\s*\(\\s*", " ( ", "g")
    let result = substitute(result, "\\s*\)\\s*", " ) ", "g")
    let result = substitute(result, "\\s*\&\\s*", " _ ", "g")
    let result = substitute(result, "\\s*\\*\\s*", " * ", "g")
    let result = substitute(result, "\\s\\s", " ", "g")
    return result
endfunction

function! BracePairsBefore(str, pos)
    let result = 0
    for i in [ 0, 1, 2 ]
        if a:str[i] == "("
            let pairs = pairs + 1
        elseif a:str[i] == ")"
            let pairs = pairs - 1
        endif
    endfor
    return result
endfunction

function! GenImpl(header)
    let ctags_output = split(system('exuberant-ctags -R --c++-types=+px --excmd=pattern --sort=no -f - "'.a:header.'"'), "\n")
    let result = ""
    let processed_defs = []
    for i in ctags_output
        let d_type = matchstr(substitute(i, "^.*\$\/;\"", "", ""), "\\a")
        if d_type == "p"
            let def = substitute(substitute(i, "^.*\/\^\\s*", "", ""), "\$\/\;\".*$", "", "")
            let class = substitute(matchstr(i, "class:\\s*.*$"), "class:\\s*", "", "")

            let def = substitute(def, "\".*\"", "", "g")
            let def = substitute(def, "\'.*\'", "", "g")
            let def = substitute(def, "\\\\/\\*.*\\*\\\\/", "", "g")
            let def = substitute(def, "\\\\/\\\\/.*$", "", "g")
            
			if matchstr(def, "virtual") == "virtual"
                if match(def, "\\s*=\\s*0\\s*;\\s*$") != -1
                    continue
                endif
			endif

            let def = substitute(def, "\\s*explicit\\s*", "", "")
            let def = substitute(def, "\\s*virtual\\s*", "", "")
            let def = substitute(def, "\\s*inline\\s*", "", "")
            let def = substitute(def, "\\s*static\\s*", "", "")
            let def = substitute(def, ";\\s*$", "", "")
            if matchstr(def, "friend") == "friend"
                let def = substitute(def, "\\s*friend\\s*", "", "")
                let class = ""
            endif
            
            if (matchstr(i, "^[^\t]*") == "operator =")
                let startpos = match(def, "\\s*=") + strlen(matchstr(def, "\\s*="))
            else
            let startpos = 0
            endif
            let pos = startpos + match(strpart(def, startpos), "\\s*=")
            while pos != startpos - 1
                let brace_count = 0
                while 1
                    let def = strpart(def, 0, pos).strpart(def, pos + 1)
                    if def[pos] == "("
                        let brace_count = brace_count + 1
                    elseif def[pos] == ")"
                        let brace_count = brace_count - 1
                        if brace_count < 0
                            break
                        endif
                    elseif def[pos] == "," && brace_count == 0
                        break
                    elseif pos >= strlen(def)
                        break
                    endif
                endwhile
                let pos = startpos + match(strpart(def, startpos), "\\s*=")
            endwhile

            if class != ""
                let func_name = escape(matchstr(i, "^[^\t]*"), "~")
                let type = substitute(def, "\\s*".func_name.".*$", "", "")
                if type != ""
                    let def = type." ".class."::".substitute(def, type."\\s*", "", "")
                else
                    let def = class."::".def
                endif
            endif
            if index(processed_defs, ToUnifiedSignature(def)) == -1
                let result .= "\n".def."\n{\n    \n}\n"
                let processed_defs = processed_defs + [ToUnifiedSignature(def)]
            endif
        endif
    endfor
    return result
endfunction

function! InsertImpl(header)
    exec "normal i".GenImpl(header)."\<esc>"
endfunction

" echo GenImpl("/tmp/f/test.h")

