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

function! GenImpl(header)
    let ctags_output = split(system('exuberant-ctags -R --c++-types=+px --excmd=pattern --sort=no -f - "'.a:header.'"'), "\n")
    let result = ""
    for i in ctags_output
        let d_type = matchstr(substitute(i, "^.*\$\/;\"", "", ""), "\\a")
        if d_type == "p"
            let def = substitute(substitute(i, "^.*\/\^\\s*", "", ""), "\$\/\;\".*$", "", "")
            let class = substitute(matchstr(i, "class:\\s*.*$"), "class:\\s*", "", "")

            let def = substitute(def, "\".*\"", "", "")
            let def = substitute(def, "\'.*\'", "", "")
            let def = substitute(def, "\\\\/\\*.*\\*\\\\/", "", "")
            let def = substitute(def, "\\\\/\\\\/.*$", "", "")

            let def = substitute(def, "\\s*explicit\\s*", "", "")
            let def = substitute(def, "\\s*virtual\\s*", "", "")
            let def = substitute(def, "\\s*inline\\s*", "", "")
            let def = substitute(def, "\\s*static\\s*", "", "")
            let def = substitute(def, ";\\s*$", "", "")

            let pos = match(def, "\\s*=")
            while pos != -1
                let brace_count = 0
                while pos != -1
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
                let pos = match(def, "\\s*=")
            endwhile

            if class != ""
                let func_name = escape(matchstr(i, "^\\S*"), "~")
                let type = substitute(def, "\\s*".func_name.".*$", "", "")
                if type != ""
                    let def = type." ".class."::".substitute(def, type."\\s*", "", "")
                else
                    let def = class."::".def
                endif
            endif
            let result .= "\n".def."\n{\n    \n}\n"
        endif
    endfor
    return result
endfunction

function! InsertImpl(header)
    exec "normal i".GenImpl(header)."\<esc>"
endfunction

"echo GenImpl("/tmp/f/test.h")

