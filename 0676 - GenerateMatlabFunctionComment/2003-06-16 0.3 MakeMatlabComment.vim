if exists("loaded_MakeMatlabComment")
   " load only once
    finish
endif
let loaded_MakeMatlabComment = 1

let g:MakeMatlabComment_paramTag=". "

function! MakeMatlabComment()
    mark d
    exec "normal o"
    let l:nextParamLine=line(".")
    exec "normal `d"
    let l:line=getline(line("."))
    let l:eqPos=match(l:line, "=")
    let l:startPos=match(l:line, "function")
    let l:matchIndex=match(l:line, "\\i\\+\s*", l:startPos+8)
    if (l:eqPos >= 0)
       exec l:nextParamLine
       exec "normal O% ^^^^^^^"
       exec "normal O% Output:"
       exec "normal O% "
       let l:nextParamLine=l:nextParamLine+3
       let l:foundParam=0
       while (l:matchIndex >= 0) 
          if (l:matchIndex >= l:eqPos)
             break
          endif
          let l:foundParam=1
          exec "normal `d"
          exec "normal " . (l:matchIndex + 1) . "|"
          let l:param=expand("<cword>")
          exec l:nextParamLine
          exec "normal O% " . g:MakeMatlabComment_paramTag . l:param
          let l:nextParamLine=l:nextParamLine+1

          exec "normal `d"
          "echo "l:startPos before: " . l:startPos
          "echo "l:matchIndex = " . l:matchIndex
          "echo "strlen=" . strlen(l:param)
          "echo "total=" . (l:matchIndex+strlen(l:param)+1)
          let l:startPos=(l:matchIndex+strlen(l:param)+1)
          let l:matchIndex=match(line,"\\i\\+\s*",l:startPos)
       endwhile
    endif
    
    exec l:nextParamLine
    exec "normal O%% AUTHOR  Author Name"
    let l:nextParamLine=l:nextParamLine+1
    
    exec "normal `d"
    exec "normal " . (l:matchIndex + 1) . "|"
    let l:fname=expand("<cword>")

    let l:startPos=match(l:line, "(")
    let l:matchIndex=match(l:line,"\\i\\+\s*[,)]",l:startPos)
    
    exec "normal o% "
    exec "normal o% Input:"
    exec "normal o% ^^^^^^"
    let l:nextParamLine=l:line+5
    
    let l:foundParam=0
    while (l:matchIndex >= 0)
        let l:foundParam=1
        exec "normal 'd" 
        exec "normal " . (l:matchIndex + 1) . "|"
        let l:param=expand("<cword>")
        exec l:nextParamLine
        exec "normal O% " . g:MakeMatlabComment_paramTag . l:param
        let l:nextParamLine=l:nextParamLine+1

        exec "normal `d"
        "echo "l:startPos before: " . l:startPos
        "echo "l:matchIndex = " . l:matchIndex
        "echo "strlen=" . strlen(l:param)
        "echo "total=" . (l:matchIndex+strlen(l:param)+1)
        let l:startPos=(l:matchIndex+strlen(l:param)+1)
        "echo "l:startPos after: " . l:startPos
        let l:matchIndex=match(line,"\\i\\+\s*[,)]",l:startPos)
    endwhile

    exec l:nextParamLine+1
    exec "normal O% " . l:fname . "  "
    exec "normal dd`dp"
    
    startinsert!
endfunction

