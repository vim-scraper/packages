if exists("loaded_MakeMatlabComment")
    finish
endif
let loaded_MakeMatlabComment = 1

if !exists("g:MakeMatlabComment_paramTag")
    let g:MakeMatlabComment_paramTag="PARAM "
endif

function! MakeMatlabComment()
    mark d
    exec "normal yypI% "
    exec "normal o"
    let l:synopsisLine=line(".")
    let l:synopsisCol=col(".")
    let l:nextParamLine=l:synopsisLine
    exec "normal `d"
    let l:line=getline(line("."))
    let l:eqPos=match(l:line, "=")
    if (l:eqPos >= 0)
       let l:startPos=match(l:line, "function")
       let l:matchIndex=match(l:line, "\\i\\+\s*", l:startPos+8)
       let l:foundParam=0
       while (l:matchIndex >= 0) 
          if (l:matchIndex >= l:eqPos)
             break
          endif
          let l:foundParam=1
          exec "normal " . (l:matchIndex + 1) . "|"
          let l:param=expand("<cword>")
          exec l:nextParamLine
          exec "normal O% " . g:MakeMatlabComment_paramTag . l:param . " [OUT] "
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
    
    exec "normal " . (l:matchIndex + 1) . "|"
    let l:fname=expand("<cword>")

    exec "normal `d"
    
    let l:startPos=match(l:line, "(")
    let l:matchIndex=match(l:line,"\\i\\+\s*[,)]",l:startPos)
    let l:foundParam=0
    while (l:matchIndex >= 0)
        let l:foundParam=1
        exec "normal " . (l:matchIndex + 1) . "|"
        let l:param=expand("<cword>")
        exec l:nextParamLine
        exec "normal O% " . g:MakeMatlabComment_paramTag . l:param . " [IN] "
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

    exec l:nextParamLine
    exec "normal O% AUTHOR  Author Name"
    
    exec l:nextParamLine+1
    exec "normal O% " . l:fname . "  "
    exec "normal dd`dp"
    
    startinsert!
endfunction

