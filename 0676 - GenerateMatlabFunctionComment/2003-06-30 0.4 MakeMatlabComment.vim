if exists("loaded_MakeMatlabComment")
   " load only once
   finish
endif
let loaded_MakeMatlabComment = 1


let g:MakeMatlabComment_paramTag=". "

function! MakeMatlabComment()
   mark d
   let l:lineStr=getline(line("."))

   let l:nextLine=line(".")+1

   let l:eqPos=match(l:lineStr, "=")
   let l:startPos=match(l:lineStr, "function")
   if (l:startPos < 0)
      return -1
   endif

   let l:funNamePos = l:eqPos
   if (l:funNamePos <= 0)
      let l:funNamePos = l:startPos+9
   endif

   " Function name
   let l:matchIndex=match(l:lineStr, "\\i\\+\s*", l:funNamePos)
   normal 'd
   exec "normal " . (l:matchIndex + 1) . "|"
   let l:funName = expand("<cword>")

   call append(l:nextLine-1, "\% " . l:funName . "  ")
   let l:nextLine=l:nextLine+1

   " Empty lineStr
   call append(l:nextLine-1, "\%")
   let l:nextLine=l:nextLine+1

   " Input parameters
   let l:parenPos=match(l:lineStr, "(")
   if (l:parenPos >= 0)
      call append(l:nextLine-1, "\% Input:")
      let l:nextLine=l:nextLine+1
      call append(l:nextLine-1, "\% ^^^^^^")
      let l:nextLine=l:nextLine+1

      let l:matchIndex=match(l:lineStr, "\\i\\+\s*", l:parenPos)
      while (l:matchIndex >= parenPos)
         normal 'd
         exec "normal " . (l:matchIndex + 1) . "|"
         let l:param = expand("<cword>")
         call append(l:nextLine-1, "\% " . g:MakeMatlabComment_paramTag . l:param)
         let l:nextLine=l:nextLine+1

         let l:paramPos=(l:matchIndex+strlen(l:param)+1)
         let l:matchIndex=match(lineStr,"\\i\\+\s*",l:paramPos)
      endwhile

      " Empty lineStr
      call append(l:nextLine-1, "\%")
      let l:nextLine = l:nextLine+1
   endif

   " Output parameters
   if (l:eqPos >= 0)
      call append(l:nextLine-1, "\% Output:")
      let l:nextLine=l:nextLine+1
      call append(l:nextLine-1, "\% ^^^^^^^")
      let l:nextLine=l:nextLine+1

      let l:oParamPos = l:startPos + 8
      let l:matchIndex = match(l:lineStr, "\\i\\+\s*", l:oParamPos)

      while (l:matchIndex >= 0) 
         if (l:matchIndex >= l:eqPos)
            break
         endif

         normal 'd
         exec "normal " . (l:matchIndex + 1) . "|"
         let l:param=expand("<cword>")

         call append(l:nextLine-1, "\% " . g:MakeMatlabComment_paramTag . l:param)
         let l:nextLine=l:nextLine+1

         let l:oParamPos=(l:matchIndex+strlen(l:param)+1)
         let l:matchIndex=match(lineStr,"\\i\\+\s*",l:oParamPos)
      endwhile

      " Empty lineStr
      call append(l:nextLine-1, "\%")
      let l:nextLine=l:nextLine+1
   endif

   " See also lineStr
   call append(l:nextLine-1, "\% See also:")
   let l:nextLine=l:nextLine+1
   call append(l:nextLine-1, "\% ^^^^^^^^^")
   let l:nextLine=l:nextLine+1

   " Empty lineStr
   call append(l:nextLine-1, "\%")
   let l:nextLine=l:nextLine+1

   " Author lineStr
   call append(l:nextLine-1, "\%% Author: Vinay Middha")
   let l:nextLine=l:nextLine+1

   " Set cursor position after function name
   normal 'dj$
   " insert mode
   startinsert!
endfunction


