"""""""""""""""""""""""""""""""" Reference to Author
" GetLatestVimScripts: 2028 1 :AUTOINSTALL: verticalCopyandPaster

" MarkLines:  Allows you to select frequently used files across directory.
" Author:     Vijayandra Singh (vsingh@vjrc.com)
" Date:       Oct,10 2008
" Version:    1.0
" License:  

" Copyright (c) 2008, Vijayandra Singh
" All rights reserved.
" 
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions are met:
"     * Redistributions of source code must retain the above copyright
"       notice, this list of conditions and the following disclaimer.
"     * Redistributions in binary form must reproduce the above copyright
"       notice, this list of conditions and the following disclaimer in the
"       documentation and/or other materials provided with the distribution.
"     * The names of the contributors may not be used to endorse or promote
"       products derived from this software without specific prior written
"       permission.
" 
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY
" EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
" DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
" DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
" (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
" LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
" ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
" (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
" SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Abort if running in vi-compatible mode or the user doesn't want us.    {{{1

if exists("loaded_verticalCopyandPaste") || &cp
    finish
endif
set guioptions+=v
let loaded_verticalCopyandPaste=1
let g:marker=0

silent nnoremap <silent> <c-c> :call StartMarkForRectangleCopy()<CR>
silent nnoremap <silent> <c-y> :call PasteMarkForRectangleCopy()<CR>

fun! StartMarkForRectangleCopy()

    if g:marker==0 || g:marker==2
        let g:startl=line('.')
        let g:startc=col('.')-1
        "echo "Pressed ctrl-c now move curser and press ctrl-c for rectangle copy"
        echo "Pressed ctrl-c now move curser and press ctrl-c for rectangle copy position marked [" .  g:startl . " " . g:startc . "]"
        let g:marker=1
    elseif g:marker==1
        let g:endl=line('.')+1
        let g:marker=2
        let g:endc =col('.')

        let startc=g:startc
        let startl=g:startl
        let endc=g:endc
        let endl=g:endl

        let sc=startc
        let ec=endc
        let counter1=endl-startl
        let xinit=0
        let yinit=0
        let g:CutList=[]

        while counter1
            "echo counter1
            let counter2=endc-startc
            while counter2
                if xinit==0
                    let @l=getline(startl)[startc]
                    let x=getline(startl)[startc]
                    let linestr=x
                    "let @b= '(' . startl . ',' . startc .')' . x
                    let startc=startc+1
                    let xinit=1
                else
                    let @L=getline(startl)[startc]
                    let x=getline(startl)[startc]
                    let linestr=linestr . x
                    "let @B= '(' . startl . ',' . startc .')' . x
                    let startc=startc+1
                endif
                let counter2=endc-startc
            endw

                call add(g:CutList,linestr)
                let linestr=''
                let @L= "\n"
                let startl=startl+1
                let counter1=endl-startl
                let startc=sc
                let endc=ec
           endw

            let @*=@l
            "echo "Rectangle copy command executed successfully and result can be retrived using ctrl-v,ctrl-y for vertical paste"
            echo "command executed,retrive using ctrl-v,ctrl-y for vertical paste " .  "start [" .  g:startl . " " . g:startc . "]" . "end [" . g:endl . " " . g:endc . "]"
            let g:marker=2
    else
    endif
endfun
""""""""""""""""""""""""""""""""""""""""""""
fun! PasteMarkForRectangleCopy()

   if(g:marker==2)
   let x=0

   let x1=0
   let y1=0
   let x2=0
   let y2=0
   let startc=0
   let startl=line('.')


        let mrk=startl
        let xinit=0

   " Determine max length of string
        let maxlen=0
        for var in g:CutList
          let result_string=strpart(getline(startl),0)

          let lng=strlen(result_string)
          if (lng > maxlen)
            let maxlen=lng
          endif
          let startl=startl+1
        endfor

        let startl=mrk

   " Now paste it in each line
        for var in g:CutList
              "Stuff empty space to look more beautifull
              let result_string=strpart(getline(startl),0)
              if startc==0
                  let result_string=s:Stuff(result_string,1," ",maxlen+1)
              else
                  let result_string=s:Stuff(result_string,0," ",maxlen+1)
              endif
              let result_string=result_string . ' ' . var
              call setline(startl,result_string)
              let startl=startl+1
        endfor

        echo "paste of command performed"
      else

        echo "Please perform cut and paste step by step"
      endif

        return
endfun
""""""""""""""""""""""""""""""""""""""""""""
func! s:Stuff(str,justif,fillchar,len)
  let out = a:str
  let intendedlen  = a:len
  let left_or_right = a:justif
  let fillch=a:fillchar

  while intendedlen > strlen(out)
    if left_or_right > 0 
        let out = out . fillch
    else
        let out = fillch . out
    endif
  endwhile
  return out
endfunc

