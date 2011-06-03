
" Vim global plugin for printing ascii table
" Maintainer: Lubos Celko <celbos@inmail.sk>
" Last change: 26. 03. 2002
" Version: 1.2


" Functions prints ascii table in eight columns. 
" All columns contains 32 rows. 


" Load this function only one
if exists("loaded_printascii")
  finish
endif
let loaded_printascii = 1


" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

  function PrintAscii()
    split printascii
    let i = 0

    "ascii table is printed in 32 rows
    while(i < 32)
      let j = 0

      "variable s:asciistring contains string of one line
      let s:asciistring = ""  

      "ascii table is printed in 8 columns
      while(j < 8)

        "add spaces before short numbers
        if ((i+j*32) < 100)
          let s:asciistring = s:asciistring." "
        endif
        if ((i+j*32) < 10)
          let s:asciistring = s:asciistring." "
        endif       

        "ascii number for enter(10) make separately
        if ((i+j*32) != 10)
          "give one ascii number in variable s:asciistring
          "ascii_number-character
          let s:asciistring = s:asciistring.(i+j*32)
          let s:asciistring = s:asciistring."-"
          let s:asciistring = s:asciistring.nr2char(i+j*32)
          let s:asciistring = s:asciistring."  "          
        else
          "enter (10)
          let s:asciistring = " 10-    "
        endif
        
        "add blank character for ascii 0
        if((i+j*32) == 0)
          let s:asciistring = s:asciistring."  " 
        endif

        "lets go on the next column
        let j = j + 1

      endwhile
     
      "finall print one line with 8 ascii numbers
      put =s:asciistring

      "lets go on the next line
      let i = i + 1

    endwhile
  endfunction


" restore 'cpo'
let &cpo = s:cpo_save
