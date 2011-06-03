
" Vim global plugin for printing ascii table
" Maintainer: Lubos Celko <celbos@inmail.sk>
" Updated by Bill McCarthy
" Last change: 19. 08. 20025
" Version: 1.3


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
    32split printascii

    " Save 'report', 'expandtab' and 'tabstop'
    let s:report_save = &report
    let s:expandtab_save = &expandtab
    let s:tabstop_save = &tabstop
    set expandtab tabstop=8 report=99999

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

    " First pretty it up a bit
    %s/\s\+/\t/g
    %s/\t\(\d\)-/\t  \1-/g
    %s/\t\(\d\d\)-/\t \1-/g
    %s/^\t//
    1d|retab
    " now restore 'report', 'expandtab' and 'tabstop'
    let &report = s:report_save
    let &expandtab = s:expandtab_save
    let &tabstop = s:tabstop_save
  endfunction


" restore 'cpo'
let &cpo = s:cpo_save
