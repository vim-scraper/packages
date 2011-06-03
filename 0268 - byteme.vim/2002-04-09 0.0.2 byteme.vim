"=============================================================================
"    Copyright: Copyright (C) 2002 Robert Roberts
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               bytme.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damamges
"               resulting from the use of this software.
" Name Of File: bytme.vim
"  Description: Goto Hex Offset Vim Plugin
"   Maintainer: Robert Roberts (res02ot0@gte.net)
"          URL: None
"  Last Change: 04-09-2002 10:18hrs
"      Version: 0.0.2
"        Usage: Normally, this file should reside in the plugins directory. 
"               If not, you must manually source this file using 
"                  :source /thepath/byteme.vim
"
"               You may use the default keymappings of
"
"                 <M-g>  - Prompts for new offset.
"
"      History: The only difference between version 0.0.1 and 0.0.2 is that I
"               forgot to add this description block and any instructions in
"               version 0.0.1 and vim.sr.net only allows new versions, not 
"               refreshes of existing versions.
"=============================================================================
"
" Vim.sf.net Summary:   Goto Hex Offset
"
" Vim.sf.net Description:
"
"       A few years ago I had to write a streaming single-page to multi-page 
"       TIFF converter.  Doing so required that I spend a lot of time 
"       wondering around in a HEX editor looking at the bytes that made up the 
"       TIFF headers.  The biggest part to tweaking the image files was 
"       getting all of the offsets calculated correctly. 

"       I was using MultiEdit for an editor at the time and needed "Goto Hex 
"       Offset" functionality that ME didn't have so I wrote a cmac function 
"       to do it for me.  It's purpose was to take me to a specified _byte_ 
"       in _me_ (MultiEdit) hence I named the function "ByteMe". 

"       A few months ago, I started using VIM.  Once again I needed "Goto Hex 
"       Offset" functionality which VIM did not have.  So, I rewrote my cmac 
"       ByteMe functionality in VIM and the result was this byteme.vim plugin. 

"       I'm fairly new to VIM so I expect my code could be much, much simpler, 
"       but this at least works for now. 

"       I wrote this on a Win2000 box using GVim 6.0 and have since upgraded 
"       to GVim 6.1 with no effect to byteme.vim. 

"       I am using the xxd utility to generate the hex.  For anyone who might 
"       not have xxd on your Windows box, I got mine by downloading and 
"       installing Cygwin and then adding c:\cygwin\bin to my path. 

"       byteme.vim is written to use the format returned by the generic xxd 
"       hex dump.  If you use any switched to get xxd to change the format, 
"       or use some other utility to generate the hex, you'll probably have to 
"       rewrite byteme.vim. 

"       To use: Once you have installed the plugin, reopen VIM, open the file 
"       you plan to view, convert to hex, hit <Alt-g>, enter the new offset in 
"       hex (e.g. fe09), hit enter, and you should be there.  An entry 
"       containing non-valid hex chars (e.g. fgtr) should take you to byte 0.  
"       If the offset you enter is past then end of your file, you should end 
"       up at the end of the file but without any major error/warning so pay 
"       attention. 
 
" Install Details 
"       Just drop byteme.vim in your plugin directory.  If you do NOT want the 
"       default keymap to use <M-g> (Alt-g), edit byteme.vim. 

" =================================================================================================
" Goto hex offset.  Will prompt for new hex value.
noremap <M-g> :call ByteMe()<CR>

" =================================================================================================
fun ByteMe()
  " Get the current line number but -1 to use base 0 to make the *16 work right.
  let curline = line(".") - 1
  " Get the current column number.
  let curcol  = col(".")
  " If the curcol is greater than 48...
  if curcol > 47
      " Make it 47.  We don't care about the text to the right of the hex.
      let curcol = 47
  endif
  " Subtract 10 from the curcol.  We don't care about the line numbers on the left.
  let curcol = curcol - 10
  " Adjust for when the cursor was sitting off in the line numbers.
  if curcol < 0
    let curcol = 0
  endif
  " Initiaze this.  It will be set to 1 later on if we are sitting on the second byte of a word.
  let midwrd = 0
  " If column is greater than 0, we are sitting the actual hex code area (like we want).
  if curcol > 0
    " This colmod and midwrd stuff is just for calculating the camefrom hex when we
    " are mid hexword.
    let colmod = curcol % 5
    " If colmod is not 0, we are midword.
    if colmod > 0
      " Will add 1 to get midword hex char.
      let midwrd = 1
    endif
    " Divide the column by 5 and then multiply by two and it will give you the
    " proper integer column number for the first byte in each 2 byte group.
    let curcol = curcol * 2 / 5
  endif
  " There are 16 bytes in each line plus the current column calculation.
  let offset = (curline * 16) + curcol
  " Add the midword adjustment for being in the middle of a hexword if needed.
  let offset = offset + midwrd
  "if offset < 0
  "  let offset = 0
  "endif
  " Convert the integer offset to Hex.
  let hexoffset = Nr2Hex(offset)
  " For display asthetics only.  When sitting at the very first char in the file,
  " the camefrom hex will be "" which naturally displays as nothing.
  if hexoffset == ""
    let hexoffset = "00"
  endif
  " Get the new hex offset (the goto) from the user.
  "let hexgoto = toupper(input("Enter New Hex Offset (" . toupper(offset) . "): "))
  let hexgoto = toupper(input("Enter New Hex Offset (" . toupper(hexoffset) . "): "))
  " Convert the hex string to an integer.
  let intgoto=Hex2Nr(hexgoto)
  " Calculate the line number of the new offset.
  let newline = intgoto / 16 + 1
  " Calculate the column number within that new line. I could do it in one line,
  " but this is less obfuscated.
  let newcol = intgoto % 16
  let newcol = newcol * 5 / 2
  let newcol = newcol + 10
  " Go to that new line.
  exec ":" . newline
  " Go to that new column.
  exec "norm " . newcol . "|"
  
  " Echo ByteMe() values.
  " Choose the one you want or write your own.
  "echo " hexfrom: [" . toupper(hexoffset) . "]" . " intfrom: [" . offset ."] hexto: [" . hexgoto ."] intto: [" . intgoto ."] newline: [" . newline . "] newcol: [" . newcol . "]"
  echo " hexfrom: [" . toupper(hexoffset) . "]" . " intfrom: [" . offset ."] hexto: [" . hexgoto ."] intto: [" . intgoto ."]"
  return ""
endfun 
" =================================================================================================
" Had to write this one myself.  I expect there is a much easier way to make this work, but I'm
" too new to VIM to be overly creative.
" The function Hex2Nr() returns the interger value of a HEX string.
" Author Note: This actually works!  I amaze myself.  ...but to explain it...
func Hex2Nr(hx)
  " Grab the passed in hex string.
  let h = a:hx
  " Initialize the result to zero.
  let nr = 0
  " Initialize the outside loop counter.
  let loopcntr=0
  " Grab the length of the hex string.
  let cntr = strlen(h)
  " Start the outside loop.
  while cntr > 0
    " Initialize the power base variable.
    let pwrbase=1
    " Initialize the power counter. The -1 was added after I found the
    " results to always be one power too high.
    let pwrcntr=cntr-1
    " Create my own pow() functionality (e.g. 16^x).
    while pwrcntr > 0
      " This will do powers of 16 only.
      let pwrbase=pwrbase*16
      " Decrement the power counter. E.g. for a HEX string with a length of
      " 4, this will do a 1*16*16*16
      let pwrcntr=pwrcntr-1
    endwhile
    " Start grabbing the chars off the left and work right.
    let xchr = strpart(h,loopcntr,1)
    " Find the char in the haystack and return its position which will equal
    " its single char value.
    let ichr = stridx("0123456789ABCDEF",xchr)
    " Multiply that single char value against the powerbase and add it to the total.
    let nr = nr + (ichr*pwrbase)
    " Tweak the counters.
    let cntr=cntr-1
    let loopcntr=loopcntr+1
  endwhile
  " return the interger value.
  return nr
endfunc
" =================================================================================================
" Found this function in the standard VIM :help documentation using :h eval-examples
" The function Nr2Hex() returns the Hex string of a number.
func Nr2Hex(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '0123456789ABCDEF'[n % 16] . r
    let n = n / 16
  endwhile
  return r
endfunc
" =================================================================================================
