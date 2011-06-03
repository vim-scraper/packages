"23-Nov-02 Sat 11:50:01 AM 
"Author: Siew Kam Onn kosiew@pd.jaring.my
"Version:1.0.0
"utility to help in debugging vim scripts
"  put this in the plugin directory
"  will add a new menu VIM->Debug->....
"    menu options 
"      DebugOn - turns debug on - will display debug text in VIM scripts
"      DebugOff - turns debug off - will not display debug text in VIM scripts
"      Debug Status - displays debug status in status line
"      Insert Debug Text - insert debug text in vim scripts

"usage:
"  eg in following vim script
"
"1 fun! <SID>JavaImpGenerate()  
"2    let impfile = tempname()
"3    let currBuff = bufnr("%")
"    
"  for debugging, insert debug text in above script as follows
"1 fun! <SID>JavaImpGenerate()  
"2    call Debug("entering JavaImpGenerate")
"3    let impfile = tempname()
"4    call Debug("impfile = " . impfile ) 
"5    let currBuff = bufnr("%")
"6    call Debug("currBuff = " . currBuff ) 

"The debugging stubs in lines 2, 4, 6 will run only if Debug status is on.

function Debug (text)
  if b:Debug == 'on' 
    call confirm (a:text)
  endif
endfunction

function DebugOn ()
  let b:Debug = 'on'
endf

function DebugOff ()
  let b:Debug = 'off'
endf

command! -nargs=0 DebugOn call DebugOn()
command! -nargs=0 DebugOff call DebugOff()

" add/remove custom menu
" VIM Menu

menu VIM.Debug.DebugOn :DebugOn
menu VIM.Debug.DebugOff :DebugOff
menu VIM.Debug.Debug\ Status :echo b:Debug
imenu VIM.Debug.Insert\ Debug\ Text call Debug(" = " .  )9<Left>a

