" =============================================================================
" File:		ruler.vim (global plugin)
" Last Changed: 08-April-2003
" Maintainer:	Jeffrey Harkavy <harkavy at qualexphoto dot com>
" Version:	1.0
" License:      Vim License
" =============================================================================
" Sometimes you just need a column-counting ruler on screen.
" 
" This will insert a ruler in a number of different formats above the current
" line.  The ruler can be printed to any width.  The columns can be ".", a
" Number, or some other character.  Every 10th column can be marked with a
" different character.
"
" The Arguments to the Ruler function are
"    1: Ones character - what character to print at every column.  "1" will print
"    the column number.  Default is "1"
"    2: Tens character - Character to print every 10th column.  Default is "*"
"    3: Width - How many columns to print.  Default is length of the current
"    line.  If the current line is empty then 80 columns w/b printed.
" 
" =============================================================================

"*****************************************************************
"* Ruler commands
"*****************************************************************
:command! -nargs=0 Rule call Ruler("1","*",80)
:command! -nargs=0 Ruldot call Ruler(".","1",80)
:command! -nargs=? Ruler call Ruler(<f-args>)

nmap <Leader>r :cal Ruler("","*",80)<CR>
nmap <Leader>r. :cal Ruler(".","1",80)<CR>
nmap <Leader>r1 :cal Ruler("1","*",80)<CR>
nmap <Leader>r.* :cal Ruler(".","*",80)<CR>
nmap <Leader>r1* :cal Ruler("1","*",80)<CR>
nmap <Leader>r.1 :cal Ruler(".","1",80)<CR>

function! s:LineLen()
     let r = strlen( getline( "." ) )
     if ( r == 0 )
        let r = 80
     endif

     return r
endfunction

function! Ruler(...)
   "echo a:0
   if ( a:0 == 0 )
      let RuleOne = "1"
      let RuleTen = "*"
      let RuleLen = s:LineLen()
   elseif ( a:0 == 1 )
      let RuleOne = ( a:1 == "" ? "." : a:1 )
      let RuleTen = "*"
      let RuleLen = s:LineLen()
   elseif ( a:0 == 2 )
      let RuleOne = ( a:1 == "" ? "." : a:1 )
      let RuleTen = ( a:2 == "" ? "*" : a:2 )
      let RuleLen = s:LineLen()
   elseif ( a:0 > 2 )
      let RuleOne = ( a:1 == "" ? "." : a:1 )
      let RuleTen = ( a:2 == "" ? "*" : a:2 )
      let RuleLen = a:3
   endif
   let n = 1
   let disptext = ""
   while ( n <= RuleLen )
     let char = ( n % 10 == 0 ? ( RuleTen == "1" ? n : RuleTen ) : ( RuleOne == "1" ? n % 10 : RuleOne ) )
     let disptext = disptext . char
     let n = n + strlen( char )
     "echo disptext
   endwhile

  put! =disptext
  "echo disptext
endfunction
