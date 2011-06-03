"=============================================================================
" File: sum.vim
" Author: Vanni Brutto <zanac@libero.it>
" Last Change:  16 Oct 2001
" Version: 1.0
" ChangeLog:
"     1.0  : first release
"
" Note: first of all i want to excuse you for my bad english: i'm italian
"       and my knowledge of British language is very poor...
"       i want to underline also that this is not a good script, there are
"       a lot of bugs yet (patch are welcome :))
"       I love feedback, so if you use my script let me know :)
"
"
"
" Usage: Sum([[init-column],[end-column]])")
"        if you omit the end-column Sum will try to understand when the number finish.
"
"        For example: here you can find 3 lines with 3 different values:
"                            10
"                            1.4
"                            45
"
"        of course the Sum of 10+1.4+45=56.4, to use sum.vim just put the cursor in the
"        line with 10, type <S-V> and enter in visual-mode, after that hilight the next
"        two lines and type : to enter a command, it will appare a prompt like:
"        :'<,'>
"                now you must simply type Sum, eg: :'<,'>Sum
"        ...after that just press enter, and a tip with 56.4 will appare :)
"
"        You can also follow a number by an operation, for example:
"                      3-
"                      5*
"                      2
"        Will return 3-5*2=-7
"        Another sample:
"                      -1
"                      -1
"                      -1
"        Will return -3
"
"        If you want to force Sum to calculate the sum of a certain column you can use
"        the two params, for example:
"                     10 100
"                     10 100
"                     10 100
"        The sum of these rows will return 30 (10+10+10), but if you want to sum 100+100+100
"        you can just use 'Sum 26' where 26 is the column that follow 10. If you type
"        something like 'Sum 26 27' you'll obtain 30, and of course 'Sum 26 26' = 3 :)
"
"        Installation: just put sum.vim in the plugin directory :)
"
"        Future addon: i love highlight using <C+V>, if i find how to get the
"                      left and right range of that area i'll remove the args
"                      


com -range -nargs=* Sum <line1>,<line2>call Sum(<f-args>)

function Write_error(msg)
    echohl Error
    echo a:msg
    echohl None
endfunction

function Write_tot(msg)
    echohl Todo
    echo "Sum: " . a:msg
    echohl None
endfunction

function Sum(...) range
      if a:0 == 1
         let par_start     = a:1
         let par_end   = -1
  elseif a:0 == 2
         let par_start     = a:1
         let par_end   = (a:2)-1
  elseif a:0 == 0
         let par_start     = 1
         let par_end   = -1
  else
         call Write_error("Too much args. Usage: Sum([[init-column],[end-column]])")
         return 1
  endif

  let cursor = a:firstline
  let tot = ""

  while cursor <= a:lastline
     let flag_dot = 0
     let flag_number = 0
     let flag_operation = 0

     let str = getline(cursor)
     let str2 = ""
     let tmp = 0
     let end_line = strlen(str)

     if par_end != -1 && end_line > par_end
         let end_line = par_end
     endif

     let i = par_start - 1

     while i <= end_line
         let tmp = strpart(str, i, 1)
         if flag_number == 2 "skip the characters: you already have the number!
         elseif tmp == '.'  && flag_dot "hum... it seems that a number like 20.5. was detected!
                let flag_number = 2
                let str2 = str2 . '+'

         elseif tmp == '.' && !flag_dot "decimal dot detected! :)
                let str2 = str2 . tmp
                let flag_dot = 1

         elseif tmp =~ '[0-9]' && !flag_number "this is the first number detected
                let str2 = str2 . tmp
                let flag_number = 1

         elseif tmp =~ '[+-]' && !flag_number "this is the sign of a number, for example +5 or -3
                let str2 = str2 . tmp
                let flag_operation = 1

         elseif tmp =~ '[+/*-]' && flag_number == 1 "you got an operation after a number
                let str2 = str2 . tmp
                let flag_number = 2

         elseif tmp =~ '[0-9]' && flag_number == 1 "the number is not finished! add the character!
                let str2 = str2 . tmp

         elseif flag_number == 1 "if none of that... set the flag_number = 2 (this will skip next chars)
                let flag_number = 2

         elseif    (flag_operation && !flag_number) || (flag_dot && !flag_number)
                let str2 = strpart(str2, 0, strlen(str2)-1)

         else
         endif

         let i = i + 1
     endwhile
     let tmp = strpart(str2, strlen(str2)-1, 1) "last character of the string get
     if tmp !~ '[+/*-]' && flag_number
         let str2 = str2 . '+'
     endif
     let tot = tot . str2
     let cursor = cursor + 1
  endwhile
  let tot = strpart(tot, 0, strlen(tot)-1)
  let tot = "echo " . tot . " | bc"
  call Write_tot(system(tot))
	  
endfunction

