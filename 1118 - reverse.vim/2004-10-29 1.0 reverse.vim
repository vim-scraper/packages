"Vim function to reverse the lines 
"(line 1 swapped with last line say 5, line 2 swap line4 ...)
"by Amit Jain amjain.hss@gmail.com

"To use this function simply source the file
"using :source <filepath>\reverse.vim
"or source it in your vimrc file
"for automatically sourcing this everytime 

"After sourcing simply 
" press <F2> if you wanna reverse all the lines
" slect lines in visual mode and press <F2> if you wanna swap only the highlighted lines


function! Rev(lnum1,lnum2)
"check whether lnum1 and lnum2 exist. if not simply return
let s:t1 = indent(a:lnum1)
let s:t2 = indent(a:lnum2)

if s:t1 < 0 || s:t2 < 0
   echo "..errror"
   return
endif   


let s:t1 = a:lnum1
let s:t2 = a:lnum2

if a:lnum1 > a:lnum2
let s:t1 = a:lnum2
let s:t2 = a:lnum1
endif

let s:diff = s:t2 - s:t1 + 1
let s:counter = s:diff/2

while s:counter > 0
let s:counter = s:counter - 1

let s:text1 = getline(s:t1)
let s:text2 = getline(s:t2)

call setline(s:t1,s:text2)
call setline(s:t2,s:text1)

let s:t1 = s:t1+1
let s:t2 = s:t2-1


endwhile

endfunction
:command! -range=% Reverse :call Rev(<line1>,<line2>)
:map <F2> :Reverse<CR>
