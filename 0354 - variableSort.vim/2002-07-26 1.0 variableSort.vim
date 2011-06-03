" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/variableSort.vim" {{{
" LAST MODIFICATION: "Fri, 26 Jul 2002 09:29:00 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" This is  a sorting function which  takes the name of  an array-like variable
" which ends  in numerical  subscripts along  the lines  of var0,  var1, var2.
" These can be created quite easily with the var{i} notation of Vim 6.
"
" Other parameters are the starting  and ending subscripts (inclusive) and the
" direction of the sort (1 or 'a' for ascending, otherwise descending).
"
" See below for the optional swap function argument.
"
" Sample custom swap function:
" function! MultipleRangeSwap( sub1, sub2 )
"   let temp = b:rangeS{a:sub1}
"   let b:rangeS{a:sub1} = b:rangeS{a:sub2}
"   let b:rangeS{a:sub2} = temp
"
"   let temp = b:rangeE{a:sub1}
"   let b:rangeE{a:sub1} = b:rangeE{a:sub2}
"   let b:rangeE{a:sub2} = temp
" endfunction
"
" Sample call:
" call SortArray( "b:rangeS", 0, b:numRanges - 1, 1, "MultipleRangeSwap" )
"
" This will perform an ascending sort  on a variable sequence named b:rangeS0,
" b:rangeS1 etc.,  using the MultipleRangeSwap  function for the  actual swap.
" This function  was needed here  because two  variables needed to  be swapped
" together  even though  only  one was  used  in the  sort.  The default  swap
" would've only swapped b:rangeS variables, leaving the b:rangeE variables out
" of sync.


" TODO:
"
" Name of a comparison  function; takes two arguments and returns  -1, 0 or 1.
" (Numerical and  string ones  are provided  and can be  passed in  instead of
" writing custom ones.)
"
" A nicer sort algorithm than the basic bubble sort.
"
" Ability to load only when necessary instead of the way it is now.


" The array is sorted in place; there is no return value from the function.
"
" Optional  argument  is the  name  of  a swap  function  that  takes the  two
" subscripts  and  swaps the  variable  (there  is  one provided  that  should
" actually be good enough for most  cases; however, if something special needs
" to be  done for the swap  -- such as  multiple records, then this  should be
" used.  In this  case, it  is  assumed the  swap function  already knows  the
" variables it's going to swap and thus aren't passed in.)
function! SortArray( arrayName, first, last, direction, ... )
  let swapFun = ''
  if ( exists( 'a:1' ) )
    let swapFun = a:1
  endif

  " echo "Swap function is " . swapFun

  let last = a:last - 1

  while ( last >= a:first )
    let swapped = 0
    let i = a:first

    while ( i <= last )
      if ( a:direction == 'a' || a:direction == 1 )
        if ( {a:arrayName}{i} > {a:arrayName}{i + 1} )
          " echo "Swapping " . {a:arrayName}{i} . " with " . {a:arrayName}{i + 1}
          if ( swapFun != '' )
            execute "call " . swapFun . "( " . i . ", " . ( i + 1 ) . " )"
          else
            call s:Swap( a:arrayName, i, i + 1 )
          endif
          let swapped = 1
        endif
      else
        if ( {a:arrayName}{i} < {a:arrayName}{i + 1} )
          " echo "Swapping " . {a:arrayName}{i} . " with " . {a:arrayName}{i + 1}
          if ( swapFun != '' )
            execute "call " . swapFun . "( " . i . ", " . ( i + 1 ) . " )"
          else
            call s:Swap( a:arrayName, i, i + 1 )
          endif
          let swapped = 1
        endif
      endif

      let i = i + 1
    endwhile

    let last = last - 1
  endwhile
endfunction

" Built in simple swapper
function! <SID>Swap( arrayName, i, j )
  let temp = {a:arrayName}{i}
  let {a:arrayName}{i} = {a:arrayName}{j}
  let {a:arrayName}{j} = temp
endfunction
