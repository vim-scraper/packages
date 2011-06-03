" multvals.vim -- Array operations on Vim multi-values, or just another array.
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Modified: 26-Nov-2002 @ 11:09
" Requires: Vim-6.0 or higher, genutils.vim(1.2) for sorting support.
" Version: 2.4.0
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://vim.sourceforge.net/script.php?script_id=171
" Environment:
"   Adds
"       MvAddElement
"       MvInsertElementAt
"       MvRemoveElement
"       MvRemoveElementAt
"       MvRemoveElementAll
"       MvReplaceElementAt
"       MvNumberOfElements
"       MvStrIndexOfElement
"       MvStrIndexAfterElement
"       MvStrIndexOfElementAt
"       MvIndexOfElement
"       MvContainsElement
"       MvElementAt
"       MvLastElement
"       MvPushToFront
"       MvPushToFrontElementAt
"       MvPullToBack
"       MvPullToBackElementAt
"       MvRotateLeftAt
"       MvRotateRightAt
"       MvSwapElementsAt
"       MvQSortElements
"       MvIterCreate
"       MvIterDestroy
"       MvIterHasNext
"       MvIterNext
"       MvCmpByPosition
"       MvPromptForElement
"       MvPromptForElement2
"     global functions.
"
" Usage:
"   - An array is nothing but a string of multiple values separated by a
"     pattern.  The simplest example being Vim's multi-value variables such as
"     tags. You can use the MvAddElement() function to create an array.
"     However, there is nothing special about this function, you can as well
"     make up the string by simply concatinating elements with the chosen
"     pattern as a separator.
"   - The pattern can be any regular expression as long as you use the array for
"     only read-only purposes. If you for example want to go over the words in
"     a sentence, then an easy way would be to treat the sentence as an array
"     with '\s\+' as a separator pattern.
"   - Suggested usage to go over the elements is to use the iterater functions
"     as shows in the below example
"   Ex Usage:
"       call MvIterCreate(&tags, ",", "Tags")
"     	while MvIterHasNext("Tags")
"     	  call input("Next element: " . MvIterNext("Tags"))
"     	endwhile
"     	call MvIterDestroy("Tags")
"
" ALMOST ALL OPERATIONS TAKE THE ARRAY AND THE SEPARATOR AS THE FIRST TWO
"   ARGUMENTS.
" All element-indexes start from 0 (like in C++ or Java).
" All string-indexes start from 0 (as it is for Vim built-in functions).
"
" Changes in 2.3:
"   - A variant of MvPromptForElement to specify the number of columns that
"     you want the elements to be formatted in.
"   - New functions MvQSortElements() and MvSwapElementsAt() 
"   - Worked-around a bug in vim that effects MvElementAt() for last element
"     in a large array.
" Changes in 2.1.1:
"   - Now all the operations work correctly with elements that have special
"     chars in them.
" Changes in 2.1.0:
"   - Improved the read-only operations to work with regular expressions as
"     patterns.
" Changes in 2.0.3:
"   - Fixed bugs in MvStrIndexOfElement(), MvIterHasNext() and MvCmpByPosition()
" Changes in 2.0.3:
"   - New functions were added.
"   - The order of arguments for MvIterCreate has been changed for the sake of
"       consistency.
"   - Prefixed all the global functions with "Mv" to avoid global name
"       conflicts.
"
" TODO:
"   The script now works with the regex patterns as separator, but it mostly
"     works for readonly operations only. It may be possible to provide an
"     additional argument to be used as the separator string which is different
"     from separator pattern and matches separator pattern, for all the write
"     operations.
"   Many writer functions can also be modified to work with pattern as a
"     separator.
"   What if there are duplicate items. I think I am not taking care of it.
"   MvRemoveElement and MvRemoveElementAt seem to be very similar, but use
"     different logic.
"   Some performance improvement should be possible in: MvElementAt,
"     MvSwapElementsAt, MvQSortElements
"   Using '\%(\s\|\n\)\+' as separator pattern for a block of text containing
"     newlines doesn't detect newlines as a separtor.
"
"

if exists("loaded_multvals")
  finish
endif
let loaded_multvals = 1


function! s:MyScriptId()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction
let s:myScriptId = s:MyScriptId()


" Adds an element and returns the new array.
" Params:
"   ele - Element to be added to the array.
" Returns:
"   the new array.
function! MvAddElement(array, sep, ele)
  let array = s:EnsureTrailingSeparator(a:array, a:sep)
  return array . a:ele . a:sep
endfunction


" Insert the element before index and return the new array. Index starts from 0.
" Params:
"   ele - Element to be inserted into the array.
"   index - The index before which the element should be inserted.
" Returns:
"   the new array.
function! MvInsertElementAt(array, sep, ele, index)
  let array = s:EnsureTrailingSeparator(a:array, a:sep)
  if a:index == 0
    return a:ele . a:sep . array
  else
    let strIndex = MvStrIndexOfElementAt(array, a:sep, a:index)
    if strIndex < 0
      return array
    endif

    let sub1 = strpart(array, 0, strIndex)
    let sub2 = strpart(array, strIndex, strlen(array))
    return sub1 . a:ele . a:sep . sub2
  endif
endfunction


" Removes the element and returns the new array.
" Params:
"   ele - Element to be removed from the array.
" Returns:
"   the new array.
function! MvRemoveElement(array, sep, ele)
  let array = s:EnsureTrailingSeparator(a:array, a:sep)
  let strIndex = MvStrIndexOfElement(array, a:sep, a:ele)
  " First remove this element.
  if strIndex != -1
    let sub = strpart(array, 0, strIndex)
    let sub = sub . strpart(array, strIndex + strlen(a:sep) + strlen(a:ele),
            \ strlen(array))
  else
    let sub = array
  endif
  return sub
endfunction


" Remove the element at index. Index starts from 0.
" Params:
"   index - The index of the element that needs to be removed.
" Returns:
"   the new array.
function! MvRemoveElementAt(array, sep, index)
  let array = s:EnsureTrailingSeparator(a:array, a:sep)
  let strIndex = MvStrIndexOfElementAt(array, a:sep, a:index)
  if strIndex < 0
    return array
  endif

  let sub1 = strpart(array, 0, strIndex)
  let sub2 = strpart(array, strIndex, strlen(array))
  let ind2 = match(sub2, a:sep)
  if ind2 < 0
    return array
  else
    let sub2 = strpart(sub2, ind2 + strlen(a:sep),
	  \ strlen(sub2) - ind2 - strlen(a:sep))
  endif
  return sub1 . sub2
endfunction


" Remove the all occurances of element in array.
" Contributed by Steve Hall <digitect at mindspring.com>
" Params:
"   ele - Element to be removed from the array.
" Returns:
"   the new array.
function! MvRemoveElementAll(array, sep, ele)
  let array = a:array
  while MvContainsElement(array, a:sep, a:ele) == 1
    let array = MvRemoveElement(array, a:sep, a:ele)
  endwhile
  return array
endfunction


" Replace the element at index with element
" Contributed by Steve Hall <digitect at mindspring.com>
" Params:
"   ele - The new element to replace in the array.
"   index - The index of the element that needs to be replaced.
" Returns:
"   the new array.
function! MvReplaceElementAt(array, sep, ele, index)
  " insert element
  let array = MvInsertElementAt(a:array, a:sep, a:ele, a:index)
  " remove element following
  let array = MvRemoveElementAt(array, a:sep, a:index + 1)
  return array
endfunction


" Rotates the array such that the element at index is on the left (the first).
" Params:
"   index - The index of the element that needs to be rotated.
" Returns:
"   the new array.
function! MvRotateLeftAt(array, sep, index)
  if a:index <= 0 " If index is 0, there is nothing that needs to be done.
    return a:array
  endif

  let array = s:EnsureTrailingSeparator(a:array, a:sep)
  let strIndex = MvStrIndexOfElementAt(array, a:sep, a:index)
  if strIndex < 0
    return array
  endif
  return strpart(array, strIndex) . strpart(array, 0, strIndex)
endfunction


" Rotates the array such that the element at index is on the right (the last).
" Params:
"   index - The index of the element that needs to be rotated.
" Returns:
"   the new array.
function! MvRotateRightAt(array, sep, index)
  if a:index < 0
    return a:array
  endif

  let array = s:EnsureTrailingSeparator(a:array, a:sep)
  let strIndex = MvStrIndexOfElementAt(array, a:sep, a:index + 1)
  if strIndex < 0
    return array
  endif
  return strpart(array, strIndex) . strpart(array, 0, strIndex)
endfunction


" Returns the number of elements in the array.
" Returns:
"   the number of elements that are present in the array.
function! MvNumberOfElements(array, sep)
  "let array = s:EnsureTrailingSeparator(a:array, a:sep)
  let array = a:array
  let pat = '\(.\)\{-}\(' . a:sep . '\)\{-1\}'

  " Replace all the elements with the separators with a single character and
  " count the number of chars.
  let mod = substitute(array, pat, 'x', 'g')
  if strridx(mod, 'x') != (strlen(mod) - 1)
    let nElements = strlen(matchstr(mod, '^x*')) + 1
  else
    let nElements = strlen(mod)
  endif
  return nElements
endfunction


" Returns the string-index of the element in the array, which can be used with
"   string manipulation functions such as strpart().
" Params:
"   ele - Element whose string-index is to be found.
" Returns:
"   the string index of the element, starts from 0.
function! MvStrIndexOfElement(array, sep, ele)
  let ele = s:Escape(a:ele)
  if s:IsRegularExpression(a:sep)
    if MvNumberOfElements(a:array, a:sep) == 1 &&
        \ !s:HasTrailingSeparator(a:array, a:sep)
      return (a:array == a:ele) ? 0 : -1
    else
      let index = match(a:array, a:sep . ele . a:sep)
      " Take care of boundary cases.
      if index == -1
        let index = match(a:array, '^' . ele . a:sep)
        if index == -1
          let index = match(a:array, a:sep . ele . '$')
          if index != -1
            let matchStr = matchstr(a:array, a:sep . ele . '$')
            let index = index + (strlen(matchStr) - strlen(a:ele))
          endif
        endif
      else
        let matchStr = matchstr(a:array, a:sep . ele . a:sep)
        let index = index + (strlen(matchStr) - strlen(
                    \ matchstr(matchStr, a:sep))) + 1
      endif
      return index
    endif
  else
    let array = a:sep . s:EnsureTrailingSeparator(a:array, a:sep)
    return match(array, a:sep . ele . a:sep)
  endif
endfunction


" Returns the index after the element.
" Params:
"   ele - Element after which the index needs to be found.
" Returns:
"   the string index after the element including the separator. Starts from 0.
"     Returns -1 if there is no such element or if it is the last index.
function! MvStrIndexAfterElement(array, sep, ele)
  let ele = s:Escape(a:ele)
  if s:IsRegularExpression(a:sep)
    if MvNumberOfElements(a:array, a:sep) == 1
      return -1
    else
      let index = matchend(a:array, a:sep . ele . a:sep)
      " Take care of boundary cases.
      if index == -1
        let index = matchend(a:array, '^' . ele . a:sep)
      endif
      if index >= strlen(a:array)
        let index = -1
      endif
      return index
    endif
  else
    let array = a:sep . s:EnsureTrailingSeparator(a:array, a:sep)
    let index = matchend(array, a:sep . ele . a:sep)
    if index >= strlen(array)
      let index = -1
    endif
    return index - strlen(a:sep)
  endif
endfunction


" Returns the string-index of the element present at element-index, which can
"   be used with string manipulation functions such as strpart().
" Params:
"   index - Index of the element whose string-index needs to be found.
" Returns:
"   the string index of the element, starts from 0.
function! MvStrIndexOfElementAt(array, sep, index)
  if a:index < 0
    return -1
  elseif a:index == 0
    return 0
  endif

  let prevEle = MvElementAt(a:array, a:sep, a:index - 1)
  return MvStrIndexAfterElement(a:array, a:sep, prevEle)
endfunction


" Returns the element-index of the element in the array, which can be used with
"   other functions that accept element-index such as MvInsertElementAt,
"   MvRemoveElementAt etc.
" Params:
"   ele - Element whose element-index is to be found.
" Returns:
"   the element-index of the element, starts from 0.
function! MvIndexOfElement(array, sep, ele)
  let strIndex = MvStrIndexOfElement(a:array, a:sep, a:ele)
  if strIndex < 0
    return -1
  endif

  let sub = strpart(a:array, 0, strIndex)
  return MvNumberOfElements(sub, a:sep)
endfunction


" Returns 1 (for true) if the element is contained in the array and 0 (for
"   false) if not.
" Params:
"   ele - Element that needs to be tested for.
" Returns:
"   1 if element is contained and 0 if not.
function! MvContainsElement(array, sep, ele)
  if MvStrIndexOfElement(a:array, a:sep, a:ele) >= 0
    return 1
  else
    return 0
  endif
endfunction


" Returns the indexth element in the array. The index starts from 0.
" See the better impl. below.
"function! MvElementAt(array, sep, index)
"  let eleCount = 0
"  call MvIterCreate(a:array, a:sep, "MyIter")
"  let ele = ""
"  while MvIterHasNext("MyIter")
"    let ele = MvIterNext("MyIter")
"    if eleCount == a:index
"      break
"    endif
"    let eleCount = eleCount + 1
"  endwhile
"  call MvIterDestroy("MyIter")
"  return ele
"endfunction

" Returns the index'th element in the array. The index starts from 0.
" Inspired by the posts in the vimdev mailing list, by Charles E. Campbell &
"   Zdenek Sekera.
" Params:
"   index - Index at which the element needs to be found.
" Returns:
"   the element at the given index.
function! MvElementAt(array, sep, index)
  if a:index < 0
    return ""
  endif
  let index = a:index + 1
  "let array = s:EnsureTrailingSeparator(a:array, a:sep)
  let array = a:array

  let nElements = MvNumberOfElements(array, a:sep)
  if index > nElements
    return ""
  endif

  let sub = ""
  if nElements == 1
    if ! s:HasTrailingSeparator(array, a:sep)
      let sub = array
    else
      let sub = strpart(array, 0,
                  \ (strlen(array) - strlen(matchstr(array, a:sep))))
    endif

  " Work-around for vim taking too long for last element, if the string is
  "   long.
  elseif index > 1 && index == nElements " Last element.
    " Extract upto the previous element.
    let pat1 = '\(\(.\{-}' . a:sep . '\)\{' . (index - 1) . '}\).*$'
    let sub1 = substitute(array, pat1, '\1','')
    if strlen(sub1) != 0
      let sub2 = strpart(array, strlen(sub1))
      if strlen(sub2) != 0
	let ind = match(sub2, a:sep)
	if ind == -1
	  let sub = sub2
	else
	  let sub = strpart(sub2, 0, ind)
	endif
      endif
    endif
  else
    " If we don't have to support a regex as separator, the following alone
    "   would suffice.
    "let array = s:EnsureTrailingSeparator(a:array, a:sep)
    let pat1 = '\(\(.\{-}' . a:sep . '\)\{' . index . '}\).*$'
    " Extract upto this element.
    let sub1 = substitute(array, pat1, '\1','')
    if strlen(sub1) != 0 && index > 1
      let pat2 = '\(\(.\{-}' . a:sep . '\)\{' . (index - 1) . '}\).*$'
      " Extract upto the previous element.
      let sub2 = substitute(sub1, pat2, '\1','')
      if strlen(sub2) != 0
        "let sub = strpart(sub1, strlen(sub2),
                "\ strlen(sub1) - strlen(sub2) - strlen(a:sep))
        let sub3 = strpart(sub1, strlen(sub2))
        if s:HasTrailingSeparator(sub3, a:sep)
          let sub = strpart(sub3, 0,
                  \ (strlen(sub3) - strlen(matchstr(sub3, a:sep))))
        else
          let sub = sub3
        endif
      endif
    else
      let sub = strpart(sub1, 0,
                  \ (strlen(sub1) - strlen(matchstr(sub1, a:sep))))
    endif
  endif
  return sub
endfunction


" Returns the last element in the array.
" Returns:
"   the last element in the array.
function! MvLastElement(array, sep)
  if s:IsRegularExpression(a:sep)
    let nElements = MvNumberOfElements(a:array, a:sep)
    if nElements == 1
      let sub = a:array
    else
      let sub = strpart(a:array,
              \ MvStrIndexAfterElement(a:array, a:sep, nElements - 1))
    endif
    if s:HasTrailingSeparator(sub, a:sep)
      let sub = strpart(sub, 0,
              \ (strlen(sub) - strlen(matchstr(sub, a:sep))))
    endif
  else
    let pat = '^.*\(.\+' . a:sep . '\)\{-1}$'
    let array = s:EnsureTrailingSeparator(a:array, a:sep)
    let sub = substitute(array, pat, '\1','')
    if strlen(sub) > 0
      let sub = strpart(sub, 0, strlen(sub) - strlen(a:sep))
    endif
  endif
  return sub
endfunction


" Moves the element to the front of the array. Useful for maintaining an MRU
"  list. Even if the element doesn't exist in the array, it is still added to
"  the front of the array. See selectbuf.vim at vim.sf.net for an example
"  usage.
" Params:
"   ele - Element that needs to be pushed to the front.
" Returns:
"   the new array.
function! MvPushToFront(array, sep, ele)
  let array = MvRemoveElement(a:array, a:sep, a:ele)
  let array = a:ele . a:sep . array
  return array
endfunction


" Moves the element at the specified index to the front of the array. Useful
"  for maintaining an MRU list. Even if the element doesn't exist in the array,
"  it is still added to the front of the array. See selectbuf.vim at vim.sf.net
"  for an example usage.
" Params:
"   index - Index of the element that needs to moved to the front of the array.
" Returns:
"   the new array.
function! MvPushToFrontElementAt(array, sep, index)
  let ele = MvElementAt(a:array, a:sep, a:index)
  return MvPushToFront(a:array, a:sep, ele)
endfunction


" Moves the element to the back of the array. Even if the element doesn't exist
"   in the array, it is still added to the back of the array.
" Params:
"   ele - Element that needs to be pulled to the back.
" Returns:
"   the new array.
function! MvPullToBack(array, sep, ele)
  let array = s:EnsureTrailingSeparator(MvRemoveElement(a:array, a:sep, a:ele),
        \ a:sep)
  let array = array . a:ele . a:sep
  return array
endfunction


" Moves the element at the specified index to the back of the array. Even if
"   the element doesn't exist in the array, it is still added to the back of
"   the array.
" Params:
"   index - Index of the element that needs to moved to the back of the array.
" Returns:
"   the new array.
function! MvPullToBackElementAt(array, sep, index)
  let ele = MvElementAt(a:array, a:sep, a:index)
  return MvPullToBack(a:array, a:sep, ele)
endfunction


" Swaps the elements at the specified indexes.
" Params:
"   index1 - index of one of the elements.
"   index2 - index of the other element.
" Returns:
"   the new array with swapped elements.
function! MvSwapElementsAt(array, sep, index1, index2)
  if a:index1 == a:index2
    return a:array
  endif

  if a:index1 > a:index2
    let index1 = a:index2
    let index2 = a:index1
  else
  let index1 = a:index1
  let index2 = a:index2
  endif
  let ele1 = MvElementAt(a:array, a:sep, index1)
  let ele2 = MvElementAt(a:array, a:sep, index2)
  let array = MvRemoveElement(a:array, a:sep, ele1)
  let array = MvRemoveElement(array, a:sep, ele2)
  if index1 >= MvNumberOfElements(array, a:sep)
    let array = MvAddElement(array, a:sep, ele2)
  else
    let array = MvInsertElementAt(array, a:sep, ele2, index1)
  endif
  if index2 >= MvNumberOfElements(array, a:sep)
    let array = MvAddElement(array, a:sep, ele1)
  else
    let array = MvInsertElementAt(array, a:sep, ele1, index2)
  endif
  return array
endfunction


" Sorts the elements in the array using the given comparator and in the given
"   direction using quick sort algorithm.
" Returns:
"   The new sorted array.
" See:
"   QSort2() function from genutils.vim
function! MvQSortElements(array, sep, cmp, direction)
  let s:arrayForSort{'array'} = a:array
  let s:arrayForSort{'sep'} = a:sep
  let nElements = MvNumberOfElements(a:array, a:sep)
  " Create an array containing indirection indexes.
  let s:sortArrayIndexes = ''
  let i = 0
  while i < nElements
    let s:sortArrayIndexes = s:sortArrayIndexes . i . ','
    let i = i + 1
  endwhile
  call QSort2(1, nElements, a:cmp, a:direction,
	\ s:myScriptId . 'SortGetElementAt', s:myScriptId . 'SortSwapElements',
	\ '')

  " Finally reconstruct the array from the sorted indexes.
  let array = ''
  let nextEle = ''
  call MvIterCreate(s:sortArrayIndexes, ',', 'MvSortElements')
  while MvIterHasNext('MvSortElements')
    let nextEle = MvElementAt(a:array, a:sep, MvIterNext('MvSortElements'))
    let array = MvAddElement(array, a:sep, nextEle)
  endwhile
  call MvIterDestroy('MvSortElements')
  return array
endfunction


function! s:SortGetElementAt(index, context)
  let index = MvElementAt(s:sortArrayIndexes, ',', a:index - 1)
  return MvElementAt(s:arrayForSort{'array'}, s:arrayForSort{'sep'}, index)
endfunction


function! s:SortSwapElements(index1, index2, context)
  let s:sortArrayIndexes = MvSwapElementsAt(s:sortArrayIndexes, ',', a:index1 - 1, a:index2 - 1)
  if (MvNumberOfElements(s:sortArrayIndexes, ',') != 5)
    echomsg "SortSwapElements: swap bug for array: " . s:sortArrayIndexes . " index1: " . a:index1 . " index2: " . a:index2
  endif
endfunction


" Creates a new iterator with the given name. This can be passed to
"   MvIterHasNext() and MvIterNext() to iterate over elements. Call MvIterDestroy()
"   to remove the space occupied by this iterator.
" Do not modify the array while using the iterator.
" Params:
"   iterName - A unique name that is used to identify this iterator. The
"                storage is alloted in the script name space (for Vim 6.0 or
"                above) or in the global name space (for previous Vim versions).
function! MvIterCreate(array, sep, iterName)
  exec "let " . s:GetVarForIter(a:iterName) . "_array = a:array"
  exec "let " . s:GetVarForIter(a:iterName) . "_sep = a:sep"
  exec "let " . s:GetVarForIter(a:iterName) . "_max = " .
        \ MvNumberOfElements(a:array, a:sep)
  exec "let " . s:GetVarForIter(a:iterName) . "_curIndex = 0"
endfunction


" Deallocates the space occupied by this iterator.
" Params:
"   iterName - The name of the iterator to be destroyed that was previously
"                created using MvIterCreate.
function! MvIterDestroy(iterName)
  exec "unlet " . s:GetVarForIter(a:iterName) . "_array"
  exec "unlet " . s:GetVarForIter(a:iterName) . "_sep"
  exec "unlet " . s:GetVarForIter(a:iterName) . "_max"
  exec "unlet " . s:GetVarForIter(a:iterName) . "_curIndex"
endfunction


" Indicates if there are more elements in this array to be iterated. Always
"   call this before calling MvIterNext().
" Do not modify the array while using the iterator.
" Params:
"   iterName - The name of the iterator that was previously created using
"                MvIterCreate.
" Returns:
"   1 (for true) if has more elements or 0 (for false).
function! MvIterHasNext(iterName)
  if ! exists(s:GetVarForIter(a:iterName) . "_curIndex")
    return 0
  endif

  exec "let max = " . s:GetVarForIter(a:iterName) . "_max"
  if max == 0
    return 0
  endif

  exec "let array = " . s:GetVarForIter(a:iterName) . "_array"
  if array == ""
    return 0
  endif

  exec "let curIndex = " . s:GetVarForIter(a:iterName) . "_curIndex"
  if curIndex < max
    return 1
  else
    return 0
  endif
endfunction


" Returns next value or "" if none. You should always call MvIterHasNext() before
"   calling this function.
" Do not modify the array while using the iterator.
" Params:
"   iterName - The name of the iterator that was previously created using
"                MvIterCreate.
" Returns:
"   the next element in the iterator (array).
function! MvIterNext(iterName)
  if ! exists(s:GetVarForIter(a:iterName) . "_curIndex")
    return ""
  endif

  exec "let curIndex = " . s:GetVarForIter(a:iterName) . "_curIndex"
  exec "let array = " . s:GetVarForIter(a:iterName) . "_array"
  exec "let sep = " . s:GetVarForIter(a:iterName) . "_sep"
  if curIndex >= 0
    let ele = MvElementAt(array, sep, curIndex)
    exec "let " . s:GetVarForIter(a:iterName) . "_curIndex = " . (curIndex + 1)
  else
    let ele = ""
  endif
  return ele
endfunction


" Compares two elements based on the order of their appearance in the array.
"   Useful for sorting based on an MRU listing.
" Params:
"   ele1 - first element to be compared by position.
"   ele2 - second element to be compared by position.
"   direction - the direction of sort, used for determining the return value.
" Returns:
"   direction if ele2 comes before ele1 (for no swap), and 0 or -direction
"     otherwise (for swap).
function! MvCmpByPosition(array, sep, ele1, ele2, direction)
  "let array = a:sep.s:EnsureTrailingSeparator(a:array, a:sep)
  "if match(array, a:sep.a:ele1.'\('.a:sep.'.\{-}\)\{-}'.a:sep.a:ele2.a:sep)
  let strIndex1 = MvStrIndexOfElement(a:array, a:sep, a:ele1)
  let strIndex2 = MvStrIndexOfElement(a:array, a:sep, a:ele2)

  if (strIndex1 == -1) && (strIndex2 != -1)
    let strIndex1 = strIndex2 + a:direction
  elseif (strIndex1 != -1) && (strIndex2 == -1)
    let strIndex2 = strIndex1 + a:direction
  endif

  if strIndex1 < strIndex2
    return -a:direction
  elseif strIndex1 > strIndex2
    return a:direction
  else
    return 0
  endif
endfunction



" Useful function to prompt user for an element out of the passed in array. The
"   user will be prompted with a list of choices to make (formatted in a
"   single column, each corresponding to an element in the array. User can
"   enter the numer of the element or the element itself as a choice. Take a
"   look at the remcmd.vim script at vim.sf.net for an example usage. Because
"   of the internal implementation, there is a limit on what characters may be
"   contained in the array elements. Some characters that result in syntax
"   errors are single-quote, double-quote, semi-colon etc.
" Params:
"   default - The default value for the selection. Default can be the
"               element-index or the element itself.
"   msg - The message that should appear in the prompt (passed to input()).
"   skip - The element that needs to be skipped from selection (pass a
"            non-existent element to disable this, such as a empty value '').
"   useDialog - if true, uses dialogs for prompts, instead of the command-line(
"                 inputdialog() instead of input()). But personally, I don't
"                 like this because the power user then can't use the
"                 expression register.
" Returns:
"   the selected element or empty string, "" if nothing is selected.
"
function! MvPromptForElement(array, sep, default, msg, skip, useDialog)
  return MvPromptForElement2(a:array, a:sep, a:default, a:msg, a:skip,
	\ a:useDialog, 1)
endfunction

" Same as above MvPromptForElement, except that you can tell the number of
"   columns that you want the elements to be formatted into.
function! MvPromptForElement2(array, sep, default, msg, skip, useDialog, nCols)
  let cnt = 0
  let nCols = a:nCols
  let array{"length"} = 0
  " Create a hach from values to their index in the array.
  call MvIterCreate(a:array, a:sep, "MvPromptForElement")
  while MvIterHasNext("MvPromptForElement")
    let nextElement = MvIterNext("MvPromptForElement")

    " Don't show the skip element.
    if nextElement == a:skip
      continue
    endif

    let array{cnt} = nextElement
    let array{nextElement} = cnt
    let array{"length"} = array{"length"} + 1
    let cnt = cnt + 1
  endwhile
  call MvIterDestroy("MvPromptForElement")

  let index = 0
  let line = ""
  let element = ""
  let optionsMsg = ""
  let colWidth = &columns / nCols - 1
  let curCol = 1
  while index < array{"length"}
    let element = index . s:Spacer(4 - s:nDigits(index)) . array{index}
    let eleColWidth = (strlen(element) - 1) / colWidth + 1
    let element = element . s:Spacer(
	  \ eleColWidth * (colWidth + 1) - strlen(element) - 1)
    let wouldBeLength = strlen(line) + strlen(element) + 1
    if wouldBeLength > (curCol * (colWidth + eleColWidth)) &&
	  \ wouldBeLength > &columns
      let splitLine = 2 " Split before adding the new element.
    elseif curCol == nCols
      let splitLine = 1 " Split after adding the new element.
    else
      let splitLine = 0
    endif
    if splitLine == 2
      if strlen(line) == &columns
	" Remove the last space as it otherwise results in an extra empty line
	" on the screen.
	let line = strpart(line, 0, strlen(line) - 1)
      endif
      let optionsMsg = optionsMsg . line . "\n"
      let line = element . ' '
      let curCol = strlen(element) / (colWidth + 1)
    else
      let line = line . element . ' '
      if splitLine == 1
	if strlen(line) == &columns
	  " Remove the last space as it otherwise results in an extra empty line
	  " on the screen.
	  let line = strpart(line, 0, strlen(line) - 1)
	endif
	let curCol = 0 " Reset col count.
	let optionsMsg = optionsMsg . line . "\n"
	let line = ""
      endif
    endif
    let curCol = curCol + 1
    let index = index + 1
  endwhile

  if !exists("array" . a:default)
    let default = ""
  elseif a:default != "" && match(a:default, '^\d\+$') == -1
    let default = array{a:default}
  else
    let default = a:default
  endif

  while !exists("selectedElement")
    if a:useDialog
      let selection = inputdialog(optionsMsg . a:msg, default)
    else
      let selection = input(optionsMsg . a:msg, default)
    endif

    if selection == ""
      let selectedElement = ""
    elseif selection != "" && match(selection, '^\d\+$') != -1 &&
	  \ exists("array" . selection)
      let selectedElement = array{selection}
    elseif exists("array" . selection)
      let selectedElement = selection
    endif
    echo "\n"
  endwhile
  return selectedElement
endfunction


function! s:Spacer(width)
  return strpart("                                                            ",
	\ 0, a:width)
endfunction


function! s:nDigits(num)
  " I know log() can be used, but since log() is not available, what is the
  " other way finding this?
  return (a:num < 10)
	\ ? 1
	\ : (a:num < 100)
	\   ? 2
	\   : (a:num < 1000)
	\     ? 3
	\     : 4
endfunction

"
" --------------------------
"

function! s:GetVarForIter(iterName)
  if v:version < 600
    return "g:" . a:iterName
  else
    return "s:" . a:iterName
  endif
endfunction


" Make sure the array ha a trailing separator, returns the new array.
function! s:EnsureTrailingSeparator(array, sep)
  if strlen(a:array) == 0
    return a:array
  endif

  let exists = 1
  if ! s:HasTrailingSeparator(a:array, a:sep)
    let array = a:array . a:sep
  else
    let array = a:array
  endif
  return array
endfunction


function! s:HasTrailingSeparator(array, sep)
  return match(a:array, a:sep . '$') != -1
endfunction


function! s:IsRegularExpression(str)
  return match(a:str, '[.\\[\]{}*^$~]') != -1
endfunction


function! s:Escape(str)
  return escape(a:str, "\\.[^$~")
endfunction


"
"function! s:Assert(actual, expected, msg)
"  if a:actual != a:expected
"    call input("Failed: " . a:msg. ": actual: " . a:actual . " expected: " . a:expected)
"  endif
"endfunction
"
"function! MvTestPrintAllWithIter(array, sep)
"  let elementCount = 0
"  call MvIterCreate(a:array, a:sep, "MyIter")
"  while MvIterHasNext("MyIter")
"    call s:Assert(MvIterNext("MyIter"), elementCount+1, "MvIterNext with array: " . a:array . " and sep: " . a:sep . " for " . (elementCount+1))
"    let elementCount = elementCount + 1
"  endwhile
"  call MvIterDestroy("MyIter")
"endfunction
"
"function! MvRunTests()
"  call MvTestPrintAllWithIter("1,,2,,3,,4,,", ",,")
"  call MvTestPrintAllWithIter("1,,2,,3,,4", ",,")
"
"  "
"  " First test the read-only operations.
"  "
"  call s:Assert(MvStrIndexOfElement("1,,2,,3,,4,,", ",,", "3"), 6, "MvStrIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 3")
"  call s:Assert(MvStrIndexOfElement("1,,2,,3,,4", ",,", "4"), 9, "MvStrIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 4")
"  call s:Assert(MvStrIndexOfElement("1,,2,,3,,4,,", ",,", "1"), 0, "MvStrIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 1")
"  " Test a fix for a previous identified bug.
"  call s:Assert(MvStrIndexOfElement("11,,1,,2,,3,,", ",,", "1"), 4, "MvStrIndexOfElement with array: 11,,1,,2,,3,, sep: ,, for element 1")
"
"  call s:Assert(MvStrIndexOfElementAt("1,,2,,3,,4", ",,", 2), 6, "MvStrIndexOfElementAt with array: 1,,2,,3,,4,, sep: ,, for index 2")
"  call s:Assert(MvStrIndexOfElementAt("1,,2,,3,,4,,", ",,", 3), 9, "MvStrIndexOfElementAt with array: 1,,2,,3,,4,, sep: ,, for index 3")
"  call s:Assert(MvStrIndexOfElementAt("1,,2,,3,,4,,", ",,", 0), 0, "MvStrIndexOfElementAt with array: 1,,2,,3,,4,, sep: ,, for index 0")
"  call s:Assert(MvStrIndexOfElementAt("1,,", ",,", 0), 0, "MvStrIndexOfElementAt with array: 1,, sep: ,, for index 0")
"  call s:Assert(MvStrIndexOfElementAt("1", ",,", 0), 0, "MvStrIndexOfElementAt with array: 1 sep: ,, for index 0")
"
"  call s:Assert(MvElementAt("1,,2,,3,,4", ",,", 2), "3", "MvElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call s:Assert(MvElementAt("1,,2,,3,,4", ",,", 0), "1", "MvElementAt with array: 1,,2,,3,,4 sep: ,, for index 0")
"
"  call s:Assert(MvIndexOfElement("1,,2,,3,,4", ",,", "3"), 2, "MvIndexOfElement with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call s:Assert(MvIndexOfElement("1,,2,,3,,4,,", ",,", "1"), 0, "MvIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 0")
"
"  call s:Assert(MvNumberOfElements("1,,2,,3,,4", ",,"), 4, "MvNumberOfElements with array: 1,,2,,3,,4 sep: ,,")
"  call s:Assert(MvNumberOfElements("1,,2,,3,,4", ",,"), 4, "MvNumberOfElements with array: 1,,2,,3,,4 sep: ,,")
"  call s:Assert(MvNumberOfElements("1,,", ",,"), 1, "MvNumberOfElements with array: 1,, sep: ,,")
"  call s:Assert(MvNumberOfElements("1", ",,"), 1, "MvNumberOfElements with array: 1 sep: ,,")
"
"  call s:Assert(MvContainsElement("1,,2,,3,,4", ",,", "3"), 1, "MvContainsElement with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call s:Assert(MvContainsElement("1,,2,,3,,4,,", ",,", "1"), 1, "MvContainsElement with array: 1,,2,,3,,4,, sep: ,, for element 1")
"  call s:Assert(MvContainsElement("1,,2,,3,,4,,", ",,", "0"), 0, "MvContainsElement with array: 1,,2,,3,,4,, sep: ,, for element 0")
"
"  call s:Assert(MvLastElement("1,,2,,3,,4", ",,"), "4", "MvLastElement with array: 1,,2,,3,,4 sep: ,,")
"  call s:Assert(MvLastElement("1,,2,,3,,4,,", ",,"), "4", "MvLastElement with array: 1,,2,,3,,4,, sep: ,,")
"
"  "
"  " Now test the write operations.
"  "
"  call s:Assert(MvAddElement("1,,2,,3,,4", ",,", "5"), "1,,2,,3,,4,,5,,", "MvAddElement with array: 1,,2,,3,,4 sep: ,, for element 5")
"  call s:Assert(MvAddElement("1,,2,,3,,4,,", ",,", "5"), "1,,2,,3,,4,,5,,", "MvAddElement with array: 1,,2,,3,,4,, sep: ,, for element 5")
"
"  call s:Assert(MvRemoveElement("1,,2,,3,,4", ",,", "3"), "1,,2,,4,,", "MvRemoveElement with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call s:Assert(MvRemoveElement("1,,2,,3,,4,,", ",,", "1"), "2,,3,,4,,", "MvRemoveElement with array: 1,,2,,3,,4,, sep: ,, for element 1")
"
"  call s:Assert(MvRemoveElementAt("1,,2,,3,,4", ",,", 2), "1,,2,,4,,", "MvRemoveElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call s:Assert(MvRemoveElementAt("1,,2,,3,,4,,", ",,", 0), "2,,3,,4,,", "MvRemoveElementAt with array: 1,,2,,3,,4,, sep: ,, for index 0")
"
"  call s:Assert(MvPushToFront("1,,2,,3,,4", ",,", "3"), "3,,1,,2,,4,,", "MvPushToFront with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call s:Assert(MvPushToFront("1,,2,,3,,4,,", ",,", "4"), "4,,1,,2,,3,,", "MvPushToFront with array: 1,,2,,3,,4,, sep: ,, for element 4")
"
"  call s:Assert(MvPushToFrontElementAt("1,,2,,3,,4", ",,", 2), "3,,1,,2,,4,,", "MvPushToFrontElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call s:Assert(MvPushToFrontElementAt("1,,2,,3,,4,,", ",,", 3), "4,,1,,2,,3,,", "MvPushToFrontElementAt with array: 1,,2,,3,,4,, sep: ,, for index 3")
"
"  call s:Assert(MvPullToBack("1,,2,,3,,4", ",,", "3"), "1,,2,,4,,3,,", "MvPullToBack with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call s:Assert(MvPullToBack("1,,2,,3,,4,,", ",,", "1"), "2,,3,,4,,1,,", "MvPullToBack with array: 1,,2,,3,,4,, sep: ,, for element 1")
"
"  call s:Assert(MvPullToBackElementAt("1,,2,,3,,4", ",,", 2), "1,,2,,4,,3,,", "MvPullToBackElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call s:Assert(MvPullToBackElementAt("1,,2,,3,,4", ",,", 0), "2,,3,,4,,1,,", "MvPullToBackElementAt with array: 1,,2,,3,,4 sep: ,, for index 0")
"
"  call s:Assert(s:EnsureTrailingSeparator("1,,2,,3,,4,,", ",,"), "1,,2,,3,,4,,", "s:EnsureTrailingSeparator with array: 1,,2,,3,,4,, sep: ,,")
"  call s:Assert(s:EnsureTrailingSeparator("1,,2,,3,,4", ",,"), "1,,2,,3,,4,,", "s:EnsureTrailingSeparator with array: 1,,2,,3,,4 sep: ,,")
"
"  call s:Assert(MvInsertElementAt("1,,2,,3,,4", ",,", "5", 2), "1,,2,,5,,3,,4,,", "MvInsertElementAt with array: 1,,2,,3,,4 sep: ,, for element 5 at index 2")
"  call s:Assert(MvInsertElementAt("1,,2,,3,,4,,", ",,", "5", 0), "5,,1,,2,,3,,4,,", "MvInsertElementAt with array: 1,,2,,3,,4,, sep: ,, for element 5 at index 0")
"
"  call s:Assert(MvRotateLeftAt("1,,2,,3,,4", ",,", 1), "2,,3,,4,,1,,", "MvRotateLeftAt with array: 1,,2,,3,,4 sep: ,, at index 1")
"  call s:Assert(MvRotateLeftAt("1,,2,,3,,4", ",,", 0), "1,,2,,3,,4", "MvRotateLeftAt with array: 1,,2,,3,,4 sep: ,, at index 0")
"  call s:Assert(MvRotateLeftAt("1,,2,,3,,4", ",,", 3), "4,,1,,2,,3,,", "MvRotateLeftAt with array: 1,,2,,3,,4 sep: ,, at index 3")
"  call s:Assert(MvRotateLeftAt("1,,2,,3,,4", ",,", 4), "1,,2,,3,,4,,", "MvRotateLeftAt with array: 1,,2,,3,,4 sep: ,, at index 4")
"
"  call s:Assert(MvRotateRightAt("1,,2,,3,,4", ",,", 1), "3,,4,,1,,2,,", "MvRotateRightAt with array: 1,,2,,3,,4 sep: ,, at index 1")
"  call s:Assert(MvRotateRightAt("1,,2,,3,,4", ",,", 0), "2,,3,,4,,1,,", "MvRotateRightAt with array: 1,,2,,3,,4 sep: ,, at index 0")
"  call s:Assert(MvRotateRightAt("1,,2,,3,,4", ",,", 3), "1,,2,,3,,4,,", "MvRotateRightAt with array: 1,,2,,3,,4 sep: ,, at index 3")
"  call s:Assert(MvRotateRightAt("1,,2,,3,,4", ",,", 4), "1,,2,,3,,4,,", "MvRotateRightAt with array: 1,,2,,3,,4 sep: ,, at index 4")
"
"
"  call s:Assert(MvPromptForElement("a,,b,,c,,d,,", ",,", "c", "Please press Enter:", "", 0), "c", "MvPromptForElement with array a,,b,,c,,d,, for default element c")
"  call s:Assert(MvPromptForElement2("a,,b,,c,,d,,", ",,", 1, "Please press Enter:", "", 0, 2), "b", "MvPromptForElement with array a,,b,,c,,d,, for default index 1")
"
"  "
"  " Test read-only operations using a regex-pattern as a separator.
"  "
"
"  call s:Assert(MvNumberOfElements("1xxxx2xxx3x4xxxx", 'x\+'), 4, "MvNumberOfElements with array: 1xxxx2xxx3x4xxxx")
"  call s:Assert(MvNumberOfElements("1xxxx2xxx3x4", 'x\+'), 4, "MvNumberOfElements with array: 1xxxx2xxx3x4")
"  call s:Assert(MvNumberOfElements("1xxxx", 'x\+'), 1, "MvNumberOfElements with array: 1xxxx")
"  call s:Assert(MvNumberOfElements("1", 'x\+'), 1, "MvNumberOfElements with array: 1")
"
"  call s:Assert(MvStrIndexOfElement("1xxxx2xxx3x4xxxx", 'x\+', "3"), 9, "MvStrIndexOfElement with array: 1xxxx2xxx3x4xxxx for element 3")
"  call s:Assert(MvStrIndexOfElement("1xxxx2xxx3x4", 'x\+', "3"), 9, "MvStrIndexOfElement with array: 1xxxx2xxx3x4 for element 3")
"  call s:Assert(MvStrIndexOfElement("1xxxx2xxx3x4", 'x\+', "4"), 11, "MvStrIndexOfElement with array: 1xxxx2xxx3x4 for element 4")
"  call s:Assert(MvStrIndexOfElement("1xxxx2xxx3x4", 'x\+', "1"), 0, "MvStrIndexOfElement with array: 1xxxx2xxx3x4 for element 1")
"  call s:Assert(MvStrIndexOfElement("1xxxx", 'x\+', "1"), 0, "MvStrIndexOfElement with array: 1xxxx for element 1")
"  call s:Assert(MvStrIndexOfElement("1", 'x\+', "1"), 0, "MvStrIndexOfElement with array: 1 for element 1")
"
"  call s:Assert(MvStrIndexOfElementAt("1xxxx2xxx3x4xxxx", 'x\+', 2), 9, "MvStrIndexOfElementAt with array: 1xxxx2xxx3x4xxxx for index 2")
"  call s:Assert(MvStrIndexOfElementAt("1xxxx2xxx3x4xxxx", 'x\+', 0), 0, "MvStrIndexOfElementAt with array: 1xxxx2xxx3x4xxxx for index 1")
"  call s:Assert(MvStrIndexOfElementAt("1xxxx2xxx3x4xxxx", 'x\+', 3), 11, "MvStrIndexOfElementAt with array: 1xxxx2xxx3x4xxxx for index 3")
"  call s:Assert(MvStrIndexOfElementAt("1xxxx", 'x\+', 0), 0, "MvStrIndexOfElementAt with array: 1xxxx for index 0")
"  call s:Assert(MvStrIndexOfElementAt("1", 'x\+', 0), 0, "MvStrIndexOfElementAt with array: 1 for index 0")
"
"  call s:Assert(MvElementAt("1xxxx2xxx3x4xxxx", 'x\+', 2), "3", "MvElementAt with array: 1xxxx2xxx3x4xxxx for index 2")
"  call s:Assert(MvElementAt("1xxxx2xxx3x4", 'x\+', 0), "1", "MvElementAt with array: 1xxxx2xxx3x4 for index 0")
"  call s:Assert(MvElementAt("1xxxx", 'x\+', 0), "1", "MvElementAt with array: 1xxxx for index 0")
"  call s:Assert(MvElementAt("1", 'x\+', 0), "1", "MvElementAt with array: 1 for index 0")
"
"  call s:Assert(MvIndexOfElement("1xxxx2xxx3x4xxxx", 'x\+', "3"), 2, "MvIndexOfElement with array: 1xxxx2xxx3x4xxxx for element 3")
"  call s:Assert(MvIndexOfElement("1xxxx2xxx3x4", 'x\+', "4"), 3, "MvIndexOfElement with array: 1xxxx2xxx3x4 for element 4")
"  call s:Assert(MvIndexOfElement("1xxxx", 'x\+', "1"), 0, "MvIndexOfElement with array: 1xxxx for element 1")
"  call s:Assert(MvIndexOfElement("1", 'x\+', "1"), 0, "MvIndexOfElement with array: 1 for element 1")
"
"  call s:Assert(MvContainsElement("1xxxx2xxx3x4xxxx", 'x\+', "3"), 1, "MvContainsElement with array: 1xxxx2xxx3x4xxxx for element 3")
"  call s:Assert(MvContainsElement("1xxxx2xxx3x4", 'x\+', "4"), 1, "MvContainsElement with array: 1xxxx2xxx3x4 for element 4")
"  call s:Assert(MvContainsElement("1xxxx", 'x\+', "1"), 1, "MvContainsElement with array: 1xxxx for element 1")
"  call s:Assert(MvContainsElement("1", 'x\+', "1"), 1, "MvContainsElement with array: 1 for element 1")
"
"  call s:Assert(MvLastElement("1xxxx2xxx3x4xxxx", 'x\+'), "4", "MvLastElement with array: 1xxxx2xxx3x4xxxx")
"  call s:Assert(MvLastElement("1xxxx2xxx3x4", 'x\+'), "4", "MvLastElement with array: 1xxxx2xxx3x4")
"  call s:Assert(MvLastElement("1xxxx", 'x\+'), "1", "MvLastElement with array: 1xxxx")
"  call s:Assert(MvLastElement("1", 'x\+'), "1", "MvLastElement with array: 1")
"
"  call s:Assert(MvSwapElementsAt("1,2,3,4,5", ',', 1, 3), "1,4,3,2,5,", "MvSwapElementsAt with array: 1,2,3,4,5 for elements: 1 and 3")
"  call s:Assert(MvSwapElementsAt("1,2,3,4,5", ',', 3, 1), "1,4,3,2,5,", "MvSwapElementsAt with array: 1,2,3,4,5 for elements: 3 and 1")
"  call s:Assert(MvSwapElementsAt("1,2,3,4,5", ',', 0, 3), "4,2,3,1,5,", "MvSwapElementsAt with array: 1,2,3,4,5 for elements: 0 and 3")
"  call s:Assert(MvSwapElementsAt("1,2,3,4,5", ',', 1, 4), "1,5,3,4,2,", "MvSwapElementsAt with array: 1,2,3,4,5 for elements: 1 and 4")
"  call s:Assert(MvSwapElementsAt("1,2,3,4,5", ',', 3, 3), "1,2,3,4,5", "MvSwapElementsAt with array: 1,2,3,4,5 for elements: 3 and 3")
"  call s:Assert(MvSwapElementsAt("1,2,3,4,5", ',', 3, 4), "1,2,3,5,4", "MvSwapElementsAt with array: 1,2,3,4,5 for elements: 3 and 4")
"endfunction
