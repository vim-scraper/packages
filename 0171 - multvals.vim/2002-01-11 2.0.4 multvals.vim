" multvals.vim -- Array operations on Vim multi-values, or just another array.
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Modified: 11-Jan-2002 @ 19:54
" Requires: Vim-5.6 or higher
" Version: 2.0.4
" Environment:
"   Adds
"       MvAddElement
"       MvInsertElementAt
"       MvRemoveElement
"       MvRemoveElementAt
"       MvNumberOfElements
"       MvStrIndexOfElement
"       MvStrIndexOfElementAt
"       MvIndexOfElement
"       MvContainsElement
"       MvElementAt
"       MvLastElement
"       MvPushToFront
"       MvPushToFrontElementAt
"       MvPullToBack
"       MvPullToBackElementAt
"       MvIterCreate
"       MvIterDestroy
"       MvIterHasNext
"       MvIterNext
"       MvCmpByPosition
"       MvPromptForElement
"     global functions.
"
" Usage:
"   An array is nothing but a string of multiple values separated by a pattern.
"     The simplest example being Vim's multi-value variables such as tags. You
"     can use the MvAddElement() function to create an array. However, there is
"     nothing special about this function, you can as well make up the string
"     by simply concatinating elements with the chosen pattern as a separator.
"   Ex Usage:
"     call MvIterCreate(&tags, ",", "Tags")
"     while MvIterHasNext("Tags")
"       call input("Next element: " . MvIterNext("Tags"))
"     endwhile
"     call MvIterDestroy("Tags")
"
" Changes in 2.0.3:
"   - New functions were added.
"   - The order of arguments for MvIterCreate has been changed for the sake of
"       consistency.
"   - Prefixed all the global functions with "Mv" to avoid global name
"       conflicts.
"
"
" ALMOST ALL OPERATIONS TAKE THE ARRAY AND THE SEPARATOR AS THE FIRST TWO
"   ARGUMENTS.
" All element-indexes start from 0 (like in C or Java).
" All string-indexes start from 0 (as it is for Vim built-in functions).
"
"
"


" Adds an element and returns the new array.
" Params:
"   ele - Element to be added to the array.
" Returns:
"   the new array.
function! MvAddElement(array, sep, ele)
  let array = EnsureTrailingSeparator(a:array, a:sep)
  return array . a:ele . a:sep
endfunction


" Insert the element before index and return the new array. Index starts from 0.
" Params:
"   ele - Element to be inserted into the array.
"   index - The index before which the element should be inserted.
" Returns:
"   the new array.
function! MvInsertElementAt(array, sep, ele, index)
  let array = EnsureTrailingSeparator(a:array, a:sep)
  if a:index == 0
    return a:ele . a:sep . array
  else
    let strIndex = MvStrIndexOfElementAt(array, a:sep, a:index)
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
  let array = EnsureTrailingSeparator(a:array, a:sep)
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
  let array = EnsureTrailingSeparator(a:array, a:sep)
  let strIndex = MvStrIndexOfElementAt(array, a:sep, a:index)
  let sub1 = strpart(array, 0, strIndex)
  let sub2 = strpart(array, strIndex, strlen(array))
  let ind2 = match(sub2, a:sep)
  if ind2 < 0
    return array
  else
    let sub2 = strpart(sub2, ind2 + strlen(a:sep), strlen(sub2) - ind2 - strlen(a:sep))
  endif
  return sub1 . sub2
endfunction


" Returns the number of elements in the array.
" Returns:
"   the number of elements that are present in the array.
function! MvNumberOfElements(array, sep)
  let array = EnsureTrailingSeparator(a:array, a:sep)
  let pat = '\(.\)\{-}\(' . a:sep . '\)\{-1\}'

  " Remove everything except the separators and count the number of separators.
  let mod = substitute(array, pat, '\2', 'g')
  return strlen(mod)/strlen(a:sep)
endfunction


" Returns the string-index of the element in the array, which can be used with
"   string manipulation functions such as strpart().
" Params:
"   ele - Element whose string-index is to be found.
" Returns:
"   the string index of the element, starts from 0.
function! MvStrIndexOfElement(array, sep, ele)
  let index = match(a:array, a:ele.a:sep)
  " Take care of missing separator at the end.
  if index == -1
    let index = match(a:array, a:ele."$")
  endif
  return index
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
  let strIndex = MvStrIndexOfElement(a:array, a:sep, prevEle)
  return strIndex + strlen(prevEle) + strlen(a:sep)
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
  let sub = ""
  if a:index < 0
    return sub
  endif
  let index = a:index + 1
  let array = EnsureTrailingSeparator(a:array, a:sep)

  let pat1 = '\(\(.\{-}' . a:sep . '\)\{' . index . '}\).*$'
  let sub1 = substitute(array, pat1, '\1','')
  if strlen(sub1) != 0 && index > 1
    let pat2 = '\(\(.\{-}' . a:sep . '\)\{' . (index - 1) . '}\).*$'
    let sub2 = substitute(sub1, pat2, '\1','')
    if strlen(sub2) != 0
      let sub = strpart(sub1, strlen(sub2),
              \ strlen(sub1) - strlen(sub2) - strlen(a:sep))
    endif
  else
    let sub = strpart(sub1, 0, strlen(sub1) - strlen(a:sep))
  endif
  return sub
endfunction


" Returns the last element in the array.
" Returns:
"   the last element in the array.
function! MvLastElement(array, sep)
  let pat = '^.*\(.\+' . a:sep . '\)\{-1}$'
  let array = EnsureTrailingSeparator(a:array, a:sep)
  let sub = substitute(array, pat, '\1','')
  if strlen(sub) > 0
    let sub = strpart(sub, 0, strlen(sub) - strlen(a:sep))
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
  let array = EnsureTrailingSeparator(MvRemoveElement(a:array, a:sep, a:ele),
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


" Creates a new iterator with the given name. This can be passed to
"   MvIterHasNext() and MvIterNext() to iterate over elements. Call MvIterDestroy()
"   to remove the space occupied by this iterator.
" Do not modify the array while using the iterator.
" Params:
"   iterName - A unique name that is used to identify this iterator. The
"                storage is alloted in the script name space (for Vim 6.0 or
"                above) or in the global name space (for previous Vim versions).
function! MvIterCreate(array, sep, iterName)
  exec "let " . GetVarForIter(a:iterName) . "_prevIndex = 0"
  exec "let " . GetVarForIter(a:iterName) . "_array = a:array"
  exec "let " . GetVarForIter(a:iterName) . "_sep = a:sep"
endfunction


" Deallocates the space occupied by this iterator.
" Params:
"   iterName - The name of the iterator to be destroyed that was previously
"                created using MvIterCreate.
function! MvIterDestroy(iterName)
  exec "unlet " . GetVarForIter(a:iterName) . "_prevIndex"
  exec "unlet " . GetVarForIter(a:iterName) . "_array"
  exec "unlet " . GetVarForIter(a:iterName) . "_sep"
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
  if ! exists(GetVarForIter(a:iterName) . "_prevIndex")
    return 0
  endif

  exec "let prevIndex = " . GetVarForIter(a:iterName) . "_prevIndex"
  if prevIndex >= 0
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
  if ! exists(GetVarForIter(a:iterName) . "_prevIndex")
    return ""
  endif

  exec "let prevIndex = " . GetVarForIter(a:iterName) . "_prevIndex"
  exec "let array = " . GetVarForIter(a:iterName) . "_array"
  exec "let sep = " . GetVarForIter(a:iterName) . "_sep"
  if prevIndex >= 0
    let ele = NextElement(array, sep, prevIndex)
    exec "let " . GetVarForIter(a:iterName) . "_prevIndex = " .
        \ NextIndex(array, sep, prevIndex + 1)
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
"   direction if ele2 comes before ele1, and -direction otherwise.
function! MvCmpByPosition(array, sep, ele1, ele2, direction)
  let strIndex1 = MvStrIndexOfElement(a:array, a:sep, a:ele1)
  let strIndex2 = MvStrIndexOfElement(a:array, a:sep, a:ele2)

  if (strIndex1 == -1) && (strIndex2 != -1)
    let strIndex1 = strIndex2 + a:direction
  elseif (strIndex2 == -1) && (strIndex2 != -1)
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
"   user will be prompted with a list of choices to make, each corresponding to
"   an element in the array. User can enter the numer of the element or the
"   element itself as a choice. Take a look at the remcmd.vim script at
"   vim.sf.net for an example usage.
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
  let cnt = 0
  let optionsMsg = ""
  let array{"length"} = 0
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
  while index < array{"length"}
    let optionsMsg = optionsMsg . index . "        " . array{index} ."\n"
    let index = index + 1
  endwhile

  if !exists("array" . a:default)
    let default = ""
  elseif match(a:default, '^\d\+$') == -1
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
    elseif match(selection, '^\d\+$') != -1 && exists("array" . selection)
      let selectedElement = array{selection}
    elseif exists("array" . selection)
      let selectedElement = selection
    endif
    echo "\n"
  endwhile
  return selectedElement
endfunction

"
" --------------------------
"

" This is a low level functions. Use iterators and other functions defined
"   above for convenience.
" Returns the next index after the specified index.
" Returns a value less than 0 if there is no next index.
function! NextIndex(array, sep, startPos)
  " Because match() behaves this way.
  if a:startPos < 0
    let startPos = 0
  else
    let startPos = a:startPos
  endif
  if v:version < 600
    let remainingString = strpart(a:array, a:startPos, strlen(a:array))
    let index = match(remainingString, a:sep)
    if index != -1
      let index = index + a:startPos
    endif
  else
    let index = match(a:array, a:sep, startPos)
  endif
  " Take care of an excess separator at the end.
  if index != -1 && (index + strlen(a:sep)) == strlen(a:array)
    let index = -2
  endif
  return index
endfunction 


" This is a low level functions. Use iterators and other functions defined
"   above for convenience.
" Returns the next element after the specified index.
function! NextElement(array, sep, prevIndex)
  if a:prevIndex == 0
    let nextPos =  a:prevIndex
  else
    let nextPos =  a:prevIndex + strlen(a:sep)
  endif
  let nextIndex = NextIndex(a:array, a:sep, nextPos)
  if nextIndex == -2
    let nextIndex = strlen(a:array) - strlen(a:sep)
  " Last element may not have a following separator.
  elseif nextIndex == -1 && (a:prevIndex + strlen(a:sep) < strlen(a:array))
      let nextIndex = strlen(a:array)
  endif
  if nextIndex != -1
    return strpart(a:array, nextPos, (nextIndex - nextPos))
  endif
endfunction


function! GetVarForIter(iterName)
  if v:version < 600
    return "g:" . a:iterName
  else
    return "s:" . a:iterName
  endif
endfunction


" Make sure the array ha a trailing separator, returns the new array.
function! EnsureTrailingSeparator(array, sep)
  if strlen(a:array) == 0
    return a:array
  endif

  let exists = 1
  if match(a:array, a:sep . '$') == -1
    let array = a:array . a:sep
  else
    let array = a:array
  endif
  return array
endfunction


" Test functions.
"function! TestPrintAll(array, sep)
"  let prevIndex = 0
"  let elementCount = 0
"  while prevIndex >= 0
"    call Assert(NextElement(a:array, a:sep, prevIndex), elementCount+1, "NextElement with array: " . a:array . " and sep: " . a:sep . " for " . (elementCount+1))
"    let prevIndex = NextIndex(a:array, a:sep, prevIndex + 1)
"    let elementCount = elementCount + 1
"  endwhile
"endfunction
"
"
"function! TestPrintAllWithIter(array, sep)
"  let elementCount = 0
"  call MvIterCreate(a:array, a:sep, "MyIter")
"  while MvIterHasNext("MyIter")
"    call Assert(MvIterNext("MyIter"), elementCount+1, "NextElement with array: " . a:array . " and sep: " . a:sep . " for " . (elementCount+1))
"    let elementCount = elementCount + 1
"  endwhile
"  call MvIterDestroy("MyIter")
"endfunction
"
"function! RunTests()
"  call TestPrintAll("1,2,3,4,", ",")
"  call TestPrintAll("1,2,3,4", ",")
"  call TestPrintAllWithIter("1,,2,,3,,4,,", ",,")
"  call TestPrintAllWithIter("1,,2,,3,,4", ",,")
"
"  call Assert(MvStrIndexOfElement("1,,2,,3,,4,,", ",,", "3"), 6, "MvStrIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 3")
"  call Assert(MvStrIndexOfElement("1,,2,,3,,4,,", ",,", "4"), 9, "MvStrIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 4")
"
"  call Assert(MvStrIndexOfElementAt("1,,2,,3,,4", ",,", 2), 6, "MvStrIndexOfElementAt with array: 1,,2,,3,,4,, sep: ,, for index 2")
"  call Assert(MvStrIndexOfElementAt("1,,2,,3,,4,,", ",,", 3), 9, "MvStrIndexOfElementAt with array: 1,,2,,3,,4,, sep: ,, for index 3")
"  call Assert(MvStrIndexOfElementAt("1,,2,,3,,4,,", ",,", 0), 0, "MvStrIndexOfElementAt with array: 1,,2,,3,,4,, sep: ,, for index 0")
"
"  call Assert(MvAddElement("1,,2,,3,,4", ",,", "5"), "1,,2,,3,,4,,5,,", "MvAddElement with array: 1,,2,,3,,4 sep: ,, for element 5")
"  call Assert(MvAddElement("1,,2,,3,,4,,", ",,", "5"), "1,,2,,3,,4,,5,,", "MvAddElement with array: 1,,2,,3,,4,, sep: ,, for element 5")
"
"  call Assert(MvRemoveElement("1,,2,,3,,4", ",,", "3"), "1,,2,,4,,", "MvRemoveElement with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call Assert(MvRemoveElement("1,,2,,3,,4,,", ",,", "1"), "2,,3,,4,,", "MvRemoveElement with array: 1,,2,,3,,4,, sep: ,, for element 1")
"
"  call Assert(MvRemoveElementAt("1,,2,,3,,4", ",,", 2), "1,,2,,4,,", "MvRemoveElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call Assert(MvRemoveElementAt("1,,2,,3,,4,,", ",,", 0), "2,,3,,4,,", "MvRemoveElementAt with array: 1,,2,,3,,4,, sep: ,, for index 0")
"
"  call Assert(MvElementAt("1,,2,,3,,4", ",,", 2), "3", "MvElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call Assert(MvElementAt("1,,2,,3,,4", ",,", 0), "1", "MvElementAt with array: 1,,2,,3,,4 sep: ,, for index 0")
"
"  call Assert(MvPushToFront("1,,2,,3,,4", ",,", "3"), "3,,1,,2,,4,,", "MvPushToFront with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call Assert(MvPushToFront("1,,2,,3,,4,,", ",,", "4"), "4,,1,,2,,3,,", "MvPushToFront with array: 1,,2,,3,,4,, sep: ,, for element 4")
"
"  call Assert(MvPushToFrontElementAt("1,,2,,3,,4", ",,", 2), "3,,1,,2,,4,,", "MvPushToFrontElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call Assert(MvPushToFrontElementAt("1,,2,,3,,4,,", ",,", 3), "4,,1,,2,,3,,", "MvPushToFrontElementAt with array: 1,,2,,3,,4,, sep: ,, for index 3")
"
"  call Assert(MvPullToBack("1,,2,,3,,4", ",,", "3"), "1,,2,,4,,3,,", "MvPullToBack with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call Assert(MvPullToBack("1,,2,,3,,4,,", ",,", "1"), "2,,3,,4,,1,,", "MvPullToBack with array: 1,,2,,3,,4,, sep: ,, for element 1")
"
"  call Assert(MvPullToBackElementAt("1,,2,,3,,4", ",,", 2), "1,,2,,4,,3,,", "MvPullToBackElementAt with array: 1,,2,,3,,4 sep: ,, for index 2")
"  call Assert(MvPullToBackElementAt("1,,2,,3,,4", ",,", 0), "2,,3,,4,,1,,", "MvPullToBackElementAt with array: 1,,2,,3,,4 sep: ,, for index 0")
"
"  call Assert(EnsureTrailingSeparator("1,,2,,3,,4,,", ",,"), "1,,2,,3,,4,,", "EnsureTrailingSeparator with array: 1,,2,,3,,4,, sep: ,,")
"  call Assert(EnsureTrailingSeparator("1,,2,,3,,4", ",,"), "1,,2,,3,,4,,", "EnsureTrailingSeparator with array: 1,,2,,3,,4 sep: ,,")
"
"  call Assert(MvIndexOfElement("1,,2,,3,,4", ",,", "3"), 2, "MvIndexOfElement with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call Assert(MvIndexOfElement("1,,2,,3,,4,,", ",,", "1"), 0, "MvIndexOfElement with array: 1,,2,,3,,4,, sep: ,, for element 0")
"
"  call Assert(MvInsertElementAt("1,,2,,3,,4", ",,", "5", 2), "1,,2,,5,,3,,4,,", "MvInsertElementAt with array: 1,,2,,3,,4 sep: ,, for element 5 at index 2")
"  call Assert(MvInsertElementAt("1,,2,,3,,4,,", ",,", "5", 0), "5,,1,,2,,3,,4,,", "MvInsertElementAt with array: 1,,2,,3,,4,, sep: ,, for element 5 at index 0")
"
"  call Assert(MvNumberOfElements("1,,2,,3,,4", ",,"), 4, "MvNumberOfElements with array: 1,,2,,3,,4 sep: ,,")
"  call Assert(MvNumberOfElements("1,,2,,3,,4,,", ",,"), 4, "MvNumberOfElements with array: 1,,2,,3,,4,, sep: ,,")
"
"  call Assert(MvContainsElement("1,,2,,3,,4", ",,", "3"), 1, "MvContainsElement with array: 1,,2,,3,,4 sep: ,, for element 3")
"  call Assert(MvContainsElement("1,,2,,3,,4,,", ",,", "1"), 1, "MvContainsElement with array: 1,,2,,3,,4,, sep: ,, for element 1")
"  call Assert(MvContainsElement("1,,2,,3,,4,,", ",,", "0"), 0, "MvContainsElement with array: 1,,2,,3,,4,, sep: ,, for element 0")
"
"  call Assert(MvLastElement("1,,2,,3,,4", ",,"), "4", "MvLastElement with array: 1,,2,,3,,4 sep: ,,")
"  call Assert(MvLastElement("1,,2,,3,,4,,", ",,"), "4", "MvLastElement with array: 1,,2,,3,,4,, sep: ,,")
"
"  call Assert(MvPromptForElement("a,,b,,c,,d,,", ",,", "c", "Please press Enter:", "", 0), "c", "MvPromptForElement with array a,,b,,c,,d,, for default element c")
"  call Assert(MvPromptForElement("a,,b,,c,,d,,", ",,", 1, "Please press Enter:", "", 0), "b", "MvPromptForElement with array a,,b,,c,,d,, for default index 1")
"endfunction
"
"function! Assert(actual, expected, msg)
"  if a:actual != a:expected
"    call input("Failed: " . a:msg. ": actual: " . a:actual . " expected: " . a:expected)
"  endif
"endfunction
