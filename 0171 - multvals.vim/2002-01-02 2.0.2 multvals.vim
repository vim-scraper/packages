" multvals.vim -- Array operations on Vim multi-values, or just another array.
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Modified: 28-Dec-2001 @ 14:19
" Requires: Vim-5.6 or higher
" Version: 2.0.2
"
" Usage:
"   An array is nothing but a string of multiple values separated by a pattern.
"     The simplest example being Vim's multi-value variables such as tags. You
"     can use the AddElement() function to create an array. However, there is
"     nothing special about this function, you can as well make up the string
"     by simply concatinating elements with the chosen pattern as a separator.
"   Ex Usage:
"     call IterCreate("Tags", &tags, ",")
"     while IterHasNext("Tags")
"       call input("Next element: " . IterNext("Tags"))
"     endwhile
"     call IterDestroy("Tags")
"
" TODO:
"   There should be a clear distinction between the string index and
"     the element index. But fornuately only the IndexOfElement() uses element
"     index and all the remaining methods use string index.
"


" Adds an element and returns the new array.
function! AddElement(array, sep, ele)
  let array = EnsureTrailingSeparator(a:array, a:sep)
  return array . a:ele . a:sep
endfunction


" Removes the element and returns the new array.
function! RemoveElement(array, sep, ele)
  let index = IndexOfElement(a:array, a:sep, a:ele)
  " First remove this element.
  if index != -1
    let array = strpart(a:array, 0, index)
    let array = array . strpart(a:array, index + strlen(a:sep) + strlen(a:ele),
            \ strlen(a:array))
  else
    let array = a:array
  endif
  return array
endfunction


" Returns the index of the element in the array, which can be used with string
"   manipulation functions such as strpart().
function! IndexOfElement(array, sep, ele)
  let index = match(a:array, a:ele.a:sep)
  " Take care of missing separator at the end.
  if index == -1
    let index = match(a:array, a:ele."$")
  endif
  return index
endfunction


" Returns 1 (for true) if the element is contained in the array and 0 (for
"   false) if not
function! ContainsElement(array, sep, ele)
  if IndexOfElement(a:array, a:sep, a:ele) >= 0
    return 1
  else
    return 0
  endif
endfunction


" Returns the indexth element in the array. The index starts from 0.
" TODO: Is there a better way of doing this?
function! ElementAt(array, sep, index)
  let eleCount = 0
  call IterCreate("MyIter", a:array, a:sep)
  let ele = ""
  while IterHasNext("MyIter")
    let ele = IterNext("MyIter")
    if eleCount == a:index
      break
    endif
    let eleCount = eleCount + 1
  endwhile
  call IterDestroy("MyIter")
  return ele
endfunction


" Moves the element to the front of the array and returns the new array.
function! PushToFront(array, sep, ele)
  let array = RemoveElement(a:array, a:sep, a:ele)
  let array = a:ele . a:sep . array
  return array
endfunction


" Moves the element to the back of the array and returns the new array.
function! PullToBack(array, sep, ele)
  let array = EnsureTrailingSeparator(RemoveElement(a:array, a:sep, a:ele),
        \ a:sep)
  let array = array . a:ele . a:sep
  return array
endfunction


" Creates a new iterator with the given name. This can be passed to
"   IterHasNext() and IterNext() to iterate over elements. Call IterDestroy()
"   to remove the space occupied by this iterator.
" Do not modify the array while using the iterator.
function! IterCreate(iterName, array, sep)
  exec "let " . GetVarForIter(a:iterName) . "_prevIndex = 0"
  exec "let " . GetVarForIter(a:iterName) . "_array = a:array"
  exec "let " . GetVarForIter(a:iterName) . "_sep = a:sep"
endfunction


" Deallocates the space occupied by this iterator.
function! IterDestroy(iterName)
  exec "unlet " . GetVarForIter(a:iterName) . "_prevIndex"
  exec "unlet " . GetVarForIter(a:iterName) . "_array"
  exec "unlet " . GetVarForIter(a:iterName) . "_sep"
endfunction


" Returns 1 (for true) if has next value or 0 (for false).
" Do not modify the array while using the iterator.
function! IterHasNext(iterName)
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


" Returns next value or "" if none. You should always call IterHasNext() before
"   calling this function.
" Do not modify the array while using the iterator.
function! IterNext(iterName)
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


" For sorting based on the element positions. Useful to sort based on an MRU
"   listing.
" Returns direction if ele2 comes before ele1, and -direction otherwise.
function! CmpByPosition(array, sep, ele1, ele2, direction)
  let index1 = IndexOfElement(a:array, a:sep, a:ele1)
  let index2 = IndexOfElement(a:array, a:sep, a:ele2)

  if (index1 == -1) && (index2 != -1)
    let index1 = index2 + a:direction
  elseif (index2 == -1) && (index2 != -1)
    let index2 = index1 + a:direction
  endif

  if index1 < index2
    return -a:direction
  elseif index1 > index2
    return a:direction
  else
    return 0
  endif
endfunction



"
" Useful function to prompt user for an element.
" The default can be the index in the array or one of the elements.
" The msg is what is passed to input().
" The skip is one of the elements that needs to be skipped (pass a non-existent
"   element to disable this).
"
function! PromptForElement(array, sep, default, msg, skip, useDialog)
  let cnt = 0
  let optionsMsg = ""
  let array{"length"} = 0
  call IterCreate("PromptForElement", a:array, a:sep)
  while IterHasNext("PromptForElement")
    let nextElement = IterNext("PromptForElement")

    " Don't show the skip element.
    if nextElement == a:skip
      continue
    endif

    let array{cnt} = nextElement
    let array{nextElement} = cnt
    let array{"length"} = array{"length"} + 1
    let cnt = cnt + 1
  endwhile
  call IterDestroy("PromptForElement")

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
  endwhile
  return selectedElement
endfunction


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
  let exists = 1
  if match(a:array, a:sep . '$') == -1
    let array = a:array . a:sep
  else
    let array = a:array
  endif
  return array
endfunction


" Test function.
function! PrintAll(array, sep)
  let prevIndex = 0
  let elementCount = 0
  while prevIndex >= 0
    call input("element (" . elementCount . ") = " . NextElement(a:array, a:sep, prevIndex))
    let prevIndex = NextIndex(a:array, a:sep, prevIndex + 1)
    let elementCount = elementCount + 1
  endwhile
endfunction


function! PrintAllWithIter(array, sep)
  let elementCount = 0
  call IterCreate("MyIter", a:array, a:sep)
  while IterHasNext("MyIter")
    call input("element (" . elementCount . ") = " . IterNext("MyIter"))
    let elementCount = elementCount + 1
  endwhile
  call IterDestroy("MyIter")
endfunction


function! RunTests()
  call input("Calling PrintAll with array: " . "1,2,3,4," . " sep: " . ",")
  call PrintAll("1,2,3,4,", ",")
  call input("Calling PrintAll with array: " . "1,2,3,4" . " sep: " . ",")
  call PrintAll("1,2,3,4", ",")
  call input("Calling PrintAllWithIter with array: " . "1,,2,,3,,4,," . " sep: " . ",,")
  call PrintAllWithIter("1,,2,,3,,4,,", ",,")
  call input("Calling PrintAllWithIter with array: " . "1,,2,,3,,4" . " sep: " . ",,")
  call PrintAllWithIter("1,,2,,3,,4", ",,")

  call input("IndexOfElement with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 3: " . IndexOfElement("1,,2,,3,,4,,", ",,", "3"))
  call input("IndexOfElement with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 4: " . IndexOfElement("1,,2,,3,,4,,", ",,", "4"))

  call input("AddElement with array: " . "1,,2,,3,,4" . " sep: " . ",," . " for 5: " . AddElement("1,,2,,3,,4", ",,", "5"))
  call input("AddElement with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 5: " . AddElement("1,,2,,3,,4,,", ",,", "5"))

  call input("RemoveElement with array: " . "1,,2,,3,,4" . " sep: " . ",," . " for 3: " . RemoveElement("1,,2,,3,,4", ",,", "3"))
  call input("RemoveElement with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 3: " . RemoveElement("1,,2,,3,,4,,", ",,", "3"))

  call input("ElementAt with array: " . "1,,2,,3,,4" . " sep: " . ",," . " for 2: " . ElementAt("1,,2,,3,,4", ",,", "2"))
  call input("ElementAt with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 2: " . ElementAt("1,,2,,3,,4,,", ",,", "2"))

  call input("PushToFront with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 3: " . PushToFront("1,,2,,3,,4,,", ",,", "3"))
  call input("PushToFront with array: " . "1,,2,,3,,4" . " sep: " . ",," . " for 4: " . PushToFront("1,,2,,3,,4", ",,", "4"))

  call input("PullToBack with array: " . "1,,2,,3,,4,," . " sep: " . ",," . " for 3: " . PullToBack("1,,2,,3,,4,,", ",,", "3"))
  call input("PullToBack with array: " . "1,,2,,3,,4" . " sep: " . ",," . " for 1: " . PullToBack("1,,2,,3,,4", ",,", "1"))

  call input("EnsureTrailingSeparator with array: " . "1,,2,,3,,4,," . " sep: " . ",," . EnsureTrailingSeparator("1,,2,,3,,4,,", ",,"))
  call input("EnsureTrailingSeparator with array: " . "1,,2,,3,,4" . " sep: " . ",," . EnsureTrailingSeparator("1,,2,,3,,4", ",,"))
endfunction
