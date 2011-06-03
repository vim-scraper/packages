" =============================================================================
" Vim plugin to find function declarations (c, c++, python, objective-c,
" perl??, and possibly other styles).
"
" if invoked with a -1 as the argument, it asks for a pattern to search on
"
" otherwise, it uses the last-searched pattern
"
" if the last-used pattern is empty (""), it will prompt the user for a
" pattern, although if used without a pattern it will just cycle through
" function declarations
"
" Maintainer:   Alex Esplin <alex.esplin@gmail.com>
" Version:      1.0
" Last Change:  2009 March 12
"
" License:      Feel free to use or copy, but I won't complain if you give me
"               some credit via linking to the original, mentioning it, etc...
"
" Disclaimer:   I make no guarantee that this will work or not work, blah,
"               blah, blah... If it doesn't work like you want it to, let me
"               know.  If it somehow blows up and zaps your hard work, I
"               really am sorry and I didn't intend for it to do that. (And
"               quite frankly I don't know how that could happen...)
" =============================================================================

" don't load more than once...
if exists("loaded_fgrep")
  finish
endif
let loaded_fgrep = 1

" declare a global for last pattern so we can use it in any buffer
let g:lastPattern = ""

fun Fdgrep(repeatLast)
  " get the last-used pattern in case we want to use it again
  let pstring = g:lastPattern

  " if the user specified -1 for repeatLast, ask for a pattern to search on,
  " and set that pattern to be the last-used pattern
  if (a:repeatLast == -1)
    let pstring = input("keyword: ")
    let g:lastPattern = pstring
  endif

  " if pstring is somehow still empty, remind the user that searching any
  " function declarations might not be what they want
  "
  " this will force them to use an extra keystroke (<enter>) if cycling
  " through all function declarations, but this isn't that big of a deal
  if (pstring == "")
    let pstring = input("You really want a keyword: ")
  endif

  " we might want to use flags later...
  let flags = ""
  
  " insert our user-supplied pattern into a regex that matches most c-like or
  " python function declarations
  let pattern = '^\w\+\s\+.*' . pstring . '.*\n*[(){:]\s*$'
  let num = search(pattern, flags)
  if (num == 0)
    echo "no match found for:" . "'" . pattern . "'"
    else
        echo getline(num)
  endif

endfun

