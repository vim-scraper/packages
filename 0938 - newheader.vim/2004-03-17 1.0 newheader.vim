" CreateFunctionHeader, by Erik Janssen
" Email: ejanssen@itmatters.nl
" 
" History:
" ========
" 27-10-2000 Creation
" xx-11-2000 Various bugfixes
" 21-01-2001 Added 'header' keyword and support for empty lines in template
" 01-05-2001 Spent lot of time, but solved some annoying bugs
" 31-05-2001 Added support for // and comments inside function declarations
" feb 2002   Minor changes while moving from vim 5.7 to 6 
" 17-07-2002 Improved handling of static functions by making initial search
"            expression non-general + moved to static functions
" 19-02-2004 Changed argument matching regexpr to allow more C++
" 24-02-2004 Changed function header matching regexpr into something that
"            allows C++ functions as well. It's workable but not perfect

" Description:
" ============
"
" (See example below)
"
" This function creates a new comment header based on a template, in which
" details of a C function below the cursor are merged. The template is a text
" file containing a default header which you made yourself (or is forced upon
" you by your QA manager).  This file should be named '_c_header'. An example
" is at the bottom of this file. This file is searched for in a couple of
" directories.  Modify the function Hdr_SearchFile to fit your needs if you want
" it different.

" The template is inserted above your C function while making the following
" replacements. It is not necessary that your template contains any/all of
" these items. The 'text' and 'args' strings are formatted to have a neat
" indentation, which is basically the big timesaving feature.  You've to have
" 'format-comments' (vim setting) enabled

" header = The function name and argument list unformatted as it appears in
"          the code (generally not very usefull)
" name   = The function name only
" date   = The current date
" text   = Any comment that precedes the function is removed an integrated
"          here. If no comment is found in the first 5 lines above the function
"          a warning text is inserted instead. This allows for _very_
"          convenient updates from quick & dirty comments to formal headers
" args   = The argument list, one argument per line so you can add
"          descriptions behind it manually afterwards

" Usage:
" ======
" 1. Copy/create the file _c_header (example below, remove vim comments chars)
"    in your current dir, $VIM dir, or $HOME/.vim/plugin (or adapt the script)
" 2. Modify it so it matches your layout
" 3. Source this script from your vimrc or at the vim command prompt
"    :source yourpath/newheader.vim
" 4. Place the cursor on or above the line of the function definition and
"    press F7 (change the mapping if you like).

" Bugs:
" =====
" - A function with no comment above it sometimes confuses the script
" - Different coding styles might lead to different behaviour, please report
" - So far, any new header template forced me to solve shortcomings...
" - Comments inside a function header could be interpreted as argument
"   descriptions and inserted in 'args' but currently they are deleted
" - function matching regexpr also may match some (C++) statements (new,
"   delete, etc)

" TODO:
" =====
" - Make it respect the 'comments' setting. Support for generic
"   template (comment characters adjusted) and language specific headers, 
"   based on 'filetype'.
" - How to make the scanning/interpreting a function decl language independed?

function! CreateFunctionHeader()
   " First scan forward to the first available definition
   " A function definition is currently: list of word seperated by ' ' or :'s
   " followed by a '('
   " BUG: "new X()" also matches. Therefore do not allow spaces at start of
   " line: that catches most of the dubious cases
   normal ^
   execute "norm! /^\\(\\w\\+[ :&*]\\{1,2}\\)\\+\\w\\+\\s*(\<CR>"
   echo getline(".")

   "Move to startbrace of argumentlist, then to start of prev word, that's the
   "function name
   execute "norm! /(\<CR>b"
   let fun_name = expand("<cword>")
   let start_position = line(".") - 1
   
   " Capture lines as long as closing brace of argument list is not found
   let fun_decl = ""
   let header_copy = ""
   let header_start = line(".")
   while match( fun_decl, ")" ) == -1
     let fun_decl = fun_decl . getline(".")
     " Remove (unnecessary) line continuation characters
     " Remove //-style comment
     let fun_decl = substitute( fun_decl, '\\\s*$', " ", "" )
     let fun_decl = substitute( fun_decl, '//.*$', "", "" )
     let header_copy = header_copy . getline(".") . "\r" 
     normal! j
   endwhile 
   " Remove C style comments inside function header
   " Remove everything from start to first open brace +
   " Remove everything from closing brace till end --> arguments extracted
   let fun_decl = substitute( fun_decl, '/\*.*\*/', "", "g" )
   let arg_list = substitute( fun_decl, '^.*( *',"","" )
   let arg_list = substitute( arg_list, '[ \t]*).*$', ",", "" )
   " Now remove any type specifiers and whitespace from the arguments:
   " first remove whitespace around comma's
   " then remove all words terminated by space or type/namespace specifiers
   " then remove remaining whitespace
   " then remove remaining newlines

   let arg_list = substitute( arg_list, '\s*,\s*', ',', "g" )
   let arg_list = substitute( arg_list, '\(\w\+[:& ]\+\)', "", "g" )
   let arg_list = substitute( arg_list, '\s\+', "", "g" )
   let arg_list = substitute( arg_list, "\n", "", "g" )

   " Now search upward a few lines for a C-style comment. If it exists,
   " swallow it in a variable and strip all non-text. The remaining text will
   " be inserted in the template

   exe "normal! " . start_position . "gg$"
   exe "normal ?\\*/\<CR>"
   if ((line(".") <= start_position) && (line(".") > (start_position - 3)))
      exe "normal ?/\\*\<CR>"
      let comment_start = line(".")
      let comment = getline(".")
      while match( comment, '\*/' ) == -1
         normal! j
         let comment = comment . " " . getline(".")
      endwhile 
      exe comment_start. "," . line(".") . "d"
      let comment = s:Hdr_StripText( comment )
      let start_position = start_position - (start_position - comment_start) 
   else
      " C style comment not found, retry for C++ comment
      exe "normal! " . start_position . "gg$"
      exe "normal ?^\\s*//\<CR>"
      if ((line(".") <= start_position) && (line(".") > (start_position - 3)))
         let comment_end = line(".")
         let comment = ""
         while match( getline("."), '^\s*//' ) == 0
           let comment = substitute( getline("."), '^\s*//',"","")." ".comment
           normal! k
         endwhile
         exe line(".") . "," comment_end . "d"
         let comment = s:Hdr_StripText( comment )
         let start_position = start_position - (start_position - line("."))
      else 
        let comment = "(No text available)"
      endif 
   endif
   
   " Go back to the first empty line and insert the header template, then
   " determine the top and bottom line of for search-replace's that follow
   
   exe "norm! " . start_position . "ggk"
   let myfile = s:Hdr_SearchFile( "_c_header" )
   if myfile != ""
      let previous_end = line("$")
      execute "read " . myfile
   else
      execute "normal! i/* " . fun_name ."() (no header template found) */\<esc>"
      return
   endif
   let top_line = line(".")
   let bot_line = top_line + line("$") - previous_end

   " Search for location to insert function header, name 
   " and date (if applicable)

   call s:Hdr_ReplaceString( top_line, bot_line, '\<header\>', header_copy, "" )
   call s:Hdr_ReplaceString( top_line, bot_line, '\<name\>', fun_name, "" )
   call s:Hdr_ReplaceString( top_line, bot_line, '\<date\>', strftime("%Y %b %d"), "")
   
   " Search for location to insert argument list, determine start column
   " and reformat arguments to align neatly. Then insert it
   
   if s:Hdr_SearchText( top_line, bot_line, '\<args\>' ) == 1
      let leading_spaces = s:Replicate( " ", col(".") - 5 )
      let replace_str1 = "\\1:\<CR>" . leading_spaces . "\\2"
      let arg_list = substitute( arg_list, '\(.\),\(.\)', replace_str1, "" )
      let arg_list = substitute( arg_list, '\(.\),\(.\)', "\\1:\<CR>\\2", "g" )
      let arg_list = substitute( arg_list, ',', ':', '' )
      exe "norm! cw" . arg_list . "\<esc>"
   endif

   " Search for location to insert comment, determine start column
   " and reformat text to align neatly. Then insert it

   if s:Hdr_SearchText( top_line, bot_line, '\<text\>' ) == 1
      let comment = s:Hdr_ReformatText( comment, col(".") -5, &tw-4 )
      exe "norm! cw" . comment . "\<esc>"
   endif
endfunction


" Replaces a string between two lines if it can find the pattern

function! s:Hdr_ReplaceString( top_line, bot_line, pattern, replacement, flags )
    exe "norm! " . a:top_line . "gg"
    exe  a:top_line "," a:bot_line "s/" . a:pattern . "/&/e"
    if line(".") != a:top_line
       exec "s+" . a:pattern . "+" . a:replacement . "+" . a:flags
       exe "norm! " . a:top_line . "gg"
    endif
endfunction

" Searches for a string and leaves the cursor on the relevant line/column if 
" found. It returns 0 if not found, 1 if found

function! s:Hdr_SearchText( top_line, bot_line, pattern )
    exe "norm! " . a:top_line . "gg"
    exe  a:top_line "," a:bot_line "s/" . a:pattern . "/&/e"
    if line(".") != a:top_line
      exe "norm! " . a:top_line . "gg"
      exe "norm! /" . a:pattern . "\<CR>"
      return 1
    endif
    return 0
endfunction

" Return a string with 'character' repeated 'count' times

function! s:Replicate( character, count )
  let my_string = ""
  let i = a:count
  while i > 0
     let my_string = my_string . a:character
     let i = i - 1
  endwhile
  return my_string
endfunction

" Search a file in any of my vim related directories and return the complete
" path and filename. If not found, return ""

function! s:Hdr_SearchFile( filename )
   if filereadable( a:filename )
     return a:filename
   elseif filereadable( $VIM . "/" . a:filename )
     return $VIM . "/" . a:filename
   elseif filereadable( $VIM . "/plugin/" . a:filename )
     return $VIM . "/plugin/" . a:filename
   elseif filereadable( $HOME . "/.vim/plugin/" . a:filename )
     return $HOME . "/.vim/plugin/" . a:filename
   elseif filereadable( $VIM . "/vimfiles/plugin/" . a:filename )
     return $VIM . "/vimfiles/plugin/" . a:filename
   elseif filereadable( "~/tmp" . a:filename )
     return "~/tmp" . a:filename
   endif
   return ""
endfunction

" Strip all C comment characters, newlines and too much whitespace from a 
" string and return the bare string

function! s:Hdr_StripText( text )
   let mytext = substitute( a:text, '\*\+\/', "", "" )
   let mytext = substitute( mytext, '\/\*\+', "", "" )
   let mytext = substitute( mytext, ' \* ', "", "g" )
   let mytext = substitute( mytext, '\t', " ", "g" )
   let mytext = substitute( mytext, '^ \+', "", "g" )
   let mytext = substitute( mytext, ' \+$', "", "g" )
   " let mytext = substitute( mytext, '  \+', " ", "g" )
   " let mytext = substitute( mytext, '<CR>', " ", "g" )
   return mytext
endfunction

" Reformats a string in such a way, that inserting it with 'format-comments'
" on will give a neat result. I have to make it independend from vim settings.

function! s:Hdr_ReformatText( text, left_margin, right_margin )
   let len = strlen(a:text)
   if len > (a:right_margin - a:left_margin)
     let pos = a:right_margin - a:left_margin
     while ((pos > 0) && (strpart( a:text, pos, 1 ) != " "))
        let pos = pos - 1
     endwhile
     let str = strpart( a:text, 0, pos )."\<CR>".s:Replicate(" ",a:left_margin).strpart(a:text,pos+1,len)
   else
     let str = a:text
   endif  
   return str
endfunction

nmap <F7> :call CreateFunctionHeader()<CR>
if has("menu")
  amenu &Local.&Function\ Header<Tab><F7>   <F7>
endif


" Example _c_header:
" ==================

" /****************************************************************************
"  *
"  * NAME:        name                                    (date)
"  *
"  * DESCRIPTION: text
"  *
"  * ARGUMENTS:   args
"  *
"  * RETURNS:     -
"  *
"  * PRE:         -
"  *
"  * POST:        -
"  ***************************************************************************/
" 

" Suppose you have this function:
" ===============================

" /* 
"  * This is just a comment that is long enough to show the features of
"  * newheader.vim. It will do some indenting for you and list your function
"  * arguments
"  */
" 
" static void show_header( int width, int length,
"                          byte font, Language *language_selection,
"                      byte color )
" {
"      ....
" }


" Result
" ======

" Place the cursor one or more lines before the function and press F7. It
" gives you:
"
" /****************************************************************************
"  *
"  * NAME:        show_header                                    (2000 okt 27)
"  *
"  * DESCRIPTION: This is just a comment that is long enough to show the
"  *              features of newheader.vim. It will do some indenting for you
"  *              and list your function arguments
"  *
"  * ARGUMENTS:   width:
"  *              length:
"  *              font:
"  *              *language_selection:
"  *              color:
"  *
"  * RETURNS:     -
"  *
"  * PRE:         -
"  *
"  * POST:        -
"  ****************************************************************************/
" 
" static void show_header( int width, int length,
"                          byte font, Language *language_selection,
"                      byte color )
" {
"      ....
" }

