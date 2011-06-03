" Copyright (c) 1998-2001
" Michael Sharpe <feline@irendi.com>
"
" We grant permission to use, copy modify, distribute, and sell this
" software for any purpose without fee, provided that the above copyright
" notice and this text are not removed. We make no guarantee about the
" sutability of this software for any purpose and we are not liable
" for any damages resulting from its use. Further, we are under no
" obligation to maintain or extend this software. It is provided on an
" "as is" basis without any express or implied warranty.

" Function : AlternateFile (PUBLIC)
" Purpose  : Opens a new buffer by looking at the extension of the current
"            buffer and finding the corresponding file. E.g. foo.c <--> foo.h
" Args     : accepts one argument. If present it used the argument as the new
"            extension.
" Returns  : nothing
" Notes    : this is becoming more complex than I imagined. Will likely rewrite
"            this soon such that a list of extensions and alternates can be 
"            registered. This will allow the core function to not have a ton of
"            tests and allow users to determine thier .cpp vs .cxx vs .cc etc
"            preferences.
" Author   : Michael Sharpe <feline@irendi.com>
if exists("loaded_alternateFile")
    finish
endif
let loaded_alternateFile = 1

func! AlternateFile(splitWindow, ...)
  let baseName = expand("%<")
  " before 5.6 if (a:1 != "") is needed instead of the following...
  if (a:0 != 0)
     let newFilename = baseName . "." . a:1
  else
     let currentFile = expand("%")
     let extension = fnamemodify(currentFile,":e")
     if (extension == "c")
        let newFilename = baseName.".h"
     elseif (extension == "cpp" || extension == "CPP")
        " first try matching with a .hpp file, which is sometimes used with cpp
        " files. If that fails go with .h which is more common.
        let newFilename = baseName . ".hpp"
        let existsCheck = BufferOrFileExists(newFilename)
        if (existsCheck == 0)
           " no hpp file about, so use the .h which is more common
           let newFilename = baseName . ".h"
        endif
     elseif (extension == "cc" || extension == "CC")
        let newFilename = baseName . ".h"
     elseif (extension == "C")
        let newFilename = baseName . ".h"
     elseif (extension == "cxx" || extension == "CXX")
        let newFilename = baseName . ".h"
     elseif (extension == "psl")
        let newFilename = baseName . ".ph"
     elseif (extension == "ph")
        let newFilename = baseName . ".psl"
     elseif (extension == "h" || extension == "H" || extension == "hpp" || extension == "HPP")
        " check to see if a .c file exists
        let newFilename = baseName . ".c"
        let existsCheck = BufferOrFileExists(newFilename)
        if (existsCheck == 0)
           " no .c try for a .cpp
           let newFilename = baseName . ".cpp"
           let existsCheck = BufferOrFileExists(newFilename)
           if (existsCheck == 0)
              " no .c or .cpp try for a .cc
              let newFilename = baseName . ".cc"
              let existsCheck = BufferOrFileExists(newFilename)
              if (existsCheck == 0)
                 " no .c, .cpp or .cc try for a .C
                 let newFilename = baseName . ".C"
                 let existsCheck = BufferOrFileExists(newFilename)
                 if (existsCheck == 0)
                    " no .c, .cpp, .cc or .C try for a .cxx
                    let newFilename = baseName . ".cxx"
                    let existsCheck = BufferOrFileExists(newFilename)
                    if (existsCheck == 0)
                       " no .c, .cpp, .cc, .C or .cxx exists default to .cpp
                       let newFilename = baseName . ".cpp"
                    endif
                 endif
              endif
           endif
        endif
     else
        echo "AlternameFile: unknown extension"
        return
     endif
  endif
  call FindOrCreateBuffer(newFilename, a:splitWindow)
endfunc
comm! -nargs=? A call AlternateFile(0, <f-args>)
comm! -nargs=? AS call AlternateFile(1, <f-args>)


" Function : BufferOrFileExists (PRIVATE)
" Purpose  : determines if a buffer or a readable file exists
" Args     : name (IN) - name of the buffer/file to check
" Returns  : TRUE if it exists, FALSE otherwise
function! BufferOrFileExists(name)
   let result = bufexists(a:name) || filereadable(a:name)
   return result
endfunction

" Function : FindOrCreateBuffer (PRIVATE)
" Purpose  : searches the buffer list (:ls) for the specified filename. If
"            found, checks the window list for the buffer. If the buffer is in
"            an already open window, it switches to the window. If the buffer
"            was not in a window, it switches to that buffer. If the buffer did
"            not exist, it creates it.
" Args     : filename (IN) -- the name of the file
"            doSplit (IN) -- indicates whether the window should be split
" Returns  : nothing
" Author   : Michael Sharpe <feline@irendi.com>
function! FindOrCreateBuffer(filename, doSplit)
  " Check to see if the buffer is already open before re-opening it.
  let bufName = bufname(a:filename)
  if (bufName == "")
     " Buffer did not exist....create it
     if (a:doSplit != 0)
        execute ":split " . a:filename
     else
        execute ":e " . a:filename
     endif
  else
     " Buffer was already open......check to see if it is in a window
     let bufWindow = bufwinnr(a:filename)
     if (bufWindow == -1) 
        if (a:doSplit != 0)
           execute ":sbuffer " . a:filename
        else
           execute ":buffer " . a:filename
        endif
     else
        " search the windows for the target window
        if bufWindow != winnr()
           " only search if the current window does not contain the buffer
	   execute "normal \<C-W>b"
	   let winNum = winnr()
	   while (winNum != bufWindow && winNum > 0)
	      execute "normal \<C-W>k"
	      let winNum = winNum - 1
	   endwhile
	   if (0 == winNum) 
	      " something wierd happened...open the buffer
	      if (a:doSplit != 0)
		 execute ":split " . a:filename
	      else
		 execute ":e " . a:filename
	      endif
	   endif
        endif
     endif
  endif
endfunction
