" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/cab.txt	[[[1
31
ONLY FOR MAC

Cab is a Vim plugin for using ack-grep (http://betterthangrep.com) in
common with iterm (http://www.iterm2.com/) for fast and easy search in
a project from vim.
It uses current working directory as a root.
It's possible to use Cab only with iterm (not with terminal) bacause cab
uses applescript API for interaction with iterm (or iterm2).

How to use Cab:

  in the main mode:
    place the cursor on the word to search and press <Leader>f
    it will open new tab of the iterm and run the coommand "ack -Q <word for search>"

  in the visual mode:
    select a range of text to search and press <Leader>f
    it will open new tab of the iterm and run the coommand "ack -Q <text for search>"

  in the command mode:
    type :Cab <text for search>
    it will open new tab of the iterm and run the coommand "ack -Q <text for search>"

Cab automatically escape special symbols and quotes. You can search for
any text. For exaple
 :Cab Search for this "quoted" 'text' and /\ slashes

You can redefine a method of invoking ack using global variable cab_ack_cmd
Example (write it in the your ~/.vimrc file):

  let g:cab_ack_cmd = "ack-grep -irn --ignore-dir=assets"
plugin/cab.vim	[[[1
45
if system("uname") !~ '\cdarwin'
  echo "cab.vim: you can use cab.vim only on the Mac OS X"

elseif empty(system("which ack"))
  echo "cab.vim: ack is not installed, you can install the ack using the Homebrew ($ brew install ack)"

else

  function! s:getVisual() range
    let reg_save = getreg('"')
    let regtype_save = getregtype('"')
    let cb_save = &clipboard
    set clipboard&
    normal! ""gvy
    let selection = getreg('"')
    call setreg('"', reg_save, regtype_save)
    let &clipboard = cb_save
    return selection
  endfunction

  let s:srcfile = expand("<sfile>")
  let s:srcdir = fnamemodify(s:srcfile, ":p:h")
  let s:applescript_file =  s:srcdir . "/cab.applescript"

  if !filereadable(s:applescript_file)
    echo "cab.vim: cab.applescript file should be placed in the same directory as the cab.vim file"
  endif

  let g:cab_ack_cmd = "ack -Q"

  function! CabSearch(...) range
    let arg = a:0 > 0 ? a:1 : expand("<cword>")
    let search_str = empty(arg) ? s:getVisual() : arg

    let search_str = substitute(search_str, "'", "'\\\\''", 'g')
    let cwd = getcwd()
    let cmd = "osascript " . s:applescript_file . " '" . g:cab_ack_cmd . "' " . cwd . " '" . search_str . "'"
    call system(cmd)
  endfunction

  command! -range -nargs=? Cab call CabSearch(<f-args>)
  nmap <Leader>f :Cab<CR>
  vmap <Leader>f :<C-U>call CabSearch("")<CR>

endif
plugin/cab.applescript	[[[1
31
on replaceString(theText, oldString, newString)
  set AppleScript's text item delimiters to oldString
  set tempList to every text item of theText
  set AppleScript's text item delimiters to newString
  set theText to the tempList as string
  set AppleScript's text item delimiters to ""
  return theText
end replaceString

on run argv
  set theAckCmd to item 1 of argv
  set thePath to item 2 of argv
  set theSearchStr to item 3 of argv
  set theEscapedSearchStr to replaceString(theSearchStr, "'", "'\\''")

  tell application "iTerm"
    activate

    if (count of terminal) = 0 then make new terminal

    tell the first terminal
      launch session "Default Session"
      tell the last session
        write text "cd " & thePath
        write text theAckCmd & " '" & theEscapedSearchStr & "'"
        set name to "ack"
      end tell
    end tell

  end tell
end run
