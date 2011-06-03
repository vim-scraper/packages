" Author:  Eric Van Dewoestine
" Version: 0.5
"
" Description: {{{
"   Initially based on tar.vim by Michael C. Toren
"   Other than the initial autocommands and some function
"   layouts, very little is the same.
"
" Platform:
"   Linux:
"     All testing done on linux so it should work fine in linux
"     environments that satisfy the dependencies below.
"   Windows (Cygwin):
"     Works in conjunction with cygwin_utils.vim plugin.
"     Make sure to read the Description comments in cygwin_utils.vim for
"     configuration instructions and other notes.
"
" Dependencies:
"   - unzip: must support -c (extract to stdout)
"   - zip: to edit files in a jar, war, ear, rar, sar, zip
"   - tar: must support -O (extract to stdout), -j (for bz2),
"     -z (for tgz, gz, Z)
"   - table_format.vim (tested with version 1.0) or
"     Align.vim (tested with version 26 ) for formatting
"     archive file layout
"   - Viewing of .class files from archives depends on the following
"     mapping (in vimrc or elsewhere) being loaded
"     (and of course having jad installed):
"       "view java class files with jad
"       augr class
"         au!
"         au bufreadpost,filereadpost *.class %!jad -noctor -ff -i -p -t2 -b %
"         au bufreadpost,filereadpost *.class silent set readonly
"         au bufreadpost,filereadpost *.class silent set ft=java
"         au bufreadpost,filereadpost *.class silent normal gg=G
"         au bufreadpost,filereadpost *.class silent set nomodified
"       augr END
"
" Usage:
"   To start the viewer
"     vim a_tar_file.tar
"
"   To view a file or nested archive, simple hit enter on the line for
"   that file.
"
"   - To add a file to an archive hit "a", and follow the prompts.
"   - To edit a file in an archive hit "e" while viewing the file, edit
"     it and save it with ":w".
"   - To delete a file hit "r" on the line for that file.  To delete more
"     than one file, simple use visual mode (V) to select the files to
"     delete.
"   - To extract a file hit "x" on the line for that file.  To extract more
"     than one file, simple use visual mode (V) to select the files to
"     extract.
"   - To extract the entire archive use "X".
"
"   For jar files there is also a global function "ViewJarUrl(url,wincmd)",
"   that takes a java standard jar url, and vim window commands to open
"   a file.
"   The format of the url is
"     jar:{url}!{file}
"   Ex.
"     jar:file:///home/myusr/myjar.jar!/adir/afile.txt
"
"   Currently, only "file:" urls are supported (not "http:", "ftp:", etc).
"
"   Ex. Usage of ViewJarUrl() to open a file in a new maximized window.
"     call ViewJarUrl("jar:file:///home/myusr/myjar.jar!/adir/afile.txt",
"       \ "new | winc _")
"
" Configuration:
"
"   g:ArchiveViewerPrompt
"     Defaults to 1.  Determines if prompts are used to capture
"     the directory name for extraction and adding files.  When
"     set to 1 prompts (using the input() function of vim) are used.
"     When set to 0, a : command is initiated for you that you can
"     complete to execute the action.
"
"     When hitting 'x' or 'X' to extract files the following : command
"     is initiated...
"       :ExtactTo 
"     Simply supply the directory to extract to after that and hit
"     enter to execute.
"
"     When hitting 'a' to add files the following : command is initiated...
"       :AddFilesFrom 
"     Supply the directory to start in (the directory that all added files
"     should be relative to) and hit enter.  You will then be prompted for
"     the files to add.
"
"     This was added because vim's input() function does not have
"     path completion, but : commands do.
"
"     Ex. let g:ArchiveViewerPrompt = 0
"
" License:
"
"   Permission is hereby granted, free of charge, to any person obtaining
"   a copy of this software and associated documentation files (the
"   "Software"), to deal in the Software without restriction, including
"   without limitation the rights to use, copy, modify, merge, publish,
"   distribute, sublicense, and/or sell copies of the Software, and to
"   permit persons to whom the Software is furnished to do so, subject to
"   the following conditions:
"
"   The above copyright notice and this permission notice shall be included
"   in all copies or substantial portions of the Software.
"
"   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
"   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"
" }}}

" Archive autocommand group {{{
augroup archive
  au!
  au BufReadCmd *.jar call s:ViewArchive(expand("<afile>"))
  au BufReadCmd *.war call s:ViewArchive(expand("<afile>"))
  au BufReadCmd *.ear call s:ViewArchive(expand("<afile>"))

  au BufReadCmd *.zip call s:ViewArchive(expand("<afile>"))

  au BufReadCmd *.tar     call s:ViewArchive(expand("<afile>"))
  au BufReadCmd *.tgz     call s:ViewArchive(expand("<afile>"))
  au BufReadCmd *.tar.gz  call s:ViewArchive(expand("<afile>"))
  au BufReadCmd *.tar.bz2 call s:ViewArchive(expand("<afile>"))
  au BufReadCmd *.tar.Z   call s:ViewArchive(expand("<afile>"))

  " on :qa the temp files don't get deleted, so use this to cleanup.
  au VimLeavePre * call s:CleanTempFiles()
augroup END
" }}}

" Global Variables {{{
if !exists("g:ArchiveViewerPrompt")
  let g:ArchiveViewerPrompt = 1
endif
" }}}

" Script Variables {{{
let s:escape_chars =  " `|\"'#&()[]!"
let s:tempdir = $TEMP
if s:tempdir == ""
    let s:tempdir = "/tmp"
endif
if has("win32") || has("win64")
  let s:tempdir = s:tempdir . '\'
else
  let s:tempdir = s:tempdir . '/'
endif
let s:tar_header = "Permissions Ownership Size Date Time Name\n" .
                  \"----------- --------- ---- ---- ---- ----\n"
let s:jar_header = ""

let s:jar_url_prefix = "jar:file:"
let s:jar_url_sep = "!"

let b:archive = ""
let b:tempfile = ""
let b:view = ""
let b:extract_std = ""
let b:extract = ""
let b:update = ""
let b:add = ""
let b:header = ""
" }}}

" ViewArchive(archive) {{{
" Views an archive in vim
function! s:ViewArchive (archive)
  noremap <silent> <buffer> <cr>
    \ :call <SID>ViewFile(<SID>GetFileName(getline(".")))<cr>
  noremap <silent> <buffer> r :call <SID>DeleteFiles()<cr>
  noremap <silent> <buffer> e
    \ :call <SID>EditFile(<SID>GetFileName(getline(".")), "new \| winc _")<cr>

  " keep things backwards compatable
  if g:ArchiveViewerPrompt
    noremap  <silent> <buffer> x :call <SID>ExtractPrompt()<cr>
    vnoremap <silent> <buffer> x :call <SID>ExtractPrompt()<cr>
    noremap  <silent> <buffer> X :call <SID>ExtractAllPrompt()<cr>
    noremap  <silent> <buffer> a :call <SID>AddFilesPrompt()<cr>
  else
    command! -range -nargs=1 -complete=dir ExtractTo
      \ :call <SID>ExtractRange('<line1>', '<line2>', '<a>')
    command! -nargs=1 -complete=dir ExtractAllTo
      \ :call <SID>ExtractAll('<a>')
    command! -nargs=* -complete=dir AddFilesFrom
      \ :call <SID>AddFiles('<a>')

    noremap  <silent> <buffer> x :<C-R>="ExtractTo"<CR> <C-L>
    vnoremap <silent> <buffer> x :<C-R>="ExtractTo"<CR> <C-L>
    noremap  <silent> <buffer> X :<C-R>="ExtractAllTo"<CR> <C-L>
    noremap  <silent> <buffer> a :<C-R>="AddFilesFrom"<CR> <C-L>
  endif

  setlocal noswapfile
  setlocal buftype=nofile
  setlocal bufhidden=delete
  setlocal filetype=
  "setlocal nobuflisted
  setlocal nowrap

  call s:ArchiveSettings(a:archive)

  call s:Write(b:header)

  normal G

  echo "Loading archive ..."

  "exec "echom \"" . b:view . s:DeterminePath(b:archive) . "\""
  call s:Execute("r!" . b:view . s:DeterminePath(b:archive))

  if v:shell_error
    normal 
    exec "echoe \"Cannot load archive '" . b:archive . "'\""
  endif

  silent normal gg3dd

  " execute the alignment command
  silent exec s:align_cmd
  normal gg

  setlocal readonly
  setlocal nomodifiable

  echo "Done."
endfunction
" }}}

" ViewJarUrl(url, wincmd) {{{
" Opens a file inside an archive using the given url and
" wincmd.
function! ViewJarUrl (url, wincmd)
  let index_prefix = stridx(a:url, s:jar_url_prefix)
  let index_sep = stridx(a:url, s:jar_url_sep)
  if index_prefix != 0 || index_sep == -1
    echom "Invalid jar url"
    return
  endif

  let l:archive = strpart(a:url,
    \ strlen(s:jar_url_prefix),
    \ (strlen(a:url) - strlen(s:jar_url_prefix)) - (strlen(a:url) - index_sep))
  if l:archive =~ '^//'
    let l:archive = strpart(l:archive, 2)
  endif

  let l:file = strpart(a:url, index_sep + 1)
  if l:file =~ '^/'
    let l:file = strpart(l:file, 1)
  endif

  call s:ArchiveSettings(l:archive)

  call s:ViewArchiveFile(l:archive, l:file, a:wincmd)
endfunction
" }}}

" ViewFile(file) {{{
" Invoked when a file is selected.
function! s:ViewFile (file)
  if a:file != ""
    let l:archive = b:archive

    call s:ViewArchiveFile(l:archive, a:file, "new | winc _")
  endif
endfunction
" }}}

" ViewArchiveFile(archive, file, wincmd) {{{
" Loads the requested file from the supplied archive.
function! s:ViewArchiveFile (archive, file, wincmd)
  let b:result = a:file
  if b:result != ""
    if s:IsArchive(b:result) || b:result =~# '\.class$'
      let l:archive = s:DeterminePath(a:archive)
      call s:ExtractTo(b:extract, l:archive, s:tempdir, b:result)

      let l:result = b:result
      let l:archive = a:archive

      if l:result != ""
        let cwd = getcwd()

        exec a:wincmd

        " in cygwin, need to edit the file from the temp dir (for class
        " files)
        "exec "lcd " . escape(s:tempdir, s:escape_chars)
        silent execute "e " . 
          \ escape(s:tempdir, s:escape_chars) .
          \ escape(l:result, s:escape_chars)
        "exec "lcd " . escape(cwd, s:escape_chars)

        " cygwin needs to have screen redrawn
        normal 

        au BufUnload *.class     call s:ViewExit()
        au BufUnload *.jar       call s:ViewExit()
        au BufUnload *.ear       call s:ViewExit()
        au BufUnload *.war       call s:ViewExit()
        au BufUnload *.zip       call s:ViewExit()
        au BufUnload *.tar       call s:ViewExit()
        au BufUnload *.tgz       call s:ViewExit()
        au BufUnload *.tar.gz2   call s:ViewExit()
        au BufUnload *.tar.bz2   call s:ViewExit()
        au BufUnload *.tar.Z     call s:ViewExit()

        let b:parent = l:archive
        let b:tempfile = s:tempdir . l:result
        if s:IsArchive(b:tempfile)
          let b:archive = b:tempfile
        endif
      endif
    else
      call s:VimViewer(a:archive, a:file, a:wincmd)
    endif

    au WinEnter * call s:ViewFocus()

    setlocal readonly
    setlocal nomodifiable
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
  endif
endfunction
" }}}

" VimViewer(archive, file, wincmd) {{{
" View the selection in vim
function! s:VimViewer (archive, file, wincmd)
  " make sure that the archive exists
  if !filereadable(a:archive)
    exec "echoe \"Cannot read archive '" . a:archive . "'\""
    return
  endif

  " remove new file autocommands
  au! BufNewFile

  let l:title = a:file
  let index = strridx(l:title, '/')
  if index != -1
    let l:title = strpart(l:title, index + 1)
  endif

  let l:title = "archive_" .  fnamemodify(a:archive, ":t") . "-" . l:title
  let l:title = substitute(l:title, ' ', '_', 'g')

  let l:extract_std = b:extract_std
  let l:archive_view = s:DeterminePath(a:archive)
  let l:result = a:file
  let l:archive = a:archive

  exec a:wincmd

  silent execute "e! " . escape(l:title, s:escape_chars)
  setlocal nobuflisted

  "exec "echom \"" . l:extract_std . l:archive_view . " " . l:result . "\""
  call s:Execute("r!" . l:extract_std . l:archive_view . " " . l:result)

  if v:shell_error
    normal 
    exec "echoe \"Cannot extract file '" . l:result . "' for viewing\""
  endif

  let b:parent = l:archive
  let b:current = l:result

  " delete extra info that unzip puts in.
  if l:extract_std =~ "^unzip"
    silent normal Gdd
    silent normal gg2dd
    let line = getline(line("."))
    if stridx(line, "inflating:") != -1 || stridx(line, "extracting:") != -1
      silent normal dd
    endif
  elseif l:extract_std =~ "^tar"
    silent normal ggdd
  endif

  noremap <silent> <buffer> e :call <SID>Edit()<cr>
endfunction
" }}}

" ViewFocus {{{
" Invoked when a buffer regains focus
function! s:ViewFocus ()
  winc _
  normal z-
endfunction
" }}}

" ViewExit() {{{
" Invoked when exitting the buffer
function! s:ViewExit ()
  " clean any temp files
  if exists("b:tempfile") && filereadable(b:tempfile)
    call s:Execute("!rm " . s:DeterminePath(b:tempfile))
  endif
endfunction
" }}}

" CleanTempFiles {{{
" Invoked when exiting vim
function! s:CleanTempFiles ()
  " clean any temp files
  let l:bufend = bufnr("%")

  bfirst
  let l:bufstart = bufnr("%")

  blast
  if bufnr("%") > l:bufend
    let l:bufend = bufnr("%")
  endif

  let l:bufnum = l:bufstart
  while l:bufnum <= l:bufend
    if bufexists(l:bufnum)
      exec "buffer " . l:bufnum
      call s:ViewExit()
    endif

    let l:bufnum = l:bufnum + 1
  endwhile
endfunction
" }}}

" ArchiveSettings(archive) {{{
" Sets the necessary buffer vars for the supplied archive
function! s:ArchiveSettings (archive)
  "if(exists("b:parent"))
  "  let b:archive = expand("%:p")
  "else
  "  let b:archive = expand("%")
  "endif
  let b:archive = a:archive

  call s:AlignmentSettings()

  let b:header= "\n\n" . s:tar_header

  if b:archive =~# '\.\(jar\|war\|ear\|zip\)$'
    let b:header = s:jar_header

    let b:add = "cd %dir% && zip -ur %archive% %file%"
    let b:delete = "zip -d %archive% %file%"
    let b:extract_std = "unzip -c "
    let b:extract = "unzip -d %dir% "
    let b:update = "cd %dir% && zip -f %archive% %file%"
    let b:view = "unzip -l "
  elseif b:archive =~# '\.tar$'
    let b:add = "tar -C %dir% -uf %archive% %file%"
    let b:delete = "tar --delete -f %archive% %file%"
    let b:extract_std = "tar -xOf "
    let b:extract = "tar -C %dir% -xf "
    let b:update = b:delete . " && " .
      \ "tar -C %dir% -uf %archive% %file%"
    let b:view = "tar -tvf "
  elseif b:archive =~# '\.\(gz\|tgz\|Z\)$'
    let b:add = ""
    "let b:add = "tar -C %dir% -zuf %archive% %file%"
    let b:delete = ""
    "let b:delete = "tar --delete -zf %archive% %file%"
    let b:extract_std = "tar -zxOf "
    let b:extract = "tar -C %dir% -zxf "
    let b:update = ""
    "let b:update = b:delete . " && " .
    "  \ "tar -C %dir% -zuf %archive% %file%"
    let b:view = "tar -ztvf "
  elseif b:archive =~# '\.bz2$'
    let b:add = ""
    "let b:add = "tar -C %dir% -juf %archive% %file%"
    let b:delete = ""
    "let b:delete = "tar --delete -jf %archive% %file%"
    let b:extract_std = "tar -jxOf "
    let b:extract = "tar -C %dir% -jxf "
    let b:update = ""
    "let b:update = b:delete . " && " .
    "  \ "tar -C %dir% -juf %archive% %file%"
    let b:view = "tar -jtvf "
  endif
endfunction
" }}}

" AlignmentSettings () {{{
" Sets the alignment command based on the available plugins.
function! s:AlignmentSettings ()
  " table_format.vim
  if exists("*Table")
    let s:align_cmd = ":1,$call Table()"

  " Align.vim
  elseif exists("*AlignCtrl")
    function! s:ArchiveAlign ()
      silent %s/^\(\s*\)\(.*\)/\=submatch(1).escape(substitute(submatch(2),'\s\+','@','g'),'\')
      silent AlignCtrl mI=lp0P0 @
      silent 1,$Align
      silent %s/@/ /g
    endfunction

    command! -buffer ArchiveAlign :call <SID>ArchiveAlign()
    let s:align_cmd = ":ArchiveAlign"

  " no alignment plugin found
  else
    echoe "*** archive_viewer.vim requires either "
      \ "Align.vim or table_format.vim to function. ***"
  endif
endfunction
" }}}

" Write(string) {{{
" Write the supplied string to the buffer
function! s:Write (string)
  let @" = a:string
  $ put
endfunction
" }}}

" AddFilesPrompt {{{
" Add a file or directory to the archive
function! s:AddFilesPrompt ()
  if s:HasCommand(b:add)
    let l:response = ""
    while l:response == ""
      let l:response = input("Specify the directory to start in: ")
    endwhile

    call s:AddFiles(l:response)
  endif
endfunction
" }}}

" AddFiles(startdir) {{{
" Add a file or directory to the archive
function! s:AddFiles (startdir)
  if s:HasCommand(b:add)
    let l:startdir = expand(a:startdir)

    if !isdirectory(l:startdir)
      exec "echo \"directory '" . l:startdir . "' doesn't exist. \""
      return
    endif
    let l:startdir = s:DeterminePath(l:startdir)

    let l:response = ""
    while l:response == ""
      let l:response = input("Files/Directories to add: ")
    endwhile

    let l:files = l:response
    let l:archive = s:DeterminePath(b:archive)

    let l:add = b:add
    let l:add = substitute(l:add, "%dir%", l:startdir, "g")
    let l:add = substitute(l:add, "%archive%", l:archive, "g")
    let l:add = substitute(l:add, "%file%", l:files, "g")

    "exec "echom \" !" . l:add . "\""
    call s:Execute("!" . l:add)

    normal 

    if v:shell_error
      exec "echoe \"Cannot add files '" . l:files .
        \ "' from dir '" . a:startdir . "'\""
    endif

    setlocal noreadonly
    setlocal modifiable

    silent normal ggVGd

    call s:ArchiveUpdate()
    call s:ViewArchive(b:archive)
  endif
endfunction
" }}}

" DeleteFiles() {{{
" Delete files from an archive
function! s:DeleteFiles () range
  if s:HasCommand(b:delete)
    let l:response = ""
    while l:response !~ "y" && l:response !~ "n"
      let l:response = input("delete selected files? (y/n): ")
    endwhile

    if l:response =~ "^n"
      return
    endif

    let l:delete = b:delete
    let l:delete =
      \ substitute(l:delete, "%archive%", s:DeterminePath(b:archive), "g")

    let l:deleted_count = 0
    " delete files
    let l:line = a:firstline
    while line <= a:lastline
      let l:file = getline(l:line)
      let l:filename = s:GetFileName(l:file)
      if l:filename != ""
        echo "deleting '" . l:filename . "'"

        let temp_delete = substitute(l:delete, "%file%", l:filename, "g")
        " delete the file
        "exec "echom \" !" . temp_delete . "\""
        call s:Execute("!" . temp_delete)

        if v:shell_error
          normal 
          exec "echoe \"Cannot delete file '" . l:filename . "'\""
          return
        endif

        let l:deleted_count = l:deleted_count + 1
      endif
      let l:line = l:line + 1
    endwhile

    normal 

    let l:archive = escape(b:archive, '\')
    exec "echo \"" . l:deleted_count . " file(s) deleted\""
    exec "echo \"from '" . l:archive . "'\""

    normal 

    setlocal noreadonly
    setlocal modifiable

    silent normal ggVGd

    call s:ArchiveUpdate()
    call s:ViewArchive(b:archive)
  endif
endfunction
" }}}

" Edit() {{{
" Edit the current file.
function! s:Edit ()
  call s:EditFile(b:current, "")
endfunction
" }}}

" EditFile(file, wincmd) {{{
" Edit the requested file.
function! s:EditFile (file, wincmd)
  if s:IsArchive(a:file)
    echom "View the nested archive (<CR>) to edit files in it."
    return
  endif

  " make sure editing is supported by the archive
  if exists("b:extract")
    if !s:HasCommand(b:update)
      return
    endif
  else
    if !s:HasCommand(getbufvar(bufnr(b:parent), "update"))
      return
    endif
  endif

  if (exists("b:parent") && getbufvar(bufnr(b:parent), "update") != "") || (exists("b:update") && b:update != "")
    let l:response = ""
    while l:response !~ "y" && l:response !~ "n"
      let l:response = input("edit file '" . a:file . "'? (y/n): ")
    endwhile

    if l:response =~ "^n"
      return
    endif

    if exists("b:extract")
      let l:extract = b:extract
      let l:parent = b:archive
    else
      let l:extract = getbufvar(bufnr(b:parent), "extract")
      let l:parent = b:parent
    endif

    call s:ExtractTo(l:extract, s:DeterminePath(l:parent), s:tempdir, a:file)

    let l:tempfile = s:tempdir . a:file
    let l:current = a:file

    silent exe a:wincmd
    silent exec "e! " . l:tempfile

    let b:tempfile = l:tempfile
    let b:current = l:current
    let b:parent = l:parent

    normal 

    setlocal nobuflisted
    au!
    au BufUnload * call s:ViewExit()
    au BufWritePost * call s:ArchiveUpdate()
    au WinEnter * call s:ViewFocus()
    au VimLeavePre * call s:CleanTempFiles()
  else
    echo "Operation not supported with this archive."
  endif
endfunction
" }}}

" ArchiveUpdate() {{{
" Update the archive with the edited file
function! s:ArchiveUpdate ()
  if !exists("b:parent")
    return
  endif

  let l:winnr = winnr()
  let l:parent = b:parent
  let l:tempfile = b:tempfile
  let l:update = getbufvar(bufnr(l:parent), "update")
  while l:winnr != -1 && l:parent != "" && l:tempfile != ""

    let l:parent_orig = l:parent

    let l:tempfile = strpart(l:tempfile, strlen(s:tempdir))

    let l:tempdir = substitute(s:tempdir, '\', '/', 'g')
    let l:parent = substitute(l:parent, '\', '/', 'g')

    let l:tempdir = s:DeterminePath(l:tempdir)
    let l:parent = s:DeterminePath(l:parent)

    let l:update = substitute(l:update, "%dir%", l:tempdir, "g")
    let l:update = substitute(l:update, "%archive%", l:parent, "g")
    let l:update = substitute(l:update, "%file%", l:tempfile, "g")

    "exec "echom \"exec !" . l:update . "\""
    call s:Execute("!" . l:update)

    normal 

    if v:shell_error
      exec "echoe \"Cannot update archive '" . l:parent . "'\""
    endif

    let l:winnr = bufwinnr(l:parent_orig)
    let l:bufnr = winbufnr(l:winnr)
    let l:parent = getbufvar(l:bufnr, "parent")
    let l:tempfile = getbufvar(l:bufnr, "tempfile")
    let l:update = getbufvar(bufnr(l:parent), "update")

  endwhile
endfunction
" }}}

" ExtractAllPrompt() {{{
" Function to prompt where to extract all files to.
function! s:ExtractAllPrompt ()
  let l:dir = s:PromptForExtractDir()
  if l:dir == ""
    return
  end

  call s:ExtractAll(l:dir)
endfunction
" }}}

" ExtractAll(dir) {{{
" Function to the entire archive
function! s:ExtractAll (dir)
  if s:DirExists(a:dir)
    call s:ExtractTo(b:extract, s:DeterminePath(b:archive), a:dir, "")

    normal 

    let l:archive = escape(b:archive, '\')
    let l:dir = escape(a:dir, '\')

    exec "echo \"" . l:archive . " extracted\""
    exec "echo \"To: '" . l:dir . "'\""
  endif
endfunction
" }}}

" ExtractPrompt() {{{
" Function to prompt where to extract files to.
function! s:ExtractPrompt () range
  let l:dir = s:PromptForExtractDir()
  if l:dir == ""
    return
  end

  call s:Extract(l:dir)
endfunction
" }}}

" Extract(dir) {{{
" Function to extract one or more files from the archive
function! s:Extract (dir) range
  call s:ExtractRange(a:firstline, a:lastline, a:dir)
endfunction
" }}}

" ExtractRange(line1, line2, dir) {{{
" Function to extract one or more files from the archive
function! s:ExtractRange (line1, line2, dir)
  if s:DirExists(a:dir)
    let l:extracted_count = 0
    " extract files
    let l:line = a:line1
    while line <= a:line2
      let l:file = getline(l:line)
      let l:filename = s:GetFileName(l:file)
      if l:filename != ""
        echo "extracting '" . l:filename . "'"
        let l:archive = s:DeterminePath(b:archive)
        call s:ExtractTo(b:extract, l:archive, a:dir, l:filename)

        let l:extracted_count = l:extracted_count + 1
      endif
      let l:line = l:line + 1
    endwhile

    normal 

    exec "echo \"" . l:extracted_count . " file(s) extracted\""
    exec "echo \"To: '" . s:DeterminePath(a:dir) . "'\""
  endif
endfunction
" }}}

" ExtractTo(extract, archive, dir, file) {{{
" Function to extract one or more files from the archive
function! s:ExtractTo (extract, archive, dir, file)
  let l:dir = escape(s:DeterminePath(a:dir), s:escape_chars)
  let l:archive = a:archive

  let l:extract = substitute(a:extract, "%dir%", l:dir, "")
  let l:extract =
    \ l:extract . " " .
    \ l:archive . " " .
    \ escape(a:file, s:escape_chars)

  "exec "echom \"" . l:extract . "\""
  call s:Execute("!" . l:extract)

  if v:shell_error
    normal 
    exec "echoe \"Cannot extract file '" . a:file . "'\""
  endif

endfunction
" }}}

" PromptForExtractDir() {{{
" Prompts the user for the dir to extract to.
function! s:PromptForExtractDir ()
  "get dir to extract to
  let l:dir = input("Dir to extract to (Ctrl-C to cancel): ")
  if l:dir == "" || l:dir == "."
    let l:dir = getcwd()
  endif

  return l:dir
endfunction
" }}}

" DirExists() {{{
" If dir doesn't exist, prompts the user to choose whether to create it or not.
function! s:DirExists (dir)
  let l:dir = expand(a:dir)

  let l:dir = s:DeterminePath(l:dir)

  if !isdirectory(a:dir)
    let l:response = ""
    while l:response !~ "y" && l:response !~ "n"
      let l:response = input("'" . l:dir . "' doesn't exist, create it? (y/n): ")
    endwhile

    if l:response =~ "^y"
      call s:Execute("!mkdir " . l:dir)
      if v:shell_error
        normal 
        exec "echoe \"Cannot create directory '" . l:dir . "'\""
        return 0
      endif
    else
      return 0
    endif
  endif

  return 1
endfunction
" }}}

" StartCommand(command) {{{
" Starts a : command.
function! s:StartCommand (command)
  return a:command
endfunction
" }}}

" HasCommand(command) {{{
" Checks if the archive supports the supplied command
function! s:HasCommand (command)
  if a:command == ""
    echo "Operation not supported with this archive."
    return 0
  endif
  return 1
endfunction
" }}}

" IsArchive(file) {{{
" Checks if supplied file is an archive.
function! s:IsArchive (archive)
  if a:archive =~# '\.\(jar\|war\|ear\|zip\|tar\|tar.bz2\|tgz\|tar.Z\|tar.gz\)$'
    return 1
  endif
  return 0
endfunction
" }}}

" GetFileName(line) {{{
" Get the file name from the current line.
function! s:GetFileName (line)
  " hacked way to get the file portion of the line.
  " depends on the header "Name" lining up with the files under it.
  if (a:line =~ "^\\s*[0-9]" || a:line =~ "^[-x].*[a-z].*[0-9]") && a:line !~ "/$"
    let l:line = line(".")
    normal gg
    call search("Name$", "W")
    let l:col = col(".")
    call cursor(l:line, l:col)

    if col(".") == l:col
      return strpart(a:line, col(".") - 1)
    endif
  endif

  return ""
endfunction
" }}}

" DeterminePath(path) {{{
" Determines the path for the target os.
function! s:DeterminePath (path)
  if exists("*CygwinPath")
    return CygwinPath(a:path)
  endif
  return a:path
endfunction
" }}}

" Execute (command) {{{
" Executes the supplied command.
function! s:Execute (command)
  if exists("*CygwinExecute")
    call CygwinExecute(a:command)
  else
    silent exec a:command
  endif
endfunction
" }}}

" System (command) {{{
" Executes the supplied command using vim's system() fuction.
function! s:System (command)
  if exists("*CygwinSystem")
    return CygwinSystem(a:command)
  endif
  return system(a:command)
endfunction
" }}}

" vim:ft=vim:fdm=marker
