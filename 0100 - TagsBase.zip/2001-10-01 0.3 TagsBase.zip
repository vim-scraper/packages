" TagsBase for Vim: plugin to make a mini database of tags in the current file
" this is then used to create a menu and to offer additional 'smart'
" functionality like finding the name of the current function.
" This is a megre of the TagsMenu.vim plugin from  Jay Dickon Glanville <jayglanville@home.com>
" and the ctags.vim script from Alexey Marinichev <lyosha@lyosha.2y.net>
" Last Modified: 30 Septembre 2001
" Maintainer: Benoit Cerrina, <benoit.cerrina@writeme.com>
" Location: http://benoitcerrina.dnsalias.org/vim/TagsBase.html.
" 
" See the accompaning documentation file for information on purpose,
" installation, requirements and available options.


" prevent multiple loadings ...
if exists("loaded_TagsBase")
    finish
endif
let loaded_TagsBase = 1


" ------------------------------------------------------------------------
" CONVIENCE MAPPINGS: mappings that I find useful.
" mapping to allow forced re-creation of the tags menu
""nmap <unique> <leader>t :call <SID>TagsBase_createMenu()<CR><CR>



" ------------------------------------------------------------------------
" AUTO COMMANDS: things to kick start the script
autocmd FileType * call <SID>TagsBase_checkFileType()
set updatetime=500
command! TBTitle call <SID>TBTitle()

function s:TBTitle()
    aug TBTitle
    if exists("s:titleOn") && s:titleOn
        let s:titleOn=0
        "remove autocommands for group TBTitle"
        au! 
        if exists("b:titlestring")
            let &titlestring=b:titlestring
        else
            let &titlestring=""
        endif
    else
        let s:titleOn=1
        let b:titlestring=&titlestring
        autocmd CursorHold * if exists('b:lines') | let &titlestring='%t%( %M%)%( (%{expand("%:~:.:h")})%)%( %a%)%='.<SID>GetTagName(line(".")) | endif
    endif
    aug END
endfunction


" ------------------------------------------------------------------------
" OPTIONS: can be set to define behaviour

" this is the command to get tag info.  If you don't have version 4.0.2 or
" greater of ctags, then modify this variable to remove the "--kind-long=yes" 
let g:TagsBase_ctagsCommand = "ctags -u -n --fields=Kn -o "
" Does this script produce debugging information?
let g:TagsBase_debug = 0
" A list of characters that need to be escaped
let g:TagsBase_escapeChars = "|"
" Are the tags grouped and submenued by tag type?
let g:TagsBase_groupByType = 1
" Does this script get automaticly run?
let g:TagsBase_useAutoCommand = 1



" ------------------------------------------------------------------------
" SCRIPT VARIABLES: constants and variables who's scope is limited to this
" script, but not limited to the inside of a method.

" The name of the menu
let s:menu_name = "Ta&gs"
" command to turn on magic
let s:yesmagic = ""
" command to turn off magic
let s:nomagic = ""
" the name of the previous tag recognized
let s:previousTag = ""
" the count of the number of repeated tags
let s:repeatedTagCount = 0



" This function is called everytime a filetype is set.  All it does is
" check the filetype setting, and if it is one of the filetypes recognized
" by ctags, then the TagsBase_createMenu() function is called.  However, the
" g:TagsBase_useAutoCommand == FALSE can veto the auto execution.
function! s:TagsBase_checkFileType()
    if !g:TagsBase_useAutoCommand
        return
    endif
    call s:DebugVariable( "filetype", &ft )
    " sorry about the bad form of this if statement, but apparently, the
    " expression needs to be terminated by an EOL. (I could use if/elseif...)
    if  (&ft == "asm") || (&ft == "awk") || (&ft == "c") || (&ft == "cpp") || (&ft == "sh") || (&ft == "cobol") || (&ft == "eiffel") || (&ft == "fortran") || (&ft == "java") || (&ft == "lisp") || (&ft == "make") || (&ft == "pascal") || (&ft == "perl") || (&ft == "php") || (&ft == "python") || (&ft == "rexx") || (&ft == "ruby") || (&ft == "scheme") || (&ft == "tcl") || (&ft == "vim") || (&ft == "cxx")
        call s:TagsBase_createMenu()
    endif
endfunction

if match(&shell, 'sh', '') == -1
    let s:slash='\'
else
    let s:slash='/'
endif

let s:tempDir=fnamemodify(tempname(),":p:h")

" Creates the tag file associated with a buffer and return its name
function! s:CreateFile()
    "build file name"
    let dir= fnamemodify(bufname("%"),":p:h")
    let name = fnamemodify(bufname("%"), ":t")
    let fileName = dir . "/" . "." . name . ".tags"
    if !filewritable(fileName) && !filewritable(dir)
        let fileName = s:tempDir . "/." . name . ".tags"
    endif
    " execute the ctags command on the current file
    if !filereadable(fileName) || getftime(fileName) < getftime(@%) || g:TagsBase_debug
        let command = g:TagsBase_ctagsCommand . fileName . " " . expand("%")
        let command = substitute(command, '[/\\]', s:slash, 'g')
        call s:DebugVariable( "command", command )
        let output = system( command )
        silent execute "badd " . fileName
        silent execute "sbuffer " . fileName
        silent g/^!/d
        silent w
        silent execute "bwipe! " . fileName
    endif
    " create and switch to a new, temporary buffer.
    return fileName
endfunction

if executable("cat")
    let g:CatProg="cat"
elseif executable("Type")
    let g:CatProg="Type"
endif

" This is the function that actually calls ctags, parses the output, and
" creates the menus.
function! s:TagsBase_createMenu() 

    call s:InitializeMenu()
    let fileName = s:CreateFile()

    " put the contents of local variable 'output' into a buffer
    ""    silent put! =output
    "read the file in a variable
    let command = g:CatProg.' '.fileName
    let command = substitute(command, '[/\\]', s:slash, 'g')

    call s:DebugVariable("command", command)
    let ctags = system(command)
    " Set up the nomagic and magic variables
    if &magic
        let s:yesmagic = ":set magic<CR>"
        let s:nomagic = ":set nomagic<CR>"
    endif

    " loop over the entire file, parsing each line.  Apparently, this can be
    " done with a single command, but I can't remember it.
    let len = strlen(ctags)
    let whilecount = 1
    let b:length = 8
    let b:lines = ''
    while strlen(ctags) > 0
        let current = strpart(ctags, 0, s:Stridx(ctags, "\n"))
        call s:ParseTag(current)
        call s:MakeMenuEntry( current )
        " b:length is one greater than the length of maximum line number.

        call s:MakeTagBaseEntry(current, whilecount)
        let whilecount = whilecount + 1
        " vim 5.x insists that strpart takes 3 arguments.
        let ctags = strpart(ctags, s:Stridx(ctags, "\n")+1, len)
    endwhile

    let b:lines = b:lines."9999999"
endfunction

" strlen(spaces) must be at least b:length.
let s:spaces = '               '


" ------------------------------------------------------------------------
" SCRIPT SCOPE FUNCTIONS: functions with a local script scope

" Initializes the menu by erasing the old one, creating a new one, and
" starting it off with a "Rebuild" command
function! s:InitializeMenu()
    " first, lets remove the old menu
    execute "amenu " . s:menu_name . ".subname :echo\\ foo"
    execute "aunmenu " . s:menu_name

    " and now, add the top of the new menu
    execute "amenu " . s:menu_name . ".&Rebuild\\ Tags\\ Menu :call <SID>TagsBase_createMenu()<CR><CR>"
    execute "amenu " . s:menu_name . ".&Toggle\\ Title\\ Autocommand :TBTitle<CR><CR>"
    execute "amenu " . s:menu_name . ".-SEP- :"
endfunction


"this is the type of line matched by the following pattern"
"bignumClass	C:\dev\jRuby\org\jruby\Ruby.java	72;"	field	class:Ruby	file:"
"this can be overriden but the parenthesis must still have the meaning in the
"folling variable
"old pattern, does not specify
""let g:ctags_pattern="^\\([^\t]\\{-}\\)\t[^\t]\\{-}\t\\(.\\{-}\\);\"\t\\([^\t]*\\)"
let g:ctags_pattern='^\([^\t]\{-}\)\t[^\t]\{-}\t\(.\{-}\);"\t\([^\t]*\)\tline:\(\d*\).*$'
let g:namePar='\1'
let g:exprPar='\2'
""let g:linePar=g:exprPar
let g:typePar='\3'
let g:linePar='\4'
"this function parses a tag entry and set the appropriate script variable
"s:name       name of the tag
"s:type       type of the tag
"s:expression expression used to find the tag
"s:line       line where the tag is defined
function! s:ParseTag(line)
    let s:name = ""
    let s:type = ""
    let s:expression = ""
    let s:line = ""
    if a:line[0] == "!"
        return
    endif

    let s:name = substitute(a:line, g:ctags_pattern, g:namePar , '')
    let s:expression = substitute(a:line, g:ctags_pattern, g:exprPar, '')
    let s:type = substitute(a:line, g:ctags_pattern, g:typePar, '')
    let s:line = substitute(a:line, g:ctags_pattern, g:linePar, '')

    if match( s:expression, "[0-9]" ) == 0
        " this expression is a line number not a pattern so prepend line number 
        " with : to make it an absolute line command not a relative one
        let s:expression = ":" . s:expression
    else
        let s:expression = ":0" . s:expression
    endif
endfunction

" This function takes a string (assumidly a line from a tag file format) and
" parses out the pertinent information, and makes a tag entry in the tag
" menu.
function! s:MakeMenuEntry(line)
    "lets make a few local variables to make things easy.

    " current is the current state of the line (hacked up as it is)
    let current = a:line
    if current[0] == "!"
        return
    endif
    let name = s:name
    let type = s:type
    let expression = s:expression

    " is this an overloaded tag?
    " this doesn't work since the tags are not sorted,
    " TODO replace this by another method (create a b:Tag#{name} variable
    " holding the multiplicity for each tag. The more complex thing would be
    " to unlet the variables)
    "    if name == s:previousTag
    "        " it is overloaded ... augment the name
    "        let s:repeatedTagCount = s:repeatedTagCount + 1
    "        let name = name . "\\ (" . s:repeatedTagCount . ")"
    "    else
    "        let s:repeatedTagCount = 0
    "        let s:previousTag = name
    "    endif

    " build the menu command
    let menu = "amenu " . s:menu_name 
    if g:TagsBase_groupByType
        let menu = menu . ".&" . type
    endif
    let menu = menu . ".&" . name
    if !g:TagsBase_groupByType
        let menu = menu . "<tab>" . type
    endif
    let menu = menu . " " . s:nomagic . expression . "<CR>" . s:yesmagic
    call s:DebugVariable( "Menu command ", menu )
    " escape some pesky characters
    execute escape( menu, g:TagsBase_escapeChars )
endfunction


" Prints debugging information in the fprint format of "%s = %s (%s)",
" name, value, remainder
function! s:DebugVariableRemainder(name, value, remainder)
    if g:TagsBase_debug
        "echo a:name . " = " . a:value . " (" . a:remainder . ")"
    endif
endfunction


" Prints debugging information in the fprintf format of "%s = %s", name, value
function! s:DebugVariable(name, value)
    if g:TagsBase_debug
        echo a:name . " = " . a:value
    endif
endfunction


if version < 600
    function! s:Stridx(haysack, needle)
        return match(a:haysack, a:needle)
    endfunction
else
    function! s:Stridx(haysack, needle)
        return stridx(a:haysack, a:needle)
    endfunction
endif

" This function builds an array of tag names.  b:lines contains line numbers;
" b:l<number> is the tag value for the line <number>.
function! s:MakeTagBaseEntry(line, index)
    let command = "let b:l".s:line. " = '".s:name."'"
    execute command
    let b:lines = strpart(b:lines.s:line.s:spaces, 0, b:length*a:index)
endfunction

" This function returns the tag line for given index.
function! s:GetLine(i)
    return strpart(b:lines, a:i*b:length, b:length)+0
endfunction



" This function does binary search in the array of tag names and returns
" corresponding tag.
function! s:GetTagName(curline)
    if !exists("b:lines") || match(b:lines, "^\s*9999999") != -1
        return ""
    endif

    let left = 0
    let right = strlen(b:lines)/b:length

    if a:curline < s:GetLine(left)
        return ""
    endif

    while left<right
        let middle = (right+left+1)/2
        let middleline = s:GetLine(middle)

        if middleline == a:curline
            let left = middle
            break
        endif

        if middleline > a:curline
            let right = middle-1
        else
            let left = middle
        endif
    endwhile

    exe "let ret=b:l".s:GetLine(left)
    return ret
endfunction

function Test()
endfunction


command! RebuildTM :call s:TagsBase_createMenu()
