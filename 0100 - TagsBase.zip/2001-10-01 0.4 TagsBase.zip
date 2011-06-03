" TagsBase for Vim: plugin to make a mini database of tags in the current file
" this is then used to create a menu and to offer additional 'smart'
" functionality like finding the name of the current function.
" This is a megre of the TagsMenu.vim plugin from  Jay Dickon Glanville <jayglanville@home.com>
" and the ctags.vim script from Alexey Marinichev <lyosha@lyosha.2y.net>
" Last Modified: 1 Octobre 2001
" Maintainer: Benoit Cerrina, <benoit.cerrina@writeme.com>
" Location: http://benoitcerrina.dnsalias.org/vim/TagsBase.html.
" Version: 0.4
" See the accompaning documentation file for information on purpose,
" installation, requirements and available options.


" prevent multiple loadings ...
if exists("loaded_TagsBase")
    finish
endif
let loaded_TagsBase = 1

" COMMANDS:
" commands accessible from outside the plugin to manipulate the TagsBase
" to gain access to the name of the current tag in the title string
" use this command
command! TagsBaseTitle call <SID>TBTitle()
command! -nargs=1 -complete=tag TagsBaseTag :call <SID>GoToTag('<args>')
command! TagsBaseRebuild :call <SID>TagsBase_createMenu()

" ------------------------------------------------------------------------
" AUTO COMMANDS: things to kick start the script
autocmd FileType * call <SID>TagsBase_checkFileType()
set updatetime=500

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

" Does this script produce debugging information?
let g:TagsBase_debug = 0
" A list of characters that need to be escaped
let g:TagsBase_escapeChars = "|"
" Are the tags grouped and submenued by tag type?
let g:TagsBase_groupByType = 1
" Does this script get automaticly run?
let g:TagsBase_useAutoCommand = 1

"TAGS PARSING OPTIONS:
"variables which can be used to customize the way the plugin
"parse tags beware they are very closely related one to another
"and changing the value of one and note the others is
"dangerous
"
"this is the command used to launch ctags. The --fields result in additional
"information being appended to the tag format, those are then used to build
"the menu and find the value of the tag preceding a given line
let g:TagsBase_ctagsCommand = "ctags --fields=Kn -o "
"
"this is the type of line matched by the following pattern"
"bignumClass	C:\dev\jRuby\org\jruby\Ruby.java	72;"	field	class:Ruby	file:"
"this can be overriden but the parenthesis must still have the meaning in the
"following variables
let g:TagsBase_pattern='^\([^\t]\{-}\)\t[^\t]\{-}\t\(.\{-}\);"\t\([^\t]*\)\tline:\(\d*\).*$'
let g:TagsBase_namePar='\1'
let g:TagsBase_exprPar='\2'
let g:TagsBase_typePar='\3'
let g:TagsBase_linePar='\4'



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
" s:length is the length of a field in the b:lines array
" s:length is one greater than the length of maximum line number.
let s:length = 8
" strlen(spaces) must be at least s:length.
let s:spaces = '               '



" ---------------------------------------------------------------------
"  RUNTIME CONFIGURATION:
"  some initialisation depending on the environment

if match(&shell, 'sh', '') == -1
    let s:slash='\'
else
    let s:slash='/'
endif

let s:tempDir=fnamemodify(tempname(),":p:h")

if !exists("g:CatProg") || g:CatProg == ""
    if executable("cat")
        let g:CatProg="cat"
    elseif has('win32') || has('win95') || has('win16') || has('dos32') || has('dos16')
        let g:CatProg="type"
    else
        let g: CatProg = inputdialog("Please enter location of cat program:")
    endif
endif

" ------------------------------------------------------------------------
" SCRIPT SCOPE FUNCTIONS: functions with a local script scope

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



" Creates the tag file associated with a buffer and return its name
function! s:CreateFile()
    "build file name"
    if !exists("b:fileName") || b:fileName == ""
        let dir= fnamemodify(bufname("%"),":p:h")
        let name = fnamemodify(bufname("%"), ":t")
        let b:fileName = dir . "/" . "." . name . ".tags"
        if !filewritable(b:fileName) && !filewritable(dir)
            let b:fileName = s:tempDir . "/." . name . ".tags"
        endif
    endif
    " execute the ctags command on the current file
    if !filereadable(b:fileName) || getftime(b:fileName) < getftime(@%) || g:TagsBase_debug
        let lCommand = g:TagsBase_ctagsCommand . b:fileName . " " . expand("%")
        let lCommand = substitute(lCommand, '[/\\]', s:slash, 'g')
        call s:DebugVariable( "lCommand", lCommand )
        call system( lCommand )
        let fileName = b:fileName   "local variable because we'll switch buffer
        
        silent execute "badd " . fileName
        silent execute "sbuffer " . fileName
        silent g/^!/d
        silent w
        silent execute "bwipe! " . fileName
    endif
    " create and switch to a new, temporary buffer.
    return b:fileName
endfunction



" This is the function that actually calls ctags, parses the output, and
" creates the menus.
function! s:TagsBase_createMenu() 

    call s:InitializeMenu()
    let fileName = s:CreateFile()

    "read the file in a variable
    let command = g:CatProg.' '.b:fileName
    let command = substitute(command, '[/\\]', s:slash, 'g')

    call s:DebugVariable("command", command)
    let ctags = system(command)

    " loop over the entire file, parsing each line.  Apparently, this can be
    " done with a single command, but I can't remember it.
    let whilecount = 1
    let b:lines = ''
    while strlen(ctags) > 0
        let current = strpart(ctags, 0, stridx(ctags, "\n"))
        call s:ParseTag(current)
        call s:MakeMenuEntry()

        call s:MakeTagBaseEntry()
        let whilecount = whilecount + 1
        let ctags = strpart(ctags, stridx(ctags, "\n")+1)
    endwhile

    let b:lines = b:lines."9999999"
endfunction



" Initializes the menu by erasing the old one, creating a new one, and
" starting it off with a "Rebuild" command
function! s:InitializeMenu()
    " first, lets remove the old menu
    execute "amenu " . s:menu_name . ".subname :echo\\ foo"
    execute "aunmenu " . s:menu_name

    " and now, add the top of the new menu
    execute "amenu " . s:menu_name . ".&Rebuild\\ Tags\\ Menu :call <SID>TagsBase_createMenu()<CR><CR>"
    execute "amenu " . s:menu_name . ".&Toggle\\ Title\\ Autocommand :call <SID>TBTitle()<CR><CR>"
    execute "amenu " . s:menu_name . ".-SEP- :"
endfunction

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

    let s:name = substitute(a:line, g:TagsBase_pattern, g:TagsBase_namePar , '')
    let s:expression = substitute(a:line, g:TagsBase_pattern, g:TagsBase_exprPar, '')
    let s:type = substitute(a:line, g:TagsBase_pattern, g:TagsBase_typePar, '')
    let s:line = substitute(a:line, g:TagsBase_pattern, g:TagsBase_linePar, '')

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
function! s:MakeMenuEntry()
    "copy other the name since we may need to change it
    "if the tag is overloaded
    let name = s:name
    " is this an overloaded tag?
    if name == s:previousTag
        " it is overloaded ... augment the name
        let s:repeatedTagCount = s:repeatedTagCount + 1
        let name = name . "\\ (" . s:repeatedTagCount . ")"
    else
        let s:repeatedTagCount = 0
        let s:previousTag = name
    endif

    " build the menu command
    let menu = "amenu " . s:menu_name 
    if g:TagsBase_groupByType
        let menu = menu . ".&" . s:type
    endif
    let menu = menu . ".&" . name
    if !g:TagsBase_groupByType
        let menu = menu . "<tab>" . s:type
    endif
    let menu = menu . " " . ":call <SID>GoToTag('". name . "')<CR>" 
    call s:DebugVariable( "Menu command ", menu )
    " escape some pesky characters
    " this is probably not usefull anymore since I doubt there are any
    " characters to escape in a tagname
    execute escape( menu, g:TagsBase_escapeChars )
endfunction

" Prints debugging information in the fprintf format of "%s = %s", name, value
function! s:DebugVariable(name, value)
    if g:TagsBase_debug
        echo a:name . " = " . a:value
    endif
endfunction

" This function builds an array of tag names.  b:lines contains line numbers;
" b:l<number> is the tag value for the line <number>.
function! s:MakeTagBaseEntry()
    let command = "let b:l".s:line. " = '".s:name."'"
    execute command
    let index = s:BinarySearch(s:line)
    let firstpart = strpart(b:lines, 0, s:length*index)
    let middlepart = strpart(s:line.s:spaces, 0, s:length)
    let lastpart = strpart(b:lines, s:length*index, strlen(b:lines))
    let b:lines = firstpart . middlepart . lastpart
endfunction

" This function returns the tag line for given index in the b:lines array.
function! s:GetLine(i)
    return strpart(b:lines, a:i*s:length, s:length)+0
endfunction

" This function does binary search in the array of tag names and returns
" the index of the corresponding line or the one immediately inferior.
" it is used to retrieve the tag name corresponding to a given line (cf
" s:GetTagName)
" and to keep the b:lines array sorted as it is being built
function! s:BinarySearch(curline)
    if !exists("b:lines") || match(b:lines, "^\s*9999999") != -1
        return -1
    endif

    if b:lines == ""
        return 0
    endif

    let left = 0
    let right = strlen(b:lines)/s:length

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
    return left
endfunction

"retrieves the name of the last tag defined for a given line
function! s:GetTagName(curline)
    let index = s:BinarySearch(a:curline)
    if index == -1 
        return ""
    endif
    exe "let ret=b:l".s:GetLine(index)
    return ret
endfunction

"uses vim tags facility to jump to a tag and push the tag to the tag stack.
"restores the value of tags afterward
function s:GoToTag(iTag)
    let oldTag = &tags
    let &tags=b:fileName
    execute "ta " . a:iTag
    let &tags=oldTag
endfunction


