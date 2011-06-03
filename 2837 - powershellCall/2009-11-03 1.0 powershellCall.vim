" File Description {{{
" =============================================================================
" File:             powerShellCall.vim
" Description:      Vim script permitting the use of powershell as a 'shell'
" Maintener:        Vincent Berthoux <vincent.berthoux@gmail.com>
" Usage:            <range>Phs
"                       to execute command in range and paste-back result
"
"                   <visual>Phsv
"                       execute the visual selection and paste the result.
"
"                   §§ (visual or normal mode)
"                       Prepare a command line to execute visual selection
"                       cursor line with powershell
"
" Setup:            Drop the file in your ~/.vim/plugin or vimfiles/plugin
"                   folder
"
" Configuration:
"       - let g:powershellLoadProfile = 1
"           If you want the powershell to load your profile when evaluating
"           your snippets from vim. Default is 0 (no profile loaded).
"
"       - let g:powershellFileEncoding = 'ascii'
"           Let you specify the encoding used by the powershell as output.
"           The default value is ASCII.
"           You can use the following values :
"               - ASCII
"               - Unicode (UTF-16)
"               - UTF7
"               - UTF8
"               - UTF32
"               - BigEndianUnicode
"               - Default
"               - OEM
"
"       - let g:powershellInputTempfilename = 'blablain'
"           If you want to change the name of the script temp file name.
"       - let g:powershellInputTempfilename = 'blablaout'
"           If you want to change the name of the output temp file.
" =============================================================================
" }}}
if exists("g:__POWERSHELLCALL_VIM__")
    finish
endif
let g:__POWERSHELLCALL_VIM__ = 1

" Avoid loading the plugin on anything but windows.
if !has("win32")
    finish
endif

" Boring: long param cehck {{{
if exists('g:powershellInputTempfilename')
    let s:tempFileNameIn = g:powershellInputTempfilename
else
    let s:tempFileNameIn = 'pshTmpiiii00.ps1'
endif

if exists('g:powershellOutputTempfilename')
    let s:tempFileNameOut = g:powershellOutputTempfilename
else
    let s:tempFileNameOut = 'pshTmp00000.txt'
endif

if exists('g:powershellFileEncoding')
    let s:selectedEncoding = 'g:powershellFileEncoding'
else
    let s:selectedEncoding = 'ascii'
endif

if exists('g:powershellLoadProfile')
    let s:loadProfile = ""
else
    let s:loadProfile = "-noprofile"
endif

let s:systemCall = 'powershell -outputformat text -nologo '
               \ . s:loadProfile . ' "&' . "'./" . s:tempFileNameIn . "'" . '"'
" }}}

" You may wonder, why do we need files to do this operation?
" well :
" - Powershell output Unicode with BOM, which vim doesn't really
"   appreciate.
" - Powershell have hard time accepting input from the standard
"   input...
"
" With this problem in minds, the simplest solution is to create
" a quick and dirty scripts which outputs everything in a file and
" read it after computation.
fun! PowerShellCall(visual, beg, end) range "{{{
    if a:visual
        let beginRange = line("'<")
        let endRange = line("'>")
    else
        let beginRange = a:beg
        let endRange = a:end
    endif

    let script = extend( ['$('], getline( beginRange , endRange ))
    call add( script, ')|out-file -encoding ' . s:selectedEncoding . ' ' . s:tempFileNameOut)
    call writefile( script, s:tempFileNameIn )
    " remove previous lines
    if a:visual
        '<,'>d
    else
        execute beginRange . ',' . endRange . 'd'
    endif

    call system(s:systemCall)
    execute (beginRange - 1) . ',' . (beginRange - 1) . 'r ' . s:tempFileNameOut
    call delete(s:tempFileNameOut)
    call delete(s:tempFileNameIn)
endfunction "}}}

nnoremap §§ :.,.Psh
vnoremap §§ :Pshv

command! -range=% Psh call PowerShellCall(0,<line1>,<line2> )
command! -range Pshv call PowerShellCall(1,0,0)

