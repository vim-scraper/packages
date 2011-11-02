
"//--------COMMENTS------------------------------------------------------------------------
"this plugin is destine to people, which would like to save list of opened
"files on vim exit, and open them on vim enter.
"Tested on Win32 platform, but on Linux it's must work too.

"version 0.2 


"skip load plugin if diff mode
if &diff 
     silent echo "skip"
else

if v:version < 700
	finish
endif

syntax on

"there is three variable which you may config:

"let g:SaveOpenedFilesListBUFFER=1              - if you like buffer one variant
"let g:SaveOpenedFilesListBUFFER=0              - if you like tabs open variant

"let g:openedFileBuffersList="pathToFileName"   - if you dont like default path of file

"let g:removeNoNameBuffer=1                     - if you hate NoName buffer, in this case it's satisfy you on 100% !
"let g:removeNoNameBuffer=0                     - in this case, we will spare NoName buffer

"
"//---------------------------------------------------------------------------------
"this function open files on start and save them to the list on close
"//---------------------------------------------------------------------------------
"//--------------------------------------------------------------------------------- 
 
"//---------------------------------------------------------------------------------
if !exists('g:removeNoNameBuffer') 
  let g:removeNoNameBuffer = 1
endif

"//---------------------------------------------------------------------------------
if !exists('g:openedFileBuffersList') 
    if has('unix') || has('macunix')
        let g:openedFileBuffersList = $HOME . "/vim_OpenedFilesList.txt"
    else
        let g:openedFileBuffersList = $VIM .  "/vim_OpenedFilesList.txt"
        if has('win32')
            if $USERPROFILE != ''
                let g:openedFileBuffersList = $USERPROFILE . "\\vim_OpenedFilesList.txt"
            endif
        endif
    endif
endif

"//---------------------------------------------------------------------------------
if !exists('g:SaveOpenedFilesListBUFFER') 
  let g:SaveOpenedFilesListBUFFER = 0
endif 

"//--------------------------------------------------------------------------------- 
func! s:SaveOpenedFilesList() 
"//--------------------------------------------------------------------------------- 
 
    let l:ListVariable = [] 
    let l:i = 0 
    let l:bName = "" 
    let l:fullFilename = "" 
 
    if filereadable(g:openedFileBuffersList) == 1     
        call delete(g:openedFileBuffersList) 
    endif 
 
    while(l:i <= bufnr('$')) 
 
        let l:bName = bufname(i) 
 
        if  ( buflisted(l:i) != 0 && l:bName != "openedFileList.txt" && l:bName != "" ) 
 
            let l:fullFilename = fnamemodify(bName, ':p') 
            call add( l:ListVariable, l:fullFilename ) 
            "call add( l:ListVariable, expand("#" . bufname(l:i) . ":p") ) 
        endif 
 
            let l:i = l:i + 1 
    endwhile 
 
    if len(l:ListVariable) > 0 
        let l:reminderVersion = " V.0.3 "
        let l:informationString = "Buffer Reminder" . l:reminderVersion . strftime("%d.%m.%Y %X")  
        call insert(l:ListVariable, l:informationString)
        call writefile(l:ListVariable, g:openedFileBuffersList) 
    endif 
 
endfunc 
"//--------------------------------------------------------------------------------- 
func! s:OpenOpenedFilesList() 
"//--------------------------------------------------------------------------------- 
 
    if filereadable(g:openedFileBuffersList) == 1     
 
        let l:i = 0 
        let l:ListOpenedBuffers = []
        let l:fileLinesList  = readfile(g:openedFileBuffersList) 
        let l:lenthOfList = len(l:fileLinesList)

        "this is our file ?(check for abbreviation)
        if ( l:lenthOfList < 2 )
            echo "file is too short to read"
            return -1
        else
            if s:ThisIsOurFile(l:fileLinesList[l:i]) == -1
                return -1
            else
                let l:i = l:i + 1
            endif
        endif
 


           "let's open files
            while ( l:i < l:lenthOfList ) 

                    "check for duplicate
                    for item in l:ListOpenedBuffers
                        if l:fileLinesList[l:i] == item
                            let l:i = l:i + 1
                            continue
                        endif
                    endfor

 
                    "what type of open we prefer?
                    if g:SaveOpenedFilesListBUFFER == 1
                        exe "e " . l:fileLinesList[l:i] 
                    else
                        exe "tabedit " . l:fileLinesList[l:i] 
                    endif

                        let l:extention = expand("%:e")

                        "extention fix
                        "if no ext, set vim hi"
                        if l:extention == ''
                            let l:extention = "vim"
                        endif
                        exe "set syntax=" . l:extention 

                    call add(l:ListOpenedBuffers, l:fileLinesList[l:i])
                    let l:i = l:i + 1 
 
            endwhile 
            call delete(g:openedFileBuffersList) 
    else 
        echo "Buffer Reminder: there is no file, skipping ..." 
    endif 
endfunc 
"//---------------------------------------------------------------------------------
func! s:ThisIsOurFile(argument1)
"//---------------------------------------------------------------------------------
" 'Buffer' and 'Reminder' is keys words
    let l:InformationList = split(a:argument1)
    if(l:InformationList[0] == "Buffer" && l:InformationList[1] == "Reminder")
        return 1
    else
        echo "Buffer Reminder: wrong information line"
        "echo l:InformationList
        return -1
    endif

endfunc
"//--------------------------------------------------------------------------------- 
func! s:removeNoNameBuffer()
"//---------------------------------------------------------------------------------

    "ByeBye NoName buffer !
    if g:removeNoNameBuffer == 1
        exe "bw 1"
    endif
 
endfunc
"//---------------------------------------------------------------------------------

"//---------------------------------------------------------------------------------
autocmd VimLeavePre * :call s:SaveOpenedFilesList() 
autocmd VimEnter    * :call s:OpenOpenedFilesList() 
autocmd VimEnter    * :call s:removeNoNameBuffer() 
"//---------------------------------------------------------------------------------

endif
