
"//--------COMMENTS------------------------------------------------------------------------
"this plugin is destine to people, which would like to save list of opened
"files on vim exit, and open them on vim enter.
"Tested on Win32 platform, but on Linux it's must work too.

"version 0.2 


"skip load plugin if diff mode
if !&diff 

if v:version < 700
	finish
endif

"there is three variable which you may config:

"let g:BufferReminder_SaveOpenedFilesListBUFFER=1  (def)       - if you like buffer one variant
"let g:BufferReminder_SaveOpenedFilesListBUFFER=0              - if you like tabs open variant

"let g:BufferReminder_openedFileBuffersList="pathToFileName"   - if you dont like default path of file

" this option work only in tabs mode
"let g:BufferReminder_removeNoNameBuffer=1    (def)            - if you hate NoName buffer, in this case it's satisfy you on 100% !
"let g:BufferReminder_removeNoNameBuffer=0                     - in this case, 
""we will spare NoName buffer 
"

"
"//---------------------------------------------------------------------------------
"this function open files on start and save them to the list on close
"//---------------------------------------------------------------------------------
"//--------------------------------------------------------------------------------- 
 

"//---------------------------------------------------------------------------------
if !exists('g:BufferReminder_AllSyntaxOn') 
  let g:BufferReminder_AllSyntaxOn = 1
endif

"//---------------------------------------------------------------------------------
if !exists('g:BufferReminder_removeNoNameBuffer') 
  let g:BufferReminder_removeNoNameBuffer = 1
endif

"//---------------------------------------------------------------------------------
if !exists('g:BufferReminder_openedFileBuffersList') 
    if has('unix') || has('macunix')
        let g:BufferReminder_openedFileBuffersList = $HOME . "/vim_OpenedFilesList.txt"
    else
        let g:BufferReminder_openedFileBuffersList = $VIM .  "/vim_OpenedFilesList.txt"
        if has('win32')
            if $USERPROFILE != ''
                let g:BufferReminder_openedFileBuffersList = $USERPROFILE . "\\vim_OpenedFilesList.txt"
            endif
        endif
    endif
endif

"//---------------------------------------------------------------------------------
if !exists('g:BufferReminder_SaveOpenedFilesListBUFFER') 
  let g:BufferReminder_SaveOpenedFilesListBUFFER = 0
endif 

"//--------------------------------------------------------------------------------- 
func! s:SaveOpenedFilesList() 
"//--------------------------------------------------------------------------------- 
 
    let l:listFromFile = [] 
    let l:fullFilename = "" 
    let l:i = 0 
    let l:addFlag = 1 


    if filereadable(g:BufferReminder_openedFileBuffersList) == 1     
        call delete(g:BufferReminder_openedFileBuffersList) 
    endif 
 
    while(l:i <= bufnr('$')) 
 
        let l:fullFilename = fnamemodify(bufname(i), ':p') 
 
        if  ( buflisted(l:i) != 0 && l:fullFilename != "" && bufname(i) != "") 
            ""check for duplicate
            let l:addFlag = 1 
            for item in l:listFromFile
                if l:fullFilename == item
                    let l:addFlag = 0 
                    break
                endif
            endfor

            if l:addFlag == 1 
                call add(l:listFromFile,            l:fullFilename) 
            endif

        endif 
            let l:i = l:i + 1 
    endwhile 
 
    if len(l:listFromFile) > 0 

        let l:reminderVersion = " V.0.5 "
        let l:informationString = "Buffer Reminder" . l:reminderVersion . strftime("%d.%m.%Y %X")  
        call insert(l:listFromFile, l:informationString)

        call writefile(l:listFromFile, g:BufferReminder_openedFileBuffersList) 
    endif 
 
endfunc 
"//--------------------------------------------------------------------------------- 
func! s:OpenOpenedFilesList() 
"//--------------------------------------------------------------------------------- 
 

    if filereadable(g:BufferReminder_openedFileBuffersList) == 1     

        let l:i = 0 
        let l:listFromFile  = readfile(g:BufferReminder_openedFileBuffersList) 
        let l:lenthOfList = len(l:listFromFile)

        let l:firstLaunch = 1

        "this is our file ?(check for abbreviation)
        if ( l:lenthOfList < 2 )
            echo "file is too short to read"
            return -1
        else
            if s:ThisIsOurFile(l:listFromFile[l:i]) == -1
                return -1
            else
                let l:i = l:i + 1
            endif
        endif
 
       "let's open files
        while ( l:i < l:lenthOfList ) 
            if l:firstLaunch && g:BufferReminder_removeNoNameBuffer == 1
                exe "e " . l:listFromFile[l:i] 
                let l:firstLaunch = 0
            else
                if g:BufferReminder_SaveOpenedFilesListBUFFER == 1
                   exe "e " . l:listFromFile[l:i] 
                else
                   exe "tabedit " . l:listFromFile[l:i]
               endif
            endif
                let l:i = l:i + 1 
        endwhile 
        call delete(g:BufferReminder_openedFileBuffersList) 

    else 
        echo "Buffer Reminder: there is no file, skipping ..." 
    endif 
endfunc 
"//---------------------------------------------------------------------------------
func! s:ThisIsOurFile(argument1)
"//---------------------------------------------------------------------------------
" 'Buffer' and 'Reminder' is keys words
try
    let l:InformationList = split(a:argument1)
    if(l:InformationList[0] == "Buffer" && l:InformationList[1] == "Reminder")
        return 1
    else
        echo "Buffer Reminder: wrong information line"
        "echo l:InformationList
        return -1
    endif
catch
    echo "Buffer Reminder: wrong file format"
    "echo v:exception
endtry

endfunc
"//--------------------------------------------------------------------------------- 

autocmd VimLeavePre *        call s:SaveOpenedFilesList() 
autocmd VimEnter    * nested call s:OpenOpenedFilesList()
"//---------------------------------------------------------------------------------

endif
