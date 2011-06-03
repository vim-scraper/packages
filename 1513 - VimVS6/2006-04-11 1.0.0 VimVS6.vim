"*****************************************************************************
"** Name:      VimVS6.vim - VIM Visual Studio 6 C++
"**
"** Type:      global VIM plugin
"**
"** Author:    Christian Habermann
"**            christian (at) habermann-net (point) de
"**
"** Copyright: (c) 2006 by Christian Habermann
"**
"** License:   GNU General Public License 2 (GPL 2) or later
"**
"**            This program is free software; you can redistribute it
"**            and/or modify it under the terms of the GNU General Public
"**            License as published by the Free Software Foundation; either
"**            version 2 of the License, or (at your option) any later
"**            version.
"**
"**            This program is distributed in the hope that it will be
"**            useful, but WITHOUT ANY WARRANTY; without even the implied
"**            warrenty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
"**            PURPOSE.
"**            See the GNU General Public License for more details.
"**
"** Version:   1.0.0
"**            tested under Win32, GVIM 6.4
"**
"** History:   1.0.0  first public release, 11.04.2006
"**
"*****************************************************************************
"**
"** Description:
"**   This script provides a way to compile and link MS Visual Studio 6 
"**   projects, jump to compiler errors, start .exe file and look up in MSDN
"**   help files from within VIM.
"**   There are other projects that try to integrate VIM in Visual Studio.
"**   VimVS6 does it the other way around and tries to integrate Visual Studio
"**   in VIM ;-)   (don't take this too seriously)
"**
"**   Visual Studio can export a make file for nmake (see menu 
"**   Project -> Export Makefile... in Visual Studio). Nmake is included in 
"**   Visual Studio 6. VimVS6 compiles a Visual Studio 6 project using nmake
"**   and the exported makefile.
"**
"**   Usage:
"**     <F5>:    make project
"**     <S-F5>:  make all
"**     <C-F5>:  start .exe file
"**     <F2>:    search for help in MSDN files for word under cursor
"**
"**     Once per VIM session the function VVS_SetEnvironment() must be called
"**     to specify paths VimVS6 looks for the project files.
"**     Do   :call VVS_SetEnvironment( 'projectpath', 'projectname' )
"**
"**     E.g. :call VSS_SetEnvironment( 'f:\prog\GALer', 'GALer' )
"**
"**     This call is not needed if only MSDN help search is used.
"**
"**   Installation:
"**     Copy this script into the plugin directory. To search in MSDN help
"**     files the free program keyHH.exe from www.keyworks.net is needed.
"**
"**   Configuration:
"**     To configure VimVS6 the following variables can be set in the _vimrc
"**     If they are not set, defaults will be used.
"**
"**     - g:vvs_VisualStudioPath
"**       Defines the path to the Visual Studio installation. Default is:
"**          let g:vvs_VisualStudioPath = 'c:\programme\microsoft visual studio'
"**
"**     - g:vvs_MSDN
"**       Defines the path and filename of the top MSDN help file. The MSDN help file
"**       is msdnXXX.col (XXX any characters and numbers). Look for this file and
"**       set g:vvs_MSDN accordingly. There is a default, but path/name depends on your
"**       Visual Studio installation, so there is a very good change that the default is
"**       wrong for your installation.
"**          let g:vvs_MSDN = 'c:\programme\microsoft visual studio\msdn98\98vs\1031\msdnvs98.col'
"**
"**     - <Plug>VSS_StartMake
"**       mapping to start make process
"**       default:
"**          map <silent> <F5> <Plug>VVS_StartMake
"**
"**     - <Plug>VSS_StartMakeAll
"**       mapping to start make all process
"**       default:
"**          map <silent> <S-F5> <Plug>VVS_StartMakeAll
"**
"**     - <Plug>VSS_StartExe
"**       mapping to start debug version of .exe file
"**       default:
"**          map <silent> <C-F5> <Plug>VVS_StartExe
"**
"**     - <Plug>VSS_SearchHelp
"**       mapping to search in help files (MSDN)
"**       default:
"**          map <silent> <F2> <Plug>VVS_SearchHelp
"**
"**
"**   Known limitations:
"**     none
"**
"**   Known bugs:
"**     none - well, up to now :-)
"**
"**
"**   Happy vimming....
"*****************************************************************************

" allow user to avoid loading this plugin and prevent loading twice
if exists ("vvs_VimVS6Loaded")
    finish
endif

let vvs_VimVS6Loaded = 1


"*****************************************************************************
"*************************           U S E R           ***********************
"*************************  C O N F I G U R A T I O N  ***********************
"*****************************************************************************

if !exists('g:vvs_VisualStudioPath')   " path to Visual Studio installation
    let g:vvs_VisualStudioPath = 'c:\programme\microsoft visual studio'
endif

if !exists('g:vvs_MSDN')               " path to the MSDN main help file, filename is msdnXXX.col
    let g:vvs_MSDN = 'c:\programme\microsoft visual studio\msdn98\98vs\1031\msdnvs98.col'
endif


" the key mappings:
if !hasmapto('<Plug>VVS_StartMake')
    map <silent> <F5> <Plug>VVS_StartMake
endif

if !hasmapto('<Plug>VVS_StartMakeAll')
    map <silent> <S-F5> <Plug>VVS_StartMakeAll
endif

if !hasmapto('<Plug>VVS_StartExe')
    map <silent> <C-F5> <Plug>VVS_StartExe
endif

if !hasmapto('<Plug>VVS_SearchHelp')
    map <silent> <F2> <Plug>VVS_SearchHelp
endif




map <silent> <script> <Plug>VVS_StartMake    :call <SID>VVS_StartMake( 0 )<CR>

map <silent> <script> <Plug>VVS_StartMakeAll :call <SID>VVS_StartMake( 1 )<CR>

map <silent> <script> <Plug>VVS_StartExe     :call <SID>VVS_StartExe()<CR>

map <silent> <script> <Plug>VVS_SearchHelp   :call <SID>VVS_SearchHelp( expand("<cword>") )<CR>



"*****************************************************************************
"************************* I N I T I A L I S A T I O N ***********************
"*****************************************************************************

" used to print name of script
let s:scriptName     = "VimVS6"

let s:strProjectPath = ""
let s:strProjectName = ""

let s:boInitialized  = 0


" some error IDs for s:Error() function
let s:ERR_PATH_NOT_FOUND = 1
let s:ERR_FILE_NOT_FOUND = 2
let s:ERR_NOT_INIT       = 3
let s:ERR_EXE_NOT_FOUND  = 4



"********************************** Setup ************************************

let $PATH=$PATH.';'.g:vvs_VisualStudioPath.'\vc98\bin'
let $PATH=$PATH.';'.g:vvs_VisualStudioPath.'\Common\MSDev98\bin'

let $INCLUDE=$INCLUDE.';'.g:vvs_VisualStudioPath.'\vc98\Include'
let $INCLUDE=$INCLUDE.';'.g:vvs_VisualStudioPath.'\vc98\ATL\Include'
let $INCLUDE=$INCLUDE.';'.g:vvs_VisualStudioPath.'\vc98\MFC\Include'

let $LIB=$LIB.';'.g:vvs_VisualStudioPath.'\vc98\Lib'
let $LIB=$LIB.';'.g:vvs_VisualStudioPath.'\vc98\MFC\Lib'




"*****************************************************************************
"** input:   strProjectPath:  path of Visual Studio project                 
"**          strProjectName:  name of Visual Studio project                 
"** output:  none                                                           
"*****************************************************************************
"** remarks:                                                                
"**   Setup some variables so that the script functions can find the make   
"**   file, exe-file...                                                     
"**   E.g.  :call VVS_SetEnvironment( 'f:\prog\GALer', 'GALer' )            
"*****************************************************************************
function VVS_SetEnvironment( strProjectPath, strProjectName )
    let s:boInitialized = 1

    let s:strProjectPath = a:strProjectPath
    let s:strProjectName = a:strProjectName
    
    let s:strMakeFile    = s:strProjectPath.'\'.s:strProjectName.'.mak'
    let s:strExeFile     = s:strProjectPath.'\debug\'.s:strProjectName.'.exe'


    set makeprg=nmake
    set errorformat&
endfunction




"*****************************************************************************
"** input:   boMakeAll: 0 make only outdated files
"**                     1 make alle files
"** output:  none
"**
"** remarks:
"**   Call make.
"*****************************************************************************
function <SID>VVS_StartMake( boMakeAll )

    if s:boInitialized == 1

        let l:strBackupCurrentPath = getcwd()

        echo "making ".s:strMakeFile

        if getfsize( s:strProjectPath ) == 0
            if getfsize( s:strMakeFile ) != -1
                execute ":cd ".s:strProjectPath

                if a:boMakeAll == 1
                    silent execute "make /A /f "."\"".s:strMakeFile."\"" 
                else
                    silent execute "make /f "."\"".s:strMakeFile."\""
                endif
                
                execute ":cd ".l:strBackupCurrentPath
            else
                call s:Error( s:ERR_FILE_NOT_FOUND, s:strMakeFile )
            endif
        else
          call s:Error( s:ERR_PATH_NOT_FOUND, s:strProjectPath )
        endif
    else
        call s:Error( s:ERR_NOT_INIT, "" )   " not initialized
    endif
    
endfunction




"*****************************************************************************
"** input:   none
"** output:  none
"**
"** remarks:
"**   start the exe file
"*****************************************************************************
function <SID>VVS_StartExe()

    if s:boInitialized == 1
        if getfsize( s:strExeFile ) != -1
            silent execute '!cmd.exe /cstart "" '."\"".s:strExeFile.'"'
        else
            call s:Error( s:ERR_EXE_NOT_FOUND, s:strExeFile )   " exe file not found
        endif
    else
        call s:Error( s:ERR_NOT_INIT, "" )   " not initialized
    endif

endfunction




"*****************************************************************************
"** input:   strHelp:  what to look for
"** output:  none
"**
"** remarks:
"**   search in MSDN help
"*****************************************************************************
function <SID>VVS_SearchHelp( strHelp )
    execute ':silent!!cmd.exe /cstart keyhh.exe -\#klink '.a:strHelp.' '.g:vvs_MSDN
endfunction




"*****************************************************************************
"** input:   errID: number which defines an error (> 0), see s:ERR_xxx
"**          str:   additional text used for some error IDs
"** output:  none
"**
"** remarks:
"**   this function prints an error-msg
"*****************************************************************************
function s:Error(errID, str)
    
    echohl WarningMsg
    
    if ( a:errID == s:ERR_PATH_NOT_FOUND )
        echo s:scriptName.": path does not exist ".a:str
    elseif ( a:errID == s:ERR_FILE_NOT_FOUND )
        echo s:scriptName.": make file does not exist ".a:str
    elseif ( a:errID == s:ERR_NOT_INIT )
        echo s:scriptName.": call VVS_SetEnvironment(\'projectpath\', \'projectname\') before using this function"
    elseif ( a:errID == s:ERR_EXE_NOT_FOUND )
        echo s:scriptName.": .exe file does not exist ".a:str
    endif

    echohl None
endfunction


" EOF
