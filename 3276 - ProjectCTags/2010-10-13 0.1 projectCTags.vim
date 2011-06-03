" ###########################################
" ####### Introduction ######################
" ###########################################
"
" This is a plugin for the VIM editor which enables the user to quickly and
" comfortably generate CTags for whole projects. It was tested with VIM 7.3
" only.
"
" Author: Timon Kelter <timon.kelter@tu-dortmund.de>
"
" 
" This plugin enables the quick & easy generation of CTags files for projects.
" A project is regarded as a directory structure under version control. The 
" tags are generated and searched for in the current and the project folder 
" where the project folder is the topmost folder which is still under version 
" control. Publicly available functions of this plugin are:
"
" Function to generate CTags in the current and in the surrounding project
" folder (if the project folder is different from the current one).
"
"   function GenerateProjectCTags( current_folder, variant, arguments )
"
" Function to discover and set the CTAGS file paths (will be invoked by
" autocommands for the set of configured file types)
"
"   function SetCTAGSFiles( folder )
"
" Function which automatically tries to regenerate CTAGS if they already were
" generated and the tags seem to be outdated.
"
"   function RegenerateCTAGS()
"
" SetCTAGSFiles will be invoked automatically for the configured languages
" (C,C++) but you have to manually invoke GenerateProjectCTags to create the
" CTAGS files. If 'g:projectCTagsAutogenerateTags' is set to '1', then the
" tags will automatically be updated when the source files change, otherwise
" you will have to to that manually.
"
" For a summary of the available options see the 'Options' paragraph


" ############################################
" ######## Startup Check #####################
" ############################################
"
" Has this plugin already been loaded?
"
if exists('g:projectCTagsLoadedPlugin')
  finish
endif
let g:projectCTagsLoadedPlugin = 1

" ############################################
" ######### Options ##########################
" ############################################

" The CTAGS executable
if !exists( 'g:projectCTagsCTagsExe' )
  let g:projectCTagsCTagsExe = "ctags"
endif

" The folder separator in the current operating system
if !exists( 'g:projectCTagsFolderSeparator' )
  let g:projectCTagsFolderSeparator = "/"
endif

" The time in seconds after which the project CTags file is regenerated
if !exists( 'g:projectCTagsProjectTagExpiration' )
  let g:projectCTagsProjectTagExpiration = 60 * 60 * 24 * 7
endif

" Whether outdated tag files should be regenerated automatically
" when opening a source file
if !exists( 'g:projectCTagsAutogenerateTags' )
  let g:projectCTagsAutogenerateTags = 0
endif

" The filename that is used for storing the used flags
if !exists( 'g:projectCTagsFlagFile' )
  let g:projectCTagsFlagFile = ".tags_options"
endif

" The filename that is used for storing the used flags
if !exists( 'g:projectCTagsFile' )
  let g:projectCTagsFile = ".tags"
endif

" ############################################
" ######### Startup checks ###################
" ############################################

if !executable( g:projectCTagsCTagsExe )
  echoerr 'Could not execute ctags as "' . g:projectCTagsCTagsExe . '"'
  finish
endif
   
" ############################################
" ######### Internal functions ###############
" ############################################

" Function to get the parent folder
function s:GetParentFolder( folder )
  if !isdirectory( a:folder )
    echoerr "Internal Error: Argument was not a folder!"
    return ""
  endif

  let l:last_separator = strridx( a:folder, g:projectCTagsFolderSeparator )
  return strpart( expand( a:folder ), 0, l:last_separator )
endfunction

" Function to get the project folder from a start folder on. The project
" folder is the topmost folder which contains the current one and is under
" version control.
function s:FindProjectFolder( start_folder, printWarnings )
  let l:proj_folder = a:start_folder

  " Handle VS systems which leave a VS folder in each directory
  let l:haveCVS = isdirectory( a:start_folder . g:projectCTagsFolderSeparator . "CVS" )
  let l:haveSVN = isdirectory( a:start_folder . g:projectCTagsFolderSeparator . ".svn" )
  if l:haveCVS || l:haveSVN

    if l:haveCVS
      let l:vs_folder = "CVS"
    elseif l:haveSVN
      let l:vs_folder = ".svn"
    else
      echoerr "Internal error!"
      return ""
    endif

    while isdirectory( <SID>GetParentFolder( l:proj_folder ) . g:projectCTagsFolderSeparator . l:vs_folder )
      let l:proj_folder = <SID>GetParentFolder( l:proj_folder )
    endwhile

  " Handle VS systems which only leave a single folder a the root
  else
    
    let l:found_root_folder = 0
    while isdirectory( l:proj_folder )
      if isdirectory( l:proj_folder . g:projectCTagsFolderSeparator . ".git" )
        let l:found_root_folder = 1
        break
      else
        let l:proj_folder = <SID>GetParentFolder( l:proj_folder )
      endif
    endwhile

    if l:found_root_folder
      return l:proj_folder
    else
      if a:printWarnings != 0
        echo "Could not determine versioning system - Project folder not found!"
      endif
      return ""
    endif

  endif

  return l:proj_folder
endfunction

" Function to generate ctags in the given folder
function s:GenerateCTags( folder, internal_arguments, arguments )
  let l:ctags_omni_flags = "--c++-kinds=+p --fields=+iaS --extra=+q"
  let l:ctags_exclude_flags = "--exclude='Makefile*' --exclude='.git' --exclude='CVS' --exclude='.svn'"
  let l:ctags_file = a:folder . g:projectCTagsFolderSeparator . g:projectCTagsFile
  let l:ctags_flags = "-f " . l:ctags_file . " " . l:ctags_omni_flags . " " . l:ctags_exclude_flags . " " . a:internal_arguments . " " . a:arguments
  if executable( g:projectCTagsCTagsExe ) 

    " Call ctags to create tag file
    let l:callstring = g:projectCTagsCTagsExe . " " . l:ctags_flags . " " . a:folder . g:projectCTagsFolderSeparator . "*"
    try
      let l:retval = system( l:callstring )
    catch /^Vim:Interrupt$/
      " Delete the incomplete tags file, because it will only cause confusion
      " (f.e. it may still be unsorted whereas vim expects a sorted file)
      call system( "command rm -f " . l:ctags_file )
    endtry
    if v:shell_error != 0 
      echoerr "CTags command failed: "
      echoerr "  " . l:callstring
      sleep 5

      " Delete the incomplete tags file, because it will only cause confusion
      " (f.e. it may still be unsorted whereas vim expects a sorted file)
      call system( "command rm -f " . l:ctags_file )
      return -1
    endif

    " Store the used options in hidden file
    let l:flagfile = a:folder . g:projectCTagsFolderSeparator . g:projectCTagsFlagFile
    let l:storestring = "echo '" . a:arguments. "' > " . l:flagfile
    let l:retval = system( l:storestring )
    if v:shell_error != 0 
      echoerr "Failed to store ctags options in file " . l:flagfile
      sleep 5
      return -1
    endif
    
  else

    echoerr "Could not execute '" . g:projectCTagsCTagsExe "'"
    sleep 5
    return -1

  endif

  return 0
endfunction

" Function to discover and set the CTAGS file paths (will be invoked by
" autocommands for the set of configured file types)
function SetCTAGSFiles( folder, printWarnings )

  " Remove local tags from "tags" option (if any)
  let l:tagfiles = &tags
  let l:tagfiles = substitute( l:tagfiles, "^\./tags,", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, ",\./tags", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, "^\./TAGS,", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, ",\./TAGS", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, "^tags,", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, ",tags", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, "^TAGS,", "", "g" )
  let l:tagfiles = substitute( l:tagfiles, ",TAGS", "", "g" )
  execute "set tags=" . l:tagfiles

  " Treat project-wide CTAGS file
  let l:proj_folder = <SID>FindProjectFolder( a:folder, a:printWarnings )
  if l:proj_folder != ""
    let l:tagfile_project = l:proj_folder . g:projectCTagsFolderSeparator . g:projectCTagsFile
    if filereadable( l:tagfile_project )
      if stridx( &tags, l:tagfile_project) < 0
        execute "set tags=" . l:tagfile_project . "," . &tags
      endif
    else
      echoerr "No project CTAGS available - could not open project tags file!"
      sleep 5
    endif
  else
    if a:printWarnings != 0
      echo "No project CTAGS available - could not find project folder!"
    endif
  endif

  " Treat private CTAGS file
  let l:tagfile_private = a:folder . g:projectCTagsFolderSeparator . g:projectCTagsFile
  if filereadable( l:tagfile_private )
    if stridx( &tags, l:tagfile_private) < 0
      execute "set tags=" . l:tagfile_private . "," . &tags
    endif
  else
    echoerr "No private CTAGS available - could not open local tags file!"
    sleep 5
  endif
  
endfunction

" Function for extracting the absolute directory path from the current
" buffer's file
function GetAbsBufferDir()
  return fnamemodify( expand( "%" ), ":p:h" )
endfunction

" Function to generate CTags in the directory of the current buffer and in 
" the surrounding project folder (if the project folder is different from 
" the current one)
function GenerateProjectCTags( variant, arguments )
  
  let l:current_file   = fnamemodify( expand( "%" ), ":p" )
  let l:current_folder = GetAbsBufferDir()

  " Generate all tags for current folder
  let l:tagfile_private = l:current_folder . g:projectCTagsFolderSeparator . g:projectCTagsFile
  if !filereadable( l:tagfile_private ) || ( getftime( l:tagfile_private ) < getftime( l:current_file ) )

    echo "Local   CTAGS [" . a:variant . "]: Generating in " . l:current_folder
    let l:priv_cflags = "--c++-kinds=+l --c-types=+l"
    let l:result = <SID>GenerateCTags( l:current_folder, l:priv_cflags, a:arguments )
    if l:result != 0
      return -1
    endif
  else
    echo "Local   CTAGS [" . a:variant . "]: Skipping (Tagfile is newer than source)"
  endif

  " Generate shorter tags for project folder
  let l:proj_folder = <SID>FindProjectFolder( l:current_folder, 0 )
  if l:proj_folder != ""
    let l:tagfile_project = l:proj_folder . g:projectCTagsFolderSeparator . g:projectCTagsFile

    " Only update the project file if it has a predefined age
    if ( localtime() - getftime( l:tagfile_project ) ) > g:projectCTagsProjectTagExpiration
    
      echo "Project CTAGS [" . a:variant . "]: Generating in " . l:proj_folder
      let l:proj_cflags = "-R --file-scope=no --tag-relative=yes"
      if l:proj_folder == l:current_folder
        let l:proj_cflags = "--append=yes " . l:proj_cflags
      endif
      let l:result = <SID>GenerateCTags( l:proj_folder, l:proj_cflags, a:arguments )
      if l:result != 0
        return -1
      endif

    else
      echo "Project CTAGS [" . a:variant . "]: Skipping (Tagfile is younger than " . g:projectCTagsProjectTagExpiration . " seconds)"
    endif

  else
    echo "Project CTAGS [" . a:variant . "]: Skipping (Could not find project folder)"
  endif

  call SetCTAGSFiles( l:current_folder, 0 )

  return 0
endfunction

" Function which automatically tries to regenerate CTAGS
function RegenerateCTAGS()
  " Try to locale the ctags option file in the folder
  let l:localOptionFile = GetAbsBufferDir() . g:projectCTagsFolderSeparator . g:projectCTagsFlagFile
  let l:projFolder      = <SID>FindProjectFolder( GetAbsBufferDir(), 0 )
  let l:projOptionsFile = l:projFolder . g:projectCTagsFolderSeparator . g:projectCTagsFlagFile

  if filereadable( l:localOptionFile )
    let l:flags = system( "cat " . l:localOptionFile )
  elseif l:projFolder != "" && filereadable( l:projOptionsFile )
    let l:flags = system( "cat " . l:projOptionsFile )
  else
    echo "No CTAGS option file present: Please generate CTAGS manually first"
    return -1
  endif
  " Remove trailing newline
  let l:flags = substitute( l:flags, "\n", "", "g" )

  return GenerateProjectCTags( "Stored", l:flags )
endfunction

if g:projectCTagsAutogenerateTags
  autocmd FileType c,cpp call RegenerateCTAGS()
else
  autocmd FileType c,cpp call SetCTAGSFiles( GetAbsBufferDir(), 1 )
endif
