" writebackup.vim: Write backups of current file with date file extension.  
"
" DESCRIPTION:
"   This is a poor man's revision control system, a primitive alternative to
"   CVS, RCS, Subversion, etc., which works with no additional software and
"   almost any file system. 
"   The ':WriteBackup' command writes subsequent backups of the current file
"   with a 'current date + counter' file extension (format '.YYYYMMDD[a-z]').  
"   The first backup of a day has letter 'a' appended, the next 'b', and so on.
"   (Which means that a file can be backed up up to 26 times on any given day.) 
"
"   By default, backups are created in the same directory as the original file,
"   but they can also be placed in a directory relative to the original file, or
"   in one common backup directory for all files (similar to VIM's 'backupdir'
"   option), or even in a file-specific location that is determined via a
"   user-provided callback function. 
"
" USAGE:
"   :WriteBackup
"
" INSTALLATION:
"   Put the script into your user or system VIM plugin directory (e.g.
"   ~/.vim/plugin). 
"
" DEPENDENCIES:
"   - Requires VIM 7.0 or higher. 
"   - writebackupVersionControl.vim (vimscript #1829) complements this script,
"     but is not required. 
"
" CONFIGURATION:
"   For a permanent configuration, put the following commands into your vimrc
"   file (see :help vimrc). 
"						      *g:WriteBackup_BackupDir*
"   To put backups into another directory, specify a backup directory via
"	let g:WriteBackup_BackupDir = 'D:\backups'
"   Please note that this setting may result in name clashes when backing up
"   files with the same name from different directories!
"
"   A directory starting with './' or '../' (or the backslashed-variants '.\'
"   for MS-DOS et al.) puts the backup file relative to where the backed-up file
"   is.  The leading '.' is replaced with the path name of the current file:
"	let g:WriteBackup_BackupDir = './backups'
"
"   Backup creation will fail if the backup directory does not exist, the
"   directory will NOT be created automatically! 
"
"						 *writebackup-dynamic-backupdir*
"   If you want to automatically create a non-existing backup directory,
"   dynamically determine the backup directory based on the current filespec or
"   any other changing circumstances, you can set a custom callback function:
"
"	function MyResolveBackupDir(originalFilespec, isQueryOnly)
"	    ...
"	    return backupDirspec
"	endfunction
"	let g:WriteBackup_BackupDir = function('MyResolveBackupDir')
"
"   This function will be invoked each time a backup is about to be written.
"   The function must accept one String argument that represents the filespec of
"   the original file (the filespec can be relative or absolute, like the output
"   of expand('%')), and one Number that represents a boolean flag whether this
"   is just a query (no backup is about to be written, so don't cause any
"   permanent side effects).
"   It must return a String representing the backup dirspec (again either
"   relative or absolute, '.' for current directory, please no trailing path
"   separator). 
"   Throw an exception if you want to abort the backup. If the exception starts
"   with 'WriteBackup:', the rest of the exception text will be nicely printed
"   as the error text to the user. 
"
"   Remember that because of the alphabetic numbering, it doesn't make much
"   sense if the backup directory changes for subsequent backups of the same
"   file. Use this functionality to adapt the backup location based on filespec,
"   file type, availability of a backup medium, etc., or to inject additional
"   side effects like creating backup directories, pruning old backups, etc. 
"
"						      *b:WriteBackup_BackupDir*
"   You can override this global setting for specific buffers via a
"   buffer-scoped variable, which can be set by an autocmd, ftplugin, or
"   manually: 
"	let b:WriteBackup_BackupDir = 'X:\special\backup\folder'
"
"
"							    *writebackup-alias*
"   In case you already have other custom VIM commands starting with W, you can
"   define a shorter command alias ':W' in your .vimrc to save some keystrokes.
"   I like the parallelism between ':w' for a normal write and ':W' for a backup
"   write. 
"	command -bar W :WriteBackup
"
" INTEGRATION:
" LIMITATIONS:
" ASSUMPTIONS:
" KNOWN PROBLEMS:
" TODO:
"
" Copyright: (C) 2007-2009 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
" REVISION	DATE		REMARKS 
"   1.30.011	11-Feb-2009	BF: On Unix, fnamemodify() doesn't simplify the
"				'/./' part; added explicit simplify() call. 
"   1.30.010	24-Jan-2009	BF: Unnamed buffers were backed up as
"				'.YYYYMMDDa'; now checking for empty original
"				filespec and throwing exception. 
"				BF: Now also allowing relative backup dir
"				in an upper directory (i.e.
"				g:WriteBackup_BackupDir starting with '../'. 
"   1.30.009	23-Jan-2009	ENH: The backup directory can now be determined
"				dynamically through a callback function. 
"				Renamed configuration variable from
"				g:writebackup_BackupDir to
"				g:WriteBackup_BackupDir. 
"   1.20.008	16-Jan-2009	Now setting v:errmsg on errors. 
"   1.20.007	21-Jul-2008	BF: Using ErrorMsg instead of Error highlight
"				group. 
"   1.20.006	13-Jun-2008	Added -bar to :WriteBackup, so that commands can
"				be chained together. 
"   1.20.005	18-Sep-2007	ENH: Added support for writing backup files into
"				a different directory (either one static backup
"				dir or relative to the original file) via
"				g:writebackup_BackupDir configuration, as
"				suggested by Vincent DiCarlo. 
"				Now requiring VIM 7.0 or later, because it's
"				using lists. 
"				BF: Special ex command characters ' \%#' must be
"				escaped for ':w' command. 
"   1.00.004	07-Mar-2007	Added documentation. 
"	0.03	06-Dec-2006	Factored out WriteBackup_GetBackupFilename() to
"				use in :WriteBackupOfSavedOriginal. 
"	0.02	14-May-2004	Avoid that the written file becomes the
"				alternate file (via set cpo-=A)
"	0.01	15-Nov-2002	file creation

" Avoid installing twice or when in compatible mode
if exists('g:loaded_writebackup') || (v:version < 700)
    finish
endif
let g:loaded_writebackup = 120

if ! exists('g:WriteBackup_BackupDir')
    let g:WriteBackup_BackupDir = '.'
endif

function! s:GetSettingFromScope( variableName, scopeList )
    for l:scope in a:scopeList
	let l:variable = l:scope . ':' . a:variableName
	if exists( l:variable )
	    execute 'return ' . l:variable
	endif
    endfor
    throw "No variable named '" . a:variableName . "' defined. "
endfunction

function! WriteBackup_GetBackupDir( originalFilespec, isQueryOnly )
    if empty(a:originalFilespec)
	throw 'WriteBackup: No file name'
    endif
    let l:BackupDir = s:GetSettingFromScope( 'WriteBackup_BackupDir', ['b', 'g'] )
    if type(l:BackupDir) == type('')
	return l:BackupDir
    else
	return call(l:BackupDir, [a:originalFilespec, a:isQueryOnly])
    endif
endfunction

function! WriteBackup_AdjustFilespecForBackupDir( originalFilespec, isQueryOnly )
    let l:backupDir = WriteBackup_GetBackupDir(a:originalFilespec, a:isQueryOnly)
    if l:backupDir == '.'
	" The backup will be placed in the same directory as the original file. 
	return a:originalFilespec
    endif

    let l:originalDirspec = fnamemodify( a:originalFilespec, ':p:h' )
    let l:originalFilename = fnamemodify( a:originalFilespec, ':t' )

    let l:adjustedDirspec = ''
    " Note: On Windows, fnamemodify( 'path/with/./', ':p' ) will convert the
    " forward slashes to backslashes by triggering a path simplification of the
    " '/./' part. On Unix, simplify() will get rid of the '/./' part. 
    if l:backupDir =~# '^\.\.\?[/\\]'
	" Backup directory is relative to original file. 
	" Modify dirspec into something relative to CWD. 
	let l:adjustedDirspec = fnamemodify( simplify(fnamemodify( l:originalDirspec . '/' . l:backupDir . '/', ':p' )), ':.' )
    else
	" One common backup directory for all original files. 
	" Modify dirspec into an absolute path. 
	let l:adjustedDirspec = simplify(fnamemodify( l:backupDir . '/./', ':p' ))
    endif
    if ! isdirectory( l:adjustedDirspec ) && ! a:isQueryOnly
	throw "WriteBackup: Backup directory '" . fnamemodify( l:adjustedDirspec, ':p' ) . "' does not exist!"
    endif
    return l:adjustedDirspec . l:originalFilename
endfunction

function! WriteBackup_GetBackupFilename( originalFilespec )
    let l:date = strftime( "%Y%m%d" )
    let l:nr = 'a'
    while( l:nr <= 'z' )
	let l:backupFilespec = WriteBackup_AdjustFilespecForBackupDir( a:originalFilespec, 0 ) . '.' . l:date . l:nr
	if( filereadable( l:backupFilespec ) )
	    " Current backup letter already exists, try next one. 
	    " Vim script cannot increment characters; so convert to number for increment. 
	    let l:nr = nr2char( char2nr(l:nr) + 1 )
	    continue
	endif
	" Found unused backup letter. 
	return l:backupFilespec
    endwhile

    " All backup letters a-z are already used; report error. 
    throw 'WriteBackup: Ran out of backup file names'
endfunction

function! s:WriteBackup()
    let l:saved_cpo = &cpo
    set cpo-=A
    try
	let l:backupFilespecInVimSyntax = escape( tr( WriteBackup_GetBackupFilename(expand('%')), '\', '/' ), ' \%#')
	execute 'write ' . l:backupFilespecInVimSyntax
    catch /^WriteBackup:/
	echohl ErrorMsg
	let v:errmsg = substitute(v:exception, '^WriteBackup:\s*', '', '')
	echomsg v:errmsg
	echohl None
    catch /^Vim\%((\a\+)\)\=:E/
	echohl ErrorMsg
	let v:errmsg = substitute(v:exception, '^Vim\%((\a\+)\)\=:', '', '')
	echomsg v:errmsg
	echohl None
    finally
	let &cpo = l:saved_cpo
    endtry
endfunction

command! -bar WriteBackup call <SID>WriteBackup()

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
