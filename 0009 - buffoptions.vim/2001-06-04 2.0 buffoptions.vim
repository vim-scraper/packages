" buffoptions.vim  : Per-Buffer options/maps/menus
" Author: Michael Geddes<michaelrgeddes@optushome.com.au>
" Version: 2.0


" Push options onto a stack - this sets the new value and remembers the old values
" The list is a series of "variable", "value" pairs.
" 'variable' can also be of the form '[nvoic]*map <LHS>' 
"    eg 'vmap <f4>'
" or of the form '[anvoic]*menu <LHS>' 
"   eg 'vmenu Tools.Test' or 'vmenu 100.100 Tools.Test'
"


fun! PushOption( ... )
	if !exists("b:optionStack") 
	  let b:optionStack="\n"
	endif
	let c=1
	let sep="\n"
	while c < a:0
	  exe 'let option=a:'.c
	  let ax='^\(i\=abbr\(ev\)\=\)\s\+'
	  let mx='^!\=\([nvoic]*\)map\s\+'
	  let menx='^!\=\([anvoic]*\)menu\s\+'
	  if option =~ mx
		let modes=substitute(matchstr(option,mx),mx,'\1','')
		if option[0] == '!'
		  let nore='nore'
		else
		  let nore=''
		endif
		let option=substitute(option,mx,'','')
		let ma=0
		while ma < strlen(modes)
		  let rhs=maparg(option,modes[ma])
		  if rhs==""
			let b:optionStack=modes[ma].'unmap '.option.sep.b:optionStack
		  else
			let b:optionStack=modes[ma].'map '.option.' '.rhs.sep.b:optionStack
		  endif
		  exe 'let nrhs=a:'.(c+1)
		  exe modes[ma].nore.'map '.option.' '.nrhs
		  let ma=ma+1
		endwhile
	  elseif option =~ ax
		" At the moment we can't silently get the old abbreviation, and it will
		" be a rare thing for it to be needed
		let lhs=substitute(option,ax,'','')
		let b:optionStack='iuna '.lhs.sep.b:optionStack
		exe 'let rhs=a:'.(c+1)
		exe 'iabbr '.lhs.' '.rhs
	  elseif option =~ menx
		" We can't silently get the old menus, which is a problem at the moment.
		let modes=substitute(matchstr(option,menx),menx,'\1','')
		if option[0] == '!'
		  let nore='nore'
		else
		  let nore=''
		endif
		let option=substitute(option,menx,'','')
		let ma=0
		while ma < strlen(modes)
"		  let rhs=menuarg(option,modes[ma])  " Don't have this :(
"		  if rhs==""
		  let b:optionStack=modes[ma].'unmenu '.option.sep.b:optionStack
"		  else
"		    let b:optionStack=modes[ma].'menu '.option.' '.rhs.sep.b:optionStack
"		  endif
		  exe 'let nrhs=a:'.(c+1)
		  exe modes[ma].nore.'menu '.option.' '.nrhs
		  let ma=ma+1
		endwhile
	  else
		" try not to remember any of the per-buffer options
		if option !~ '\<\(ts\|sw\|tabstop\|shiftwidth\|softtabstop\|sts\)\>'
		  exe 'let curval=&'.option
		  let b:optionStack='let &'.option."='".curval."'".sep.b:optionStack
		endif
		exe 'let &'.option.'=a:'.(c+1)
	  endif
	  let c=c+2
	endwhile
endfun

" Restore the options pushed onto the stack
fun! RestoreOptions()
  if exists("b:optionStack")
	let mx="^\n\\=[^\n]*"
	while b:optionStack != ''
	  let erm=v:errmsg
	  let v:errmsg=""
	  exe matchstr(b:optionStack,mx)
	  if v:errmsg!="" 
		echohl ErrorMsg
		echo 'In Line: '.matchstr(b:optionStack,mx)
		echohl None
	  else
		let v:errmsg=erm
	  endif
	  let b:optionStack=substitute(b:optionStack,mx,'','')
	endwhile
  endif
endfun

"Effectively Create some Buffer Enter and Leave User autocommands.
" Append "Enter" and "Leave" to the filetype to get the autocommand
aug MRGEnterLeave
au!
au BufEnter * exe "do User ".&filetype."Enter"
"au BufLeave * exe "do User ".&filetype."Leave"
au User *Enter :
"au User *Leave call RestoreOptions()
au BufLeave,BufUnload * call RestoreOptions()
aug END

" This is a little trick to allow mappings defined for a particular file within
" the file. The lines recognized are : 
"<comment>vimexe:"option1","value","nmap map2","value2
" where the arguments are the same as for PushOption.
" 
"vimexe:"nmap <c-cr>",":echo 'hi'<cr>"
"vimexe:"nmap <m-p>",":echo 'hi ther'<cr>"

"aug MyOpts
"au!
"au User vimEnter call GetOptionsFromFile('"')
"au User vimLeave call RestoreOptions() 
"aug END

fun! GetOptionsFromFile(com)
  if !exists("b:VIMEXE")
	let mx='^'.a:com.'vimexe:\s*\(.*\)$'
	let x=''
	let i=1
	while i<10
	  let cur=getline(i)
	  if cur =~ mx 
		let x=x.'call PushOption('.substitute(getline(i),mx,'\1','').")\|"
	  endif
	  let i=i+1
	endwhile
	let b:VIMEXE=x	
  endif
  exe b:VIMEXE
endfun 

" And now for an example of how to use it:
"
"aug specialEdit
"au!
"au User sgmlEnter call PushOptions("keywordprg","/bin/htmlkey")
"au User cEnter call PushOptions("breakat",". )&|",  "complete",".,k/usr/dict,]",  "nmap <c-enter>","<esc>o","amenu 10.100 Tools.Compile\\ C",":make<cr>" )
"
"au User sgmlLeave,cLeave  call RestoreOptions()
"aug END

" This allows you to source a file and have the maps (and menus?) be entered into
" a per-filetype list.  The file also gets sourced without the maps... so any
" functions get loaded.

" Usage: call ReadFileTypeMap(txt',$home.'/vim/txtmappings.vim')
" This will cause the mappings for ~/vim/txtmappings.vim to be available in
" file of FileType 'txt'.

fun! FTExe(filetypes, line)
  return  FileType__ExeLine('','',-1, a:filetypes, a:line)
endfun

com! -nargs=+ Fexe call  FTExe( <f-args>)

if !exists('g:buffoptions_do_bufmap')
  let g:buffoptions_do_bufmap=(version>=600)
endif


fun! FileType__ExeLine(group,filename, lineno, filetypes, line)

  " let mmx='^\s*[onvica]*\(map\|menu\)\>'

  let mapx='^\s*\([onvic]*\)map\s\+\(\S*\)\s\+\(.\{-}\)\s*$'
  let menx='^\s*\([onviac]*\)menu\s\+\(\(\<[0-9.]\+\>\)\s\+\)\=\(\(\\.\|[^\\	 ]\+\)\+\)\s\+\(.\{-}\)\s*$'

  " if a:line!~mmx | continue | endif
  let ismap=0
  if a:line=~mapx
	let mapt = substitute(a:line,mapx,'\1','') 
	let mapa = 'map '.substitute(a:line,mapx,'\2','') 
	let mapb = substitute(a:line,mapx,'\3','')
	if mapt=='' | let mapt='nvo' | endif
	let ismap=1
  elseif a:line=~menx
	let mapt = substitute(a:line,menx,'\1','') 
	let mapa = 'menu '.substitute(a:line,menx,'\2 \4','') 
	let mapb = substitute(a:line,menx,'\6','')
	if mapt=='' | let mapt='nvo' | endif
  else
	let erm=v:errmsg
	let v:errmsg=''
	exe a:line
	if v:errmsg!="" && lineno>0 
	  echohl ErrorMsg
	  echo 'In line number: '.a:lineno.' of '.a:filename
	  echohl None
	else
	  let v:errmsg=erm
	endif
	return
  endif

  let erm=v:errmsg
  let v:errmsg=''
  if a:group !=""
	exe 'aug '.a:group
  endif
  if ismap && g:buffoptions_do_bufmap
	  let entertypes=substitute(substitute(a:filetypes,'\(\<[a-z]\+\>\)[ ,]*','\1,','g'),',\s*$','','')
	  let nx='^\(map \)\(!\=\)'
	  let nore=''
	  if substitute(mapa,nx,'\2','')=='!'
		let nore='nore'
	  endif
	  let mapa=substitute(mapa,nx,'\1 <buffer> ','')
	  if strlen(mapt)>1
		let ma=0
		while ma < strlen(mapt)
		  exe 'au FileType '.entertypes.' '.mapt[ma].nore.mapa.' '.mapb
		  let ma=ma+1
		endwhile
	  else
		exe 'au FileType '.entertypes.' '.mapt.nore.mapa.' '.mapb
	  endif
  else
	let entertypes=substitute(substitute(a:filetypes,'\(\<[a-z]\+\>\)[ ,]*','\1Enter,','g'),',\s*$','','')
	exe 'au User '.entertypes." call PushOption(\"".escape(mapt.mapa,'\"')."\",\"".escape(mapb,'\"')."\")"
  endif
  if v:errmsg!="" 
	echohl ErrorMsg
	echo 'In line number: '.a:lineno.' of '.a:filename
	echohl None
  else
	let v:errmsg=erm
  endif
  if a:group != ""
	exe 'aug END'
  endif
endfun

let path_for_buffoptions=expand('<sfile>:p:h')

if !exists('g:buffoptions_cache')
  let g:buffoptions_cache='./.ft/'
endif
fun! ReadFileTypeMap(types, filename)
  let g:DoingSOURCE=1
  let filename=expand(a:filename)
  if !filereadable(filename)
	echohl ErrorMsg
	echo "Can't open file ".a:filename
	echohl None
	return "NO"
  endif

  let group='FileTypeMap_'.substitute(fnamemodify(filename,':t'),'[^a-zA-Z0-9]','_','g')
  exe 'aug '.group
  au!
  exe 'aug END'

  let cached=0
  let do_cache=exists('g:buffoptions_cache') &&  g:buffoptions_cache != '0'
  if do_cache
	let cache_dir=fnamemodify(a:filename,':p:h:gs?\\?/?:s?[^/\\]$?&/?').g:buffoptions_cache.'/'
	let cache_file=cache_dir.fnamemodify(a:filename,':t')
	if isdirectory(cache_dir)
	  if filereadable(cache_file)
		if (getftime(cache_file) >= getftime(a:filename))
		  let cached=1
		else
		  call delete(cache_file)
"		  call input("Delete: ".cache_file)
		endif
	  endif
"	  call input("Cached: ".cached)
	else
	  call system('mkdir "'.fnamemodify(cache_dir,':gs?/?\\?').'"')
	endif
  endif

  if !do_cache
	let cache_file=tempname();
  endif

  if !cached
	call system("vim ".filename." -R -e -n -u ".g:path_for_buffoptions."/buffoptions_mac.vim -c \"call DoIt('".group."','".a:types."','".cache_file."')\"") 
  endif
  let erm=v:errmsg
  let v:errmsg=''
  exe "so ".cache_file
  if v:errmsg!="" 
	echohl ErrorMsg
	echo 'Error SOURCEing File: '.a:filename
	echohl None
  else
	let v:errmsg=erm
  endif
  " call input('Continue?'.cache_file.'?')
  if !do_cache &&  filereadable(cache_file)
	call delete(cache_file)
  endif
  unlet g:DoingSOURCE

  call RestoreOptions()
  if &filetype!=''
	exe 'doau User '.&filetype.'Enter'
  endif

  let g:filetype_redo=g:filetype_redo+1
  call FileTypeRedoLevel()
endfun
if !exists('g:filetype_redo')
  let g:filetype_redo=0
endif

if g:buffoptions_do_bufmap
  fun! FileTypeRedoLevel()
	if !exists('b:filetype_redo')
	  let b:filetype_redo=0
	endif
	if b:filetype_redo < g:filetype_redo
	  let b:filetype_redo = g:filetype_redo
	  if &filetype!=''
		exe 'doau FileType '.&filetype
	  endif
	endif
  endfun
  aug MRGEnterRedo
  au!
  au BufEnter * call FileTypeRedoLevel()
  aug END
endif

com! -complete=file -nargs=1 SOURCE call ReadFileTypeMap( "", <q-args> )

if 0
" buffoptions_mac.vim
"--------------8<-------------------------------
fun DoIt(group,types,tfilename)
  set noro
  let filetypes=a:types
  let typex='^\s*"\s*FileTypes\=\s*:\s*\(.*\)\s*$'
  g /^\s*\(\([onvica]*\(map\|menu\)\>\)\|"\s*FileTypes\=\s*:\)/ if getline('.')=~typex |let filetypes=substitute(getline('.'),typex,'\1','') | else | if filetypes != ''| exe 's/["\\]/\\&/g | s/^.*$/call FileType__ExeLine("'.a:group.'",'."'".escape(expand('%:p:gs?\\?/?'),'/\&.*')."'".','.line('.').',"'.filetypes.'","&")/'|endif |endif
  
  exe 'w '.a:tfilename
  q!
endfun
"--------------8<-------------------------------
endif
