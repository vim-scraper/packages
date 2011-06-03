"=============================================================================
" File:        autoupload.vim
" Author:      Mariusz Gniazdowski (mrg at risp.pl)
" Last Change: Jan 30 03:23:23 CET 2005
" Version:     1.1
"
" Usage:
" 1. Create autoupload.info (or .autoupload.info) file. Files like those are
"    called 'maps'.
"
" 2. Each line in map is an entry in format:
" 	filename : {ftp|dav|scp|etc...}://address
" 		or
" 	filename | {ftp|dav|scp|etc...}://address
"
"    >> Address must end with file name! <<
"    Example entry:
"    localfile.txt | scp://me@mymachine.com/tmp/remfile.txt
"
" 3. Then while editing some file pressing <Leader>up ('u' key and 'p' key)
" will cause script to:
"    - read autoupload.info (.autoupload.info) from dir in which actual
"      buffer is stored (or from absolute path if customized that way)
"    - store actual buffer in remote location according to entry in map.
"
" If no entry for actual buffer is found in autoupload.info, then
" .autoupload.info is examined.
"
" Customization:
" g:autoUploadMaps (default: "autoupload.info:.autoupload.info")
"
" 				File names/paths to maps.
"
" 				Can contain multiple file names separated  by
" 				: or |  Files will be examined in order given
" 				in this variable.
" 				Paths can be:
" 				- absolute,
" 				- realtive - they will be appended to path in
" 				which actual buffer is stored!
"
" g:autoUploadStopAfterFirstUpload (default: 1)
" 				If is set to 1 then  maximum one upload  will
" 				be done  no matter  now many maps  will  have
" 				proper entries  and  how  many proper entries
" 				will be in one map.
" 				In  other  words: Find first remote  location
" 				that is assigned to actual buffer, use it and
" 				return to editor. This one variable turns off
" 				all multiple upload possibilities.
"
" g:autoUploadStopAfterFirstMap	(default: 1)
"				If 1, then processing of maps will stop after
"				first  map  that  had  at  least one matching
"				entry.
"				So you can assign  multiple remote  locations
"				to  one  file  in  one  map  and  if variable
"				g:autoUploadStopAfterFirstUpload  will be set 
"				to 0 then multiple uploads of the  same  file 
"				will be taken.
"
" Combinations of two last options:
" g:autoUploadStopAfterFirstUpload = 0
" g:autoUploadStopAfterFirstMap	= 1
" 				This  will  do multiple  uploads to addresses
" 				taken from only one map  ( first one that has
" 				some match).
" g:autoUploadStopAfterFirstUpload = 0
" g:autoUploadStopAfterFirstMap	= 0
" 				This will  upload  current  buffer  to  every
" 				assigned remote location - multiple locations
" 				per map and multiple maps will be used.
"
" :call MG_AutoUpload()		Map this to some key combination
"
" Example_customization:
" 	let g:autoUploadMaps = ".autoupload.info:/root/.remote.inf"
" 	let g:autoUploadStopAfterFirstUpload = 0
" 	nnoremap <F12> :call MG_AutoUpload()<CR>
"
" Worth_to_read:
" :help netrw
"
" Warning:
" 	Global maps files are dangerous because filenames can repeat
" 	(index.php main.c, etc). You should use it for special cases
" 	only.
"
" Changes:
" 1.1  	- multiple file names supported
" 	- absolute paths supported
" 	- file format change - colums can be separated by ':' now,
" 	- variable holding file paths changed to g:autoUploadMaps
" 	- message returning used destination address after upload ( remember
" 	  about :messages command )
" 	- modes in which processing of files does not stop - it is possible
" 	  to upload one file to many locations which can be taken from many
" 	  files
" 1.01 	- fixed bug when file name bbaa was recognised as matching aabbcc
"
" ============================================================================

if exists('loaded_autoupload') || &cp
  finish
endif
let loaded_autoupload=1

if !exists('g:autoUploadMaps')
	let g:autoUploadMaps = "autoupload.info:.autoupload.info"
endif
if !exists('g:autoUploadStopAfterFirstUpload')
	let g:autoUploadStopAfterFirstUpload = 1
endif
if !exists('g:autoUploadStopAfterFirstMap')
	let g:autoUploadStopAfterFirstMap = 1
endif

" Default mapping
nnoremap <Leader>up :call MG_AutoUpload()<CR>
" This one would be nice too
"nnoremap <Leader><UP> :call MG_AutoUpload()<CR>

"
" Walk thru every given map and try to find
" there a mach for actual buffer
"
function MG_AutoUpload() " {{{1
	let baseDir = expand("%:p:h") . "/"
	let actFileName = expand("%:t")
	let sum_matched_idx = -1 
	let infoFileName = ""
	let checkedFiles = ""
	let wasMatch = 0
	while 1
		" Skip last ':' or '|'
		let sum_matched_idx = sum_matched_idx + 1
		" Get everything until ':' or '|'
		let infoFileName = matchstr(g:autoUploadMaps, "^[^|:]\\+", sum_matched_idx)
		" No match -> whole list is processed -> exit
		if match(infoFileName,"^[[:space:]]*$") >= 0
			if wasMatch == 0
				echomsg "No entry found for actual buffer: `".actFileName."' (examined files: " . checkedFiles . ")"
			endif
			break
		endif
		let sum_matched_idx = sum_matched_idx + strlen(infoFileName)
		" Process that match and optionaly stop walking thru rest 
		" of maps
		if <SID>ProcessInfoFile(infoFileName) > 0 
			" Stop after first map that uploaded something?
			if (g:autoUploadStopAfterFirstMap == 1) || (g:autoUploadStopAfterFirstUpload == 1)
				break
			else
				let wasMatch = 1
			endif
		endif
		let checkedFiles = checkedFiles . "`" . infoFileName . "' "
	endw
endf " 1}}}

"
" Main function.
" 1. Reads given maps (which contains entries associating 
"    remote locations to file names)
" 2. Lineary searches thru it to find line fitting to actual buffer
" 3. If succeded, then it calls Nwrite to store actual buffer under
"    remote address
"
function <SID>ProcessInfoFile(infoFileName) " {{{1
	" Prepend only non absolute filenames
	if a:infoFileName[0] == "/" || a:infoFileName[0] == "~"
		let fullInfoFilePath = a:infoFileName
	else
		" Prepend with directory in which actual buffer is stored (not
		" with current working directory!)
		let fullInfoFilePath = expand("%:p:h") . "/" . a:infoFileName
	endif

	if !filereadable(fullInfoFilePath)
		" echomsg "Can't read `" . a:infoFileName . "' file"
		return 0
	endif

	" Get buffer file name
	let actFileName = expand("%:t")

	" Read map file
	exec "sp " . fullInfoFilePath
	let lineCount=line("$")
	let i = 1
	let wasMatch = 0
	let destinationAddresses = ""
	" Process each line (maybe binary search should be added at some stage..)
	while i<=lineCount
		let line = getline(i)
		" File name is every char up to '|' or ':'
		let fileName = matchstr(line,"[^|:]*")
		let idx = match(fileName, "^[[:space:]]*".actFileName."[[:space:]]*$")
		if idx>=0
			let wasMatch = 1
			" Found entry -> add address too 'list'
			let destinationAddresses = destinationAddresses . matchstr(line,".*$", strlen(fileName) +1 ) . "\n"
			" User want us to do only one upload?
			if g:autoUploadStopAfterFirstUpload == 1
				break
			endif
		endif
		let i=i+1
	endwhile
	" Remove buffer with map
	bw!

	" Loop thru results
	if wasMatch == 1
		" Damn dirty
		let sum_matched_idx = -1
		while 1
			" Skip last '\n'
			let sum_matched_idx = sum_matched_idx + 1
			" Get everything until '\n'
			let destAdr = matchstr(destinationAddresses, "^[^\n]\\+", sum_matched_idx)
			" No match?
			if match(destAdr,"^[[:space:]]*$") >= 0
				" There sure was at least one upload
				return 1
			endif
			let sum_matched_idx = sum_matched_idx + strlen(destAdr)
			let v:errmsg = ""
			exec ":Nwrite ". destAdr
			if v:errmsg != "" 
				echo v:errmsg
				sl 2000m
			else
				echomsg "Written at: `" . destAdr . "' (address taken from file: `".a:infoFileName."')"
			endif
		endw
	endif

	return wasMatch
endf " 1}}}



