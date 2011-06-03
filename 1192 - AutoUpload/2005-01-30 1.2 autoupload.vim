"=============================================================================
" File:        autoupload.vim
" Author:      Mariusz Gniazdowski (mrg at risp.pl)
" Last Change: Mon Jan 31 02:26:10 CET 2005
" Version:     1.2
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
" 4. Or as an alternative:
"    - to first or last lines of buffer to upload add pattern:
" 		[text]{white}autoupload:remotepath1[;remotepath2[...]]
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
" g:autoUploadScanLines (default: 3)
" 				Script  can  scan last  and  first lines of a
" 				buffer for pattern:
" 		[text]{white}autoupload:remotepath1[;remotepath2[...]]
" 				Fields are separated by ';'  or  '|'.  ';' is
" 				used because ':' is always a part of protocol
" 				name, for example ftp://...
"
"				This  variable  contains number of  lines  to
"				scan.
"
" g:autoUploadModelineStart (default: 'autoupload:')
" 				Pattern which modeline should begin with. Can
" 				contain regex.
"
" g:autoUploadUseBuffer (default: 1, possible: 0, 1, 2)
" 				If  g:autoUploadUseBuffer  is  set to 0, then
" 				no scan is done.
" 				If  g:autoUploadUseBuffer>g:autoUploadUseMaps
" 				then  script will scan  buffer  before  maps.
" 				If g:autoUploadUseBuffer<=g:autoUploadUseMaps
" 				then  script will scan  maps  before  buffer.
" 				
" g:autoUploadUseMaps (default: 1, possible: 0, 1, 2)
" 				If  g:autoUploadUseMaps is set to 0 , then no
" 				no maps are searched.
" 				If g:autoUploadUseMaps=>g:autoUploadUseBuffer
" 				then  script will  scan  maps before  buffer.
" 				If  g:autoUploadUseMaps<g:autoUploadUseBuffer
" 				then  script will  scan  buffer before  maps.
"
" g:autoUploadStopAfterFirstUpload (default: 1, possible: 0, 1)
" 				If is set to 1 then  maximum one upload  will
" 				be done  no matter  now many maps  will  have
" 				proper entries  and  how  many proper entries
" 				will be in one map.
" 				In  other  words: Find first remote  location
" 				that is assigned to actual buffer, use it and
" 				return to editor. This one variable turns off
" 				all multiple upload possibilities.
"
" g:autoUploadStopAfterFirstMap	(default: 1, possible: 0, 1)
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
" 1.2	- remote locations can now be contained in buffers (in 'modelines')
" 	- fixed bug that maked uploading of file with name containing spaces
" 	  impossible
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
if !exists('g:autoUploadUseBuffer')
	let g:autoUploadUseBuffer = 1
endif
if !exists('g:autoUploadUseMaps')
	let g:autoUploadUseMaps = 2
endif
if !exists('g:autoUploadScanLines')
	let g:autoUploadScanLines = 3
endif
if !exists('g:autoUploadModelineStart')
	let g:autoUploadModelineStart = "autoupload:"
endif

" Default mapping
nnoremap <Leader>up :call MG_AutoUpload()<CR>
" This one would be nice too
"nnoremap <Leader><UP> :call MG_AutoUpload()<CR>


"
" Start operation of upload
" First decide wheather to start scanning buffer or maps first.
" Then start that operations.
"
function MG_AutoUpload() " {{{1
	let s:wasMatchInBuffer = 0
	" Scan buffer first?
	if (g:autoUploadUseBuffer > g:autoUploadUseMaps) && ( g:autoUploadUseBuffer != 0)
		call <SID>ScanBuffer()
	endif

	if g:autoUploadUseMaps != 0
		call <SID>SearchForMaps()
	endif

	if (g:autoUploadUseBuffer <= g:autoUploadUseMaps) && ( g:autoUploadUseBuffer != 0)
		call <SID>ScanBuffer()
	endif
endf " 1}}}

"
" Searches for modelines and starts processing of each of them
"
function <SID>ScanBuffer() " {{{1
	" Calculate number of top lines
	if (line("$") - g:autoUploadScanLines) < 0
		let topLineCount = line("$")
	else
		let topLineCount = g:autoUploadScanLines
	endif
	" Number of bottom lines
	if (line("$") - g:autoUploadScanLines) < 0
		let bottomLineCount = 0
	elseif ( (line("$") - g:autoUploadScanLines) - g:autoUploadScanLines) < 0
		let bottomLineCount = line("$") - g:autoUploadScanLines
	else
		let bottomLineCount = g:autoUploadScanLines
	endif

	" Top lines
	let i = 1
	while i <= topLineCount
		call <SID>ProcessBufferLine(getline(i))
		let i=i+1
		if (g:autoUploadStopAfterFirstUpload == 1) && (s:wasMatchInBuffer == 1)
			return
		endif
	endwhile

	" Bottom lines
	let i = line("$")
	while i > (line("$") - bottomLineCount)
		call <SID>ProcessBufferLine(getline(i))
		let i=i-1
		if (g:autoUploadStopAfterFirstUpload == 1) && (s:wasMatchInBuffer == 1)
			return
		endif
	endwhile
endf " 1}}}

"
" Processes line in format:
" [text]{white}autoupload:remotepath1[;remotepath2[...]]
"
" [text] is defined as 'not white'
"
function <SID>ProcessBufferLine(line) " {{{1
	let start_garbage = matchstr(a:line,"^[^[:space:]]*[[:space:]][[:space:]]*".g:autoUploadModelineStart)
	if start_garbage == ""
		let start_garbage = matchstr(a:line,"^[[:space:]]*".g:autoUploadModelineStart)
		if start_garbage == ""
			return
		endif
	endif

	" Loop thru paths if any
	let sum_matched_idx = strlen(start_garbage) - 1
	while 1
		" Skip last ';' or '|'
		let sum_matched_idx = sum_matched_idx + 1
		" Get everything until '\n'
		let destAdr = matchstr(a:line, "[^;|]\\+", sum_matched_idx)
		" No match?
		if match(destAdr,"^[[:space:]]*$") >= 0
			return 0
		endif
		let sum_matched_idx = sum_matched_idx + strlen(destAdr)
		" Trim
		let destAdr = substitute(destAdr,"^[[:space:]]*", "", "")
		let v:errmsg = ""
		exec ":Nwrite ". escape(escape(destAdr," \t\\"), '\')
		if v:errmsg != "" 
			echomsg v:errmsg
			sl 2000m
		else
			echomsg "Written at: `" . destAdr . "' (address taken from buffer)"
		endif
		" Each, even unsuccessful try of upload counts
		let s:wasMatchInBuffer = 1
		if g:autoUploadStopAfterFirstUpload == 1
			break
		endif
	endw
endfunction " 1}}}

"
" Walk thru every given map and try to find
" there a mach for actual buffer
"
function <SID>SearchForMaps() " {{{1
	let baseDir = expand("%:p:h") . "/"
	let actFileName = expand("%:t")
	let sum_matched_idx = -1 
	let mapFileName = ""
	let checkedFiles = ""
	let wasMatch = 0
	while 1
		" Skip last ':' or '|'
		let sum_matched_idx = sum_matched_idx + 1
		" Get everything until ':' or '|'
		let mapFileName = matchstr(g:autoUploadMaps, "^[^|:]\\+", sum_matched_idx)
		" No match -> whole list is processed -> exit
		if match(mapFileName,"^[[:space:]]*$") >= 0
			if (wasMatch == 0) && (s:wasMatchInBuffer == 0)
				if g:autoUploadUseBuffer != 0
					let checkedFiles = checkedFiles. " and buffer content"
				endif
				echomsg "No entry found for actual buffer: `".actFileName."' (examined files:" . checkedFiles . ")"
			endif
			break
		endif
		let sum_matched_idx = sum_matched_idx + strlen(mapFileName)
		" Process that match and optionaly stop walking thru rest 
		" of maps
		if <SID>ProcessMapFile(mapFileName) > 0 
			" Stop after first map that uploaded something?
			if (g:autoUploadStopAfterFirstMap == 1) || (g:autoUploadStopAfterFirstUpload == 1)
				break
			else
				let wasMatch = 1
			endif
		endif
		let checkedFiles = checkedFiles . " `" . mapFileName . "'"
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
function <SID>ProcessMapFile(mapFileName) " {{{1
	" Prepend only non absolute filenames
	if a:mapFileName[0] == "/" || a:mapFileName[0] == "~"
		let fullInfoFilePath = a:mapFileName
	else
		" Prepend with directory in which actual buffer is stored (not
		" with current working directory!)
		let fullInfoFilePath = expand("%:p:h") . "/" . a:mapFileName
	endif

	if !filereadable(fullInfoFilePath)
		" echomsg "Can't read `" . a:mapFileName . "' file"
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
			" Trim
			let destAdr = substitute(destAdr,"^[[:space:]]*", "", "")
			let v:errmsg = ""
			exec ":Nwrite ". escape(escape(destAdr," \t\\"),'\')
			if v:errmsg != "" 
				echo v:errmsg
				sl 2000m
			else
				echomsg "Written at: `" . destAdr . "' (address taken from file: `".a:mapFileName."')"
			endif
		endw
	endif

	return wasMatch
endf " 1}}}
