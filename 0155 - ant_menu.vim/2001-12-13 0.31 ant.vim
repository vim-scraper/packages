"ant.vim : VIM menu for ant
"Another Neat Tool (http://jakarta.apache.org/ant/index.html)
"Author : Shad Gregory <shadg@mailcity.com>
"$Date: 2001/12/13 $
"$Revision: 0.31 $
"
"Keyboard Commands:
"	,s -> This will prompt you for the location and name of the build
"		file.  Not necessary if the build file is in the current
"		directory and named 'build.xml'
"
"	,b -> Executes 'ant -buildfile <build file>'  This will include any
"		option you have set via the menu.
"
"	,f -> Executes 'ant -find' along with any option you have set via
"		the menu.
"
"	,l -> Sets log file.  All ant output will be directed to the
"		file you set with this option.

function! BuildTargetMenu()
	new
	silent! exec 'read '.g:buildFile
  	silent! exec 'g/^$/d'
	"leave only target tags
	silent! exec 'g!/^\s<target/d'
	silent! exec '%s/\s*<target\s\(name="[^"]*"\).\+/\1/eg'
	silent! exec '%s/name//g'
	silent! exec '%s/"//g'
	silent! exec '%s/=//g'
	"escape any periods
	silent! exec '%s/\./\\./g'
  	let entries=line("$")
	let target = 1
	silent! unmenu '&ANT.\ Target'
	while target <= entries
		let cmdString = ':call DoAntCmd(g:antOption." -buildfile",g:buildFile,"'.getline(target).'")<cr>'
		let menuString = '&ANT.\ Target.\ ' . getline(target) . '	' . cmdString
		exe 'amenu ' . menuString . '<cr>'
		let target = target + 1
	endwhile
	set nomodified
	bwipeout
	return 1
endfunction

if exists("loaded_antmenu")
	aunmenu ANT
else
  	let loaded_antmenu=1
endif

"globals
if !exists("g:buildFile")
	let g:buildFile = './build.xml'
endif
if !exists("g:logFile")
	let g:logFile = ''
endif
let g:antOption = ''

"keyboard shortcuts
map	,b	:call DoAntCmd(g:antOption.' -buildfile',g:buildFile)<cr>
map	,s	:call SetBuildFile()<cr>
map	,f	:call DoAntFind()<cr>
map	,l	:call SetLogFile()<cr>

"build ant menu
amenu &ANT.\ &Build	:call DoAntCmd(g:antOption.' -buildfile',g:buildFile)<cr>
amenu &ANT.\ &Find	:call DoAntFind()<cr>

"parse build file if one exists in current directory
if filereadable(g:buildFile)
	call BuildTargetMenu()
endif

amenu &ANT.\ &Set\ Option.\ &Quiet 	:let g:antOption = '-quiet'<cr>
amenu &ANT.\ &Set\ Option.\ &Verbose	:let g:antOption = '-verbose'<cr>
amenu &ANT.\ &Set\ Option.\ &Debug	:let g:antOption = '-debug'<cr>
amenu &ANT.\ &Set\ Option.\ &Emacs	:let g:antOption = '-emacs'<cr>
amenu &ANT.\ &Set\ Option.\ &None	:let g:antOption = ''<cr>
amenu &ANT.\ &Set\ Option.\ &Display\ Current	:echo g:antOption<cr>
amenu &ANT.\ &Files.\ set\ build\ file	:call SetBuildFile()<cr>
amenu &ANT.\ &Files.\ echo\ build\ file	:echo g:buildFile<cr>
amenu &ANT.\ &Files.\ set\ log\ file	:call SetLogFile()<cr>
amenu &ANT.\ &Files.\ echo\ log\ file	:echo g:logFile<cr>
amenu &ANT.\ &Files.\ no\ log\ file	:let g:logFile = ''<cr>
amenu &ANT.\ &Project\ Help 	:call DoAntCmd('-projecthelp')<cr>
amenu &ANT.\ &Help	 	:call DoAntCmd('-help')<cr>
amenu &ANT.\ &Version 		:call DoAntCmd('-version')<cr>

"Allows user to set build.xml.  If the file does not exist, gives a
"statusline message and resets buildFile back to default.
function! SetBuildFile()
	let g:buildFile=escape(input('build.xml location: '), '"<>|&')
	if !filereadable(g:buildFile)
		redraw
		echo g:buildFile.' does not exist!'
		let g:buildFile = './build.xml'
		return
	endif
	call BuildTargetMenu()
	return
endfunction

function! SetLogFile()
	let g:logFile=escape(input('name of log file: '), '"<>|&')
	return
endfunction

function! DoAntCmd(cmd,...)
    	let regbak=@z
	if !exists("a:1")
		let @z=system('ant '.a:cmd)
	else
		if !filereadable(a:1)
			redraw
			echo 'build.xml is not readable!'
			return
		endif
		if exists("a:2")
			if (g:logFile == '')
				let @z=system('ant '.a:cmd.' '.a:1.' '.a:2)
			else
				let @z=system('ant -logfile '.g:logFile.' '.a:cmd.' '.a:1.' '.a:2)
			endif
		else
			if (g:logFile == '')
				let @z=system('ant '.a:cmd.' '.a:1)
			else
				let @z=system('ant -logfile '.g:logFile.' '.a:cmd.' '.a:1)
			endif
		endif
	endif
	if (g:logFile == '')
		new
		silent normal "zP
		let @z=regbak
	else
		redraw
		echo 'check '.g:logFile.' for ant output'
	endif
endfunction

function! DoAntFind(...)
    	let regbak=@z
	let @z=system('ant '.g:antOption.' -find '.g:buildFile)
	new
	silent normal "zP
	let @z=regbak
endfunction
