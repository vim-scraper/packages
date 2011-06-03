"ant.vim : VIM menu for using ant 
"Another Neat Tool (http://jakarta.apache.org/ant/index.html)
"Author : Shad Gregory <shadg@mailcity.com>
"
"globals
let g:buildFile = 'build.xml'

if exists("loaded_antmenu")
	aunmenu ANT
endif

"keyboard shortcuts
map	,b	:call DoAntCmd('-buildfile',g:buildFile)<cr>
map	,s	:call SetBuildFile()<cr>
map	,f	:call DoAntFind()<cr>

"build ant menu
amenu &ANT.\ &Build.\ &Default	:call DoAntCmd('-buildfile',g:buildFile)<cr>
amenu &ANT.\ &Build.\ &Find	:call DoAntFind()<cr>
amenu &ANT.\ &Build.\ &Debug 	:call DoAntCmd('-debug -buildfile',g:buildFile)<cr>
amenu &ANT.\ &Build.\ &Verbose	:call DoAntCmd('-verbose -buildfile',g:buildFile)<cr>
amenu &ANT.\ &Build.\ &Quiet 	:call DoAntCmd('-quiet -buildfile',g:buildFile)<cr>
amenu &ANT.\ &Set\ build\ file	:call SetBuildFile()<cr>
amenu &ANT.\ &ANT\ Help	 	:call DoAntCmd('--help')<cr>
amenu &ANT.\ &ANT\ Version 	:call DoAntCmd('-version')<cr>

if !exists("loaded_antmenu")
  let loaded_antmenu=1
endif

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
	new
	exec 'read '.g:buildFile
  	exec 'g/^$/d'
	"leave only target tags
	exec 'g!/^\s<target/d'
  	let entries=line("$")
	let target = 1
	redraw
	echo 'There are '.entries.' targets'
	set nomodified
	bwipeout
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
		let @z=system('ant '.a:cmd.' '.a:1)
	endif
	new
	silent normal "zP
	let @z=regbak
endfunction

function! DoAntFind(...)
    	let regbak=@z
	let @z=system('ant -find '.g:buildFile)
	new
	silent normal "zP
	let @z=regbak
endfunction
