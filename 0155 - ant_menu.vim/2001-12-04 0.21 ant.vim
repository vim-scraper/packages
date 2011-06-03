"ant.vim : VIM menu for using ant
"Another Neat Tool (http://jakarta.apache.org/ant/index.html)
"Author : Shad Gregory <shadg@mailcity.com>
"$Revision: 0.2.1
"
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
let g:buildFile = './build.xml'
let g:antOption = ''

"keyboard shortcuts
map	,b	:call DoAntCmd(g:antOption.' -buildfile',g:buildFile)<cr>
map	,s	:call SetBuildFile()<cr>
map	,f	:call DoAntFind()<cr>

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
amenu &ANT.\ &Set\ build\ file	:call SetBuildFile()<cr>
amenu &ANT.\ &Help	 	:call DoAntCmd('-help')<cr>
amenu &ANT.\ &Version 	:call DoAntCmd('-version')<cr>

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
			let @z=system('ant '.a:cmd.' '.a:1.' '.a:2)
		else
			let @z=system('ant '.a:cmd.' '.a:1)
		endif
	endif
	new
	silent normal "zP
	let @z=regbak
endfunction

function! DoAntFind(...)
    	let regbak=@z
	let @z=system('ant '.g:antOption.' -find '.g:buildFile)
	new
	silent normal "zP
	let @z=regbak
endfunction

