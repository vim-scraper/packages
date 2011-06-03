"ant.vim : VIM menu for ant
"Another Neat Tool (http://jakarta.apache.org/ant/index.html)
"Author : Shad Gregory <shadg@mailcity.com>
"http://home.austin.rr.com/shadgregory
"$Date: 01/18/2003 $
"$Revision: 0.5 $
"
"Configuration comments:
"	You can set ant.vim options.  Let's say that you always use the
"	'-debug' option and for some reason you've called your build 
"	file 'JimBob.'  Then you should put the following lines in
"	your .vimrc or _vimrc.
"
"	let g:buildFile = 'JimBob'
"	let g:antOption = '-debug'
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
"
"	,g -> If you are in the error buffer, and you press ',g' at a the
"		line that identifies which file the error came from,
"		then vim will open that file in a new buffer.  The cursor
"		should be at the line containing the first error.
"
"	,t -> Prompts you for the name of the build target and executes
"		the specified target after enter is pressed.  (Thanks to
"		Anton Straka for this bit of code.)
"
"	Thanks:
"		Anton Straka, Ronny Wilms, Nathan Smith, Keith Corwin, Mark
"		Healy, David Fishburn

function! GetProbFile()
	let l:badFile = getline(".")
  	"Is this jikes 1.17?
	if getline(".") =~ 'Found \d.*syntax'
	  	echo "jikes117!"
		let l:badFile = substitute(l:badFile,'^||\(.*\)','\1','')
		let l:badFile = substitute(l:badFile,'\[javac\]\(.*\)','\1','')
		let l:badFile = substitute(l:badFile,'Found.*"\(.*\.java\)":','\1','')
		let l:badFile = substitute(l:badFile,'^\s*\(.*\)','\1','')
		let l:badFile = substitute(l:badFile,'^\s*\(.*\)\s*\s$','\1','')
		let l:current = line('.') + 2
		let l:lineNumber = getline(l:current)
		let l:lineNumber = substitute(l:lineNumber,'^.*\s\(\d*\)\..*','\1','')
		if (bufexists(l:badFile))
		  	let l:bufferNumber = bufnr(l:badFile)
			silent! exec l:bufferNumber . 'wincmd w'
			exec "normal " . l:lineNumber . "gg"
			"silent! exec l:bufferNumber . 'b'
		  	"silent! exec 'split +' . l:lineNumber . ' ' .l:badFile
		else
			silent! exec '!gvim +' . l:lineNumber . ' ' .l:badFile
		endif
		return
	"is this jikes 1.15?
	elseif getline(".") =~ '\.java:\d*:\d'
	  	echo "jikes115!"
		let l:badFile = substitute(l:badFile,'^||\(.*\)','\1','')
	        let l:badFile = substitute(l:badFile,'\(^.*\):\d*:.*','\1','')
        	let l:badFile = substitute(l:badFile,'\(^.*.java\):\d*:.*','\1','')
        	let l:badFile = substitute(l:badFile,'\[javac\]\(.*\)','\1','')
		let l:badFile = substitute(l:badFile,'^\s*\(.*\)','\1','')
        	let l:current = getline(".")
        	let l:lineNumber = substitute(l:current,'.*:\(\d*\):.*','\1','')
        	let l:lineNumber = substitute(l:current,'.*.java:\(\d*\):.*','\1','')
		if (bufexists(l:badFile))
		  	silent! exec 'split +' . l:lineNumber . ' ' .l:badFile
		else
			silent! exec '!gvim +' . l:lineNumber . ' ' .l:badFile
		endif
		return
	"Is this sun's jdk?
	elseif getline(".") =~ '\.java:\d.*:'
	  	echo "javac!"
		let l:badFile = substitute(l:badFile,'^||\(.*\)','\1','')
		let l:badFile = substitute(l:badFile,'\(^.*\):\d*:.*','\1','')
		let l:badFile = substitute(l:badFile,'\[javac\]\(.*\)','\1','')
		let l:badFile = substitute(l:badFile,'^\s*\(.*\)','\1','')
		let l:current = getline(".")
		let l:lineNumber = substitute(l:current,'.*:\(\d*\):.*','\1','')
		if (bufexists(l:badFile))
		  	l:bufferNumber = bufnr(l:badFile)
			silent! exec 'split +' . l:lineNumber . ' ' .l:badFile
		else
			silent! exec '!gvim +' . l:lineNumber . ' ' .l:badFile
	        endif
		return
	else
		redraw
		echo 'Cannot parse file from this line!'
		return
	endif
endfunction

function! BuildTargetMenu()
	new
	silent! exec 'read '.g:buildFile
  	silent! exec 'g/^$/d'
	"leave only target tags
	silent! exec 'g/^\s*<!--.*-->$/d'
	silent! exec 'g/<target.*[^>]$/exe "norm! v/>\<CR>J"'
        silent! exec 'g/<!--.*\_.*.*-->/exe "norm! v/-->\<CR>J"'
	silent! exec 'g!/<target/d'
        silent! exec '%s/^\s*<target.*name="\([^"]*\)".\+/\1/eg'
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
if !exists("g:antOption")
	let g:antOption = ''
endif

"keyboard shortcuts
map	,b	:call DoAntCmd(g:antOption.' -buildfile',g:buildFile)<cr>
map	,s	:call SetBuildFile()<cr>
map	,f	:call DoAntCmd(g:antOption.' -find',g:buildFile)<cr>
map	,l	:call SetLogFile()<cr>
map	,g	:call GetProbFile()<cr>
map	,t	:call SetBuildTarget()<cr>

"build ant menu
amenu &ANT.\ &Build	:call DoAntCmd(g:antOption.' -buildfile',g:buildFile)<cr>
"amenu &ANT.\ &Find	:call DoAntFind()<cr>
amenu &ANT.\ &Find	:call DoAntCmd(g:antOption.' -find',g:buildFile)<cr>

"parse build file if one exists in current directory
if filereadable(g:buildFile)
	call BuildTargetMenu()
endif

amenu &ANT.\ &Set\ Option.\ &Quiet 	:let g:antOption = g:antOption . ' -quiet '<cr>
amenu &ANT.\ &Set\ Option.\ &Verbose	:let g:antOption = g:antOption . ' -verbose '<cr>
amenu &ANT.\ &Set\ Option.\ &Debug	:let g:antOption = g:antOption . ' -debug '<cr>
amenu &ANT.\ &Set\ Option.\ &Emacs	:let g:antOption = g:antOption . ' -emacs '<cr>
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
	if !exists("a:1")
		let ant_cmd='ant '.a:cmd
	else
		if !filereadable(a:1) && a:cmd != ' -find'
			redraw
			echo 'build.xml is not readable!'
			return
		endif
		if exists("a:2")
			if (g:logFile == '')
				let ant_cmd='ant '.a:cmd.' '.a:1.' '.a:2
			else
				let ant_cmd='ant -logfile '.g:logFile.' '.a:cmd.' '.a:1.' '.a:2
			endif
		else
			if (g:logFile == '')
				let ant_cmd='ant '.a:cmd.' '.a:1
			else
				let ant_cmd='ant -logfile '.g:logFile.' '.a:cmd.' '.a:1
			endif
		endif
	endif
        " jikes format 
        let &errorformat="\ %#[javac]\ %#%f:%l:%c:%*\\d:%*\\d:\ %t%[%^:]%#:%m"
        " ant [javac] format 
        let &errorformat=&errorformat . "," .
                    \"\%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%#"
        let &makeprg=ant_cmd
        silent! execute 'make'
        silent! execute 'cwindow'
        silent! execute 'copen'
endfunction

function! SetBuildTarget()
	let target=escape(input('name of build target: '), '"<>|&')
	call DoAntCmd(g:antOption.' -buildfile',g:buildFile, target)
endfunction
