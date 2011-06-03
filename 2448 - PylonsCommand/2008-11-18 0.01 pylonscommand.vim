" Author:
"   Original: Bartek Chaber<czaber@gmail.com>
" Version: 0.01
"
" Requirements: Linux(not tested on Windows), Pylons
" Description: {{{
"	Use vim commands to run Pylons commands
"
"	Default Pylons project looks like this:
"
"	MyProject/
"		myproject/
"		myproject.egg-info/
"		data/
"		docs/
"		developement.ini
"		MANIFEST.in
"		README.txt
"		setup.cfg
"		setup.py
"		test.ini
"	
"	Now, what is what?
"	g:PylonsProjectDir = path/to/MyProject/
"	g:PylonsProjectName = myproject/
"	
"	To start using this plugin you have to 'open' Pylon project with:
"		:PylonsOpen path/to/MyProject/myproject/
"	if everything goes well you should see:
"		Project set: /absolute/path/to/MyProject/myproject
"
"	Now you can use following functions:
"	PylonsOpen <dir> : open another project, current (if any) will be lost
"	PylonsServer [start|stop|restart] : start, stop, restart paster server
"	PylonsCreate [project|controller|template]:
"		project : Create new project TODO: not implemented yet
"		controller : Create new controler. It will as you for name.
"		template : Create new template. It will as you for name.
"		Note: you can use:
"			PylonsCreate template
"			'Template: path/to/template.mako'
"			['Template: path/to/template will' work the same]
"		This will create template.mako in
"				/absolute/path/to/MyProject/myproject/tempates/path/to/
"	PylonsPreview : this will open localhost:5000 with default browser (set in g:PylonsBrowser) 
" 
" Notes:
" I know that code can be buggy. It's my first vim-script, so XXX USE ON OWN RISK XXX.
" }}}

if !exists("g:PylonsProjectName")
	let g:PylonsProjectDir = ""
endif

if !exists("g:PylonsProjectName")
	let g:PylonsProjectName = ""
endif

if !exists("g:PylonsPID")
	let g:PylonsPID = "paster.pid"
endif

if !exists("g:PylonsIniFile")
	let g:PylonsIniFile = "development.ini"
endif

if !exists("g:PylonsBrowser")
	let g:PylonsBrowser = "opera"
endif

if !exists("g:PylonsHost")
	let g:PylonsHost = "localhost"
endif

if !exists("g:PylonsPort")
	let g:PylonsPort = "5000"
endif

"
" Bindings
"

command! -nargs=1 -bar -complete=dir PylonsOpen call g:PylonsOpenProject('<args>')
command! -nargs=1 -bar -complete=customlist,CompleteServer PylonsServer call g:PylonsServer('<args>')
command! -nargs=1 -bar -complete=customlist,CompleteCreate PylonsCreate call g:PylonsCreate('<args>')
command! -nargs=0 -bar PylonsPreview call g:PylonsPreview()

"
" Helpers
"

function! s:PylonsRun(cmd)
	if s:isInvalidPylonsProjectDir(g:PylonsProjectDir)
		return 0
	else
		let s:current = getcwd()
		silent execute "cd ".g:PylonsProjectDir
		execute "!".a:cmd
		silent execute "cd ".s:current
	endif
	return 1
endfunction


function! s:absolutePath(path)
	" absolute
	if match(a:path,"/") == 0
		return a:path
	else
		return getcwd()."/".a:path
	endif
endfunction

function! s:isInvalidDir(dir)
	if isdirectory(a:dir)
		return 0
	else
		return 1
	endif
endfunction

function! s:isNotProjectDir(dir)
	if filereadable(a:dir.g:PylonsIniFile)
		return 0
	endif
	return 1
endfunction

function! s:isNotProjectName(dir,name)
	if isdirectory(a:dir.a:name."/templates")
		return 0
	endif
	if isdirectory(a:dir.a:name."/controllers")
		return 0
	endif
	if isdirectory(a:dir.a:name."/public")
		return 0
	endif
	if isdirectory(a:dir.a:name."/lib")
		return 0
	endif
	if isdirectory(a:dir.a:name."/model")
		return 0
	endif
	return 1
endfunction

function! s:isInvalidPylonsProjectDir(dir)
	if a:dir == ""
		echo "Project directory not set! Use :PylonsOpen"
		return 1
	endif

	if s:isInvalidDir(a:dir)
		echo a:dir.': directory not found'
		return 1
	endif

	if s:isNotProjectDir(a:dir)
		echo 'No Pylons project in '.a:dir
		return 1
	endif
	return 0
endfunction

"
" OpenProject
"

function! g:PylonsOpenProject(p_dir)
	let p_dir = a:p_dir
	let last_slash = strridx(p_dir, "/") + 1
	let len = strlen(p_dir)

	" remove '/' at end
	if last_slash == len
		let p_dir = strpart(p_dir, 0, len - 1)
		let last_slash = strridx(p_dir, "/") + 1
	endif

	let projectDir  = strpart(p_dir, 0, last_slash)
	let projectName = strpart(p_dir, last_slash)

	if projectDir == ""
		let projectDir = "./"
	endif

	if s:isInvalidPylonsProjectDir(projectDir)
		return 0
	endif

	if s:isNotProjectName(projectDir, projectName)
		echo 'No project named '.projectName.' in '.projectDir
		return 0
	endif

	let g:PylonsProjectDir = s:absolutePath(projectDir)
	let g:PylonsProjectName = projectName
	echo "Project set: ".g:PylonsProjectDir.g:PylonsProjectName
	silent execute "cd ".g:PylonsProjectDir.g:PylonsProjectName
	return 1
endfunction

"
" Server managing
"

function! CompleteServer(ArgLead, CmdLine, CursorPos)
  return ['start', 'stop', 'restart']
endfunction

function! g:PylonsServerStarted()
	if s:isInvalidPylonsProjectDir(g:PylonsProjectDir)
		return -1
	else
		return filereadable(g:PylonsProjectDir.g:PylonsPID)
	endif
endfunction

function! g:PylonsServer(action)
	if a:action == "start"
		call g:PylonsServerStart()
	elseif a:action == "stop"
		call g:PylonsServerStop()
	elseif a:action == "restart"
		call g:PylonsServerRestart()
	endif
endfunction

function! g:PylonsServerStart()
	if g:PylonsServerStarted()
		call g:PylonsServerRestart()
	else
		let cmd = "paster serve --daemon --pid-file=".g:PylonsPID." ".g:PylonsIniFile." start"
		call s:PylonsRun(cmd)
	endif
endfunction

function! g:PylonsServerStop()
	if g:PylonsServerStarted()
		let cmd = "paster serve --daemon ".g:PylonsIniFile." stop"
		call s:PylonsRun(cmd)
		let g:PylonsServerStarted = 0
	else
		echo "Server not started"
	endif
endfunction

function! g:PylonsServerRestart()
	if g:PylonsServerStarted()
		let cmd = "paster serve --daemon ".g:PylonsIniFile." restart"
		call s:PylonsRun(cmd)
	else
		call g:PylonsServerStart()
	endif
endfunction

"
" Creating
"

function! CompleteCreate(ArgLead, CmdLine, CursorPos)
  return ['project', 'controller', 'template']
endfunction

function! g:PylonsCreate(type)
	if a:type == "project"
		call g:PylonsCreateProject()
	elseif a:type == "controller"
		call g:PylonsCreateController()
	elseif a:type == "template"
		call g:PylonsCreateTemplate()
	endif
endfunction

" TODO
function! g:PylonsCreateProject()
	throw "Function Unimplemented"
"	let name = input("Project's name: ")
"	let cmd = "paster create -t pylons ".name
"	let g:PylonsProjectName = name
"	call s:PylonsRun(cmd)
endfunction

function! g:PylonsCreateController()
	let name = input("Controller's name: ")
	let cmd = "paster controller ".name
	call s:PylonsRun(s:cmd)
endfunction

function! g:PylonsCreateTemplate()
	let name = input("Template: ")
	if match(name,".mako$") == -1
		let name = name.".mako"
	endif
	let cmd = "touch ".g:PylonsProjectDir.g:PylonsProjectName."/templates/".name
	if s:PylonsRun(cmd)
		echo "Template \'".g:PylonsProjectDir.g:PylonsProjectName."/templates/".name." created"
	endif
endfunction

" Preview in Browser
function! g:PylonsPreview()
	let cmd = g:PylonsBrowser." ".g:PylonsHost.":".g:PylonsPort
	call s:PylonsRun(cmd)
endfunction

