"  makeinbuilddir.vim: (global plugin) Build in a separate directory for
"  autoconf and libtool
"  Maintainer:  sylvain.joyeux@m4x.org
"  Version:     0.1 for vim 7.0+
"  URL:		
"
" Documentation:
"   Let's consider separate source and build directories
"	source_dir
"	source_dir/build
"
"   If you're editing a file in source_dir/src and type Make
"   then 
"	1/ the name of the build directory will be asked (here 'build')
"	   (it is asked once)
"	2/ :make will be run in source_dir/build/src
"
"   source_dir is found by searching for configure.ac, configure.in
"   or configure
"
"   The following additional commands are defined:
"	Compile - compiles the current file by calling the libtool-specific
"		:make basename.lo
"	MakeAll - runs :make in source_dir regardless of the directory of
"		the current file
"	Reconfigure - runs "config.status --recheck" in source_dir/build
"	RegenAll - runs "config.status" in source_dir/build
"	Run execname - starts execname instead of :make
"	Debug execname - start a libtool-wrapped gdb (for libtool generated
"		executables)
"

if exists("loaded_makebuilddir")
    finish
endif
let loaded_makebuilddir=1

" Find the directory containing 'test_file'. test_file
" can be either a file (test_kind == 'file') or a directory
" (test_kind == 'dir')
function s:FindDir(test_file, test_kind)
    let test_with = {'dir': 'isdirectory', 'file': 'filereadable'}
    let test_function = test_with[a:test_kind]

    let curdir = expand("%:p:h")
    let basedir = curdir
    while curdir != "/" && !call(test_function, [curdir . "/" . a:test_file])
	let curdir = fnamemodify(curdir, ':h')
    endwhile

    if curdir == '/'
        echoerr "cannot find the " . a:test_kind . " " . a:test_file . " from " . basedir
    elseif curdir !~ '/$'
        return curdir . '/'
    else
        return curdir
    endif
endfunction

function s:FindTopdir()
    return s:FindDir('configure', 'file')
endfunction

function s:Setup()
    let cwd = getcwd()

    let basedir=expand('%:p:h')
    try
	execute "lcd" basedir
	let topdir = s:FindTopdir()
	let subdir=substitute(basedir, topdir . '\?', "", "")

	execute "lcd" topdir
	while 1
	    if !exists("g:builddir")
		let builddir=input("Build directory: ")
	    else
		let builddir=g:builddir
	    endif

	    unlet! g:builddir
	    let subdir_full=builddir . "/" . subdir

	    echohl WarningMsg
	    if !isdirectory(builddir)
		echo builddir . " does not exist or is not a directory\n"
	    elseif !filereadable(builddir . "/Makefile")
		echo builddir . "/Makefile does not exist\n"
	    elseif !filereadable(builddir . "/config.status")
		echo builddir . "/config.status does not exist\n"
	    elseif !isdirectory(subdir_full)
		echo subdir_full . " does not exist or is not a directory\n"
	    elseif ( filereadable(subdir . "/Makefile.in") || filereadable(subdir . "/Makefile") ) && !filereadable(subdir_full . "/Makefile") 
		echo subdir_full . "/Makefile does not exist\n"
	    else
		echohl None
		let g:builddir=builddir
		break
	    endif
	    echohl None
	endwhile
    finally
	execute "lcd" cwd
    endtry

    return [subdir, topdir]
endfunction

function s:FunctionInBuilddir(command, use_subdir, ...)
    let [subdir, topdir]=s:Setup()
    let curdir=getcwd()

    let newdir=topdir . "/" . g:builddir
    if a:use_subdir
        let newdir=newdir . "/" . subdir
    endif

    try
	execute "lcd" newdir
	execute a:command join(a:000, ' ')
    finally
	execute "lcd" curdir
    endtry
endfunction

function s:CompileInBuilddir()
    let file=expand("%:t")
    let file=substitute(file, "\\.\\a*$", ".lo", "")
    let filedir=expand("%:h")

    try
	let curdir=getcwd()
	execute "lcd" filedir
	call s:FunctionInBuilddir("make " . file, 1)
    finally
        execute "lcd" curdir
    endtry
endfunction

function s:ChooseBuilddir()
    unlet! g:builddir
    call s:Setup()
    echo getcwd() . "/" . g:builddir
endfunction 

command Compile :call s:CompileInBuilddir()
command -nargs=* Make :call s:FunctionInBuilddir("make", (exists("makeall") ? ! makeall : 1), <q-args>)
command -nargs=* Makeall :call s:FunctionInBuilddir("make", 0, <q-args>)
command -nargs=1 Run :call s:FunctionInBuilddir("! " . <args>, 1)
command -nargs=1 Debug :call s:FunctionInBuilddir("! libtool --mode=execute gdb " . <args>, 1)
command Reconfigure :call s:FunctionInBuilddir("! ./config.status --recheck && ./config.status", 0)
command RegenAll :call s:FunctionInBuilddir("! ./config.status", 0)
command Builddir :call s:ChooseBuilddir()

