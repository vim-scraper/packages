" Vim autoload script
" File:         testname.vim
" Created:      2008 Sep 16
" Last Change:  2009 Oct 19
" Rev Days:     9
" Author:	Andy Wokula <anwoku@yahoo.de>
" License:	Vim License, see :h license

" Description:
"   Get unused file name for testing purpose.
"
"   File names are counted A1,A2,..,A9,B10,..,B99,C100,..  Several "sets"
"   can be defined.  A set specifies path and extension of the file name to
"   be generated.  Files named "lastnr.dat" maintain the counter(s) for each
"   set.  Select a set with a KEY.  An expr-abbr is a useful way to trigger
"   the functions.

" Installation:
" - put script in ~\vimfiles\autoload (any autoload folder)
" - define at least g:testname#set (see below) and one or more abbrevs:
"	:cabbr <expr> tn# testname#GetFreeName(KEY)
"
"   one KEY that already works is "vim":
"	:cabbr <expr> tn# testname#GetFreeName("vim") 

" Usage:
"   :" edit a new test file:
"   :new tn#

" Customization:
"   variables in the vimrc

" g:testname#set    (dictionary)
"
"   :let g:testname#set = {}
"   :let g:testname#set[KEY] = { "ext": EXT, "path": PATH [, "lnk": LNPK] }
"   :...
"
"   KEY - (string) the selector; for use in the abbreviation, e.g. "vim"
"   EXT - (string) extension for the file name to be generated, e.g. ".vim"
"   PATH - (string) path in which to create the new files and "lastnr.dat"
"	(user creates the files, script only creates lastnr.dat)
"   LNPK - (string) optional selector to choose the path for lastnr.dat from
"	another set; "LastNr.dat Path Key".  Makes it possible to have a
"	single lastnr.dat file for all paths.
"
"   * You can overwrite the one default KEY "vim".
"   * Different KEYs can share PATH (the lastnr file contains a line for
"     each KEY), only PATH + EXT must be unique (in general).

" g:testname#lastkey	(string)
"
"   default argument for testname#GetTestPath(), as long as
"   testname#GetFreeName() not called yet (it will set this var)

" g:testname#lastnrfile	    (string)
"
"   name of lastnr-file in PATH; must be given without (esp. absolute) path;
"   default is "lastnr.dat"

" Problems:
" + If a free file name has been requested, but the user doesn't write the
"   file, the file name must be kept available.
" + If a free file name has been requested, the user writes the file and
"   then moves the file to another place, the file name must not be reused.
"   ! 10-07-2008 install a BufWrite autocommand
" + free name requested, but file not written yet; another free name
"   requested and written; if first file is written it will overwrite the
"   second file (same file name)
"   ! not a problem: same file name = same buffer

" Init:
if !exists("g:testname#set")
    let g:testname#set = {}
endif

call extend(g:testname#set, {
    \	"vim": {"ext": ".vim",
    \		"path": matchstr(&rtp,'[^,]*')}}
    \,"keep")

if !exists("g:testname#lastkey")
    let g:testname#lastkey = "vim"
endif

if !exists("g:testname#lastnrfile")
    let g:testname#lastnrfile = "lastnr.dat"
endif

if !exists("g:testname#firstnr")
    let g:testname#firstnr = "A1"
endif

let s:onwrite = {}

augroup testname
augroup End

let s:alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

func! testname#Next(filenr)
    " filenr -- "A1", "B12",..., "C123"; NOT: "C001", ...
    let nr = matchstr(a:filenr, '\d\+') + 1
    return s:alpha[strlen(nr)-1]. nr
endfunc

" a preceding letter stands for the number of following digits
" A:1, B:2, C:3, ..., G:8, ...
" - sorting alpha-numerical
" - short file names with lower numbers
" - almost unlimited number of file names
"   `-> no need to reuse filenames
func! testname#GetFreeName(key) abort
    if !has_key(g:testname#set, a:key)
	echoerr "testname: not a key in testname#set:" a:key
	return "wrong_key"
    endif
    let actset = g:testname#set[a:key]
    let g:testname#lastkey = a:key
    let testpath = testname#GetTestPath(a:key)
    let lanfile = g:testname#lastnrfile

    if !has_key(actset, "lnk")
	let lastnr_fqname = testpath. '/'. lanfile
    else
	try
	    let lnpath = testname#GetTestPath(actset.lnk)
	catch
	    let lnpath = testpath
	endtry
	let lastnr_fqname = lnpath. '/'. lanfile
    endif

    try
	let lines = readfile(lastnr_fqname)
	let keylnr = match(lines, "^". a:key. " ")
	if keylnr >= 0
	    let filenr = matchstr(lines[keylnr], ' \zs\w\+')
	else
	    let filenr = g:testname#firstnr
	    call add(lines, a:key. " ". filenr)
	    let keylnr = len(lines)-1
	endif

	" let filenr = readfile(lastnr_fqname)[0]
	let mknewlastnrfile = 0
    catch /:E484:/
	" can't open file
	let filenr = g:testname#firstnr
	let lines = [""]
	let keylnr = 0
	let mknewlastnrfile = 1
    endtry
    if filenr !~ '\a\d*'
	let filenr = g:testname#firstnr
    endif

    let testfilename = testpath. '/'. filenr. actset.ext
    while glob(testfilename) != ""
	let filenr = testname#Next(filenr)
	let testfilename = testpath. '/'. filenr. actset.ext
    endwhile

    let lines[keylnr] = a:key. " ". filenr
    call writefile(lines, lastnr_fqname)
    " just abort here if writing fails
    if mknewlastnrfile
	echomsg "testname: '".lastnr_fqname."' created"
    endif

    let autocmdpat = tr(testpath,'\','/').'/*'. actset.ext
    exec "au! testname BufWrite" autocmdpat 'call s:IncOnWrite("'.a:key.'","'.filenr.'")'
    let s:onwrite[a:key] = [keylnr, lastnr_fqname, autocmdpat]

    return testfilename
endfunc

func! testname#GetTestPath(...)
    let key = a:0 >= 1 ? a:1 : g:testname#lastkey
    let testpath = glob(g:testname#set[key].path)
    if !isdirectory(testpath)
	throw 'Testname: Not a directory: '. g:testname#set[key].path
    endif
    return testpath
endfunc

func! s:IncOnWrite(key, usednr)
    " need (once): lastnr_fqname, autocmdpat
    let [keylnr, lastnr_fqname, autocmdpat] = s:onwrite[a:key]
    let lines = readfile(lastnr_fqname)
    " requires that the line number has not changed in the file:
    let filenr = matchstr(lines[keylnr], ' \zs\w\+')
    if filenr == a:usednr
	let filenr = testname#Next(filenr)
	let lines[keylnr] = a:key. " ". filenr
	call writefile(lines, lastnr_fqname)
    endif
    exec "au! testname BufWrite" autocmdpat
    unlet s:onwrite[a:key]
endfunc

