" Vim syntax file
" Language: Jam (specifically boost jam)
" Maintainer: Markus Peloquin <markus@cs.wisc.edu>
" URL: http://www.cs.wisc.edu/~markus
" Last Change: 2009-02-24
"
" Giving credit where credit is due, I based this off of the version by
" Matt Armstrong (see his message here):
"     http://maillist.perforce.com/pipermail/jamming/2000-February/000857.html
" Also, I seriously hope somebody takes this and makes it better, because I
" am absolutely clueless about both jam and vim syntax files.  I only needed
" enough to make my text look pretty.
"
" To install, I put it in ~/.vim/syntax and put the following to
" ~/.vim/filetype.vim:
"
"	if exists("did_load_filetypes")
"		finish
"	endif
"
"	" jam detection
"	augroup filetypedetect
"		au! BufRead,BufNewFile Jamroot		setfiletype jam
"	augroup END


" Remove any old syntax stuff hanging around
sy clear

sy case match

" Jam keywords
sy keyword jamCond	if else for switch
sy keyword jamLabel	case default
sy keyword jamKey	all in actions rule local include on
sy keyword jamAction	bind existing ignore piecemeal quietly together updated
sy keyword jamBuiltinVar	UNIX NT VMS MAC OS2 OS OSVER OSPLAT JAMVERSION
sy keyword jamBuiltinVar	JAMUNAME HDRSCAN
sy keyword jamBuiltin	ALWAYS DEPENDS ECHO EXIT DIE INCLUDES LEAVES NOCARE
sy keyword jamBultin	NOTFILE NOUPDATE TEMPORARY SEARCH LOCATE HDRRULE

" Jambase stuff
sy keyword Jambase	first shell files lib exe obj dirs clean uninstall using Archive As BULK Bulk C++ Cc CcMv Chgrp Chmod Chown Clean CreLib FILE File Fortran GenFile GenFile1 HDRRULE HardLink HdrRule Install InstallBin InstallFile InstallInto InstallLib InstallMan InstallShell Lex Library LibraryFromObjects Link Link LinkLibraries Main MainFromObjects MakeLocate MkDir MkDir1 Object ObjectC++Flags ObjectCcFlags ObjectHdrs Objects Ranlib RmTemps Setuid Shell SubDir SubDirC++Flags SubDirCcFlags SubDirHdrs SubInclude Undefines UserObject Yacc Yacc1 addDirName makeCommon makeDirName makeGrist makeGristedName makeRelPath makeString makeSubDir makeSuffixed unmakeDir AR ARFLAGS AS ASFLAGS AWK BINDIR C++ C++FLAGS CC CCFLAGS CHMOD CP CRELIB CW CWGUSI CWMAC CWMSL DOT DOTDOT EXEMODE FILEMODE FORTRAN FORTRANFLAGS HDRS INSTALL JAMFILE JAMRULES JAMSHELL LEX LIBDIR LINK LINKFLAGS LINKLIBS LN MANDIR MKDIR MSIMPLIB MSLIB MSLINK MSRC MV NOARSCAN OPTIM RANLIB RCP RM RSH RUNVMS SED SHELLHEADER SHELLMODE SLASH STDHDRS STDLIBPATH SUFEXE SUFLIB SUFOBJ UNDEFFLAG WATCOM YACC YACCFILES YACCFLAGS SUBDIRCCFLAGS RELOCATE SEARCH_SOURCE SUBDIRHDRS MODE OSFULL SUBDIRASFLAGS SUBDIR_TOKENS LOCATE_SOURCE LOCATE_TARGET UNDEFS SOURCE_GRIST GROUP OWNER NEEDLIBS SLASHINC

" Variables
" $(XX), where XX contains no " character
sy match jamVar '\$([^)"]*)'
" $XX where XX does not start with a ( and contains no space, ", or \
sy match jamVar '\$[^( \t"\\][^ \t"\\]*'
" <XX> is sort of like a variable
sy match jamVar '<[^>]*>'

" Strings
" (there are no special escapes like \x01 or \n)
sy match jamStringSpecial "\\." contained
sy cluster jamStringElement contains=jamStringSpecial,jamVar
sy region jamString matchgroup=jamStringStartEnd start='"' end='"' contains=@jamStringElement

" Comments
" (only comments [by bjam standards] if they follow whitespace)
sy match jamComment "\([ \t]\|^\)#.*" contains=jamTodo,jamVar,jamString
sy keyword jamTodo TODO FIXME XXX contained

" Errors
" (semicolons and colons typically can't be preceded or followed by anything
" but whitespace)
sy match jamSemiError "[^ \t];"hs=s+1
sy match jamSemiError ";[^ \t]"hs=s+1
sy match jamColonError "[^ \t]:"hs=s+1
sy match jamColonError ":[^ \t]"hs=s+1

if !exists("did_jamstyle_syntax_inits")
	let did_jamstyle_syntax_inits = 1

	hi link jamColonError jamError
	hi link jamSemiError jamError

	hi link jamCond Conditional
	hi link jamLabel Label
	hi link jamKey Keyword
	hi link jamAction Keyword
	hi link jamBuiltin Keyword
	hi link jamBuiltinVar Keyword
	hi link Jambase Identifier

	hi link jamVar Identifier

	hi link jamString String
	hi link jamStringSpecial Special

	hi link jamComment Comment
	hi link jamTodo Todo

	hi link jamError Error
endif

let b:current_syntax = "jamstyle"
