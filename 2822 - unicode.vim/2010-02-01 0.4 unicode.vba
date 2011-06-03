" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/unicode.vim	[[[1
217
" unicodePlugin : A completion plugin for Unicode glyphs
" Author: C.Brabandt <cb@256bit.org>
" Version: 0.4
" Copyright: (c) 2009 by Christian Brabandt
"            The VIM LICENSE applies to unicode.vim, and unicode.txt
"            (see |copyright|) except use "unicode" instead of "Vim".
"            No warranty, express or implied.
"  *** ***   Use At-Your-Own-Risk!   *** ***
"
" GetLatestVimScripts: 2822 4 :AutoInstall: unicode.vim

" ---------------------------------------------------------------------


if exists("g:unicode_URL")
    let s:unicode_URL=g:unicode_URL
else
    "let s:unicode_URL='http://www.unicode.org/Public/UNIDATA/Index.txt'
    let s:unicode_URL='http://www.unicode.org/Public/UNIDATA/UnicodeData.txt'
endif


let s:file=matchstr(s:unicode_URL, '[^/]*$')

let s:directory  = expand("<sfile>:p:h")."/unicode"
let s:UniFile    = s:directory . '/UnicodeData.txt'

let s:debug = 0

fu! unicode#CheckUniFile(force)"{{{
    if (!filereadable(s:UniFile) || (getfsize(s:UniFile) == 0)) || a:force
	echohl WarningMsg
	echomsg "File " . s:UniFile . " does not exist or is zero."
	echomsg "Let's see, if we can download it."
	echomsg "If this doesn't work, you should download "
	echomsg s:unicode_URL . " and save it as " . s:UniFile
	echohl Normal
	if exists(":Nread")
	    try
		sp +enew
		"It seems like when netrw uses elinks, the file is downloaded 
		"corrupted. Therefore download Index.txt using wget
		exe ":lcd " . s:directory
		let g:netrw_http_cmd="wget"
		exe "0Nread " . s:unicode_URL
		if getfsize(s:UniFile)==0
		    throw "Error downloading ".s:UniFile
		endif
	    catch
		echoerr "Error fetching Unicode File from " . s:unicode_URL
		return 0
	    endtry
	    $d
	    exe ":w!" . s:UniFile
	    bw
	else
	    echoerr "Please download " . s:unicode_URL
	    echoerr "and save it as " . s:UniFile
	    echoerr "Quitting"
	    return 0
	endif
    endif
    return 1
endfu"}}}

fu! unicode#CheckDir()"{{{
    if (!isdirectory(s:directory))
	try
	    call mkdir(s:directory)
	catch
	    echoer "Error creating Directory: " . s:directory
	    return 0
	endtry
    endif
    return unicode#CheckUniFile(0)
endfu"}}}

fu! unicode#UnicodeDict()"{{{
    let dict={}
    let list=readfile(s:UniFile)
    for glyph in list
	let val          = split(glyph, ";")
	let U1Name       = val[10]
	let U1Name       = (!empty(U1Name)?' ('.U1Name.')':'')
	let Name         = val[1]
        let dict[Name]   = str2nr(val[0],16)
    endfor
"    let dict=filter(dict, 'v:key !~ "Control Code"')
    return dict
endfu"}}}

fu! unicode#CompleteUnicode(findstart,base)
  if !exists("s:numeric")
      let s:numeric=0
  endif
  if a:findstart
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\w\|+'
      let start -= 1
    endwhile
    if line[start] =~# 'U' && line[start+1] == '+' && col('.')-1 >=start+2
	let s:numeric=1
    else
	let s:numeric=0
    endif
    return start
  else
    if exists("g:showDigraphCode")
	let s:showDigraphCode=g:showDigraphCode
    else
	let s:showDigraphCode = 0
    endif
    "let glyphs=unicode#UnicodeDict()
    if s:numeric
	let complete_list = filter(copy(s:UniDict), 'printf("%04X", v:val) =~? "^0*".a:base[2:]')
    else
	let complete_list = filter(copy(s:UniDict), 'v:key =~? a:base')
    endif
    for [key, value] in sort(items(complete_list), "unicode#CompareList")
    	"let key=matchstr(key, "^[^0-9 ]*")
        if s:showDigraphCode
	    let dg_char=unicode#GetDigraphChars(value)
	    if !empty(dg_char)
		let fstring=printf("U+%04X %s (%s):%s", value, key, dg_char, nr2char(value))
	    else
		let fstring=printf("U+%04X %s:%s", value, key, nr2char(value))
	    endif
	else
	    let fstring=printf("U+%04X %s:%s", value, key, nr2char(value))
	endif
	    
    	call complete_add({'word':nr2char(value), 'abbr':fstring})
	if complete_check()
	  break
	endif
    endfor
    	
    return {}
  endif
endfu

fu! unicode#GetDigraph()
    redir => digraphs
	silent digraphs
    redir END
    let dlist=[]
    let dlist=map(split(substitute(digraphs, "\n", ' ', 'g'), '..\s<\?.\{1,2\}>\?\s\+\d\{1,5\}\zs'), 'substitute(v:val, "^\\s\\+", "", "")')
    " special case: digraph 57344: starts with 2 spaces
    "return filter(dlist, 'v:val =~ "57344$"')
    let idx=match(dlist, '57344$')
    let dlist[idx]='   '.dlist[idx]

    return dlist
endfu

fu! unicode#GetDigraphChars(code)
    let dlist = unicode#GetDigraph()
    let ddict = {}
    for digraph in dlist
	let key=matchstr(digraph, '\d\+$')+0
	let val=split(digraph)
	let ddict[key] = val[0]
    endfor
    return get(ddict, a:code, '')
endfu

fu! unicode#CompleteDigraph()
   let prevchar=getline('.')[col('.')-2]
   let dlist=unicode#GetDigraph()
   if prevchar !~ '\s' && !empty(prevchar)
       let dlist=filter(dlist, 'v:val =~ "^".prevchar')
       let col=col('.')-1
   else
       let col=col('.')
   endif
   let tlist=[]
   for args in dlist
       let t=matchlist(args, '^\(..\)\s<\?\(..\?\)>\?\s\+\(\d\+\)$')
       echo args
       "if !empty(t)
	   let format=printf("%s %s U+%05d",t[1], t[2], t[3])
	   call add(tlist, {'word':nr2char(t[3]), 'abbr':format})
       "endif
   endfor
   call complete(col, tlist)
   return ''
endfu


"com! Digraph for i in unicode#GetDigraph() | echo i | endfor



fu! unicode#CompareList(l1, l2)
    return a:l1[1] == a:l2[1] ? 0 : a:l1[1] > a:l2[1] ? 1 : -1
endfu


fu! unicode#Init(enable)
    if a:enable
	let b:oldfunc=&l:cfu
	if (unicode#CheckDir())
	    let s:UniDict = unicode#UnicodeDict()
	    setl completefunc=unicode#CompleteUnicode
	    set completeopt+=menuone
	    inoremap <C-X><C-C> <C-R>=unicode#CompleteDigraph()<CR>
	    echo "Unicode Completion " . (a:enable?'ON':'OFF')
	endif
    else
	if !empty(b:oldfunc)
	    let &l:cfu=b:oldfunc
	endif
	unlet s:UniDict
	echo "Unicode Completion " . (a:enable?'ON':'OFF')
    endif
endfu
plugin/unicode.vim	[[[1
39
" unicodePlugin : A completion plugin for Unicode glyphs
" Author: C.Brabandt <cb@256bit.org>
" Version: 0.4
" Copyright: (c) 2009 by Christian Brabandt
"            The VIM LICENSE applies to unicode.vim, and unicode.txt
"            (see |copyright|) except use "unicode" instead of "Vim".
"            No warranty, express or implied.
"  *** ***   Use At-Your-Own-Risk!   *** ***
"
" TODO: enable GLVS:
" GetLatestVimScripts: 2822 4 :AutoInstall: unicode.vim

" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("g:loaded_unicodePlugin")
 finish
endif
let g:loaded_unicodePlugin = 1
let s:keepcpo              = &cpo
set cpo&vim

let s:enableUnicodeCompletion = 0
" ------------------------------------------------------------------------------
" Public Interface: {{{1
com! EnableUnicodeCompletion call unicode#Init(1)
com! DisableUnicodeCompletion call unicode#Init(0)

if s:enableUnicodeCompletion
    call unicode#Init(s:enableUnicodeCompletion)
    "let s:enableUnicodeCompletion = !s:enableUnicodeCompletion
endif



" =====================================================================
" Restoration And Modelines: {{{1
" vim: fdm=marker
let &cpo= s:keepcpo
unlet s:keepcpo
doc/unicode.txt	[[[1
127
*unicode.txt* A completion plugin for Unicode glyphs - Vers 0.4  Feb 01, 2010

Author:  Christian Brabandt <cb@256bit.org>
Copyright: (c) 2009 by Christian Brabandt 		    *unicode-copyright*
           The VIM LICENSE applies to unicode.vim and unicode.txt
           (see |copyright|) except use unicode instead of "Vim".
	   NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.


==============================================================================
                                                              *unicode-plugin*
1. Functionality

This plugin was written to enable an easier use of any Unicode glyph
available. The unicode.vim Plugin uses the data available from the Unicode
Consortium's website (http://www.unicode.org) to let you enter Unicode
characters using a completion function.

By default, the plugin creates a directory unicode below the path autoload
where this plugin is located. Within this directory it will store  the file
UnicodeData.txt from http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
which it will try to download using |netrw| . If this is unsuccessfull, or
you do not have |netrw| enabled, dowload the file manually and save it in the
unicode directory below the autoload directory in which unicode.vim is
located.


						 *:EnableUnicodeCompletion*
By default the plugin is not enabled. To enable it enter
:EnableUnicodeCompletion
When you run this command, *unicode.vim* checks for the availability of
UnicodeData.txt from the Unicode Consortium, and if it is not available,
it will try to download it. 

This will also set up the completion function |completefunc| to use for your
buffer. You can use |i_CTRL-X_CTRL-U| then to start the completion. 

						 *:DisableUnicodeCompletion*
If you want to disable the plugin, enter
:DisableUnicodeCompletion

==============================================================================
						    *unicode-plugin-usage*
2. Completing Unicode chars

If you have enabled the plugin using |:EnableUnicodeCompletion| then there are
2 possibilities to use the plugin. You can either enter the Unicode Character
name, or enter the Unicode-Codeposition.

For example, you would like to enter Æ, so you enter AE and press |<C-X><C-U>|
while in insert mode. Alternatively you can enter the Unicode-Codepoint: U+C6
and press |<C-X><C-U>| and the popup menu will show you all characters, that
have a codepoint like C6 with leading zeros, eg. U+00C6 and U+0C66

A popup menu will appear, showing you the Unicode-Codeposition value, the
Unicode Character Name and the Unicode Character (and if you have enabled it,
it can also show you the digraph characters needed to create this character in
paranthesis, see |unicode-plugin-config| ). You can scroll down in the menu by
pressing <C-N> and up by pressing <C-P>.

						    *unicode-plugin-config*
The plugin can be customized to include the 2 digraph characters you have to
type, to get that character. This works only, if there is a digraph defined
for that Unicode char. If you would like this you need to set
g:showDigraphCode, e.g.
:let g:showDigraphCode=1
This functionality is by default disabled, cause it seems to cause some delay
and screen-rendering errors in the menu. Enter
:let g:showDigraphCode=0
to disable the Digraph feature afterwards.

If you would like to specify a different URL from which to download
UnicodeData.txt, enter the URL as:
:let g:unicode_URL='http:....'
To force downloading the file from that new url, enter
:call unicode#CheckUniFile(1)

						     *unicode-plugin-error*
If the plugin gives an error, first check, that UnicodeData.txt from the
Unicode Consortium has been successfully downloaded. It should look something
like this:
0020;SPACE;Zs;0;WS;;;;;N;;;;;
0021;EXCLAMATION MARK;Po;0;ON;;;;;N;;;;;
0022;QUOTATION MARK;Po;0;ON;;;;;N;;;;;
0023;NUMBER SIGN;Po;0;ET;;;;;N;;;;;
0024;DOLLAR SIGN;Sc;0;ET;;;;;N;;;;;
0025;PERCENT SIGN;Po;0;ET;;;;;N;;;;;
0026;AMPERSAND;Po;0;ON;;;;;N;;;;;
0027;APOSTROPHE;Po;0;ON;;;;;N;APOSTROPHE-QUOTE;;;;
0028;LEFT PARENTHESIS;Ps;0;ON;;;;;Y;OPENING PARENTHESIS;;;;
0029;RIGHT PARENTHESIS;Pe;0;ON;;;;;Y;CLOSING PARENTHESIS;;;;
[...]
(several thounsand lines following)

elinks is known to mangle that file and make it unusable, so please check
first if the format is right.

If the file looks correct, and the plugin is still not working correctly
contact the maintainer. You'll find his email-adress in the first line of this
document. Please be patient, it might take a while, until I can take care of
your report.

==============================================================================
					*i_CTRL-X_CTRL-C* *digraph-completion*
3. Completing digraphs

CTRL-X CTRL-C		Search for the character in front of the cursor and
			try to complete this letter using a digraph. If there
			is no letter in front of the cursor, a list with all
			available digraphs is shown in a popup menu.
       CTRL-N           Use next match. This match replaces the previous
			match.
       CTRL-P           Use previous match. This match replaces the previous
			one.


==============================================================================
4. unicode History			              *unicode-plugin-history*
    0.4: Feb 01, 2010:  Use UnicodeData.txt to generate Data
                        (Index.txt does not contain all glyphs).
			Check for empty file UnicodeData.txt
    0.3: Oct 27, 2009:	Digraph Completion
    0.2: Oct 22, 2009:	Enabled GetLatestScripts (|GLVS|)
    0.1: Oct 22, 2009:	First working version

==============================================================================
vim:tw=78:ts=8:ft=help
