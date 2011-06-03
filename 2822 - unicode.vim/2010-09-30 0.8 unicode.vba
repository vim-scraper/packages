" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/unicode.vim	[[[1
40
" unicodePlugin : A completion plugin for Unicode glyphs
" Author: C.Brabandt <cb@256bit.org>
" Version: 0.8
" Copyright: (c) 2009 by Christian Brabandt
"            The VIM LICENSE applies to unicode.vim, and unicode.txt
"            (see |copyright|) except use "unicode" instead of "Vim".
"            No warranty, express or implied.
"  *** ***   Use At-Your-Own-Risk!   *** ***
"
" TODO: enable GLVS:
" GetLatestVimScripts: 2822 8 :AutoInstall: unicode.vim

" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("g:loaded_unicodePlugin")
 finish
endif
let g:loaded_unicodePlugin = 1
let s:keepcpo              = &cpo
set cpo&vim

let s:enableUnicodeCompletion = (exists("g:enableUnicodeCompletion") ? g:enableUnicodeCompletion : 0)
" ------------------------------------------------------------------------------
" Public Interface: {{{1
com! EnableUnicodeCompletion call unicode#Init(1)
com! DisableUnicodeCompletion call unicode#Init(0)
com! UnicodeName call unicode#GetUniChar()

if s:enableUnicodeCompletion
    call unicode#Init(s:enableUnicodeCompletion)
    "let s:enableUnicodeCompletion = !s:enableUnicodeCompletion
endif



" =====================================================================
" Restoration And Modelines: {{{1
" vim: fdm=marker
let &cpo= s:keepcpo
unlet s:keepcpo
autoload/unicode.vim	[[[1
252
" unicodePlugin : A completion plugin for Unicode glyphs
" Author: C.Brabandt <cb@256bit.org>
" Version: 0.8
" Copyright: (c) 2009 by Christian Brabandt
"            The VIM LICENSE applies to unicode.vim, and unicode.txt
"            (see |copyright|) except use "unicode" instead of "Vim".
"            No warranty, express or implied.
"  *** ***   Use At-Your-Own-Risk!   *** ***
"
" GetLatestVimScripts: 2822 8 :AutoInstall: unicode.vim

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

fun! <sid>WarningMsg(msg)"{{{1
        echohl WarningMsg
        let msg = "UnicodePlugin: " . a:msg
        if exists(":unsilent") == 2
                unsilent echomsg msg
        else
                echomsg msg
        endif
        echohl Normal
endfun

fu! unicode#CheckUniFile(force) "{{{1
    if (!filereadable(s:UniFile) || (getfsize(s:UniFile) == 0)) || a:force
		call s:WarningMsg("File " . s:UniFile . " does not exist or is zero.")
		call s:WarningMsg("Let's see, if we can download it.")
		call s:WarningMsg("If this doesn't work, you should download ")
		call s:WarningMsg(s:unicode_URL . " and save it as " . s:UniFile)
		sleep 10
		if exists(":Nread")
			sp +enew
			" Use the default download method. You can specify a different one,
			" using :let g:netrw_http_cmd="wget"
			exe ":lcd " . s:directory
			exe "0Nread " . s:unicode_URL
			$d _
			exe ":w!" . s:UniFile
			if getfsize(s:UniFile)==0
				call s:WarningMsg("Error fetching Unicode File from " . s:unicode_URL)
				return 0
			endif
			bw
		else
			call s:WarningMsg("Please download " . s:unicode_URL)
			call s:WarningMsg("and save it as " . s:UniFile)
			call s:WarningMsg("Quitting")
			return 0
		endif
    endif
    return 1
endfu

fu! unicode#CheckDir() "{{{1
    try
		if (!isdirectory(s:directory))
			call mkdir(s:directory)
		endif
    catch
		call s:WarningMsg("Error creating Directory: " . s:directory)
		return 0
    endtry
    return unicode#CheckUniFile(0)
endfu

fu! unicode#UnicodeDict() "{{{1
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
endfu

fu! unicode#CompleteUnicode(findstart,base) "{{{1
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
		let dg_char=unicode#GetDigraphChars(value)
        if s:showDigraphCode
			if !empty(dg_char)
				let fstring=printf("U+%04X %s (%s):'%s'", value, key, dg_char, nr2char(value))
			else
			let fstring=printf("U+%04X %s:%s", value, key, nr2char(value))
			endif
		else
			let fstring=printf("U+%04X %s:'%s'", value, key, nr2char(value))
		endif
		let istring=printf("U+%04X %s%s:'%s'", value, key, empty(dg_char) ? '' : '('.dg_char.')', nr2char(value))
	    
    	call complete_add({'word':nr2char(value), 'abbr':fstring, 'info': istring})
		if complete_check()
			break
		endif
    endfor
    	
    return {}
  endif
endfu

fu! unicode#GetDigraph() "{{{1
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

fu! unicode#GetDigraphChars(code) "{{{1
    let dlist = unicode#GetDigraph()
    let ddict = {}
    for digraph in dlist
		let key=matchstr(digraph, '\d\+$')+0
		let val=split(digraph)
		let ddict[key] = val[0]
    endfor
    return get(ddict, a:code, '')
endfu

fu! unicode#CompleteDigraph() "{{{1
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
       "echo args
       "if !empty(t)
	   let format=printf("'%s' %s U+%04X",t[1], t[2], t[3])
	   call add(tlist, {'word':nr2char(t[3]), 'abbr':format,
				   \'info': printf("Abbrev\tGlyph\tCodepoint\n%s\t%s\tU+%04X",t[1],t[2],t[3])})
       "endif
   endfor
   call complete(col, tlist)
   return ''
endfu

fu! unicode#CompareList(l1, l2) "{{{1
    return a:l1[1] == a:l2[1] ? 0 : a:l1[1] > a:l2[1] ? 1 : -1
endfu

fu! unicode#Init(enable) "{{{1
    if a:enable
		let b:oldfunc=&l:cfu
		if (unicode#CheckDir())
			let s:UniDict = unicode#UnicodeDict()
			setl completefunc=unicode#CompleteUnicode
			set completeopt+=menuone
			inoremap <C-X><C-G> <C-R>=unicode#CompleteDigraph()<CR>
		endif
    else
		if !empty(b:oldfunc)
			let &l:cfu=b:oldfunc
		endif
		unlet s:UniDict
    endif
	echo "Unicode Completion " . (a:enable?'ON':'OFF')
endfu

fu! unicode#GetUniChar() "{{{1
	if !exists("s:UniDict")
		let s:UniDict=unicode#UnicodeDict()
	endif
	" Save unnamed register
	let reg=getreg('"',1)
	let regtype = getregtype('"')
    
	" Get glyph at Cursor
	norm! yl
	let glyph=@"

	" CJK Unigraphs start at U+4E00 and go until U+9FFF
	if char2nr(glyph) >= 0x4E00 &&
	\  char2nr(glyph) <= 0x9FFF
		echohl Title
		echo printf("'%s' U+%04X CJK Ideograph", glyph, char2nr(glyph))
		echohl Normal
	else

		for [key, value] in items(s:UniDict)
			if value == char2nr(glyph)
				echohl Title
				echo printf("'%s' U+%04X %s", glyph, value, key)
				echohl Normal
				break
			endif
		endfor
	endif

	" Restore old register contents
	call setreg('"',reg, regtype)
endfun

" Modeline "{{{1
" vim: ts=4 sts=4 fdm=marker com+=l\:\" fdl=0
doc/unicode.txt	[[[1
187
*unicode.txt* A completion plugin for Unicode glyphs

Author:  Christian Brabandt <cb@256bit.org>
Version: 0.8 Thu, 30 Sep 2010 20:55:43 +0200
Copyright: (c) 2009, 2010 by Christian Brabandt 	    *unicode-copyright*
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
By default the plugin is not enabled. To enable it enter: >

    :EnableUnicodeCompletion

When you run this command, unicode.vim checks for the availability of
UnicodeData.txt from the Unicode Consortium, and if it is not available,
it will try to download it. 

This will also set up the completion function |completefunc| to use for your
buffer. You can use |i_CTRL-X_CTRL-U| then to start the completion. 

						 *:DisableUnicodeCompletion*
If you want to disable the plugin, enter >

    :DisableUnicodeCompletion
<
	 						 *:UnicodeName*
Suppose, you want to know, what the Unicode Name for the Character under the
cursor is. You simply enter the ex command: >

    :UnicodeName

The plugin will then output the character, the character's hexadecimal value
and the official Unicode name.

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

Regardless, of you configured the plugin to display the digraph shortcut in
the menu, a |preview-window| will be opened, if your Vim was compiled with the
quickfix-feature and the preview window displays the hexadecimal Unicode
Codepoint, the name, the digraph characters in parenthesis (if they exist)
followed by the glyph itself.

						    *unicode-plugin-config*
The plugin can be customized to include the 2 digraph characters you have to
type, to get that character. This works only, if there is a digraph defined
for that Unicode char. If you would like this you need to set
g:showDigraphCode, e.g. >

    :let g:showDigraphCode=1

This functionality is by default disabled, cause it seems to cause some delay
and screen-rendering errors in the menu. However, the preview window will
always show the digraph character in parenthesis. 
Enter >

    :let g:showDigraphCode=0

to disable the Digraph feature afterwards.

If you would like to specify a different URL from which to download
UnicodeData.txt, enter the URL as: >

    :let g:unicode_URL='http:....'

To force downloading the file from that new url, enter >

    :call unicode#CheckUniFile(1)

If you'd like Unicode completion to be always enabled, you can set the
variable g:enableUnicodeCompletion to 1 in your |.vimrc| like this: >

     let g:enableUnicodeCompletion = 1
<
Thus, you won't need to use EnableUnicodeCompletion, it will be already
available.

						     *unicode-plugin-error*
If the plugin gives an error or does not complete anything, first check, that
UnicodeData.txt from the Unicode Consortium has been successfully downloaded.
It should be located below the autoload/unicode.vim script in a directory
called unicode. So if you have installed unicode.vim into
/home/user/.vim, UnicodeData.txt should be located at:
/home/user/.vim/autoload/unicode/UnicodeData.txt and should look like this:

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

If the file looks correct, and the plugin is still not working correctly
contact the maintainer. You'll find his email-adress in the first line of this
document. Please be patient, it might take a while, until I can take care of
your report.

==============================================================================
					*i_CTRL-X_CTRL-G* *digraph-completion*
3. Completing digraphs

CTRL-X CTRL-G		Search for the character in front of the cursor and
			try to complete this letter using a digraph. If there
			is no letter in front of the cursor, a list with all
			available digraphs is shown in a popup menu.
			(Think of Glyph)
       CTRL-N           Use next match. This match replaces the previous
			match.
       CTRL-P           Use previous match. This match replaces the previous
			one.


==============================================================================
4. unicode History			              *unicode-plugin-history*
    0.8: Sep 30, 2010:  - Fix an issue with configuring the plugin (Thanks jgm)
			- Code cleanup
			- Make use of the preview window, when completing
			  Digraph or Unicode Glyphs
			- By default, the Digraph Glyphs will now be enabled
			  using |i_Ctrl-X_CTRL-G| instead of using
			  Ctrl-X_Ctrl-C which wouldn't work in a terminal
			- |:UnicodeName| now displays the hexadecimal Unicode
			  Codepoint instead of the decimal one (as this seems
			  to be the official way to display unicode
			  codepoints).
    0.7: Sep 23, 2010:  - |:UnicodeName|
                        - specify g:enableUnicodeCompletion to have unicode
			  completion always enabled.
    0.6: Aug 26, 2010:  - many small bugfixes with regard to error-handling
                          and error displaying
                        - use default netrw_http_cmd (instead of hardwiring
			  wget)
			- small documentation update (Inlude a snippet of
			  UnicodeData.txt and get rid of Index.txt data)
    0.5: Apr 19, 2010:  Created a public repository for this plugin at
			    http://github.com/chrisbra/unicode.vim
    0.4: Feb 01, 2010:  Use UnicodeData.txt to generate Data
                        (Index.txt does not contain all glyphs).
			Check for empty file UnicodeData.txt
    0.3: Oct 27, 2009:	Digraph Completion
    0.2: Oct 22, 2009:	Enabled GetLatestScripts (|GLVS|)
    0.1: Oct 22, 2009:	First working version

==============================================================================
vim:tw=78:ts=8:ft=help
