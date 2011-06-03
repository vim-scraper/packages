" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/lh-map-tools.txt	[[[1
695
*lh-map-tools.txt*	Tools for helping mappings-definitions
			For Vim version 5.7.+	Last change: 19th Feb 2008

		   Map Tools MANUAL	by Luc Hermitte
		    Version 0.6.1


------------------------------------------------------------------------------
Presentation: ~
lh-map-tools.tar.gz contains three |plugin|s which define many functions
and commands that I use to program smart-|mapping|s and |abbreviations|.
These definitions are not aimed at final users, but at vim (ft)plugins
developers.
They can be seen as API-plugins.

Nevertheless, the two bracketing oriented plugins can be integrated very
easily into your system. The typical way to use them is to drop the plugin
files into your {rtp}/plugin/ directory, and to customize/define a |ftplugin|
for every filetype you wish to use the bracketing system with.
You will find examples of use of this bracketing system into my ftplugin
files: vim_set.vim, tex-smart_chars.vim, ML_set.vim, html_set.vim and
|cpp_set.vim|.

Contents~
|misc_map.vim|		Addresses the definition of |insert-mode| and
			|visual-mode| mappings that insert pieces of text like
			program code.
    |mm-How-To|		Advices addressing the best way to use these helpers.
|bracketing.base.vim|	Bases of my bracketing system: markers.
|common_brackets.vim|	Typical pairs of brackets.
|mt-Download|		Where to obtain these files.
|add-local-help|	Instructions on installing this file


------------------------------------------------------------------------------
							*misc_map.vim*
misc_map.vim~
			For Vim version 5.7. and more

This plugin defines several functions and commands to use when you want to
program smart-mappings and abbreviations:
|MapContext()|		for context dependent mappings,
|Map4TheseContexts|
|MapNoContext()|
|MapNoContext2()|
|InsertAroundVisual()|	that inserts a pair of tokens around a visual area,
|BuildMapSeq()|		that expands mappings contained into strings, 
|EatChar()|		function that eats the <space> ending abbreviations,
|InsertSeq()|		high level function to define smart mappings and
                        abbreviations,
|Surround()|		high level fn. to insert text around a visual area.
|IsAMarker()|		that tells whether a |marker| is currently selected.

The typical way to use these helpers is addressed into |mm-How-To|.

In the future, I will certainly move it to the {rtp}/macros/ directory.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
					*MapNoContext()* *MapNoContext2()*
MapNoContext({key}, {sequence}) & MapNoContext2({k},{s})~

These two functions enable to define context-dependent macros -- |mapping|s
and |abbreviations|. Regarding the context of the character under the current
position of the cursor, the functions return either the {key} or the
interpreted {sequence}. The {key} is returned when the context matches either
comments, strings or characters. In a C program, I let you test the following
mapping within comments, strings or anywhere else: >
	:Inoreabbr if <C-R>=MapNoContext('if', 
		\ 'if() {\<CR\>}\<ESC\>?)\<CR\>I')<CR>
<
Regarding the difference between the two functions, the second form is
relevant to define |mapping|s (not |abbreviations|!) when we are up to use
variables like "tarif".
Indeed, if the character before the cursor is a 'keyword' character, the key
is returned instead of the interpreted {sequence} ; cf. 'iskeyword' for more
information about keyword characters. 

Reserve the first form of the function to "keyword-less" {keys}, like the
opening curly-bracket for instance, or |abbreviations|.

Regarding the format of the {sequence} to interpret, every special character
must see its greater-than and lesser-than symbols escaped with
backslash. Thus <esc> becomes '\<esc\>'.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
						*MapContext()*
MapContext({key}, {syn-ctx1} {seq1}, {syn-ctx2}, {seq2}, ... [, {default-seq}])~

This function also enables to define context-dependent macros.
However this function is more precise than |MapNoContext()|.

Regarding the context of the character under the current position of the
cursor, the function returns either:
- the {key} when the context is a string, a comment or a character,
- or the interpreted {seq1} when the context is {syn-ctx1}
- ...
- or the interpreted {default-seq} otherwise ; default value: {key}.

Try for instance in an HTML file: >
	:Inoreab if <c-r>=MapContext('if ', 
	    \ 'javaScript', 'if () {}\<esc\>?)\<cr\>i', 
	    \ 'vb', 'If\<CR\>Then\<CR\>Endif\<esc\>?If\<CR\>o', 
	    \ 'if ')<cr>
<
Regarding the format of the {sequences} to interpret, every special character
must see its greater-than and lesser-than symbols escaped with
backslash. Thus <esc> becomes '\<esc\>'.

Note: Works with Vim 6+ only.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
						*Map4TheseContexts*
Map4TheseContexts({key}, {syn-ctx1} {seq1}, {syn-ctx2}, {seq2}, ... [, {default-seq}])~

This function also enables to define context-dependent macros.
This function is even more precise than |MapContext()|.
It does not make any assumption for strings-, comments-, characters- and
doxygen-context.

Regarding the context of the character under the current position of the
cursor, the function returns either:
- the interpreted {seq1} when the context is {syn-ctx1}
- ...
- or, the interpreted {default-seq} otherwise ; default value: {key}.

Regarding the format of the {sequences} to interpret, every special character
must see its greater-than and lesser-than symbols escaped with
backslash. Thus <esc> becomes '\<esc\>'.

Note: Works with Vim 6+ only.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
						*InsertAroundVisual()*
						*MapAroundVisualLine()*
InsertAroundVisual({begin}, {end}, {isLine}, {isIndented})~
MapAroundVisualLine({begin}, {end}, {isLine}, {isIndented}) [depreciated]~

This helper function is dedicated to the addition of text around a visual
selection. {begin} is added before the selection while {end} is added just
after. The function accepts two options that indicate whether the mapping
should be line-wise and whether the selected text should be re-indented after
the operation.

It can be used to define things like (LaTeX): >
	:vnoremap ]ec :call InsertAroundVisual('\begin{center}',
        \ '\end{center}',1,1)<CR>
>
It also fits perfectly to define the C & co. dedicated visual mappings: >
	:vnoremap ,else :call InsertAroundVisual('else {', '}',1,1)<CR>%
<
Rem.: there are still problems with the indenting and more precisely when a
text is |smartindent|ed under VIM 5.xx ; it seems to work fine with VIM 6.0.
BTW, never use stuff that could be expanded as an |abbreviation| within
{begin} or {end} ; unless you like oddities.

Note: this function will correctly work whatever the value of some options
like 'selection'. Thus, you'd better use this function instead of the
classical: >
	:vnoremap <buffer> < <esc>`>a><esc>`<i<<esc>`>ll

Very important: if the mapping is binded to the same unique key an insert-mode
mapping is binded to and, if the insert-mode mapping is to be preferred when
a |marker| is selected, in select-mode, then we must prefer the function
|Surround()|. Surround() is also able to interpret the sequences passed to
{begin} and {end}.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
							*BuildMapSeq()*
BuildMapSeq( {sequence} )~

This function is aimed at enabling recursive (only one level) context
dependent mappings. Actually, it expands every mapping of the form
'!.\{-}!' and build a string that can be used as second parameter by the
|MapNoContext()| functions.
The expanded mappings are considered to be dedicated to the |INSERT-mode|.

My first aim for this function was to be able to use |markers| within
context-dependent macros.

Here is an example of what I have done with some C++ constructs: >
	:Inoreabbr for <C-R>=Def_Map(
	    \ 'for', 
	    \ '\<c-f\>for (;;) {\<cr\>}\<esc\>?(\<cr\>a',
	    \ '\<c-f\>for (;!mark!;!mark!) '
	    \   . '\<cr\>!mark!\<cr\>}!mark!\<esc\>?(\<cr\>a')<CR>

	function! Def_Map(key, expr1, expr2)
	  if exists('b:usemarks') && b:usemarks
	    return "\<c-r>=MapNoContext2('"
	       \  . a:key."',BuildMapSeq(\"".a:expr2."\"))\<cr>"
	  else
	    return "\<c-r>=MapNoContext2('".a:key."', \"".a:expr1."\")\<cr>"
	  endif
	endfunction
<
Then, within normal context, when |b:usemarks| is set, "for" is expanded into: >
	for (;«»;«») {
	    «»
	}«»
and the cursor placed just after the opening parenthesis.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
					*EatChar()* *:Inoreabbr* *:Iabbr*
EatChar({pattern}), :Inoreabbr, :Iabbr~

If you have always dreamed of |abbreviations| that do not insert the <space>
you typed to make it (the abbrev.) expand, this function is for you!

Let's suppose you want to map "if<space>" to "if ()<left>". Doing this is
quite easy thanks to |imap|. But it does not display the characters as you
type them, unless you use |iabbr|. Unfortunately, this time when you type
"if<space>", a space will be added between the parenthesis. That's not what
you want either.

The function proposed here, and the two commands |:Iabbr| and |:Inoreabbr|
address this problem. Define your abbreviations thanks to these commands, and
spaces won't show up. 

N.B.: I am not the original author of this tip. You have to thank Bram
Moolenar, Benji Fisher and some other people on the VIM mailing list
(|mail-list|) for this. The version I propose in my file does not support
multi-byte characters for the moment. 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
							    *InsertSeq()*
InsertSeq({key}, {sequence} [, {context}])~

This function is meant to return the {sequence} to insert when the {key} is
typed. The result will be function of several things:
- the {sequence} will be interpreted:
  - special characters can be used: '\<cr\>', '\<esc\>', ...  (see
    |ReinterpretEscapedChar()|) ; '\n'
  - we can embed insert-mode mappings whose keybindings match '!.\{-}!' (see
    |BuildMapSeq()|)
    A special treatment is applied on:
    - |!mark!| : according to |b:usemarks|, it is replaced by |Marker_Txt()|
      or nothing
    - |!cursorhere!| : will move the cursor to that position in the sequence
      once it have been expanded.
- the context ; by default, it returns the interpreted sequence when we are
  not within string, character or comment context.  (see |MapNoContext()|).
  Thanks to the optional parameter {context}, we can ask to expand and
  interpret the {sequence} only within some particular {context}.

Examples:~
(*) Excerpt from my vim-ftplugin: >
    inoremap  <buffer> <silent> <M-c> 
      \ <c-r>=InsertSeq('<m-c>', ':call !cursorhere!(!mark!)!mark!')<cr>
    inoreab  <buffer> <silent>  fun      
      \ <C-R>=InsertSeq('fun', 
      \ 'function!!cursorhere!(!mark!)\n!mark!\nendfunction!mark!')<CR>

(*) Excerpt from my c-ftplugin: >
    " Put the current selection within the parenthesis.
    vnoremap <buffer> <silent> <localleader><localleader>wh 
	\ <c-\><c-n>@=Surround('while (', 
	\ '!cursorhere!) {\n!mark!\n}!mark!',
	\ 0, 1, '', 1, 'while ')<cr>
<
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
							    *Surround()*
Surround({begin},{end},{isLine},{isIndented},{goback},{mustInterpret}[,{imSeq]})~

This function is a smart wrapper around |InsertAroundVisual()|.
It permit to interpret {begin} and {end} and it also recognizes whether what
we must surround is a |marker| or not.

The point is that there is no :smap command in VimL, and that insert-mode
mappings (imm) should have the precedence over visual-mode mappings (vmm) when
we deals with selected markers (select-mode) ; unfortunatelly, it is the
contrary: in this context, Vim gives the priority to vmm over imm.

Parameters:~
{begin}, {end}	strings
    The visual selection is surrounded by {begin} and {end}, unless what is
    selected is one (and only one) |marker|. In that latter case, the function
    returns a sequence that will replace the selection by {begin} ; if {begin}
    matches the keybinding of an insert-mode mapping, it will be expanded.
{goback}	string
    This is the normal-mode sequence to execute after the selected text has
    been surrounded; it is meant to place the cursor at the end of {end}
    Typical values are '%' for true-brackets (), {}, [] or '`>ll' when
    strlen({end}) == 1.
    Note: This sequence will be expanded if it contains mappings or
    abbreviations -- this is a feature. see {rtp}/ftplugin/vim_set.vim
{mustInterpret}	boolean
    Indicates whether we must try to find and expand mappings of the form
    "!.\{-1,}!" within {begin} and {end}
    When true:
    - |b:usemarks| is taken into account: when |b:usemarks| is false, {begin}
      and {end} will be cleared from every occurrence of "|!mark!|".
    - if {begin} or {end} contain "|!cursorhere!|", {goback} will be ignored
      and replaced by a more appropriate value.
[{a:1}=={imSeq}]	string, optional
    Insert-mode sequence that must be returned instead of {begin} if we try to
    surround a |marker|.
    Note: This sequence will be expanded if it contains mappings or
    abbreviations -- this is a feature. see {rtp}/ftplugin/vim_set.vim

Usage:~
	:vnoremap <buffer> {key} <c-\><c-n>@=Surround({parameters})<cr>

Examples:~
(*) Excerpt from common_brackets.vim >
   :vnoremap <buffer> <localleader>[
    \ <c-\><c-n>@=Surround('[', ']', 0, 0, '%', 0)<cr>
(*) Excerpt from my vim-ftplugin >
   :vnoremap <buffer> <silent> <m-f> 
    \ <c-\><c-n>@=Surround('function! !cursorhere!(!mark!)', 'endfunction',
    \ 1, 1, '', 1, 'fun ')<cr>
(*) Excerpt from my c-ftplugin >
   :vnoremap <buffer> <silent> <localleader>for 
    \ <c-\><c-n>@=Surround('for (!cursorhere!;!mark!;!mark!) {', '}!mark!',
    \ 1, 1, '', 1, 'for ')<cr>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
							    *!cursorhere!*

"!cursorhere!" has a special meaning for |InsertSeq()| and |Surround()|. This
special string is used to indicate where the cursor should go once the
sequence to insert has been inserted and expanded.
Unlike |!mark!|, there is no mapping associated to !cursorhere!.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
							    *IsAMarker()*
IsAMarker()~
This function tells whether the currently selected text is actually a
|marker|. It helps to compensate the fact Vim does not provide select-mode
mappings (with something like :smap for instance).

Why is this needed ? 
Well. |!jump!| (and similar mappings) selects the next |marker| and goes into
select-mode. At this point anything that is typed replaces the |marker|.
However, if a visual-mode mapping is detected, it is expanded. Unfortunatelly,
we may need visual-mode mappings to not expand. 

Typically, having '{' expanded to surround the |marker| with a pair of
brackets is undesired. Regarding the brackets, |common_brackets.vim| already
handles this -- thanks to |Surround()|, that delegates the detection to
|IsAMarker()|. However, we may still run into visual-mode mappings we do not
wish to see expanded when a marker is selected. 
|IsAMarker()| is provided for this very reason.

Example: g= that evaluates the current selection~
>
    fun! s:Evaluate()
      if IsAMarker() 
        return 'gv"_sg='
      else 
        normal! gvy
        exe ":echo ".@"  
        return ''
      endif
    endfunction
    vnoremap <silent> g= <c-\><c-n>@=<sid>Evaluate()<cr>

Of course, select-mode mappings would have been better. But there is no such
thing (yet?).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
							    *mm-How-To*
How to use the helpers from misc_map.vim~
|misc_map.vim| defines several functions addressing very close issues. Indeed,
these functions rely on each other definitions. 

Hence the questions: "Which one must I use ...
|mmht-im| ... to define Insert-Mode mappings ?"
|mmht-a|  ... to define abbreviations ?"
|mmht-vm| ... to define Visual-Mode mappings ?"
|mmht-cm| ... to define a complex mapping ?"


							    *mmht-im*
Which one(s) must I use to define Insert-Mode mappings ?~
see |InsertSeq()|, |i_CTRL-R|, |!mark!|, |!cursorhere!|, |:inoremap|

Excerpt from my vim-ftplugin: >
    inoremap  <buffer> <silent> <M-c> 
      \ <c-r>=InsertSeq('<m-c>', ':call !cursorhere!(!mark!)!mark!')<cr>
<
							    *mmht-a*
Which one(s) must I use to define abbreviations ?~
see |InsertSeq()|, |i_CTRL-R|, |!mark!|, |!cursorhere!|, |:Inoreabbr|

Excerpt from my vim-ftplugin: >
    inoreab  <buffer> <silent>  fun      
      \ <C-R>=InsertSeq('fun', 
      \ 'function!!cursorhere!(!mark!)\n!mark!\nendfunction!mark!')<CR>

Excerpt from my C-ftplugin: >
    function! Def_AbbrC(key,expr)
      let rhs = a:expr
      if exists('g:c_nl_before_bracket') && g:c_nl_before_bracket
	let rhs = substitute(rhs, '\s*(', '\\n(', 'g')
      endif
      if exists('g:c_nl_before_curlyB') && g:c_nl_before_curlyB
	let rhs = substitute(rhs, '\s*{', '\\n{', 'g')
      endif
      return InsertSeq(a:key, rhs)
    endfunction

    Inoreabbr <buffer> <silent> for <C-R>=Def_AbbrC('for ',
      \ '\<c-f\>for (!cursorhere!;!mark!;!mark!) {\n!mark!\n}!mark!')<cr>
<
							    *mmht-vm*
Which one(s) must I use to define Visual-Mode mappings ?~
see |Surround()|, |v_CTRL-\_CTRL-N|, |@|, |!mark!|, |!cursorhere!|,
|:vnoremap|, |IsAMarker()|.

							    *mmht-cm*
Which one(s) must I use to define a complex mapping ?~
If you need to define a mapping for which |InsertSeq()| can not help you, it
is likely that you are looking for a better management of the context. At this
time, there is no function ready to use. You may have to write your own
function that will delegate its work to |Map4TheseContexts()|.

If the problem only consists in expanding other custom mappings, then define
an adapter (like the design patern) mapping whose keybinding matches
'!.\{-}!'. Have a look at the definition of !cursorhere! and !gotocursor! in
the implementation of |Surround()| and |InsertSeq()|.


------------------------------------------------------------------------------
							*bracketing.base.vim*
bracketing.base.vim ~
			For Vim version 6.0.+ only

						*markers* 
						*!mark!* *!jump!* *!jumpB!*
This file defines the bases of the bracketing system I use. What I propose
here is an improvement over Stephen Riehm original system -- you could find the
whole package with an important documentation and other remarks on Benji
Fisher's homepage. Stephen Riehm introduces several language-independent
mappings to manage brackets-like characters. He also introduces two
interesting global mappings: |!mark!| and |!jump!|, to which I add |!jumpB!|.
They are used to mark (with text markers) positions within the file edited
and, to jump to them.

In visual-mode (and select-mode), |!mark!| will toggle the presence of the
pair of marker-string around the visual selection.


						*marker* 
						*b:marker_open* *b:marker_close*
As I do not expect the same things from a bracketing system when I am
developing in C++ or writing LaTeX documents, I have conducted a little
modification on the original system. I added the possibility to dynamically
change the text markers used -- «» is fine for me except within French LaTeX
documents. It is achieved through two buffer-relative variables (options):
|b:marker_open| and |b:marker_close|. I keep the same default values than the
ones chosen by Stephen Riehm. All the other mappings have been conserved if
you wish to use this file as Stephen Riehm initially planned to. 

						*:SetMarker*
If you wish to change the values of |b:marker_open| and |b:marker_close|, you
can also use the command:
	:SetMarker {open} {close}


I have also added some other features:
						*g:marker_prefers_select*
(*) The option |g:marker_prefers_select| (default 1) determines if the text
    (comment) within a |marker| should be echoed or if the whole marker should
    be selected (in |SELECT-mode|) -- from a Gergely Kontra's idea. Beware
    one thing: the SELECT-mode is considered to be a declension of the
    |VISUAL-mode|. Hence all the |imap|s won't expand in SELECT-mode!
    Fortunately, the |iabbr|s will still expand. To workaround this problem,
    define your |vmap|s with |Surround()|.

						*g:marker_select_empty_marks*
(*) The option |g:marker_select_empty_marks| (default 1) determines whether an
    empty marker should be selected or deleted when a |!jump!| is encountered.
    Only works if |g:marker_prefers_select| is set. 

				*b:use_place_holders* *g:use_place_holders*
(*) The option |g:use_place_holders| (default 0) determines whether the
    marker-characters used are *[bg]:map_PlaceHolderStart* and
    *[bg]:map_PlaceHolderEnd* instead of |b:marker_open| and |b:marker_close|.
    This option ensures a compatibility with Srinath Avadhanula's |imaps.vim|
    plugin.
    I meant to support Srinath's variations on my own variations ; he named
    differently the variables used to define the marker-characters.

			    *b:marker_select_current* *g:marker_select_current*
(*) When the option |g:marker_select_current| (default 0) is set to 1, the
    'jump backward' mechanism will select the current marker (the cursor is
    within) if applicable.  Otherwise, we jump to the marker before the one
    the cursor is within.

		    *b:marker_select_current_fwd* *g:marker_select_current_fwd*
(*) When the option |g:marker_select_current_fwd| (default 1) is set to 1, the
    'jump forward' mechanism will select the current marker (the cursor is
    within) if applicable.  Otherwise, we jump to the marker after the one the
    cursor is within.
    Notes:
    - Even if set, and if a marker is currectly selected, we jump to the next
      before.
    - A template-expander plugin may need to set momentarilly this option to 1
      (this is what |mu-template| does to avoid ignoring the first marker)
    - The last character of a marker is not considered as part of the marker
      regarding this option.

					*b:marker_center* *g:marker_center*
(*) When this option is set to 1 (default 1), the line of the marker (we jump
    to) is moved to the middle of the window.


(*) The next markers are searched according to the 'wrapscan' option.


BTW, I map |!mark!| to <M-Insert>, |!jump!| to <M-Delete>, and |!jumpB!| to
<M-S-Del>. 
If you'd rather use other keybindings, then add into your .vimrc something
like: >
	imap <C-J>	<Plug>MarkersJumpF
	 map <C-J>	<Plug>MarkersJumpF
	imap <C-K>	<Plug>MarkersJumpB
	 map <C-K>	<Plug>MarkersJumpB
	imap <C-<>	<Plug>MarkersMark
	nmap <C-<>	<Plug>MarkersMark
	vmap <C-<>	<Plug>MarkersMark
	

Plugins developpers~
I provide several global functions that could be useful to develop your own
plugins or template-files.
    *Marker_Open()*	returns the first part of the pair of marker-strings.
    *Marker_Close()*	returns the last part of the pair of marker-strings.
    *Marker_Txt()*	builds a marker with the optional parameter passed
Note: the marker-string are automatically converted to match the current
'encoding'.

Also, two mappings *!jump-and-del!* and *!bjump-and-del!* always jump to the
next/previous marker and delete it whatever the options are. It ensures that
the (ft)plugins that depend on these mappings will always do the same thing.

						*g:marker_highlight*
By default markers will be highlighted in a lightgray background.
To disable this feature, set |g:marker_highlight| to 0 in your |.vimrc|.

The dynamic toggling of this feature is tricky at this moment.
You will have to:
- first change the value of |g:marker_highlight|,
- and then do something like >
    :let &encoding=&encoding


------------------------------------------------------------------------------
							*common_brackets.vim*
common_brackets.vim ~
			    For Vim version 6.0.+ only

This file eases the insertion of brackets pairs. It is used by quite all my
consequent ftplugins. 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Main features~
								*b:cb_*
Through a set of buffer-relative options -- cf. the implementation and the
different other language ftplugins of mine -- , it enables and configures
bracketing mappings. 

The bracket-like pairs supported are: 
* parenthesis:      ( and ), 		*b:cb_parent*
* brackets:         [ and ],  		*b:cb_bracket*
* curly-brackets:   { and }, 		*b:cb_acco*
* comparison signs: < and >, 		*b:cb_cmp* *b:cb_ltFn* *b:cb_gtFn*
* single-quotes:    ' and ', 		*b:cb_quotes*
* double-quotes:    " and ",		*b:cb_Dquotes* *b:cb_DqFn*
* and dollars: $ and $ for LaTeX mathematical mode.	*b:cb_mathMode*

The different mappings, described in the following paragraphs, are activated
only when the corresponding buffer-relative options |b:cb_| are set to 1.
These options are not meant to be changed dynamically, but to be set once
within |ftplugins|.

Some behaviors can be tuned much more finely with callback functions:
|b:cb_DqFn| for double quotes, |b:cb_ltFn| for '<' and |b:cb_gtFn| for '>'.
Cf. ML_set.vim and vim_set.vim for examples.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
How it works~

When we hit (in |insert-mode|) the starting character of a pair, the second
one is automatically inserted. In |visual-mode| (resp. |normal-mode|) the
opening character can be hit once [1,2] in order to insert the pair around the
visual area (resp.  current word).  

[1] except for quotes and dollars that have to be hit twice
[2] if |b:cb_bracket| is set to 1, we must hit '<localleader>['. If it is set
    to 2, we can simply hit '['. However, it will hide every standard Vim
    keybindings starting with an open square bracket (|[c|, |[{|, |[z|, ...).

If *b:cb_jump_on_close* is not set to false (0), pressing the second character
from the pair puts the cursor just after the next occurrence of this character
(and in |insert-mode|). But you'd better use |!jump!| in conjunction with the
|markers|. This must be set _before_ Brackets() is called by the various
ftplugins.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Dynamic (de-)activations~

When |Trigger.vim| is installed, the macros defined here can be activated and
deactivated as a whole by pressing <F9>.

								*b:usemarks*
In |insert-mode|, two operating modes are provided: one very classic, and one
that takes advantage of Stephen Riehm's markers when the buffer-relative
option |b:usemarks| is set to 1. In that latter case, markers are inserted at
the end of the pair of brakets.  
The option can be toggled by hitting the trigger <M-F9> -- if |Trigger.vim| is
installed. 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Accessibility~

For personal and technical (the meta-mappings do not suit to the layout of my
French keyboard) reasons, I prefer to use the opening character of a
bracket-like structure in order to insert the whole structure. 

In insert and normal modes, it is also possible to hit <Meta> + the opening
character to insert the corresponding structure around the current/previous
word -- this is not fully supported on French keyboards and is incompatible
with Stephen Riehm's default meta-mappings. 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
						    *brackets_manipulations*
Brackets manipulations~

I have stolen some functions from auctex.vim, and integrated them into
common_brackets.vim -- they are not useful only to LaTeX editing.

(*) When in |insert-mode|, a backslash is under the cursor, and we hit the
    opening character of a pair of "brackets", then another backslash is added
    before the closing "bracket".
(*) In |normal-mode|, we can:
    - delete a pair of brackets                  : with <M-b>x or <M-b><Delete>
    - change a pair of brackets to parenthesis   : with <M-b>(
    - change a pair of brackets to square ones   : with <M-b>[
    - change a pair of brackets to curly ones    : with <M-b>{
    - change a pair of brackets to angle ones    : with <M-b><
    - toggle the backslash on a pair of brackets : with <M-b>\


						*brackets_manipulation_mode*
Actually, by default, there is only one mapping (to <M-b>) that makes VIM
enter a brackets manipulation mode (much like |i_CTRL-X|). From there, you can
hit 'x', '<delete>', '(', '[', '{', '<', or '<F1>' that will behave like
exposed earlier, or display a little help message.

If you don't want of this brackets manipulation mode, in your .vimrc enforce
*g:cb_want_mode* to 0. It values 1 by default.

		    *BracketsManipMode()*
		    *<Plug>DeleteBrackets* *<Plug>ChangeToRoundBrackets*
		    *<Plug>ChangeToSquareBrackets* *<Plug>ChangeToCurlyBrackets*
		    *<Plug>ToggleBackslash*
Again, if you don't like the default keybinding, with Vim 6.0+ you can change
them into your .vimrc with something like: >
	:noremap <silent> <C-L>b	:call BracketsManipMode("\<C-L>b")<cr>
otherwise, if |g:cb_want_mode| is left to 1, define: >
	    :map	<C-L>bx		<Plug>DeleteBrackets
	    :map	<C-L>b<del>	<Plug>DeleteBrackets
	    :map	<C-L>b(		<Plug>ChangeToRoundBrackets
	    :map	<C-L>b[		<Plug>ChangeToSquareBrackets
	    :map	<C-L>b{		<Plug>ChangeToCurlyBrackets
	    :map	<C-L>b<		<Plug>ChangeToAngleBrackets
	    :map	<C-L>b\		<Plug>ToggleBackslash
>
	
------------------------------------------------------------------------------
							*mt-Download*
Where to obtain these files ?~

These plugins can be obtained ...
- ... independently from each other directly on my web site ; browse from:
      <http://hermitte.free.fr/vim/ressources/>
  I advice against proceeding that way because of dependencies.

- ... as a tarball archive available on my web site:
      <http://hermitte.free.fr/vim/ressources/lh-map-tools.tar.gz>
  or on Vim's web site on sourceforge:
      <http://vim.sourceforge.net/scripts/script.php?script_id=50>
  NB: I may not update the archive on SF every time I fix something or enhance
  the plugins.

- ... as part of other (ft)plugins I maintain and share like |lh-cpp|,
  |mu-template|, ....


------------------------------------------------------------------------------
 © Luc Hermitte, 2001-2005 <http://hermitte.free.fr/vim/>
 $Id: lh-map-tools.txt 32 2008-02-19 00:15:03Z luc.hermitte $
 VIM: let b:VS_language = 'american' 
 vim:ts=8:sw=4:tw=78:fo=tcq2:isk=!-~,^*,^\|,^\":ft=help:
plugin/bracketing.base.vim	[[[1
670
" ======================================================================
" $Id: bracketing.base.vim 32 2008-02-19 00:15:03Z luc.hermitte $
" File:		bracketing.base.vim
" Maintainer:	Luc Hermitte <MAIL:hermitte {at} free {dot} fr>
" 		<URL:http://hermitte.free.fr/vim/>
" Last Update:	$Date: 2008-02-19 01:15:03 +0100 (mar., 19 fÃ©vr. 2008) $
" Version:	0.6.1
"
"	Stephen Riehm's braketing macros for vim
"	Customizations by Luc Hermitte.
"
"URL: http://hermitte.free.fr/vim/ressources/vimfiles/plugin/bracketing.base.vim
" ======================================================================
" History:	{{{1
"	15th Feb 2008   by LH
"		* g:marker_select_current_fwd to select the current marker when
"		the cursor on anywhere on it (but the last character)
"	25th May 2006	by LH
"		* Workaround for UTF-8.
"		  Never defined a !imapping! as a call to a script-local
"		  function. Only global functions can be used in UTF-8.
"	22nd Nov 2004	by LH
"		* New behaviour for !mark! in visual mode: 
"		  If a marker is selected (in visual or select mode), then the
"		  marker is replaced by its contents, and we end in insert
"		  mode, just after the contents.
"		  e.g. "<+contents+>" becomes "contents"
"		  Otherwise, the visual selection is wrapped into a pair of
"		  marker characters.
"		* New mappings: !mark! and <Plug>MarkersMark has been added for
"		  the normal mode. 
"		* Revert to g:loaded_bracketing_base
"	23rd May 2004	by LH
"		* Use s:loaded_bracketing_base instead of
"		  g:loaded_bracketing_base
"	17th Sep 2003	by LH
"		* Marker_Jump() works correctly even if 'selection' is set to
"		  exclusive.
"	25th Aug 2003	by LH
"		* And 2 new mappingS: !jump-and-del! !bjump-and-del! which
"		  always delete the marker. Useful to always have the same
"		  behaviour when programming (ft)plugins.
"	15th Aug 2003	by LH, from an idea of Christophe "CLeek" Sahut
"		* New option: [bg]:marker_center used to center the display
"		  area on the marker we have jumped to.
"	25th Jun 2003	by LH
"		* Small correction when b:Marker_Open is set
"	09th jan 2003	by LH
"		* The :vmap for jumping backward uses: "`>" (Srinath's idea)
"		* Add option [bg]:marker_select_current.
"	07th jan 2003	by LH
"		* Changes go into black hole register -> '"_c' -> '@"' is left
"		  unchanged.
"		* Doesn't mess any more the search history ; but the tag
"		  within a marker is not supposed to spread accross several
"		  lines -- I think it is a reasonable requirement.
"		* The marker string may contain escape characters
"		* Insensible to 'magic'
"		* The marker string is automatically converted according to
"		  the current encoding -- may work only between latin1 and
"		  utf-8 because of iconv().
"		* Going into normal mode with <C-\><C-N> instead of
"		  '<esc><c-l>' -> less screen flashes
"		* Little bug fixed with :foldopen
"	10th dec 2002	by LH
"		* Add compatibility with vim-latex::imaps.vim
"	09th dec 2002	by LH
"		* First steps to support encodings different than latin1
"		  - ¡...! mappings changed to !...!
"		  - using nr2char(char2nr("\xab"))
"		* Name of the <Plug> mappings changed
"	20th nov 2002	by LH
"		* Don't mess with folding
"	08th nov 2002	by LH
"		* Pure vim6 solution.
"		* s/brkt/marker in options and functions names.
"		* Support closing markers of several characters.
"		* New mapping to jump to the previous marker
"		* Two commands: :MN and :MP that work in normal, visual and
"		  insert modes and permit to jump to next/previous marker.
"	21st jul 2002	by LH
"		* e-mail address obfuscated for spammers
"	04th apr 2002	by LH
"		* Marker_Txt takes an optional parameter : the text
"	21st feb 2002	by LH
"		* When a comment is within a mark, we now have the possibility
"		  to select the mark (in Select-MODE ...) instead of echoing
"		  the comment.
"		* In this mode, an empty mark can be chosen or deleted.
"	Previous Version: ??????	by LH
"		* Use accessors to acces b:marker_open and b:marker_close. 
"		  Reason: Changing buffers with VIM6 does not re-source this
"		  file.
"	Previous Version: 22nd sep 2001	by LH
"		* Delete all the settings that should depends on ftplugins
"		* Enable to use other markers set as a buffer-relative option.
"	Based On Version: 16.01.2000
"		(C)opyleft: Stephen Riehm 1991 - 2000
"
"	Needs: 	* VIM 6.0 +
"		* misc_map.vim	(MapAroundVisualLines, by LH)
"
"	Credits:
"		Stephen Riehm, Benji Fisher, Gergely Kontra, Robert Kelly IV.
"
"------------------------------------------------------------------------
" Options:	{{{1
" b:marker_open		-> the characters that opens the marker  ; default '«'
" b:marker_close	-> the characters that closes the marker ; default '»'
"	They are buffer-relative in order to be assigned to different values
"	regarding the filetype of the current buffer ; e.g. '«»' is not an
"	appropriate marker with LaTeX files.
"
" g:marker_prefers_select					; default 1
"	Option to determine if the comment within a marker should be echoed or
"	if the whole marker should be selected (select-mode).
"	Beware: The select-mode is considered to be a visual-mode. Hence all
"	the i*map won't expand in select-mode! i*abbr fortunately does.
"	
" g:marker_select_empty_marks					; default 1
"	Option to determine if an empty marker should be selected or deleted.
"	Works only if g:marker_prefers_select is set.
"
" [bg]:use_place_holders					; default 0
"	Option that says the characters to use are [bg]:Imap_PlaceHolderStart
"	and [bg]:Imap_PlaceHolderEnd. 
"	These characters are the ones used by Srinath Avadhanula's imaps.vim 
"	I meant to support Srinath's variations on my own variations ; this
"	way, this script defines correct and advanced jumping functions for
"	the vim-latex suite.
"
" [bg]:marker_select_current					; default 0
"	When this option is set to 1, the 'jump backward' mecanism will
"	select the current marker (the cursor is within) if applicable.
"	Otherwise, we jump to the marker before the one the cursor is within.
"
" [bg]:marker_center						; default 1
"	When this option is set to 1, the line of the marker (we jump to) is
"	moved to the middle of the window.
"
"------------------------------------------------------------------------
" }}}1
" ===========================================================================
" scriptencoding latin1
" Settings	{{{1
" ========
"	These settings are required for the macros to work.
"	Essentially all you need is to tell vim not to be vi compatible,
"	and to do some kind of groovy autoindenting.
"
"	Tell vim not to do screwy things when searching
"	(This is the default value, without c)
"set cpoptions=BeFs
set cpoptions-=c
" Avoid reinclusion
if exists("g:loaded_bracketing_base") && !exists('g:force_load_bracketing_base')
  finish 
endif
let g:loaded_bracketing_base = 1

let s:cpo_save = &cpo
set cpo&vim

" Mappings that can be redefined {{{1
" ==============================
" (LH) As I use <del> a lot, I use different keys than those proposed by SR.
"
if !hasmapto('<Plug>MarkersMark', 'n') && (mapcheck("<M-Insert>", "n") == "")
  nmap <unique> <M-Insert> <Plug>MarkersMark
endif
if !hasmapto('<Plug>MarkersMark', 'v') && (mapcheck("<M-Insert>", "v") == "")
  vmap <unique> <M-Insert> <Plug>MarkersMark
endif
if !hasmapto('<Plug>MarkersMark', 'i') && (mapcheck("<M-Insert>", "i") == "")
  imap <unique> <M-Insert> <Plug>MarkersMark
endif
if !hasmapto('<Plug>MarkersJumpF', 'i') && (mapcheck("<M-Del>", "i") == "")
  imap <unique> <M-Del> <Plug>MarkersJumpF
endif
if !hasmapto('<Plug>MarkersJumpF') && (mapcheck("<M-Del>") == "")
  map <unique> <M-Del> <Plug>MarkersJumpF
endif
if !hasmapto('<Plug>MarkersJumpB', 'i') && (mapcheck("<M-S-Del>", "i") == "")
  imap <unique> <M-S-Del> <Plug>MarkersJumpB
endif
if !hasmapto('<Plug>MarkersJumpB') && (mapcheck("<M-S-Del>") == "")
  map <unique> <M-S-Del> <Plug>MarkersJumpB
endif

" imap <Plug>MarkersMark  !mark!<C-R>=<sid>MoveWithinMarker()<cr>
imap <Plug>MarkersMark  !mark!<C-R>=LHMoveWithinMarker()<cr>
vmap <Plug>MarkersMark  !mark!
nmap <Plug>MarkersMark  !mark!
 map <Plug>MarkersJumpF !jump!
imap <Plug>MarkersJumpF !jump!
 map <Plug>MarkersJumpB !jumpB!
imap <Plug>MarkersJumpB !jumpB!
" Note: don't add "<script>" within the four previous <Plug>-mappings or else
" they won't work anymore.
" }}}

" Commands {{{1
" ========
:command! -nargs=0 -range MP exe ":normal <Plug>MarkersJumpB"
:command! -nargs=0 -range MN exe ":normal <Plug>MarkersJumpF"
:command! -nargs=* -range MI :call s:MarkerInsert(<q-args>)
" :command! -nargs=0 MN <Plug>MarkersJumpF
" :command! -nargs=0 MI <Plug>MarkersMark

" This test function is incapable of detecting the current mode.
" There is no way to know when we are in insert mode.
" There is no way to know if we are in visual mode or if we are in normal mode
" and the cursor is on the start of the previous visual region.
function! s:MarkerInsert(text) range
  let mode =  confirm("'< = (".line("'<").','.virtcol("'<").
	\ ")\n'> =(".line("'>").','.virtcol("'>"). 
	\ ")\n.  =(".line(".").','.virtcol("."). ")\n\n Mode ?",
	\ "&Visual\n&Normal\n&Insert", 1)
  if mode == 1
    normal gv!mark!
  elseif mode == 2
    normal viw!mark!
  elseif mode == 3
    "<c-o>:MI titi toto<cr>
    let text = Marker_Txt(a:text)
    exe "normal! i".text."\<esc>l"
  endif
endfunction

" }}}1

" Jump to next marker {{{1
" ===================
" Rem: 
" * Two working modes : display the text between the markers or select it
" * &wrapscan is implicitly taken into acount
" * The use of the SELECT-mode is inspired by 
"   Gergely Kontra <kgergely at mcl.hu>
" * The backward search of markers is due to by Robert Kelly IV.
" * @" isn't messed thanks to Srinath Avadhanula (/Benji Fisher ?)
"	
function! Marker_Jump(...) " {{{2
  " ¿ forward([1]) or backward(0) ?
  let direction = ((a:0 > 0) && (a:1=='1')) ? '' : 'b'
  let delete    = ((a:0 > 1) && (a:2=='1'))

  " little optimization
  let mo = Marker_Open()	| let emo = escape(mo, '\')
  let mc = Marker_Close()	| let emc = escape(mc, '\')

  " if within a marker, and going backward, {{{3
  if (direction == 'b') && !s:Option('marker_select_current', 0)
    " echomsg 'B, !C'
    let position = line('.') . "normal! ".virtcol('.').'|'
    " then: go to the start of the marker.
    " Principle: {{{
    " 1- search backward the pair {open, close}
    "    In order to find the current pair correctly, we must consider the
    "    beginning of the match (\zs) to be just before the last character of
    "    the second pair.
    " 2- Then, in order to be sure we did jump to a match of the open marker,
    "    we search forward for its closing counter-part.
    "    Test: with open='«', close = 'ééé', and the text:{{{
    "       blah «»
    "       «1ééé  «2ééé
    "       «3ééé foo
    "       «4ééé
    "    with the cursor on any character. }}}
    "    Without this second test, the cursor would have been moved to the end
    "    of "blah «" which is not the beginning of a marker. 
    " }}}
    if searchpair('\V'.emo, '', '\V'.substitute(emc, '.$', '\\zs\0', ''), 'b')
      echo '1-'.string(getpos('.'))
      if ! searchpair('\V'.emo, '', '\V'.emc, 'n')
	echo '2-'.string(getpos('.'))
	" restore cursor position as we are not within a marker.
	exe position
	" echomsg position
      endif
    endif
  endif
  " if within a marker, and going forward, {{{3
  if (direction == '') && s:Option('marker_select_current_fwd', 1)
    " This option must be reserved to 
    " echomsg 'F, C'
    let position = line('.') . "normal! ".virtcol('.').'|'
    " then: go to the start of the marker.
    if searchpair('\V'.emo, '', '\V'.emc, 'w')
      " echomsg '1-'.string(getpos('.'))
      if ! searchpair('\V'.emo, '', '\V'.emc, 'b')
      " echomsg '2-'.string(getpos('.'))
	" restore cursor position as we are not within a marker.
	exe position
	" echomsg position
      else
	" echomsg "premature found"
	return s:DoSelect(emo, emc, delete)
      endif
    endif
  endif
  " }}}3
  " "&ws?'w':'W'" is implicit with search()
  if !search('\V'.emo.'\.\{-}'.emc, direction) " {{{3
    " Case:		No more marker
    " Traitment:	None
    return ""
  else " found! {{{3
    return s:DoSelect(emo, emc, delete)
  endif
endfunction " }}}2


function! s:DoSelect(emo, emc, delete)
  silent! foldopen!
  if s:Option('marker_center', 1)
    exe "normal! zz" 
  endif
  if s:Select_or_Echo() " select! {{{4
    "OldAndVerby: let select = "v/".Marker_Close()."/e\<cr>"
    let select = 'v'.virtcol('.').'|o'
    if &selection == 'exclusive' | let select = select . 'l' | endif
    let c = col('.')
    " search for the last character of the closing string.
    call search('\V'.substitute(a:emc, '.$', '\\zs\0', ''))
    "Old: let select = 'v!mark_close!'
    "Old: if s:Select_Empty_Mark() || (getline('.')[col('.')]!=mc)
    " let se = '\%'.c.'c\zs'.a:emo.'.\{-}'.a:emc.'\ze'
    " call confirm(matchstr(getline('.'), se). "\n".se, "&Ok", 1 )
    if !a:delete && 
	  \ (s:Select_Empty_Mark() || 
	  \ (matchstr(getline('.'),'\V\%'.c.'c'.a:emo.'\zs\.\{-}\ze'.a:emc)!= ''))
      " Case:		Marker containing a tag, e.g.: «tag»
      " Traitment:	The marker is selected, going into SELECT-mode
      return select."\<c-g>"
    else
      " Case:		Empty marker, i.e. not containing a tag, e.g.: «»
      " Traitment:	The marker is deleted, going into INSERT-mode.
      return select.'"_c'
    endif
  else " Echo! {{{4
    " Case:		g:marker_prefers_select == 0
    " Traitment:	Echo the tag within the marker
    return "a:\<c-v>\"\<esc>h\"my/".Marker_Close() . "/\<cr>" .
	  \ "h@m\<cr>!Cmark_close!"
  endif
endfunction

"Old: " Thanks to this trick, we can silently select with "v/pattern/e<cr>"
"Old: vnoremap <silent> !mark_close! /<c-r>=Marker_Close()<cr>/e<cr>
nnoremap <silent> !Cmark_close! "_c/<c-r>=Marker_Close()<cr>/e<cr>
" ------------------------------------------------------------------------
" Internals         {{{1
" =================
function! s:Option(name, default) " {{{2
  if     exists('b:'.a:name) | return b:{a:name}
  elseif exists('g:'.a:name) | return g:{a:name}
  else                       | return a:default
  endif
endfunction

" Accessors to markers definition:  {{{2
function! s:ICONV(expr, from, to)  " {{{3
  " call Dfunc("s:ICONV(".a:expr.','.a:from.','.a:to.')')
  if has('iconv') || has('iconv/dyn') ||
	\ ((a:from=~'latin1\|utf-8') && (a:to=~'latin1\|utf-8'))
    " call confirm('encoding: '.&enc."\nto:".a:to, "&Ok", 1)
    " call Dret("s:ICONV convert=".iconv(a:expr, a:from, a:to))
    return iconv(a:expr,a:from,a:to)
  else
    " call Dret("s:ICONV  no convert=".a:expr)
    return a:expr
  endif
endfunction

function! s:SetMarker(open, close, ...) " {{{3
  let from = (a:0!=0) ? a:1 : 'latin1'
  " :call Dfunc('s:SetMarker('.a:open.','.a:close.','.from.')')
  
  " let ret = ''
  if '' != a:open
    let b:marker_open  = s:ICONV(a:open, from, &enc)
    " let ret = ret. "  b:open=".b:marker_open
  endif
  if '' != a:close
    let b:marker_close = s:ICONV(a:close, from, &enc)
    " let ret = ret . "  b:close=".b:marker_close
  endif
  " :call Dret("s:SetMarker".ret) 
endfunction
command! -nargs=* SetMarker :call <sid>SetMarker(<f-args>, &enc)

function! Marker_Open()            " {{{3
  " call Dfunc('Marker_Open()')
  if s:Option('use_place_holders', 0) && exists('*IMAP_GetPlaceHolderStart')
    let m = IMAP_GetPlaceHolderStart()
    if "" != m 
      " call Dret('Marker_Open '.m.'  using IMAP placeholder characters')
      return m 
    endif
  endif
  if !exists("b:marker_open") 
    " :call Decho( "b:marker_open is not set")
    " Note: \xab <=> <C-K><<
    call s:SetMarker("\xab", '')
    " :call Decho( "b:last_encoding_used is set to ".&enc)
    let b:last_encoding_used = &enc
  else
    if !exists('s:last_encoding_used')
      " :call Decho( "s:last_encoding_used is not set")
      call s:SetMarker(b:marker_open, b:marker_close, &enc)
      " :call Decho( "b:last_encoding_used is set to ".&enc)
      let b:last_encoding_used = &enc
    elseif &enc != b:last_encoding_used
      call s:SetMarker(b:marker_open, b:marker_close, b:last_encoding_used)
      " :call Decho( "b:last_encoding_used is changed to ".&enc)
      let b:last_encoding_used = &enc
    endif
  endif
  " call Dret('Marker_Open '.b:marker_open)
  return b:marker_open
endfunction

function! Marker_Close()           " {{{3
  if s:Option('use_place_holders', 0) && exists('*IMAP_GetPlaceHolderEnd')
    let m = IMAP_GetPlaceHolderEnd()
    if "" != m 
      " call Dret('Marker_Close '.m.'  using IMAP placeholder characters')
      return m 
    endif
  endif
  if !exists("b:marker_close") 
    " :call Decho( "b:marker_close is not set")
    " Note: \xbb <=> <C-K>>>
    call s:SetMarker('', "\xbb")
    " :call Decho( "b:last_encoding_used is set to ".&enc)
    let b:last_encoding_used = &enc
  else " if exists('s:last_encoding_used')
    if &enc != b:last_encoding_used
      " :call Decho( "b:last_encoding_used is different from current")
      call s:SetMarker(b:marker_open, b:marker_close, b:last_encoding_used)
      " :call Decho( "b:last_encoding_used is changed from ".b:last_encoding_used." to ".&enc)
      let b:last_encoding_used = &enc
    endif
  endif
  return b:marker_close
endfunction

function! Marker_Txt(...)          " {{{3
  return Marker_Open() . ((a:0>0) ? a:1 : '') . Marker_Close()
endfunction

function! LHMoveWithinMarker()     " {{{3
" function! s:MoveWithinMarker()    
  " Purpose: move the cursor within the marker just inserted.
  " Here, b:marker_close exists
  return "\<esc>" . strlen(s:ICONV(Marker_Close(),&enc, 'latin1')) . 'ha'
endfunction

function! LHToggleMarkerInVisual() " {{{3
" function! s:ToggleMarkerInVisual()
  " Purpose: Toggle the marker characters around a visual zone.
  " 1- Check wheither we areselecting a marker
  if line("'<") == line("'>") " I suppose markers don't spread over several lines
    " Extract the selected text
    let a_save = @a
    normal! gv"ay
    let a = @a
    let @a = a_save

    " Check whether the selected text is strictly a marker (and only one)
    if (a =~ '^'.Marker_Txt('.\{-}').'$') 
	  \ && (a !~ '\%(.*'.Marker_Close().'\)\{2}')
      " 2- If so, strip the marker characters
      let a = substitute(a, Marker_Txt('\(.\{-}\)'), '\1', '')
      let unnamed_save=@"
      exe "normal! gvs".a."\<esc>"
      " escape(a, '\') must not be used.
      " exe "normal! gvs".a."\<esc>v".(strlen(a)-1)."ho\<c-g>"
      let @"=unnamed_save
      return 'a'
    endif
  endif

  " 3- Else insert the pair of marker characters around the visual selection
  call InsertAroundVisual(Marker_Open(),Marker_Close(),0,0)
  return '`>'.strlen(Marker_Txt()).'l'
endfunction

" Other options:                    {{{2
" b:usemarks

function! s:Select_or_Echo()    "                  {{{3
  return exists("g:marker_prefers_select") ? g:marker_prefers_select : 1
endfunction

function! s:Select_Empty_Mark() " or delete them ? {{{3
  return exists("g:marker_select_empty_marks") ? g:marker_select_empty_marks : 1
endfunction

" Syntax highlighting of markers   {{{2
function! s:UpdateHighlight()
  silent! syn clear marker
  if s:Option('marker_highlight', 1)
    exe 'syn match marker /'.Marker_Txt('.\{-}').'/ containedin=ALL'
    hi marker guibg=#d0d0d0 ctermbg=lightgray
  endif
endfunction

if s:Option('marker_highlight', 1)
  aug markerHL
    au!
    au BufWinEnter,EncodingChanged * :call s:UpdateHighlight()
  aug END
endif

" Internal mappings {{{1
" =================
" Defines: !mark! and !jump!
" Note: these mappings are the one used by some other (ft)plugin I maintain.

" Set a marker ; contrary to <Plug>!mark!, !mark! doesn't move the cursor
" between the marker characters.
inoremap <silent> !mark! <c-r>=Marker_Txt()<cr>
" vnoremap <silent> !mark! <C-\><C-N>@=<sid>ToggleMarkerInVisual()<cr>
vnoremap <silent> !mark! <C-\><C-N>@=LHToggleMarkerInVisual()<cr>
    nmap <silent> !mark! viw!mark!
"Old: imap !mark! <C-V>«<C-V>»
"Old: vmap !mark! "zc<C-V>«<C-R>z<C-V>»<ESC>

" <C-\><C-N> is like '<ESC>', but without any screen flash. Here, we unselect
" the current selection and go into normal mode.
vnoremap <silent> !jump! <C-\><C-N>@=Marker_Jump(1)<cr>
nnoremap <silent> !jump! @=Marker_Jump(1)<cr>
    imap <silent> !jump! <C-\><C-N>!jump!
vnoremap <silent> !jumpB! <C-\><C-N>`<@=Marker_Jump(0)<cr>
nnoremap <silent> !jumpB! @=Marker_Jump(0)<cr>
    imap <silent> !jumpB! <ESC>!jumpB!
"Old: map !jump! /«.\{-}»/<C-M>a:"<ESC>h"myt»h@m<C-M>cf»

vnoremap <silent> !jump-and-del! <C-\><C-N>@=Marker_Jump(1,1)<cr>
nnoremap <silent> !jump-and-del! @=Marker_Jump(1,1)<cr>
    imap <silent> !jump-and-del! <ESC>!jump-and-del!
vnoremap <silent> !bjump-and-del! <C-\><C-N>@=Marker_Jump(0,1)<cr>
nnoremap <silent> !bjump-and-del! @=Marker_Jump(0,1)<cr>
    imap <silent> !bjump-and-del! <ESC>!bjump-and-del!

" Help stuff        {{{1
" check http://hermitte.free.fr/vim/
if !exists(":VimrcHelp") 
  command! -nargs=1 VimrcHelp 
endif

:VimrcHelp " 
:VimrcHelp " <M-Insert>   : Inserts a marker                                   [I+V]
:VimrcHelp " <M-Del>      : Jumps forward to the next marker                   [I+N+V]
:VimrcHelp " <M-Del>      : Jumps backward to the previous marker              [I+N+V]
" }}}1
" ============================================================================
" Stephen Riehm's Bracketing macros {{{1
" ========== You should not need to change anything below this line ==========
"

"
"	Quoting/bracketting macros
"	Note: The z cut-buffer is used to temporarily store data!
"
"	double quotes
imap !"! <C-V>"<C-V>"!mark!<ESC>F"i
vmap !"! "zc"<C-R>z"<ESC>
"	single quotes
imap !'! <C-V>'<C-V>'!mark!<ESC>F'i
vmap !'! "zc'<C-R>z'<ESC>
"	stars
imap !*! <C-V>*<C-V>*!mark!<ESC>F*i
vmap !*! "zc*<C-R>z*<ESC>
"	braces
imap !(! <C-V>(<C-V>)!mark!<ESC>F)i
vmap !(! "zc(<C-R>z)<ESC>
"	braces - with padding
imap !)! <C-V>(  <C-V>)!mark!<ESC>F i
vmap !)! "zc( <C-R>z )<ESC>
"	underlines
imap !_! <C-V>_<C-V>_!mark!<ESC>F_i
vmap !_! "zc_<C-R>z_<ESC>
"	angle-brackets
imap !<! <C-V><<C-V>>!mark!<ESC>F>i
vmap !<! "zc<<C-R>z><ESC>
"	angle-brackets with padding
imap !>! <C-V><  <C-V>>!mark!<ESC>F i
vmap !>! "zc< <C-R>z ><ESC>
"	square brackets
imap ![! <C-V>[<C-V>]!mark!<ESC>F]i
vmap ![! "zc[<C-R>z]<ESC>
"	square brackets with padding
imap !]! <C-V>[  <C-V>]!mark!<ESC>F i
vmap !]! "zc[ <C-R>z ]<ESC>
"	back-quotes
imap !`! <C-V>`<C-V>`!mark!<ESC>F`i
vmap !`! "zc`<C-R>z`<ESC>
"	curlie brackets
imap !{! <C-V>{<C-V>}!mark!<ESC>F}i
vmap !{! "zc{<C-R>z}<ESC>
"	new block bound by curlie brackets
imap !}! <ESC>o{<C-M>!mark!<ESC>o}!mark!<ESC>^%!jump!
vmap !}! >'<O{<ESC>'>o}<ESC>^
"	spaces :-)
imap !space! .  !mark!<ESC>F.xa
vmap !space! "zc <C-R>z <ESC>
"	Nroff bold
imap !nroffb! \fB\fP!mark!<ESC>F\i
vmap !nroffb! "zc\fB<C-R>z\fP<ESC>
"	Nroff italic
imap !nroffi! \fI\fP!mark!<ESC>F\i
vmap !nroffi! "zc\fI<C-R>z\fP<ESC>

"
" Extended / Combined macros
"	mostly of use to programmers only
"
"	typical function call
imap !();!  <C-V>(<C-V>);!mark!<ESC>F)i
imap !(+);! <C-V>(  <C-V>);!mark!<ESC>F i
"	variables
imap !$! $!{!
vmap !$! "zc${<C-R>z}<ESC>
"	function definition
imap !func! !)!!mark!!jump!!}!!mark!<ESC>kk0!jump!
vmap !func! !}!'<kO!)!!mark!!jump!<ESC>I

"
" Special additions:
"
"	indent mail
vmap !mail! :s/^[^ <TAB>]*$/> &/<C-M>
map  !mail! :%s/^[^ <TAB>]*$/> &/<C-M>
"	comment marked lines
imap !#comment! <ESC>0i# <ESC>A
vmap !#comment! :s/^/# /<C-M>
map  !#comment! :s/^/# /<C-M>j
imap !/comment! <ESC>0i// <ESC>A
vmap !/comment! :s,^,// ,<C-M>
map  !/comment! :s,^,// ,<C-M>j
imap !*comment! <ESC>0i/* <ESC>A<TAB>*/<ESC>F<TAB>i
vmap !*comment! :s,.*,/* &	*/,<C-M>
map  !*comment! :s,.*,/* &	*/,<C-M>j
"	uncomment marked lines (strip first few chars)
"	doesn't work for /* comments */
vmap !stripcomment! :s,^[ <TAB>]*[#>/]\+[ <TAB>]\=,,<C-M>
map  !stripcomment! :s,^[ <TAB>]*[#>/]\+[ <TAB>]\=,,<C-M>j

"
" HTML Macros
" ===========
"
"	turn the current word into a HTML tag pair, ie b -> <b></b>
imap !Htag! <ESC>"zyiwciw<<C-R>z></<C-R>z>!mark!<ESC>F<i
vmap !Htag! "zc<!mark!><C-R>z</!mark!><ESC>`<!jump!
"
"	set up a HREF
imap !Href! <a href="!mark!">!mark!</a>!mark!<ESC>`[!jump!
vmap !Href! "zc<a href="!mark!"><C-R>z</a>!mark!<ESC>`<!jump!
"
"	set up a HREF name (tag)
imap !Hname! <a name="!mark!">!mark!</a>!mark!<ESC>`[!jump!
vmap !Hname! "zc<a name="!mark!"><C-R>z</a>!mark!<ESC>`<!jump!

" }}}1
" ======================================================================
let &cpo = s:cpo_save
" vim600: set fdm=marker:
plugin/common_brackets.vim	[[[1
653
"===========================================================================
" $Id: common_brackets.vim 26 2008-02-15 00:07:34Z luc.hermitte $
" File:		common_brackets.vim
" Author:	Luc Hermitte <MAIL:hermitte {at} free {dot} fr>
" 		<URL:http://hermitte.free.fr/vim/>
" Last Update:	$Date: 2008-02-15 01:07:34 +0100 (ven., 15 fÃ©vr. 2008) $
" Version:	0.6.0
" Purpose:      {{{1
" 		This file defines a function (Brackets) that brings
" 		together several macros dedicated to insert pairs of
" 		caracters when the first one is typed. Typical examples are
" 		the parenthesis, brackets, <,>, etc. 
" 		One can choose the macro he wants to activate thanks to the
" 		buffer-relative options listed below.
"
" 		This function is used by different ftplugins: <vim_set.vim>,
" 		<ML_set.vim>, <html_set.vim>, <php_set.vim> and <tex_set.vim>
" 		-- available on my VIM web site.
"
" 		BTW, they can be activated or desactivated by pressing <F9>
" 		Rem.: exe "noremap" is not yet supported by Triggers.vim
" 		Hence the trick with the intermediary functions.
"
" History:      {{{1
" Version 0.6.0:
" 		* UTF-8 bug fix in Brkt_lt(), Brkt_gt(), Brkt_Dquote()
" 		* New numerotation used in versionning
" 		* Project Added in SVN
" Version 0.5.4
" 		* b:cb_bracket == 2 replaces the previous behavior (==1)
"                 b:cb_bracket == 1, maps <localleader>[ in normal- and
"                 visual-modes, which does not mess anymore with vim default
"                 bindings of [a, [c, [[, [(, ...
" Version 0.5.3:
"		* Brackets manipulations support angle brackets
" Version 0.5.2:
"		* Triggers.vim can be installed into {rtp}/macros
" Version 0.5.1:
"		* Fix a small bug when editing vimL files.
" Version 0.5.0:
"		* Compatible with Srinath Avadhanula's imaps.vim
"               * Vim buffers: smarter keybindings for \(, \%( and ( 
"                 (requires imaps.vim)
"               * Visual-mode mappings for the brackets do not surround markers
"                 (/placeholders) anymore, now they are discarded
" Version 0.4.1:
"		* Uses InsertAroundVisual() in order to work even when
"                 'selection' is set to exclusive.
" Version 0.4.0:
"		* New option: b:cb_jump_on_close that specify weither the
"                 mappings for the closing brackets are defined or not
"                 default: true (1)
" Version 0.3.9:
"		* Updated to match changes within bracketing.base.vim
" 		 -> ¡xxx! mappings changed to !xxx!
" 		 [encodings issue]
" Version 0.3.8:
"		* Updated to match changes within bracketing.base.vim
" 		* Markers-mappings moved back to bracketing.base.vim
" Version 0.3.7:
"		* Brackets manipulation mappings for normal mode can be changed
" 		  They are now <Plug> mappings.
" 		  Same enhancement for mappings to ¡mark! and ¡jump!
" Version 0.3.6c:
"		* Change every 'normal' to 'normal!'
" Version 0.3.6b:
"		* address obfuscated for spammers
" Version 0.3.6:
"		* accept default value for b:usemarks
" Version 0.3.5:
"		* add continuation lines support ; cf 'cpoptions'
" Version 0.3.4:
"		* Works correctly when editing several files (like with 
" 		"vim foo1.x foo2.x").
" 		* ')' and '}' don't search for the end of the bracket when we
" 		are within a comment.
" Version 0.3.3:
"		* Add support for \{, \(, \[, \<
"               * Plus some functions to change the type of brackets and
"               toggle backslashes before brackets.
"               Inspired from AucTeX.vim.
" Version 0.3.2:
"		* Bugs fixed with {
" Version 0.3.1:
"		* Triggers.vim and help.vim used, but not required.
" Version 0.3.0:
"		* Pure VIM6
" Version 0.2.1a:
"		* Some little change with the requirements
" Version 0.2.1:
"		* Use b:usemarks in the mapping of curly-brackets
" Version 0.2.0:
"		* Lately, I've discovered (SR) Stephen Riehm's bracketing
" 		macros and felt in love with the markers feature. So, here is
" 		the ver 2.x based on his package.
" 		I still bring an original feature : a centralized way to
" 		customize these pairs regarding options specified within
" 		the ftplugins.
" 		Note that I planned to use this file with my customized
" 		version of Stephan Riehm's file.
" 
" Options:      {{{1
" 	(*) b:cb_bracket			: [ -> [ & ]
"	(*) b:cb_cmp				: < -> < & >
"	    could be customized thanks to b:cb_ltFn and b:cb_gtFn [ML_set.vim]
"	(*) b:cb_acco				: { -> { & }
"	(*) b:cb_parent				: ( -> ( & )
"	(*) b:cb_mathMode			: $ -> $ & $	[tex_set.vim]
"	    type $$ in visual/normal mode
"	(*) b:cb_quotes				: ' -> ' & '
"		== 2  => non active within comment or strings
"	(*) b:cb_Dquotes			: " -> " & "
"	    could be customized thanks to b:cb_DqFn ;	[vim_set.vim]
"		== 2  => non active within comment or strings
"	(*) b:usemarks				: 
"		indicates the wish to use the marking feature first defined by
"		Stephan Riehm.
"	(*) b:cb_jump_on_close			: ), ], }
"	        == 0  => no mappings for ), ] and }
"	        == 1  => mappings for ), ] and } (default)
"
" Dependancies: {{{1
" 	Triggers.vim		(Not required)
" 	misc_map.vim		(required)
" 	bracketing.base.vim	(required)
" 	help.vim for vimrc_core.vim (:VimrcHelp)     (recognized and used.)
"
" Todo:         {{{1
" 	(*) Option b:cb_double that defines weither we must hit '(' or '(('
" 	(*) Support '\%(\)' for vim when imaps.vim is not installed
" 	(*) Support '||', '\|\|' and '&&' (within eqnarray[*]) for LaTeX.
"	(*) Systematically use b:usemarks for opening and closing
" }}}1
"===========================================================================
"
"======================================================================
" line continuation used here ??
let s:cpo_save = &cpo
set cpo&vim

" Make sure imaps.vim, if installed, is loaded before this plugin
if !exists("*IMAP")
  runtime plugin/imaps.vim
endif
" ------------------------------------------------------------------
" The main function that defines all the key-bindings. " {{{
function! Brackets()
  " Code to toggle brackets-mappings if imaps.vim is installed {{{
  " This permits to toogle the brackets-mappings when imap.vim is present on
  " the system
  if exists('*IMAP')
    TRIGGER "let g:Imap_FreezeImap=0", "let g:Imap_FreezeImap=1" 
  endif
  " imaps.vim special }}}
  "
  " [ & ] {{{
  if exists('b:cb_bracket') && b:cb_bracket
    if exists('*IMAP')
      if &ft == 'tex'
	call IMAP('\[', "\<C-R>=Insert_sqbracket(1)\<cr>", &ft)
      endif
      call IMAP('[', "\<C-R>=Insert_sqbracket(0)\<cr>", &ft)
    else
      inoremap <buffer> [ <C-R>=<sid>EscapableBrackets('[','\<C-V\>[','\<C-V\>]')<cr>
    endif
    if     1 == b:cb_bracket
      vnoremap <buffer> <localleader>[ <c-\><c-n>@=Surround('[', ']', 0, 0, '%', 0)<cr>
	  nmap <buffer> <localleader>[ viw<localleader>[
	  nmap <buffer> <M-[> viw<localleader>[
    elseif 2 == b:cb_bracket
      vnoremap <buffer> [ <c-\><c-n>@=Surround('[', ']', 0, 0, '%', 0)<cr>
	  nmap <buffer> [ viw[
	  nmap <buffer> <M-[> viw[
    endif
	imap <buffer> <M-[> <esc><M-[>a
  endif
  " [ & ] }}}
  "
  " < & > {{{
  if exists('b:cb_cmp') && b:cb_cmp
    if exists('*IMAP')
      call IMAP('<', "\<c-r>=Brkt_lt()\<cr>", &ft)
      " if !exists('b:cb_ltFn') || 0==b:cb_ltFn
	" call IMAP('\<', "\<c-r>=Insert_lt_gt(1)\<cr>", &ft)
      " endif
      call IMAP('>', "\<c-r>=Brkt_gt()\<cr>", &ft)
    else
      imap <buffer> < <c-r>=Brkt_lt()<cr>
      imap <buffer> > <c-r>=Brkt_gt()<cr>
    endif
    vnoremap <buffer> < <c-\><c-n>@=Surround('<', '>', 0, 0, '`>ll', 0)<CR>
        "nmap <buffer> < viw<
	nmap <buffer> <M-<> viw<
	imap <buffer> <M-<> <esc><M-<>a
  endif
  " < & > }}}
  "
  " { & } {{{
  if exists('b:cb_acco') && b:cb_acco
    if exists('*IMAP')
      if &ft == 'tex'
	call IMAP('{', "\<C-R>=Insert_clbracket(0,0)\<cr>", &ft)
	" Required or not ?
	" call IMAP('\{', "\<C-R>=Insert_clbracket(1,0)\<cr>", &ft)
      else
	call IMAP('{', "\<C-R>=Insert_clbracket(0,1)\<cr>", &ft)
	call IMAP('#{', "\<C-R>=Insert_clbracket(0,0)\<cr>", &ft)
      endif
    else
      if &syntax == "tex"
	inoremap <buffer> { <C-R>=<sid>EscapableBrackets('{','{','}')<cr>
      else
	" inoremap <buffer> { <C-R>=<sid>EscapableBracketsLn('{','{','}')<cr>
	inoremap <buffer>  { <C-R>=Smart_insert_seq1( '{','{\<cr\>}\<esc\>O','{\<cr\>}!mark!\<esc\>O')<cr>
	inoremap <buffer> #{ <C-R>=Smart_insert_seq1('#{','{}\<esc\>i','{}!mark!\<esc\>F{a')<cr>
      endif
    endif
    vnoremap <buffer> { <c-\><c-n>@=Surround('{', '}', 0, 0, '%', 0)<cr>
        nmap <buffer> { viw{
    if !exists('b:cb_jump_on_close') || b:cb_jump_on_close
      nnoremap <buffer> } :call search('}\\|\.\\|&\\|]\\|\$')<CR>a
      ""inoremap <buffer> } <c-r>=MapNoContext('}',BuildMapSeq('!find}!'))<cr>
      " Next line does not work well (vim 6.1.362)
      inoremap <buffer> } <C-R>=MapNoContext('}', '\<c-o\>}\<left\>')<CR>
    endif
  endif
  " { & } }}}
  "
  " ( & ) {{{
  if exists('b:cb_parent') && b:cb_parent
    if exists('*IMAP')
      if &ft == 'vim'
	" context à bloquer comments only
	call IMAP('\(', "\<C-R>=Insert_rdbracket(1)\<cr>", &ft)
	call IMAP('\%(', "\<C-R>=Insert_rdbracket(2)\<cr>", &ft)
      elseif &ft == 'tex'
	call IMAP('\(', "\<C-R>=Insert_rdbracket(1)\<cr>", &ft)
      endif
      call IMAP('(', "\<C-R>=Insert_rdbracket(0)\<cr>", &ft)
    else
      inoremap <buffer> ( <C-R>=<sid>EscapableBrackets('(','(',')')<cr>
    endif
    if !exists('b:cb_jump_on_close') || b:cb_jump_on_close
      noremap <buffer> ) :call search(')')<cr>a
	 imap <buffer> ) <C-R>=MapNoContext(')', '\<c-o\>/)/e+1/\<cr\>')<CR>
	 " inoremap <buffer> ) <C-R>=MapNoContext(')', '\<esc\>:call search(")")\<cr\>a')<CR>
    endif
    vnoremap <buffer> (        <c-\><c-n>@=Surround('(', ')', 0, 0, '%', 0)<cr>
    nnoremap <buffer> (     viw<c-\><c-n>@=Surround('(', ')', 0, 0, '%', 0)<cr>
    nnoremap <buffer> <M-(> viw<c-\><c-n>@=Surround('(', ')', 0, 0, '%', 0)<cr>
        imap <buffer> <M-(>    <esc><M-(>a
  endif
  " ( & ) }}}

  " $ & $ {{{
  if exists('b:cb_mathMode') && b:cb_mathMode
    if exists('*IMAP')
      call IMAP( '$', "\<c-r>=Insert_LaTeX_TwoDollars()\<cr>", &ft)
      call IMAP( '\$', '\$', &ft)
    else
      inoremap <buffer> $ <c-r>=Insert_LaTeX_Dollar()<cr>
    endif
    vnoremap <buffer> $$ <c-\><c-n>@=Surround('$', '$', 0, 0, '`>ll', 0)<cr>
	nmap <buffer> $$ viw$$
	nmap <buffer> <M-$> viw$$
	imap <buffer> <M-$> <esc><M-$>
  endif
  " $ & $ }}}
  "
  " quotes {{{
  if exists('b:cb_quotes') && b:cb_quotes
    inoremap <buffer> ' <c-r>=Brkt_quote()<cr>
    vnoremap <buffer> '' <c-\><c-n>@=Surround("'", "'", 0, 0, '`>ll', 0)<cr>
        nmap <buffer> ''    viw''
	nmap <buffer> <M-'> viw''
	" add quotes around the word under the cursor
	imap <buffer> <M-'> <esc><M-'>a
  endif
  " quotes }}}
  "
  " double-quotes {{{
  if exists('b:cb_Dquotes') && b:cb_Dquotes
    inoremap <buffer> " <c-r>=Brkt_Dquote()<cr>
    vnoremap <buffer> "" <c-\><c-n>@=Surround('"', '"', 0, 0, '`>ll', 0)<cr>
	nmap <buffer> ""    viw""
	nmap <buffer> <M-"> viw""
	" add dquotes around the word under the cursor
	imap <buffer> <M-"> <esc><M-">a
  endif
  " double-quotes }}}
endfunction " }}}

if !exists('b:usemarks') | let b:usemarks=1 | endif

" Defines a command and the mode switching mappings (with <F9>) {{{
if !exists("*Trigger_Function")
  runtime plugin/Triggers.vim macros/Triggers.vim
endif
if exists("*Trigger_Function")
  au Bufenter * :call <SID>LoadBrackets()
  let s:scriptname = expand("<sfile>:p")

  function! s:LoadBrackets()
    if !exists('b:usemarks') | let b:usemarks=1 | endif
    if exists("b:loaded_common_bracket_buff") | return | endif
    let b:loaded_common_bracket_buff = 1
    silent call Trigger_Function('<F9>', 'Brackets', s:scriptname,1,1)
    imap <buffer> <F9> <SPACE><ESC><F9>a<BS>
    silent call Trigger_DoSwitch('<M-F9>',
	  \ ':let b:usemarks='.b:usemarks,':let b:usemarks='.(1-b:usemarks),1,1)
    imap <buffer> <M-F9> <SPACE><ESC><M-F9>a<BS>
  endfunction
endif
" }}}
"======================================================================
" Global definitions : functions & mappings
if exists("g:loaded_common_brackets") 
      \ && !exists('g:force_reload_common_brackets')
  let &cpo = s:cpo_save
  finish 
endif
let g:loaded_common_brackets = 1

" ===========================================================================
" Tool functions {{{

" s:EscapableBrackets, and s:EscapableBracketsLn are two different functions
" in order to acheive a little optimisation
function! s:EscapableBrackets(key, left, right) " {{{
  let r = ((getline('.')[col('.')-2] == '\') ? '\\\\' : "") . a:right
  let expr1 = a:left.r.'\<esc\>i'
  let expr2 = a:left.r.'!mark!\<esc\>F'.a:key.'a'
  if exists('b:usemarks') && b:usemarks
    return "\<c-r>=MapNoContext('".a:key."',BuildMapSeq('".expr2."'))\<cr>"
  else
    return "\<c-r>=MapNoContext('".a:key."', '".expr1."')\<cr>"
  endif
endfunction " }}}

function! s:EscapableBracketsLn(key, left, right) " {{{
  let r = ((getline('.')[col('.')-2] == '\') ? '\\\\' : "") . a:right
  let expr1 = a:left.'\<cr\>'.r.'\<esc\>O'
  let expr2 = a:left.'\<cr\>'.r.'!mark!\<esc\>O'
  if exists('b:usemarks') && b:usemarks
    return "\<c-r>=MapNoContext('".a:key."',BuildMapSeq('".expr2."'))\<cr>"
  else
    return "\<c-r>=MapNoContext('".a:key."', '".expr1."')\<cr>"
  endif
endfunction " }}}

" Tool functions }}}
" ===========================================================================
" The core functions for the previous mappings {{{
" If a backslash precede the current cursor position, insert one dollar,
" and two otherwise.
function! Insert_LaTeX_TwoDollars() " {{{
  " return "\<c-v>$\<c-v>$\<c-r>=Marker_Txt()\<cr>\<esc>F$i"
  return IMAP_PutTextWithMovement(Smart_insert_seq2('$', '$<++>$!mark!'))
endfunction " }}}
function! Insert_LaTeX_Dollar() " {{{
  if getline('.')[col('.')-2] == '\'
    return '$'
  else
    return "\<c-v>$\<c-v>$\<c-r>=Marker_Txt()\<cr>\<esc>F$i"
  endif
endfunction " }}}

" Insert the various kind of brackets {{{
function! Insert_rdbracket(esc)
  return s:Insert_bracket('(', ')', a:esc, 0)
endfunction

function! Insert_sqbracket(esc)
  return s:Insert_bracket('[', ']', a:esc, 0)
endfunction

function! Insert_clbracket(esc, nl)
  return s:Insert_bracket('{', '}', a:esc, a:nl)
endfunction

function! Insert_lt_gt(esc)
  return s:Insert_bracket('<', '>', a:esc, 0)
endfunction
" }}}

" Function: s:Insert_bracket(obrkt, cbrkt, esc, nl)  {{{
" Internal function.
" {obrkt}:	open bracket
" {cbrkt}:	close bracket
" {esc}:	escaped version 0:none, 1:\, 2:\%
" {nm}:		new line between {obrkt} and {cbrkt}
function! s:Insert_bracket(obrkt, cbrkt, esc, nl) 
  " Generic function used by the others
  if     a:esc == 0 | let open = ''   | let close = ''
  elseif a:esc == 1 | let open = '\'  | let close = '\'
  elseif a:esc == 2 | let open = '\%' | let close = '\'
  else
    echoerr "Case not handled (yet)!"
  endif
  let key = open . a:obrkt
  let middle = a:nl ? "\<cr><++>\<cr>" : '<++>'
  let expr = key . middle . close . a:cbrkt .'!mark!'
  if &ft == "vim" && a:esc " expand only within strings
    return IMAP_PutTextWithMovement(Smart_insert_seq2(key,expr, 'string\|PatSep'))
  else
    return IMAP_PutTextWithMovement(Smart_insert_seq2(key,expr))
  endif
endfunction "}}}

" Calls a custom function or returns <> regarding the options
function! Brkt_lt() " {{{
  if exists('b:cb_ltFn')
    return "\<C-R>=InsertSeq('<'," . b:cb_ltFn . ")\<CR>"
  else
    if exists('*IMAP')
      return Insert_lt_gt(0)
    else
      " Is it even useful ?
      return <SID>EscapableBrackets('<', '\<C-V\><', '\<C-V\>>')
    endif
  endif
endfunction " }}}

" Calls a custom function, or search for the next '>', or return '>'
" regarding the options.
function! Brkt_gt() " {{{
  if exists('b:cb_gtFn')        | return "\<C-R>=InsertSeq('>', " . b:cb_gtFn . ")\<CR>"
                                " return "\<C-R>=" . b:cb_gtFn . "\<CR>"
  elseif exists('b:cb_gtFind')  | return "\<esc>/>/\<cr>a"
  else                          | return ">"
  endif
endfunction " }}}

" Centralize all the INSERT-mode mappings associated to quotes
function! Brkt_quote() " {{{
  if b:cb_quotes == 2
    if exists("b:usemarks") && b:usemarks == 1
      return "\<c-r>=MapNoContext(\"'\", " .
	\    "\"''\\<C-R\>=Marker_Txt()\\<CR\>\\<esc\>F'i\")\<cr>"
    else 
      return "\<c-r>=MapNoContext(\"'\", \"''\\<Left\>\")\<cr>"
    endif
  else
    if exists("b:usemarks") && b:usemarks == 1
      return "\<C-V>'\<C-V>'\<c-r>=Marker_Txt()\<cr>\<ESC>F'i"
    else 
      return "''\<left>"
    endif
  endif
endfunction " }}}

" Centralize all the INSERT-mode mappings associated to double-quotes
function! Brkt_Dquote() " {{{
  if b:cb_Dquotes == 2
    if exists("b:usemarks") && b:usemarks == 1
      return "\<c-r>=MapNoContext('\"', '" . '\"\"' . "'." . 
       \ '"\\<C-R\>=Marker_Txt()\\<CR\>\\<esc\>F\\\"i")' . "\<cr>"
    else 
      return "\<c-r>=MapNoContext('\"', '" . 
	\    '\"\"' . "'." . '"\\<Left\>")' . "\<cr>"
    endif
  else
    if exists('b:cb_DqFn')
      " TEST: OK sans imaps.vim
      return "\<C-R>=InsertSeq('\"', escape(" . b:cb_DqFn . ", '\'))\<CR>"
    elseif exists("b:usemarks") && b:usemarks == 1
      return "\<C-V>\"\<C-V>\"\<c-r>=Marker_Txt()\<cr>\<ESC>F\"i"
    else 
      return "\"\"\<left>"
    endif
  endif
endfunction " }}}

" The core functions for the previous mappings }}}
"======================================================================

"======================================================================
" Matching Brackets Macros, From AuCTeX.vim (due to Saul Lubkin).   {{{
" Except, that I use differently the chanching-brackets functions.
" For normal mode.

" Bindings for the Bracket Macros {{{
if !exists('g:cb_want_mode ') | let g:cb_want_mode = 1 | endif
if g:cb_want_mode " {{{
  if !hasmapto('BracketsManipMode')
    noremap <silent> <M-b>	:call BracketsManipMode("\<M-b>")<cr>
  endif
  " }}}
else " {{{
  if !hasmapto('<Plug>DeleteBrackets')
    map <M-b>x		<Plug>DeleteBrackets
    map <M-b><Del>	<Plug>DeleteBrackets
  endif
  noremap <silent> <Plug>DeleteBrackets	:call <SID>DeleteBrackets()<CR>

  if !hasmapto('<Plug>ChangeToRoundBrackets')
    map <M-b>(		<Plug>ChangeToRoundBrackets
  endif
  noremap <silent> <Plug>ChangeToRoundBrackets	:call <SID>ChangeRound()<CR>

  if !hasmapto('<Plug>ChangeToSquareBrackets')
    map <M-b>[		<Plug>ChangeToSquareBrackets
  endif
  noremap <silent> <Plug>ChangeToSquareBrackets	:call <SID>ChangeSquare()<CR>

  if !hasmapto('<Plug>ChangeToCurlyBrackets')
    map <M-b>{		<Plug>ChangeToCurlyBrackets
  endif
  noremap <silent> <Plug>ChangeToCurlyBrackets	:call <SID>ChangeCurly()<CR>

  if !hasmapto('<Plug>ChangeToAngleBrackets')
    map <M-b>{		<Plug>ChangeToAngleBrackets
  endif
  noremap <silent> <Plug>ChangeToAngleBrackets	:call <SID>ChangeAngle()<CR>

  if !hasmapto('<Plug>ToggleBackslash')
    map <M-b>\		<Plug>ToggleBackslash
  endif
  noremap <silent> <Plug>ToggleBackslash	:call <SID>ToggleBackslash()<CR>
endif " }}}
" Bindings for the Bracket Macros }}}

"inoremap <C-Del> :call <SID>DeleteBrackets()<CR>
"inoremap <C-BS> <Left><C-O>:call <SID>DeleteBrackets()<CR>

" Then the procedures. {{{
function! s:DeleteBrackets() " {{{
  let s:b = getline(line("."))[col(".") - 2]
  let s:c = getline(line("."))[col(".") - 1]
  if s:b == '\' && (s:c == '{' || s:c == '}')
    normal! X%X%
  endif
  if s:c == '{' || s:c == '[' || s:c == '('
    normal! %x``x
  elseif s:c == '}' || s:c == ']' || s:c == ')'
    normal! %%x``x``
  endif
endfunction " }}}

function! s:ChangeCurly() " {{{
  let s_matchpairs = &matchpairs
  set matchpairs+=<:>,(:),{:},[:]
  let s:c = getline(line("."))[col(".") - 1]
  if s:c =~ '[\|(\|<'     | normal! %r}``r{
  elseif s:c =~ ']\|)\|>' | normal! %%r}``r{%
  endif
  let &matchpairs = s_matchpairs
endfunction " }}}

function! s:ChangeSquare() " {{{ " {{{
  let s_matchpairs = &matchpairs
  set matchpairs+=<:>,(:),{:},[:]
  let s:c = getline(line("."))[col(".") - 1]
  if s:c =~ '[(<{]'     | normal! %r]``r[
  elseif s:c =~ '[)}>]' | normal! %%r]``r[%
  endif
  let &matchpairs = s_matchpairs
endfunction " }}} " }}}

function! s:ChangeAngle() " {{{ " {{{
  let s_matchpairs = &matchpairs
  set matchpairs+=<:>,(:),{:},[:]
  let s:c = getline(line("."))[col(".") - 1]
  if s:c =~ '[[({]'       | normal! %r>``r<
  elseif s:c =~ ')\|}\|]' | normal! %%r>``r<``
  endif
  let &matchpairs = s_matchpairs
endfunction " }}} " }}}

function! s:ChangeRound() " {{{
  let s_matchpairs = &matchpairs
  set matchpairs+=<:>,(:),{:},[:]
  let s:c = getline(line("."))[col(".") - 1]
  if s:c =~ '[\|{\|<'     | normal! %r)``r(
  elseif s:c =~ ']\|}\|>' | normal! %%r)``r(%
  endif
  let &matchpairs = s_matchpairs
endfunction " }}}

function! s:ToggleBackslash() " {{{
  let s:b = getline(line("."))[col(".") - 2]
  let s:c = getline(line("."))[col(".") - 1]
  if s:b == '\'
    if s:c =~ '(\|{\|['     | normal! %X``X
    elseif s:c =~ ')\|}\|]' | normal! %%X``X%
    endif
  else
    if s:c =~ '(\|{\|['     | exe "normal! %i\\\<esc>``i\\\<esc>"
    elseif s:c =~ ')\|}\|]' | exe "normal! %%i\\\<esc>``i\\\<esc>%"
    endif
  endif
endfunction " }}}
 
function! BracketsManipMode(starting_key) " {{{
  redraw! " clear the msg line
  while 1
    echohl StatusLineNC
    echo "\r-- brackets manipulation mode (/x/(/[/{/</\\/<F1>/q/)"
    echohl None
    let key = getchar()
    let bracketsManip=nr2char(key)
    if (-1 != stridx("x([{<\\q",bracketsManip)) || 
	  \ (key =~ "\\(\<F1>\\|\<Del>\\)")
      if     bracketsManip == "x"      || key == "\<Del>" 
	call s:DeleteBrackets() | redraw! | return ''
      elseif bracketsManip == "("      | call s:ChangeRound()
      elseif bracketsManip == "["      | call s:ChangeSquare()
      elseif bracketsManip == "{"      | call s:ChangeCurly()
      elseif bracketsManip == "<"      | call s:ChangeAngle()
      elseif bracketsManip == "\\"     | call s:ToggleBackslash()
      elseif key == "\<F1>"
	redraw! " clear the msg line
	echo "\r *x* -- delete the current brackets pair\n"
	echo " *(* -- change the current brackets pair to round brackets ()\n"
	echo " *[* -- change the current brackets pair to square brackets []\n"
	echo " *{* -- change the current brackets pair to curly brackets {}\n"
	echo " *<* -- change the current brackets pair to angle brackets <>\n"
	echo " *\\* -- toggle a backslash before the current brackets pair\n"
	echo " *q* -- quit the mode\n"
	continue
      elseif bracketsManip == "q"
	redraw! " clear the msg line
	return ''
      " else
      endif
      redraw! " clear the msg line
    else
      redraw! " clear the msg line
      return a:starting_key.bracketsManip
    endif
  endwhile
endfunction " }}}
" Then the procedures. }}}

" Matching Brackets Macros, From AuCTeX.vim (due to Saul Lubkin).   }}}
" ===========================================================================
  let &cpo = s:cpo_save
" ===========================================================================
" Implementation and other remarks : {{{
" (*) Whitin the vnoremaps, `>ll at the end put the cursor at the
"     previously last character of the selected area and slide left twice
"     (ll) to compensate the addition of the sourrounding characters.
" (*) The <M-xxx> key-binding used in insert mode apply on the word
"     currently under the cursor. There also exist the normal mode version
"     of these macros.
"     Unfortunately several of these are not accessible from the french
"     keyboard layout -> <M-{>, <M-[>, <M-`>, etc
" (*) nmap <buffer> " ... is a very bad idea, hence nmap ""
" (*) !mark! and !jump! can't be called yet from MapNoContext().
"     but <c-r>=Marker_Txt()<cr> can.
" }}}
" ===========================================================================
" vim600: set fdm=marker:
plugin/misc_map.vim	[[[1
586
"===========================================================================
" $Id: misc_map.vim 26 2008-02-15 00:07:34Z luc.hermitte $
" File:		misc_map.vim
" Author:	Luc Hermitte <MAIL:hermitte {at} free {dot} fr>
" 		<URL:http://hermitte.free.fr/vim/>
" Last Update:	$Date: 2008-02-15 01:07:34 +0100 (ven., 15 fÃ©vr. 2008) $
" Version:	0.6.0
"
" Purpose:	API plugin: Several mapping-oriented functions
"
" Todo:		Use the version of EatChar() from Benji Fisher's foo.vim
"---------------------------------------------------------------------------
" Function:	MapNoContext( key, sequence)				{{{
" Purpose:	Regarding the context of the current position of the
" 		cursor, it returns either the value of key or the
" 		interpreted value of sequence.
" Parameters:	<key> - returned while whithin comments, strings or characters 
" 		<sequence> - returned otherwise. In order to enable the
" 			interpretation of escaped caracters, <sequence>
" 			must be a double-quoted string. A backslash must be
" 			inserted before every '<' and '>' sign. Actually,
" 			the '<' after the second one (included) must be
" 			backslashed twice.
" Example:	A mapping of 'if' for C programmation :
"   inoremap if<space> <C-R>=MapNoContext("if ",
"   \				'\<c-f\>if () {\<cr\>}\<esc\>?)\<cr\>i')<CR>
" }}}
"---------------------------------------------------------------------------
" Function:	MapNoContext2( key, sequence)				{{{
" Purpose:	Exactly the same purpose than MapNoContext(). There is a
"		slight difference, the previous function is really boring
"		when we want to use variables like 'tarif' in the code.
"		So this function also returns <key> when the character
"		before the current cursor position is not a keyword
"		character ('h: iskeyword' for more info). 
" Hint:		Use MapNoContext2() for mapping keywords like 'if', etc.
"		and MapNoContext() for other mappings like parenthesis,
"		punctuations signs, and so on.
" }}}
"---------------------------------------------------------------------------
" Function:	MapContext( key, syn1, seq1, ...[, default-seq])	{{{
" Purpose:	Exactly the same purpose than MapNoContext(), but more precise.
"               Returns:
"               - {key} within string, character or comment context,
"               - interpreted {seq_i} within {syn_i} context,
"               - interpreted {default-seq} otherwise ; default value: {key}
" }}}
"---------------------------------------------------------------------------
" Function:	Map4TheseContexts( key, syn1, seq1, ...[, default-seq])	{{{
" Purpose:	Exactly the same purpose than MapContext(), but even more
"               precise. It does not make any assumption for strings-,
"               comments-, characters- and doxygen-context.
"               Returns:
"               - interpreted {seq_i} within {syn_i} context,
"               - interpreted {default-seq} otherwise ; default value: {key}
" }}}
"---------------------------------------------------------------------------
" Function:	BuildMapSeq( sequence )					{{{
" Purpose:	This fonction is to be used to generate the sequences used
" 		by the Â«MapNoContextÂ» functions. It considers that every
" 		Â«!.\{-}!Â» pattern is associated to an INSERT-mode mapping and
" 		expands it.
" 		It is used to define marked mappings ; cf <c_set.vim>
" }}}
"---------------------------------------------------------------------------
" Function:	InsertAroundVisual(begin,end,isLine,isIndented) range {{{
" Old Name:	MapAroundVisualLines(begin,end,isLine,isIndented) range 
" Purpose:	Ease the definition of visual mappings that add text
" 		around the selected one.
" Examples:
"   (*) LaTeX-like stuff
"       if &ft=="tex"
"         vnoremap ;; :call InsertAroundVisual(
"		      \ '\begin{center}','\end{center}',1,1)<cr>
"   (*) C like stuff
"       elseif &ft=="c" || &ft=="cpp"
"         vnoremap ;; :call InsertAroundVisual('else {','}',1,1)<cr>
"   (*) VIM-like stuff
"       elseif &ft=="vim" 
"         vnoremap ;; :call InsertAroundVisual('if','endif',1,1)<cr>
"       endif

" Fixed Problem: 
" * if a word from 'begin' or 'end' is used as a terminaison of an
" abbreviation, this function yields to an incorrect behaviour. 
" Problems: 
" * Smartindent is not properly managed. [Vim 5.xx]
" Todo:
" * Add a positionning feature -> ?{<cr>a
"   => Use Surround()
" }}}
"---------------------------------------------------------------------------
" Function:	ReinterpretEscapedChar(sequence)  {{{
" Purpose:	This function transform '\<cr\>', '\<esc\>', ... '\<{keys}\>'
" 		into the interpreted sequences "\<cr>", "\<esc>", ...
" 		"\<{keys}>".
" 		It is meant to be used by fonctions like MapNoContext(),
" 		InsertSeq(), ... as we can not define mappings (/abbreviations)
" 		that contain "\<{keys}>" into the sequence to insert.
" Note:		It accepts sequences containing double-quotes.
" }}}
"---------------------------------------------------------------------------
" Function: 	InsertSeq(key, sequence, [context]) {{{
" Purpose:	This function is meant to return the {sequence} to insert when
" 		the {key} is typed. The result will be function of several
" 		things:
" 		- the {sequence} will be interpreted:
" 		  - special characters can be used: '\<cr\>', '\<esc\>', ...
" 		    (see ReinterpretEscapedChar()) ; '\n'
" 		  - we can embed insert-mode mappings whose keybindings match
" 		    '!.\{-}!' (see BuildMapSeq())
" 		    A special treatment is applied on:
" 		    - !mark! : according to b:usemarks, it is replaced by
" 		      Marker_Txt() or nothing
" 		    - !cursorhere! : will move the cursor to that position in
" 		      the sequence once it have been expanded.
" 		- the context ; by default, it returns the interpreted sequence
" 		  when we are not within string, character or comment context.
" 		  (see MapNoContext())
" 		  Thanks to the optional parameter {context}, we can ask to
" 		  expand and interpret the {sequence} only within some
" 		  particular {context}.
"
" Examples:
"  (*) Excerpt for my vim-ftplugin
"    inoremap  <buffer> <silent> <M-c> 
"     \ <c-r>=InsertSeq('<m-c>', ':call !cursorhere!(!mark!)!mark!')<cr>
"    inoreab  <buffer> <silent>  fun      
"     \ <C-R>=InsertSeq('fun', 
"     \ 'function!!cursorhere!(!mark!)\n!mark!\nendfunction!mark!')<CR>
" }}}
"---------------------------------------------------------------------------
" Function:	Surround(begin,end,isLine,isIndented,goback,mustInterpret [, im_seq]) range {{{
" Purpose:	This function is a smart wrapper around InsertAroundVisual().
" 		It permit to interpret {begin} and {end} and it also recognizes
" 		whether what we must surround is a marker or not.
"
" 		The point is that there is no :smap command in VimL, and that
" 		insert-mode mappings (imm) should have the precedence over
" 		visual-mode mappings (vmm) when we deals with selected markers
" 		(select-mode) ; unfortunatelly, it is the contrary: vim gives
" 		the priority to vmm over imm.
"
" Parameters:
" {begin}, {end}	strings
" 	The visual selection is surrounded by {begin} and {end}, unless what is
" 	selected is one (and only one) marker. In that latter case, the
" 	function returns a sequence that will replace the selection by {begin}
" 	; if {begin} matches the keybinding of an insert-mode mapping, it will
" 	be expanded.
" {goback}		string
" 	This is the normal-mode sequence to execute after the selected text has
" 	been surrounded; it is meant to place the cursor at the end of {end}
"       Typical values are '%' for true-brackets (), {}, []
"       or '`>ll' when strlen(a:end) == 1.
" 	Note: This sequence will be expanded if it contains mappings or
" 	abbreviations -- this is a feature. see {rtp}/ftplugin/vim_set.vim
" {mustInterpret}	boolean
" 	Indicates whether we must try to find and expand mappings of the form
" 	"!.\{-1,}!" within {begin} and {end}
" 	When true:
" 	- b:usemarks is taken into account: when false, {begin} and {end} will
" 	  be cleared from every occurence of "!mark!".
" 	- if {begin} or {end} contain "!cursorhere!", {goback} will be ignored
" 	  and replaced by a more appropriate value.
" [{a:1}=={im_seq}]	string
" 	Insert-mode sequence that must be returned instead of {begin} if we try
" 	to surround a marker.
" 	Note: This sequence will be expanded if it contains mappings or
" 	abbreviations -- this is a feature. see {rtp}/ftplugin/vim_set.vim
"
" Usage:
" 	:vnoremap <buffer> {key} <c-\><c-n>@=Surround({parameters})<cr>
"
" Examples:
"  (*) Excerpt from common_brackets.vim
"    :vnoremap <buffer> [ <c-\><c-n>@=Surround('[', ']', 0, 0, '%', 0)<cr>
"  (*) Excerpt from my vim-ftplugin
"    :vnoremap <buffer> <silent> <m-f> 
"     \ <c-\><c-n>@=Surround('function! !cursorhere!(!mark!)', 'endfunction',
"     \ 1, 1, '', 1, 'fun ')<cr>
"  (*) Excerpt from my c-ftplugin
"    :vnoremap <buffer> <silent> <localleader>for 
"     \ <c-\><c-n>@=Surround('for (!cursorhere!;!mark!;!mark!) {', '}!mark!',
"     \ 1, 1, '', 1, 'for ')<cr>
"
" }}}
"===========================================================================
"
"---------------------------------------------------------------------------
" Avoid reinclusion
if !exists('g:misc_map_loaded') || exists('g:force_reload_misc_map')
  let g:misc_map_loaded = 1
  let cpop = &cpoptions
  set cpoptions-=C
  scriptencoding latin1
"
if !exists(':Silent') " {{{
  if version < 600
    command! -nargs=+ -bang Silent exe "<args>"
  else
    command! -nargs=+                -bang Silent silent<bang> <args>
  endif
endif
" }}}
"---------------------------------------------------------------------------
function! Map4TheseContexts(key, ...) " {{{
  " Note: requires Vim 6.x
  let syn = synIDattr(synID(line('.'),col('.')-1,1),'name') 
  let i = 1
  while i < a:0
    if (a:{i} =~ '^\(\k\|\\|\)\+$') && (syn =~? a:{i})
      return ReinterpretEscapedChar(a:{i+1})
      " exe 'return "' . 
	    " \   substitute( a:{i+1}, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
    endif
    let i = i + 2
  endwhile
  " Else: default case
  if i == a:0
    return ReinterpretEscapedChar(a:{a:0})
    " exe 'return "' . 
	  " \   substitute( a:{a:0}, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
  else
    return a:key
  endif
endfunction
" }}}
"---------------------------------------------------------------------------
function! MapContext(key, ...) " {{{
  " Note: requires Vim 6.x
  let syn = synIDattr(synID(line('.'),col('.')-1,1),'name') 
  if syn =~? 'comment\|string\|character\|doxygen'
    return a:key
  else
    let i = 1
    while i < a:0
      if (a:{i} =~ '^\k\+$') && (syn =~? a:{i})
	return ReinterpretEscapedChar(a:{i+1})
	" exe 'return "' . 
	      " \   substitute( a:{i+1}, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
      endif
      let i = i + 2
    endwhile
    " Else: default case
    if i == a:0
      return ReinterpretEscapedChar(a:{a:0})
      " exe 'return "' . 
	    " \   substitute( a:{a:0}, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
    else
      return a:key
    endif
  endif 
endfunction
" }}}
"---------------------------------------------------------------------------
function! MapNoContext(key, seq) " {{{
  let syn = synIDattr(synID(line('.'),col('.')-1,1),'name') 
  if syn =~? 'comment\|string\|character\|doxygen'
    return a:key
  else
    return ReinterpretEscapedChar(a:seq)
    " exe 'return "' . 
      " \   substitute( a:seq, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
  endif 
endfunction
" }}}
"---------------------------------------------------------------------------
function! MapNoContext2(key, seq) " {{{
  let c = col('.')-1
  let l = line('.')
  let syn = synIDattr(synID(l,c,1), 'name') 
  if syn =~? 'comment\|string\|character\|doxygen'
    return a:key
  elseif getline(l)[c-1] =~ '\k'
    return a:key
  else
    return ReinterpretEscapedChar(a:seq)
    " exe 'return "' . 
      " \   substitute( a:seq, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
  endif 
endfunction
" }}}
"---------------------------------------------------------------------------
function! BuildMapSeq(seq) " {{{
  let r = ''
  let s = a:seq
  while strlen(s) != 0 " For every '!.*!' pattern, extract it
    let r = r . substitute(s,'^\(.\{-}\)\(\(!\k\{-1,}!\)\(.*\)\)\=$', '\1', '')
    let c =     substitute(s,'^\(.\{-}\)\(\(!\k\{-1,}!\)\(.*\)\)\=$', '\3', '')
    let s =     substitute(s,'^\(.\{-}\)\(\(!\k\{-1,}!\)\(.*\)\)\=$', '\4', '')
    let m = maparg(c,'i')
    if strlen(m) != 0
      silent exe 'let m="' . substitute(m, '<\(.\{-1,}\)>', '"."\\<\1>"."', 'g') . '"'
      if has('iconv') " small workaround for !imappings! in UTF-8 on linux
	let m = iconv(m, "latin1", &encoding)
      endif
      " let m = ReinterpretEscapedChar(m)
      let r = r . m
    else
      let r = r . c
    endif
  endwhile
  return ReinterpretEscapedChar(r)
  " silent exe 'return "' . 
    " \   substitute( r, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
endfunction
" }}}
"---------------------------------------------------------------------------
function! ReinterpretEscapedChar(seq) " {{{
  let seq = escape(a:seq, '"')
  exe 'return "' . 
    \   substitute( seq, '\\<\(.\{-}\)\\>', '"."\\<\1>"."', 'g' ) .  '"'
endfunction
" }}}
"---------------------------------------------------------------------------
function! MapAroundVisualLines(begin,end,isLine,isIndented) range " {{{
  :'<,'>call InsertAroundVisual(a:begin, a:end, a:isLine, a:isIndented)
endfunction " }}}

function! InsertAroundVisual(begin,end,isLine,isIndented) range " {{{
  " Note: to detect a marker before surrounding it, use Surround()
  let pp = &paste
  set paste
  " 'H' stands for 'High' ; 'B' stands for 'Bottom' 
  " 'L' stands for 'Left', 'R' for 'Right'
  let HL = "`<i"
  if &selection == 'exclusive'
    let BL = "\<esc>`>i"
  else
    let BL = "\<esc>`>a"
  endif
  let HR = "\<esc>"
  let BR = "\<esc>"
  " If visual-line mode macros -> jump between stuffs
  if a:isLine == 1
    let HR="\<cr>".HR
    let BL=BL."\<cr>"
  endif
  " If indentation is used
  if a:isIndented == 1
    if version < 600 " -----------Version 6.xx {{{
      if &cindent == 1	" C like sources -> <c-f> defined
	let HR="\<c-f>".HR
	let BR="\<c-t>".BR
      else		" Otherwise like LaTeX, VIM
	let HR=HR.":>\<cr>"
	let BR=BR.":<\<cr>"
      endif
      let BL='>'.BL  " }}}
    else " -----------------------Version 6.xx
      let HR=HR."gv``="
    endif
  endif
  " The substitute is here to compensate a little problem with HTML tags
  Silent exe "normal! gv". BL.substitute(a:end,'>',"\<c-v>>",'').BR.HL.a:begin.HR
  " 'gv' is used to refocus on the current visual zone
  "  call confirm(strtrans( "normal! gv". BL.a:end.BR.HL.a:begin.HR), "&Ok")
  let &paste=pp
endfunction
" }}}
"---------------------------------------------------------------------------
" Function: EatChar()	{{{
" Thanks to the VIM Mailing list ; 
" Note: In it's foo.vim, Benji Fisher maintains a more robust version of this
" function; see: http://www.vim.org/script.php?script_id=72
" NB: To make it work with VIM 5.x, replace the '? :' operator with an 'if
" then' test.
" This version does not support multi-bytes characters.
" Todo: add support for <buffer>
function! EatChar(pat)
  let c = nr2char(getchar())
  return (c =~ a:pat) ? '' : c
endfunction

command! -narg=+ Iabbr execute "iabbr " <q-args>."<C-R>=EatChar('\\s')<CR>"
command! -narg=+ Inoreabbr 
      \ execute "inoreabbr " <q-args>."<C-R>=EatChar('\\s')<CR>"

" }}}
"---------------------------------------------------------------------------
" In order to define things like '{'
function! Smart_insert_seq1(key,expr1,expr2) " {{{
  return s:Smart_insert_seq1(a:key, a:expr1, a:expr2)
endfunction " }}}
function! s:Smart_insert_seq1(key,expr1,expr2) " {{{
  if exists('b:usemarks') && b:usemarks
    return MapNoContext(a:key,BuildMapSeq(a:expr2))
    " return "\<c-r>=MapNoContext('".a:key."',BuildMapSeq('".a:expr2."'))\<cr>"
  else
    return MapNoContext(a:key,a:expr1)
    " return "\<c-r>=MapNoContext('".a:key."', '".a:expr1."')\<cr>"
  endif
endfunction " }}}

function! Smart_insert_seq2(key,expr,...) " {{{
  if a:0 > 0
    return s:Smart_insert_seq(a:key, a:expr, a:1)
  else
    return s:Smart_insert_seq(a:key, a:expr)
  endif
endfunction " }}}
function! s:Smart_insert_seq(key,expr, ...) " {{{
  let rhs = escape(a:expr, '\')
  " Strip marks (/placeholders) if they are not wanted
  if !exists('b:usemarks') || !b:usemarks
    let rhs = substitute(rhs, '!mark!', '', 'g')
  endif
  " Interpret the sequence if it is meant to
  if rhs =~ '\m!\(mark\%(here\)\=\|movecursor\)!'
    " may be, the regex should be '\m!\S\{-}!'
    let rhs = BuildMapSeq(escape(rhs, '\'))
  endif
  " Build & return the context dependent sequence to insert
  if a:0 > 0
    return Map4TheseContexts(a:key, a:1, rhs)
  else
    return MapNoContext(a:key,rhs)
  endif
endfunction " }}}
"---------------------------------------------------------------------------
" Mark where the cursor should be at the end of the insertion {{{
function! LHCursorHere(...)
  " NB: ``|'' requires virtcol() but cursor() requires col()
  " let s:gotomark = line('.') . 'normal! '.virtcol('.')."|"
  " let s:gotomark = 'call cursor ('.line('.').','.col('.').')'
  if a:0 > 0
    let s:goto_lin_{a:1} = line('.')
    let s:goto_col_{a:1} = virtcol('.')
    " let g:repos = "Repos (".a:1.") at: ". s:goto_lin_{a:1} . 'normal! ' . s:goto_col_{a:1} . '|'
  else
    let s:goto_lin = line('.')
    let s:goto_col = virtcol('.')
    " let g:repos = "Repos at: ". s:goto_lin . 'normal! ' . s:goto_col . '|'
  endif
  let s:old_indent = indent(line('.'))
  " return ''
endfunction

function! LHGotoMark()
  let s:old_indent = indent(s:goto_lin) - s:old_indent 
  if s:old_indent != 0
    let s:goto_col = s:goto_col + s:old_indent
  endif
  " uses {lig}'normal! {col}|' because of the possible reindent
  execute s:goto_lin . 'normal! ' . s:goto_col . '|'
  " return ''
endfunction
function! LHGotoEndMark()
  let s:old_indent = indent(s:goto_lin) - s:old_indent 
  if s:old_indent != 0
    let s:goto_col = s:goto_col + s:old_indent
  endif
  if     s:goto_lin != s:goto_lin_2
    " TODO: !!
  else
    let s:goto_col = s:goto_col + s:goto_col_2 - s:goto_col_1
  endif
  " uses {lig}'normal! {col}|' because of the possible reindent
  execute s:goto_lin . 'normal! ' . s:goto_col . '|'
  " return ''
endfunction
" }}}
"---------------------------------------------------------------------------
" Function: InsertSeq(key, seq, [context]) {{{
function! InsertSeq(key,seq, ...)
  let s:gotomark = ''
  let mark = a:seq =~ '!cursorhere!'
  let seq = ReinterpretEscapedChar(a:seq)
  let seq = seq . (mark ? '!movecursor!' : '')
  " internal mappings
  inoremap !cursorhere! <c-\><c-n>:call LHCursorHere()<cr>a
  inoremap !movecursor! <c-\><c-n>:call LHGotoMark()<cr>a
  "inoremap !cursorhere! <c-\><c-n>:call <sid>CursorHere()<cr>a
  "inoremap !movecursor! <c-\><c-n>:call <sid>GotoMark()<cr>a
  " Build the sequence to insert
  if a:0 > 0
    let res = s:Smart_insert_seq(a:key, seq, a:1)
  else
    let res = s:Smart_insert_seq(a:key, seq)
  endif
  " purge the internal mappings
  iunmap !cursorhere!
  iunmap !movecursor!
  return res
endfunction
" }}}
"---------------------------------------------------------------------------
" Function: IsAMarker() {{{
" Returns whether the text currently selected matches a marker and only one.
function! IsAMarker()
  if line("'<") == line("'>") " I suppose markers don't spread over several lines
    " Extract the selected text
    let a_save = @a
    normal! gv"ay
    let a = @a
    let @a = a_save

    " Check whether the selected text matches a marker (and only one)
    if (a =~ '^'.Marker_Txt('.\{-}').'$') 
	  \ && (a !~ '\%(.*'.Marker_Close().'\)\{2}')
      " If so, return {a:begin}, or {im_seq} if provided
      " return 'gv"_c'.((a:0>0) ? (a:1) : (a:begin))
      return 1
    endif
  endif
  return 0
endfunction
"}}}

" Surround any visual selection but not a marker! 
" Function: Surround(begin,end, isIndented, goback, mustInterpret [, imSeq] ) {{{
function! Surround(
      \ begin, end, isLine, isIndented, goback, mustInterpret, ...) range
  if IsAMarker()
      return 'gv"_c'.((a:0>0) ? (a:1) : (a:begin))
  endif

  " Prepare {a:begin} and {a:end} to be inserted around the visual selection
  let begin = a:begin
  let end = a:end
  let goback = a:goback
  if a:mustInterpret
    " internal mappings
    " <c-o> should be better for !cursorhere! as it does not move the cursor
    " But only <c-\><c-n> works correctly. 
    " inoremap !cursorhere! <c-o>:call <sid>CursorHere()<cr>
    if 0
      " inoremap !cursorhere! <c-\><c-n>:call <sid>CursorHere()<cr>a
      " inoremap !cursorpos1! <c-o>:call <sid>CursorHere(1)<cr>
      " inoremap !cursorpos2! <c-o>:call <sid>CursorHere(2)<cr>
      " inoremap !movecursor! <c-\><c-n>:call <sid>GotoMark()<cr>a
      " inoremap !movecursor2! <c-\><c-n>:call <sid>GotoEndMark()<cr>a
    endif
    inoremap !cursorhere! <c-\><c-n>:call LHCursorHere()<cr>a
    " Weird: cursorpos1 & 2 require <c-o> an not <c-\><c-n>
    inoremap !cursorpos1! <c-o>:call LHCursorHere(1)<cr>
    inoremap !cursorpos2! <c-o>:call LHCursorHere(2)<cr>
    " <c-\><c-n>....a is better for !movecursor! as it leaves the cursor `in'
    " insert-mode... <c-o> does not; that's odd.
    inoremap !movecursor! <c-\><c-n>:call LHGotoMark()<cr>a
    inoremap !movecursor2! <c-\><c-n>:call LHGotoEndMark()<cr>a
    " inoremap !movecursor! <sid>GotoMark().'a'

    " Check whether markers must be used
    if (!exists('b:usemarks') || !b:usemarks)
      let begin = substitute(begin, '!mark!', '', 'g')
      let end = substitute(end, '!mark!', '', 'g')
    endif
    " Override the value of {goback} if "!cursorhere!" is used.
    if (begin =~ '!cursorhere!') 
      let goback = BuildMapSeq('!movecursor!')
    endif
    if (end =~ '!cursorhere!')
      let begin = '!cursorpos1!'.begin.'!cursorpos2!'
      let goback = BuildMapSeq('!movecursor2!')
      if !a:isLine && (line("'>") == line("'<")) && ('V'==visualmode())
	    \ && (getline("'>")[0] =~ '\s') 
	:normal! 0"_dw
	" TODO: fix when &selection == exclusive
      endif
    endif
    " Transform {begin} and {end} (interpret the "inlined" mappings)
    let begin = BuildMapSeq(begin)
    let end = BuildMapSeq(end)

    " purge the internal mappings
    iunmap !cursorhere!
    iunmap !cursorpos1!
    iunmap !cursorpos2!
    iunmap !movecursor!
  endif
  " Call the function that really insert the text around the selection
  call InsertAroundVisual(begin, end, a:isLine, a:isIndented)
  " Return the nomal-mode sequence to execute at the end.
  return goback
endfunction
" }}}
"---------------------------------------------------------------------------

" Avoid reinclusion
  let &cpoptions = cpop
endif

"===========================================================================
" vim600: set fdm=marker:
