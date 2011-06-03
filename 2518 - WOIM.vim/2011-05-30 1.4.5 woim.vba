" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
syntax/woim.vim	[[[1
225
" Vim syntax and filetype plugin for WOIM files (.woim)
" Language:		Self defined markup for WOIM lists in Vim
" Author:		Geir Isene <geir@isene.com>
" Web_site:		http://isene.com/
" WOIM_def:		http://isene.com/woim.pdf
" License:		I release all copyright claims. 
"				This code is in the public domain.
"				Permission is granted to use, copy modify, distribute, and
"				sell this software for any purpose. I make no guarantee
"				about the suitability of this software for any purpose and
"				I am not liable for any damages resulting from its use.
"				Further, I am under no obligation to maintain or extend
"				this software. It is provided on an 'as is' basis without
"				any expressed or implied warranty.
" Version:		1.4.5 - compatible with WOIM v. 1.4
" Modified:		2011-05-30
"
" Changes since previous mod:
" Overhaul:		Created the documentation, includin the whole WOIM
"				definition to make it easily accessible within VIM.
"				Added the INSTALL and README files, zipped and ready for
"				easy installation.
"
" INSTRUCTIONS
"
" Use tabs/shifts or * for indentations
"
" Use <SPACE> to toggle one fold
" Use \0 to \9, \a, \b, \c, \d, \e, \f to show up to 15 levels expanded
"
" Use <leader>s to remove underlining of States (prefixed with S:)
" Use <leader>S to add underlining of States (prefixed with S:)
" Use <leader>t to remove underlining of Transitions (prefixed with T:)
" Use <leader>T to add underlining of Transitions (prefixed with T:)
"
" Use <leader>v to add a checkbox at start of item or to toggle a checkbox
" Use <leader>V to add/toggle a checkbox with a date stamp for completion
"
" Use <leader><SPACE> to go to the next open template element
" (A template element is a WOIM item ending in an equal sign)
"
" Syntax updated at start and every time you leave Insert mode


" Initializing
if exists("b:current_syntax")
  finish
endif

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Basics
let	b:current_syntax="WOIM"
set textwidth=0
set	shiftwidth=2
set	tabstop=2
set	softtabstop=2
set noexpandtab
set	foldmethod=syntax
set fillchars=fold:\ 
syn sync fromstart
autocmd InsertLeave * :syntax sync fromstart

" Identifier (any number in front)
syn	match	WOIMident	 "\(\t\|\*\)*[0-9.]\+\.\s"

" Multi-line
syn match	WOIMmulti	"^\(\t\|\*\)*+"

" Qualifiers are enclosed within [ ]
syn	match	WOIMqual	"\[.\{-}\]" contains=WOIMtodo,WOIMref,WOIMcomment

" Tags - anything that ends in a colon
syn	match	WOIMtag		'\(\s\|\*\)\@<=[a-zA-ZæøåÆØÅ0-9,._&?%= \-\/+<>#']\{-2,}:\s' contains=WOIMtodo,WOIMcomment,WOIMquote,WOIMref

" WOIM operators
syn	match	WOIMop		"\s[A-ZÆØÅ_/]\{-2,}:\s" contains=WOIMcomment,WOIMquote

" Mark semicolon as stringing together lines
syn match	WOIMsc		";"

" References start with a hash (#)
syn	match	WOIMref		"#\{1,2}\(\'[a-zA-ZæøåÆØÅ0-9,.:/ _&?%=\-\*]\+\'\|[a-zA-ZæøåÆØÅ0-9.:/_&?%=\-\*]\+\)" contains=WOIMcomment

" Comments are enclosed within ( )
syn	match	WOIMcomment	"(\_.\{-})" contains=WOIMtodo,WOIMref

" Text in quotation marks
syn	match	WOIMquote	'"\_.\{-}"' contains=WOIMtodo,WOIMref

" TODO  or FIXME
syn	keyword WOIMtodo	TODO FIXME						

" Item motion
syn match	WOIMmove	">>\|<<\|->\|<-"

" Bold and Italic
syn	match   WOIMb		" \@<=\*.\{-}\* "
syn	match   WOIMi		" \@<=/.\{-}/ "
syn	match   WOIMu		" \@<=_.\{-}_ "

" State & Transitions
syn match	WOIMstate	"\([.* \t]S: \)\@<=[^;]*" contains=WOIMtodo,WOIMop,WOIMcomment,WOIMref,WOIMqual,WOIMsc,WOIMmove,WOIMtag,WOIMquote
syn match	WOIMtrans	"\([.* \t]T: \)\@<=[^;]*" contains=WOIMtodo,WOIMop,WOIMcomment,WOIMref,WOIMqual,WOIMsc,WOIMmove,WOIMtag,WOIMquote

" Cluster the above
syn cluster WOIMtxt contains=WOIMident,WOIMmulti,WOIMop,WOIMqual,WOIMtag,WOIMref,WOIMcomment,WOIMquote,WOIMsc,WOIMtodo,WOIMmove,WOIMb,WOIMi,WOIMu,WOIMstate,WOIMtrans

" Levels
syn region L15 start="^\(\t\|\*\)\{14} \=\S" end="^\(^\(\t\|\*\)\{15,} \=\S\)\@!" fold contains=@WOIMtxt
syn region L14 start="^\(\t\|\*\)\{13} \=\S" end="^\(^\(\t\|\*\)\{14,} \=\S\)\@!" fold contains=@WOIMtxt,L15
syn region L13 start="^\(\t\|\*\)\{12} \=\S" end="^\(^\(\t\|\*\)\{13,} \=\S\)\@!" fold contains=@WOIMtxt,L14,L15
syn region L12 start="^\(\t\|\*\)\{11} \=\S" end="^\(^\(\t\|\*\)\{12,} \=\S\)\@!" fold contains=@WOIMtxt,L13,L14,L15
syn region L11 start="^\(\t\|\*\)\{10} \=\S" end="^\(^\(\t\|\*\)\{11,} \=\S\)\@!" fold contains=@WOIMtxt,L12,L13,L14,L15
syn region L10 start="^\(\t\|\*\)\{9} \=\S"  end="^\(^\(\t\|\*\)\{10,} \=\S\)\@!" fold contains=@WOIMtxt,L11,L12,L13,L14,L15
syn region L9 start="^\(\t\|\*\)\{8} \=\S"   end="^\(^\(\t\|\*\)\{9,} \=\S\)\@!"  fold contains=@WOIMtxt,L10,L11,L12,L13,L14,L15
syn region L8 start="^\(\t\|\*\)\{7} \=\S"   end="^\(^\(\t\|\*\)\{8,} \=\S\)\@!"  fold contains=@WOIMtxt,L9,L10,L11,L12,L13,L14,L15
syn region L7 start="^\(\t\|\*\)\{6} \=\S"   end="^\(^\(\t\|\*\)\{7,} \=\S\)\@!"  fold contains=@WOIMtxt,L8,L9,L10,L11,L12,L13,L14,L15
syn region L6 start="^\(\t\|\*\)\{5} \=\S"   end="^\(^\(\t\|\*\)\{6,} \=\S\)\@!"  fold contains=@WOIMtxt,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L5 start="^\(\t\|\*\)\{4} \=\S"   end="^\(^\(\t\|\*\)\{5,} \=\S\)\@!"  fold contains=@WOIMtxt,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L4 start="^\(\t\|\*\)\{3} \=\S"   end="^\(^\(\t\|\*\)\{4,} \=\S\)\@!"  fold contains=@WOIMtxt,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L3 start="^\(\t\|\*\)\{2} \=\S"   end="^\(^\(\t\|\*\)\{3,} \=\S\)\@!"  fold contains=@WOIMtxt,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L2 start="^\(\t\|\*\)\{1} \=\S"   end="^\(^\(\t\|\*\)\{2,} \=\S\)\@!"  fold contains=@WOIMtxt,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L1 start="^\S"                    end="^\(^\(\t\|\*\)\{1,} \=\S\)\@!"  fold contains=@WOIMtxt,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15

" Folds
set foldtext=WOIMFoldText()
function! WOIMFoldText()
  let line = getline(v:foldstart)
  let myindent = indent(v:foldstart)
  let line = substitute(line, '^\s*', '', 'g')
  while myindent != 0
    let myindent = myindent - 1
    let line = ' ' . line
  endwhile
  return line
endfunction

" Checkbox and timestamp
function! CheckItem (stamp)
  let current_line = getline('.')
  if match(current_line,'\V[_]') >= 0
    let time = strftime("%Y-%m-%d %H.%M")
    exe 's/\V[_]/[x]/'
    if a:stamp == "stamped"
      exe "normal 0f]a ".time.":"
    endif
  elseif match(current_line,'\V[x]') >= 0
    exe 's/\V[x]/[_]/i'
    exe 's/\V[_] \d\d\d\d-\d\d-\d\d \d\d\.\d\d:/[_]/e'
  else
    exe "normal ^i[_] "
  endif
endfunction

" Goto reference
function! GotoRef()
  let ref_word = expand("<cWORD>")
  let ref_word = substitute(ref_word, '#', '', 'g')
  let ref_word = substitute(ref_word, "\'", '', 'g')
  let ref_dest = substitute(ref_word, '/', '.*\\n\\s*.\\{-}', 'g')
  let @/ = ref_dest
  call search(ref_dest)
endfunction


" Highlighting and Linking :
hi				Folded			ctermfg=yellow ctermbg=none
hi				L1				gui=bold term=bold cterm=bold
hi def link		WOIMident		Define
hi def link		WOIMmulti		String
hi def link		WOIMop			Function
hi def link		WOIMqual		Type
hi def link		WOIMtag			String
hi def link		WOIMref			Define
hi def link		WOIMcomment		Comment
hi def link		WOIMquote		Comment
hi def link		WOIMsc			Type
hi def link		WOIMtodo		Todo
hi def link		WOIMmove		Error
hi				WOIMb			ctermfg=none ctermbg=none gui=bold term=bold cterm=bold
hi				WOIMi			ctermfg=none ctermbg=none gui=italic term=italic cterm=italic
hi link			WOIMu			underlined
hi link			WOIMstate		underlined

" VIM parameters
syn	match		WOIMvim			"^vim:.*"
hi def link		WOIMvim			Function

" macros
map <leader>0	:set foldlevel=0<CR>
map <leader>1	:set foldlevel=1<CR>
map <leader>2	:set foldlevel=2<CR>
map <leader>3	:set foldlevel=3<CR>
map <leader>4	:set foldlevel=4<CR>
map <leader>5	:set foldlevel=5<CR>
map <leader>6	:set foldlevel=6<CR>
map <leader>7	:set foldlevel=7<CR>
map <leader>8	:set foldlevel=8<CR>
map <leader>9	:set foldlevel=9<CR>
map <leader>a	:set foldlevel=10<CR>
map <leader>b	:set foldlevel=11<CR>
map <leader>c	:set foldlevel=12<CR>
map <leader>d	:set foldlevel=13<CR>
map <leader>e	:set foldlevel=14<CR>
map <leader>f	:set foldlevel=15<CR>
map <SPACE>		za

map <leader>s	:hi link WOIMstate NONE<CR>
map <leader>S	:hi link WOIMstate underlined<CR>
map <leader>t	:hi link WOIMtrans NONE<CR>
map <leader>T	:hi link WOIMtrans underlined<CR>

map <leader>v	:call CheckItem("")<CR>
map <leader>V	:call CheckItem("stamped")<CR>

map gr			:call GotoRef()<CR>

map <leader><SPACE>	/=\s*$<CR>A

" vim: ts=4
doc/woim.txt	[[[1
822
*woim.txt*   The VIM plugin for WOIM

WOIM is a methodology to describe anything - any state, item(s), pattern,
action, process, transition, program, instruction set etc. So, you can use
it as an outliner, a ToDo list handler, a process design tool, a data
modeler, or any other way you want to describe something.

For information on WOIM, read the WOIM description in full:

  http://isene.com/woim.pdf


==============================================================================
CONTENTS                                                       *WOIM-Contents*

    1.Plugin Intro............................|WOIM|
    2.Plugin Functionality....................|WOIM-Plugin-Functionality|
    3.WOIM Definition.........................|WOIM-Definition|
		3.1 A WOIM list...........................|WOIM-List|
		3.2 A WOIM item...........................|WOIM-Item|
		3.2.1 Starter.............................|WOIM-Starter|
		3.2.2 Type................................|WOIM-Type|
		3.2.3 Content.............................|WOIM-Content|
		3.2.3.1 Elements..........................|WOIM-Elements|
		3.2.3.1.1 Operator........................|WOIM-Operator|
		3.2.3.1.2 Qualifier.......................|WOIM-Qualifier|
		3.2.3.1.2 Tags............................|WOIM-Tags|
		3.2.3.1.3 Description.....................|WOIM-Description|
		3.2.3.2 Additives.........................|WOIM-Additives|
		3.2.3.2.1 References......................|WOIM-References|
		3.2.3.2.2 Comments........................|WOIM-Comments|
		3.2.3.2.3 Quotes..........................|WOIM-Quotes|
		3.2.3.2.4 Change Markup...................|WOIM-ChangeMarkup|
		3.2.3 Separator...........................|WOIM-Separator|
		3.3 WOIM of WOIM..........................|WOIM-WOIM|
    4.About...................................|WOIM-About|
    5.Changelog...............................|WOIM-Changelog|
    6.Credits.................................|WOIM-Credits|
    7.License.................................|WOIM-License|

==============================================================================
1. Plugin Intro                                                         *WOIM*

This plugin does both highlighting and various automatic handling of WOIM
lists, like collapsing lists or parts of lists in a sophisticated way.

==============================================================================
2. Plugin Functionality                            *WOIM-Plugin-functionality*

Use tabs/shifts or * for indentations

Use <SPACE> to toggle one fold
Use \0 to \9, \a, \b, \c, \d, \e, \f to show up to 15 levels expanded

Use <leader>s to remove underlining of States (prefixed with S:)
Use <leader>S to add underlining of States (prefixed with S:)
Use <leader>t to remove underlining of Transitions (prefixed with T:)
Use <leader>T to add underlining of Transitions (prefixed with T:)

Use <leader>v to add a checkbox at start of item or to toggle a checkbox
Use <leader>V to add/toggle a checkbox with a date stamp for completion

Use "gr" (without the quotation marks, signifies "Goto Ref") while the
cursor is on a WOIM reference to jump to that destination in a WOIM list

Use <leader><SPACE> to go to the next open template element
(A template element is a WOIM item ending in an equal sign)

Syntax updated at start and every time you leave Insert mode 

==============================================================================
3. WOIM Definition                                           *WOIM-Definition*

WOIM :: Version 1.4 (2011-05-23)

Having worked extensively with flowcharts, relational charts, Venn diagrams,
Warnier-Orr diagrams and other ways of representing processes, states,
instructions, programs and data models, I knew there was something missing. It
would be great to have a way of representing anything - any state or action -
in a way that would be more conducive to collaboration. 

Most people know about flowcharts. But there are other ways of representing
data, states, actions or processes: UML, Sankey diagrams, decision trees, Petri
Net, organizational charts, mind maps, Process Flow diagrams, Feynman diagrams,
Data Flow diagrams, Concept Maps, OBASHI, Task Lists (or To-Do lists),
Warnier/Orr diagrams and various other diagrams. More than we can list here.
They have various uses, various strengths and shortcomings.

Most methodologies for representing states or flows were born out of specific
needs and not to be used as generic methods for representing everything as
simple as possible. What if there was a way to represent any state, set of
things, actions, flows or transitions? What if the method was simple? What if
it was also Turing complete? 

Enter WOIM – an acronym for Warnier/Orr/Isene/Moeller. At this time it may as
well have been called something else as it is very different from the
Warnier/Orr diagrams – but I would like to honor the origins of the idea.

WOIM is a system for representing data. Any data. Static or dynamic. It can be
used to describe any state; A thing or set of things, an area, a concept or
collection of concepts etc. It can also be used to describe any action or plan,
transformation or transition. In fact, it can describe anything - with one
complete markup set. WOIM can be be used as an outliner on steroids or a
todo-list managing system with endless possibilities.

After searching for a complete markup methodology for both states or things and
actions or transitions, I came across the Warnier/Orr diagrams. They seemed to
be the best foundation for what I needed. The Warnier/Orr diagrams were
expanded to be able to as easily as possible describe anything. I removed the
graphical parts of Warnier/Orr and expanded the methodology substantially. My
colleague Egil Moeller helped in the early refinements of WOIM.

The strengths of WOIM are many:
* can represent any state or action with any amount of levels 
* Turing complete 
* text-based (it is wiki-able - i.e. it is easy to collaborate when creating WOIM lists) 
* not graphical (although it can easily be made graphical for ease of consumtion or eye candy) 
* an easy syntax, humanly very readable 
* compact 
* can represent negatives (NOT, don't do the following actions) 
* can represent any number of choices in a decision (hard to do in a flow chart) 
* easy to do loop counts 
* easy to show attributes such as time or timing, location, responsibility etc. 
* potentially easy to map to other representation methods (graphical or not) 
* In its simplest form, a WOIM list is just a list of items - 
  like your regular shopping list - but it can be so much more if you need it. 
	A couple of examples will give you the basic idea.

An example of describing a state:
 Car (not complete example)
    Exterior
       Paint
       Chrome decor
       Windows
          Robber linings
       [4] Wheels
    Interior
       Seats
          [2] Front
          [3] Back
    Mechanics
       Motor
          [6] Cylinders
          [24] Valves
       Breaks

A transition example (task list):

 Walk the dog
    Check the weather
       [?rain] AND/OR:
          Get rain coat
          Get umbrella
       Dress for the temperature
    Get chain
    Call for the dog
    OR:
       Go through the woods
       Walk the usual track
    AND: (concurrency)
       Ensure the dog has done its "tasks"
       Ensure the dog is exercised
          [5+] throw the favorite stick
    Walk home

And this can all be done in collaboration on a wiki. States are described,
processes are mapped, plans and todo-lists are forged. It's rather easy, and
WOIM can accomodate for any level of complexity.

Let's go through the various parts of WOIM and all its possibilities.

------------------------------------------------------------------------------
3.1 A WOIM list                                                    *WOIM-List*

A WOIM list consist of one or more WOIM items.

------------------------------------------------------------------------------
3.2 A WOIM item                                                    *WOIM-Item*

A WOIM item is a line in a WOIM list. It can have "children". Children are WOIM
items indented to the right below the item.

A WOIM item consists of an optional "Starter", an optional "Type", "Content"
and a "Separator" in that sequence. All the various parts of a WOIM item is
described below and in the sequence they appear in an item.

------------------------------------------------------------------------------
3.2.1 Starter                                                   *WOIM-Starter*

A WOIM item may begin with a "Starter". A "Starter" can be either an
"Identifier" or a "Multi-line Indicator".

An Identifier is a unique indicator that can be used in referring to that item. 

A numbering scheme such as X.Y.Z. can be used i.e. the first item in a WOIM
list would be 1. (note the period after the each number, also the last one).
The second item would be 2. etc. A child to the second item would be 2.1. and
the second child of 2. would be identified as 2.2. whereas the cild of 2.2.
would be 2.2.1.

A shorter from mixing numbers and letters can also be used, such as 1A1A for
the first fourth level item. The next fourth level item would be 1A1B. When
using this scheme, there is no need for any periods. The identifier “21H2AD”
would be equivalent to “21.8.2.30.”, saving 4 characters.

An item that spans several lines must have a Starter. The second line of an
item will be indented to the same level as the first with an added space in
front.

You don't have to use an Identifier just because an item spans several lines.
You may use a "Multi-line Indicator" instead. Just prefix an item with a plus
("+"), and it shows that the item spans more than one line.

If you use a Starter on one item, then all the items in that same group of
items on the same level/indent must also have a Starter.

------------------------------------------------------------------------------
3.2.2 Type                                                         *WOIM-Type*

If it is not obvious or for clearity and strictness, prefix an item with "S:"
if the item is a static or state-item (something which does not denote action).
Use "T:" for a transition item (an item indicating action). Alternatively you
may use “|” instead of “S:”, and “/” instead of “T:”. The Type indicator comes
after the optional starter. 

Children of a certain type (state or transition) inherit their parents type
unless otherwise specified.

------------------------------------------------------------------------------
3.2.3 Content                                                   *WOIM-Content*

A WOIM item must have some sort of Content. The Content can be an "Element"
and/or  an "Additive".

------------------------------------------------------------------------------
3.2.3.1 Elements                                               *WOIM-Elements*

Elements are either an "Operator", a "Qualifier", a "Tag" or a "Description".
Let's treat each of these concepts.

------------------------------------------------------------------------------
3.2.3.1.1 Operator                                             *WOIM-Operator*

An Operator is anything that operates on an item or a set of items. It can be
any of the usual logical operators. It can also be other operators such as
"EXAMPLE:", "EXAMPLES:", "CHOOSE:", "ONE OF THESE:", "IMPLIES:", "CONTINUOUS:"
(makes an item or set of items run continuously), etc. An operator is written
in capital letters and ends in a colon.

------------------------------------------------------------------------------
3.2.3.1.2 Qualifier                                           *WOIM-Qualifier*

A Qualifier does as its name suggest, qualify an item. The format is
"[count?condition]". The usage is best described by a few examples:

	Do item 3 times = "[3]"
	Do item 2 to 4 times = "[2..4]"
	Do item in the context of "apples", then "oranges", then "grapes" = "[apples,oranges,grapes]"
	With this you can reuse a procedure in many contexts
	Do item if "the mail has arrived" = "[?the mail has arrived]" 
	Do item 2 times while "foo=true" = "[2?foo=true]" 
	Do item from 3 to 5 times while "bar=false" = "[3..5?bar=false]" 
	Do item 1 or more times = "[1+]" 
	Do item 1 or more times while "Bob is polite" = "[1+?Bob is polite]" 
	Do item up to 4 times only while "zoo=0" = "[<4?zoo=0]"

The question mark is read "if".

To indicate that an item is optional, use the short form "[?]".

If you use WOIM as a todo-list manager or project management tool, there is a
nifty way of showing items to be done and items done; simply add “[_]” in the
beginning of the line for an “unchecked” item and “[x]” for a “checked” item.
You may add a time stamp for the completion after a checked item (“[x]
YYYY-MM-DD hh.mm:”). In this way, you combine the qualifier with a tag.

------------------------------------------------------------------------------
3.2.3.1.2 Tags                                                     *WOIM-Tags*

A Tag is any attribute describing the Content. It ends in a colon.

Examples of tags could be: "Location = Someplace:", "Responsible = Someone:"
and "Strength = Medium". 

A timestamp is a usual Tag with date and time format as "YYYY-MM-DD hhmmss",
conforming to the standard ISO-8601. The time/date format can be shortened to
the appropriate time granularity such as "YYYY-MM-DD hhmm", "YYYY-MM-DD hh" or
"YYYY". One can add a time stamp such as "Time = 2012-12-24 17:" or simply just
"2012-12-24 17:"

Time stamp tags may be relative such as:

	Time relative to previous item = "+YYYY-MM-DD:" 
	Less than a certain time after previous item = "<+YYYY-MM-DD:" 
	More than a certain time after previous item = ">+YYYY-MM-DD:" 
	Time relative to next item = "-YYYY-MM-DD:" 
	Less than a certain time before next item = "<-YYYY-MM-DD:" 
	More than a certain time before next item = ">-YYYY-MM-DD:" 
	Time relative to other item, example = "+YYYY-MM-DD#otheritem:" 

The last example introduces a new concept, the "Reference". References will be
discussed later.

Other obvious time tags may be used, such as: 

	"+1 week:" 
	"-2 Marsian years:" 

Some practical examples:

	Wait one month before doing the item = "+YYYY-01-DD:" 
	Do item less than 4 days before next item = "<-YYYY-MM-04:" 
	Wait one year and two days after item X = "+0001-00-02#X:" 

It is also possible to use recurring items in WOIM. 

The strict format being “YYYY-MM-DD+X Day hh.mm+Y - YYYY-MM-DD hh.mm”. The
first date marks the starting date and the last date marks the end of the
repetition interval. The “+X” is the repetition of the date, while the “+Y” is
the repetition of the time. You use what you need, i.e. If there is no
repetition within a day, obviously the “+Y” would be skipped. Some examples:

	"2011-05-01+7 13.00" = 2011-05-01 1pm, repeated every 7 days
	"2011-05-01+2,3,2" = Every 2, then 3, then 2 days, etc
	"2011-05-01+2 - 2012-05-01" = Every second day for one year
	"2011-05-01 13.00+1" = 2011-05-01 1pm, repeated every hour
	"2011-05-01 Fri,Sat - 2011-10-01" = Fri & Sat in interval

You can also use all possible intuitive variations by leaving certain parts of
the date stamp undescribed: 

	"YYYY-MM-03" = Every third day of the month
	"YYYY-12-DD" = Every day in every December
	"2011-MM-05" = Every fifth day of every month of 2011
	"Tue,Fri 12.00" = Noon every Tuesday and Friday
	
------------------------------------------------------------------------------
3.2.3.1.3 Description                                       *WOIM-Description*

The Description is the main body, the "meat" of the item. Although an item may
not have a Description, such as a line containing only the Operator "OR:", most
items have a description. 

Many items have only a Description such as a simple todo-list.

------------------------------------------------------------------------------
3.2.3.2 Additives                                             *WOIM-Additives*

Additives can be used alone or in combination with Elements.

An Addition can either be a "Reference", a "Comment", a "Quote" or a "Change
Markup".

------------------------------------------------------------------------------
3.2.3.2.1 References                                         *WOIM-References*

A "#" followed by the name of an item, list or anything else is a Reference. An
example would be a Reference to a website such as "#http://www.isene.com/". A
Reference with spaces must be enclosed in single quotation marks - "#'This is a
reference'" as opposed to "#Reference".

There are two types of Reference:
	1. A redirection or hard reference 
	2. A soft reference 
	
An item consisting only of an Reference is a redirection. For a transition item
this means one would jump to the reference and continue execution from there.
If the redirect is to jump back after executing the Reference (and its
children), then add another hash at the beginning such as "##ref". This makes
it easy to create more compact WOIM Lists by adding a set of commonly used
subroutines at the end of the list. For a state item, a Reference means one
would include the referenced item (and its children) at the Reference point.

There are two special redirections for transition items:

An item consisting only of the key word "SKIP" ends the current WOIM level 

An item consisting only of the key word "END" ends the whole WOIM list 

If the Reference is only part of an Item, it is a "soft reference". It
indicates that one would look for more information at the referred Item. An
even softer reference would be to put the Reference in parenthesis such as
"(#ref)", indicating only an apropos. Parenthesis is used for Comments and
discussed later.

If you reference an item higher up in the WOIM List, a simple reference to the
item is all that is neede. One would refer to the Identifier or to the
appropriate Content, usually the Description. It would make sense to use an
Identifier for an Item if the Description is long if you would want to refer to
that Item. If you refer to an Item further down the hierarchy, you would use a
forward slash ("/") to separate each level (like the "path" used in a URL). If
you want to refer to an item where you first need to "climb the tree" and then
go down another "branch", you start the path with the highest common level and
reference the path from there. An example of a reference with a path would be
"#'Somewhere higher up/one level down/referred item'". You may use a unique
concatenation of the path to shorten it, such as "#'Somewhere higher up/one
lev.../ref...'". The three periods indicate concatenation.

------------------------------------------------------------------------------
3.2.3.2.2 Comments                                             *WOIM-Comments*

Anything within parenthesis is a comment. Comments are are not executed as WOIM
commands - i.e. they are not actions in a list of transition items.

------------------------------------------------------------------------------
3.2.3.2.3 Quotes                                                 *WOIM-Quotes*

Anything in quotation marks is a quote. Like a Comment, a Quote is not executed
as a WOIM command.

------------------------------------------------------------------------------
3.2.3.2.4 Change Markup                                    *WOIM-ChangeMarkup*

When working with WOIM Lists, especially on paper, there may come a need to
mark deletion of items and to show where an item should be moved to. To
accomodate for this need, Change Markup is introduced.

If "<<" is added at the end of an Item, it is slated for deletion.

To show that an Item should be moved, add ">>" at the end followed by a
Reference showing to which Item it should moved below.

To indent an item to the left, add "<-". Indenting an item to the right is
marked by "->". It is quite possible to combine moving and indenting an item
such as moving an item and making it a child of another: ">>#ref->".

------------------------------------------------------------------------------
3.2.3 Separator                                               *WOIM-Separator*

A Separator separates one Item from another. A line in a WOIM List is usually
one Item, and Items are then usually separated by a "newline" (hitting the
"Enter" on the keyboard). But it is possble to string several Items together on
one line by separating them with semicolons.

By separating an Item by a newline and then indenting it to the right, you
create a child Item. A child adds information to its parent.

If the parent Item contains a Description, a separator is read as "then". A
newline and indent to the right (a children) reads "with" or "consists of". If
the parent Item does not contain a Description, the separator is read as
"applies to:". A couple of examples should suffice:

 A kitchen
    Stove
    Table
       Plates
       Knives
       Forks

This would read: "A kitchen with stove and table with plates, knives and forks".

 Time = 2010: 
    Olympic Games
    Soccer world championship

This would read: "Time = 2010: applies to: Olympic Games and Soccer world championship".

 Walk the dog
    Check the weather
       [?rain] AND/OR:
          Get rain coat
          Get umbrella
       Dress for the temperature
    Get chain

And this would read: "Walk the dog consists of Check the weather consists of:
If rain, AND/OR: apply to children: Get rain coat, Get umbrella, then Dress for
the temperature, then Get chain". Or more humanly: "Walk the dog consists of
check the weather which consists of either or get the rain coat and get
umbrealla. Then dress for the temperature and then get the chain."

------------------------------------------------------------------------------
3.3 WOIM of WOIM                                                   *WOIM-WOIM*

WOIM is self describing.

Now that we have covered all the possibilities in a WOIM list, it should be
obvious that WOIM could extend into a vast array of descriptions. It should
even be possible to write a parser or compiler for WOIM and use it as a
programming language.

Would it be possible to compact the descriptions above into a WOIM list? Yes
indeed. The WOIM system is self-describing. The following list shows the legal
structure and syntax of a WOIM list. It cover all you have read above:

WOIM list (2011-05-22)
   [1+] WOIM item
      [?] Starter; OR: 
         Identifier (Numbers: Format = "1.1.1.1. ", Mixed: Format = "1A1A ")
            [?Multi-line item] The Identifier serves as a plus sign ("+")
               #1.
         Multi-line indicator = "+"
            1. Following lines are of same indent with a "space" before
             the text
            + If one item on a certain level/indent is multi-line, all items
             on the same level/indent must start with a plus sign ("+") or #Identifier
      [?] Type
         OR: 
            State = "S: " OR "| "
            Transition = "T: " OR "/ "
         Children inherit Type from parent unless marked with different type
         Can be skipped when the item is obviously a state or transition
      Content; AND: 
         [?] Element
            Operator
               Anything operating on an item or a set of items
                  [?Set of items] Items are indented below the operator
               Can be any of the usual logical operators
               Is written in capitals ending in a colon and a space
               EXAMPLES: "AND: ", "OR: ", "AND/OR: ", "NOT: ", "IMPLIES: "
               To make the item run continuously, use "CONTINUOUS: " 
                  Item is done concurrent with remaining items
                  The operator can be combined with a time stamp tag
                     EXAMPLE: "CONTINUOUS: YYYY-MM-07:" = Do the item weekly
            Qualifier
               Format = "[Count?Condition]"; EXAMPLES: 
                  Do item 3 times = "[3]"
                  Do item if "the mail has arrived" = "[?the mail has arrived]"
                  Do item 2 times while "foo=true" = "[2?foo=true]"
                  Do item from 3 to 5 times while "bar=false" = "[3,5?bar=false]"
                  Do item 1 or more times = "[1+]"
                  Do item 1 or more times while "Bob is polite" = "[1+?Bob is polite]"
                  Do item up to 4 times only while "zoo=0" = "[<4?zoo=0]"
               Optional item = "[?]"
               Checking off items
                  Unchecked item = "[_]"
                  Checked item = "[x]" 
                     [?] Timestamp after ("[x] YYYY-MM-DD hh.mm:")
            Tag
               Any attribute to the #Content, ending in a colon
                  Time stamp = "YYYY-MM-DD hh.mm.ss:"
                     Shorten the format to the appropriate granularity
                  Time relations
                     Time relative to previous item = "+YYYY-MM-DD:"
                     Less than a certain time after previous item = "<+YYYY-MM-DD"
                     More than a certain time after previous item = ">+YYYY-MM-DD"
                     Time relative to next item = "-YYYY-MM-DD"
                     Less than a certain time before next item = "<-YYYY-MM-DD"
                     More than a certain time before next item = ">-YYYY-MM-DD"
                     Time relative to other item = "+YYYY-MM-DD#item:"
                     Other obvious time indicators may be used; EXAMPLES: 
                        "+1 week:"
                        "-2 Marsian years:"
                     EXAMPLES: 
                        Wait one month before doing the item = "+YYYY-01-DD:"
                        Do item less than 4 days before next item = "<-YYYY-MM-04:"
                        Wait one year and two days after item X = "+0001-00-02#X:"
                  Time repetition
                     Obvious/intuitive repetition
                        EXAMPLES: 
                           "YYYY-MM-03" = Every third day of the month
                           "YYYY-12-DD" = Every day in every December
                           "2011-MM-05" = Every fifth day of every month of 2011
                           "Tue,Fri 12.00" = Noon every Tuesday and Friday
                     Strict convention
                        Format = YYYY-MM-DD+X Day hh.mm+Y - YYYY-MM-DD hh.mm; EXAMPLES: 
                           "2011-05-01+7 13.00" = 2011-05-01 1pm, repeated every 7 days
                           "2011-05-01+2,3,2" = Every 2, then 3, then 2 days, etc
                           "2011-05-01+2 - 2012-05-01" = Every second day for one year
                           "2011-05-01 13.00+1" = 2011-05-01 1pm, repeated every hour
                           "2011-05-01 Fri,Sat - 2011-10-01" = Fri & Sat in interval
                  Any other tag
                     EXAMPLES: "Location = Someplace:", "Responsible = Someone:"
            Description
               The main body of the WOIM item, the "meat" of the line
         [?] Additive
            Reference
               + A "#" followed by the name of an item, list or anything else
                EXAMPLE: Reference to a website = #http://www.isene.com/
               + A Reference with spaces is enclosed in single quotation marks
                (#'This is a reference' as opposed to #Reference)
               + There are two types of references; OR: 
                  Redirection (hard Reference)
                     An item consisting only of an reference is a redirection
                        For a transition item = Jump to reference, continue execution
                           + If the redirect is to jump back after executing 
                            the reference (and its children), then add another 
                            hash at the beginning (##ref)
                              + EXAMPLE: Use this when creating subroutines at 
                               the end of the list
                        For a state item = Include the referenced item
                     An item consisting only of the key word "SKIP" 
                        ends the current WOIM level
                     An item consisting only of the key word "END" 
                        ends the whole WOIM list
                  Soft reference
                     Reference is part of an item
                        Look at referred item for info only
                     Even softer reference = have the reference in "()"
                        An apropos
               + A reference to any line upward in the WOIM list is simply a
                reference to the items #Content
               + A reference downward needs a "/" to separate each level (to
                make a "path" (like a URL) to the item
                  EXAMPLE: Reference from here to #Element/Starter/Identifier
               + For long items in a reference, concatenation can be used
                  The concatenation must be unique
                  EXAMPLE: Reference from here to #Comment/Anything...
            Comment
               Anything within parenthesis is a comment
               Is not executed as WOIM commands
            Quote
               Anything in quotation marks is a quote
               Is not executed as WOIM commands
            Change Markup; OR: 
               Deletion
                  Remove the item by adding "<<" at the end of the item
               Motion; OPTIONS: 
                  Move the item by adding ">>#Ref"
                     This moves the item just below the referenced item
                  Move the item one level in by adding "<-" at the end of the item
                  Move the item one level out by adding "->" at the end of the item
               EXAMPLE: Move an item as a child to Reference = ">>#Ref->"
      Separator 
         OR: 
            Semicolon
               A semicolon is used to separate two WOIM items on the same line
            Newline
               Used to add another item on the same level
               Indent
                  A Tab or an Asterisk ("*")
                     Used to add a child
                        A child adds information to its parent
                        A child is another regular #'WOIM item'
         Definition
            A separator and no indent reads "then:"
            [? parent contains #Description] 
               The separator and right indent reads "consists of:"
            [? NOT: parent contains #Description] 
               The separator and right indent reads "applies to:"

Read and re-read the WOIM list above, and you will be a WOIM master after a
while. It's all there. The whole markup.

==============================================================================
4. About                                                          *WOIM-About*

The author of the VIM plugin for WOIM is also the chief maintainer of WOIM
itself; Geir Isene. More at http//isene.com 

==============================================================================
5. Changelog                                                  *WOIM-Changelog*

VERSION 1.4.5		2011-05-30

	Overhaul:
	Created the documentation, includin the whole WOIM definition to make it
	easily accessible within VIM.  Added the INSTALL and README files, zipped and
	ready for easy installation.

VERSION 1.4.2   2011-05-29

	Fixed "gr" (Goto Ref) for references with single quotes ('')

	Added the search pattern from "gr" to the search register so that "n" can
	successively be used to test if the referenced destination is unique (which
	it should be).

VERSION 1.4.1   2011-05-27

	New feature:
	Goto Reference: With the cursor at a WOIM reference, press "gr" to jump to
	that reference in the WOIM list.

	Now you can navigate more easily in WOIM lists.

VERSION 1.4	    2011-05-23

	This could have been the long awaited version 1.0 - but instead I decided to
	synchronize the version numbering of this VIM plugin and the WOIM description
	itself. From now on the releases will be synchronized, with minor fixes in
	the VIM plugin released as minor releases (i.e. a fix to 1.4 would be 1.4.1
	and would still be on par with the WOIM definition version 1.4). The version
	1.4 of the WOIM definition adds time repetition as well as "checking" of
	todo-list items.

	This release:

	New feature: Added the option of checkboxes for items (Thanks to Christopher
	Truett (VIM script #3584). You can now easily add a checkbox in front of any
	item by <leader>v and subsequently toggle that checkbox via the same
	(<leader>v) or <leader>V if you want to add a time stamp to a box that you
	"check" (this also toggles the timestamp if you "uncheck" the item).

	Fixes: Some minor cleanup.

VERSION 0.9.9   2011-04-17

	Fix: Fixed interference between Operators and Tags

VERSION 0.9.8   2010-12-14

	Feature: States (S:) is underlined by default.  <leader>s removes the
	underlining, while <leader>S turns on underlining of States.  Transitions are
	not underlined by default.  <leader>T turns on underlining, while <leader>t
	removes the underlining of Transitions.
	
	Fix: Removed unnecessary "contained" to make lists syntax marked even within
	stub lists.
	
	Fix: Small fixes in grouping and containing of elements.

VERSION 0.9.6   2010-12-03

	Feature: Added "*" as possible indentation

	Fix:         Changed Multiline indicator from "*" to "+"

	Now compatible with WOIM v. 1.2

VERSION 0.9.3   2009-12-11

	Christian Bryn caught an important bug/lacking setting. The syntax file
	now sets noexpandtab.

VERSION 0.9.2   2009-10-25

	A few needed minor fixes.

VERSION 0.9.1   2009-08-21

	New_feature:
	Added highlighting of item motions:
	<<          means "delete this item" (put at the end of a line)
	>>#1.1. means "move this item to after item 1.1."
	->           means "indent item right"
	<-<-       means "indent item two left"
	>>#1.-> means "move item to after item 1. and indent right"

VERSION 0.9	    2009-08-10

	New_feature: Accommodated for the use of subroutine calls (##ref)
	Fix: Cleaned up syntax variable names to fit modern WOIM
	Fix: Multi-lines have consecutive lines start with a <space>
	Fix: Quotes or Comment can now span several lines
	Fix: Comments allowed in Operators
	Fix: Comments and references allowed inside Qualifiers
	Fix: Identifier must end in a period and then a space
	Bug_fix: Allowing a period to be part of a tag
	Bug_fix: Fixed wrong markup for astrices not used for multi-line

VERSION 0.8.6   2009-07-24

	Bug fix: Corrected attributes ending in capitals that was treated as a WOIMkey
	Bug fix: Fixed references containing a hyphen

VERSION 0.8.5   2009-07-23

	New feature: Expanded Attributes to include relative times and
	greater/smaller than.

	New feature: References with spaces are now accommodated for by putting it in
	quotes.

	New feature: Made references in attributes possible.
	
	Bug fix: Fixed references that includes ampersands ("&").

VERSION 0.8.1   2009-07-22

	Bug fix: Fixed highlighting of attributes with a colon (like time stamps)

VERSION 0.8	    2009-07-21

	New feature: Expanded folding to a maximum of 15 levels with folding levels
	set with <leader>a to <leader>f for levels 10 to 15.  Improvement: Better
	syntax highlighting for indexes

	Bug fix: Fixed syntax syncing when entering the document in the first place

VERSION 0.7.2   2009-07-15

	Better syntax highlighting for references.
	Added unobtrusive highlighting of vim bottom set-lines.

VERSION 0.7.1   2009-06-16

	Bug fix: Fixed an error in syntax highlighting properties containing a "-"
	(like ISO dates).

VERSION 0.7	    2009-06-13

	Added macro to jump to next point in a template and fill in the value. A
	template item is an item that ends in an equal sign and where the value after
	the equal sign is to be filled out. Example:

	ExampleTask
		 Task name =
		 Responsible person =
		 Deadline =

VERSION 0.6	    2009-04-13

	Some minor adjustments, but a needed upgrade if you are a WOIM user.

VERSION 0.4	    2009-01-16

	Initial upload

==============================================================================
6. Credits                                                      *WOIM-Credits*

Thanks to Jean-Dominique Warnier and Kenneth Orr for the original idea of this
type of markup.

Thanks to Egil Möller for helping to cultivate the first versions.

Thanks to Axel Liljencrantz for his input in outlining WOIM.

Thanks to Christian Bryn for testing of the plugin.

Thanks to Christopher Truett for his Checkbox VIM plugin.

==============================================================================
7. License                                                      *WOIM-License*

I release all copyright claims. This code is in the public domain.  Permission
is granted to use, copy modify, distribute, and sell this software for any
purpose. I make no guarantee about the suitability of this software for any
purpose and I am not liable for any damages resulting from its use. Further, I
am under no obligation to maintain or extend this software. It is provided on
an 'as is' basis without any expressed or implied warranty.
INSTALL_WOIM	[[[1
34
== Install instructions for the VIM plugin for WOIM ==

Unzip the file woim.zip into you VIM directory.

The files INSTALL_WOIM and README_WOIM will appear in the VIM directory,
while the documentation will be placed in the "doc" subdirectory and most
importantly, the WOIM pluging will be placed in teh "syntax" subdirectory.

Add to your .vimrc file or create a file named "woim.vim" with the
following and drop that file in the "ftdetect" directory:

  au BufRead,BufNewFile *.woim set filetype=woim

To use WOIM lists within other file types (other than ".woim"), add the
following to those syntax files:

  syn include @WOIM ~/.vim/syntax/woim.vim
  syn region WoimSnip matchgroup=Snip start="WOIMstart" end="WOIMend" contains=@WOIM
  hi link Snip SpecialComment


The documentation file contains all of the WOIM definition as found here:

  http://isene.com/woim.pdf


LICENSE
I release all copyright claims. This code is in the public domain.
Permission is granted to use, copy modify, distribute, and sell this
software for any purpose. I make no guarantee about the suitability of
this software for any purpose and I am not liable for any damages
resulting from its use. Further, I am under no obligation to maintain or
extend this software. It is provided on an 'as is' basis without any
expressed or implied warranty.
README_WOIM	[[[1
52
== General instructions for the VIM plugin for WOIM ==

WOIM is a methodology to describe anything - any state, item(s), pattern,
action, process, transition, program, instruction set etc. So, you can use
it as an outliner, a ToDo list handler, a process design tool, a data
modeler, or any other way you want to describe something.

This plugin does both highlighting and various automatic handling of WOIM
lists, like collapsing lists or parts of lists in a sophisticated way.

For information on WOIM, read the WOIM description in full:

  http://isene.com/woim.pdf


INSTRUCTIONS

Use tabs/shifts or * for indentations

Use <SPACE> to toggle one fold
Use \0 to \9, \a, \b, \c, \d, \e, \f to show up to 15 levels expanded

Use <leader>s to remove underlining of States (prefixed with S:)
Use <leader>S to add underlining of States (prefixed with S:)
Use <leader>t to remove underlining of Transitions (prefixed with T:)
Use <leader>T to add underlining of Transitions (prefixed with T:)

Use <leader>v to add a checkbox at start of item or to toggle a checkbox
Use <leader>V to add/toggle a checkbox with a date stamp for completion

Use "gr" (without the quotation marks, signifies "Goto Ref") while the
cursor is on a WOIM reference to jump to that destination in a WOIM list

Use <leader><SPACE> to go to the next open template element
(A template element is a WOIM item ending in an equal sign)

Syntax updated at start and every time you leave Insert mode 

For this help and more, including the full WOIM definition/description, type 

  :help woim


LICENSE
I release all copyright claims. This code is in the public domain.
Permission is granted to use, copy modify, distribute, and sell this
software for any purpose. I make no guarantee about the suitability of
this software for any purpose and I am not liable for any damages
resulting from its use. Further, I am under no obligation to maintain or
extend this software. It is provided on an 'as is' basis without any
expressed or implied warranty.

