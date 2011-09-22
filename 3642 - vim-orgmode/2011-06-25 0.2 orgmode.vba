" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftdetect/org.vim	[[[1
1
autocmd BufNewFile,BufRead *.org set filetype=org
syntax/org.vim	[[[1
237
" Variables With Default Settings:
"
" Define the highlighting colors/group names for headings
" let g:org_heading_highlight_colors = ['Title', 'Constant', 'Identifier', 'Statement', 'PreProc', 'Type', 'Special']
"
" Definie the number of level of highlighting. If this number is bigger than
" the length of g:org_heading_highlight_colors the colors of
" g:org_heading_highlight_colors are repeated
" let g:org_heading_highlight_levels = len(g:org_heading_highlight_colors)
"
" Defines if leading stars are displayed in the color of the heading or if a
" special NonText highlighting is used that hides them from user
" let g:org_heading_shade_leading_stars = 1
"
" Defines the keywords that are highlighted in headings. For more information
" about this variable, please consult the org-mode documentation
" (http://orgmode.org/org.html#index-org_002dtodo_002dkeywords-511)
" let g:org_todo_keywords = ['TODO', '|', 'DONE']
"
" Defines special faces (styles) for displaying g:org_todo_keywords. Please
" refer to vim documentation (topic |attr-list|) for allowed values for
" :weight, :slant, :decoration. Muliple colors can be separated by comma for
" :foreground and :background faces to provide different colors for gui and
" terminal mode.
" let g:org_todo_keyword_faces = []
"
" Examples:
"
" Define an additionaly keyword 'WAITING' and set the foreground color to
" 'cyan'. Define another keyword 'CANCELED' and set the foreground color to
" red, background to black and the weight to normal, slant to italc and
" decoration to underline
" let g:org_todo_keywords = [['TODO', 'WAITING', '|', 'DONE'], ['|', 'CANCELED']]
" let g:org_todo_keyword_faces = [['WAITING', 'cyan'], ['CANCELED', [':foreground red', ':background black', ':weight bold', ':slant italic', ':decoration underline']]]

" Headings
if !exists('g:org_heading_highlight_colors')
	let g:org_heading_highlight_colors = ['Title', 'Constant', 'Identifier', 'Statement', 'PreProc', 'Type', 'Special']
endif

if !exists('g:org_heading_highlight_levels')
	let g:org_heading_highlight_levels = len(g:org_heading_highlight_colors)
endif

if !exists('g:org_heading_shade_leading_stars')
	let g:org_heading_shade_leading_stars = 1
endif

unlet! s:i s:j s:contains
let s:i = 1
let s:j = len(g:org_heading_highlight_colors)
let s:contains = ' contains=org_timestamp,org_timestamp_inactive'
if g:org_heading_shade_leading_stars == 1
	let s:contains = s:contains . ',org_shade_stars'
	syntax match org_shade_stars /^\*\{2,\}/me=e-1 contained
	hi def link org_shade_stars NonText
else
	hi clear org_shade_stars
endif

while s:i <= g:org_heading_highlight_levels
	exec 'syntax match org_heading' . s:i . ' /^\*\{' . s:i . '\}\s.*/' . s:contains
	exec 'hi def link org_heading' . s:i . ' ' . g:org_heading_highlight_colors[(s:i - 1) % s:j]
	let s:i += 1
endwhile
unlet! s:i s:j s:contains

" Todo keywords
if !exists('g:org_todo_keywords')
	let g:org_todo_keywords = ['TODO', '|', 'DONE']
endif

if !exists('g:org_todo_keyword_faces')
	let g:org_todo_keyword_faces = []
endif

let s:todo_headings = ''
let s:i = 1
while s:i <= g:org_heading_highlight_levels
	if s:todo_headings == ''
		let s:todo_headings = 'containedin=org_heading' . s:i
	else
		let s:todo_headings = s:todo_headings . ',org_heading' . s:i
	endif
	let s:i += 1
endwhile
unlet! s:i

if !exists('g:loaded_org_syntax')
	let g:loaded_org_syntax = 1

	function! s:ExtendHighlightingGroup(base_group, new_group, settings)
		let l:base_hi = ''
		redir => l:base_hi
		silent execute 'highlight ' . a:base_group
		redir END
		let l:group_hi = substitute(split(l:base_hi, '\n')[0], '^' . a:base_group . '\s\+xxx', '', '')
		execute 'highlight ' . a:new_group . l:group_hi . ' ' . a:settings
	endfunction

	function! s:InterpretFaces(faces)
		let l:res_faces = ''
		if type(a:faces) == 3
			let l:style = []
			for l:f in a:faces
				let l:_f = [l:f]
				if type(l:f) == 3
					let l:_f = l:f
				endif
				for l:g in l:_f
					if type(l:g) == 1 && l:g =~ '^:'
						if l:g !~ '[\t ]'
							continue
						endif
						let l:k_v = split(l:g)
						if l:k_v[0] == ':foreground'
							let l:gui_color = ''
							let l:found_gui_color = 0
							for l:color in split(l:k_v[1], ',')
								if l:color =~ '^#'
									let l:found_gui_color = 1
									let l:res_faces = l:res_faces . ' guifg=' . l:color
								elseif l:color != ''
									let l:gui_color = l:color
									let l:res_faces = l:res_faces . ' ctermfg=' . l:color
								endif
							endfor
							if ! l:found_gui_color && l:gui_color != ''
								let l:res_faces = l:res_faces . ' guifg=' . l:gui_color
							endif
						elseif l:k_v[0] == ':background'
							let l:gui_color = ''
							let l:found_gui_color = 0
							for l:color in split(l:k_v[1], ',')
								if l:color =~ '^#'
									let l:found_gui_color = 1
									let l:res_faces = l:res_faces . ' guibg=' . l:color
								elseif l:color != ''
									let l:gui_color = l:color
									let l:res_faces = l:res_faces . ' ctermbg=' . l:color
								endif
							endfor
							if ! l:found_gui_color && l:gui_color != ''
								let l:res_faces = l:res_faces . ' guibg=' . l:gui_color
							endif
						elseif l:k_v[0] == ':weight' || l:k_v[0] == ':slant' || l:k_v[0] == ':decoration'
							if index(l:style, l:k_v[1]) == -1
								call add(l:style, l:k_v[1])
							endif
						endif
					elseif type(l:g) == 1
						" TODO emacs interprets the color and automatically determines
						" whether it should be set as foreground or background color
						let l:res_faces = l:res_faces . ' ctermfg=' . l:k_v[1] . ' guifg=' . l:k_v[1]
					endif
				endfor
			endfor
			let l:s = ''
			for l:i in l:style
				if l:s == ''
					let l:s = l:i
				else
					let l:s = l:s . ','. l:i
				endif
			endfor
			if l:s != ''
				let l:res_faces = l:res_faces . ' term=' . l:s . ' cterm=' . l:s . ' gui=' . l:s
			endif
		elseif type(a:faces) == 1
			" TODO emacs interprets the color and automatically determines
			" whether it should be set as foreground or background color
			let l:res_faces = l:res_faces . ' ctermfg=' . a:faces . ' guifg=' . a:faces
		endif
		return l:res_faces
	endfunction

	function! s:ReadTodoKeywords(keywords, todo_headings)
		let l:default_group = 'Todo'
		for l:i in a:keywords
			if type(l:i) == 3
				call s:ReadTodoKeywords(l:i, a:todo_headings)
				continue
			endif
			if l:i == '|'
				let l:default_group = 'Question'
				continue
			endif
			let l:group = l:default_group
			for l:j in g:org_todo_keyword_faces
				if l:j[0] == l:i
					let l:group = 'org_todo_keyword_face_' . l:i
					call s:ExtendHighlightingGroup(l:default_group, l:group, s:InterpretFaces(l:j[1]))
					break
				endif
			endfor
			exec 'syntax match org_todo_keyword_' . l:i . ' /\*\{1,\}\s\{1,\}\zs' . l:i .'/ ' . a:todo_headings
			exec 'hi def link org_todo_keyword_' . l:i . ' ' . l:group
		endfor
	endfunction
endif

call s:ReadTodoKeywords(g:org_todo_keywords, s:todo_headings)
unlet! s:todo_headings

" Propteries
syn region Error matchgroup=org_properties_delimiter start=/^\s*:PROPERTIES:\s*$/ end=/^\s*:END:\s*$/ contains=org_property keepend
syn match org_property /^\s*:[^\t :]\+:\s\+[^\t ]/ contained contains=org_property_value
syn match org_property_value /:\s\zs.*/ contained
hi def link org_properties_delimiter PreProc
hi def link org_property Statement
hi def link org_property_value Constant

" Timestamps
syn match org_timestamp /\(<\d\{4\}-\d\{2\}-\d\{2\} .\+>\|<\d\{4\}-\d\{2\}-\d\{2\} .\+>--<\d\{4\}-\d\{2\}-\d\{2\} .\+>\|<%%(diary-float.\+>\)/
syn match org_timestamp_inactive /\(\[\d\{4\}-\d\{2\}-\d\{2\} .\+\]\|\[\d\{4\}-\d\{2\}-\d\{2\} .\+\]--\[\d\{4\}-\d\{2\}-\d\{2\} .\+\]\|\[%%(diary-float.\+\]\)/
hi def link org_timestamp PreProc
hi def link org_timestamp_inactive Comment

" Deadline/Schedule
syn match org_deadline_scheduled /^\s*\(DEADLINE\|SCHEDULED\):/
hi def link org_deadline_scheduled PreProc

" Tables
syn match org_table /^\s*|.*/ contains=org_timestamp,org_timestamp_inactive,hyperlink,org_table_separator,org_table_horizontal_line
syn match org_table_separator /\(^\s*|[-+]\+|\?\||\)/ contained
hi def link org_table_separator Type

" Hyperlinks
syntax match hyperlink	"\[\{2}[^][]*\(\]\[[^][]*\)\?\]\{2}" contains=hyperlinkBracketsLeft,hyperlinkURL,hyperlinkBracketsRight containedin=ALL
syntax match hyperlinkBracketsLeft		contained "\[\{2}" conceal
syntax match hyperlinkURL				contained "[^][]*\]\[" conceal
syntax match hyperlinkBracketsRight		contained "\]\{2}" conceal
hi def link hyperlink Underlined

" Comments
syntax match org_comment /^#.*/
hi def link org_comment Comment
ftplugin/org.vim	[[[1
105
" org.vim -- An attempt to port org-mode to vim
" @Author       : Jan Christoph Ebersbach (jceb@e-jc.de)
" @License      : AGPL3 (see http://www.gnu.org/licenses/agpl.txt)
" @Created      : 2010-10-03
" @Last Modified: Sat 25. Jun 2011 17:39:50 +0200 CEST
" @Revision     : 0.2
" @vi           : ft=vim:tw=80:sw=4:ts=4

if ! exists("b:did_ftplugin")
	" default emacs settings
	setlocal comments-=s1:/*,mb:*,ex:*/ cole=2 cocu=nc tabstop=8 shiftwidth=8 commentstring=#\ %s

	" register keybindings if they don't have been registered before
	if has('python') && exists("g:loaded_org")
		python ORGMODE.register_keybindings()
	endif
endif

" load plugin just once
if &cp || exists("g:loaded_org")
    finish
endif
let g:loaded_org = 1

" display error message if python is not available
if ! has('python')
	echom 'Python not found, orgmode plugin is not usable.'
	finish
endif

" general setting plugins that should be loaded and their order
if ! exists('g:org_plugins') && ! exists('b:org_plugins')
	let g:org_plugins = ['ShowHide', '|', 'Navigator', 'EditStructure', '|', 'Hyperlinks', '|', 'Todo', 'TagsProperties', 'Date', 'Misc']
endif

if ! exists('g:org_syntax_highlight_leading_stars') && ! exists('b:org_syntax_highlight_leading_stars')
	let g:org_syntax_highlight_leading_stars = 1
endif


" make sure repeat plugin is load (or not)
try
	call repeat#set()
catch
endtry

function! <SID>OrgRegisterMenu()
	python ORGMODE.register_menu()
endfunction

function! <SID>OrgUnregisterMenu()
	python ORGMODE.unregister_menu()
endfunction

function! <SID>OrgDeleteUnusedDocument(bufnr)
python << EOF
b = int(vim.eval('a:bufnr'))
if b in ORGMODE._documents:
	del ORGMODE._documents[b]
EOF
endfunction

" show and hide Org menu depending on the filetype
augroup orgmode
	au BufEnter		*		:if &filetype == "org" | call <SID>OrgRegisterMenu() | endif
	au BufLeave		*		:if &filetype == "org" | call <SID>OrgUnregisterMenu() | endif
	au BufDelete	*		:call <SID>OrgDeleteUnusedDocument(expand('<abuf>'))
augroup END

" Expand our path
python << EOF
import vim, os, sys

for p in vim.eval("&runtimepath").split(','):
   dname = os.path.join(p, "ftplugin")
   if os.path.exists(os.path.join(dname, "orgmode")):
      if dname not in sys.path:
         sys.path.append(dname)
      break

from orgmode import ORGMODE
ORGMODE.start()
EOF

" ******************** Taglist/Tagbar integration ********************

" tag-bar support for org-mode
let g:tagbar_type_org = {
			\ 'ctagstype' : 'org',
			\ 'kinds'     : [
				\ 's:sections',
				\ 'h:hyperlinks',
			\ ],
			\ 'sort'    : 0,
			\ 'deffile' : expand('<sfile>:p:h') . '/org.cnf'
			\ }

" taglist support for org-mode
if !exists('g:Tlist_Ctags_Cmd')
	finish
endif

" Pass parameters to taglist
let g:tlist_org_settings = 'org;s:section;h:hyperlinks'
let g:Tlist_Ctags_Cmd .= ' --options=' . expand('<sfile>:p:h') . '/org.cnf '
ftplugin/orgmode/plugins/Hyperlinks.py	[[[1
167
# -*- coding: utf-8 -*-

from orgmode import echom, ORGMODE, realign_tags
from orgmode.menu import Submenu, Separator, ActionEntry
from orgmode.keybinding import Keybinding, Plug, Command

import vim
import re

class Hyperlinks(object):
	u""" Hyperlinks plugin """

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'Hyperlinks')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

		# commands for this plugin
		self.commands = []

	uri_match = re.compile(r'^\[{2}(?P<uri>[^][]*)(\]\[(?P<description>[^][]*))?\]{2}')

	@classmethod
	def _get_link(cls, cursor=None):
		u"""
		Get the link the cursor is on and return it's URI and description

		:cursor: None or (Line, Column)
		:returns: None if no link was found, otherwise {uri:URI, description:DESCRIPTION, line:LINE, start:START, end:END} or uri and description could be None if not set
		"""
		cursor = cursor if cursor else vim.current.window.cursor
		line = vim.current.buffer[cursor[0] - 1].decode(u'utf-8')

		# if the cursor is on the last bracket, it's not recognized as a hyperlink
		start = line.rfind(u'[[', 0, cursor[1])
		if start == -1:
			start = line.rfind(u'[[', 0, cursor[1] + 2)
		end = line.find(u']]', cursor[1])
		if end == -1:
			end = line.find(u']]', cursor[1] - 1)

		# extract link
		if start != -1 and end != -1:
			end += 2
			match = Hyperlinks.uri_match.match(line[start:end])

			res = {u'line':line, u'start':start, u'end':end, u'uri':None, u'description':None}
			if match:
				res.update(match.groupdict())
			return res

	@classmethod
	def follow(cls, action=u'openLink', visual=u''):
		u""" Follow hyperlink. If called on a regular string UTL determines the
		outcome. Normally a file with that name will be opened.

		:action: "copy" if the link should be copied to clipboard, otherwise the link will be opened
		:visual: "visual" if Universal Text Linking should be triggered in visual mode

		:returns: URI or None
		"""
		if not int(vim.eval(u'exists(":Utl")')):
			echom(u'Universal Text Linking plugin not installed, unable to proceed.')
			return

		action = u'copyLink' if action and action.startswith(u'copy') else u'openLink'
		visual = u'visual' if visual and visual.startswith(u'visual') else u''

		link = Hyperlinks._get_link()

		if link and link[u'uri'] is not None:
			# call UTL with the URI
			vim.command((u'Utl %s %s %s' % (action, visual, link[u'uri'])).encode(u'utf-8'))
			return link[u'uri']
		else:
			# call UTL and let it decide what to do
			vim.command((u'Utl %s %s' % (action, visual)).encode(u'utf-8'))

	@classmethod
	@realign_tags
	def insert(cls, uri=None, description=None):
		u""" Inserts a hyperlink. If no arguments are provided, an interactive
		query will be started.

		:uri: The URI that will be opened
		:description: An optional description that will be displayed instead of the URI

		:returns: (URI, description)
	    """
		link = Hyperlinks._get_link()
		if link:
			if uri is None and link[u'uri'] is not None:
				uri = link[u'uri']
			if description is None and link[u'description'] is not None:
				description = link[u'description']

		if uri is None:
			uri = vim.eval(u'input("Link: ")').decode(u'utf-8')
		elif link:
			uri = vim.eval(u'input("Link: ", "%s")' % link[u'uri']).decode(u'utf-8')
		if uri is None:
			return

		if description is None:
			description = vim.eval(u'input("Description: ")').decode(u'utf-8')
		elif link:
			description = vim.eval(u'input("Description: ", "%s")' % link[u'description']).decode(u'utf-8')
		if description is None:
			return

		cursor = vim.current.window.cursor
		cl = vim.current.buffer[cursor[0] - 1].decode(u'utf-8')
		head = cl[:cursor[1] + 1] if not link else cl[:link[u'start']]
		tail = cl[cursor[1] + 1:] if not link else cl[link[u'end']:]

		separator = u''
		if description:
			separator = u']['

		if uri or description:
			vim.current.buffer[cursor[0] - 1] = (u''.join((head, u'[[%s%s%s]]' % (uri, separator, description), tail))).encode(u'utf-8')
		elif link:
			vim.current.buffer[cursor[0] - 1] = (u''.join((head, tail))).encode(u'utf-8')

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		self.commands.append(Command(u'OrgHyperlinkFollow', u':py ORGMODE.plugins[u"Hyperlinks"].follow()'))
		self.keybindings.append(Keybinding(u'gl', Plug(u'OrgHyperlinkFollow', self.commands[-1])))
		self.menu + ActionEntry(u'&Follow Link', self.keybindings[-1])

		self.commands.append(Command(u'OrgHyperlinkCopy', u':py ORGMODE.plugins[u"Hyperlinks"].follow(action=u"copy")'))
		self.keybindings.append(Keybinding(u'gyl', Plug(u'OrgHyperlinkCopy', self.commands[-1])))
		self.menu + ActionEntry(u'&Copy Link', self.keybindings[-1])

		self.commands.append(Command(u'OrgHyperlinkInsert', u':py ORGMODE.plugins[u"Hyperlinks"].insert(<f-args>)', arguments=u'*'))
		self.keybindings.append(Keybinding(u'gil', Plug(u'OrgHyperlinkInsert', self.commands[-1])))
		self.menu + ActionEntry(u'&Insert Link', self.keybindings[-1])

		self.menu + Separator()

		# find next link
		self.commands.append(Command(u'OrgHyperlinkNextLink', u":if search('\[\{2}\zs[^][]*\(\]\[[^][]*\)\?\ze\]\{2}', 's') == 0 | echo 'No further link found.' | endif"))
		self.keybindings.append(Keybinding(u'gn', Plug(u'OrgHyperlinkNextLink', self.commands[-1])))
		self.menu + ActionEntry(u'&Next Link', self.keybindings[-1])

		# find previous link
		self.commands.append(Command(u'OrgHyperlinkPreviousLink', u":if search('\[\{2}\zs[^][]*\(\]\[[^][]*\)\?\ze\]\{2}', 'bs') == 0 | echo 'No further link found.' | endif"))
		self.keybindings.append(Keybinding(u'go', Plug(u'OrgHyperlinkPreviousLink', self.commands[-1])))
		self.menu + ActionEntry(u'&Previous Link', self.keybindings[-1])

		self.menu + Separator()

		# Descriptive Links
		self.commands.append(Command(u'OrgHyperlinkDescriptiveLinks', u':setlocal cole=2'))
		self.menu + ActionEntry(u'&Descriptive Links', self.commands[-1])

		# Literal Links
		self.commands.append(Command(u'OrgHyperlinkLiteralLinks', u':setlocal cole=0'))
		self.menu + ActionEntry(u'&Literal Links', self.commands[-1])
ftplugin/orgmode/plugins/Misc.py	[[[1
170
# -*- coding: utf-8 -*-

from orgmode import ORGMODE, apply_count
from orgmode.menu import Submenu
from orgmode.keybinding import Keybinding, Plug, MODE_VISUAL, MODE_OPERATOR

import vim

class Misc(object):
	u""" Miscellaneous functionality """

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'Misc')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

	@classmethod
	def jump_to_first_character(cls):
		heading = ORGMODE.get_document().current_heading()
		if not heading:
			vim.eval(u'feedkeys("^", "n")'.encode(u'utf-8'))
			return

		vim.current.window.cursor = (vim.current.window.cursor[0], heading.level + 1)

	@classmethod
	def edit_at_first_character(cls):
		heading = ORGMODE.get_document().current_heading()
		if not heading or heading.start_vim != vim.current.window.cursor[0]:
			vim.eval(u'feedkeys("I", "n")'.encode(u'utf-8'))
			return

		vim.current.window.cursor = (vim.current.window.cursor[0], heading.level + 1)
		vim.command(u'startinsert'.encode(u'utf-8'))

	#@repeat
	@classmethod
	@apply_count
	def i_heading(cls, mode=u'visual', selection=u'inner', skip_children=False):
		u"""
		inner heading text object
		"""
		heading = ORGMODE.get_document().current_heading()
		if heading:
			if selection != u'inner':
				heading = heading if not heading.parent else heading.parent

			line_start, col_start = [ int(i) for i in vim.eval(u'getpos("\'<")'.encode(u'utf-8'))[1:3] ]
			line_end, col_end = [ int(i) for i in vim.eval(u'getpos("\'>")'.encode(u'utf-8'))[1:3] ]

			if mode != u'visual':
				line_start = vim.current.window.cursor[0]
				line_end = line_start

			start = line_start
			end = line_end
			move_one_character_back = u'' if mode == u'visual' else u'h'

			if heading.start_vim < line_start:
				start = heading.start_vim
			if heading.end_vim > line_end and not skip_children:
				end = heading.end_vim
			elif heading.end_of_last_child_vim > line_end and skip_children:
				end = heading.end_of_last_child_vim

			if mode != u'visual' and not vim.current.buffer[end - 1]:
				end -= 1
				move_one_character_back = u''

			swap_cursor = u'o' if vim.current.window.cursor[0] == line_start else u''

			if selection == u'inner' and vim.current.window.cursor[0] != line_start:
				h = ORGMODE.get_document().current_heading()
				if h:
					heading = h

			visualmode = vim.eval(u'visualmode()').decode(u'utf-8') if mode == u'visual' else u'v'

			if line_start == start and line_start != heading.start_vim:
				if col_start in (0, 1):
					vim.command((u'normal! %dgg0%s%dgg$%s%s' % \
							(start, visualmode, end, move_one_character_back, swap_cursor)).encode(u'utf-8'))
				else:
					vim.command((u'normal! %dgg0%dl%s%dgg$%s%s' % \
							(start, col_start - 1, visualmode, end, move_one_character_back, swap_cursor)).encode(u'utf-8'))
			else:
				vim.command((u'normal! %dgg0%dl%s%dgg$%s%s' % \
						(start, heading.level + 1, visualmode, end, move_one_character_back, swap_cursor)).encode(u'utf-8'))

			if selection == u'inner':
				if mode == u'visual':
					return u'OrgInnerHeadingVisual' if not skip_children else u'OrgInnerTreeVisual'
				else:
					return u'OrgInnerHeadingOperator' if not skip_children else u'OrgInnerTreeOperator'
			else:
				if mode == u'visual':
					return u'OrgOuterHeadingVisual' if not skip_children else u'OrgOuterTreeVisual'
				else:
					return u'OrgOuterHeadingOperator' if not skip_children else u'OrgOuterTreeOperator'
		elif mode == u'visual':
			vim.command(u'normal! gv'.encode(u'utf-8'))

	#@repeat
	@classmethod
	@apply_count
	def a_heading(cls, selection=u'inner', skip_children=False):
		u"""
		a heading text object
		"""
		heading = ORGMODE.get_document().current_heading()
		if heading:
			if selection != u'inner':
				heading = heading if not heading.parent else heading.parent

			line_start, col_start = [ int(i) for i in vim.eval(u'getpos("\'<")'.encode(u'utf-8'))[1:3] ]
			line_end, col_end = [ int(i) for i in vim.eval(u'getpos("\'>")'.encode(u'utf-8'))[1:3] ]

			start = line_start
			end = line_end

			if heading.start_vim < line_start:
				start = heading.start_vim
			if heading.end_vim > line_end and not skip_children:
				end = heading.end_vim
			elif heading.end_of_last_child_vim > line_end and skip_children:
				end = heading.end_of_last_child_vim

			swap_cursor = u'o' if vim.current.window.cursor[0] == line_start else u''

			vim.command((u'normal! %dgg%s%dgg$%s' % \
					(start, vim.eval(u'visualmode()'.encode(u'utf-8')), end, swap_cursor)).encode(u'utf-8'))
			if selection == u'inner':
				return u'OrgAInnerHeadingVisual' if not skip_children else u'OrgAInnerTreeVisual'
			else:
				return u'OrgAOuterHeadingVisual' if not skip_children else u'OrgAOuterTreeVisual'
		else:
			vim.command(u'normal! gv'.encode(u'utf-8'))

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		self.keybindings.append(Keybinding(u'^', Plug(u'OrgJumpToFirstCharacter', u':py ORGMODE.plugins[u"Misc"].jump_to_first_character()<CR>')))
		self.keybindings.append(Keybinding(u'I', Plug(u'OrgEditAtFirstCharacter', u':py ORGMODE.plugins[u"Misc"].edit_at_first_character()<CR>')))

		self.keybindings.append(Keybinding(u'ih', Plug(u'OrgInnerHeadingVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].i_heading()<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'ah', Plug(u'OrgAInnerHeadingVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].a_heading()<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'Oh', Plug(u'OrgOuterHeadingVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].i_heading(selection=u"outer")<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'OH', Plug(u'OrgAOuterHeadingVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].a_heading(selection=u"outer")<CR>', mode=MODE_VISUAL)))

		self.keybindings.append(Keybinding(u'ih', Plug(u'OrgInnerHeadingOperator', u':<C-u>py ORGMODE.plugins[u"Misc"].i_heading(mode=u"operator")<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u'ah', u':normal Vah<CR>', mode=MODE_OPERATOR))
		self.keybindings.append(Keybinding(u'Oh', Plug(u'OrgOuterHeadingOperator', ':<C-u>py ORGMODE.plugins[u"Misc"].i_heading(mode=u"operator", selection=u"outer")<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u'OH', u':normal VOH<CR>', mode=MODE_OPERATOR))

		self.keybindings.append(Keybinding(u'it', Plug(u'OrgInnerTreeVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].i_heading(skip_children=True)<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'at', Plug(u'OrgAInnerTreeVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].a_heading(skip_children=True)<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'Ot', Plug(u'OrgOuterTreeVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].i_heading(selection=u"outer", skip_children=True)<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'OT', Plug(u'OrgAOuterTreeVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Misc"].a_heading(selection=u"outer", skip_children=True)<CR>', mode=MODE_VISUAL)))

		self.keybindings.append(Keybinding(u'it', Plug(u'OrgInnerTreeOperator', u':<C-u>py ORGMODE.plugins[u"Misc"].i_heading(mode=u"operator")<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u'at', u':normal Vat<CR>', mode=MODE_OPERATOR))
		self.keybindings.append(Keybinding(u'Ot', Plug(u'OrgOuterTreeOperator', u':<C-u>py ORGMODE.plugins[u"Misc"].i_heading(mode=u"operator", selection=u"outer", skip_children=True)<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u'OT', u':normal VOT<CR>', mode=MODE_OPERATOR))
ftplugin/orgmode/plugins/__init__.py	[[[1
1
# -*- coding: utf-8 -*-
ftplugin/orgmode/plugins/Navigator.py	[[[1
275
# -*- coding: utf-8 -*-

from orgmode import echo, ORGMODE, apply_count
from orgmode.menu import Submenu, ActionEntry
from orgmode.keybinding import Keybinding, MODE_VISUAL, MODE_OPERATOR, Plug
from liborgmode import DIRECTION_FORWARD, DIRECTION_BACKWARD

import vim

class Navigator(object):
	u""" Implement navigation in org-mode documents """

	def __init__(self):
		object.__init__(self)
		self.menu = ORGMODE.orgmenu + Submenu(u'&Navigate Headings')
		self.keybindings = []

	@classmethod
	@apply_count
	def parent(cls, mode):
		u"""
		Focus parent heading

		:returns: parent heading or None
		"""
		heading = ORGMODE.get_document().current_heading()
		if not heading:
			if mode == u'visual':
				vim.command(u'normal gv'.encode(u'utf-8'))
			else:
				echo(u'No heading found')
			return

		if not heading.parent:
			if mode == u'visual':
				vim.command(u'normal gv'.encode(u'utf-8'))
			else:
				echo(u'No parent heading found')
			return

		if mode == u'visual':
			cls._change_visual_selection(heading, heading.parent, direction=DIRECTION_BACKWARD, parent=True)
		else:
			vim.current.window.cursor = (heading.parent.start_vim, heading.parent.level + 1)
		return heading.parent


	@classmethod
	def _change_visual_selection(cls, current_heading, heading, direction=DIRECTION_FORWARD, noheadingfound=False, parent=False):
		current = vim.current.window.cursor[0]
		line_start, col_start = [ int(i) for i in vim.eval(u'getpos("\'<")'.encode(u'utf-8'))[1:3] ]
		line_end, col_end = [ int(i) for i in vim.eval(u'getpos("\'>")'.encode(u'utf-8'))[1:3] ]

		f_start = heading.start_vim
		f_end = heading.end_vim
		swap_cursor = True

		# << |visual start
		# selection end >>
		if current == line_start:
			if (direction == DIRECTION_FORWARD and line_end < f_start) or noheadingfound and not direction == DIRECTION_BACKWARD:
				swap_cursor = False

			# focus heading HERE
			# << |visual start
			# selection end >>

			# << |visual start
			# focus heading HERE
			# selection end >>
			if f_start < line_start and direction == DIRECTION_BACKWARD:
				if current_heading.start_vim < line_start and not parent:
					line_start = current_heading.start_vim
				else:
					line_start = f_start

			elif (f_start < line_start or f_start < line_end) and not noheadingfound:
				line_start = f_start

			# << |visual start
			# selection end >>
			# focus heading HERE
			else:
				if direction == DIRECTION_FORWARD:
					if line_end < f_start and not line_start == f_start - 1 and current_heading:
						# focus end of previous heading instead of beginning of next heading
						line_start = line_end
						line_end = f_start - 1
					else:
						# focus end of next heading
						line_start = line_end
						line_end = f_end
				elif direction == DIRECTION_BACKWARD:
					if line_end < f_end:
						pass
				else:
					line_start = line_end
					line_end = f_end

		# << visual start
		# selection end| >>
		else:
			# focus heading HERE
			# << visual start
			# selection end| >>
			if line_start > f_start and line_end > f_end and not parent:
				line_end = f_end
				swap_cursor = False

			elif (line_start > f_start or \
					line_start == f_start) and line_end <= f_end and direction == DIRECTION_BACKWARD:
				line_end = line_start
				line_start = f_start

			# << visual start
			# selection end and focus heading end HERE| >>

			# << visual start
			# focus heading HERE
			# selection end| >>

			# << visual start
			# selection end| >>
			# focus heading HERE
			else:
				if direction == DIRECTION_FORWARD:
					if line_end < f_start - 1:
						# focus end of previous heading instead of beginning of next heading
						line_end = f_start - 1
					else:
						# focus end of next heading
						line_end = f_end
				else:
					line_end = f_end
				swap_cursor = False

		move_col_start = u'%dl' % (col_start - 1) if (col_start - 1) > 0 and (col_start - 1) < 2000000000 else u''
		move_col_end = u'%dl' % (col_end - 1) if (col_end - 1) > 0 and (col_end - 1) < 2000000000 else u''
		swap = u'o' if swap_cursor else u''

		vim.command((u'normal %dgg%s%s%dgg%s%s' % \
				(line_start, move_col_start, vim.eval(u'visualmode()'.encode(u'utf-8')), line_end, move_col_end, swap)).encode(u'utf-8'))

	@classmethod
	def _focus_heading(cls, mode, direction=DIRECTION_FORWARD, skip_children=False):
		u"""
		Focus next or previous heading in the given direction

		:direction: True for next heading, False for previous heading
		:returns: next heading or None
		"""
		d = ORGMODE.get_document()
		current_heading = d.current_heading()
		heading = current_heading
		focus_heading = None
		# FIXME this is just a piece of really ugly and unmaintainable code. It
		# should be rewritten
		if not heading:
			if direction == DIRECTION_FORWARD and d.headings \
					and vim.current.window.cursor[0] < d.headings[0].start_vim:
				# the cursor is in the meta information are, therefore focus
				# first heading
				focus_heading = d.headings[0]
			if not (heading or focus_heading):
				if mode == u'visual':
					# restore visual selection when no heading was found
					vim.command(u'normal gv'.encode(u'utf-8'))
				else:
					echo(u'No heading found')
				return
		elif direction == DIRECTION_BACKWARD:
			if vim.current.window.cursor[0] != heading.start_vim:
				# the cursor is in the body of the current heading, therefore
				# the current heading will be focused
				if mode == u'visual':
					line_start, col_start = [ int(i) for i in vim.eval(u'getpos("\'<")'.encode(u'utf-8'))[1:3] ]
					line_end, col_end = [ int(i) for i in vim.eval(u'getpos("\'>")'.encode(u'utf-8'))[1:3] ]
					if line_start >= heading.start_vim and line_end > heading.start_vim:
						focus_heading = heading
				else:
					focus_heading = heading

		# so far no heading has been found that the next focus should be on
		if not focus_heading:
			if not skip_children and direction == DIRECTION_FORWARD and heading.children:
				focus_heading = heading.children[0]
			elif direction == DIRECTION_FORWARD and heading.next_sibling:
				focus_heading = heading.next_sibling
			elif direction == DIRECTION_BACKWARD and heading.previous_sibling:
				focus_heading = heading.previous_sibling
				if not skip_children:
					while focus_heading.children:
						focus_heading = focus_heading.children[-1]
			else:
				if direction == DIRECTION_FORWARD:
					focus_heading = current_heading.next_heading
				else:
					focus_heading = current_heading.previous_heading

		noheadingfound = False
		if not focus_heading:
			if mode in (u'visual', u'operator'):
				# the cursor seems to be on the last or first heading of this
				# document and performes another next/previous operation
				focus_heading = heading
				noheadingfound = True
			else:
				if direction == DIRECTION_FORWARD:
					echo(u'Already focussing last heading')
				else:
					echo(u'Already focussing first heading')
				return

		if mode == u'visual':
			cls._change_visual_selection(current_heading, focus_heading, direction=direction, noheadingfound=noheadingfound)
		elif mode == u'operator':
			if direction == DIRECTION_FORWARD and vim.current.window.cursor[0] >= focus_heading.start_vim:
				vim.current.window.cursor = (focus_heading.end_vim, len(vim.current.buffer[focus_heading.end].decode(u'utf-8')))
			else:
				vim.current.window.cursor = (focus_heading.start_vim, 0)
		else:
			vim.current.window.cursor = (focus_heading.start_vim, focus_heading.level + 1)
		if noheadingfound:
			return
		return focus_heading

	@classmethod
	@apply_count
	def previous(cls, mode, skip_children=False):
		u"""
		Focus previous heading
		"""
		return cls._focus_heading(mode, direction=DIRECTION_BACKWARD, skip_children=skip_children)

	@classmethod
	@apply_count
	def next(cls, mode, skip_children=False):
		u"""
		Focus next heading
		"""
		return cls._focus_heading(mode, direction=DIRECTION_FORWARD, skip_children=skip_children)

	def register(self):
		# normal mode
		self.keybindings.append(Keybinding(u'g{', Plug('OrgJumpToParentNormal', u':py ORGMODE.plugins[u"Navigator"].parent(mode=u"normal")<CR>')))
		self.menu + ActionEntry(u'&Up', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'{', Plug(u'OrgJumpToPreviousNormal', u':py ORGMODE.plugins[u"Navigator"].previous(mode=u"normal")<CR>')))
		self.menu + ActionEntry(u'&Previous', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'}', Plug(u'OrgJumpToNextNormal', u':py ORGMODE.plugins[u"Navigator"].next(mode=u"normal")<CR>')))
		self.menu + ActionEntry(u'&Next', self.keybindings[-1])

		# visual mode
		self.keybindings.append(Keybinding(u'g{', Plug(u'OrgJumpToParentVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Navigator"].parent(mode=u"visual")<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'{', Plug(u'OrgJumpToPreviousVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Navigator"].previous(mode=u"visual")<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u'}', Plug(u'OrgJumpToNextVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Navigator"].next(mode=u"visual")<CR>', mode=MODE_VISUAL)))

		# operator-pending mode
		self.keybindings.append(Keybinding(u'g{', Plug(u'OrgJumpToParentOperator', u':<C-u>py ORGMODE.plugins[u"Navigator"].parent(mode=u"operator")<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u'{', Plug(u'OrgJumpToPreviousOperator', u':<C-u>py ORGMODE.plugins[u"Navigator"].previous(mode=u"operator")<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u'}', Plug(u'OrgJumpToNextOperator', u':<C-u>py ORGMODE.plugins[u"Navigator"].next(mode=u"operator")<CR>', mode=MODE_OPERATOR)))

		# section wise movement (skip children)
		# normal mode
		self.keybindings.append(Keybinding(u'[[', Plug(u'OrgJumpToPreviousSkipChildrenNormal', u':py ORGMODE.plugins[u"Navigator"].previous(mode=u"normal", skip_children=True)<CR>')))
		self.menu + ActionEntry(u'Ne&xt Same Level', self.keybindings[-1])
		self.keybindings.append(Keybinding(u']]', Plug(u'OrgJumpToNextSkipChildrenNormal', u':py ORGMODE.plugins[u"Navigator"].next(mode=u"normal", skip_children=True)<CR>')))
		self.menu + ActionEntry(u'Pre&vious Same Level', self.keybindings[-1])

		# visual mode
		self.keybindings.append(Keybinding(u'[[', Plug(u'OrgJumpToPreviousSkipChildrenVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Navigator"].previous(mode=u"visual", skip_children=True)<CR>', mode=MODE_VISUAL)))
		self.keybindings.append(Keybinding(u']]', Plug(u'OrgJumpToNextSkipChildrenVisual', u'<Esc>:<C-u>py ORGMODE.plugins[u"Navigator"].next(mode=u"visual", skip_children=True)<CR>', mode=MODE_VISUAL)))

		# operator-pending mode
		self.keybindings.append(Keybinding(u'[[', Plug(u'OrgJumpToPreviousSkipChildrenOperator', u':<C-u>py ORGMODE.plugins[u"Navigator"].previous(mode=u"operator", skip_children=True)<CR>', mode=MODE_OPERATOR)))
		self.keybindings.append(Keybinding(u']]', Plug(u'OrgJumpToNextSkipChildrenOperator', u':<C-u>py ORGMODE.plugins[u"Navigator"].next(mode=u"operator", skip_children=True)<CR>', mode=MODE_OPERATOR)))
ftplugin/orgmode/plugins/LoggingWork.py	[[[1
40
# -*- coding: utf-8 -*-

from orgmode import echo, echom, echoe, ORGMODE, apply_count, repeat
from orgmode.menu import Submenu, Separator, ActionEntry
from orgmode.keybinding import Keybinding, Plug, Command

import vim

class LoggingWork(object):
	u""" LoggingWork plugin """

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'&Logging work')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

		# commands for this plugin
		self.commands = []

	@classmethod
	def action(cls):
		u""" Some kind of action

		:returns: TODO
		"""
		pass

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		# an Action menu entry which binds "keybinding" to action ":action"
		self.commands.append(Command(u'OrgLoggingRecordDoneTime', u':py ORGMODE.plugins[u"LoggingWork"].action()'))
		self.menu + ActionEntry(u'&Record DONE time', self.commands[-1])
ftplugin/orgmode/plugins/Todo.py	[[[1
151
# -*- coding: utf-8 -*-

from orgmode import echom, ORGMODE, apply_count, repeat, realign_tags, DIRECTION_FORWARD, DIRECTION_BACKWARD
from orgmode.menu import Submenu, ActionEntry
from orgmode import settings
from orgmode.keybinding import Keybinding, Plug

import vim


class Todo(object):
	u"""
	Todo plugin.

	Description taken from orgmode.org:

	You can use TODO keywords to indicate different sequential states in the
	process of working on an item, for example:

	["TODO", "FEEDBACK", "VERIFY", "|", "DONE", "DELEGATED"]

	The vertical bar separates the TODO keywords (states that need action) from
	the DONE states (which need no further action). If you don't provide the
	separator bar, the last state is used as the DONE state. With this setup,
	the command ``,d`` will cycle an entry from TODO to FEEDBACK, then to
	VERIFY, and finally to DONE and DELEGATED.
	"""

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'&TODO Lists')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

	@classmethod
	def _get_states(cls):
		u"""
		Return the next states divided in TODO states and DONE states.
		"""
		states = settings.get(u'org_todo_keywords', [])
		if not u'|' in states:
			return states[:-1], [states[-1]]
		else:
			seperator_pos = states.index(u'|')
			return states[0:seperator_pos], states[seperator_pos + 1:]

	@classmethod
	def _get_next_state(cls, current_state, all_states,
			direction=DIRECTION_FORWARD):
		u"""
		Return the next state as string, or NONE if the next state is no state.
		"""
		if not current_state in all_states:
			if direction == DIRECTION_FORWARD:
				return all_states[0]
			else:
				return all_states[-1]
		else:
			current_pos = all_states.index(current_state)
			if direction == DIRECTION_FORWARD:
				next_pos = current_pos + 1
			else:
				next_pos = current_pos - 1

			if next_pos < 0 or next_pos >= len(all_states):
				return None
			return all_states[next_pos]

	@classmethod
	@realign_tags
	@repeat
	@apply_count
	def toggle_todo_state(cls, direction=DIRECTION_FORWARD):
		u""" Toggle state of TODO item

		:returns: The changed heading
		"""
		d = ORGMODE.get_document(allow_dirty=True)
		lineno, colno = vim.current.window.cursor

		# get heading
		heading = d.find_current_heading()
		if not heading:
			vim.eval(u'feedkeys("^", "n")')
			return

		# get todo states
		todo_states, done_states = Todo._get_states()
		all_states = todo_states + done_states
		if len(all_states) < 2:
			echom(u'No todo keywords configured.')
			return

		# current_state
		current_state = heading.todo

		# get new state
		new_state = Todo._get_next_state(current_state, all_states, direction)

		# move cursor along with the inserted state only when current position
		# is in the heading; otherwite do nothing
		if heading.start_vim == lineno:
			if current_state is None:
				offset = len(new_state)
			elif new_state is None:
				offset = -len(current_state)
			else:
				offset = len(current_state) - len(new_state)
			vim.current.window.cursor = (lineno, colno + offset)

		# set new headline
		heading.todo = new_state

		# plug
		plug = u'OrgToggleTodoForward'
		if direction == DIRECTION_BACKWARD:
			plug = u'OrgToggleTodoBackward'

		d.write_heading(heading)

		return plug

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		settings.set(u'org_leader', u',')
		leader = settings.get(u'org_leader', u',')

		self.keybindings.append(Keybinding(u'%sd' % leader, Plug(
			u'OrgToggleTodoToggle',
			u':silent! py ORGMODE.plugins[u"Todo"].toggle_todo_state()<CR>')))
		self.menu + ActionEntry(u'&TODO/DONE/-', self.keybindings[-1])
		submenu = self.menu + Submenu(u'Select &keyword')
		self.keybindings.append(Keybinding(u'<S-Right>', Plug(
			u'OrgToggleTodoForward',
			u':silent! py ORGMODE.plugins[u"Todo"].toggle_todo_state()<CR>')))
		submenu + ActionEntry(u'&Next keyword', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'<S-Left>', Plug(
			u'OrgToggleTodoBackward',
			u':silent! py ORGMODE.plugins[u"Todo"].toggle_todo_state(False)<CR>')))
		submenu + ActionEntry(u'&Previous keyword', self.keybindings[-1])

		settings.set(u'org_todo_keywords', [u'TODO', u'|', u'DONE'])

# vim: set noexpandtab:
ftplugin/orgmode/plugins/ShowHide.py	[[[1
114
# -*- coding: utf-8 -*-

from orgmode import settings
from orgmode import ORGMODE, apply_count
from orgmode.menu import Submenu, ActionEntry
from orgmode.keybinding import Keybinding, Plug, MODE_NORMAL

import vim

class ShowHide(object):
	u""" Show Hide plugin """

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'&Show Hide')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

	@classmethod
	@apply_count
	def toggle_folding(cls):
		u""" Toggle folding similar to the way orgmode does

		This is just a convenience function, don't hesitate to use the z*
		keybindings vim offers to deal with folding!
		"""
		d = ORGMODE.get_document()
		heading = d.current_heading()
		if not heading:
			vim.eval(u'feedkeys("<Tab>", "n")'.encode(u'utf-8'))
			return

		cursor = vim.current.window.cursor[:]

		if int(vim.eval((u'foldclosed(%d)' % heading.start_vim).encode(u'utf-8'))) != -1:
			# open closed fold
			p = heading.number_of_parents
			if not p:
				p = heading.level
			vim.command((u'normal %dzo' % p).encode(u'utf-8'))
			vim.current.window.cursor = cursor
			return heading

		found_fold = False
		open_depth = 0

		def fold_depth(h):
			if int(vim.eval((u'foldclosed(%d)' % h.start_vim).encode(u'utf-8'))) != -1:
				return (h.number_of_parents, True)
			else:
				res = [h.number_of_parents + 1]
				found = False

				for c in h.children:
					d, f = fold_depth(c)
					res.append(d)
					found |= f

				return (max(res), found)

		def open_fold(h):
			if h.number_of_parents <= open_depth:
				vim.command((u'normal %dgg%dzo' % (h.start_vim, open_depth)).encode(u'utf-8'))
			if h.children:
				for c in h.children:
					open_fold(c)

		# find deepest fold
		open_depth, found_fold = fold_depth(heading)

		# recursively open folds
		for child in heading.children:
			# find deepest fold
			if found_fold:
				open_fold(child)

		if not found_fold:
			vim.command((u'%d,%dfoldclose!' % (heading.start_vim, heading.end_of_last_child_vim)).encode(u'utf-8'))

			if heading.number_of_parents:
				# restore cursor position, it might have been changed by open_fold
				vim.current.window.cursor = cursor

				p = heading.number_of_parents
				if not p:
					p = heading.level
				# reopen fold again beacause the former closing of the fold closed all levels, including parents!
				vim.command((u'normal %dzo' % (p, )).encode(u'utf-8'))

		# restore cursor position
		vim.current.window.cursor = cursor
		return heading

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		# register plug

		self.keybindings.append(Keybinding(u'<Tab>', Plug(u'OrgToggleFolding', u':py ORGMODE.plugins[u"ShowHide"].toggle_folding()<CR>')))
		self.menu + ActionEntry(u'&Cycle Visibility', self.keybindings[-1])

		settings.set(u'org_leader', u',')
		leader = settings.get(u'org_leader', u',')

		self.keybindings.append(Keybinding(u'%s,' % (leader, ), u':exe ":set fdl=". (&fdl - 1)<CR>', mode=MODE_NORMAL))
		self.keybindings.append(Keybinding(u'%s.' % (leader, ), u':exe ":set fdl=". (&fdl + 1)<CR>', mode=MODE_NORMAL))
		for i in xrange(0, 10):
			self.keybindings.append(Keybinding(u'%s%d' % (leader, i), u'zM:set fdl=%d<CR>' % i, mode=MODE_NORMAL))
ftplugin/orgmode/plugins/TagsProperties.py	[[[1
158
# -*- coding: utf-8 -*-

from orgmode import ORGMODE, repeat
from orgmode.menu import Submenu, ActionEntry
from orgmode.keybinding import Keybinding, Plug, Command
from orgmode import settings

import vim

class TagsProperties(object):
	u""" TagsProperties plugin """

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'&TAGS and Properties')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

		# commands for this plugin
		self.commands = []

	@classmethod
	def complete_tags(cls):
		u""" build a list of tags and store it in variable b:org_tag_completion
		"""
		d = ORGMODE.get_document()
		heading = d.current_heading()
		if not heading:
			return

		leading_portion = vim.eval(u'a:ArgLead').decode(u'utf-8')
		cursor = int(vim.eval(u'a:CursorPos'))

		# extract currently completed tag
		idx_orig = leading_portion.rfind(u':', 0, cursor)
		if idx_orig == -1:
			idx = 0
		else:
			idx = idx_orig

		current_tag = leading_portion[idx: cursor].lstrip(u':')
		head = leading_portion[:idx + 1]
		if idx_orig == -1:
			head = u''
		tail = leading_portion[cursor:]

		# extract all tags of the current file
		all_tags = set()
		for h in d.all_headings():
			for t in h.tags:
				all_tags.add(t)

		ignorecase = bool(int(settings.get(u'org_tag_completion_ignorecase', int(vim.eval(u'&ignorecase')))))
		possible_tags = []
		current_tags = heading.tags
		for t in all_tags:
			if ignorecase:
				if t.lower().startswith(current_tag.lower()):
					possible_tags.append(t)
			elif t.startswith(current_tag):
				possible_tags.append(t)

		vim.command((u'let b:org_complete_tags = [%s]' % u', '.join([u'"%s%s:%s"' % (head, i, tail) for i in possible_tags])).encode(u'utf-8'))

	@classmethod
	@repeat
	def set_tags(cls):
		u""" Set tags for current heading
		"""
		d = ORGMODE.get_document()
		heading = d.current_heading()
		if not heading:
			return

		# retrieve tags
		res = None
		if heading.tags:
			res = vim.eval(u'input("Tags: ", ":%s:", "customlist,Org_complete_tags")' % u':'.join(heading.tags))
		else:
			res = vim.eval(u'input("Tags: ", "", "customlist,Org_complete_tags")')

		if res is None:
			# user pressed <Esc> abort any further processing
			return

		# remove empty tags
		heading.tags = filter(lambda x: x.strip() != u'', res.decode(u'utf-8').strip().strip(u':').split(u':'))

		d.write()

		return u'OrgSetTags'

	@classmethod
	def realign_tags(cls):
		u"""
		Updates tags when user finished editing a heading
		"""
		d = ORGMODE.get_document(allow_dirty=True)
		heading = d.find_current_heading()
		if not heading:
			return

		if vim.current.window.cursor[0] == heading.start_vim:
			heading.set_dirty_heading()
			d.write_heading(heading, including_children=False)

	@classmethod
	def realign_all_tags(cls):
		u"""
		Updates tags when user finishes editing a heading
		"""
		d = ORGMODE.get_document()
		for heading in d.all_headings():
			heading.set_dirty_heading()

		d.write()

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		# an Action menu entry which binds "keybinding" to action ":action"
		settings.set(u'org_tag_column', u'77')

		settings.set(u'org_tag_completion_ignorecase', int(vim.eval(u'&ignorecase')))

		settings.set(u'org_leader', u',')
		leader = settings.get(u'org_leader', u',')

		self.keybindings.append(Keybinding(u'%st' % leader, Plug(u'OrgSetTags', u':py ORGMODE.plugins[u"TagsProperties"].set_tags()<CR>')))
		self.menu + ActionEntry(u'Set &Tags', self.keybindings[-1])

		self.commands.append(Command(u'OrgTagsRealign', u":py ORGMODE.plugins[u'TagsProperties'].realign_all_tags()"))

		# workaround to align tags when user is leaving insert mode
		vim.command(u"""function Org_complete_tags(ArgLead, CmdLine, CursorPos)
python << EOF
ORGMODE.plugins[u'TagsProperties'].complete_tags()
EOF
if exists('b:org_complete_tags')
	let tmp = b:org_complete_tags
	unlet b:org_complete_tags
	return tmp
else
	return []
endif
endfunction""".encode(u'utf-8'))

		# this is for all org files opened after this file
		vim.command(u"au FileType org :au InsertLeave <buffer> :py ORGMODE.plugins[u'TagsProperties'].realign_tags()".encode(u'utf-8'))

		# this is for the current file
		vim.command(u"au InsertLeave <buffer> :py ORGMODE.plugins[u'TagsProperties'].realign_tags()".encode(u'utf-8'))
ftplugin/orgmode/plugins/Date.py	[[[1
229
# -*- coding: utf-8 -*-
import re
from datetime import timedelta, date, datetime

import vim
from orgmode import ORGMODE, settings, echom, insert_at_cursor, get_user_input
from orgmode.keybinding import Keybinding, Plug
from orgmode.menu import Submenu, ActionEntry


class Date(object):
	u"""
	Handles all date and timestamp related tasks.

	TODO: extend functionality (calendar, repetitions, ranges). See
			http://orgmode.org/guide/Dates-and-Times.html#Dates-and-Times
	"""

	date_regex = r"\d\d\d\d-\d\d-\d\d"
	datetime_regex = r"[A-Z]\w\w \d\d\d\d-\d\d-\d\d \d\d:\d\d>"

	month_mapping = {u'jan': 1, u'feb':2, u'mar':3, u'apr':4, u'may':5, u'jun':6,
			u'jul': 7, u'aug': 8, u'sep': 9, u'oct': 10, u'nov': 11, u'dec': 12}

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'Dates and Scheduling')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

		# commands for this plugin
		self.commands = []

		# set speeddating format that is compatible with orgmode
		try:
			if int(vim.eval(u'exists(":SpeedDatingFormat")')):
				vim.command(u':1SpeedDatingFormat %Y-%m-%d %a'.encode(u'utf-8'))
				vim.command(u':1SpeedDatingFormat %Y-%m-%d %a %H:%M'.encode(u'utf-8'))
			else:
				echom(u'Speeddating plugin not installed. Please install it.')
		except:
			echom(u'Speeddating plugin not installed. Please install it.')

	@classmethod
	def _modify_time(cls, startdate, modifier):
		u"""Modify the given startdate according to modifier. Return the new time.

		See http://orgmode.org/manual/The-date_002ftime-prompt.html#The-date_002ftime-prompt
		"""
		if modifier is None:
			return startdate

		# check real date
		date_regex = r"(\d\d\d\d)-(\d\d)-(\d\d)"
		match = re. search(date_regex, modifier)
		if match:
			year, month, day = match.groups()
			t = date(int(year), int(month), int(day))
			return t

		# check abbreviated date, seperated with '-'
		date_regex = u"(\d{1,2})-(\d+)-(\d+)"
		match = re. search(date_regex, modifier)
		if match:
			year, month, day = match.groups()
			t = date(2000 + int(year), int(month), int(day))
			return t

		# check abbreviated date, seperated with '/'
		# month/day/year
		date_regex = u"(\d{1,2})/(\d+)/(\d+)"
		match = re. search(date_regex, modifier)
		if match:
			month, day, year = match.groups()
			t = date(2000 + int(year), int(month), int(day))
			return t

		# check abbreviated date, seperated with '/'
		# month/day
		date_regex = u"(\d{1,2})/(\d{1,2})"
		match = re. search(date_regex, modifier)
		if match:
			month, day = match.groups()
			newdate = date(startdate.year, int(month), int(day))
			# date should be always in the future
			if newdate < startdate:
				newdate = date(startdate.year+1, int(month), int(day))
			return newdate

		# check full date, seperated with 'space'
		# month day year
		# 'sep 12 9' --> 2009 9 12
		date_regex = u"(\w\w\w) (\d{1,2}) (\d{1,2})"
		match = re. search(date_regex, modifier)
		if match:
			gr = match.groups()
			day = int(gr[1])
			month = int(cls.month_mapping[gr[0]])
			year = 2000 + int(gr[2])
			return date(year, int(month), int(day))

		# check days as integers
		date_regex = u"^(\d{1,2})$"
		match = re. search(date_regex, modifier)
		if match:
			newday, = match.groups()
			newday = int(newday)
			if newday > startdate.day:
				newdate = date(startdate.year, startdate.month, newday)
			else:
				# TODO: DIRTY, fix this
				#       this does NOT cover all edge cases
				newdate = startdate + timedelta(days=28)
				newdate = date(newdate.year, newdate.month, newday)
			return newdate

		# check for full days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
		modifier_lc = modifier.lower()
		match = re.search(u'mon|tue|wed|thu|fri|sat|sun', modifier_lc)
		if match:
			weekday_mapping = {u'mon': 0, u'tue': 1, u'wed': 2, u'thu': 3,
					u'fri': 4, u'sat': 5, u'sun': 6}
			diff = (weekday_mapping[modifier_lc] - startdate.weekday()) % 7
			# use next weeks weekday if current weekday is the same as modifier
			if diff == 0:
				diff = 7

			return startdate + timedelta(days=diff)

		# check for month day
		modifier_lc = modifier.lower()
		match = re.search(u'(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec) (\d{1,2})',
				modifier_lc)
		if match:
			month = cls.month_mapping[match.groups()[0]]
			day = int(match.groups()[1])

			newdate = date(startdate.year, int(month), int(day))
			# date should be always in the future
			if newdate < startdate:
				newdate = date(startdate.year+1, int(month), int(day))
			return newdate

		# check for time: HH:MM
		# '12:45' --> datetime(2006,06,13, 12,45))
		match = re.search(u'(\d{1,2}):(\d\d)', modifier)
		if match:
			return datetime(startdate.year, startdate.month, startdate.day,
					int(match.groups()[0]), int(match.groups()[1]))

		# check for days modifier
		match = re.search(u'\+(\d*)d', modifier)
		if match:
			days = int(match.groups()[0])
			return startdate + timedelta(days=days)

		# check for week modifier
		match = re.search(u'\+(\d+)w', modifier)
		if match:
			weeks = int(match.groups()[0])
			return startdate + timedelta(weeks=weeks)

		# check for week modifier
		match = re.search(u'\+(\d+)m', modifier)
		if match:
			months = int(match.groups()[0])
			return date(startdate.year, startdate.month + months, startdate.day)

		# check for year modifier
		match = re.search(u'\+(\d*)y', modifier)
		if match:
			years = int(match.groups()[0])
			return date(startdate.year + years, startdate.month, startdate.day)

		return startdate

	@classmethod
	def insert_timestamp(cls, active=True):
		u"""
		Insert a timestamp (today) at the cursor position.

		TODO: show fancy calendar to pick the date from.
		"""
		today = date.today()
		msg = u''.join([u'Insert Date: ', today.strftime(u'%Y-%m-%d %a'.encode(u'utf-8')),
				u' | Change date'])
		modifier = get_user_input(msg)
		echom(modifier)

		newdate = cls._modify_time(today, modifier)

		# format
		if isinstance(newdate, datetime):
			newdate = newdate.strftime(u'%Y-%m-%d %a %H:%M').decode(u'utf-8')
		else:
			newdate = newdate.strftime(u'%Y-%m-%d %a').decode(u'utf-8')
		timestamp = u'<%s>' % newdate if active else u'[%s]' % newdate

		insert_at_cursor(timestamp)

	def register(self):
		u"""
		Registration of the plugin.

		Key bindings and other initialization should be done here.
		"""
		settings.set(u'org_leader', u',')
		leader = settings.get(u'org_leader', u',')

		self.keybindings.append(Keybinding(u'%ssa' % leader,
				Plug(u'OrgDateInsertTimestampActive',
				u':py ORGMODE.plugins[u"Date"].insert_timestamp()<CR>')))
		self.menu + ActionEntry(u'Timest&amp', self.keybindings[-1])

		self.keybindings.append(Keybinding(u'%ssi' % leader,
				Plug(u'OrgDateInsertTimestampInactive',
					u':py ORGMODE.plugins[u"Date"].insert_timestamp(False)<CR>')))
		self.menu + ActionEntry(u'Timestamp (&inactive)', self.keybindings[-1])

		submenu = self.menu + Submenu(u'Change &Date')
		submenu + ActionEntry(u'Day &Earlier', u'<C-x>', u'<C-x>')
		submenu + ActionEntry(u'Day &Later', u'<C-a>', u'<C-a>')

# vim: set noexpandtab:
ftplugin/orgmode/plugins/EditStructure.py	[[[1
366
# -*- coding: utf-8 -*-

from orgmode import ORGMODE, apply_count, repeat, realign_tags, DIRECTION_FORWARD, DIRECTION_BACKWARD
from orgmode.menu import Submenu, Separator, ActionEntry
from orgmode.keybinding import Keybinding, Plug, MODE_INSERT, MODE_NORMAL
from liborgmode import Heading
from orgmode.exceptions import HeadingDomError

import vim

class EditStructure(object):
	u""" EditStructure plugin """

	def __init__(self):
		u""" Initialize plugin """
		object.__init__(self)
		# menu entries this plugin should create
		self.menu = ORGMODE.orgmenu + Submenu(u'&Edit Structure')

		# key bindings for this plugin
		# key bindings are also registered through the menu so only additional
		# bindings should be put in this variable
		self.keybindings = []

	@classmethod
	def new_heading(cls, below=None, insert_mode=False, end_of_last_child=False):
		u"""
		:below:				True, insert heading below current heading, False,
							insert heading above current heading, None, special
							behavior for insert mode, use the current text as
							heading
		:insert_mode:		True, if action is performed in insert mode
		:end_of_last_child:	True, insert heading at the end of last child,
							otherwise the newly created heading will "take
							over" the current heading's children
		"""
		d = ORGMODE.get_document()
		current_heading = d.current_heading()
		cursor = vim.current.window.cursor[:]
		if not current_heading:
			# the user is in meta data region
			pos = cursor[0] - 1
			heading = Heading(title=d.meta_information[pos], body=d.meta_information[pos + 1:])
			d.headings.insert(0, heading)
			del d.meta_information[pos:]
			d.write()

			if insert_mode:
				vim.command((u'exe "normal %dgg"|startinsert!' % (heading.start_vim, )).encode(u'utf-8'))
			else:
				vim.current.window.cursor = (pos + 1, heading.level + 1)
			return heading

		heading = Heading(level=current_heading.level)

		# it's weird but this is the behavior of original orgmode
		if below is None:
			below = cursor[1] != 0 or end_of_last_child

		heading_insert_position = 0
		if below:
			heading_insert_position = 1
			if not end_of_last_child:
				# append heading at the end of current heading but also take
				# over the children of current heading
				heading.children = current_heading.children[:]
				del current_heading.children

		# if cursor is currently on a heading, insert parts of it into the
		# newly created heading
		if insert_mode and cursor[1] != 0 and cursor[0] == current_heading.start_vim:
			offset = cursor[1] - current_heading.level - 1 - (len(current_heading.todo) + 1 if current_heading.todo else 0)
			if offset < 0:
				offset = 0
			heading.title = current_heading.title[offset:]
			current_heading.title = current_heading.title[:offset]
			heading.body = current_heading.body[:]
			current_heading.body = []

		# insert newly created heading
		l = current_heading.get_parent_list()
		idx = current_heading.get_index_in_parent_list()
		if l is not None and idx is not None:
			l.insert(idx + heading_insert_position, heading)
		else:
			raise HeadingDomError(u'Current heading is not properly linked in DOM')

		d.write()

		if insert_mode:
			vim.command((u'exe "normal %dgg"|startinsert!' % (heading.start_vim, )).encode(u'utf-8'))
		else:
			vim.current.window.cursor = (heading.start_vim, cursor[1] + heading.level + 1)

		# return newly created heading
		return heading

	@classmethod
	def _change_heading_level(cls, level, including_children=True, on_heading=False, insert_mode=False):
		u"""
		Change level of heading realtively with or without including children.
		"""
		d = ORGMODE.get_document()
		current_heading = d.current_heading()
		if not current_heading or on_heading and current_heading.start_vim != vim.current.window.cursor[0]:
			# TODO figure out the actually pressed keybinding and feed these
			# keys instead of making keys up like this
			if level > 0:
				if insert_mode:
					vim.eval(u'feedkeys("\<C-t>", "n")'.encode(u'utf-8'))
				elif including_children:
					vim.eval(u'feedkeys(">]]", "n")'.encode(u'utf-8'))
				elif on_heading:
					vim.eval(u'feedkeys(">>", "n")'.encode(u'utf-8'))
				else:
					vim.eval(u'feedkeys(">}", "n")'.encode(u'utf-8'))
			else:
				if insert_mode:
					vim.eval(u'feedkeys("\<C-d>", "n")'.encode(u'utf-8'))
				elif including_children:
					vim.eval(u'feedkeys("<]]", "n")'.encode(u'utf-8'))
				elif on_heading:
					vim.eval(u'feedkeys("<<", "n")'.encode(u'utf-8'))
				else:
					vim.eval(u'feedkeys("<}", "n")'.encode(u'utf-8'))
			# return True because otherwise apply_count will not work
			return True

		# don't allow demotion below level 1
		if current_heading.level == 1 and level < 1:
			return False

		# reduce level of demotion to a minimum heading level of 1
		if (current_heading.level + level) < 1:
			level = 1

		def indent(heading, ic):
			if not heading:
				return
			heading.level += level

			if ic:
				for child in heading.children:
					indent(child, ic)

		# save cursor position
		c = vim.current.window.cursor[:]

		# indent the promoted/demoted heading
		indent_end_vim = current_heading.end_of_last_child_vim if including_children else current_heading.end_vim
		indent(current_heading, including_children)

		# when changing the level of a heading, it's position in the DOM
		# needs to be updated. It's likely that the heading gets a new
		# parent and new children when demoted or promoted

		# find new parent
		p = current_heading.parent
		pl = current_heading.get_parent_list()
		ps = current_heading.previous_sibling
		nhl = current_heading.level

		def append_heading(heading, parent):
			if heading.level <= parent.level:
				raise ValueError('Heading level not is lower than parent level: %d ! > %d' % (heading.level, parent.level))

			if parent.children and parent.children[-1].level < heading.level:
				append_heading(heading, parent.children[-1])
			else:
				parent.children.append(heading)

		if level > 0:
			# demotion
			# subheading or top level heading
			if ps and nhl > ps.level:
				idx = current_heading.get_index_in_parent_list()
				pl.remove(current_heading)
				# find heading that is the new parent heading
				oh = ps
				h = ps
				while nhl > h.level:
					oh = h
					if h.children:
						h = h.children[-1]
					else:
						break
				np = h if nhl > h.level else oh

				# append current heading to new heading
				np.children.append(current_heading)

				# if children are not included, distribute them among the
				# parent heading and it's siblings
				if not including_children:
					for h in current_heading.children[:]:
						if h.level <= nhl:
							current_heading.children.remove(h)
							append_heading(h, p)
		else:
			# promotion
			if p and nhl <= p.level:
				idx = current_heading.get_index_in_parent_list() + 1
				# find the new parent heading
				oh = p
				h = p
				while nhl <= h.level:
					# append new children to current heading
					[ append_heading(child.copy(), current_heading) for child in h.children[idx:] ]
					del h.children[idx:]
					oh = h
					idx = h.get_index_in_parent_list() + 1
					if h.parent:
						h = h.parent
					else:
						break
				ns = oh.next_sibling
				while ns and ns.level > current_heading.level:
					nns = ns.next_sibling
					append_heading(ns, current_heading)
					ns = nns

				# append current heading to new parent heading / document
				pl.remove(current_heading)
				if nhl > h.level:
					h.children.insert(idx, current_heading)
				else:
					d.headings.insert(idx, current_heading)

		d.write()
		if indent_end_vim != current_heading.start_vim:
			vim.command((u'normal %dggV%dgg=' % (current_heading.start_vim, indent_end_vim)).encode(u'utf-8'))
		# restore cursor position
		vim.current.window.cursor = (c[0], c[1] + level)

		return True

	@classmethod
	@realign_tags
	@repeat
	@apply_count
	def demote_heading(cls, including_children=True, on_heading=False, insert_mode=False):
		if cls._change_heading_level(1, including_children=including_children, on_heading=on_heading, insert_mode=insert_mode):
			if including_children:
				return u'OrgDemoteSubtree'
			return u'OrgDemoteHeading'

	@classmethod
	@realign_tags
	@repeat
	@apply_count
	def promote_heading(cls, including_children=True, on_heading=False, insert_mode=False):
		if cls._change_heading_level(-1, including_children=including_children, on_heading=on_heading, insert_mode=insert_mode):
			if including_children:
				return u'OrgPromoteSubtreeNormal'
			return u'OrgPromoteHeadingNormal'

	@classmethod
	def _move_heading(cls, direction=DIRECTION_FORWARD, including_children=True):
		u""" Move heading up or down

		:returns: heading or None
		"""
		d = ORGMODE.get_document()
		heading = d.current_heading()
		if (not heading) or \
				(direction == DIRECTION_FORWARD and not heading.next_sibling) or \
				(direction == DIRECTION_BACKWARD and not heading.previous_sibling):
			return None

		cursor_offset_within_the_heading_vim = vim.current.window.cursor[0] - (heading._orig_start + 1)

		if not including_children:
			heading.previous_sibling.children.extend(heading.children)
			del heading.children

		heading_insert_position = 0 if direction == DIRECTION_FORWARD else -1
		l = heading.get_parent_list()
		idx = heading.get_index_in_parent_list()
		del l[idx]
		if l is not None and idx is not None:
			l.insert(idx + heading_insert_position, heading)
		else:
			raise HeadingDomError(u'Current heading is not properly linked in DOM')

		d.write()

		vim.current.window.cursor = (heading.start_vim + cursor_offset_within_the_heading_vim, vim.current.window.cursor[1])

		return True

	@classmethod
	@repeat
	@apply_count
	def move_heading_upward(cls, including_children=True):
		if cls._move_heading(direction=DIRECTION_BACKWARD, including_children=including_children):
			return u'OrgMoveHeadingUpward'

	@classmethod
	@repeat
	@apply_count
	def move_heading_downward(cls, including_children=True):
		if cls._move_heading(direction=DIRECTION_FORWARD, including_children=including_children):
			return u'OrgMoveHeadingDownward'

	def register(self):
		u"""
		Registration of plugin. Key bindings and other initialization should be done.
		"""
		self.keybindings.append(Keybinding(u'<C-S-CR>', Plug(u'OrgNewHeadingAboveNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].new_heading(below=False)<CR>')))
		self.menu + ActionEntry(u'New Heading &above', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'<S-CR>', Plug(u'OrgNewHeadingBelowNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].new_heading(below=True)<CR>')))
		self.menu + ActionEntry(u'New Heading &below', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'<C-CR>', Plug(u'OrgNewHeadingBelowAfterChildrenNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].new_heading(below=True, end_of_last_child=True)<CR>')))
		self.menu + ActionEntry(u'New Heading below, after &children', self.keybindings[-1])

		self.keybindings.append(Keybinding(u'<C-S-CR>', Plug(u'OrgNewHeadingAboveInsert', u'<C-o>:<C-u>silent! py ORGMODE.plugins[u"EditStructure"].new_heading(below=False, insert_mode=True)<CR>', mode=MODE_INSERT)))
		self.keybindings.append(Keybinding(u'<S-CR>', Plug(u'OrgNewHeadingBelowInsert', u'<C-o>:<C-u>silent! py ORGMODE.plugins[u"EditStructure"].new_heading(insert_mode=True)<CR>', mode=MODE_INSERT)))
		self.keybindings.append(Keybinding(u'<C-CR>', Plug(u'OrgNewHeadingBelowAfterChildrenInsert', u'<C-o>:<C-u>silent! py ORGMODE.plugins[u"EditStructure"].new_heading(insert_mode=True, end_of_last_child=True)<CR>', mode=MODE_INSERT)))

		self.menu + Separator()

		self.keybindings.append(Keybinding(u'm{', Plug(u'OrgMoveHeadingUpward', u':py ORGMODE.plugins[u"EditStructure"].move_heading_upward(including_children=False)<CR>')))
		self.keybindings.append(Keybinding(u'm[[', Plug(u'OrgMoveSubtreeUpward', u':py ORGMODE.plugins[u"EditStructure"].move_heading_upward()<CR>')))
		self.menu + ActionEntry(u'Move Subtree &Up', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'm}', Plug(u'OrgMoveHeadingDownward', u':py ORGMODE.plugins[u"EditStructure"].move_heading_downward(including_children=False)<CR>')))
		self.keybindings.append(Keybinding(u'm]]', Plug(u'OrgMoveSubtreeDownward', u':py ORGMODE.plugins[u"EditStructure"].move_heading_downward()<CR>')))
		self.menu + ActionEntry(u'Move Subtree &Down', self.keybindings[-1])

		self.menu + Separator()

		self.menu + ActionEntry(u'&Copy Heading', u'yah', u'yah')
		self.menu + ActionEntry(u'C&ut Heading', u'dah', u'dah')

		self.menu + Separator()

		self.menu + ActionEntry(u'&Copy Subtree', u'yat', u'yat')
		self.menu + ActionEntry(u'C&ut Subtree', u'dat', u'dat')
		self.menu + ActionEntry(u'&Paste Subtree', u'p', u'p')

		self.menu + Separator()

		self.keybindings.append(Keybinding(u'<ah', Plug(u'OrgPromoteHeadingNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=False)<CR>')))
		self.menu + ActionEntry(u'&Promote Heading', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'<<', Plug(u'OrgPromoteOnHeadingNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=False, on_heading=True)<CR>')))
		self.keybindings.append(Keybinding(u'<{', u'<Plug>OrgPromoteHeadingNormal', mode=MODE_NORMAL))
		self.keybindings.append(Keybinding(u'<ih', u'<Plug>OrgPromoteHeadingNormal', mode=MODE_NORMAL))

		self.keybindings.append(Keybinding(u'<at', Plug(u'OrgPromoteSubtreeNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].promote_heading()<CR>')))
		self.menu + ActionEntry(u'&Promote Subtree', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'<[[', u'<Plug>OrgPromoteSubtreeNormal', mode=MODE_NORMAL))
		self.keybindings.append(Keybinding(u'<it', u'<Plug>OrgPromoteSubtreeNormal', mode=MODE_NORMAL))

		self.keybindings.append(Keybinding(u'>ah', Plug(u'OrgDemoteHeadingNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=False)<CR>')))
		self.menu + ActionEntry(u'&Demote Heading', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'>>', Plug(u'OrgDemoteOnHeadingNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=False, on_heading=True)<CR>')))
		self.keybindings.append(Keybinding(u'>}', u'>Plug>OrgDemoteHeadingNormal', mode=MODE_NORMAL))
		self.keybindings.append(Keybinding(u'>ih', u'>Plug>OrgDemoteHeadingNormal', mode=MODE_NORMAL))

		self.keybindings.append(Keybinding(u'>at', Plug(u'OrgDemoteSubtreeNormal', u':silent! py ORGMODE.plugins[u"EditStructure"].demote_heading()<CR>')))
		self.menu + ActionEntry(u'&Demote Subtree', self.keybindings[-1])
		self.keybindings.append(Keybinding(u'>]]', u'<Plug>OrgDemoteSubtreeNormal', mode=MODE_NORMAL))
		self.keybindings.append(Keybinding(u'>it', u'<Plug>OrgDemoteSubtreeNormal', mode=MODE_NORMAL))

		# other keybindings
		self.keybindings.append(Keybinding(u'<C-d>', Plug(u'OrgPromoteOnHeadingInsert', u'<C-o>:silent! py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=False, on_heading=True, insert_mode=True)<CR>', mode=MODE_INSERT)))
		self.keybindings.append(Keybinding(u'<C-t>', Plug(u'OrgDemoteOnHeadingInsert', u'<C-o>:silent! py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=False, on_heading=True, insert_mode=True)<CR>', mode=MODE_INSERT)))
ftplugin/orgmode/menu.py	[[[1
147
# -*- coding: utf-8 -*-

import vim

from orgmode.keybinding import Keybinding, MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT

def register_menu(f):
	def r(*args, **kwargs):
		p = f(*args, **kwargs)
		def create(entry):
			if isinstance(entry, Submenu) or isinstance(entry, Separator) \
					or isinstance(entry, ActionEntry):
				entry.create()

		if hasattr(p, u'menu'):
			if isinstance(p.menu, list) or isinstance(p.menu, tuple):
				for e in p.menu:
					create(e)
			else:
				create(p.menu)
		return p
	return r

class Submenu(object):
	u""" Submenu entry """

	def __init__(self, name, parent=None):
		object.__init__(self)
		self.name = name
		self.parent = parent
		self._children = []

	def __add__(self, entry):
		if entry not in self._children:
			self._children.append(entry)
			entry.parent = self
			return entry

	def __sub__(self, entry):
		if entry in self._children:
			idx = self._children.index(entry)
			del self._children[idx]

	@property
	def children(self):
		return self._children[:]

	def get_menu(self):
		n = self.name.replace(u' ', u'\\ ')
		if self.parent:
			return u'%s.%s' % (self.parent.get_menu(), n)
		return n

	def create(self):
		for c in self.children:
			c.create()

	def __str__(self):
		res = self.name
		for c in self.children:
			res += str(c)
		return res

class Separator(object):
	u""" Menu entry for a Separator """

	def __init__(self, parent=None):
		object.__init__(self)
		self.parent = parent

	def __unicode__(self):
		return u'-----'

	def __str__(self):
		return self.__unicode__().encode(u'utf-8')

	def create(self):
		if self.parent:
			menu = self.parent.get_menu()
			vim.command((u'menu %s.-%s- :' % (menu, id(self))).encode(u'utf-8'))

class ActionEntry(object):
	u""" ActionEntry entry """

	def __init__(self, lname, action, rname=None, mode=MODE_NORMAL, parent=None):
		u"""
		:lname: menu title on the left hand side of the menu entry
		:action: could be a vim command sequence or an actual Keybinding
		:rname: menu title that appears on the right hand side of the menu
				entry. If action is a Keybinding this value ignored and is
				taken from the Keybinding
		:mode: defines when the menu entry/action is executable
		:parent: the parent instance of this object. The only valid parent is Submenu
		"""
		object.__init__(self)
		self._lname = lname
		self._action = action
		self._rname = rname
		if mode not in (MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT):
			raise ValueError(u'Parameter mode not in MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT')
		self._mode = mode
		self.parent = parent

	def __str__(self):
		return u'%s\t%s' % (self.lname, self.rname)

	@property
	def lname(self):
		return self._lname.replace(u' ', u'\\ ')

	@property
	def action(self):
		if isinstance(self._action, Keybinding):
			return self._action.action
		return self._action

	@property
	def rname(self):
		if isinstance(self._action, Keybinding):
			return self._action.key.replace(u'<Tab>', u'Tab')
		return self._rname

	@property
	def mode(self):
		if isinstance(self._action, Keybinding):
			return self._action.mode
		return self._mode

	def create(self):
		menucmd = u':%smenu ' % self.mode
		menu = u''
		cmd = u''

		if self.parent:
			menu = self.parent.get_menu()
		menu += u'.%s' % self.lname

		if self.rname:
			cmd = u'%s %s<Tab>%s %s' % (menucmd, menu, self.rname, self.action)
		else:
			cmd = u'%s %s %s' % (menucmd, menu, self.action)

		vim.command(cmd.encode(u'utf-8'))

		# keybindings should be stored in the plugin.keybindings property and be registered by the appropriate keybinding registrar
		#if isinstance(self._action, Keybinding):
		#	self._action.create()
ftplugin/orgmode/__init__.py	[[[1
307
# -*- coding: utf-8 -*-

import vim

import types
import imp
import time

import orgmode.plugins
import orgmode.menu
import orgmode.keybinding
import orgmode.settings
from orgmode.document   import VimBuffer
from orgmode.exceptions import PluginError

from liborgmode import DIRECTION_FORWARD, DIRECTION_BACKWARD

REPEAT_EXISTS = bool(int(vim.eval('exists("*repeat#set()")')))
TAGSPROPERTIES_EXISTS = False

def realign_tags(f):
	u"""
	Update tag alignment, dependency to TagsProperties plugin!
	"""
	def r(*args, **kwargs):
		global TAGSPROPERTIES_EXISTS
		res = f(*args, **kwargs)

		if not TAGSPROPERTIES_EXISTS and u'TagsProperties' in ORGMODE.plugins:
			TAGSPROPERTIES_EXISTS = True

		if TAGSPROPERTIES_EXISTS:
			ORGMODE.plugins[u'TagsProperties'].realign_tags()

		return res
	return r

def repeat(f):
	u"""
	Integrate with the repeat plugin if available

	The decorated function must return the name of the <Plug> command to
	execute by the repeat plugin.
	"""
	def r(*args, **kwargs):
		res = f(*args, **kwargs)
		if REPEAT_EXISTS and isinstance(res, basestring):
			vim.command((u'silent! call repeat#set("\\<Plug>%s")' % res).encode(u'utf-8'))
		return res
	return r

def apply_count(f):
	u"""
	Decorator which executes function v:count or v:prevount (not implemented,
	yet) times. The decorated function must return a value that evaluates to
	True otherwise the function is not repeated.
	"""
	def r(*args, **kwargs):
		count = 0
		try:
			count = int(vim.eval(u'v:count'.encode('utf-8')))

			# visual count is not implemented yet
			#if not count:
			#	count = int(vim.eval(u'v:prevcount'.encode(u'utf-8')))
		except Exception, e:
			pass

		res = f(*args, **kwargs)
		count -= 1
		while res and count > 0:
			f(*args, **kwargs)
			count -= 1
		return res
	return r

def echo(message):
	u"""
	Print a regular message that will not be visible to the user when
	multiple lines are printed
	"""
	vim.command((u':echo "%s"' % message).encode(u'utf-8'))

def echom(message):
	u"""
	Print a regular message that will be visible to the user, even when
	multiple lines are printed
	"""
	# probably some escaping is needed here
	vim.command((u':echomsg "%s"' % message).encode(u'utf-8'))

def echoe(message):
	u"""
	Print an error message. This should only be used for serious errors!
	"""
	# probably some escaping is needed here
	vim.command((u':echoerr "%s"' % message).encode(u'utf-8'))

def insert_at_cursor(text, move=True, start_insertmode=False):
	u"""Insert text at the position of the cursor.

	If move==True move the cursor with the inserted text.
	"""
	d = ORGMODE.get_document(allow_dirty=True)
	line, col = vim.current.window.cursor
	_text = d._content[line - 1]
	d._content[line - 1] = _text[:col + 1] + text + _text[col + 1:]
	if move:
		vim.current.window.cursor = (line, col + len(text))
	if start_insertmode:
		vim.command(u'startinsert'.encode(u'utf-8'))

def get_user_input(message):
    u"""Print the message and take input from the user.
    Return the input.
    """
    vim.command(u'call inputsave()'.encode(u'utf-8'))
    vim.command((u"let user_input = input('" + message + u": ')").encode(u'utf-8'))
    vim.command(u'call inputrestore()'.encode(u'utf-8'))
    return vim.eval(u'user_input'.encode(u'utf-8')).decode(u'utf-8')

def indent_orgmode():
	u""" Set the indent value for the current line in the variable b:indent_level
	Vim prerequisites:
		:setlocal indentexpr=Method-which-calls-indent_orgmode

	:returns: None
	"""
	line = int(vim.eval(u'v:lnum'.encode(u'utf-8')))
	d = ORGMODE.get_document(allow_dirty=True)
	heading = d.find_current_heading(position=line - 1)
	if heading and line != heading.start_vim:
		vim.command((u'let b:indent_level = %d' % (heading.level + 1)).encode(u'utf-8'))

def fold_text():
	u""" Set the fold text
		:setlocal foldtext=Method-which-calls-foldtext

	:returns: None
	"""
	line = int(vim.eval(u'v:foldstart'.encode(u'utf-8')))
	d = ORGMODE.get_document(allow_dirty=True)
	heading = d.find_current_heading(position=line - 1)
	if heading:
		str_heading = unicode(heading)

		# expand tabs
		ts = int(vim.eval(u'&ts'.encode('utf-8')))
		idx = str_heading.find(u'\t')
		if idx != -1:
			tabs, spaces = divmod(idx, ts)

			str_heading = str_heading.replace(u'\t', u' '*(ts - spaces), 1)
			str_heading = str_heading.replace(u'\t', u' '*ts)

		vim.command((u'let b:foldtext = "%s... "' % (str_heading, )).encode('utf-8'))

def fold_orgmode():
	u""" Set the fold expression/value for the current line in the variable b:fold_expr
	Vim prerequisites:
		:setlocal foldmethod=expr
		:setlocal foldexpr=Method-which-calls-fold_orgmode

	:returns: None
	"""
	line = int(vim.eval(u'v:lnum'.encode(u'utf-8')))
	d = ORGMODE.get_document(allow_dirty=True)
	heading = d.find_current_heading(position=line - 1)
	if heading:
		if line == heading.start_vim:
			vim.command((u'let b:fold_expr = ">%d"' % heading.level).encode(u'utf-8'))
		#elif line == heading.end_vim:
		#	vim.command((u'let b:fold_expr = "<%d"' % heading.level).encode(u'utf-8'))
		# end_of_last_child_vim is a performance junky and is actually not needed
		#elif line == heading.end_of_last_child_vim:
		#	vim.command((u'let b:fold_expr = "<%d"' % heading.level).encode(u'utf-8'))
		else:
			vim.command((u'let b:fold_expr = %d' % heading.level).encode(u'utf-8'))

class OrgMode(object):
	u""" Vim Buffer """

	def __init__(self):
		object.__init__(self)
		self.debug = bool(int(orgmode.settings.get(u'org_debug', False)))

		self.orgmenu = orgmode.menu.Submenu(u'&Org')
		self._plugins = {}
		# list of vim buffer objects
		self._documents = {}

	def get_document(self, bufnr=0, allow_dirty=False):
		""" Retrieve instance of vim buffer document. This Document should be
		used for manipulating the vim buffer.

		:bufnr:			Retrieve document with bufnr
		:allow_dirty:	Allow the retrieved document to be dirty

		:returns:	vim buffer instance
		"""
		if bufnr == 0:
			bufnr = vim.current.buffer.number

		if bufnr in self._documents:
			if allow_dirty or self._documents[bufnr].is_insync:
				return self._documents[bufnr]
		self._documents[bufnr] = VimBuffer(bufnr).init_dom()
		return self._documents[bufnr]

	@property
	def plugins(self):
		return self._plugins.copy()

	@orgmode.keybinding.register_keybindings
	@orgmode.keybinding.register_commands
	@orgmode.menu.register_menu
	def register_plugin(self, plugin):
		if not isinstance(plugin, basestring):
			raise ValueError(u'Parameter plugin is not of type string')

		if plugin == u'|':
			self.orgmenu + orgmode.menu.Separator()
			self.orgmenu.children[-1].create()
			return

		if self._plugins.has_key(plugin):
			raise PluginError(u'Plugin %s has already been loaded')

		# a python module
		module = None

		# actual plugin class
		_class = None

		# locate module and initialize plugin class
		try:
			module = imp.find_module(plugin, orgmode.plugins.__path__)
		except ImportError, e:
			echom(u'Plugin not found: %s' % plugin)
			if self.debug:
				raise e
			return

		if not module:
			echom(u'Plugin not found: %s' % plugin)
			return

		try:
			module = imp.load_module(plugin, *module)
			if not hasattr(module, plugin):
				echoe(u'Unable to find plugin: %s' % plugin)
				if self.debug:
					raise PluginError(u'Unable to find class %s' % plugin)
				return
			_class = getattr(module, plugin)
			self._plugins[plugin] = _class()
			self._plugins[plugin].register()
			if self.debug:
				echo(u'Plugin registered: %s' % plugin)
			return self._plugins[plugin]
		except Exception, e:
			echoe(u'Unable to activate plugin: %s' % plugin)
			if self.debug:
				import traceback
				echoe(traceback.format_exc())

	def register_keybindings(self):
		@orgmode.keybinding.register_keybindings
		def dummy(plugin):
			return plugin

		for p in self.plugins.itervalues():
			dummy(p)

	def register_menu(self):
		self.orgmenu.create()

	def unregister_menu(self):
		vim.command(u'silent! aunmenu Org'.encode(u'utf-8'))

	def start(self):
		u""" Start orgmode and load all requested plugins
		"""
		plugins = orgmode.settings.get(u"org_plugins")

		if not plugins:
			echom(u'orgmode: No plugins registered.')

		if isinstance(plugins, basestring):
			try:
				self.register_plugin(plugins)
			except Exception, e:
				import traceback
				traceback.print_exc()
		elif isinstance(plugins, types.ListType) or \
				isinstance(plugins, types.TupleType):
			for p in plugins:
				try:
					self.register_plugin(p)
				except Exception, e:
					echoe('Error in %s plugin:' % p)
					import traceback
					traceback.print_exc()

		return plugins

ORGMODE = OrgMode()
ftplugin/orgmode/exceptions.py	[[[1
17
# -*- coding: utf-8 -*-

class PluginError(Exception):
	def __init__(self, message):
		Exception.__init__(self, message)

class BufferNotFound(Exception):
	def __init__(self, message):
		Exception.__init__(self, message)

class BufferNotInSync(Exception):
	def __init__(self, message):
		Exception.__init__(self, message)

class HeadingDomError(Exception):
	def __init__(self, message):
		Exception.__init__(self, message)
ftplugin/orgmode/settings.py	[[[1
68
# -*- coding: utf-8 -*-

import vim

# for all vim-orgmode buffers
SCOPE_GLOBAL = True

# just for the current buffer - has priority before the global settings
SCOPE_BUFFER = False

VARIABLE_LEADER = {SCOPE_GLOBAL: u'g', SCOPE_BUFFER: u'b'}

u""" Evaluate and store settings """

def get(setting, default=None):
	u""" Evaluate setting in scope of the current buffer,
	globally and also from the contents of the current buffer

	WARNING: Only string values are converted to unicode. If a different value
	is received, e.g. a list or dict, no conversion is done.

	:setting: name of the variable to evaluate
	:default: default value in case the variable is empty

	:returns: variable value
	"""
	# TODO first read setting from org file which take precedence over vim
	# variable settings
	if int(vim.eval((u'exists("b:%s")' % setting).encode(u'utf-8'))):
		res = vim.eval((u"b:%s" % setting).encode(u'utf-8'))
		if type(res) in (unicode, str):
			return res.decode(u'utf-8')
		return res

	elif int(vim.eval((u'exists("g:%s")' % setting).encode(u'utf-8'))):
		res = vim.eval((u"g:%s" % setting).encode(u'utf-8'))
		if type(res) in (unicode, str):
			return res.decode(u'utf-8')
		return res
	elif default is not None:
		return default

def set(setting, value, scope=SCOPE_GLOBAL, overwrite=False):
	u""" Store setting in the definied scope

	WARNING: For the return value, only string are converted to unicode. If a
	different value is received by vim.eval, e.g. a list or dict, no conversion
	is done.

	:setting: name of the setting
	:value: the actual value, repr is called on the value to create a string representation
	:scope: the scope o the setting/variable
	:overwrite: overwrite existing settings (probably user definied settings)

	:returns: the new value in case of overwrite==False the current value
	"""
	if not overwrite and int(vim.eval((u'exists("%s:%s")' % (VARIABLE_LEADER[scope], setting)).encode(u'utf-8'))):
		res = vim.eval((u'%s:%s' % (VARIABLE_LEADER[scope], setting)).encode(u'utf-8'))
		if type(res) in (unicode, str):
			return res.decode(u'utf-8')
		return res
	v = repr(value)
	if type(value) == unicode:
		# strip leading u of unicode string representations
		v = v[1:]

	vim.command((u'let %s:%s = %s' % (VARIABLE_LEADER[scope], setting, v)).encode(u'utf-8'))
	return value
ftplugin/orgmode/document.py	[[[1
336
# -*- coding: utf-8 -*-

from exceptions import BufferNotFound, BufferNotInSync
from liborgmode import Document, Heading, MultiPurposeList, DIRECTION_BACKWARD
import settings
import vim
from UserList import UserList

class VimBufferContent(MultiPurposeList):
	u""" Vim Buffer Content is a UTF-8 wrapper around a vim buffer. When
	retrieving or setting items in the buffer an automatic conversion is
	performed.

	This ensures UTF-8 usage on the side of liborgmode and the vim plugin
	vim-orgmode.
	"""

	def __init__(self, vimbuffer, on_change=None):
		MultiPurposeList.__init__(self, on_change=on_change)

		# replace data with vimbuffer to make operations change the actual
		# buffer
		self.data = vimbuffer

	def __contains__(self, item):
		i = item
		if type(i) is unicode:
			i = item.encode(u'utf-8')
		return MultiPurposeList.__contains__(self, i)

	def __getitem__(self, i):
		item = MultiPurposeList.__getitem__(self, i)
		if type(item) is str:
			return item.decode(u'utf-8')
		return item

	def __getslice__(self, i, j):
		return [item.decode(u'utf-8') if type(item) is str else item \
				for item in MultiPurposeList.__getslice__(self, i, j)]

	def __setitem__(self, i, item):
		_i = item
		if type(_i) is unicode:
			_i = item.encode(u'utf-8')

		MultiPurposeList.__setitem__(self, i, _i)

	def __setslice__(self, i, j, other):
		o = []
		o_tmp = other
		if type(o_tmp) not in (list, tuple) and not isinstance(o_tmp, UserList):
			o_tmp = list(o_tmp)
		for item in o_tmp:
			if type(item) == unicode:
				o.append(item.encode(u'utf-8'))
			else:
				o.append(item)
		MultiPurposeList.__setslice__(self, i, j, o)

	def __add__(self, other):
		raise NotImplementedError()
		# TODO: implement me
		if isinstance(other, UserList):
			return self.__class__(self.data + other.data)
		elif isinstance(other, type(self.data)):
			return self.__class__(self.data + other)
		else:
			return self.__class__(self.data + list(other))

	def __radd__(self, other):
		raise NotImplementedError()
		# TODO: implement me
		if isinstance(other, UserList):
			return self.__class__(other.data + self.data)
		elif isinstance(other, type(self.data)):
			return self.__class__(other + self.data)
		else:
			return self.__class__(list(other) + self.data)

	def __iadd__(self, other):
		o = []
		o_tmp = other
		if type(o_tmp) not in (list, tuple) and not isinstance(o_tmp, UserList):
			o_tmp = list(o_tmp)
		for i in o_tmp:
			if type(i) is unicode:
				o.append(i.encode(u'utf-8'))
			else:
				o.append(i)

		return MultiPurposeList.__iadd__(self, o)

	def append(self, item):
		i = item
		if type(item) is str:
			i = item.encode(u'utf-8')
		MultiPurposeList.append(self, i)

	def insert(self, i, item):
		_i = item
		if type(_i) is str:
			_i = item.encode(u'utf-8')
		MultiPurposeList.insert(self, i, _i)

	def index(self, item, *args):
		i = item
		if type(i) is unicode:
			i = item.encode(u'utf-8')
		MultiPurposeList.index(self, i, *args)

	def pop(self, i=-1):
		return MultiPurposeList.pop(self, i).decode(u'utf-8')

	def extend(self, other):
		o = []
		o_tmp = other
		if type(o_tmp) not in (list, tuple) and not isinstance(o_tmp, UserList):
			o_tmp = list(o_tmp)
		for i in o_tmp:
			if type(i) is unicode:
				o.append(i.encode(u'utf-8'))
			else:
				o.append(i)
		MultiPurposeList.extend(self, o)

class VimBuffer(Document):
	def __init__(self, bufnr=0):
		u"""
		:bufnr:		0: current buffer, every other number refers to another buffer
		"""
		Document.__init__(self)
		self._bufnr            = vim.current.buffer.number if bufnr == 0 else bufnr
		self._changedtick      = -1

		if self._bufnr == vim.current.buffer.number:
			self._content = VimBufferContent(vim.current.buffer)
		else:
			_buffer = None
			for b in vim.buffers:
				if self._bufnr == b.number:
					_buffer = b
					break

			if not _buffer:
				raise BufferNotFound(u'Unable to locate buffer number #%d' % self._bufnr)
			self._content = VimBufferContent(_buffer)

		self.update_changedtick()
		self._orig_changedtick = self._changedtick

	@property
	def tabstop(self):
		return int(vim.eval(u'&ts'.encode(u'utf-8')))

	@property
	def tag_column(self):
		return int(settings.get('org_tag_column', '77'))

	@property
	def is_insync(self):
		if self._changedtick == self._orig_changedtick:
			self.update_changedtick()
		return self._changedtick == self._orig_changedtick

	@property
	def bufnr(self):
		u"""
		:returns:	The buffer's number for the current document
		"""
		return self._bufnr

	def changedtick():
		""" Number of changes in vimbuffer """
		def fget(self):
			return self._changedtick
		def fset(self, value):
			self._changedtick = value
		return locals()
	changedtick = property(**changedtick())

	def update_changedtick(self):
		if self.bufnr == vim.current.buffer.number:
			self._changedtick = int(vim.eval(u'b:changedtick'.encode(u'utf-8')))
		else:
			vim.command(u'unlet! g:org_changedtick | let g:org_lz = &lz | let g:org_hidden = &hidden | set lz hidden'.encode(u'utf-8'))
			# TODO is this likely to fail? maybe some error hangling should be added
			vim.command((u'keepalt buffer %d | let g:org_changedtick = b:changedtick | buffer %d' % \
					(self.bufnr, vim.current.buffer.number)).encode(u'utf-8'))
			vim.command(u'let &lz = g:org_lz | let &hidden = g:org_hidden | unlet! g:org_lz g:org_hidden | redraw'.encode(u'utf-8'))
			self._changedtick = int(vim.eval(u'g:org_changedtick'.encode(u'utf-8')))

	def write(self):
		u""" write the changes to the vim buffer

		:returns:	True if something was written, otherwise False
		"""
		if not self.is_dirty:
			return False

		self.update_changedtick()
		if not self.is_insync:
			raise BufferNotInSync(u'Buffer is not in sync with vim!')

		# write meta information
		if self.is_dirty_meta_information:
			meta_end = 0 if self._orig_meta_information_len is None else self._orig_meta_information_len
			self._content[:meta_end] = self.meta_information
			self._orig_meta_information_len = len(self.meta_information)

		# remove deleted headings
		already_deleted = []
		for h in sorted(self._deleted_headings, cmp=lambda x, y: cmp(x._orig_start, y._orig_start), reverse=True):
			if h._orig_start is not None and h._orig_start not in already_deleted:
				# this is a heading that actually exists on the buffer and it
				# needs to be removed
				del self._content[h._orig_start:h._orig_start + h._orig_len]
				already_deleted.append(h._orig_start)
		del self._deleted_headings[:]
		del already_deleted

		# update changed headings and add new headings
		for h in self.all_headings():
			if h.is_dirty:
				if h._orig_start is not None:
					# this is a heading that existed before and was changed. It
					# needs to be replaced
					if h.is_dirty_heading:
						self._content[h.start:h.start + 1] = [unicode(h)]
					if h.is_dirty_body:
						self._content[h.start + 1:h.start + h._orig_len] = h.body
				else:
					# this is a new heading. It needs to be inserted
					self._content[h.start:h.start] = [unicode(h)] + h.body
				h._dirty_heading = False
				h._dirty_body = False
			# for all headings the length and start offset needs to be updated
			h._orig_start = h.start
			h._orig_len = len(h)

		self._dirty_meta_information = False
		self._dirty_document = False

		self.update_changedtick()
		self._orig_changedtick = self._changedtick
		return True

	def write_heading(self, heading, including_children=True):
		""" WARNING: use this function only when you know what you are doing!
		This function writes a heading to the vim buffer. It offers performance
		advantages over the regular write() function. This advantage is
		combined with no sanity checks! Whenever you use this function, make
		sure the heading you are writing contains the right offsets
		(Heading._orig_start, Heading._orig_len).

		Usage example:
			# Retrieve a potentially dirty document
			d = ORGMODE.get_document(allow_dirty=True)
			# Don't rely on the DOM, retrieve the heading afresh
			h = d.find_heading(direction=DIRECTION_FORWARD, position=100)
			# Update tags
			h.tags = ['tag1', 'tag2']
			# Write the heading
			d.write_heading(h)

		This function can't be used to delete a heading!

		:heading:				Write this heading with to the vim buffer
		:including_children:	Also include children in the update

		:returns				The written heading
		"""
		if including_children and heading.children:
			for child in heading.children[::-1]:
				self.write_heading(child, including_children)

		if heading.is_dirty:
			if heading._orig_start is not None:
				# this is a heading that existed before and was changed. It
				# needs to be replaced
				if heading.is_dirty_heading:
					self._content[heading._orig_start:heading._orig_start + 1] = [unicode(heading)]
				if heading.is_dirty_body:
					self._content[heading._orig_start + 1:heading._orig_start + heading._orig_len] = heading.body
			else:
				# this is a new heading. It needs to be inserted
				raise ValueError('Heading must contain the attribute _orig_start! %s' % heading)
			heading._dirty_heading = False
			heading._dirty_body = False
		# for all headings the length offset needs to be updated
		heading._orig_len = len(heading)

		return heading

	def previous_heading(self, position=None):
		u""" Find the next heading (search forward) and return the related object
		:returns:	 Heading object or None
		"""
		h = self.current_heading(position=position)
		if h:
			return h.previous_heading

	def current_heading(self, position=None):
		u""" Find the current heading (search backward) and return the related object
		:returns:	Heading object or None
		"""
		if position is None:
			position = vim.current.window.cursor[0] - 1
		for h in self.all_headings():
			if h.start <= position and h.end >= position:
				return h

	def next_heading(self, position=None):
		u""" Find the next heading (search forward) and return the related object
		:returns:	 Heading object or None
		"""
		h = self.current_heading(position=position)
		if h:
			return h.next_heading

	def find_current_heading(self, position=None, heading=Heading):
		u""" Find the next heading backwards from the position of the cursor.
		The difference to the function current_heading is that the returned
		object is not built into the DOM. In case the DOM doesn't exist or is
		out of sync this function is much faster in fetching the current
		heading.

		:position:	The position to start the search from

		:heading:	The base class for the returned heading

		:returns:	 Heading object or None
		"""
		return self.find_heading(vim.current.window.cursor[0] - 1 \
				if position is None else position, \
				direction=DIRECTION_BACKWARD, heading=heading, \
				connect_with_document=False)
ftplugin/orgmode/keybinding.py	[[[1
207
# -*- coding: utf-8 -*-

import vim

MODE_ALL = u'a'
MODE_NORMAL = u'n'
MODE_VISUAL = u'v'
MODE_INSERT = u'i'
MODE_OPERATOR = u'o'

OPTION_BUFFER_ONLY = u'<buffer>'
OPTION_SLIENT = u'<silent>'

def _register(f, name):
	def r(*args, **kwargs):
		p = f(*args, **kwargs)
		if hasattr(p, name) and isinstance(getattr(p, name), list):
			for i in getattr(p, name):
				i.create()
		return p
	return r

def register_keybindings(f):
	return _register(f, u'keybindings')

def register_commands(f):
	return _register(f, u'commands')

class Command(object):
	u""" A vim command """

	def __init__(self, name, command, arguments=u'0', complete=None, overwrite_exisiting=False):
		u"""
		:name:		The name of command, first character must be uppercase
		:command:	The actual command that is executed
		:arguments:	See :h :command-nargs, only the arguments need to be specified
		:complete:	See :h :command-completion, only the completion arguments need to be specified
		"""
		object.__init__(self)

		self._name                = name
		self._command             = command
		self._arguments           = arguments
		self._complete            = complete
		self._overwrite_exisiting = overwrite_exisiting

	def __unicode__(self):
		return u':%s<CR>' % self.name

	def __str__(self):
		return self.__unicode__().encode(u'utf-8')

	@property
	def name(self):
		return self._name

	@property
	def command(self):
		return self._command

	@property
	def arguments(self):
		return self._arguments

	@property
	def complete(self):
		return self._complete

	@property
	def overwrite_exisiting(self):
		return self._overwrite_exisiting

	def create(self):
		u""" Register/create the command
		"""
		vim.command((':command%(overwrite)s -nargs=%(arguments)s %(complete)s %(name)s %(command)s' %
				{u'overwrite': '!' if self.overwrite_exisiting else '',
					u'arguments': self.arguments.encode(u'utf-8'),
					u'complete': '-complete=%s' % self.complete.encode(u'utf-8') if self.complete else '',
					u'name': self.name,
					u'command': self.command}
				).encode(u'utf-8'))

class Plug(object):
	u""" Represents a <Plug> to an abitrary command """

	def __init__(self, name, command, mode=MODE_NORMAL):
		u"""
		:name: the name of the <Plug> should be ScriptnameCommandname
		:command: the actual command
		"""
		object.__init__(self)

		if mode not in (MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT, MODE_OPERATOR):
			raise ValueError(u'Parameter mode not in MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT, MODE_OPERATOR')
		self._mode = mode

		self.name = name
		self.command = command
		self.created = False

	def __unicode__(self):
		return u'<Plug>%s' % self.name

	def __str__(self):
		return self.__unicode__().encode(u'utf-8')

	def create(self):
		if not self.created:
			self.created = True
			cmd = self._mode
			if cmd == MODE_ALL:
				cmd = u''
			vim.command((u':%snoremap %s %s' % (cmd, str(self), self.command)).encode(u'utf-8'))

	@property
	def mode(self):
		return self._mode

class Keybinding(object):
	u""" Representation of a single key binding """

	def __init__(self, key, action, mode=None, options=None, remap=True, buffer_only=True, silent=True):
		u"""
		:key: the key(s) action is bound to
		:action: the action triggered by key(s)
		:mode: definition in which vim modes the key binding is valid. Should be one of MODE_*
		:option: list of other options like <silent>, <buffer> ...
		:repmap: allow or disallow nested mapping
		:buffer_only: define the key binding only for the current buffer
		"""
		object.__init__(self)
		self._key = key
		self._action = action

		# grab mode from plug if not set otherwise
		if isinstance(self._action, Plug) and not mode:
			mode = self._action.mode

		if mode not in (MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT, MODE_OPERATOR):
			raise ValueError(u'Parameter mode not in MODE_ALL, MODE_NORMAL, MODE_VISUAL, MODE_INSERT, MODE_OPERATOR')
		self._mode = mode
		self._options = options
		if self._options is None:
			self._options = []
		self._remap = remap
		self._buffer_only = buffer_only
		self._silent = silent

		if self._buffer_only and OPTION_BUFFER_ONLY not in self._options:
			self._options.append(OPTION_BUFFER_ONLY)

		if self._silent and OPTION_SLIENT not in self._options:
			self._options.append(OPTION_SLIENT)

	@property
	def key(self):
		return self._key

	@property
	def action(self):
		return str(self._action)

	@property
	def mode(self):
		return self._mode

	@property
	def options(self):
		return self._options[:]

	@property
	def remap(self):
		return self._remap

	@property
	def buffer_only(self):
		return self._buffer_only

	@property
	def silent(self):
		return self._silent

	def create(self):
		from orgmode import ORGMODE, echom

		cmd = self._mode
		if cmd == MODE_ALL:
			cmd = u''
		if not self._remap:
			cmd += u'nore'
		try:
			create_mapping = True
			if isinstance(self._action, Plug):
				# create plug
				self._action.create()
				if int(vim.eval((u'hasmapto("%s")' % (self._action, )).encode(u'utf-8'))):
					create_mapping = False
			if isinstance(self._action, Command):
				# create command
				self._action.create()

			if create_mapping:
				vim.command((u':%smap %s %s %s' % (cmd, u' '.join(self._options), self._key, self._action)).encode(u'utf-8'))
		except Exception, e:
			if ORGMODE.debug:
				echom(u'Failed to register key binding %s %s' % (self._key, self._action))
ftplugin/liborgmode.py	[[[1
1060
# -*- coding: utf-8 -*-

import re
from UserList import UserList

DIRECTION_FORWARD  = True
DIRECTION_BACKWARD = False

def flatten_list(l):
	res = []
	if type(l) in (tuple, list) or isinstance(l, UserList):
		for i in l:
			if type(i) in (list, tuple) or isinstance(i, UserList):
				res.extend(flatten_list(i))
			else:
				res.append(i)
	return res

class MultiPurposeList(UserList):
	u""" A Multi Purpose List is a list that calls a user defined hook on
	change. The impelementation is very basic - the hook is called without any
	parameters. Otherwise the Multi Purpose List can be used like any other
	list.

	The member element "data" can be used to fill the list without causing the
	list to be marked dirty. This should only be used during initialization!
	"""

	def __init__(self, initlist=None, on_change=None):
		UserList.__init__(self, initlist)
		self._on_change = on_change

	def _changed(self):
		u"""
		Call hook
		"""
		if callable(self._on_change):
			self._on_change()

	def __setitem__(self, i, item):
		UserList.__setitem__(self, i, item)
		self._changed()

	def __delitem__(self, i):
		UserList.__delitem__(self, i)
		self._changed()

	def __setslice__(self, i, j, other):
		UserList.__setslice__(self, i, j, other)
		self._changed()

	def __delslice__(self, i, j):
		UserList.__delslice__(self, i, j)
		self._changed()

	def __getslice__(self, i, j):
		# fix UserList - don't return a new list of the same type but just the
		# normal list item
		i = max(i, 0); j = max(j, 0)
		return self.data[i:j]

	def __iadd__(self, other):
		res = UserList.__iadd__(self, other)
		self._changed()
		return res

	def __imul__(self, n):
		res = UserList.__imul__(self, n)
		self._changed()
		return res

	def append(self, item):
		UserList.append(self, item)
		self._changed()

	def insert(self, i, item):
		UserList.insert(self, i, item)
		self._changed()

	def pop(self, i=-1):
		item = self[i]
		del self[i]
		return item

	def remove(self, item):
		self.__delitem__(self.index(item))

	def reverse(self):
		UserList.reverse(self)
		self._changed()

	def sort(self, *args, **kwds):
		UserList.sort(self, *args, **kwds)
		self._changed()

	def extend(self, other):
		UserList.extend(self, other)
		self._changed()

class HeadingList(MultiPurposeList):
	u"""
	A Heading List just contains headings. It's used for documents to store top
	level headings and for headings to store subheadings.

	A Heading List must be linked to a Document or Heading!

	See documenatation of MultiPurposeList for more information.
	"""
	def __init__(self, initlist=None, obj=None):
		"""
		:initlist:	Initial data
		:obj:		Link to a concrete Heading or Document object
		"""
		# it's not necessary to register a on_change hook because the heading
		# list will itself take care of marking headings dirty or adding
		# headings to the deleted headings list
		MultiPurposeList.__init__(self)

		self._obj = obj

		# initialization must be done here, because
		# self._document is not initialized when the
		# constructor of MultiPurposeList is called
		if initlist:
			self.extend(initlist)

	@classmethod
	def is_heading(cls, obj):
		return isinstance(obj, Heading)

	def _get_document(self):
		if self.__class__.is_heading(self._obj):
			return self._obj._document
		return self._obj

	def _add_to_deleted_headings(self, item):
		u"""
		Serialize headings so that all subheadings are also marked for deletion
		"""
		if not self._get_document():
			# HeadingList has not yet been associated
			return

		if type(item) in (list, tuple) or isinstance(item, UserList):
			for i in flatten_list(item):
				self._add_to_deleted_headings(i)
		else:
			self._get_document()._deleted_headings.append(item.copy(including_children=False))
			self._add_to_deleted_headings(item.children)
			self._get_document().set_dirty_document()

	def _associate_heading(self, heading, previous_sibling, next_sibling, children=False):
		"""
		:heading:		The heading or list to associate with the current heading
		:previous_sibling:	The previous sibling of the current heading. If
							heading is a list the first heading will be
							connected with the previous sibling and the last
							heading with the next sibling. The items in between
							will be linked with one another.
		:next_sibling:	The next sibling of the current heading. If
							heading is a list the first heading will be
							connected with the previous sibling and the last
							heading with the next sibling. The items in between
							will be linked with one another.
		:children:	Marks whether children are processed in the current
					iteration or not (should not be use, it's set automatically)
		"""
		# TODO this method should be externalized and moved to the Heading class
		if type(heading) in (list, tuple) or isinstance(heading, UserList):
			prev = previous_sibling
			current = None
			for _next in flatten_list(heading):
				if current:
					self._associate_heading(current, prev, _next, children=children)
					prev = current
				current = _next
			if current:
				self._associate_heading(current, prev, next_sibling, children=children)
		else:
			heading._orig_start = None
			heading._orig_len = None
			d = self._get_document()
			if heading._document != d:
				heading._document = d
			if not children:
				# connect heading with previous and next headings
				heading._previous_sibling = previous_sibling
				if previous_sibling:
					previous_sibling._next_sibling = heading
				heading._next_sibling = next_sibling
				if next_sibling:
					next_sibling._previous_sibling = heading

				if d == self._obj:
					# self._obj is a Document
					heading._parent = None
				elif heading._parent != self._obj:
					# self._obj is a Heading
					heading._parent = self._obj
			heading.set_dirty()

			self._associate_heading(heading.children, None, None, children=True)

	def __setitem__(self, i, item):
		if not self.__class__.is_heading(item):
			raise ValueError(u'Item is not a heading!')
		if item in self:
			raise ValueError(u'Heading is already part of this list!')
		self._add_to_deleted_headings(self[i])

		self._associate_heading(item, \
				self[i - 1] if i - 1 >= 0 else None, \
				self[i + 1] if i + 1 < len(self) else None)
		MultiPurposeList.__setitem__(self, i, item)

	def __setslice__(self, i, j, other):
		o = other
		if self.__class__.is_heading(o):
			o = (o, )
		o = flatten_list(o)
		for item in o:
			if not self.__class__.is_heading(item):
				raise ValueError(u'List contains items that are not a heading!')
		i = max(i, 0); j = max(j, 0)
		self._add_to_deleted_headings(self[i:j])
		self._associate_heading(o, \
				self[i - 1] if i - 1 >= 0 and i < len(self) else None, \
				self[j] if j >= 0 and j < len(self) else None)
		MultiPurposeList.__setslice__(self, i, j, o)

	def __delitem__(self, i):
		item = self[i]
		if item.previous_sibling:
			item.previous_sibling._next_sibling = item.next_sibling
		if item.next_sibling:
			item.next_sibling._previous_sibling = item.previous_sibling

		self._add_to_deleted_headings(item)
		MultiPurposeList.__delitem__(self, i)

	def __delslice__(self, i, j):
		i = max(i, 0); j = max(j, 0)
		items = self[i:j]
		if items:
			first = items[0]
			last = items[-1]
			if first.previous_sibling:
				first.previous_sibling._next_sibling = last.next_sibling
			if last.next_sibling:
				last.next_sibling._previous_sibling = first.previous_sibling
		self._add_to_deleted_headings(items)
		MultiPurposeList.__delslice__(self, i, j)

	def __iadd__(self, other):
		o = other
		if self.__class__.is_heading(o):
			o = (o, )
		for item in flatten_list(o):
			if not self.__class__.is_heading(item):
				raise ValueError(u'List contains items that are not a heading!')
		self._associate_heading(o, self[-1] if len(self) > 0 else None, None)
		return MultiPurposeList.__iadd__(self, o)

	def __imul__(self, n):
		# TODO das müsste eigentlich ein klonen von objekten zur Folge haben
		return MultiPurposeList.__imul__(self, n)

	def append(self, item):
		if not self.__class__.is_heading(item):
			raise ValueError(u'Item is not a heading!')
		if item in self:
			raise ValueError(u'Heading is already part of this list!')
		self._associate_heading(item, self[-1] if len(self) > 0 else None, None)
		MultiPurposeList.append(self, item)

	def insert(self, i, item):
		self._associate_heading(item, \
				self[i - 1] if i - 1 >= 0 and i - 1 < len(self) else None,
				self[i] if i >= 0 and i < len(self) else None)
		MultiPurposeList.insert(self, i, item)

	def pop(self, i=-1):
		item = self[i]
		self._add_to_deleted_headings(item)
		del self[i]
		return item

	def remove(self, item):
		self.__delitem__(self.index(item))

	def reverse(self):
		MultiPurposeList.reverse(self)
		prev_h = None
		for h in self:
			h._previous_sibling = prev_h
			h._next_sibling = None
			prev_h._next_sibling = h
			h.set_dirty()
			prev_h = h

	def sort(self, *args, **kwds):
		MultiPurposeList.sort(*args, **kwds)
		prev_h = None
		for h in self:
			h._previous_sibling = prev_h
			h._next_sibling = None
			prev_h._next_sibling = h
			h.set_dirty()
			prev_h = h

	def extend(self, other):
		o = other
		if self.__class__.is_heading(o):
			o = (o, )
		for item in o:
			if not self.__class__.is_heading(item):
				raise ValueError(u'List contains items that are not a heading!')
		self._associate_heading(o, self[-1] if len(self) > 0 else None, None)
		MultiPurposeList.extend(self, o)

REGEX_HEADING = re.compile(r'^(?P<level>\*+)(?P<todotitle>(\s+(?P<todo>[^\s]+))?(\s+(?P<title>.*?))?)\s*(\s(?P<tags>:[\w_:]+:))?$', flags=re.U|re.L)
REGEX_TAGS = re.compile(r'^\s*(?P<title>[^\s]*?)\s+(?P<tags>:[\w_:]+:)$', flags=re.U|re.L)
REGEX_TODO = re.compile(r'^[^\s]*$')

class Heading(object):
	u""" Structural heading object """

	def __init__(self, level=1, title=u'', tags=None, todo=None, body=None):
		u"""
		:level:		Level of the heading
		:title:		Title of the heading
		:tags:		Tags of the heading
		:todo:		Todo state of the heading
		:body:		Body of the heading
		"""
		object.__init__(self)

		self._document      = None
		self._parent        = None
		self._previous_sibling = None
		self._next_sibling  = None
		self._children      = HeadingList(obj=self)
		self._orig_start    = None
		self._orig_len      = 0

		self._dirty_heading = False
		self._level         = level
		self._todo          = None
		if todo: self.todo  = todo
		self._tags          = MultiPurposeList(on_change=self.set_dirty_heading)
		self._title         = u''
		if title: self.title = title
		if tags: self.tags   = tags

		self._dirty_body    = False
		self._body          = MultiPurposeList(on_change=self.set_dirty_body)
		if body: self.body  = body

	def __unicode__(self):
		res = u'*' * self.level
		if self.todo:
			res = u' '.join((res, self.todo))
		if self.title:
			res = u' '.join((res, self.title))

		# compute position of tags
		if self.tags:
			tabs = 0
			spaces = 2
			tags = (u':%s:' % (u':'.join(self.tags)))

			ts = 8
			tag_column = 77
			if self.document:
				ts = self.document.tabstop
				tag_column = self.document.tag_column

			len_heading = len(res)
			len_tags = len(tags)
			if len_heading + spaces + len_tags < tag_column:
				spaces_to_next_tabstop =  ts - divmod(len_heading, ts)[1]

				if len_heading + spaces_to_next_tabstop + len_tags < tag_column:
					tabs, spaces = divmod(tag_column - (len_heading + spaces_to_next_tabstop + len_tags), ts)

					if spaces_to_next_tabstop:
						tabs += 1
				else:
					spaces = tag_column - (len_heading + len_tags)

			res += u'\t' * tabs + u' ' * spaces + tags

		# append a trailing space when there are just * and no text
		if len(res) == self.level:
			res += u' '
		return res

	def __str__(self):
		return self.__unicode__().encode(u'utf-8')

	def __len__(self):
		# 1 is for the heading's title
		return 1 + len(self.body)

	def copy(self, including_children=True, parent=None):
		u"""
		Create a copy of the current heading. The heading will be completely
		detached and not even belong to a document anymore.

		:including_children:	If True a copy of all children is create as well. If False the returned heading doesn't have any children.
		"""
		heading = self.__class__(level=self.level, title=self.title, \
				tags=self.tags, todo=self.todo, body=self.body[:])
		if parent:
			parent.children.append(heading)
		if including_children and self.children:
			[item.copy(including_children=including_children, parent=heading) \
					for item in self.children]
		heading._orig_start    = self._orig_start
		heading._orig_len      = self._orig_len

		heading._dirty_heading = self.is_dirty_heading


		return heading

	@classmethod
	def parse_heading_from_data(cls, data, document=None, orig_start=None):
		u""" Construct a new heading from the provided data

		:document:		The document object this heading belongs to
		:data:			List of lines
		:orig_start:	The original start of the heading in case it was read
						from a document. If orig_start is provided, the
						resulting heading will not be marked dirty.

		:returns:	The newly created heading
	    """
		def parse_title(heading_line):
			# WARNING this regular expression fails if there is just one or no
			# word in the heading but a tag!
			m = REGEX_HEADING.match(heading_line)
			if m:
				r = m.groupdict()
				tags = filter(lambda x: x != u'', r[u'tags'].split(u':')) if r[u'tags'] else []

				# if there is just one or no word in the heading, redo the parsing
				mt = REGEX_TAGS.match(r[u'todotitle'])
				if not tags and mt:
					rt = mt.groupdict()
					tags = filter(lambda x: x != u'', rt[u'tags'].split(u':')) if rt[u'tags'] else []
					todo = rt[u'title']
					if not todo or todo == todo.upper():
						title = u''
					else:
						todo = None
						title = rt[u'title'].strip()
				else:
					todo = r[u'todo']
					if not todo or todo == todo.upper():
						title = r[u'title'] if r[u'title'] else u''
					else:
						todo = None
						title = r[u'todotitle'].strip()
				return (len(r[u'level']), todo, title, tags)
			raise ValueError(u'Data doesn\'t start with a heading definition.')

		if not data:
			raise ValueError(u'Unable to create heading, no data provided.')

		h = cls()
		h.level, h.todo, h.title, h.tags = parse_title(data[0])
		h.body = data[1:]
		if orig_start is not None:
			h._dirty_heading = False
			h._dirty_body    = False
			h._orig_start    = orig_start
			h._orig_len      = len(h)
		if document:
			h._document = document

		return h

	@classmethod
	def identify_heading(cls, line):
		u""" Test if a certain line is a heading or not.

		:line: the line to check

		:returns: level
		"""
		level = 0
		if not line:
			return None
		for i in xrange(0, len(line)):
			if line[i] == u'*':
				level += 1
				if len(line) > (i + 1) and line[i+1] in (u'\t', u' '):
					return level
			else:
				return None

	@property
	def is_dirty(self):
		u""" Return True if the heading's body is marked dirty """
		return self._dirty_heading or self._dirty_body

	@property
	def is_dirty_heading(self):
		u""" Return True if the heading is marked dirty """
		return self._dirty_heading

	@property
	def is_dirty_body(self):
		u""" Return True if the heading's body is marked dirty """
		return self._dirty_body

	def get_index_in_parent_list(self):
		""" Retrieve the index value of current heading in the parents list of
		headings. This works also for top level headings.

		:returns:	Index value or None if heading doesn't have a
					parent/document or is not in the list of headings
	    """
		if self.parent:
			if self in self.parent.children:
				return self.parent.children.index(self)
		elif self.document:
			if self in self.document.headings:
				return self.document.headings.index(self)

	def get_parent_list(self):
		""" Retrieve the parents list of headings. This works also for top
		level headings.

		:returns:	List of headings or None if heading doesn't have a
					parent/document or is not in the list of headings
	    """
		if self.parent:
			if self in self.parent.children:
				return self.parent.children
		elif self.document:
			if self in self.document.headings:
				return self.document.headings

	def set_dirty(self):
		u""" Mark the heading and body dirty so that it will be rewritten when
		saving the document """
		self._dirty_heading = True
		self._dirty_body = True
		if self._document:
			self._document.set_dirty_document()

	def set_dirty_heading(self):
		u""" Mark the heading dirty so that it will be rewritten when saving the
		document """
		self._dirty_heading = True
		if self._document:
			self._document.set_dirty_document()

	def set_dirty_body(self):
		u""" Mark the heading's body dirty so that it will be rewritten when
		saving the document """
		self._dirty_body = True
		if self._document:
			self._document.set_dirty_document()

	@property
	def document(self):
		u""" Read only access to the document. If you want to change the
		document, just assign the heading to another document """
		return self._document

	@property
	def parent(self):
		u""" Access to the parent heading """
		return self._parent

	@property
	def number_of_parents(self):
		u""" Access to the number of parent headings before reaching the root
		document """
		def count_parents(h):
			if h.parent:
				return 1 + count_parents(h.parent)
			else:
				return 0
		return count_parents(self)

	@property
	def previous_sibling(self):
		u""" Access to the previous heading that's a sibling of the current one
		"""
		return self._previous_sibling

	@property
	def next_sibling(self):
		u""" Access to the next heading that's a sibling of the current one """
		return self._next_sibling

	@property
	def previous_heading(self):
		u""" Serialized access to the previous heading """
		if self.previous_sibling:
			h = self.previous_sibling
			while h.children:
				h = h.children[-1]
			return h
		elif self.parent:
			return self.parent

	@property
	def next_heading(self):
		u""" Serialized access to the next heading """
		if self.children:
			return self.children[0]
		elif self.next_sibling:
			return self.next_sibling
		else:
			h = self.parent
			while h:
				if h.next_sibling:
					return h.next_sibling
				else:
					h = h.parent

	@property
	def start(self):
		u""" Access to the starting line of the heading """
		if self.document is None:
			return self._orig_start

		# static computation of start
		if not self.document.is_dirty:
			return self._orig_start

		# dynamic computation of start, really slow!
		def compute_start(h):
			if h:
				return len(h) + compute_start(h.previous_heading)
			return len(self.document.meta_information) if self.document.meta_information else 0
		return compute_start(self.previous_heading)

	@property
	def start_vim(self):
		if self.start is not None:
			return self.start + 1

	@property
	def end(self):
		u""" Access to the ending line of the heading """
		if self.start is not None:
			return self.start + len(self.body)

	@property
	def end_vim(self):
		if self.end is not None:
			return self.end + 1

	@property
	def end_of_last_child(self):
		u""" Access to end of the last child """
		if self.children:
			child = self.children[-1]
			while child.children:
				child = child.children[-1]
			return child.end
		return self.end

	@property
	def end_of_last_child_vim(self):
		return self.end_of_last_child + 1

	def children():
		u""" Subheadings of the current heading """
		def fget(self):
			return self._children
		def fset(self, value):
			v = value
			if type(v) in (list, tuple) or isinstance(v, UserList):
				v = flatten_list(v)
			self._children[:] = v
		def fdel(self):
			del self.children[:]
		return locals()
	children = property(**children())

	@property
	def first_child(self):
		u""" Access to the first child heading or None if no children exist """
		if self.children:
			return self.children[0]

	@property
	def last_child(self):
		u""" Access to the last child heading or None if no children exist """
		if self.children:
			return self.children[-1]

	def level():
		u""" Access to the heading level """
		def fget(self):
			return self._level
		def fset(self, value):
			self._level = int(value)
			self.set_dirty_heading()
		def fdel(self):
			self.level = None
		return locals()
	level = property(**level())

	def todo():
		u""" Todo state of current heading. When todo state is set, it will be
		converted to uppercase """
		def fget(self):
			# extract todo state from heading
			return self._todo
		def fset(self, value):
			# update todo state
			if type(value) not in (unicode, str, type(None)):
				raise ValueError(u'Todo state must be a string or None.')
			if value and not REGEX_TODO.match(value):
				raise ValueError(u'Found non allowed character in todo state! %s' % value)
			if not value:
				self._todo = None
			else:
				v = value
				if type(v) == str:
					v = v.decode(u'utf-8')
				self._todo = v.upper()
			self.set_dirty_heading()
		def fdel(self):
			self.todo = None
		return locals()
	todo = property(**todo())

	def title():
		u""" Title of current heading """
		def fget(self):
			return self._title.strip()
		def fset(self, value):
			if type(value) not in (unicode, str):
				raise ValueError(u'Title must be a string.')
			v = value
			if type(v) == str:
				v = v.decode(u'utf-8')
			self._title = v.strip()
			self.set_dirty_heading()
		def fdel(self):
			self.title = u''
		return locals()
	title = property(**title())

	def tags():
		u""" Tags of the current heading """
		def fget(self):
			return self._tags
		def fset(self, value):
			v = value
			if type(v) not in (list, tuple) or isinstance(v, UserList):
				v = list(unicode(v))
			if type(v) in (unicode, str):
				v = list(v)
			v = flatten_list(v)
			v_decoded = []
			for i in v:
				if type(i) not in (unicode, str):
					raise ValueError(u'Found non string value in tags! %s' % unicode(i))
				if u':' in i:
					raise ValueError(u'Found non allowed character in tag! %s' % i)
				i_tmp = i.strip().replace(' ', '_').replace('\t', '_')
				if type(i) == str:
					i_tmp = i.decode(u'utf-8')
				v_decoded.append(i_tmp)

			self._tags[:] = v_decoded
		def fdel(self):
			self.tags = []
		return locals()
	tags = property(**tags())

	def body():
		u""" Holds the content belonging to the heading """
		def fget(self):
			return self._body

		def fset(self, value):
			if type(value) in (list, tuple) or isinstance(value, UserList):
				self._body[:] = flatten_list(value)
			elif type(value) in (str, ):
				self._body[:] = value.decode('utf-8').split(u'\n')
			elif type(value) in (unicode, ):
				self._body[:] = value.split(u'\n')
			else:
				self.body = list(unicode(value))
		def fdel(self):
			self.body = []
		return locals()
	body = property(**body())

class Document(object):
	u""" Representation of a whole org-mode document """

	def __init__(self):
		u"""
		Don't call this constructor directly but use one of the concrete
		implementations.
		"""
		object.__init__(self)

		# is a list - only the Document methods should work with this list!
		self._content                   = None
		self._dirty_meta_information    = False
		self._dirty_document            = False
		self._meta_information          = MultiPurposeList(on_change = self.set_dirty_meta_information)
		self._orig_meta_information_len = None
		self._headings                  = HeadingList(obj=self)
		self._deleted_headings          = []

		# settings needed to align tags properly
		self._tabstop                    = 8
		self._tag_column                 = 77

	def __unicode__(self):
		if self.meta_information is None:
			return '\n'.join(self.all_headings())
		return '\n'.join(self.meta_information) + '\n' + '\n'.join(['\n'.join([unicode(i)] + i.body) for i in self.all_headings()])

	def __str__(self):
		return self.__unicode__().encode(u'utf-8')

	def tabstop():
		""" Tabstop for this document """
		def fget(self):
			return self._tabstop
		def fset(self, value):
			self._tabstop = value
		return locals()
	tabstop = property(**tabstop())

	def tag_column():
		""" The column all tags are right-aligned to """
		def fget(self):
			return self._tag_column
		def fset(self, value):
			self._tag_column = value
		return locals()
	tag_column = property(**tag_column())

	def init_dom(self, heading=Heading):
		u""" Initialize all headings in document - build DOM. This method
		should be call prior to accessing the document.

		:returns:	self
	    """
		def init_heading(_h):
			u"""
			:returns	the initialized heading
			"""
			start = _h.end + 1
			prev_heading = None
			while True:
				new_heading = self.find_heading(start, heading=heading)

				# * Heading 1 <- heading
				# * Heading 1 <- sibling
				# or
				# * Heading 2 <- heading
				# * Heading 1 <- parent's sibling
				if not new_heading or \
						new_heading.level <= _h.level:
					break

				# * Heading 1 <- heading
				#  * Heading 2 <- first child
				#  * Heading 2 <- another child
				new_heading._parent = _h
				if prev_heading:
					prev_heading._next_sibling = new_heading
					new_heading._previous_sibling = prev_heading
				_h.children.data.append(new_heading)
				# the start and end computation is only
				# possible when the new heading was properly
				# added to the document structure
				init_heading(new_heading)
				if new_heading.children:
					# skip children
					start = new_heading.end_of_last_child + 1
				else:
					start = new_heading.end + 1
				prev_heading = new_heading

			return _h

		h = self.find_heading(heading=heading)
		# initialize meta information
		if h:
			self._meta_information.data.extend(self._content[:h._orig_start])
		else:
			self._meta_information.data.extend(self._content[:])
		self._orig_meta_information_len = len(self.meta_information)

		# initialize dom tree
		prev_h = None
		while h:
			if prev_h:
				prev_h._next_sibling = h
				h._previous_sibling = prev_h
			self.headings.data.append(h)
			init_heading(h)
			prev_h = h
			h = self.find_heading(h.end_of_last_child + 1, heading=heading)

		return self

	def meta_information():
		u"""
		Meta information is text that precedes all headings in an org-mode
		document. It might contain additional information about the document,
		e.g. author
		 """
		def fget(self):
			return self._meta_information

		def fset(self, value):
			if self._orig_meta_information_len is None:
				self._orig_meta_information_len = len(self.meta_information)
			if type(value) in (list, tuple) or isinstance(value, UserList):
				self._meta_information[:] = flatten_list(value)
			elif type(value) in (str, ):
				self._meta_information[:] = value.decode(u'utf-8').split(u'\n')
			elif type(value) in (unicode, ):
				self._meta_information[:] = value.split(u'\n')
			self.set_dirty_meta_information()
		def fdel(self):
			self.meta_information = u''
		return locals()
	meta_information = property(**meta_information())

	def headings():
		u""" List of top level headings """
		def fget(self):
			return self._headings
		def fset(self, value):
			self._headings[:] = value
		def fdel(self):
			del self.headings[:]
		return locals()
	headings = property(**headings())

	def write(self):
		u""" write the document

		:returns:	True if something was written, otherwise False
		"""
		raise NotImplementedError(u'Abstract method, please use concrete impelementation!')

	def set_dirty_meta_information(self):
		u""" Mark the meta information dirty so that it will be rewritten when
		saving the document """
		self._dirty_meta_information = True

	def set_dirty_document(self):
		u""" Mark the whole document dirty. When changing a heading this
		method must be executed in order to changed computation of start and
		end positions from a static to a dynamic computation """
		self._dirty_document = True

	@property
	def is_dirty(self):
		u"""
		Return information about unsaved changes for the document and all
		related headings.

		:returns:	 Return True if document contains unsaved changes.
		"""
		if self.is_dirty_meta_information:
			return True

		if self.is_dirty_document:
			return True

		if self._deleted_headings:
			return True

		return False

	@property
	def is_dirty_meta_information(self):
		u""" Return True if the meta information is marked dirty """
		return self._dirty_meta_information

	@property
	def is_dirty_document(self):
		u""" Return True if the document is marked dirty """
		return self._dirty_document

	def all_headings(self):
		u""" Iterate over all headings of the current document in serialized
		order

		:returns:	Returns an iterator object which returns all headings of
					the current file in serialized order
		"""
		if not self.headings:
			raise StopIteration()

		h = self.headings[0]
		while h:
			yield h
			h = h.next_heading
		raise StopIteration()

	def find_heading(self, position=0, direction=DIRECTION_FORWARD, \
			heading=Heading, connect_with_document=True):
		u""" Find heading in the given direction

		:postition:	starting line, counting from 0 (in vim you start counting from 1, don't forget)
		:direction:	downwards == DIRECTION_FORWARD, upwards == DIRECTION_BACKWARD
		:heading:	Heading class from which new heading objects will be instanciated
		:connect_with_document:	if True, the newly created heading will be connected with the document, otherwise not

		:returns:	New heading object or None
		"""
		len_cb = len(self._content)

		if position < 0 or position > len_cb:
			return

		tmp_line = position
		start = None
		end = None

		# Search heading upwards
		if direction == DIRECTION_FORWARD:
			while tmp_line < len_cb:
				if heading.identify_heading(self._content[tmp_line]) is not None:
					if start is None:
						start = tmp_line
					elif end is None:
						end = tmp_line - 1
					if start is not None and end is not None:
						break
				tmp_line += 1
		else:
			while tmp_line >= 0 and tmp_line < len_cb:
				if heading.identify_heading(self._content[tmp_line]) is not None:
					if start is None:
						start = tmp_line
					elif end is None:
						end = tmp_line - 1
					if start is not None and end is not None:
						break
				tmp_line -= 1 if start is None else -1

		if start is not None and end is None:
			end = len_cb - 1
		if start is not None and end is not None:
			return heading.parse_heading_from_data(self._content[start:end + 1], \
					document=self if connect_with_document else None, orig_start=start)
doc/org.txt	[[[1
206
*org.txt*		For Vim version 7.3		Last change: 2011 June 25

==============================================================================
Table of Contents						   *org* *org-toc*

    1. Description			|org-description|
    2. Installation			|org-installation|
    3. Usage				|org-usage|
    4. Links				|org-links|
    5. Changelog			|org-changelog|
    6. License				|org-license|

==============================================================================
Description						       *org-description*

vim-orgmode is an attempt to port org-mode (http://orgmode.org) to vim.

==============================================================================
Installation						      *org-installation*

Prerequisites~
   Add the following lines to your .vimrc file to ensure that filetype plugins
   are loaded properly:

>
	filetype on
	filetype plugin on
	filetype indent on
<

   Please install the Universal Text Linking
   (http://www.vim.org/scripts/script.php?script_id=293) addon, otherwise
   hyperlinks won't work. Other plugins that integrate well with vim orgmode
   are listed in section Suggested plugins.

From .vba file~
   If you want to install the vim-orgmode plugin for a single user, this is
   the preferred way.

   Open the file in vim and source it. Restart vim and the plugin is active:

   $ vim orgmode.vba
>
    :so %
<

Installation into a specific directory~
    If you want to install the plugin into a specific directory, e.g. when you
    are using pathogen, then just add the desired directory to the runtimepath
    before sourcing the vba-file.

    $ vim orgmode.vba.gz
>
    :set rtp=$HOME/.vim/bundle/orgmode,&rtp
    :so %
<

From .deb file~
   If you want to install the vim-orgmode plugin for all users of your
   computer, this is the preferred way.

   Install the plugin by using the Debian Package Manager:

   dpkg -i vim-orgmode_X.X.X-X.deb

   The plugin is installed in /usr/lib/vim/addons. Please make sure this
   directory part of your runtimepath. By default it's not! Add the following
   command to your .vimrc to add the path on startup:

>
   :set rtp=/usr/lib/vim/addons,&rtp
<

From git checkout~
   This method is mainly used for development purposes.

You are using pathegon~

    Change to the bundle directory
    $ cd ~/.vim/bundle

    Clone the repository
    $ git clone https://github.com/jceb/vim-orgmode.git

    And restart vim. That's it.

You are using no addon manager~
    Clone the repository:
    $ git clone https://github.com/jceb/vim-orgmode.git

    Copy the directories doc, ftdetect, ftplugin, indent and syntax to
    your $HOME/.vim directory:
    $ cd vim-orgmode
    $ cp -r doc ftdetect ftplugin indent syntax ~/.vim/

==============================================================================
Usage								     *org-usage*
  vim-orgmode aims to be clone of the original orgmode for Emacs. Since Emacs
  is not vim the clone is not aiming for 100% compatibility.  Especially in
  terms of the keybindings there will be major differences!

  You'll definitively enjoy the modal interface, this where vim's strength is.
  Almost all keybindings for orgmode work only in normal and visual mode where
  as in insert mode just a few are available.

  Start of with vim-orgmode by open a file with the extension .org. An
  additional menu "Org" is shown that gives an overview of the implemented
  functionality and the keybindings.

Text objects~
   Vim offers a mighty feature called text-objects. A text-object is bound to
   a certain character sequence that can be used in combination with all kinds
   of editing and selection tasks. vim-orgmode offers the following
   text-objects:

   ih - inner heading, referring to the current heading excluding the heading
        level characters (*)
   ah - a heading, referring to the current heading including everything
   it - inner subtree, starting with the current heading
   at - a subtree, starting with the current heading
   Oh - inner outer heading, referring to the parent
   Ot - inner outer heading, including subtree, referring to the parent
   OH - a outer heading
   OT - a outer subtree

   Movement commands can also be used for editing like text-objects:

   g{ - execute command from current position to the beginning of the parent
        heading
   {  - execute command from current position to the beginning of the current
        heading
   }  - execute command from current position to the end of the current
        heading
   [[ - execute command from current position to the beginning of the previous
        heading sibling
   ]] - execute command from current position to the end of the next heading
        sibling

   For further information please read :h text-objects-changed

Suggested plugins~
  - pathogen (http:/www.vim.org/scripts/script.php?script_id=2332)
    easy management of multiple vim plugins

  - repeat (http://www.vim.org/scripts/script.php?script_id=2136)
    repeat actions that would not be repeatable otherwise

  - taglist ([http://www.vim.org/scripts/script.php?script_id=273)
    display tags for the currently edited file

  - tagbar (http://www.vim.org/scripts/script.php?script_id=3465)
    a new approach to displaying tags for the currently edited file

  - speeddating (http://www.vim.org/scripts/script.php?script_id=2120)
    in-/decrease dates the vim way: C-a and C-x

  - Narrow Region (http://www.vim.org/scripts/script.php?script_id=3075)
    emulation of Emacs' Narrow Region feature

  - Universal Text Linking
    (http://www.vim.org/scripts/script.php?script_id=293) general support for
    text linking, the Hyperlinks feature of vim-orgmode depends on this plugin

==============================================================================
Links								     *org-links*

Original org-mode for Emacs (http://orgmode.org)

VimOrganizer, another vim port of org-mode
(http://www.vim.org/scripts/script.php?script_id=3342)

==============================================================================
Changelog							 *org-changelog*

0.2.0-0, 2011-06-25 initial release

==============================================================================
License								   *org-license*

Copyright (C) 2010,2011 Jan Christoph Ebersbach

http://www.e-jc.de/

All rights reserved.

The source code of this program is made available under the terms of the GNU
Affero General Public License version 3 (GNU AGPL V3) as published by the Free
Software Foundation.

Binary versions of this program provided by Univention to you as well as other
copyrighted, protected or trademarked materials like Logos, graphics, fonts,
specific documentations and configurations, cryptographic keys etc. are
subject to a license agreement between you and Univention and not subject to
the GNU AGPL V3.

In the case you use this program under the terms of the GNU AGPL V3, the
program is provided in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
details.

You should have received a copy of the GNU Affero General Public License with
the Debian GNU/Linux or Univention distribution in file
/usr/share/common-licenses/AGPL-3; if not, see <http://www.gnu.org/licenses/>.

 vim:tw=78:ts=8:ft=help:norl:
indent/org.vim	[[[1
54
" Delete the next line to avoid the special indention of items
if !exists("g:org_indent")
  let g:org_indent = 0
endif

setlocal foldtext=GetOrgFoldtext()
setlocal fillchars-=fold:-
setlocal fillchars+=fold:\ 
setlocal foldexpr=GetOrgFolding()
setlocal foldmethod=expr
setlocal indentexpr=GetOrgIndent()
setlocal nolisp
setlocal nosmartindent
setlocal autoindent

function! GetOrgIndent()
python << EOF
from orgmode import indent_orgmode
indent_orgmode()
EOF
	if exists('b:indent_level')
		let tmp = b:indent_level
		unlet b:indent_level
		return tmp
	else
		return -1
	endif
endfunction

function! GetOrgFolding()
python << EOF
from orgmode import fold_orgmode
fold_orgmode()
EOF
	if exists('b:fold_expr')
		let tmp = b:fold_expr
		unlet b:fold_expr
		return tmp
	else
		return -1
	endif
endfunction

function! GetOrgFoldtext()
python << EOF
from orgmode import fold_text
fold_text()
EOF
	if exists('b:foldtext')
		let tmp = b:foldtext
		unlet b:foldtext
		return tmp
	endif
endfunction
