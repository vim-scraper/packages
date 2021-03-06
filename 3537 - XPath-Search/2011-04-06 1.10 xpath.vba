" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/xpath.txt	[[[1
65
*xpath.txt* Plugin for XPath search in XML files

This plugin adds XPath search functionality to Vim. 

|xpath-installation|	Installing the plugin
|xpath-usage|		Using the plugin
|xpath-configure|	Plugin configuration options

==============================================================================
INSTALLATION						*xpath-installation*

To install the plugin, extract the directory structure into your vimfiles, or 
source the |vimball|.

For the plugin to run, you'll need python support enabled. You can check this
with |:version|, and looking for +python.

You'll also need python-lxml installed (ideally 2.7 or higher).

Without these the plugin will let you know on startup, then stop loading.

==============================================================================
USAGE							*xpath-usage*

When you open a file which the plugin recognises (by default: xml, xslt - to 
define additional filetypes see |xpath-configure|), the mapping X in normal
mode will launch an XPath search prompt. Type your xpath in here, then hit 
<Enter> to open a new results window.

The search prompt supports context-sensitive tab completion. To tab complete
attributes in the current context, type an @ sign at the beginning of the
current context (e.g. //Parent/Child/@<Tab> will complete all attributes
for Child).

When you do a search, the cursor will jump to the first result in the results
window. If the result has a line number, you can also press <Enter> on the
highlighted result, and the cursor will jump to the line in your XML file
that matched the search.

If the XML file is not valid, the plugin will let you know of any XML 
validation errors. Again if there is a line number associated with the error
you can hit <Enter> to jump there.

Pressing X in normal mode on the results window will cause it to close again.

XPath search defines several functions, but the main interface to the plugin 
is through the XPathSearchPrompt() function, which launches the search input 
prompt.

==============================================================================
CONFIGURATION						*xpath-configure*

To disable the plugin for certain situtations, add the following in your
.vimrc:

let g:loaded_xpath = 1

To add additional filetypes to add the normal mode X mapping to, append a list
of the filetypes to the g:xpath_search_filetypes variable. For example, to 
enable the plugin for htm and html files, you can use the following:

let g:xpath_search_filetypes += ['htm', 'html']

To display the filetype for a file you currently have open in order to set up 
a new match, you can use the command :set filetype.
plugin/xpath.vim	[[[1
190
"Vim global plugin for XPATH search
"Last Change:	2011 Apr 02
"Maintainer:	Dave Aitken <dave.aitken@gmail.com>
"License:	This file is placed in the public domain.

if exists("g:loaded_xpath")
	finish
endif
let g:loaded_xpath = 1

let g:xpath_search_filetypes = ['xml', 'xslt']

if !(has("python"))
	echoerr "XPath plugin not loaded due to no python support."
	finish
endif

py import vim, re

let s:scriptfile = expand("<sfile>")
execute "pyfile ".fnameescape(fnamemodify(s:scriptfile, ":h"). "/../xpath/xpath.py")

py vim.command("let lxml_imported = " + str(LXML_IMPORTED))

if !(lxml_imported)
	echoerr "XPath plugin not loaded - python lxml is required"
	finish
endif

"Used to track the results buffer
let s:results_buffer_name = 'xpath_search_results'
let s:result_pattern = '^|\(\d\+\).*$'

autocmd FileType * :call XPathFileType(expand("<amatch>"))

function! XPathFileType(bufft)
	for ft in g:xpath_search_filetypes
		if (a:bufft == ft)
			nnoremap <buffer> X :call XPathSearchPrompt()<cr>
		end
	endfor
endfunction

function! XPathSearchPrompt()

	let s:search_buffer = bufnr('%')
	silent call XPathResultsSplit(s:search_buffer)

	call inputsave()
	let l:xpath = input("XPath: ", "//", "customlist,XPathSearchPromptCompletion")
	call inputrestore()

	if l:xpath != ""
		call XPathSearch(l:xpath, s:search_buffer)
		call JumpToFirstXPathResult()
	endif

endfunction

function! XPathSearchPromptCompletion(lead, line, pos)

	py xpath = vim.eval("a:line")
	py search_buffer_name = vim.eval("bufname('%')")
	py completions = xpath_interface.get_completions(search_buffer_name, xpath)

	py vim.command("let l:complist = " + str(completions))

	redraw

	return l:complist

endfunction

function! XPathSearch(xpath, search_buffer)

	let l:search_window = bufwinnr(a:search_buffer)

	let [l:results_buffer, l:results_window] = XPathResultsSplit(a:search_buffer)
	
	py xpath = vim.eval("a:xpath")
	py search_buffer_name = vim.eval("bufname('%')")

	py xpath_interface.xpath_search(search_buffer_name, xpath)

endfunction

function! JumpToFirstXPathResult()

	let l:results_buffer = bufnr('^' . s:results_buffer_name . '$')
	let l:search_window = bufwinnr(l:results_buffer)
	exe l:search_window . 'wincmd w'

	call search(s:result_pattern)

endfunction

function! XPathResultsSplit(search_buffer)

	let l:not_loaded = -1

	let l:results_buffer = bufnr('^' . s:results_buffer_name . '$')
	
	"Create a results buffer if one doesn't exist
	if l:results_buffer == l:not_loaded
		let l:results_buffer = CreateXPathResultsBuffer(s:results_buffer_name)

		py results_buffer_name = vim.eval('s:results_buffer_name')
		py xpath_interface = VimXPathInterface(vim, results_buffer_name)

	endif

	let l:results_window = bufwinnr(l:results_buffer)

	"Create a results window if one doesn't exist
	if l:results_window == l:not_loaded
		let l:results_window = CreateXPathResultsWindow(l:results_buffer)
	endif

	call SetupXPathResultsWindow(l:results_window, a:search_buffer)

	return [l:results_buffer, l:results_window]

endfunction

function! CreateXPathResultsBuffer(results_buffer_name)
	exe 'badd ' . a:results_buffer_name
	let l:results_buffer = bufnr('^' . a:results_buffer_name . '$')
	return l:results_buffer
endfunction

function! CreateXPathResultsWindow(results_buffer)
	below 10new
	exe 'buffer ' . a:results_buffer
	let l:results_window = bufwinnr(a:results_buffer)
	return l:results_window
endfunction

function! SetupXPathResultsWindow(results_window, search_buffer)
	exe a:results_window . 'wincmd w'

	call SetupXPathResultsBuffer(a:search_buffer)
	
	let l:search_window = bufwinnr(a:search_buffer)
	exe l:search_window . 'wincmd w'
endfunction

function! SetupXPathResultsBuffer(search_buffer)
	"These commands must be called when the 
	"current window is the results window
	setlocal buftype=nofile bufhidden=hide noswapfile syntax=xpathresults nowrap wfh

	nmap <buffer> <silent> X :q<cr>
	autocmd CursorMoved <buffer> :call XPathResultsCursorlineCheck()
	autocmd VimResized <buffer> :py xpath_interface.window_resized()

	if bufname(a:search_buffer) != s:results_buffer_name
		let s:search_buffer_name  = bufname(a:search_buffer)
		exe "nmap <buffer> <silent> <cr> :call XPathJumpToResult(" . a:search_buffer . ")<cr>"
	endif
endfunction

function! XPathJumpToResult(search_buffer)

	let l:current_line = getline('.')
	let l:line_number_pattern_results = matchlist(l:current_line, s:result_pattern)
	try

		let l:result_line = l:line_number_pattern_results[1]

		let l:search_window = bufwinnr(a:search_buffer)
		exe l:search_window . 'wincmd w'

		exe l:result_line
		normal zv

	catch /E684:/
	endtry

endfunction

function! XPathResultsCursorlineCheck()

	let l:syntax_under_cursor = synIDattr(synID(line("."), col("."), 1), "name")

	if (l:syntax_under_cursor == 'CurrentXPathResult')
		setlocal cursorline
	else
		setlocal nocursorline
	endif
endfunction
syntax/xpathresults.vim	[[[1
4
syntax match XPathResult /^|\d\+.*$/
syntax match CurrentXPathResult /\(\(^|\d\+.*\%#.*$\)\|\(^|\%#\d\+.*$\)\|\(^\%#|\d\+.*$\)\)/

hi link CurrentXPathResult Error
xpath/xpath.py	[[[1
508
#-*- encoding:utf-8 -*-
import re

try:
	from lxml import etree
	LXML_IMPORTED = 1
except ImportError:
	LXML_IMPORTED = 0

class VimXPathInterface(object):

	def __init__(self, vim, results_buffer_name):
		self.buffer_manager = self.build_buffer_manager(vim, results_buffer_name)
		self.searcher = XPathSearcher()

		self.previous = {'xpath': None, 'search_buffer_name': None}

	def build_buffer_manager(self, vim, result_buffer_name):
		buffer_manager = VimBufferManager(vim)
		buffer_manager.define_buffer('results', results_buffer_name)
		return buffer_manager

	def xpath_search(self, search_buffer_name, xpath):
		results = self.get_search_results(search_buffer_name, xpath)
		self.output_results(xpath, results)

		self.previous['xpath'] = xpath
		self.previous['search_buffer_name'] = search_buffer_name

	def get_search_results(self, search_buffer_name, xpath):
		self.prep_searcher(search_buffer_name)
		results = self.searcher.search(xpath)

		return results

	def get_completions(self, search_buffer_name, xpath):
		results = self.get_completion_search_results(search_buffer_name, xpath)

		formatter = CompletionFormatter(results)
		results_list = formatter.get_formatted_list()
		results_set = list(set(results_list))
		results_set.sort()

		return results_set

	def get_completion_search_results(self, search_buffer_name, xpath):
		self.prep_searcher(search_buffer_name)
		results = self.searcher.completion_search(xpath)

		return results

	def prep_searcher(self, search_buffer_name):
		if search_buffer_name != self.buffer_manager.defined_buffers['results']:
			search_buffer = self.buffer_manager.get_buffer(search_buffer_name)

			search_text = self.buffer_manager.get_buffer_content(search_buffer)
			self.searcher.build_tree(search_text)

	def output_results(self, xpath, results):
		results_buffer = self.buffer_manager.get_defined_buffer('results')
		results_window = self.buffer_manager.get_window(results_buffer)

		width = results_window.width
		formatter = ResultsFormatter(width, xpath, results)
		lines = formatter.get_formatted_lines()

		self.buffer_manager.set_buffer_content(results_buffer, lines)


	def window_resized(self):
		if self.previous['xpath'] is not None:
			self.xpath_search(self.previous['search_buffer_name'], self.previous['xpath'])

class VimBufferManager(object):

	def __init__(self, vim):
		self.vim = vim
		self.defined_buffers = {}

	def define_buffer(self, defname, buffer_name):
		self.defined_buffers[defname] = buffer_name

	def get_defined_buffer(self, defname):
		buffer_name = self.defined_buffers[defname]
		return self.get_buffer(buffer_name)

	def get_buffer(self, buffer_name):
		if buffer_name is None:
			return self.vim.current.buffer

		for buf in [b for b in self.vim.buffers if b.name is not None] :
			if buf.name.endswith(buffer_name):
				return buf

		return None

	def set_buffer_content(self, buffer, lines):
		del buffer[:]
		for l in lines:
			buffer.append(l)

		del buffer[0]

	def get_buffer_content(self, buffer):
		content = '\n'.join(buffer)
		return content

	def get_window(self, buffer):
		for w in self.vim.windows:
			if w.buffer.name == buffer.name:
				return w

		return None

class XPathSearcher(object):

	def __init__(self):
		self.cached_search_text = None
		self.xml_tree = None

		self.cache = {'xml': None, 'tree': None, 'eval': None, 'error': None}

	def build_tree(self, xml):
		if self.cache['xml'] != xml:
			self.cache['xml'] = xml
			try:
				self.cache['tree'] = etree.XML(xml)
				self.cache['eval'] = etree.XPathEvaluator(self.cache['tree'])
				self.cache['error'] = None

			except Exception as xmlerr:
				err_text = str(xmlerr)
				self.cache['error'] = XPathParseErrorResult(err_text)

	def search(self, xpath):
		if self.cache['error'] is None:
			try:
				raw_results = self.cache['eval'](xpath)
				results = self.parse_results(raw_results)

			except Exception as xpatherr:
				err_text = str(xpatherr)
				results = [XPathSearchErrorResult(err_text, xpath)]
		else:
			results = [self.cache['error']]

		return results

	def completion_search(self, xpath):

		split = xpath.split('|')

		results_base = '|'.join(split[:-1])
		if len(split) > 1:
			results_base += '|'

		completion_seed = split[-1]

		partition = completion_seed.rpartition('/')

		xpath_base = ''.join(partition[:-1])
		results_base += xpath_base

		search_name = partition[-1]
		if search_name.startswith('@'):
			xpath_type_base  = '@'
			search_name = search_name[1:]
		else:
			xpath_type_base = ''

		xpath_expr = xpath_base + xpath_type_base + "*[starts-with(name(), '" + search_name + "')]"
		results = self.search(xpath_expr)

		return {'base': results_base, 'results': results}

	def parse_results(self, raw_results):
		results = []

		for r in raw_results:
			parsed = self.parse(r)
			results.append(parsed)

		return results

	def parse(self, raw_result):
		parse_class = self.get_parse_class(raw_result)
		parsed = parse_class(raw_result)

		return parsed

	def get_parse_class(self, raw_result):
		result = XPathTagResult
		if isinstance(raw_result, etree._ElementStringResult):
			if raw_result.is_attribute:
				result = XPathAttrResult

		return result

class TBL(object):
#	TL = '┏'; TC = '┳'; TR = '┓'; T = '━'
#	HL = '┣'; HC = '╋'; HR = '┫'; H = '━'
#	ML = '┃'; MC = '┃'; MR = '┃'; M = '━'
#	BL = '┗'; BC = '┻'; BR = '┛'; B = '━'

	TL = '+'; TC = '+'; TR = '+'; T = '-'
	HL = '+'; HC = '+'; HR = '+'; H = '-'
	ML = '|'; MC = '|'; MR = '|'; M = '-'
	BL = '+'; BC = '+'; BR = '+'; B = '-'


class ResultsFormatter(object):

	def __init__(self, window_width, xpath, results):

		self.xpath_string = xpath
		self.width = window_width

		results_contain_errors = False

		for r in results:
			if isinstance(r, XPathErrorResult):
				results_contain_errors = True
				break

		if results_contain_errors:
			columns = [
					ResultsFormatterTableColumn('line', 'Line', contract_contents=False, expand_target_pct=5),
					ResultsFormatterTableColumn('column', 'Column', contract_contents=False, expand_target_pct=5),
					ResultsFormatterTableColumn('error', 'Error', contract_contents=False, expand_target_pct=95)
					]
		else:
			if len(results) == 0:
				columns = [ResultsFormatterTableColumn('result', '', contract_contents=False, expand_target_pct=100)]
				results = [XPathNoResultsResult()]
			else:
				columns = [
					ResultsFormatterTableColumn('line', 'Line', contract_contents=False, expand_target_pct=5),
					ResultsFormatterTableColumn('tag', 'Tag', expand_target_pct=15),
					ResultsFormatterTableColumn('xmlattr', 'Attribute', expand_target_pct=10),
					ResultsFormatterTableColumn('result', 'Result', expand_target_pct=70)
					]

		#Leave space for column delimiters
		data_width = self.width - (len(columns) + 1)

		self.table = ResultsFormatterTable(data_width, columns)
		self.table.add_results(results)
		self.table.build()

	def get_formatted_lines(self):
		lines = []
		lines += self.build_header()
		lines += self.build_body()
		lines += self.build_footer()

		lines = [x.replace("\n", " ") for x in lines]

		return lines

	def build_header(self):
		header_lines = []
		
		header_lines.append(TBL.TL + TBL.T* (self.width-2) + TBL.TR)

		header_text = 'Results: ' + self.xpath_string
		header_lines.append(TBL.ML + header_text + ' ' * (self.width - len(header_text) - 2) + TBL.MR)

		lines = [TBL.HL, TBL.ML, TBL.HL]
		for c in self.table.columns:
			lines[0] += TBL.T * c.width + TBL.TC
			lines[1] += c.title + ' '*(c.width - len(c.title)) + TBL.MC
			lines[2] += TBL.H * c.width + TBL.HC

		lines[0] = lines[0][:-len(TBL.TC)] + TBL.HR
		lines[2] = lines[2][:-len(TBL.HC)] + TBL.HR

		header_lines += lines

		return header_lines

	def build_body(self):
		body_lines = []

		for r in self.table.rows:
			line = TBL.ML
			for c in self.table.columns:
				contents = r.cells.get(c, '')
				if len(contents) > c.width:
					contents = contents[:c.width-3] + '...'
				else:
					contents += ' '*(c.width - len(contents))

				line += contents + TBL.MC

			body_lines.append(line)

		return body_lines

	def build_footer(self):
		footer_lines = []
		line = TBL.BL
		for c in self.table.columns:
			line += TBL.B * c.width + TBL.BC

		line = line[:-len(TBL.BC)] + TBL.BR

		footer_lines.append(line)
		return footer_lines

class ResultsFormatterTable(object):

	def __init__(self, table_width, columns):

		self.width = table_width

		self.columns = columns
		self.rows = []

	def add_results(self, results):
		for r in results:
			row = ResultsFormatterTableRow(self.columns, r)
			if len(row.cells.keys()) > 0:
				self.rows.append(row)

	def build(self):
		self.calculate_column_data_widths()
		self.fit_columns_based_on_column_settings()

	def calculate_column_data_widths(self):
		for col in self.columns:
			for r in self.rows:
				data = r.cells.get(col, '')
				col.max_data_width = max(col.max_data_width, len(data))

	def fit_columns_based_on_column_settings(self):

		self.assign_space_for_non_contractable_columns()

		free_space = self.calculate_free_space()
		self.assign_free_space_to_columns_that_want_it(free_space)

	def assign_space_for_non_contractable_columns(self):
		for col in [c for c in self.columns if not(c.contract_contents)]:
			col.width = max(col.max_data_width, len(col.title))

	def calculate_free_space(self):
		free_space = self.width - sum([c.width for c in self.columns])
		return free_space

	def assign_free_space_to_columns_that_want_it(self, free_space):
		still_assigning = True
		while free_space > 0 and still_assigning:
			still_assigning = False
			for col in self.columns:
				if col.wants_more_space(self.width):
					if free_space > 0:
						col.width += 1
						free_space -= 1
						still_assigning = True
			
class ResultsFormatterTableColumn(object):
	def __init__(self, name, title, contract_contents=True, expand_target_pct=0):
		self.name = name
		self.title = title

		self.width = 0
		self.max_data_width = 0

		self.contract_contents = contract_contents
		self.expand_target_pct = expand_target_pct

	def current_percentage_width(self, table_width):
		return (self.width / float(table_width)) * 100

	def wants_more_space(self, table_width):
		data_is_larger = (self.width < self.max_data_width)
		desired_pct_is_larger = (self.current_percentage_width(table_width) < self.expand_target_pct)
		if (data_is_larger or desired_pct_is_larger):
			return True
		else:
			return False


class ResultsFormatterTableRow(object):

	def __init__(self, columns, result):
		self.cells = {}
		for c in columns:
			try:
				cell = result.__getattribute__(c.name)
				if cell is None:
					self.cells[c] = ''
				else:
					self.cells[c] = str(cell)
			except AttributeError as inst:
				pass

class CompletionFormatter(object):
	def __init__(self, results):
		self.base = results['base']
		self.results = results['results']

	def get_formatted_list(self):
		valid_results = [r for r in self.results if isinstance(r, XPathValidResult)]
		formatted_list = []

		for r in valid_results:

			formatted = r.tag
			if isinstance(r, XPathAttrResult):
				formatted = r.xmlattr

			formatted_list.append(self.base + formatted)

		return formatted_list

class XPathResult(object):
	pass

class XPathErrorResult(XPathResult):
	pass

class XPathParseErrorResult(XPathErrorResult):
	def __init__(self, error):
		self.error = error
		self.set_error_position(error)

	def set_error_position(self, error):
		self.line = self.first_group_match('line (\d*)', error)
		self.column = self.first_group_match('column (\d*)', error)

	def first_group_match(self, pattern, text):
		search = re.search(pattern, text)
		groups = search.groups()
		return groups[0]

class XPathSearchErrorResult(XPathErrorResult):
	def __init__(self, error, xpath):
		self.error = 'Error with XPath (' + xpath + '): ' + error
		self.line = '*'
		self.column = '*'

class XPathNoResultsResult(XPathResult):
	def __init__(self):
		self.result = 'No results found.'

class XPathValidResult(XPathResult):
	def __init__(self, el):
		self.line = self.build_line(el)
		self.tag = self.build_tag(el)
		self.xmlattr = self.build_xmlattr(el)
		self.result = self.build_result(el)

	def build_line(self, el):
		pass

	def build_tag(self, el):
		pass

	def build_xmlattr(self, el):
		pass

	def build_result(self, el):
		pass

class XPathNodeResult(XPathValidResult):
	def build_line(self, el):
		return el.sourceline

	def build_tag(self, el):
		return el.tag

class XPathTagResult(XPathNodeResult):
	def build_result(self, el):
		text = ''
		if el.text is not None:
			text = el.text

		if re.sub('\s', '', text) == '':
			attrib_string = ''
			for a in el.attrib.keys():
				attrib_string += '@' + a + ': "' + el.attrib[a] + '" '
			
			return attrib_string
		else:
			return el.text

class XPathStringResult(XPathValidResult):
	def build_line(self, el):
		parent = el.getparent()
		return parent.sourceline

	def build_tag(self, el):
		parent = el.getparent()
		return parent.tag

class XPathAttrResult(XPathStringResult):
	def build_xmlattr(self, el):
		try:
			xmlattr = '@' + el.attrname
		except AttributeError:
			xmlattr = None

		return xmlattr

	def build_result(self, el):
		return str(el)

