" Vim syntax file
" Language:	JavaScript
" !Maintainer:	Claudio Fleiner <claudio@fleiner.com>
" !URL:		http://www.fleiner.com/vim/syntax/javascript.vim
" Last Change:	2005 Oct 18

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
" tuning parameters:
" unlet javaScript_fold

if !exists("main_syntax")
	if version < 600
		syntax clear
	elseif exists("b:current_syntax")
		finish
	endif
	let main_syntax = 'javascript'
endif

" Drop fold if it set but vim doesn't support it.
if version < 600 && exists("javaScript_fold")
	unlet javaScript_fold
endif

syn case ignore



syn match	javaScriptOp				"[-=+%^&|*!.~?:]"
syn match	javaScriptOp				"[-+*/%^&|.]="
syn match	javaScriptOp				"&&\|||"
syn match	javaScriptOp				"[!=<>]="
syn match	javaScriptOp				"[<>]"
syn keyword javaScriptCommentTodo      TODO FIXME XXX TBD contained
syn match   javaScriptLineComment      "\/\/.*$" contains=javaScriptCommentTodo
syn match   javaScriptCommentSkip      "^[ \t]*\*\($\|[ \t]\+\)"
syn region  javaScriptComment	       start="/\*"  end="\*/" contains=javaScriptCommentTodo
syn match   javaScriptSpecial	       "\\\d\d\d\|\\."
syn region  javaScriptStringD	       start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=javaScriptSpecial,@htmlPreproc
syn region  javaScriptStringS	       start=+'+  skip=+\\\\\|\\'+  end=+'+  contains=javaScriptSpecial,@htmlPreproc
syn match   javaScriptSpecialCharacter "'\\.'"
syn match   javaScriptNumber	       "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn region  javaScriptRegexpString     start=+/[^/*]+me=e-1 skip=+\\\\\|\\/+ end=+/[gi]\?\s*$+ end=+/[gi]\?\s*[;.,)]+me=e-1 contains=@htmlPreproc oneline
syn keyword	javaScriptException		catch throw try
syn keyword javaScriptConditional	if else
syn keyword javaScriptRepeat		while for
syn keyword javaScriptBranch		break continue switch case default
syn keyword javaScriptOperator		new in
syn keyword javaScriptType		this var const
syn keyword javaScriptStatement		return with
syn keyword javaScriptBoolean		true false
syn keyword javaScriptNull		null
syn keyword javaScriptPreDef		typeof length charcodeat charat substr substring tolowercase array Math min max
syn keyword javaScriptHTMLVars		document window event style layerX layerY navigator userAgent innerHTML outerHTML forms indexOf readyState responseXML status self screen availwidth availheight value selectionstart selectionend getselectionrange createtextrange setselectionrange scrolltop focus text caretpos readonly checked disabled className name selection width height
syn keyword javaScriptHTMLFunc		getElementById XMLHttpRequest onreadystatechange open setRequestHeader send select createrange selectnodecontents deletecontents createcontextualfragment setstartbefore settimeout Date getTime duplicate Image 
syn keyword javaScriptHTMLEvents	onmouseover onmouseout onclick onsubmit onblur onfocus onselect onmousemove

syn keyword javaScriptDOMConst		ATTRIBUTE_NODE CDATA_SECTION_NODE COMMENT_NODE DOCUMENT_FRAGMENT_NODE DOCUMENT_NODE DOCUMENT_TYPE_NODE ELEMENT_NODE ENTITY_NODE ENTITY_REFERENCE_NODE NOTATION_NODE PROCESSING_INSTRUCTION_NODE TEXT_NODE DOCUMENT_POSITION_CONTAINED_BY DOCUMENT_POSITION_CONTAINS DOCUMENT_POSITION_DISCONNECTED DOCUMENT_POSITION_FOLLOWING DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC DOCUMENT_POSITION_PRECEDING DERIVATION_EXTENSION DERIVATION_LIST DERIVATION_RESTRICTION DERIVATION_UNION NODE_ADOPTED NODE_CLONED NODE_DELETED NODE_IMPORTED NODE_RENAMED

syn keyword javaScriptDOMErr		DOMSTRING_SIZE_ERR HIERARCHY_REQUEST_ERR INDEX_SIZE_ERR INUSE_ATTRIBUTE_ERR INVALID_ACCESS_ERR INVALID_CHARACTER_ERR INVALID_MODIFICATION_ERR INVALID_STATE_ERR NAMESPACE_ERR NOT_FOUND_ERR NOT_SUPPORTED_ERR NO_DATA_ALLOWED_ERR NO_MODIFICATION_ALLOWED_ERR SYNTAX_ERR TYPE_MISMATCH_ERR VALIDATION_ERR WRONG_DOCUMENT_ERR SEVERITY_ERROR SEVERITY_FATAL_ERROR SEVERITY_WARNING

syn keyword javaScriptDOMAttrib		attributes baseURI childNodes firstChild lastChild localName namespaceURI nextSibling nodeName nodeType nodeValue ownerDocument parentNode prefix previousSibling textContent length data isId ownerElement schemaTypeInfo specified value tagName isElementContentWhitespace wholeText typeName typeNamespace location message relatedData relatedException severity type byteOffset columnNumber lineNumber relatedNode uri utf16Offset parameterNames

syn keyword javaScriptDOMFunc		appendChild cloneNode compareDocumentPosition getFeature getUserData hasAttributes hasChildNodes insertBefore isDefaultNamespace isEqualNode isSameNode isSupported lookupNamespaceURI lookupPrefix normalize removeChild replaceChild setUserData item getNamedItem getNamedItemNS item removeNamedItem removeNamedItemNS setNamedItem setNamedItemNS appendData deleteData insertData replaceData substringData getAttribute getAttributeNS getAttributeNode getAttributeNodeNS getElementsByTagName getElementsByTagNameNS hasAttribute hasAttributeNS removeAttribute removeAttributeNS removeAttributeNode setAttribute setAttributeNS setAttributeNode setAttributeNodeNS setIdAttribute setIdAttributeNS setIdAttributeNode replaceWholeText splitText isDerivedFrom handle handleError canSetParameter getParameter setParameter 

syn match javaScriptIE			"\.all"

if exists("javaScript_fold")
	syn match	javaScriptFunction      "\<function\>"
	syn region	javaScriptFunctionFold	start="\<function\>.*[^};]$" end="^\z1}.*$" transparent fold keepend

	syn sync match javaScriptSync	grouphere javaScriptFunctionFold "\<function\>"
	syn sync match javaScriptSync	grouphere NONE "^}"

	setlocal foldmethod=syntax
	setlocal foldtext=getline(v:foldstart)
else

	syn keyword	javaScriptFunction      function
	syn match	javaScriptBraces	   "[{}]"
endif

syn sync fromstart
syn sync maxlines=100

if main_syntax == "javascript"
	syn sync ccomment javaScriptComment
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_javascript_syn_inits")
	if version < 508
		let did_javascript_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif
	HiLink javaScriptComment		Comment
	HiLink javaScriptLineComment		Comment
	HiLink javaScriptCommentTodo		Todo
	HiLink javaScriptSpecial		Special
	HiLink javaScriptStringS		String
	HiLink javaScriptStringD		String
	HiLink javaScriptCharacter		Character
	HiLink javaScriptSpecialCharacter	javaScriptSpecial
	HiLink javaScriptException		Special
	hi javaScriptNumber			guifg=#ccffdd
	hi javaScriptOp			guifg=#4499ff
	HiLink javaScriptConditional		Conditional
	HiLink javaScriptRepeat		Repeat
	HiLink javaScriptBranch		Conditional
	HiLink javaScriptOperator		Operator
	HiLink javaScriptType			Type
	HiLink javaScriptStatement		Statement
	HiLink javaScriptFunction		Function
	HiLink javaScriptBraces		Function
	HiLink javaScriptError		Error
	HiLink javaScrParenError		javaScriptError
	HiLink javaScriptNull			Keyword
	HiLink javaScriptPredef		Function
	hi javaScriptHTMLVars			guifg=#ffaaaa
	hi javaScriptHTMLFunc			guifg=#ffcc55
	hi javaScriptHTMLEvents		guifg=#ff88cc
	hi javaScriptDOMConst			guifg=#ccdd33 gui=bold,italic
	hi javaScriptDOMErr			guifg=#ff4433 gui=bold
	hi javaScriptDOMAttrib		guifg=#bbff99
	hi javaScriptDOMFunc			guifg=#aaee55
	HiLink javaScriptBoolean		Boolean
	HiLink javaScriptRegexpString		String
	hi javaScriptIE			guifg=#ffffff guibg=red
	delcommand HiLink
endif

let b:current_syntax = "javascript"
if main_syntax == 'javascript'
	unlet main_syntax
endif

