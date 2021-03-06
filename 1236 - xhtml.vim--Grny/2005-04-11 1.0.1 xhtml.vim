" Vim syntax file
" Language:	XHTML
" Filenames:	*.xhtml *.html *.htm *.shtml *.stm
" Maintainer:	Michal Gorny <michal-gorny@wp.pl>
" URL:		http://mig.webpark.pl/vim/xhtml.vim
" Last_change:  2005 Apr 11
" Credits:	Based on Claudio Fleiner's html.vim

if !exists("main_syntax")
  if exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'xhtml'
endif

" Load XML syntax file
runtime! syntax/xml.vim

syn cluster xmlTagHook add=@xhtmlTagHook
syn cluster xmlAttribHook add=@xhtmlAttribHook

syn case match

" XHTML ELEMENTS

syn cluster xhtmlTagHook add=xhtmlElement

" XHTML 1.0 Strict elements
syn keyword xhtmlElement contained abbr acronym address area base bdo big
syn keyword xhtmlElement contained blockquote body br button caption cite code
syn keyword xhtmlElement contained col colgroup dd del dfn div dl dt em
syn keyword xhtmlElement contained fieldset form head hr img ins input kbd
syn keyword xhtmlElement contained label legend li link map meta noscript
syn keyword xhtmlElement contained object ol optgroup option param pre samp
syn keyword xhtmlElement contained script span select small strong style sub
syn keyword xhtmlElement contained sup table tbody td textarea tfoot th thead
syn keyword xhtmlElement contained title tr tt ul var
syn match   xhtmlElement contained /\<h[1-6]\>/
syn match   xhtmlElement contained /\<html\>[^:]/me=e-1
syn match   xhtmlElement contained /\<\(a\|b\|i\|p\|q\)\>[^:]/me=e-1

" Deprecated XHTML 1.0 elements (Transitional and Frameset)
syn keyword xhtmlElement contained applet basefont center dir font
syn keyword xhtmlElement contained iframe isindex menu strike
syn match   xhtmlElement contained /\<\(s\|u\)\>[^:]/me=e-1
" Elements only in XHTML 1.0 Frameset
syn keyword xhtmlElement contained frame noframes frameset

" Elements new in XHTML 1.1 (Ruby Annotation)
syn keyword xhtmlElement contained ruby rbc rtc rb rt rp

" Elements new in XHTML 2.0 (as of 6th Working Draft)
syn keyword xhtmlElement contained blockcode di section separator
syn keyword xhtmlElement contained nl quote standby summary
syn match   xhtmlElement contained /\<\(h\|l\)\>[^:]/me=e-1
" XForms elements (XHTML 2.0)
syn keyword xhtmlElement contained action alert bind case choices copy delete
syn keyword xhtmlElement contained dispatch extension filename group help hint
syn keyword xhtmlElement contained input insert instance item itemset label
syn keyword xhtmlElement contained load mediatype message model output range
syn keyword xhtmlElement contained rebuild recalculate refresh repeat reset
syn keyword xhtmlElement contained revalidate secret select select1 send
syn keyword xhtmlElement contained setfocus setindex setvalue submission submit
syn keyword xhtmlElement contained switch textarea toggle trigger upload value
" XML Events element (XHTML 2.0)
syn keyword xhtmlElement contained listener

" XHTML ATTRIBUTES

syn cluster xhtmlAttribHook add=xhtmlAttr

" XHTML 1.0 Strict attributes
syn keyword xhtmlAttr contained abbr accept accesskey action align alt archive
syn keyword xhtmlAttr contained axis border cellpadding cellspacing char
syn keyword xhtmlAttr contained charoff charset checked cite class classid
syn keyword xhtmlAttr contained codebase codetype cols colspan content coords
syn keyword xhtmlAttr contained data datetime declare defer dir disabled
syn keyword xhtmlAttr contained enctype for frame headers height href hreflang
syn keyword xhtmlAttr contained id ismap label lang longdesc maxlength media
syn keyword xhtmlAttr contained method multiple name nohref profile readonly
syn keyword xhtmlAttr contained rel rev rows rowspan rules scheme scope
syn keyword xhtmlAttr contained selected shape size span src standby summary
syn keyword xhtmlAttr contained tabindex title type usemap valign value
syn keyword xhtmlAttr contained valuetype width
syn match   xhtmlAttr contained /\<\(accept-charset\|http-equiv\|style\)\>/
syn match   xhtmlAttr contained /\<xmlns\>[^:]/me=e-1

" Deprecated XHTML 1.0 attributes (Transitional and Frameset)
syn keyword xhtmlAttr contained alink background bgcolor clear code color
syn keyword xhtmlAttr contained compact face hspace language link noshade
syn keyword xhtmlAttr contained nowrap object prompt start target text version
syn keyword xhtmlAttr contained vlink vspace
" Attributes only in XHTML 1.0 Frameset
syn keyword xhtmlAttr contained frameborder marginheight marginwidth
syn keyword xhtmlAttr contained noresize scrolling

" Events attributes (XHTML 1.x)
if exists("xhtml_no_events_rendering")
  syn cluster xmlAttribHook add=xhtmlEventAttr
  syn match xhtmlEventAttr contained /\<on\(\(un\)\?load\|\(dbl\)\?click\|mouse\(down\|up\|over\|move\|out\)\|focus\|blur\|key\(press\|down\|up\)\|submit\|reset\|select\|change\)\>/
endif

" Attribute new in XHTML 1.1 (Ruby Annotation)
syn keyword xhtmlAttr contained rbspan

" Attributes new in XHTML 2.0
syn keyword xhtmlAttr contained about access datatype edit hreftype nextfocus
syn keyword xhtmlAttr contained prevfocus property resource restype
syn match   xhtmlAttr contained /\<\(xml:\)\@<=base\>/
" XForms attributes (XHTML 2.0)
syn match xhtmlAttr contained "\<\(repeat-model\|repeat-bind\|repeat-nodeset\|repeat-startindex\|repeat-number\)\>"
" XML Events attributes (XHTML 2.0)
syn keyword xhtmlAttr contained defaultAction event handler observer phase
syn keyword xhtmlAttr contained propagate target

" SPECIALS

" Embedded MathML (DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1)
if !exists("xhtml_no_embedded_mathml")
  syn include @xhtmlMathML syntax/mathml.vim
  unlet b:current_syntax
  syn cluster xmlTagHook remove=mathmlElement
  syn cluster xmlAttribHook remove=mathmlAttr
  syn region xhtmlMaths start="<\(\w\+:\)\?math\>" keepend end="</\(\w\+:\)\?math>" contains=mathmlTag,mathmlEndTag,xmlEntity,xmlComment
  syn region mathmlTag start=+<[^ /!?<>"']\@=+ keepend end=+>+ contained contains=mathmlTagName,mathmlAttr,xmlEqual,xmlString
  syn match mathmlTagName +[<]\@<=[^ /!?<>"']\++ contained contains=xmlNamespace,xmlAttribPunct,mathmlElement,@xmlTagHook display
  syn match mathmlEndTag +</[^ /!?<>"']\+>+ contained contains=xmlNamespace,xmlAttribPunct,mathmlElement
endif

" Embedded SVG (DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1)
if !exists("xhtml_no_embedded_svg")
  syn include @xhtmlSVG syntax/svg.vim
  unlet b:current_syntax
  syn cluster xmlTagHook remove=svgElement
  syn cluster xmlAttribHook remove=svgAttr,svgEventAttr,svgEvent
  syn region xhtmlGraph start=+<\(\w\+:\)\?svg\>+ keepend end=+</\(\w\+:\)\?svg>+ contains=svgTag,svgEndTag,xmlEntity,xmlComment
  syn region svgTag start=+<[^ /!?<>"']\@=+ keepend end=+>+ contained contains=svgTagName,svgAttr,svgEventAttr,svgEvent,xmlEqual,xmlString
  syn match svgTagName +[<]\@<=[^ /!?<>"']\++ contained contains=xmlNamespace,xmlAttribPunct,svgElement,@xmlTagHook display
  syn match svgEndTag +</[^ /!?<>"']\+>+ contained contains=xmlNamespace,xmlAttribPunct,svgElement
endif

" XML declaration attributes
syn keyword xmlDeclAttr version encoding standalone containedin=xmlProcessing

" Server Side Includes (SSI)
syn region xhtmlSSI start="<!--#" end="-->" contains=xhtmlSSIStmt,xhtmlSSIConditStmt,xhtmlSSIError,xhtmlSSIAttr
syn match xhtmlSSIStmt contained "<!--#\(config\|echo\|exec\|flastmod\|fsize\|include\|printenv\|set\)\>"
syn match xhtmlSSIError contained "<!--#\S*"ms=s+4
syn match xhtmlSSIAttr contained "\w\+=[^"]\S\+" contains=xhtmlSSIAttrError,xhtmlSSIAttrName,xhtmlSSIConditAttrName
syn region xhtmlSSIAttr contained start=+\w\+="+ skip=+\\\\\|\\"+ end=+"+ contains=xhtmlSSIAttrName,xhtmlSSIConditAttrName keepend
syn match xhtmlSSIAttrError contained "\w\+="he=e-1
syn match xhtmlSSIAttrName contained "\(errmsg\|sizefmt\|timefmt\|var\|cgi\|cmd\|file\|value\|virtual\)="he=e-1
" Extended Server Side Includes (XSSI)
syn match xhtmlSSIConditStmt contained "<!--#\(if\|elif\|else\|endif\)\>"
syn match xhtmlSSIConditAttrName contained "expr="he=e-1

" Embedded JavaScript
if main_syntax != 'svg' && (main_syntax != 'java' || exists("java_javascript"))
  syn include @xhtmlJavaScript syntax/javascript.vim
  unlet b:current_syntax
  syn region javaScript start=+<script[^>]*[^/]>+ keepend end=+</script>+me=s-1 contains=@xhtmlJavaScript,xhtmlScriptTag,@xhtmlPreProc
  syn region xhtmlScriptTag contained start=+<script+ end=+>+ contains=xmlTagName,xmlString,xmlAttrib

  " Events attributes rendering
  if !exists("xhtml_no_events_rendering")
    syn cluster xmlAttribHook add=xhtmlEvent
    syn region xhtmlEvent contained start=+\<on\(\(un\)\?load\|\(dbl\)\?click\|mouse\(down\|up\|over\|move\|out\)\|focus\|blur\|key\(press\|down\|up\)\|submit\|reset\|select\|change\)\s*=[\t ]*'+ keepend end=+'+ contains=xhtmlEventSQ
    syn region xhtmlEvent contained start=+\<on\(\(un\)\?load\|\(dbl\)\?click\|mouse\(down\|up\|over\|move\|out\)\|focus\|blur\|key\(press\|down\|up\)\|submit\|reset\|select\|change\)\s*=[\t ]*"+ keepend end=+"+ contains=xhtmlEventDQ
    syn region xhtmlEventSQ contained start=+'+ms=s+1 end=+'+me=s-1 contains=@xhtmlJavaScript
    syn region xhtmlEventDQ contained start=+"+ms=s+1 end=+"+me=s-1 contains=@xhtmlJavaScript
    hi def link xhtmlEventSQ xhtmlEvent
    hi def link xhtmlEventDQ xhtmlEvent
  endif
endif

" Embedded VBScript
if main_syntax != 'java' || exists("java_vb")
  syn include @xhtmlVbScript syntax/vb.vim
  unlet b:current_syntax
  syn region vbScript start=+<script [^>]*type[ \t\n]*=[ \t\n]*["'][^"']\+vbscript[^>]*[^/]>+ keepend end=+</script>+me=s-1 contains=@xhtmlVbScript,xhtmlScriptTag,@xhtmlPreProc
  syn region xhtmlScriptTag contained start=+<script+ end=+>+ contains=xmlTagName,xmlString,xmlAttrib
endif

" Embedded Cascading Style Sheets
if main_syntax != 'java' || exists("java_css")
  syn include @xhtmlCss syntax/css.vim
  unlet b:current_syntax
  syn region cssStyle start=+<style[^>]*>+ keepend end=+</style>+me=s-1 contains=@xhtmlCss,xhtmlStyleTag,xhtmlCssStyleComment,xmlCdata,@xhtmlPreProc
  syn cluster xmlCdataHook add=@xhtmlCss,xhtmlCssStyleComment,@xhtmlPreProc
  syn match xhtmlCssStyleComment contained "\(<!--\|-->\)"
  syn region xhtmlStyleTag contained start=+<\/\?style+ end=+>+ contains=xmlTagName,xmlString,xmlAttrib
  if !exists("xhtml_no_inline_css_rendering")
    syn region xhtmlInlineCss contained matchgroup=xhtmlAttr start=+style="+ keepend matchgroup=xmlString end=+"+ contains=css.*Attr,css.*Prop,cssComment,cssLength,cssValue.*,cssColor,cssURL,cssImportant,cssError,cssString,@xhtmlPreProc
    syn cluster xmlAttribHook add=xhtmlInlineCss
  endif
endif

" Rendering
if !exists("xhtml_no_rendering")
  syn cluster xhtmlTop contains=@Spell,xmlTag,xmlEndTag,xmlEntity,xhtmlSSI,xmlComment,xhtmlLink,javaScript,@xhtmlPreProc

  syn region xhtmlBold start="<b\>" end="</b>"me=e-4 contains=@xhtmlTop,xhtmlBoldUnderline,xhtmlBoldItalic
  syn region xhtmlBold start="<strong\>" end="</strong>"me=e-9 contains=@xhtmlTop,xhtmlBoldUnderline,xhtmlBoldItalic
  syn region xhtmlBoldUnderline contained start="<u\>" end="</u>"me=e-4 contains=@xhtmlTop,xhtmlBoldUnderlineItalic
  syn region xhtmlBoldItalic contained start="<i\>" end="</i>"me=e-4 contains=@xhtmlTop,xhtmlBoldItalicUnderline
  syn region xhtmlBoldItalic contained start="<em\>" end="</em>"me=e-5 contains=@xhtmlTop,xhtmlBoldItalicUnderline
  syn region xhtmlBoldUnderlineItalic contained start="<i\>" end="</i>"me=e-4 contains=@xhtmlTop
  syn region xhtmlBoldUnderlineItalic contained start="<em\>" end="</em>"me=e-5 contains=@xhtmlTop
  syn region xhtmlBoldItalicUnderline contained start="<u\>" end="</u>"me=e-4 contains=@xhtmlTop,xhtmlBoldUnderlineItalic

  syn region xhtmlItalic start="<i\>" end="</i>"me=e-4 contains=@xhtmlTop,xhtmlItalicBold,xhtmlItalicUnderline
  syn region xhtmlItalic start="<em\>" end="</em>"me=e-5 contains=@xhtmlTop
  syn region xhtmlItalicBold contained start="<b\>" end="</b>"me=e-4 contains=@xhtmlTop,xhtmlItalicBoldUnderline
  syn region xhtmlItalicBold contained start="<strong\>" end="</strong>"me=e-9 contains=@xhtmlTop,xhtmlItalicBoldUnderline
  syn region xhtmlItalicBoldUnderline contained start="<u\>" end="</u>"me=e-4 contains=@xhtmlTop
  syn region xhtmlItalicUnderline contained start="<u\>" end="</u>"me=e-4 contains=@xhtmlTop,xhtmlItalicUnderlineBold
  syn region xhtmlItalicUnderlineBold contained start="<b\>" end="</b>"me=e-4 contains=@xhtmlTop
  syn region xhtmlItalicUnderlineBold contained start="<strong\>" end="</strong>"me=e-9 contains=@xhtmlTop

  syn region xhtmlUnderline start="<u\>" end="</u>"me=e-4 contains=@xhtmlTop,xhtmlUnderlineBold,xhtmlUnderlineItalic
  syn region xhtmlUnderlineBold contained start="<b\>" end="</b>"me=e-4 contains=@xhtmlTop,xhtmlUnderlineBoldItalic
  syn region xhtmlUnderlineBold contained start="<strong\>" end="</strong>"me=e-9 contains=@xhtmlTop,xhtmlUnderlineBoldItalic
  syn region xhtmlUnderlineItalic contained start="<i\>" end="</i>"me=e-4 contains=@xhtmlTop,htmUnderlineItalicBold
  syn region xhtmlUnderlineItalic contained start="<em\>" end="</em>"me=e-5 contains=@xhtmlTop,htmUnderlineItalicBold
  syn region xhtmlUnderlineItalicBold contained start="<b\>" end="</b>"me=e-4 contains=@xhtmlTop
  syn region xhtmlUnderlineItalicBold contained start="<strong\>" end="</strong>"me=e-9 contains=@xhtmlTop
  syn region xhtmlUnderlineBoldItalic contained start="<i\>" end="</i>"me=e-4 contains=@xhtmlTop
  syn region xhtmlUnderlineBoldItalic contained start="<em\>" end="</em>"me=e-5 contains=@xhtmlTop

  " In XHTML 2.0 href attribute may appear on any element
  syn region xhtmlLink start="<\z([A-Za-z:]\+\)\>\_[^>]*[ \t\n]href\>\_[^>]\+[^/]>" keepend skip="<\z1\>[^<]\+</\z1>" end="</\z1>"re=s contains=@Spell,xmlTag,xmlEndTag,xmlEntity,xhtmlSSI,xmlComment,javaScript,@xhtmlPreProc

  syn region xhtmlTitle start="<title\>" end="</title>"me=e-8 contains=@Spell,xmlTag,xmlEndTag,xmlEntity,xhtmlSSI,xmlComment,javaScript,@xhtmlPreProc
  syn region xhtmlH start="<h\>" end="</h>"me=e-4 contains=@xhtmlTop
  syn region xhtmlH1 start="<h1\>" end="</h1>"me=e-5 contains=@xhtmlTop
  syn region xhtmlH2 start="<h2\>" end="</h2>"me=e-5 contains=@xhtmlTop
  syn region xhtmlH3 start="<h3\>" end="</h3>"me=e-5 contains=@xhtmlTop
  syn region xhtmlH4 start="<h4\>" end="</h4>"me=e-5 contains=@xhtmlTop
  syn region xhtmlH5 start="<h5\>" end="</h5>"me=e-5 contains=@xhtmlTop
  syn region xhtmlH6 start="<h6\>" end="</h6>"me=e-5 contains=@xhtmlTop
endif

" Synchronizing
if main_syntax == "xhtml"
  syn sync match xhtmlHighlight groupthere NONE "<[/a-zA-Z]"
  syn sync match xhtmlHighlight groupthere javaScript "<script"
  syn sync match xhtmlHighlightSkip "^.*['\"].*$"
  syn sync minlines=100
endif

" Highlighting
hi link     xmlAttrib			Function
hi link     xmlEntity			Special
hi link     xmlEntityPunct		Special
hi def link xmlDeclAttr			Type
hi def link xhtmlAttr			Type
hi def link xhtmlEventAttr		Type
hi def link xhtmlElement		Statement
hi def link mathmlTag			Function
hi def link mathmlEndTag		Identifier
hi def link mathmlElement		Statement
hi def link svgTag			Function
hi def link svgEndTag			Identifier
hi def link svgElement			Statement
hi def link xhtmlSSIStmt		PreProc
hi def link xhtmlSSIConditStmt		PreCondit
hi def link xhtmlSSIError		Error
hi def link xhtmlSSI			PreProc
hi def link xhtmlSSIAttr		String
hi def link xhtmlSSIAttrName		PreProc
hi def link xhtmlSSIConditAttrName	PreCondit
hi def link xhtmlSSIAttrError		Error
hi def link javaScript			Special
hi def link xhtmlEvent			javaScript
hi def link vbScript			Special
hi def link xhtmlInlineCss		xmlString
hi def link xhtmlCssStyleComment	Comment
hi def link xhtmlScriptTag		xmlTag
hi def link xhtmlStyleTag		xmlTag
if !exists("xhtml_no_rendering")
  hi def link xhtmlH			Title
  hi def link xhtmlH1			xhtmlH
  hi def link xhtmlH2			xhtmlH1
  hi def link xhtmlH3			xhtmlH2
  hi def link xhtmlH4			xhtmlH3
  hi def link xhtmlH5			xhtmlH4
  hi def link xhtmlH6			xhtmlH5
  hi def link xhtmlTitle		Title
  hi def link xhtmlBoldItalicUnderline	xhtmlBoldUnderlineItalic
  hi def link xhtmlUnderlineBold	xhtmlBoldUnderline
  hi def link xhtmlUnderlineItalicBold	xhtmlBoldUnderlineItalic
  hi def link xhtmlUnderlineBoldItalic	xhtmlBoldUnderlineItalic
  hi def link xhtmlItalicUnderline	xhtmlUnderlineItalic
  hi def link xhtmlItalicBold		xhtmlBoldItalic
  hi def link xhtmlItalicBoldUnderline	xhtmlBoldUnderlineItalic
  hi def link xhtmlItalicUnderlineBold	xhtmlBoldUnderlineItalic
  hi def link xhtmlLink			Underlined
  if !exists("xhtml_my_rendering")
    hi def xhtmlBold		term=bold cterm=bold gui=bold
    hi def xhtmlBoldUnderline	term=bold,underline cterm=bold,underline gui=bold,underline
    hi def xhtmlBoldItalic	term=bold,italic cterm=bold,italic gui=bold,italic
    hi def xhtmlBoldUnderlineItalic	term=bold,italic,underline cterm=bold,italic,underline gui=bold,italic,underline
    hi def xhtmlUnderline	term=underline cterm=underline gui=underline
    hi def xhtmlUnderlineItalic	term=italic,underline cterm=italic,underline gui=italic,underline
    hi def xhtmlItalic		term=italic cterm=italic gui=italic
  endif
endif

let b:current_syntax = "xhtml"

if main_syntax == 'xhtml'
  unlet main_syntax
endif

" vim: ts=8
