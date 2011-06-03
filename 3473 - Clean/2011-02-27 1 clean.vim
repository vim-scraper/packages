" Vim color file (no GUI)
" Name: Clean
" Author: Dee Sub Wun


set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "clean"


"""""""""""
" .Xdefaults / .Xresources for better resuts
""""""""""""""""""""""""""""""""""""""""""""""""""
" ! Cursor
" XTerm*cursorColor: #FF0000

" ! Fonts (xft)
" Xft*antialias: true
" Xft*hinting: true
" Xft*hintstyle: 3
" Xft*dpi: 96
"
" !XTerm*faceName: xft: Terminus:pixelsize=17:antialias=true
" OR:
" XTerm*faceName: xft: Monospace:pixelsize=14:antialias=true
"
" ! Colors
" XTerm*background: #FFFFFF
" XTerm*foreground: #000000

" !----- NON BOLD -----!
" XTerm*color0: #000000
" XTerm*color1: #993333
" XTerm*color2: #008C00
" XTerm*color3: #E69900
" XTerm*color4: #0043CC
" XTerm*color5: #990099
" XTerm*color6: #009999
" XTerm*color7: #E6E6E6

" !------- BOLD -------!
" XTerm*color8: #595959
" XTerm*color9: #E60027
" XTerm*color10: #6CD900
" XTerm*color11: #FFD040
" XTerm*color12: #1C89F3
" XTerm*color13: #D900D9
" XTerm*color14: #00D9D9
" XTerm*color15: #FFFFFF


""""""
" Interface
""""""""""""""""""""""""
hi Cursor           			cterm=NONE          ctermfg=grey            ctermbg=red
hi Directory        			cterm=NONE          ctermfg=blue      		ctermbg=NONE
hi ErrorMsg         			cterm=NONE          ctermfg=white           ctermbg=darkred
hi MatchParen       			cterm=NONE          ctermfg=white           ctermbg=green
hi ModeMsg          			cterm=NONE          ctermfg=blue   		    ctermbg=NONE
hi MoreMsg          			cterm=NONE          ctermfg=darkgreen       ctermbg=NONE
hi NonText          			cterm=NONE          ctermfg=darkcyan        ctermbg=NONE
hi Normal						cterm=NONE			ctermfg=black			ctermbg=NONE
hi Question         			cterm=NONE          ctermfg=green           ctermbg=NONE
hi SpecialKey       			cterm=NONE          ctermfg=darkgreen       ctermbg=NONE
hi VertSplit        			cterm=NONE			ctermfg=black	        ctermbg=black
hi WarningMsg       			cterm=NONE          ctermfg=grey            ctermbg=darkred
hi WildMenu         			cterm=NONE          ctermfg=white           ctermbg=blue
hi LineNr           			cterm=NONE          ctermfg=grey	   		ctermbg=darkcyan


""""""
" Tabs
""""""""""""""""""""""""
hi TabLine          			cterm=NONE			ctermfg=darkcyan       	ctermbg=black
hi TabLineFill      			cterm=NONE      	ctermfg=darkcyan       	ctermbg=black
hi TabLineSel       			cterm=NONE 			ctermfg=white        	ctermbg=darkcyan


""""""
" Status
""""""""""""""""""""""""
hi StatusLine       			cterm=bold      	ctermfg=white	    	ctermbg=darkcyan
hi StatusLineNC     			cterm=NONE		    ctermfg=darkcyan       	ctermbg=black


""""""
" Completion
""""""""""""""""""""""""
hi Pmenu            			cterm=reverse	    ctermfg=black           ctermbg=darkcyan
hi PmenuSel         			cterm=bold          ctermfg=white           ctermbg=cyan


""""""
" Diff
""""""""""""""""""""""""
hi DiffAdd          			cterm=NONE          ctermfg=grey 	        ctermbg=green
hi DiffChange       			cterm=NONE          ctermfg=black           ctermbg=brown
hi DiffDelete       			cterm=NONE          ctermfg=grey	        ctermbg=red
hi DiffText         			cterm=NONE          ctermfg=red 	        ctermbg=brown


""""""
" Visual
""""""""""""""""""""""""
hi Visual          				cterm=NONE          ctermfg=grey        	ctermbg=blue
hi VisualNOS        			cterm=NONE			ctermfg=NONE            ctermbg=NONE


""""""
" Folding
""""""""""""""""""""""""
hi Folded           			cterm=NONE          ctermfg=grey   	    	ctermbg=blue
hi FoldColumn       			cterm=NONE          ctermfg=grey       		ctermbg=blue


""""""
" Search
""""""""""""""""""""""""
hi IncSearch        			cterm=NONE          ctermfg=grey            ctermbg=green
hi Search           			cterm=NONE          ctermfg=grey            ctermbg=green


""""""
" Main Highlighting
""""""""""""""""""""""""
hi Constant         			cterm=NONE          ctermfg=darkgreen       ctermbg=NONE
hi String           			cterm=NONE          ctermfg=darkcyan       	ctermbg=NONE
hi Character        			cterm=NONE          ctermfg=darkcyan        ctermbg=NONE
hi Number           			cterm=NONE          ctermfg=darkcyan      	ctermbg=NONE
hi Boolean          			cterm=NONE          ctermfg=darkcyan       	ctermbg=NONE
hi Float            			cterm=NONE          ctermfg=darkcyan       	ctermbg=NONE
hi Comment          			cterm=NONE          ctermfg=darkred         ctermbg=NONE

hi Identifier       			cterm=NONE          ctermfg=darkgrey       	ctermbg=NONE
hi Function         			cterm=NONE          ctermfg=black           ctermbg=NONE

hi Statement        			cterm=NONE          ctermfg=darkgrey     	ctermbg=NONE
hi Keyword          			cterm=NONE          ctermfg=darkgrey       	ctermbg=NONE

hi Conditional      			cterm=NONE          ctermfg=cyan     		ctermbg=NONE
hi Repeat           			cterm=NONE          ctermfg=cyan     		ctermbg=NONE
hi Label            			cterm=NONE          ctermfg=darkgrey   		ctermbg=NONE
hi Operator         			cterm=NONE          ctermfg=blue         	ctermbg=NONE

hi Exception        			cterm=NONE          ctermfg=darkgrey	    ctermbg=NONE

hi PreProc          			cterm=NONE          ctermfg=magenta     	ctermbg=NONE
hi Include          			cterm=NONE          ctermfg=magenta    	 	ctermbg=NONE
hi Define           			cterm=NONE          ctermfg=magenta    		ctermbg=NONE
hi Macro            			cterm=NONE          ctermfg=magenta     	ctermbg=NONE
hi PreCondit        			cterm=NONE          ctermfg=magenta     	ctermbg=NONE

hi Type             			cterm=NONE          ctermfg=green   	    ctermbg=NONE
hi StorageClass     			cterm=NONE          ctermfg=darkgreen      	ctermbg=NONE
hi Structure        			cterm=NONE          ctermfg=green       	ctermbg=NONE
hi Typedef          			cterm=NONE          ctermfg=green       	ctermbg=NONE

hi Special          			cterm=NONE          ctermfg=black           ctermbg=NONE
hi SpecialChar      			cterm=NONE          ctermfg=blue     	    ctermbg=NONE

hi Underlined       			cterm=underline     ctermfg=grey            ctermbg=NONE

hi Ignore          		 		cterm=NONE          ctermfg=darkgrey        ctermbg=NONE

hi Error            			cterm=NONE          ctermfg=grey            ctermbg=darkred

hi Todo             			cterm=NONE          ctermfg=grey            ctermbg=darkgreen


""""""
" HTML
""""""""""""""""""""""""
hi Title            			cterm=NONE          ctermfg=darkgrey	    ctermbg=NONE
hi htmlItalic					cterm=NONE		  	ctermfg=NONE			ctermbg=NONE
hi htmlTag		    			cterm=NONE          ctermfg=darkgrey       	ctermbg=NONE
hi htmlEndTag					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi htmlTagError					cterm=NONE		  	ctermfg=grey			ctermbg=darkred
hi htmlTagName     		 		cterm=NONE		    ctermfg=blue			ctermbg=NONE
hi htmlSpecialTagName			cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi htmlArg          			cterm=NONE		    ctermfg=darkgrey 		ctermbg=NONE
hi htmlString					cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi htmlComment					cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi htmlCommentPart				cterm=NONE		  	ctermfg=darkred			ctermbg=grey
hi htmlCommentError				cterm=NONE		  	ctermfg=white			ctermbg=red
hi htmlEvent					cterm=NONE		  	ctermfg=blue			ctermbg=NONE


""""""
" XML
""""""""""""""""""""""""
hi xmlTodo						cterm=NONE			ctermfg=white			ctermbg=red
hi xmlTag						cterm=NONE			ctermfg=darkgrey		ctermbg=NONE
hi xmlTagName					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi xmlEndTag					cterm=NONE			ctermfg=darkgrey		ctermbg=NONE
hi xmlEntity					cterm=NONE			ctermfg=darkcyan		ctermbg=NONE
hi xmlEntityPunct				cterm=NONE			ctermfg=darkcyan		ctermbg=NONE

hi xmlAttrib					cterm=NONE			ctermfg=black			ctermbg=NONE

hi xmlString					cterm=NONE			ctermfg=brown			ctermbg=NONE
hi xmlComment					cterm=NONE			ctermfg=darkred			ctermbg=NONE
hi xmlCommentStart				cterm=NONE			ctermfg=darkred			ctermbg=NONE
hi xmlCommentPart				cterm=NONE			ctermfg=darkred			ctermbg=grey
hi xmlCommentError				cterm=NONE			ctermfg=white			ctermbg=darkred
hi xmlError						cterm=NONE			ctermfg=white			ctermbg=darkred


""""""
" JavaScript
""""""""""""""""""""""""
hi javaScript					cterm=NONE		  	ctermfg=black			ctermbg=NONE
hi javaScriptComment			cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi javaScriptLineComment		cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi javaScriptStringD			cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi javaScriptNumber				cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi javaScriptConditional		cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi javaScriptRepeat				cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi javaScriptBranch				cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi javaScriptStatement			cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi javaScriptFunction			cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi javaScriptBraces				cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi javaScriptNull				cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi javaScriptBoolean			cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi javaScriptRegexpString		cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi javaScriptLabel				cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi javaScriptMessage			cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi javaScriptMember				cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi javaScriptIdentifier			cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE


""""""
" CSS
""""""""""""""""""""""""
hi cssComment					cterm=NONE			ctermfg=darkred			ctermbg=NONE
hi cssTagName					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi cssCommonAttr				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssFontAttr					cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssColorAttr					cterm=NONE			ctermfg=darkgreen		ctermbg=NONE
hi cssRenderProp				cterm=NONE			ctermfg=darkgreen		ctermbg=NONE
hi cssGeneratedContentProp		cterm=NONE			ctermfg=darkgreen		ctermbg=NONE
hi cssBoxAttr					cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssPseudoClass				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssPseudoClassId				cterm=NONE			ctermfg=magenta			ctermbg=NONE
hi cssColor						cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssURL						cterm=NONE			ctermfg=brown			ctermbg=NONE
hi cssIdentifier				cterm=NONE			ctermfg=blue			ctermbg=NONE
hi cssBraces					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi cssString					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi cssRenderAttr				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssUIAttr					cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssValueLength				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssValueNumber				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssFunctionName				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssBraceError				cterm=NONE			ctermfg=white			ctermbg=red
hi cssMediaComma				cterm=NONE			ctermfg=black			ctermbg=NONE
hi cssClassName					cterm=NONE			ctermfg=blue			ctermbg=NONE


""""""
" PHP
""""""""""""""""""""""""
hi phpComment					cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi phpBoolean					cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi phpStructure					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpStringSingle				cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi phpStringDouble				cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi phpNumber					cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi phpFloat						cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi phpMethods					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpFunctions					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpRepeat					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpConditional				cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpLabel						cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi phpStatement					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi phpKeyword					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi phpType						cterm=NONE		  	ctermfg=black			ctermbg=NONE
hi phpInclude					cterm=NONE		  	ctermfg=green			ctermbg=NONE
hi phpDefine					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpParent					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi phpOperator					cterm=NONE		  	ctermfg=black			ctermbg=NONE
hi phpVarSelector				cterm=NONE		  	ctermfg=black			ctermbg=NONE
hi phpIdentifier				cterm=NONE		  	ctermfg=black			ctermbg=NONE


""""""
" C
""""""""""""""""""""""""
hi cFormat						cterm=NONE		  	ctermfg=magenta			ctermbg=NONE
hi cCppString					cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi cCommentStart				cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi cLabel						cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cUserLabel					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cConditional					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi cRepeat						cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi cCharacter					cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi cSpecialCharacter			cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi cNumber						cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi cOctal						cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi cOctalZero					cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi cFloat						cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi cOctalError					cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cParenError					cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cErrInParen					cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cErrInBracket				cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cCommentError				cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cCommentStartError			cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cSpaceError					cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cSpecialError				cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cCurlyError					cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cOperator					cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi cStorageClass				cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cInclude						cterm=NONE		  	ctermfg=green			ctermbg=NONE
hi cDefine						cterm=NONE		  	ctermfg=black			ctermbg=NONE
hi cIncluded					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cError						cterm=NONE		  	ctermfg=white			ctermbg=darkred
hi cStatement					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cPreCondit					cterm=NONE		  	ctermfg=black			ctermbg=NONE
hi cType						cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cConstant					cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi cString						cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi cComment						cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi cSpecial						cterm=NONE		  	ctermfg=blue			ctermbg=NONE


""""""
" Python
""""""""""""""""""""""""
hi pythonBuiltin				cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi pythonStatement				cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi pythonConditional			cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi pythonRepeat					cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi pythonOperator				cterm=NONE		  	ctermfg=darkgrey		ctermbg=NONE
hi pythonInclude				cterm=NONE		  	ctermfg=green			ctermbg=NONE
hi pythonFunction				cterm=NONE		  	ctermfg=magenta			ctermbg=NONE
hi pythonComment				cterm=NONE		  	ctermfg=darkred			ctermbg=NONE
hi pythonTodo					cterm=NONE		  	ctermfg=white			ctermbg=red
hi pythonString					cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi pythonRawString				cterm=NONE		  	ctermfg=brown			ctermbg=NONE
hi pythonNumber					cterm=NONE		  	ctermfg=darkgreen		ctermbg=NONE
hi pythonDoctest				cterm=NONE		  	ctermfg=blue			ctermbg=NONE
hi pythonDoctestValue			cterm=NONE		  	ctermfg=blue			ctermbg=NONE


""""""
" Ruby
""""""""""""""""""""""""
hi rubyClass					cterm=NONE			ctermfg=green			ctermbg=NONE
hi rubyModule					cterm=NONE			ctermfg=magenta			ctermbg=NONE
hi rubyDefine					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyFunction					cterm=NONE			ctermfg=magenta			ctermbg=NONE
hi rubyConditional				cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyConditionalModifier		cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRepeat					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRepeatModifier			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyOptionalDo				cterm=NONE			ctermfg=red				ctermbg=NONE
hi rubyControl					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyInclude					cterm=NONE			ctermfg=green			ctermbg=NONE
hi rubyInteger					cterm=NONE			ctermfg=darkgreen		ctermbg=NONE
hi rubyASCIICode				cterm=NONE			ctermfg=darkred			ctermbg=NONE
hi rubyFloat					cterm=NONE			ctermfg=darkgreen		ctermbg=NONE
hi rubyBoolean					cterm=NONE			ctermfg=darkgreen		ctermbg=NONE
hi rubyException				cterm=NONE			ctermfg=blue			ctermbg=NONE

hi rubyClassVariable			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyConstant					cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyGlobalVariable			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyInstanceVariable			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyPseudoVariable			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyBlockParameter			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyPredefinedIdentifier		cterm=NONE			ctermfg=darkmagenta		ctermbg=NONE
hi rubyPredefinedConstant		cterm=NONE			ctermfg=darkmagenta		ctermbg=NONE
hi rubyPredefinedVariable		cterm=NONE			ctermfg=darkmagenta		ctermbg=NONE
hi rubySymbol					cterm=NONE			ctermfg=darkgrey		ctermbg=NONE
hi rubyAccess					cterm=NONE			ctermfg=darkgrey		ctermbg=NONE

hi rubyComment					cterm=NONE			ctermfg=darkred			ctermbg=NONE
hi rubyTodo						cterm=NONE			ctermfg=white			ctermbg=darkred

hi rubyStringEscape				cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyInterpolationDelimiter	cterm=NONE			ctermfg=darkgrey		ctermbg=NONE
hi rubySymbolDelimiter			cterm=NONE			ctermfg=darkgrey		ctermbg=NONE
hi rubyStringDelimiter			cterm=NONE			ctermfg=brown			ctermbg=NONE
hi rubyString					cterm=NONE			ctermfg=brown			ctermbg=NONE
hi rubyRegexpDelimiter			cterm=NONE			ctermfg=cyan			ctermbg=NONE
hi rubyRegexpEscape				cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexpQuantifier			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexpAnchor				cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexpDot				cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexpCharClass			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexpSpecial			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexpComment			cterm=NONE			ctermfg=blue			ctermbg=NONE
hi rubyRegexp					cterm=NONE			ctermfg=blue			ctermbg=NONE

hi rubyInvalidVariable			cterm=NONE			ctermfg=white			ctermbg=darkred
hi rubyError					cterm=NONE			ctermfg=white			ctermbg=darkred
hi rubySpaceError				cterm=NONE			ctermfg=white			ctermbg=darkred


"vim: sw=4

