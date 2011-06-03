" Wikipedia syntax with added functionality for taking notes
"
" Name: wikinotes
" Creator: Thomas Schmall - www.oxpal.com
" Last Change: November 2 2009
" URL: http://www.vim.org/scripts/script.php?script_id=2836
" Version: 1.0
"
" Based on Wikipedia syntax file (from 2003-04 / authorless).
"
" Notes: 
"     This file is thought for two purposes: 
"     * Editing Wikipedia texts as textfiles
"     * having useful and simple syntax-highlighting swhen taking notes in
"     textfiles
" 
" Syntax Description:
"   Supported is Wikisyntax as well as additional practical note-highligting,
"   all in human-readable syntax style
"
" Supported Wikisyntax:
"   ''Italic text''   '''Bold text'''   '''''Bold and Italic'''''
"
"   *unordered lists
"   *unordered lists
"
"   #ordered lists
"   #ordered lists
"
"   [[Wiki Links]]
"   Wikipedia weblinks (also mailto and so on): [http://www.oxpal.com/]
"
"   =Headline 1=    ==Headline 2==
"   ===Headline 3===    ====Headline 4====
"   =====Headline 5=====   ======Headline 6======
"
"    Unformated text (indent with at least one space/Tab)
"
" Added Syntax For Notes:
"  //C style Comments
"  /*
"  C style 
"  block comments 
"  */
"
"  TODO FIXME XXX ??? - highlighting (very practical)
"
"  Normal link highlighting: 
"  html:// ftp:// mailto: 
"
"  Quirks:
"  *There is a lot of "html" still noted in the file as a remain from the
"  wikipedia syntax. Removing it breaks stuff like the "bold" and "italic"
"  highlighting though. 
"  *The normal links end on a space, but the space is still highlighted
"  Also some other small things - if someone is proficient
"  enough to help clean up, it would be much appreciated.
"
"
"
"  Last Changes:
"  *


if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
  finish
endif
  let main_syntax = 'html'
endif

if version < 508
  command! -nargs=+ HtmlHiLink hi link <args>
else
  command! -nargs=+ HtmlHiLink hi def link <args>
endif

syn case ignore

" TODO FIXME -- not wikipedia, practical though
setlocal iskeyword+=?
syn keyword myTodo	TODO FIXME XXX ???

" comments
syn region  cComment  start="//" skip="\\$" end="$"  contains=myTodo
syn region  cComment  start="/\*" end="\*/" contains=myTodo

hi link myTodo           Todo 
hi link cComment      Comment 

" wiki formatting
syn region wikiItalic			start=+'\@<!'''\@!+	end=+''+ contains=wikiLink,wikiItalicBold
syn region wikiBold				start=+'''+			end=+'''+ contains=wikiLink,wikiBoldItalic
syn region wikiBoldAndItalic	start=+'''''+		end=+'''''+ contains=wikiLink

syn region wikiBoldItalic	contained	start=+'\@<!'''\@!+	end=+''+ contains=wikiLink
syn region wikiItalicBold	contained	start=+'''+			end=+'''+ contains=wikiLink

"are those three really necessary?
syn match wikiParaFormatChar /^[\:|\*|;|#]\+/
syn match wikiParaFormatChar /^-----*/
syn match wikiPre /^\ .*$/

syn region wikiH1 start="^=" 		end="=" 	skip="<nowiki>.*</nowiki>" oneline contains=wikiLink
syn region wikiH2 start="^==" 		end="==" 	skip="<nowiki>.*</nowiki>" oneline contains=wikiLink
syn region wikiH3 start="^===" 		end="===" 	skip="<nowiki>.*</nowiki>" oneline contains=wikiLink
syn region wikiH4 start="^====" 	end="====" 	skip="<nowiki>.*</nowiki>" oneline contains=wikiLink
syn region wikiH5 start="^=====" 	end="=====" 	skip="<nowiki>.*</nowiki>" oneline contains=wikiLink
syn region wikiH6 start="^======" 	end="======" 	skip="<nowiki>.*</nowiki>" oneline contains=wikiLink
syn region wikiLink start="\[\[" end="\]\]" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="\[http:" end="\]" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="\[https:" end="\]" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="\[ftp:" end="\]" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="\[gopher:" end="\]" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="\[news:" end="\]" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="\[mailto:" end="\]" skip="<nowiki>.*</nowiki>" oneline


"!Added Links
" syn region wikiLink start="http://" end=" "  skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="http:" end="$" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="http:" end=" " skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="https:" end="$" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="https:" end=" " skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="ftp:" end="$" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="ftp:" end=" " skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="gopher:" end="$" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="gopher:" end=" " skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="news:" end="$" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="news:" end=" " skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="mailto:" end="$" skip="<nowiki>.*</nowiki>" oneline
syn region wikiLink start="mailto:" end=" " skip="<nowiki>.*</nowiki>" oneline

syn match wikiParaFormatChar /^[\:|\*|;|#]\+/
syn match wikiParaFormatChar /^-----*/
syn match wikiPre /^\ .*$/

" HTML highlighting

HtmlHiLink htmlTag                     Function
HtmlHiLink htmlEndTag                  Identifier
HtmlHiLink htmlArg                     Type
HtmlHiLink htmlTagName                 htmlStatement
HtmlHiLink htmlSpecialTagName          Exception
HtmlHiLink htmlValue                     String

HtmlHiLink htmlH1                      Title
HtmlHiLink htmlH2                      htmlH1
HtmlHiLink htmlH3                      htmlH2
HtmlHiLink htmlH4                      htmlH3
HtmlHiLink htmlH5                      htmlH4
HtmlHiLink htmlH6                      htmlH5
HtmlHiLink htmlHead                    PreProc
HtmlHiLink htmlTitle                   Title
HtmlHiLink htmlItalicBold              htmlBoldItalic

HtmlHiLink htmlSpecial            Special
HtmlHiLink htmlString             String
HtmlHiLink htmlStatement          Statement
HtmlHiLink htmlComment            Comment
HtmlHiLink htmlCommentPart        Comment
HtmlHiLink htmlValue              String
HtmlHiLink htmlCommentError       htmlError
HtmlHiLink htmlTagError           htmlError
HtmlHiLink htmlEvent              javaScript
HtmlHiLink htmlError              Error

" The default highlighting.
if version >= 508 || !exists("did_html_syn_inits")
  if version < 508
    let did_html_syn_inits = 1
  endif
  HtmlHiLink htmlTag                     Function
  HtmlHiLink htmlEndTag                  Identifier
  HtmlHiLink htmlArg                     Type
  HtmlHiLink htmlTagName                 htmlStatement
  HtmlHiLink htmlSpecialTagName          Exception
  HtmlHiLink htmlValue                     String

if !exists("html_no_rendering")
    HtmlHiLink htmlH1                      Title
    HtmlHiLink htmlH2                      htmlH1
    HtmlHiLink htmlH3                      htmlH2
    HtmlHiLink htmlH4                      htmlH3
    HtmlHiLink htmlH5                      htmlH4
    HtmlHiLink htmlH6                      htmlH5
    HtmlHiLink htmlHead                    PreProc
    HtmlHiLink htmlTitle                   Title
    HtmlHiLink htmlItalicBold              htmlBoldItalic
    HtmlHiLink htmlLink			   Underlined
  if !exists("html_my_rendering")
    hi def htmlBold                term=bold cterm=bold gui=bold
    hi def htmlBoldItalic          term=bold,italic cterm=bold,italic gui=bold,italic
    hi def htmlItalic              term=italic cterm=italic gui=italic
  endif
endif

  HtmlHiLink htmlPreStmt            PreProc
  HtmlHiLink htmlPreError           Error
  HtmlHiLink htmlPreProc            PreProc
  HtmlHiLink htmlPreAttr            String
  HtmlHiLink htmlPreProcAttrName    PreProc
  HtmlHiLink htmlPreProcAttrError   Error
  HtmlHiLink htmlSpecial            Special
  HtmlHiLink htmlString             String
  HtmlHiLink htmlStatement          Statement
  HtmlHiLink htmlComment            Comment
  HtmlHiLink htmlCommentPart        Comment
  HtmlHiLink htmlValue              String
  HtmlHiLink htmlCommentError       htmlError
  HtmlHiLink htmlTagError           htmlError
  HtmlHiLink htmlEvent              javaScript
  HtmlHiLink htmlError              Error
endif

" wiki highlighting

HtmlHiLink wikiItalic		htmlItalic
HtmlHiLink wikiBold		htmlBold

HtmlHiLink wikiBoldItalic	htmlBoldItalic
HtmlHiLink wikiItalicBold	htmlBoldItalic

HtmlHiLink wikiBoldAndItalic	htmlBoldItalic

HtmlHiLink wikiH1		htmlH1
HtmlHiLink wikiH2		htmlH2
HtmlHiLink wikiH3		htmlH3
HtmlHiLink wikiH4		htmlH4
HtmlHiLink wikiH5		htmlH5
HtmlHiLink wikiH6		htmlH6
HtmlHiLink wikiLink		Underlined
HtmlHiLink wikiParaFormatChar	Special
HtmlHiLink wikiPre		Constant


let b:current_syntax = "html"

delcommand HtmlHiLink

if main_syntax == 'html'
  unlet main_syntax
endif
