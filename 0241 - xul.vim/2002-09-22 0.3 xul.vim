" Vim synatx file
" Language:     XUL
" Maintainer:   Nikolai Nespor <nikolai.nespor@utanet.at>
" URL:          http://www.unet.univie.ac.at/~a9600989/vim/xul.vim
" Last Change:  2002 09 21
"
" Remarks:      Adds XUL-Highlighting (based on docbk.vim)
"               If you want XUL-Attribute-Highlighting put a line
"               like this:
"               
"               hl_xul_atts = 1
"
"               in your $HOME/.vimrc. Only common attributes are
"               highlighted
"
" $Id$
"

" Backward compatibility
"
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" set filetype to xml (xml highlighting for free ;-)
" 
doau FileType xml

" syntax-matching is case-sensitive
"
syn case match

" add XUL-Keywords to XML-Tags
" 
syn cluster xmlTagHook add=xulKW

" if attribute highlighting is enabled
"
if exists("g:hl_xul_atts")
  syn cluster xmlAttribHook add=xulAT
endif

" XUL-Keywords
" 
" label is not recognized yet (problems with attribute label)
"
syn keyword xulKW action arrowscrollbox autorepeatbutton binding contained
syn keyword xulKW bindings box broadcaster broadcasterset contained
syn keyword xulKW browser button caption checkbox contained
syn keyword xulKW colorpicker column columns command commands contained
syn keyword xulKW commandset conditions content deck description contained
syn keyword xulKW dialog dialogheader editor grid grippy groupbox contained
syn keyword xulKW hbox iframe image key keybinding keyset label contained
syn keyword xulKW listbox listcell listcol listcols listhead contained
syn keyword xulKW listheader listitem member menu menubar menubutton contained
syn keyword xulKW menuitem menulist menupopup menuseparator observes contained
syn keyword xulKW overlay page popup popupset progressmeter radio contained
syn keyword xulKW radiogroup resizer resourcebundle row rows rule contained
syn keyword xulKW script scrollbar scrollbarbutton scrollbox contained
syn keyword xulKW separator slider spacer splitter stack contained
syn keyword xulKW statusbar statusbarpanel stringbundle contained
syn keyword xulKW stringbundleset tab tabbrowser tabbox tabpanel contained
syn keyword xulKW tabpanels tabs template text textnode textbox contained
syn keyword xulKW thumb titlebar toolbar toolbarbutton toolbargrippy contained
syn keyword xulKW toolbarseparator toolbox tooltip tree treecell contained
syn keyword xulKW treechildren treecol treecols treeseparator contained
syn keyword xulKW treeitem treerow triple vbox window wizard contained
syn keyword xulKW wizardpage contained

" XUL-Attributes
"
syn keyword xulAT align allowevents class collapsed container contained
syn keyword xulAT containment context datasources debug dir contained
syn keyword xulAT empty equalsize flex flexgroup height id contained
syn keyword xulAT insertafter insertbefore left maxheight contained
syn keyword xulAT maxwidth minheight minwidth observes ordinal contained
syn keyword xulAT orient pack persist position ref style template contained
syn keyword xulAT tooltip tooltiptext top uri width contained

" define highlighting colors
"
"hi def link xulKW Statement
"hi def link xulAT PreProc

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_xul_syn_inits")
  if version < 508
    let did_getmail_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink xulKW Statement
  HiLink xulAT PreProc
  delcommand HiLink
endif

" finally set syntax
"
let b:current_syntax = "xul"

" vim: ts=8
