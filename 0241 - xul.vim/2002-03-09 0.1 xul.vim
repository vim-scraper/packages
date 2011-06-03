" Vim synatx file
" Language:     XUL
" Maintainer:   Nikolai Nespor <nikolai.nespor@utanet.at>
" URL:          not yet
" Last Change:  2002 03 09
"
" Remarks:      Adds XUL-Highlighting (based on docbk.vim)
"               If you want XUL-Attribute-Highlighting put a line
"               like this:
"               
"               hl_xul_atts = 1
"
"               in your $HOME/.vimrc
"
" $Id: xul.vim,v 1.3 2002/03/09 14:49:43 nikolai Exp $
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
syn keyword xulKW bulletinboard button browser checkbox caption contained
syn keyword xulKW colorpicker column columns commands commandset contained
syn keyword xulKW command conditions content deck description contained
syn keyword xulKW dialog editor grid grippy groupbox hbox html contained
syn keyword xulKW iframe image key keyset member menu contained
syn keyword xulKW menubar menubutton menuitem menulist menupopup contained
syn keyword xulKW menuseparator observes outliner outlinerbody contained
syn keyword xulKW outlinercell outlinercol outlinercols outlinerrow contained
syn keyword xulKW overlay page popup popupset progressmeter radio contained
syn keyword xulKW radiogroup resizer resourcebundle row rows rule contained
syn keyword xulKW script scrollbar scrollbarbutton scrollbox contained
syn keyword xulKW separator slider spacer splitter spring stack contained
syn keyword xulKW statusbar statusbarpanel stringbundle contained
syn keyword xulKW stringbundleset tab tabbrowser tabbox tabpanel contained
syn keyword xulKW tabpanels tabs template text textnode textbox contained
syn keyword xulKW thumb titlebar toolbar toolbarbutton toolbargrippy contained
syn keyword xulKW toolbarseparator toolbox tooltip tree treecell contained
syn keyword xulKW treechildren treecol treecolgroup treehead contained
syn keyword xulKW treeitem treerow triple vbox window wizard contained
syn keyword xulKW wizardpage contained

" XUL-Attributes
"
syn keyword xulAT align allowevents allownegativeassertions class contained
syn keyword xulAT coalesceduplicatearcs collapsed container contained
syn keyword xulAT containment contextmenu datasources debug dir contained
syn keyword xulAT empty equalsize flags flex flexgroup height id contained
syn keyword xulAT insertafter insertbefore label left maxheight contained
syn keyword xulAT maxwidth menu minheight minwidth observes ordinal contained
syn keyword xulAT orient pack persist position ref removeelement contained
syn keyword xulAT statustext style template tooltip tooltiptext top contained
syn keyword xulAT uri width contained

" define highlighting colors
"
hi def link xulKW Statement
hi def link xulAT PreProc

" finally set syntax
"
let b:current_syntax = "xul"

" vim: ts=8
