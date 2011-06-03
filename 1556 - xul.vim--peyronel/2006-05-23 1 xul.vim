"
" Bonjour il s'agit d'un petit script d'aide a la saisie des fichiers XML XUL
" Il doit etre installé dans le dossier vimfiles/syntax.
" Pour que la reconnaissance se fasse il faut faire un set filetype=xul dans
" votre fichier source.
" Pour vous aider il suffit de faire un <CTRL-X><CTRL-U> 
" Les balises sont prises en comptes et les attributs
"
" Maintainer:	jean-jacques PEYRONEL <jjp@libertysurf.fr>
" Last Change:	2006 Mai 24


let b:XULdict = {}

let b:XULelem = { 'align':'start|center|end|baseline|stretch|left|center|right', 'allowevents':'boolean', 'allownegativeassertions':'boolean', 'class':'chaine', 'coalesceduplicatearcs':'boolean', 'collapsed':'boolean', 'container':'boolean', 'containment':'URI of an RDF predicate', 'context':'popup element id', 'contextmenu':'popup element id', 'datasources':'space separated list of datasource URIs', 'dir':'normal|reverse', 'empty':'boolean', 'equalsize':'always|never', 'flags':'dont-test-empty|dont-build-content', 'flex':'integer', 'height':'integer', 'hidden':'boolean', 'id':'element id, must be unique in the window', 'insertafter':'element id', 'insertbefore':'element id', 'left':'integer', 'maxheight':'integer', 'maxwidth':'integer', 'menu':'popup element id', 'minheight':'integer', 'minwidth':'integer', 'mousethrough':'always|never', 'observes':'broadcaster element id', 'ordinal':'integer', 'orient':'horizontal|vertical', 'pack':'start|center|end', 'persist':'space separated list of attribute names', 'popup':'popup element id', 'position':'integer', 'preference-editable':'boolean', 'ref':'URI of an RDF resource', 'removeelement':'element id','sortDirection':'ascending|descending|natural', 'sortResource':'URI of an RDF predicate', 'sortResource2':'URI of an RDF predicate', 'statustext':'string', 'style':'CSS inline style', 'template':'element id', 'tooltip':'tooltip element id', 'tooltiptext':'string', 'top':'integer', 'uri':'string', 'width':'integer' }

let b:XULdict['<action']={}
let b:XULdict['<arrowscrollbox']={}
let b:XULdict['<bbox']={}
let b:XULdict['<binding']={}
let b:XULdict['<bindings']={}
let b:XULdict['<box']={}
let b:XULdict['<broadcaster']={}
let b:XULdict['<broadcasterset']={}
let b:XULdict['<browser']={'autocompleteenabled':'boolean','autocompletepopup':'id of popup element','autoscroll':'boolean','disablehistory':'boolean','disablesecurity':'boolean','homepage':'string home page URL','src':'URL','type':'string'}
let b:XULdict['<button']={'accesskey':'character','autoCheck':'boolean','checkState':'integer: 0, 1 or 2','checked':'boolean','command':'element id','crop':'start|end|left|right|center|none','dir':'ltr|rtl','disabled':'boolean','dlgType':'accept|cancel|help|disclosure','group':'string group name','image':'image URL','label':'string','open':'boolean','orient':'horizontal|vertical','tabindex':'integer','type':'checkbox|menu|menu-button|radio'}
let b:XULdict['<caption']={'accesskey':'character','crop':'start|end|left|right|center|none','image':'image URL','label':'string'}
let b:XULdict['<checkbox']={'accesskey':'character','checked':'boolean','command':'element id','crop':'start|end|left|right|center|none','disabled':'boolean','image':'image URL','label':'string','tabindex':'integer'}
let b:XULdict['<colorpicker']={'color':'color string','onchange':'script code','type':'string'}
let b:XULdict['<column']={}
let b:XULdict['<columns']={}
let b:XULdict['<command']={'disabled':'boolean','label':'string','oncommand':'script code'}
let b:XULdict['<commandset']={'commandupdater':'boolean','events':'comma separated list','oncommandupdate':'script code','targets':'comma separated list of element ids'}
let b:XULdict['<conditions']={}
let b:XULdict['<content']={}
let b:XULdict['<deck']={'selectedIndex':'integer'}
let b:XULdict['<description']={'crop':'start|end|left|right|center|none','disabled':'boolean','value':'string'}
let b:XULdict['<dialog']={'buttonalign':'string','buttondir':'string','buttonorient':'string','buttonpack':'string','buttons':'comma-separated list of the values below','ondialogaccept':'script code','ondialogcancel':'script code','ondialogdisclosure':'script code','ondialoghelp':'script code','title':'string'}
let b:XULdict['<dialogheader']={'crop':'start|end|left|right|center|none','description':'string','title':'string'}
let b:XULdict['<editor']={'editortype':'html|text','src':'document URL','type':'string'}
let b:XULdict['<grid']={}
let b:XULdict['<grippy']={}
let b:XULdict['<groupbox']={}
let b:XULdict['<hbox']={}
let b:XULdict['<iframe']={'src':'URL'}
let b:XULdict['<image']={'onerror':'script code','onload':'script code','src':'image URL','validate':'always|never'}
let b:XULdict['<key']={'command':'element id','disabled':'boolean','key':'character','keycode':'string key code','keytext':'string','modifiers':'space separated list of the values below','oncommand':'script code','phase':'string'}
let b:XULdict['<keyset']={}
let b:XULdict['<label']={'accesskey':'character','control':'element id','crop':'start|end|left|right|center|none','disabled':'boolean','value':'string'}
let b:XULdict['<listbox']={'disableKeyNavigation':'boolean','disabled':'boolean','rows':'integer','seltype':'single|multiple','suppressonselect':'boolean','tabindex':'integer','value':'string'}
let b:XULdict['<listcell']={'crop':'start|end|left|right|center|none','disabled':'boolean','image':'image URL','label':'string','type':'string'}
let b:XULdict['<listcol']={}
let b:XULdict['<listcols']={}
let b:XULdict['<listhead']={}
let b:XULdict['<listheader']={'crop':'start|end|left|right|center|none','label':'string','sortDirection':'ascending|descending|natural'}
let b:XULdict['<listitem']={'accesskey':'character','checked':'boolean','command':'element id','crop':'start|end|left|right|center|none','current':'boolean','disabled':'boolean','image':'image URL','label':'string','selected':'boolean','type':'string','value':'string'}
let b:XULdict['<member']={}
let b:XULdict['<menu']={'acceltext':'string','accesskey':'character','allowevents':'boolean','crop':'start|end|left|right|center|none','disabled':'boolean','key':'key element id','label':'string','menuactive':'boolean','open':'boolean','sizetopopup':'string','value':'string'}
let b:XULdict['<menubar']={'accessible':'nsIAccessible','grippyhidden':'boolean','statusbar':'id of statusbar element'}
let b:XULdict['<menuitem']={'acceltext':'string','accessible':'nsIAccessible','accesskey':'character','allowevents':'boolean','autocheck':'','checked':'boolean','command':'element id','crop':'start|end|left|right|center|none','disabled':'boolean','image':'image URL','key':'key element id','label':'string','name':'string name','selected':'boolean','type':'checkbox|radio','validate':'always|never','value':'string'}
let b:XULdict['<menulist']={'accesskey':'character','crop':'start|end|left|right|center|none','disableautoselect':'boolean','disabled':'boolean','editable':'boolean','focused':'boolean','image':'image URL','label':'string','open':'boolean','src':'image URL','tabindex':'integer','value':'string'}
let b:XULdict['<menupopup']={'ignorekeys':'boolean','left':'integer','onpopuphidden':'script code','onpopuphiding':'script code','onpopupshowing':'script code','onpopupshown':'script code','position':'after_start|after_end|before_start|before_end|end_after|end_before|start_after|start_before|overlap|at_pointer|after_pointer','top':'integer'}
let b:XULdict['<menuseparator']={}
let b:XULdict['<observes']={'attribute':'attribute name','element':'broadcaster element id'}
let b:XULdict['<overlay']={'class':'string','id':'element id, must be unique in the window'}
let b:XULdict['<page']={}
let b:XULdict['<popup']={'ignorekeys':'boolean','left':'integer','onpopuphidden':'script code','onpopuphiding':'script code','onpopupshowing':'script code','onpopupshown':'script code','position':'after_start|after_end|before_start|before_end|end_after|end_before|start_after|start_before|overlap|at_pointer|after_pointer','top':'integer'}
let b:XULdict['<popupset']={}
let b:XULdict['<progressmeter']={'mode':'determined|undetermined','value':'inetger'}
let b:XULdict['<radio']={'accesskey':'character','command':'element id','crop':'start|end|left|right|center|none','disabled':'boolean','focused':'boolean','label':'string','selected':'boolean','src':'image URL','value':'string'}
let b:XULdict['<radiogroup']={'disabled':'boolean','focused':'boolean','tabindex':'integer','value':'string'}
let b:XULdict['<rdf:bookmarks']={'BookmarkAddDate (http://home.netscape.com/NC-rdf#BookmarkAddDate)':'','Description (http://home.netscape.com/NC-rdf#Description)':'','LastModifiedDate (http://home.netscape.com/WEB-rdf#LastModifiedDate)':'','LastVisitDate (http://home.netscape.com/WEB-rdf#LastVisitDate)':'','Name (http://home.netscape.com/NC-rdf#Name)':'','Schedule (http://home.netscape.com/WEB-rdf#Schedule)':'','ShortcutURL (http://home.netscape.com/NC-rdf#ShortcutURL)':'','URL (http://home.netscape.com/NC-rdf#URL)':'','status (http://home.netscape.com/WEB-rdf#status)':'','type (http://www.w3.org/1999/02/22-rdf-syntax-ns#type)':'http://home.netscape.com/NC-RDF#Bookmark|http://home.netscape.com/NC-RDF#BookmarkSeparator|http://home.netscape.com/NC-RDF#Folder'}
let b:XULdict['<rdf:files']={'Content-Length (http://home.netscape.com/NC-rdf#Content-Length)':'','LastModifiedDate (http://home.netscape.com/WEB-rdf#LastModifiedDate)':'','Name (http://home.netscape.com/NC-rdf#Name)':'','URL (http://home.netscape.com/NC-rdf#URL)':'','extension (http://home.netscape.com/NC-rdf#extension)':''}
let b:XULdict['<rdf:history']={'Date (http://home.netscape.com/NC-rdf#Date)':'','FirstVisitDate (http://home.netscape.com/NC-rdf#FirstVisitDate)':'','Hostname (http://home.netscape.com/NC-rdf#Hostname)':'','Name (http://home.netscape.com/NC-rdf#Name)':'','Referrer (http://home.netscape.com/NC-rdf#Referrer)':'','URL (http://home.netscape.com/NC-rdf#URL)':'','VisitCount (http://home.netscape.com/NC-rdf#VisitCount)':''}
let b:XULdict['<rdf:window-mediator']={'Name (http://home.netscape.com/NC-rdf#Name)':''}
let b:XULdict['<resizer']={'dir':'left|right|top|bottom|bottomleft|bottomright|topleft|topright'}
let b:XULdict['<row']={}
let b:XULdict['<rows']={}
let b:XULdict['<rule']={'iscontainer':'boolean','isempty':'boolean','parent':'element tag name','parsetype':'string'}
let b:XULdict['<script']={'src':'script URL','type':'language content type'}
let b:XULdict['<scrollbar']={'curpos':'integer','increment':'integer','maxpos':'integer','pageincrement':'integer'}
let b:XULdict['<scrollbox']={}
let b:XULdict['<separator']={'orient':'horizontal|vertical'}
let b:XULdict['<spacer']={}
let b:XULdict['<splitter']={'collapse':'none|before|after','resizeafter':'closest|farthest|grow','resizebefore':'closest|farthest','state':'open|collapsed|dragging'}
let b:XULdict['<stack']={}
let b:XULdict['<statusbar']={}
let b:XULdict['<statusbarpanel']={'crop':'start|end|left|right|center|none','label':'string','src':'image URL'}
let b:XULdict['<stringbundle']={'src':'string properties file URL'}
let b:XULdict['<stringbundleset']={}
let b:XULdict['<tab']={'accesskey':'character','afterselected':'boolean','beforeselected':'boolean','crop':'start|end|left|right|center|none','disabled':'boolean','image':'image URL','label':'string','linkedpanel':'id of a tabpanel element','selected':'boolean','validate':'always|never'}
let b:XULdict['<tabbox']={'eventnode':'parent|window|document','handleCtrlPageUpDown':'boolean','handleCtrlTab':'boolean'}
let b:XULdict['<tabbrowser']={'autocompleteenabled':'boolean','autocompletepopup':'id of popup element','autoscroll':'boolean','contentcontextmenu':'id of a menupopup element','contenttooltip':'id of a tooltip element','handleCtrlPageUpDown':'boolean','onbookmarkgroup':'script code','onnewtab':'script code'}
let b:XULdict['<tabpanel']={}
let b:XULdict['<tabpanels']={'selectedIndex':'integer'}
let b:XULdict['<tabs']={'closebutton':'boolean','disableclose':'boolean','first-tab':'boolean','last-tab':'boolean','onclosetab':'script code','onnewtab':'script code','onselect':'script code','setfocus':'boolean','tooltiptextnew':'string'}
let b:XULdict['<template']={}
let b:XULdict['<textbox']={'cols':'integer','disabled':'boolean','maxlength':'integer','multiline':'boolean','onchange':'script code','oninput':'script code','readonly':'boolean','rows':'integer','size':'integer','tabindex':'integer','timeout':'integer','type':'autocomplete|password|timed','value':'string','wrap':'string'}
let b:XULdict['<textbox (Firefox Auto Complete)']={'accesskey':'character','autocompletepopup':'id of popup element','autocompletesearch':'space separated list of values','autocompletesearchparam':'string','completedefaultindex':'boolean','crop':'start|end|left|right|center|none','disableautocomplete':'boolean','disabled':'boolean','disablekeynavigation':'boolean','enablehistory':'boolean','focused':'boolean','forcecomplete':'boolean','ignoreblurwhilesearching':'boolean','inputtooltiptext':'string','label':'string','maxlength':'integer','maxrows':'integer','minresultsforpopup':'integer','nomatch':'boolean','onchange':'script code','oninput':'script code','onsearchcomplete':'','ontextentered':'','ontextreverted':'','open':'boolean','readonly':'boolean','showcommentcolumn':'boolean','size':'integer','tabindex':'integer','tabscrolling':'boolean','timeout':'integer','type':'string','value':'string'}
let b:XULdict['<textbox (Mozilla Auto Complete)']={'alwaysopenpopup':'boolean','autoFill':'boolean','autoFillAfterMatch':'boolean','crop':'start|end|left|right|center|none','disableAutocomplete':'boolean','disabled':'boolean','disablehistory':'','focused':'boolean','forceComplete':'boolean','ignoreBlurWhileSearching':'boolean','inputtooltiptext':'string','label':'string','maxlength':'integer','maxrows':'','minResultsForPopup':'integer','nomatch':'','onerrorcommand':'','ontextcommand':'','ontextrevert':'','open':'boolean','searchSessions':'space separated list of session names','showCommentColumn':'boolean','showpopup':'boolean','size':'integer','tabScrolling':'boolean','tabindex':'integer','timeout':'','type':'string','userAction':'none|typing|scrolling','value':'string'}
let b:XULdict['<textnode']={'value':'URL of an RDF predicate'}
let b:XULdict['<titlebar']={}
let b:XULdict['<toolbar']={'currentset':'','customindex':'','customizable':'','defaultset':'','grippyhidden':'boolean','grippytooltiptext':'string','toolbarname':''}
let b:XULdict['<toolbarbutton']={'accesskey':'character','autoCheck':'boolean','checkState':'integer: 0, 1 or 2','checked':'boolean','command':'element id','crop':'start|end|left|right|center|none','dir':'ltr|rtl','disabled':'boolean','dlgType':'accept|cancel|help|disclosure','group':'string group name','image':'image URL','label':'string','open':'boolean','orient':'horizontal|vertical','tabindex':'integer','type':'string','validate':'always|never'}
let b:XULdict['<toolbargrippy']={}
let b:XULdict['<toolbaritem']={}
let b:XULdict['<toolbarpalette']={}
let b:XULdict['<toolbarseparator']={}
let b:XULdict['<toolbarset']={}
let b:XULdict['<toolbarspacer']={}
let b:XULdict['<toolbarspring']={}
let b:XULdict['<toolbox']={}
let b:XULdict['<tooltip']={'crop':'start|end|left|right|center|none','default':'boolean','label':'string','noautohide':'boolean','onpopuphidden':'script code','onpopuphiding':'script code','onpopupshowing':'script code','onpopupshown':'script code','position':'after_start|after_end|before_start|before_end|end_after|end_before|start_after|start_before|overlap|at_pointer|after_pointer'}
let b:XULdict['<tree']={'alternatingbackground':'boolean','disableKeyNavigation':'boolean','enableColumnDrag':'boolean','flags':'string','hidecolumnpicker':'boolean','onselect':'script code','rows':'integer','seltype':'single|multiple','statedatasource':'datasource URI'}
let b:XULdict['<treecell']={'label':'string','mode':'none|normal|undetermined','properties':'space separated list of property names','ref':'id of a treecol element','src':'image URL','value':'inetger'}
let b:XULdict['<treechildren']={}
let b:XULdict['<treecol']={'crop':'start|end|left|right|center|none','cycler':'boolean','dragging':'boolean','fixed':'boolean','hidden':'boolean','hideheader':'boolean','ignoreincolumnpicker':'boolean','label':'string','primary':'boolean','sort':'URI of an RDF predicate','sortActive':'boolean','sortDirection':'ascending|descending|natural','src':'image URL','type':'checkbox|progressmeter|text'}
let b:XULdict['<treecols']={'pickertooltiptext':'string'}
let b:XULdict['<treeitem']={'container':'boolean','empty':'boolean','label':'string','open':'boolean','uri':''}
let b:XULdict['<treerow']={'properties':'space separated list of property names'}
let b:XULdict['<treeseparator']={'properties':'space separated list of property names'}
let b:XULdict['<triple']={}
let b:XULdict['<vbox']={}
let b:XULdict['<window']={'height':'integer','hidechrome':'boolean','id':'element id, must be unique in the window','screenX':'integer','screenY':'integer','sizemode':'maximized|minimized|normal','title':'string','width':'integer','windowtype':'string'}
let b:XULdict['<wizard']={'firstpage':'boolean','lastpage':'boolean','onwizardback':'script code','onwizardcancel':'script code','onwizardfinish':'script code','onwizardnext':'script code','pagestep':'integer','title':'string'}
let b:XULdict['<wizardpage']={'description':'string','label':'string','next':'string wizardpage id','onpageadvanced':'script code','onpagehide':'script code','onpagerewound':'script code','onpageshow':'script code','pageid':'string wizardpage id'}
let b:XULdict['<binding']={'display':'string','extends':'binding URL','id':'string','inheritstyle':'boolean'}
let b:XULdict['<bindings']={'id':'element id, must be unique in the window'}
let b:XULdict['<body']={'id':'element id, must be unique in the window'}
let b:XULdict['<children']={'id':'element id, must be unique in the window','includes':''}
let b:XULdict['<constructor']={'action':'','id':'element id, must be unique in the window'}
let b:XULdict['<content']={'id':'element id, must be unique in the window'}
let b:XULdict['<destructor']={'action':'','id':'element id, must be unique in the window'}
let b:XULdict['<field']={'id':'element id, must be unique in the window','name':'','readonly':''}
let b:XULdict['<getter']={'id':'element id, must be unique in the window'}
let b:XULdict['<handler']={'action':'','button':'0|1|2','charcode':'','clickcount':'','command':'','event':'','id':'element id, must be unique in the window','keycode':'','modifiers':'space separated list of the values below','phase':'','preventdefault':''}
let b:XULdict['<handlers']={'id':'element id, must be unique in the window'}
let b:XULdict['<image']={'id':'element id, must be unique in the window','src':''}
let b:XULdict['<implementation']={'id':'element id, must be unique in the window','implements':'','name':''}
let b:XULdict['<method']={'id':'element id, must be unique in the window','name':''}
let b:XULdict['<parameter']={'id':'element id, must be unique in the window','name':''}
let b:XULdict['<property']={'id':'element id, must be unique in the window','name':'','onget':'','onset':'','readonly':''}
let b:XULdict['<resources']={'id':'element id, must be unique in the window'}
let b:XULdict['<setter']={'id':'element id, must be unique in the window'}
let b:XULdict['<stylesheet']={'id':'element id, must be unique in the window','src':''}

fun! CompleteXUL(findstart, base)
	if a:findstart
		let line = getline('.')
		let start = col('.')
		while start > 0 && line[start] != '<'
			let start -= 1
		endwhile
		return start
	else
		" Si on a un matching perfect cela veut dire que l'on cherche
		" les attributs sinon on cherche le nom de la balise
		" correspondante c'est un peu bourrin mais efficace.
		
		let res = []
		let balise = matchstr(a:base, '<\a\+')
		for m in keys(b:XULdict)
			if m =~ '^' . balise . '$' "On est dans le cas d'un renvoi des paramètres
				let res = []
				for a in items(extend(copy(b:XULdict[m]), copy(b:XULelem)))
					call add(res, a:base . ' ' . a[0] . '="' . a[1] . '"')
				endfor
				return res
			else
				if m =~ '^' . a:base
					call add(res, m)
				endif
			endif
		endfor
		return res
	endif
endfun

set completefunc=CompleteXUL

runtime! syntax/xml.vim
