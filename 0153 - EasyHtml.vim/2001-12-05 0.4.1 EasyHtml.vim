" File : EasyHtml.vim
" Last Change: 2001 Dec 02
" Maintainer: Gontran BAERTS <gbcreation@free.fr>
" Version: 0.4.1
"
" Please don't hesitate to correct my english :)
" Send corrections to <gbcreation@free.fr>
"
"-----------------------------------------------------------------------------
" Description: With EasyHtml, you no longer need to look for tags attributes,
" attributes values or CSS properties values while editing HTML files.
" EasyHtml let you select the right attribute or value by showing you an
" attributes/values list for the tag/attribute/CSS property under the cursor.
"
"-----------------------------------------------------------------------------
" To Enable: Normally, this file will reside in your plugins directory and be
" automatically sourced.  If not, you must manually source this file" using :
" source EasyHtml.vim
"
"-----------------------------------------------------------------------------
" Usage: To display the attributes/values list, move the cursor on the
" tag, attribute, or CSS property word and hit <F3> key.
" Use :
"
"       - h,j,k,l or <Left>,<Down>,<Up>,<Right> keys to change selected
"         item.
"       - <PageUp>/<PageDown> or <C-F>,<C-B> keys scroll list one page
"         downward/forward.
"       - <Home> or <C-Home> select the first item.
"       - <End> or <C-End> select the last item.
"       - <ENTER> add selected item and exit from items list.
"       - q or <ESC> to exit without adding selected item.
"
" Deprecated attributes as declared by W3C are red highlighted, while right
" attributes are blue highlighted.
"
" Set g:easyHtmlSplitRight variable to 0 or 1 to open items list at left
" or right of current buffer. By default, use splitright setting.
"
" Set g:eh_singlequote variable to 0 or 1 to use double or single quote when
" adding attributes (For example id="" or id='')
"
" Set g:eh_incsearch variable to 0 or 1 to dis- or en-able incremental list
" search. This feature allows to select an item by typing its beginning. When
" this is enable, 'q', 'h', 'j', 'k' and 'l' keys aren't used to exit from list
" and to move highlighting. Use 'Q', '<Left>', '<Down>', '<Up>' and '<Right>'
" instead.
"
"-----------------------------------------------------------------------------
" Updates:
" in version 0.4.1
" - Fix infinite loop to find window when easyhtml buffer is hidden. Thanks to
"   Jonathon Merz who pointed out the bug and send me the patch.
"
" in version 0.4
" - Added values for the "style" attribute (CSS2 properties)
" - Added values for CSS2 properties
" - <PageUp> and <PageDown> are now usable to move highlight through the list
" - When adding a value for an attribute, current attribute value (if exists)
"   is replaced by the selected one, except for "style" attribute for which
"   values are append
" - Set g:eh_singlequote variable to 0 or 1 to use double or single quote when
"   adding attributes (For example id="" or id='')
"
" in version 0.3
"  - Attributes list updated
"  - Don't display attributes list for closing tags
"  - Now, display values list when hitting <F3> with cursor on attribute word
"    (for some attributes only).
"
" in version 0.2.1
" - Fix global modifiable setting instead of local
"
" in version 0.2
" - Attributes list is now alphabetically sorted
" - Hitting <F3> allows to display attributes list in Insert mode too
" - Allows to select an attribute by incremental search :-)
"   For example, with <body> tag, typing "onk" (normal mode) in the attributes
"   list buffer automatically select "onkeydown" attribute. Use backspace
"   (<BS>) to remove characters. This behavior is enable by setting
"   g:eh_incsearch variable to 1. Warning : when incremental attribute search
"   is on, 'q', 'h', 'j', 'k' and 'l' keys aren't used to exit from list and
"   to move highlighting. Use 'Q', '<Left>', '<Down>', '<Up>' and '<Right>'
"   instead.
" - Check for attributes list already opened, and reuse it
"
" in version 0.1
" - First version " Has this already been loaded ?

if exists("loaded_easyhtml")
       finish
endif
let loaded_easyhtml=1

if !exists("g:easyHtmlSplitRight")
	let g:easyHtmlSplitRight = &splitright
endif

if !exists("g:eh_incsearch")
	let g:eh_incsearch = 0
endif

if !exists("g:eh_singlequote")
	let g:eh_singlequote = 0
endif

:nmap <F3> :call LaunchEasyHtml()<cr>
:imap <F3> <esc>:call LaunchEasyHtml()<cr>

"**
" Script Variables:
"**
let s:srch = ""
let s:maxAttrLength = 0
let s:currentPos = 2
" HTML tags and their attributs
let s:coreattrs = "id=\"\" class=\"\" style=\"\" title=\"\""
let s:i18n = "lang=\"\" dir=\"\""
let s:events = "onclick=\"\" ondblclick=\"\" onmousedown=\"\" onmouseup=\"\" onmouseover=\"\" onmousemove=\"\" onmouseout=\"\" onkeypress=\"\" onkeydown=\"\" onkeyup=\"\""
let s:cellhalign = "align=\"\" char=\"\" charoff=\"\""
let s:cellvalign = "valign=\"\""
let s:attrs = "%coreattrs %i18n %events"
let s:HTMLTags = "<a %attrs charset=\"\" target=\"\" type=\"\" name=\"\" href=\"\" hreflang=\"\" rel=\"\" rev=\"\" accesskey=\"\" shape=\"\" coords=\"\" tabindex=\"\" onfocus=\"\" onblur=\"\""
	\ . ",<abbr %attrs"
	\ . ",<acronym %attrs"
	\ . ",<address %attrs"
	\ . ",<applet %coreattrs alt=\"\" align-D=\"\"-D hspace-D=\"\" vspace-D=\"\" codebase-D=\"\" code-D=\"\" name-D=\"\" archive-D=\"\" object-D=\"\" width-D=\"\" height-D=\"\""
	\ . ",<area %attrs shape=\"\" coords=\"\" nohref name=\"\" alt=\"\" href=\"\" tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" target=\"\""
	\ . ",<b %attrs"
	\ . ",<base href=\"\" target=\"\""
	\ . ",&lt;basefont size-D=\"\" color-D=\"\" face-D=\"\""
	\ . ",<bdo %coreattrs lang=\"\" dir=\"\""
	\ . ",<big %attrs"
	\ . ",<blockquote %attrs cite=\"\""
	\ . ",<body %attrs onload=\"\" onunload=\"\" background-D=\"\" bgcolor-D=\"\" text-D=\"\" link-D=\"\" vlink-D=\"\" alink-D=\"\""
	\ . ",<br %coreattrs clear-D=\"\""
	\ . ",<button %attrs name=\"\" value=\"\" type=\"\" disabled tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\""
	\ . ",<caption %attrs align-D=\"\""
	\ . ",<center %attrs"
	\ . ",<cite %attrs"
	\ . ",<code %attrs"
	\ . ",<col %attrs span=\"\" width=\"\" %cellhalign %cellvalign"
	\ . ",<colgroup %attrs span=\"\" width=\"\" %cellhalign %cellvalign"
	\ . ",<dd %attrs"
	\ . ",<del %attrs cite=\"\" datetime=\"\""
	\ . ",<dfn %attrs"
	\ . ",<dir %attrs compact-D"
	\ . ",<div %attrs align-D=\"\""
	\ . ",<dl %attrs compact-D"
	\ . ",<dt %attrs"
	\ . ",<em %attrs"
	\ . ",<fieldset %attrs"
	\ . ",<font %coreattrs %i18n size-D=\"\"-D color-D=\"\" face-D=\"\""
	\ . ",<form %attrs action=\"\" method=\"\" enctype=\"\" name=\"\" onsubmit=\"\" onreset=\"\" accept=\"\" accept-charset=\"\" target=\"\""
	\ . ",<frame %coreattrs longdesc=\"\" name=\"\" src=\"\" frameborder=\"\" marginwidth=\"\" marginheight=\"\" noresize scrolling=\"\""
	\ . ",<frameset %coreattrs rows=\"\" cols=\"\" onload=\"\" onunload=\"\""
	\ . ",<h1 %attrs align-D=\"\""
	\ . ",<h2 %attrs align-D=\"\""
	\ . ",<h3 %attrs align-D=\"\""
	\ . ",<h4 %attrs align-D=\"\""
	\ . ",<h5 %attrs align-D=\"\""
	\ . ",<h6 %attrs align-D=\"\""
	\ . ",<head %i18n profile=\"\""
	\ . ",<hr %attrs align-D=\"\" noshade-D size-D=\"\" width-D=\"\""
	\ . ",<html %i18n version-D=\"\""
	\ . ",<i %attrs"
	\ . ",<iframe %coreattrs longdesc=\"\" name=\"\" src=\"\" frameborder=\"\" marginwidth=\"\" marginheight=\"\" scrolling=\"\" align-D=\"\" height=\"\" width=\"\""
	\ . ",<img %attrs src=\"\" alt=\"\" longdesc=\"\" name=\"\" height=\"\" width=\"\" usemap=\"\" ismap align-D=\"\" border-D=\"\" hspace-D=\"\" vspace-D=\"\""
	\ . ",<input %attrs type=\"\" name=\"\" value=\"\" checked disabled readonly size=\"\" maxlength=\"\" src=\"\" alt=\"\" usemap=\"\" ismap tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" onselect=\"\" onchange=\"\" accept=\"\" align-D=\"\""
	\ . ",<ins %attrs cite=\"\" datetime=\"\""
	\ . ",<isindex %coreattrs %i18n prompt-D=\"\""
	\ . ",<kbd %attrs"
	\ . ",<label %attrs for=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\""
	\ . ",<legend %attrs accesskey=\"\" align-D=\"\""
	\ . ",<li %attrs type-D=\"\" value-D=\"\""
	\ . ",<link %attrs charset=\"\" href=\"\" hreflang=\"\" type=\"\" rel=\"\" rev=\"\" media=\"\" target=\"\""
	\ . ",<map %attrs name=\"\""
	\ . ",<menu %attrs compact-D"
	\ . ",<meta %i18n http-equiv=\"\" name=\"\" content=\"\" scheme=\"\""
	\ . ",<noframes %attrs"
	\ . ",<noscript %attrs"
	\ . ",<object %attrs declare classid=\"\" codebase=\"\" data=\"\" type=\"\" codetype=\"\" archive=\"\" standby=\"\" height=\"\" width=\"\" usemap=\"\" name=\"\" tabindex=\"\" align-D=\"\" border-D=\"\" hspace-D=\"\" vspace-D=\"\""
	\ . ",<ol %attrs type-D=\"\" start-D=\"\" compact-D"
	\ . ",<optgroup %attrs disabled label=\"\""
	\ . ",<option %attrs selected disabled label=\"\" value=\"\""
	\ . ",<p %attrs align-D=\"\""
	\ . ",<param id=\"\" name=\"\" value=\"\" valuetype=\"\" type=\"\""
	\ . ",<pre %attrs width-D=\"\""
	\ . ",<q %attrs cite=\"\""
	\ . ",<s %attrs"
	\ . ",<samp %attrs"
	\ . ",<script charset=\"\" type=\"\" src=\"\" defer language-D=\"\""
	\ . ",<select %attrs name=\"\" size=\"\" multiple disabled tabindex=\"\" onfocus=\"\" onblur=\"\" onchange=\"\""
	\ . ",<small %attrs"
	\ . ",<span %attrs"
	\ . ",<strike %attrs"
	\ . ",<strong %attrs"
	\ . ",<style %i18n type=\"\" media=\"\" title=\"\""
	\ . ",<sub %attrs"
	\ . ",<sup %attrs"
	\ . ",<table %attrs summary=\"\" width=\"\" border=\"\" frame=\"\&quot; rules=\"\" cellspacing=\"\" cellpadding=\"\" align-D=\"\" bgcolor-D=\"\""
	\ . ",<tbody %attrs %cellhalign %cellvalign"
	\ . ",<td %attrs abbr=\"\" axis=\"\" headers=\"\" scope=\"\" rowspan=\"\" colspan=\"\" %cellhalign %cellvalign nowrap-D width-D=\"\" height-D=\"\" bgcolor-D=\"\""
	\ . ",<textarea %attrs name=\"\" rows=\"\" cols=\"\" disabled readonly tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" onselect=\"\" onchange=\"\""
	\ . ",<tfoot %attrs %cellhalign %cellvalign"
	\ . ",<th %attrs abbr=\"\" axis=\"\" headers=\"\" scope=\"\" rowspan=\"\" colspan=\"\" %cellhalign %cellvalign nowrap-D width-D=\"\" height-D=\"\" bgcolor-D=\"\""
	\ . ",<thead %attrs %cellhalign %cellvalign"
	\ . ",<title %i18n"
	\ . ",<tr %attrs %cellhalign %cellvalign bgcolor-D=\"\""
	\ . ",<tt %attrs"
	\ . ",<u %attrs"
	\ . ",<ul %attrs type-D=\"\" compact-D"
	\ . ",<var %attrs"
" Attributes and their values
let s:TagsAttributs = "align=\" bottom center char justify left middle right top"
	\ . ",alink=\" Aqua Black Blue Fuchsia Gray Green Lime Maroon Navy Olive Purple Red Silver Teal White Yellow"
	\ . ",bgcolor=\" Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"
	\ . ",charset=\" ISO-8859-1 SHIFT_JIS UTF-8"
	\ . ",clear=\" all none left right"
	\ . ",color=\" Aqua Black Blue Fuchsia Gray Green Lime Maroon Navy Olive Purple Red Silver Teal White Yellow"
	\ . ",content=\" text/css"
	\ . ",dir=\" ltr rtl"
	\ . ",frameborder=\" 0 1"
	\ . ",frame=\" above below border box hsides lhs rhs void vsides"
	\ . ",hreflang=\" aa ab af am ar as ay az ba be bg bh bi bn bo br ca co cs cy da de dz el en eo es et eu fa fi fj fo fr fy ga gd gl gn gu ha he hi hr hu hy ia id ie ik is it iu ja jv ka kk kl km kn ko ks ku ky la ln lo lt lv mg mi mk ml mn mo mr ms mt my na ne nl no oc om or pa pl ps pt qu rm rn ro ru rw sa sd sg sh si sk sl sm sn so sq sr ss st su sv sw ta te tg th ti tk tl tn to tr ts tt tw ug uk ur uz vi vo wo xh yi yo za zh zu"
	\ . ",http-equiv=\" Content-Script-Type Content-Style-Type Content-Type Expires PICS-Label"
	\ . ",lang=\" aa ab af am ar as ay az ba be bg bh bi bn bo br ca co cs cy da de dz el en eo es et eu fa fi fj fo fr fy ga gd gl gn gu ha he hi hr hu hy ia id ie ik is it iu ja jv ka kk kl km kn ko ks ku ky la ln lo lt lv mg mi mk ml mn mo mr ms mt my na ne nl no oc om or pa pl ps pt qu rm rn ro ru rw sa sd sg sh si sk sl sm sn so sq sr ss st su sv sw ta te tg th ti tk tl tn to tr ts tt tw ug uk ur uz vi vo wo xh yi yo za zh zu"
	\ . ",link=\" Aqua Black Blue Fuchsia Gray Green Lime Maroon Navy Olive Purple Red Silver Teal White Yellow"
	\ . ",media=\" all aural braille handheld print projection screen tty tv"
	\ . ",method=\" get post"
	\ . ",name=\" Author copyright date keywords"
	\ . ",rel=\" Alternate Appendix Bookmark Chapter Contents Copyright Glossary Help Index Made Next Prev Section Start StyleSheet Subsection"
	\ . ",rev=\" Alternate Appendix Bookmark Chapter Contents Copyright Glossary Help Index Made Next Prev Section Start StyleSheet Subsection"
	\ . ",rules=\" all cols groups none rows"
	\ . ",scope=\" col colgroup row rowgroup"
	\ . ",scrolling=\" auto no yes"
	\ . ",shape=\" circle default poly rect"
	\ . ",style=\" azimuth:; background:; background-attachment:; background-color:; background-image:; background-position:; background-repeat:; border:; border-bottom:; border-bottom-color:; border-bottom-style:; border-bottom-width:; border-collapse:; border-color:; border-left:; border-left-color:; border-left-style:; border-left-width:; border-right:; border-right-color:; border-right-style:; border-right-width:; border-spacing:; border-style:; border-top:; border-top-color:; border-top-style:; border-top-width:; border-width:; bottom:; caption-side:; clear:; clip:; color:; content:; counter-increment:; counter-reset:; cue:; cue-after:; cue-before:; cursor:; direction:; display:; elevation:; empty-cells:; float:; font:; font-family:; font-size:; font-size-adjust:; font-stretch:; font-style:; font-variant:; font-weight:; height:; left:; letter-spacing:; line-height:; list-style:; list-style-image:; list-style-position:; list-style-type:; margin:; margin-bottom:; margin-left:; margin-right:; margin-top:; marker-offset:; marks:; max-height:; max-width:; min-height:; min-width:; orphans:; outline:; outline-color:; outline-style:; outline-width:; overflow:; padding:; padding-bottom:; padding-left:; padding-right:; padding-top:; page:; page-break-after:; page-break-before:; page-break-inside:; pause:; pause-after:; pause-before:; pitch:; pitch-range:; play-during:; position:; quotes:; richness:; right:; size:; speak:; speak-header:; speak-numeral:; speak-punctuation:; speech-rate:; stress:; table-layout:; text-align:; text-decoration:; text-indent:; text-shadow:; text-transform:; top:; unicode-bidi:; vertical-align:; visibility:; voice-family:; volume:; white-space:; widows:; width:; word-spacing:; z-index:;"
	\ . ",target=\" _blank _parent _self _top"
	\ . ",text=\" Aqua Black Blue Fuchsia Gray Green Lime Maroon Navy Olive Purple Red Silver Teal White Yellow"
	\ . ",type=\" 1 a A application/java button checkbox circle disc file hidden i I image image/jpeg model/vrml password radio reset square submit text text/css text/html text/javascript video/quicktime"
	\ . ",valign=\" baseline bottom middle top"
	\ . ",valuetype=\" data object ref"
	\ . ",vlink=\" Aqua Black Blue Fuchsia Gray Green Lime Maroon Navy Olive Purple Red Silver Teal White Yellow"
" CSS properties and their values
let s:angle = "deg grad rad"
let s:color = "# ActiveBorder ActiveCaption AppWorkspace Background ButtonFace ButtonHighlight ButtonShadow ButtonText CaptionText GrayText Highlight HighlightText InactiveBorder InactiveCaption InactiveCaptionText InfoBackground InfoText Menu MenuText rgb() Scrollbar ThreeDDarkShadow ThreeDFace ThreeDHighlight ThreeDLightShadow ThreeDShadow Window WindowFrame WindowText"
let s:length = "cm em ex in mm pc pt px"
let s:borderstyle = "dashed dotted double groove hidden inset none outset ridge solid"
let s:borderwidth = "%length medium thick thin"
let s:genericfamily = "cursive fantasy monospace sans-serif serif"
let s:absolutesize = "large medium small x-large x-small xx-large xx-small"
let s:relativesize = "larger smaller"
let s:marginwidth = "auto %length"
let s:time = "ms s"
let s:frequency = "Hz kHz"
let s:genericvoice = "child female male"
let s:CSSProperties = "azimuth: %angle behind center center-left center-right far-left far-right inherit left left-side leftwards right right-side rightwards,"
	\ . "background: bottom left center %color fixed inherit %length none no-repeat repeat repeat-x repeat-y right scroll top transparent url(),"
	\ . "background-attachment: fixed inherit scroll,"
	\ . "background-color: %color inherit transparent,"
	\ . "background-image: inherit none url(),"
	\ . "background-position: bottomleft center inherit %length right top,"
	\ . "background-repeat: inherit no-repeat repeat repeat-x repeat-y,"
	\ . "border: %borderstyle %borderwidth %color inherit,"
	\ . "border-collapse: collapse inherit separate,"
	\ . "border-color: %color inherit transparent,"
	\ . "border-spacing: inherit %length,"
	\ . "border-style: %borderstyle inherit,"
	\ . "border-top: %borderstyle %borderwidth %color inherit,"
	\ . "border-right: %borderstyle %borderwidth %color inherit,"
	\ . "border-bottom: %borderstyle %borderwidth %color inherit,"
	\ . "border-left: %borderstyle %borderwidth %color inherit,"
	\ . "border-top-color: %color inherit,"
	\ . "border-right-color: %color inherit,"
	\ . "border-bottom-color: %color inherit,"
	\ . "border-left-color: %color inherit,"
	\ . "border-top-style: %borderstyle inherit,"
	\ . "border-right-style: %borderstyle inherit,"
	\ . "border-bottom-style: %borderstyle inherit,"
	\ . "border-left-style: %borderstyle inherit,"
	\ . "border-top-width: %borderwidth inherit,"
	\ . "border-right-width: %borderwidth inherit,"
	\ . "border-bottom-width: %borderwidth inherit,"
	\ . "border-left-width: %borderwidth inherit,"
	\ . "border-width: %borderwidth inherit,"
	\ . "bottom: auto inherit %length,"
	\ . "caption-side: bottom inherit left right top,"
	\ . "clear: both inherit left none right,"
	\ . "clip: auto inherit rect(),"
	\ . "color: %color inherit,"
	\ . "content: attr(X) close-quote counter() inherit no-close-quote no-open-quote open-quote url(),"
	\ . "counter-increment: inherit none,"
	\ . "counter-reset: inherit none,"
	\ . "cue: inherit none url(),"
	\ . "cue-after: inherit none url(),"
	\ . "cue-before: inherit none url(),"
	\ . "cursor: auto crosshair default e-resize help inherit move ne-resize n-resize nw-resize pointer se-resize s-resize sw-resize url() wait w-resizetext,"
	\ . "direction: inherit ltr rtl,"
	\ . "display: block compact inherit inline inline-table list-item marker none run-in table table-caption table-cell table-column table-column-group table-footer-group table-header-group table-row table-row-group,"
	\ . "elevation: above %angle below higher inherit level lower,"
	\ . "empty-cells: hide inherit show,"
	\ . "float: inherit left none right,"
	\ . "font: 100 200 300 400 500 600 700 800 900 %absolutesize bold bolder caption %genericfamily icon inherit italic %length lighter menu message-box normal oblique %relativesize small-caps small-caption status-bar,"
	\ . "font-family: %genericfamily inherit,"
	\ . "font-size: %absolutesize inherit %length %relativesize,"
	\ . "font-size-adjust: inherit none,"
	\ . "font-stretch: condensed expanded extra-condensed extra-expanded inherit narrower normal semi-condensed semi-expanded ultra-condensed ultra-expanded wider,"
	\ . "font-style: inherit italic normal oblique,"
	\ . "font-variant: inherit normal small-caps,"
	\ . "font-weight: 100 200 300 400 500 600 700 800 900 bold bolder inherit lighter normal,"
	\ . "height: auto inherit %length,"
	\ . "left: auto inherit %length,"
	\ . "letter-spacing: inherit %length normal,"
	\ . "line-height: inherit %length normal,"
	\ . "list-style: armenian circle cjk-ideographic decimal decimal-leading-zero disc georgian hebrew hiragana hiragana-iroha inherit inside katakana katakana-iroha lower-alpha lower-greek lower-latin lower-roman none outside square upper-alpha upper-latin upper-roman url(),"
	\ . "list-style-image: inherit none url(),"
	\ . "list-style-position: inherit inside outside,"
	\ . "list-style-type: armenian circle cjk-ideographic decimal decimal-leading-zero disc georgian hebrew hiragana hiragana-iroha inherit katakana katakana-iroha lower-alpha lower-greek lower-latin lower-roman none square upper-alpha upper-latin upper-roman,"
	\ . "margin: inherit %marginwidth,"
	\ . "margin-top: inherit %marginwidth,"
	\ . "margin-right: inherit %marginwidth,"
	\ . "margin-bottom: inherit %marginwidth,"
	\ . "margin-left: inherit %marginwidth,"
	\ . "marker-offset: auto inherit %length,"
	\ . "marks: crop cross inherit none,"
	\ . "max-height: inherit %length none,"
	\ . "max-width: inherit %length none,"
	\ . "min-height: inherit %length,"
	\ . "min-width: inherit %length,"
	\ . "orphans: inherit,"
	\ . "outline: inherit %color invert %borderstyle %borderwidth,"
	\ . "outline-color: %color inherit invert,"
	\ . "outline-style: %borderstyle inherit,"
	\ . "outline-width: %borderwidth inherit,"
	\ . "overflow: auto hidden inherit scroll visible,"
	\ . "padding: inherit %length,"
	\ . "padding-top: inherit %length,"
	\ . "padding-right: inherit %length,"
	\ . "padding-bottom: inherit %length,"
	\ . "padding-left: inherit %length,"
	\ . "page: auto,"
	\ . "page-break-after: always auto avoid inherit left right,"
	\ . "page-break-before: always auto avoid inherit left right,"
	\ . "page-break-inside: auto avoid inherit,"
	\ . "pause: inherit %time,"
	\ . "pause-after: inherit %time,"
	\ . "pause-before: inherit %time,"
	\ . "pitch: %frequency high inherit low medium x-high x-low,"
	\ . "pitch-range: inherit,"
	\ . "play-during: auto inherit mix none repeat url(),"
	\ . "position: absolute fixed inherit relative static,"
	\ . "quotes: inherit none,"
	\ . "richness: inherit,"
	\ . "right: auto inherit %length,"
	\ . "size: auto inherit landscape %length portrait,"
	\ . "speak: inherit none normal spell-out,"
	\ . "speak-header: always inherit once,"
	\ . "speak-numeral: continuous digits inherit,"
	\ . "speak-punctuation: code inherit none,"
	\ . "speech-rate: fast faster inherit medium slow slower x-fast x-slow,"
	\ . "stress: inherit,"
	\ . "table-layout: auto fixed inherit,"
	\ . "text-align: center inherit justify left right,"
	\ . "text-decoration: blink inherit line-through none overline underline,"
	\ . "text-indent: inherit %length,"
	\ . "text-shadow: %color inherit %length none,"
	\ . "text-transform: capitalize inherit lowercase none uppercase,"
	\ . "top: auto inherit %length,"
	\ . "unicode-bidi: bidi-override embed inherit normal,"
	\ . "vertical-align: baseline bottom inherit %length middle sub super text-bottom text-top top,"
	\ . "visibility: collapse hidden inherit visible,"
	\ . "voice-family: %genericvoice inherit,"
	\ . "volume: inherit loud medium silent soft x-loud x-soft,"
	\ . "white-space: inherit normal nowrap pre,"
	\ . "widows: inherit,"
	\ . "width: auto inherit %length,"
	\ . "word-spacing: inherit %length normal,"
	\ . "z-index: auto inherit,"

"**
" List Functions:
"**
function! GetListItem( array, index )
	if a:index == 0
		return matchstr( a:array, '^[^' . s:listSep . ']\+' )
	else
		return matchstr( a:array, "[^" . s:listSep . "]\\+", matchend( a:array, '\(\%(^\|' . s:listSep . '\)[^' . s:listSep . ']\+\)\{' . a:index . '\}' . s:listSep ) )
	endif
endfunction

function! GetListMatchItem( array, pattern )
	return matchstr( a:array, '[^' . s:listSep . ']*' . a:pattern . '[^' . s:listSep . ']*' )
endfunction

function! ReplaceListItem( array, index, item )
	if a:index == 0
		return substitute( a:array, '^[^' .s:listSep. ']\+', a:item, "" )
	else
		return substitute( a:array, '\(\%(\%(^\|' . s:listSep . '\)[^' . s:listSep . ']\+\)\{' . a:index . '\}\)' . s:listSep . '[^' . s:listSep . ']\+', '\1' . s:listSep . a:item , "" )
	endif
endfunction

function! RemoveListItem( array, index )
	if a:index == 0
		return substitute( a:array, '^[^' .s:listSep. ']\+\(' . s:listSep . '\|$\)', "", "" )
	else
		return substitute( a:array, '\(\%(\%(^\|' . s:listSep . '\)[^' . s:listSep . ']\+\)\{' . a:index . '\}\)' . s:listSep . '[^' . s:listSep . ']\+', '\1', "" )
	endif
endfunction

function! GetListCount( array )
	if a:array == "" | return 0 | endif
	let pos = 0
	let cnt = 0
	while pos != -1
		let pos = matchend( a:array, s:listSep, pos )
		let cnt = cnt + 1
	endwhile
	return cnt
endfunction

function! QuickSortList( tabEnt, deb, fin )
	let tabEnt = a:tabEnt
	let pivot = GetListItem( tabEnt, a:deb )
	let g = a:deb
	let d = a:fin
	while g < d
		while GetListItem( tabEnt, d ) > pivot
			let d = d - 1
		endwhile
		if g != d
			let tabEnt = ReplaceListItem( tabEnt, g, GetListItem( tabEnt, d ) )
			let tabEnt = ReplaceListItem( tabEnt, d, pivot )
			let g = g + 1
		endif

		while GetListItem( tabEnt, g ) < pivot
			let g = g + 1
		endwhile
		if g != d
			let tabEnt = ReplaceListItem( tabEnt, d, GetListItem( tabEnt, g ) )
			let tabEnt = ReplaceListItem( tabEnt, g, pivot )
			let d = d - 1
		endif
	endwhile
	if a:deb < g-1
		let tabEnt = QuickSortList( tabEnt, a:deb, g-1 )
	endif
	if a:fin > g+1
		let tabEnt = QuickSortList( tabEnt, g+1, a:fin )
	endif
	return tabEnt
endfunction

"**
" LaunchEasyHtml:
" Search if there are attributs for word under cursor, and display them in a
" new buffer.
"**
function! LaunchEasyHtml()
	" Look for attributs for the current word
	call s:SearchAttributes()
	" If the longest attribut length is 0, there is no attribut for the
	" current word
	if s:maxAttrLength == 0
		echohl ErrorMsg
		echo "No attributes\\values found. (If it's a closing tag, try on opening tag.)"
		echohl NONE
		return
	endif

	" Is there an attributes list already running and is it in a window?
	let BufNr = bufnr( '--\ EasyHtml\ --' )
	if BufNr != -1 && bufwinnr(BufNr) != -1
		let CurBufNr = bufnr("%")
		while CurBufNr != BufNr
			wincmd w
			let CurBufNr = bufnr("%")
		endwhile
	let BufNr = bufnr( '--\ EasyHtml\ --' )
	else
		" Save the user's settings for splitright
		let savesplitright = &splitright
		" Configure vertical splitting side
		let &splitright = g:easyHtmlSplitRight
		" Open new vertical window with right size
		execute s:maxAttrLength . 'vnew --\ EasyHtml\ --'
		" Restore user settings
		let &splitright = savesplitright
		" Turn off the swapfile, set the buffer type so that it won't get
		" written, and so that it will get deleted when it gets hidden.
		setlocal modifiable
		setlocal noswapfile
		setlocal buftype=nowrite
		setlocal bufhidden=delete
		setlocal nonumber
		" Don't wrap around long lines
		setlocal nowrap
		" No need for any insertmode abbreviations, since we don't allow
		" insertions anyway!
		iabc <buffer>
		" Highlighting
		syntax match selectedAttribut /^<.*>$/
		syntax match deprecatedAttribut /^(.*)$/
		syntax match hiddenX /X/
		hi selectedAttribut guibg=lightblue guifg=black
		hi deprecatedAttribut guibg=lightred guifg=black
		let color= s:GetBgColor()
		if color != ""
			exe "hi hiddenX guibg=" . color . " guifg=" . color
		endif

		" Set up mappings for this buffer
		nnoremap <buffer> <Left> :call <SID>MoveSelect( line(".")-1 )<CR>
		nnoremap <buffer> <Up> :call <SID>MoveSelect( line(".")-1 )<CR>
		nnoremap <buffer> <Right> :call <SID>MoveSelect( line(".")+1 )<CR>
		nnoremap <buffer> <Down> :call <SID>MoveSelect( line(".")+1 )<CR>
		nnoremap <buffer> <PageUp> :call <SID>PageUp()<cr>
		nnoremap <buffer> <PageDown> :call <SID>PageDown()<cr>
		nnoremap <buffer> <C-Home> :call <SID>MoveSelect( 1 )<cr>
		nnoremap <buffer> <C-End> :call <SID>MoveSelect( line("$") )<cr>
		nnoremap <buffer> <Home> :call <SID>MoveSelect( 1 )<cr>
		nnoremap <buffer> <End> :call <SID>MoveSelect( line("$") )<cr>
		nnoremap <buffer> <cr> :call <SID>AddItem()<cr>
		nnoremap <buffer> <2-LeftMouse> :call <SID>AddItem()<cr>
		nnoremap <buffer> <esc> :call <SID>CloseWindow()<cr>

		" If incremental search required, initialize it
		if( g:eh_incsearch == 1 )
			nnoremap <buffer> Q :call <SID>CloseWindow()<cr>
			nnoremap <buffer> <BS> :call <SID>SelectSearch( "" )<cr>
			let char = 97
			while char < 123
				exe "nnoremap <buffer> " . nr2char(char) . " :call <SID>SelectSearch( '" . nr2char(char) . "' )<cr>"
				let char = char + 1
			endwhile
		else
			nnoremap <buffer> h :call <SID>MoveSelect( line(".")-1 )<CR>
			nnoremap <buffer> k :call <SID>MoveSelect( line(".")-1 )<CR>
			nnoremap <buffer> l :call <SID>MoveSelect( line(".")+1 )<CR>
			nnoremap <buffer> j :call <SID>MoveSelect( line(".")+1 )<CR>
			nnoremap <buffer> q :call <SID>CloseWindow()<cr>
		endif
	endif
	" Reset incremental search
	let s:srch = ""
	" Fill attributs list
	call s:ShowAttributes()
	" User don't need to modify content
	setlocal nomodifiable
endfunction

"**
" SearchAttributes:
" Look for attributs for word under cursor. tr
"**
function! s:SearchAttributes()
	" Ignore case
	let l:CurrentCase = &ignorecase
	set ignorecase
	let l:CurrentIkw = &iskeyword

	let s:attributs = ""
	let s:maxAttrLength = 0
	let s:listSep = ","
	let l:attributsLine = ""

	setlocal iskeyword +=<
	if match( expand("<cword>"), '^<' ) == 0 " Is it a tag ?
		let s:itemtype = "T" " Yes, a tag
		let l:attributsLine = GetListMatchItem( s:HTMLTags, expand("<cword>") . ' ' )
	else
		setlocal iskeyword -=<
		setlocal iskeyword +=-,=
		if match( expand("<cword>"), '=$' ) != -1 " or an attribute ?
			let s:itemtype = "A" " Yes, an attribute
			let l:attributsLine = GetListMatchItem( s:TagsAttributs, expand("<cword>") . '" ' )
		else
			setlocal iskeyword -==
			setlocal iskeyword +=:
			if match( expand("<cword>"), ':' ) != -1 " or a CSS property ?
				let s:itemtype = "C" " Yes, a CSS property
				let l:attributsLine = GetListMatchItem( s:CSSProperties, '\<' . matchstr( expand("<cword>"), '^.\{-}:' ) . ' ' )
			endif
		endif
	endif

	if l:attributsLine != ""
		let s:listSep = " "
		let l:attributsLine = RemoveListItem( l:attributsLine, 0 )
		if l:attributsLine != ""
			" Attributes values are already sorted and expanded
			if s:itemtype != "A"
				" Insert %xxxx variables content
				let l:attribut = matchstr( l:attributsLine, '%[^ ]\+' )
				while l:attribut != ""
					exe "let l:attributsLine = substitute( l:attributsLine, '" .l:attribut. "', s:" . strpart( l:attribut, 1 ) . ", '')"
					let l:attribut = matchstr( l:attributsLine, '%[^ ]\+' )
				endwhile                                 " Sort items
				let l:attributsLine = QuickSortList( l:attributsLine, 0, GetListCount(l:attributsLine)-1 )
			endif
			let l:attribut = GetListItem( l:attributsLine, 0 )
			while l:attribut != ""
				" Keep max length
				if s:maxAttrLength < strlen( l:attribut )
					let s:maxAttrLength = strlen( l:attribut )
				endif
				" Remove current attribut
				let l:attributsLine = RemoveListItem( l:attributsLine, 0 )
				" Is it a depracated attribute ?
				set noignorecase
				if l:attribut =~ "-D"
					let l:attribut = substitute( l:attribut, "-D", "", "")
					let s:attributs = s:attributs . "X" . l:attribut . " \n"
				else
					let s:attributs = s:attributs . " " . l:attribut . " \n"
				endif
				" Next attribut
				let l:attribut = GetListItem( l:attributsLine, 0 )
			endwhile
		endif
		" If longest attribute size is zero, then there is no attribute
		" for this tag
		if s:maxAttrLength != 0
			let s:maxAttrLength = s:maxAttrLength + 2
		endif
	endif
	let &iskeyword = l:CurrentIkw
	let &ignorecase = l:CurrentCase
endfunction

"**
" ShowAttributes:
" Display attributs list in current buffer.
"**
function! s:ShowAttributes()
	" Prevent a report of our actions from showing up
	let oldRep=&report
	let save_sc = &sc
	set report=10000 nosc
	setlocal modifiable
	" Erase content
	%delete
	" Put content of register f after the cursor
	put! =s:attributs
	" Erase last line
	exe "normal G"
	d
	" Move to first item
	call s:MoveSelect(1)
	set nomodifiable

	" Restore config
	let &report=oldRep
	let &sc = save_sc
endfunction

"**
" AddItem:
" Add selected item to tag/attribute/CSS2 property.
"**
function! s:AddItem()
	let l:CurrentCase = &ignorecase
	set noignorecase
	" Get attribute and clean it
	let save_f=@f
	let @f = substitute( getline("."), '[<>X]', '', "g" )
	let @f = substitute( @f, '^(', '', "g" )
	let @f = substitute( @f, ' \+)$', '', "g" )
	let @f = substitute( @f, ' ', '', "g" )
	let &ignorecase = l:CurrentCase
	" Go to previous window
	wincmd p
	" if it's a tag, put attribute at end of tag
	if s:itemtype == "T"
		if g:eh_singlequote == 1
			let @f = substitute( @f , '"', "'", 'g' )
		endif
		let @f = ' ' . @f
		exec 'normal f>"fP'
	elseif s:itemtype == "A" " If it's an attribute ...
		if g:eh_singlequote == 0
			let quote = '"'
		else
			let quote = "'"
		endif
		if expand("<cword>") !~ 'style' " ... append selected value to it for style
			exec 'normal 2f' .quote. 'dT' .quote. '"fP'
		else " ... or replace current value by selected value
			exec 'normal f' .quote. '"fp'
		endif
	elseif s:itemtype == "C" " If it's an CSS property, add value to it
		exec 'normal f:"fgp'
	endif
	startinsert
	" Return to attributes window
	wincmd p
	let @f=save_f
	" Close window
	call s:CloseWindow()
endfunction

"**
" MoveSelect:
" Move highlight to line newLineNumber.
"
" Parameter:
" newLineNumber line number to highlight.
"**
function! s:MoveSelect( newLineNumber )
	if( a:newLineNumber < 1 || a:newLineNumber > line("$") )
		return
	endif
	setlocal modifiable
	" Restore current line
	if( exists("s:currentLine") )
		call setline( ".", s:currentLine )
	endif
	" Go to new line
	let s:currentPos = a:newLineNumber
	exec s:currentPos
	" Save new current line
	let s:currentLine = getline(".")
	let modifiedLine = s:currentLine
	" Complete string with spaces
	let len = strlen(l:modifiedLine)
	while len < s:maxAttrLength
		let modifiedLine = modifiedLine . " "
		let len = len + 1
	endwhile

	" Is it a deprecated attribute marked with 'X' ?
	if l:modifiedLine =~ "^X"
		let modifiedLine = substitute( modifiedLine, "^X", "(", "" )
		let modifiedLine = substitute( modifiedLine, " $", ")", "" )
	else
		let modifiedLine = substitute( modifiedLine, "^ ", "<", "" )
		let modifiedLine = substitute( modifiedLine, " $", ">", "" )
	endif
	call setline( ".", l:modifiedLine )
	setlocal nomodifiable
endfunction

"**
" PageDown:
" Move highlight one page down.
"**
function! s:PageDown()
	exe "normal L"
	let pos = line(".")
	exe "normal ''"
	call s:MoveSelect( pos )
	exe "normal zt"
endfunction

"**
" PageUp:
" Move highlight one page up.
"**
function! s:PageUp()
	exe "normal H"
	let pos = line(".")
	exe "normal ''"
	call s:MoveSelect( pos )
	exe "normal zb"
endfunction

"**
" CloseWindow:
" Clear unused variables and highlights, reinit variables for next use and
" close current window.
"**
function! s:CloseWindow()
	unlet s:currentLine
	unlet s:attributs
	let s:maxAttrLength = 0
	let s:currentPos = 2
	highlight clear selectedAttribut
	highlight clear deprecatedAttribut
	highlight clear hiddenX
	wincmd q
endfunction

"**
" GetBgColor:
" Try to get background color (may be not sure)
"**
function! s:GetBgColor()
	let bgColor = synIDattr(synIDtrans(synID(1, 1, 1)), "bg")
	if bgColor == ""
		if &background == "light"
			let bgColor = "white"
		else
			let bgColor = "black"
		endif
	endif
	return bgColor
endfunction

"**
" SelectSearch:
" Used for incremental attribut search
"
" Parameter:
" Char  character to add to current search pattern
"**
function! s:SelectSearch( char )
	if a:char == "" && s:srch != ""
		let s:srch = strpart( s:srch, 0, strlen( s:srch )-1 )
	else
		let s:srch = s:srch . a:char
	endif
	let linenr = line(".")
	if s:srch != ""
		1
		let findlinenr =  search( '\(\<\|X\)'.s:srch, "W" )
		exe ":".linenr
		if findlinenr != 0
			echo "Attributs search : " . s:srch
			call s:MoveSelect( findlinenr )
		else
			echohl ErrorMsg
			echo "No attribut for \"" . s:srch . "\" (use backspace)"
			echohl NONE
		endif
	endif
endfunction
