" django_templates.vim - version 0.2 - Macros and menus for django template documents
"
" Copyright 2010 by branislav.sobotka@gmail.com
"  Permission to copy this document or make derivative works is granted
"  without condition.
"
" | Django templates menu:
" |------------------------------|
" | Template filters -->		 |
" |------------------------------|
" | Template tags	             |
" | ...							 |
" | ...							 |
" |------------------------------|
" | Forloop variables -->	     |
" |------------------------------|
" | ...							 |
" | ...							 |
" |								 |
" |------------------------------|
"
" Almost each keyword may be accessed by its short cut (and by the menu if using a gui)
" in each of 3 modes.  For most tags, these rules apply:
"
" Insert Mode:
" Inserts the keyword at the current cursor position and move cursor to place
" when is assumptioned your typing.
"
" Normal Mode:
" Inserts the keyword at the current cursor position.
"
" Visual Mode:
" Wrap selected text into a keyword
" *****************************************************************************

"variable
map <F10> i{{  }}<Esc>2hi
imap <F10> {{  }}<Esc>2hi

"autoescape
map <F8>ae i{% autoescape on/off %}<CR><CR>{% endautoescape %}<Esc>1ki
imap <F8>ae {% autoescape on/off %}<CR><CR>{% endautoescape %}<Esc>1ki
vmap <F8>ae <Esc>`>a<CR>{% endautoescape %}<Esc>`<i{% autoescape on/off %} <CR><Esc>i

"block
map <F8>bl i{% block  %}<CR><CR>{% endblock %}<Esc>1ki
imap <F8>bl {% block  %}<CR><CR>{% endblock %}<Esc>1ki
vmap <F8>bl <Esc>`>a<CR>{% endblock %}<Esc>`<i{% block  %} <CR><Esc>i

"comment
map <F8>com i{% comment %}<CR><CR>{% endcomment %}<Esc>1ki
imap <F8>com {% comment %}<CR><CR>{% endcomment %}<Esc>1ki
vmap <F8>com <Esc>`>a<CR>{% endcomment %}<Esc>`<i{% comment %} <CR><Esc>i

"cycle
map <F8>cy i{% cycle  %}<Esc>2hi
imap <F8>cy {% cycle  %}<Esc>2hi

"extends
map <F8>ex i{% extends  %}<Esc>2hi
imap <F8>ex {% extends  %}<Esc>2hi

"filter
map <F8>fi i{% filter %}<CR><CR>{% endfilter %}<Esc>1ki
imap <F8>fi {% filter %}<CR><CR>{% endfilter %}<Esc>1ki
vmap <F8>fi <Esc>`>a<CR>{% endfilter %}<Esc>`<i{% filter %} <CR><Esc>i

"cycle
map <F8>for i{% for o in some_list %}<CR><CR>{% endfor %}<Esc>1ki
imap <F8>for {% for o in some_list %}<CR><CR>{% endfor %}<Esc>1ki
vmap <F8>for <Esc>`>a<CR>{% endfor %}<Esc>`<i{% for o in some_list %} <CR><Esc>i

"for ... empty
map <F8>fore i{% for o in some_list %}<CR><CR>{% empty %}<CR><CR>{% endfor %}<Esc>3ki
imap <F8>fore {% for o in some_list %}<CR><CR>{% empty %}<CR><CR>{% endfor %}<Esc>3ki

"forloop.counter
map <F8>flc iforloop.counter <Esc>i
imap <F8>flc forloop.counter <Esc>i
 
"forloop.counter0
map <F8>flc0 iforloop.counter0 <Esc>i
imap <F8>flc0 forloop.counter0 <Esc>i
 
"forloop.revcounter
map <F8>flr iforloop.revcounter <Esc>i
imap <F8>flr forloop.revcounter <Esc>i
 
"forloop.revcounter0
map <F8>flr0 iforloop.revcounter0 <Esc>i
imap <F8>flr0 forloop.revcounter0 <Esc>i
 
"forloop.first
map <F8>flf iforloop.first <Esc>i
imap <F8>flf forloop.first <Esc>i
 
"forloop.last
map <F8>fll iforloop.last <Esc>i
imap <F8>fll forloop.last <Esc>i
 
"forloop.parentloop
map <F8>flp iforloop.parentloop <Esc>i
imap <F8>flp forloop.parentloop <Esc>i

"if
map <F8>if i{% if  %}<CR><CR>{% endif %}<Esc>1ki
imap <F8>if {% if  %}<CR><CR>{% endif %}<Esc>1ki
vmap <F8>if <Esc>`>a{% endif %}<Esc>`<i{% if  %}<CR><Esc>i

"if ... else
map <F8>ife i{% if  %}<CR><CR>{% else %}<CR><CR>{% endif %}<Esc>3ki
imap <F8>ife {% if  %}<CR><CR>{% else %}<CR><CR>{% endif %}<Esc>3ki
vmap <F8>ife <Esc>`>a{% else %}<CR><CR>{% endif %}<Esc>`<i{% if  %}<CR><Esc>i

"ifchanged
map <F8>ifc i{% ifchanged  %}<CR><CR>{% endifchanged %}<Esc>1ki
imap <F8>ifc {% ifchanged  %}<CR><CR>{% endifchanged %}<Esc>1ki
vmap <F8>ifc <Esc>`>a{% endifchanged %}<Esc>`<i{% ifchanged  %}<CR><Esc>i

"ifequal
map <F8>ifeq i{% ifequal  %}<CR><CR>{% endifequal %}<Esc>1ki
imap <F8>ifeq {% ifequal  %}<CR><CR>{% endifequal %}<Esc>1ki
vmap <F8>ifeq <Esc>`>a{% endifequal %}<Esc>`<i{% ifequal  %}<CR><Esc>i

"ifnotequal
map <F8>ifne i{% ifnotequal  %}<CR><CR>{% endifnotequal %}<Esc>1ki
imap <F8>ifne {% ifnotequal  %}<CR><CR>{% endifnotequal %}<Esc>1ki
vmap <F8>ifne <Esc>`>a{% endifnotequal %}<Esc>`<i{% ifnotequal  %}<CR><Esc>i

"include
map <F8>in i{% include  %}<Esc>2hi
imap <F8>in {% include  %}<Esc>2hi

"load
map <F8>lo i{% load  %}<Esc>2hi
imap <F8>lo {% load  %}<Esc>2hi

"now
map <F8>in i{% now  %}<Esc>2hi
imap <F8>in {% now  %}<Esc>2hi

"regroup
map <F8>rg i{% regroup  %}<Esc>2hi
imap <F8>rg {% regroup  %}<Esc>2hi

"spaceless
map <F8>sl i{% spaceless  %}<CR><CR>{% endspaceless %}<Esc>1ki
imap <F8>sl {% spaceless  %}<CR><CR>{% endspaceless %}<Esc>1ki
vmap <F8>sl <Esc>`>a{% endspaceless %}<Esc>`<i{% spaceless  %}<CR><Esc>i

"url
map <F8>url i{% url  %}<Esc>2hi
imap <F8>url {% url  %}<Esc>2hi

"with
map <F8>with i{% with  as  %}<CR><CR>{% endwith %}<Esc>1ki
imap <F8>with {% with  as  %}<CR><CR>{% endwith %}<Esc>1ki
vmap <F8>with <Esc>`>a{% endwith %}<Esc>`<i{% with  as  %}<CR><Esc>i

"------------------------------ FILTERS
"add
map <F8>ad iadd <Esc>i
imap <F8>ad add <Esc>i

 
"addslashes
map <F8>as iaddslashes <Esc>i
imap <F8>as addslashes <Esc>i
 
"capfirst
map <F8>cf icapfirst <Esc>i
imap <F8>cf capfirst <Esc>i
 
"center
map <F8>ce icenter <Esc>i
imap <F8>ce center <Esc>i
 
"cut
map <F8>cu icut <Esc>i
imap <F8>cu cut <Esc>i
 
"date
map <F8>da idate <Esc>i
imap <F8>da date <Esc>i

"default
map <F8>de idefault <Esc>i
imap <F8>de default <Esc>i
 
"default_if_none
map <F8>din idefault_if_none <Esc>i
imap <F8>din default_if_none <Esc>i
 
"dictsort
map <F8>ds idictsort <Esc>i
imap <F8>ds dictsort <Esc>i
 
"dictsortreversed
map <F8>dsr idictsortreversed <Esc>i
imap <F8>dsr dictsortreversed <Esc>i
 
"divisibleby
map <F8>di idivisibleby <Esc>i
imap <F8>di divisibleby <Esc>i
 
"escape
map <F8>es iescape <Esc>i
imap <F8>es escape <Esc>i
 
"escapejs
map <F8>ej iescapejs <Esc>i
imap <F8>ej escapejs <Esc>i
 
"filesizeformat
map <F8>fs ifilesizeformat <Esc>i
imap <F8>fs filesizeformat <Esc>i
 
"first
map <F8>fi ifirst <Esc>i
imap <F8>fi first <Esc>i

 
"fix_ampersands
map <F8>fa ifix_ampersands <Esc>i
imap <F8>fa fix_ampersands <Esc>i

 
"floatformat
map <F8>ff ifloatformat <Esc>i
imap <F8>ff floatformat <Esc>i

 
"force_escape
map <F8>fe iforce_escape <Esc>i
imap <F8>fe force_escape <Esc>i

 
"get_digit
map <F8>gd iget_digit <Esc>i
imap <F8>gd get_digit <Esc>i

 
"iriencode
map <F8>ie iiriencode <Esc>i
imap <F8>ie iriencode <Esc>i

 
"join
map <F8>jo ijoin <Esc>i
imap <F8>jo join <Esc>i

 
"last
map <F8>la ilast <Esc>i
imap <F8>la last <Esc>i

 
"length
map <F8>le ilength <Esc>i
imap <F8>le length <Esc>i

 
"length_is
map <F8>li ilength_is <Esc>i
imap <F8>li length_is <Esc>i

 
"linebreaks
map <F8>lb ilinebreaks <Esc>i
imap <F8>lb linebreaks <Esc>i

 
"linebreaksbr
map <F8>lbb ilinebreaksbr <Esc>i
imap <F8>lbb linebreaksbr <Esc>i

 
"linenumbers
map <F8>ln ilinenumbers <Esc>i
imap <F8>ln linenumbers <Esc>i

 
"ljust
map <F8>lj iljust <Esc>i
imap <F8>lj ljust <Esc>i

 
"lower
map <F8>lo ilower <Esc>i
imap <F8>lo lower <Esc>i

 
"make_list
map <F8>ml imake_list <Esc>i
imap <F8>ml make_list <Esc>i

 
"phone2numeric
map <F8>pn iphone2numeric <Esc>i
imap <F8>pn phone2numeric <Esc>i

 
"pluralize
map <F8>pl ipluralize <Esc>i
imap <F8>pl pluralize <Esc>i

 
"pprint
map <F8>pp ipprint <Esc>i
imap <F8>pp pprint <Esc>i

 
"random
map <F8>ra irandom <Esc>i
imap <F8>ra random <Esc>i

 
"removetags
map <F8>rt iremovetags <Esc>i
imap <F8>rt removetags <Esc>i

 
"rjust
map <F8>rj irjust <Esc>i
imap <F8>rj rjust <Esc>i

 
"safe
map <F8>sa isafe <Esc>i
imap <F8>sa safe <Esc>i

 
"safeseq
map <F8>ss isafeseq <Esc>i
imap <F8>ss safeseq <Esc>i

 
"slice
map <F8>sl islice <Esc>i
imap <F8>sl slice <Esc>i

 
"slugify
map <F8>slu islugify <Esc>i
imap <F8>slu slugify <Esc>i

 
"stringformat
map <F8>sf istringformat <Esc>i
imap <F8>sf stringformat <Esc>i

 
"striptags
map <F8>st istriptags <Esc>i
imap <F8>st striptags <Esc>i

 
"time
map <F8>ti itime <Esc>i
imap <F8>ti time <Esc>i

 
"timesince
map <F8>ts itimesince <Esc>i
imap <F8>ts timesince <Esc>i

 
"timeuntil
map <F8>tu itimeuntil <Esc>i
imap <F8>tu timeuntil <Esc>i

 
"title
map <F8>tit ititle <Esc>i
imap <F8>tit title <Esc>i

 
"truncatewords
map <F8>tw itruncatewords <Esc>i
imap <F8>tw truncatewords <Esc>i

 
"truncatewords_html
map <F8>twh itruncatewords_html <Esc>i
imap <F8>twh truncatewords_html <Esc>i

 
"unordered_list
map <F8>ul iunordered_list <Esc>i
imap <F8>ul unordered_list <Esc>i

 
"upper
map <F8>up iupper <Esc>i
imap <F8>up upper <Esc>i

 
"urlencode
map <F8>ue iurlencode <Esc>i
imap <F8>ue urlencode <Esc>i

 
"urlize
map <F8>ur iurlize <Esc>i
imap <F8>ur urlize <Esc>i

 
"urlizetrunc
map <F8>ut iurlizetrunc <Esc>i
imap <F8>ut urlizetrunc <Esc>i

 
"wordcount
map <F8>wc iwordcount <Esc>i
imap <F8>wc wordcount <Esc>i

 
"wordwrap
map <F8>ww iwordwrap <Esc>i
imap <F8>ww wordwrap <Esc>i

 
"yesno
map <F8>yn iyesno <Esc>i
imap <F8>yn yesno <Esc>i


if has("gui")

	nnoremenu Django templates.Filters.Add<Tab><F8>ad iadd <Esc>i	
	inoremenu Django templates.Filters.Add<Tab><F8>ad add <Esc>i


	nnoremenu Django templates.Filters.Addslashes<Tab><F8>as iaddslashes <Esc>i	
	inoremenu Django templates.Filters.Addslashes<Tab><F8>as addslashes <Esc>i


	nnoremenu Django templates.Filters.Capfirst<Tab><F8>cf icapfirst <Esc>i	
	inoremenu Django templates.Filters.Capfirst<Tab><F8>cf capfirst <Esc>i


	nnoremenu Django templates.Filters.Center<Tab><F8>ce icenter <Esc>i	
	inoremenu Django templates.Filters.Center<Tab><F8>ce center <Esc>i


	nnoremenu Django templates.Filters.Cut<Tab><F8>cu icut <Esc>i	
	inoremenu Django templates.Filters.Cut<Tab><F8>cu cut <Esc>i


	nnoremenu Django templates.Filters.Date<Tab><F8>da idate <Esc>i	
	inoremenu Django templates.Filters.Date<Tab><F8>da date <Esc>i


	nnoremenu Django templates.Filters.Default<Tab><F8>de idefault <Esc>i	
	inoremenu Django templates.Filters.Default<Tab><F8>de default <Esc>i


	nnoremenu Django templates.Filters.Default_if_none<Tab><F8>din idefault_if_none <Esc>i	
	inoremenu Django templates.Filters.Default_if_none<Tab><F8>din default_if_none <Esc>i


	nnoremenu Django templates.Filters.Dictsort<Tab><F8>ds idictsort <Esc>i	
	inoremenu Django templates.Filters.Dictsort<Tab><F8>ds dictsort <Esc>i


	nnoremenu Django templates.Filters.Dictsortreversed<Tab><F8>dsr idictsortreversed <Esc>i	
	inoremenu Django templates.Filters.Dictsortreversed<Tab><F8>dsr dictsortreversed <Esc>i


	nnoremenu Django templates.Filters.Divisibleby<Tab><F8>di idivisibleby <Esc>i	
	inoremenu Django templates.Filters.Divisibleby<Tab><F8>di divisibleby <Esc>i


	nnoremenu Django templates.Filters.Escape<Tab><F8>es iescape <Esc>i	
	inoremenu Django templates.Filters.Escape<Tab><F8>es escape <Esc>i


	nnoremenu Django templates.Filters.Escapejs<Tab><F8>ej iescapejs <Esc>i	
	inoremenu Django templates.Filters.Escapejs<Tab><F8>ej escapejs <Esc>i


	nnoremenu Django templates.Filters.Filesizeformat<Tab><F8>fs ifilesizeformat <Esc>i	
	inoremenu Django templates.Filters.Filesizeformat<Tab><F8>fs filesizeformat <Esc>i


	nnoremenu Django templates.Filters.First<Tab><F8>fi ifirst <Esc>i	
	inoremenu Django templates.Filters.First<Tab><F8>fi first <Esc>i


	nnoremenu Django templates.Filters.Fix_ampersands<Tab><F8>fa ifix_ampersands <Esc>i	
	inoremenu Django templates.Filters.Fix_ampersands<Tab><F8>fa fix_ampersands <Esc>i


	nnoremenu Django templates.Filters.Floatformat<Tab><F8>ff ifloatformat <Esc>i	
	inoremenu Django templates.Filters.Floatformat<Tab><F8>ff floatformat <Esc>i


	nnoremenu Django templates.Filters.Force_escape<Tab><F8>fe iforce_escape <Esc>i	
	inoremenu Django templates.Filters.Force_escape<Tab><F8>fe force_escape <Esc>i


	nnoremenu Django templates.Filters.Get_digit<Tab><F8>gd iget_digit <Esc>i	
	inoremenu Django templates.Filters.Get_digit<Tab><F8>gd get_digit <Esc>i


	nnoremenu Django templates.Filters.Iriencode<Tab><F8>ie iiriencode <Esc>i	
	inoremenu Django templates.Filters.Iriencode<Tab><F8>ie iriencode <Esc>i


	nnoremenu Django templates.Filters.Join<Tab><F8>jo ijoin <Esc>i	
	inoremenu Django templates.Filters.Join<Tab><F8>jo join <Esc>i


	nnoremenu Django templates.Filters.Last<Tab><F8>la ilast <Esc>i	
	inoremenu Django templates.Filters.Last<Tab><F8>la last <Esc>i


	nnoremenu Django templates.Filters.Length<Tab><F8>le ilength <Esc>i	
	inoremenu Django templates.Filters.Length<Tab><F8>le length <Esc>i


	nnoremenu Django templates.Filters.Length_is<Tab><F8>le ilength_is <Esc>i	
	inoremenu Django templates.Filters.Length_is<Tab><F8>le length_is <Esc>i


	nnoremenu Django templates.Filters.Linebreaks<Tab><F8>li ilinebreaks <Esc>i	
	inoremenu Django templates.Filters.Linebreaks<Tab><F8>li linebreaks <Esc>i


	nnoremenu Django templates.Filters.Linebreaksbr<Tab><F8>li ilinebreaksbr <Esc>i	
	inoremenu Django templates.Filters.Linebreaksbr<Tab><F8>li linebreaksbr <Esc>i


	nnoremenu Django templates.Filters.Linenumbers<Tab><F8>li ilinenumbers <Esc>i	
	inoremenu Django templates.Filters.Linenumbers<Tab><F8>li linenumbers <Esc>i


	nnoremenu Django templates.Filters.Ljust<Tab><F8>lj iljust <Esc>i	
	inoremenu Django templates.Filters.Ljust<Tab><F8>lj ljust <Esc>i


	nnoremenu Django templates.Filters.Lower<Tab><F8>lo ilower <Esc>i	
	inoremenu Django templates.Filters.Lower<Tab><F8>lo lower <Esc>i


	nnoremenu Django templates.Filters.Make_list<Tab><F8>ma imake_list <Esc>i	
	inoremenu Django templates.Filters.Make_list<Tab><F8>ma make_list <Esc>i


	nnoremenu Django templates.Filters.Phone2numeric<Tab><F8>pn iphone2numeric <Esc>i	
	inoremenu Django templates.Filters.Phone2numeric<Tab><F8>pn phone2numeric <Esc>i


	nnoremenu Django templates.Filters.Pluralize<Tab><F8>pl ipluralize <Esc>i	
	inoremenu Django templates.Filters.Pluralize<Tab><F8>pl pluralize <Esc>i


	nnoremenu Django templates.Filters.Pprint<Tab><F8>pp ipprint <Esc>i	
	inoremenu Django templates.Filters.Pprint<Tab><F8>pp pprint <Esc>i


	nnoremenu Django templates.Filters.Random<Tab><F8>ra irandom <Esc>i	
	inoremenu Django templates.Filters.Random<Tab><F8>ra random <Esc>i


	nnoremenu Django templates.Filters.Removetags<Tab><F8>rt iremovetags <Esc>i	
	inoremenu Django templates.Filters.Removetags<Tab><F8>rt removetags <Esc>i


	nnoremenu Django templates.Filters.Rjust<Tab><F8>rj irjust <Esc>i	
	inoremenu Django templates.Filters.Rjust<Tab><F8>rj rjust <Esc>i


	nnoremenu Django templates.Filters.Safe<Tab><F8>sa isafe <Esc>i	
	inoremenu Django templates.Filters.Safe<Tab><F8>sa safe <Esc>i


	nnoremenu Django templates.Filters.Safeseq<Tab><F8>ss isafeseq <Esc>i	
	inoremenu Django templates.Filters.Safeseq<Tab><F8>ss safeseq <Esc>i


	nnoremenu Django templates.Filters.Slice<Tab><F8>sl islice <Esc>i	
	inoremenu Django templates.Filters.Slice<Tab><F8>sl slice <Esc>i


	nnoremenu Django templates.Filters.Slugify<Tab><F8>slu islugify <Esc>i	
	inoremenu Django templates.Filters.Slugify<Tab><F8>slu slugify <Esc>i


	nnoremenu Django templates.Filters.Stringformat<Tab><F8>sf istringformat <Esc>i	
	inoremenu Django templates.Filters.Stringformat<Tab><F8>sf stringformat <Esc>i


	nnoremenu Django templates.Filters.Striptags<Tab><F8>st istriptags <Esc>i	
	inoremenu Django templates.Filters.Striptags<Tab><F8>st striptags <Esc>i


	nnoremenu Django templates.Filters.Time<Tab><F8>ti itime <Esc>i	
	inoremenu Django templates.Filters.Time<Tab><F8>ti time <Esc>i


	nnoremenu Django templates.Filters.Timesince<Tab><F8>ts itimesince <Esc>i	
	inoremenu Django templates.Filters.Timesince<Tab><F8>ts timesince <Esc>i


	nnoremenu Django templates.Filters.Timeuntil<Tab><F8>tu itimeuntil <Esc>i	
	inoremenu Django templates.Filters.Timeuntil<Tab><F8>tu timeuntil <Esc>i


	nnoremenu Django templates.Filters.Title<Tab><F8>tit ititle <Esc>i	
	inoremenu Django templates.Filters.Title<Tab><F8>tit title <Esc>i


	nnoremenu Django templates.Filters.Truncatewords<Tab><F8>tw itruncatewords <Esc>i	
	inoremenu Django templates.Filters.Truncatewords<Tab><F8>tw truncatewords <Esc>i


	nnoremenu Django templates.Filters.Truncatewords_html<Tab><F8>twh itruncatewords_html <Esc>i	
	inoremenu Django templates.Filters.Truncatewords_html<Tab><F8>twh truncatewords_html <Esc>i


	nnoremenu Django templates.Filters.Unordered_list<Tab><F8>un iunordered_list <Esc>i	
	inoremenu Django templates.Filters.Unordered_list<Tab><F8>un unordered_list <Esc>i


	nnoremenu Django templates.Filters.Upper<Tab><F8>up iupper <Esc>i	
	inoremenu Django templates.Filters.Upper<Tab><F8>up upper <Esc>i


	nnoremenu Django templates.Filters.Urlencode<Tab><F8>ue iurlencode <Esc>i	
	inoremenu Django templates.Filters.Urlencode<Tab><F8>ue urlencode <Esc>i


	nnoremenu Django templates.Filters.Urlize<Tab><F8>ur iurlize <Esc>i	
	inoremenu Django templates.Filters.Urlize<Tab><F8>ur urlize <Esc>i


	nnoremenu Django templates.Filters.Urlizetrunc<Tab><F8>ut iurlizetrunc <Esc>i	
	inoremenu Django templates.Filters.Urlizetrunc<Tab><F8>ut urlizetrunc <Esc>i


	nnoremenu Django templates.Filters.Wordcount<Tab><F8>wc iwordcount <Esc>i	
	inoremenu Django templates.Filters.Wordcount<Tab><F8>wc wordcount <Esc>i


	nnoremenu Django templates.Filters.Wordwrap<Tab><F8>ww iwordwrap <Esc>i	
	inoremenu Django templates.Filters.Wordwrap<Tab><F8>ww wordwrap <Esc>i


	nnoremenu Django templates.Filters.Yesno<Tab><F8>yn iyesno <Esc>i	
	inoremenu Django templates.Filters.Yesno<Tab><F8>yn yesno <Esc>i
	
	:amenu Django templates.-Sep0-	:
	
	nnoremenu Django templates.{{\ }}<Tab><F10> i{{  }}<Esc>2hi
	inoremenu Django templates.{{\ }}<Tab><F10> {{  }}<Esc>2hi

	nnoremenu Django templates.Autoescape<Tab><F8>ae i{% autoescape on/off %}<CR><CR>{% endautoescape %}<Esc>1ki
	inoremenu Django templates.Autoescape<Tab><F8>ae {% autoescape on/off %}<CR><CR>{% endautoescape %}<Esc>1ki

	nnoremenu Django templates.Block<Tab><F8>bl i{% block  %}<CR><CR>{% endblock %}<Esc>1ki
	inoremenu Django templates.Block<Tab><F8>bl {% block  %}<CR><CR>{% endblock %}<Esc>1ki

	nnoremenu Django templates.Comment<Tab><F8>com i{% comment %}<CR><CR>{% endcomment %}<Esc>1ki
	inoremenu Django templates.Comment<Tab><F8>com {% comment %}<CR><CR>{% endcomment %}<Esc>1ki

	nnoremenu Django templates.Cycle<Tab><F8>cy i{% cycle  %}<Esc>2hi
	inoremenu Django templates.Cycle<Tab><F8>cy {% cycle  %}<Esc>2hi

	nnoremenu Django templates.Extends<Tab><F8>ex i{% extends  %}<Esc>2hi
	inoremenu Django templates.Extends<Tab><F8>ex {% extends  %}<Esc>2hi

	nnoremenu Django templates.Filter<Tab><F8>fi i{% filter %}<CR><CR>{% endfilter %}<Esc>1ki
	inoremenu Django templates.Filter<Tab><F8>fi {% filter %}<CR><CR>{% endfilter %}<Esc>1ki

	nnoremenu Django templates.For<Tab><F8>for i{% for o in some_list %}<CR><CR>{% endfor %}<Esc>1ki
	inoremenu Django templates.For<Tab><F8>for {% for o in some_list %}<CR><CR>{% endfor %}<Esc>1ki

	nnoremenu Django templates.For Empty<Tab><F8>fore i{% for o in some_list %}<CR><CR>{% empty %}<CR><CR>{% endfor %}<Esc>3ki
	inoremenu Django templates.For Empty<Tab><F8>fore {% for o in some_list %}<CR><CR>{% empty %}<CR><CR>{% endfor %}<Esc>3ki

	:amenu Django templates.-Sep1-	:

	nnoremenu Django templates.Forloop.counter<Tab><F8>flc iforloop.counter <Esc>i	
	inoremenu Django templates.Forloop.counter<Tab><F8>flc forloop.counter <Esc>i


	nnoremenu Django templates.Forloop.counter0<Tab><F8>flc0 iforloop.counter0 <Esc>i	
	inoremenu Django templates.Forloop.counter0<Tab><F8>flc0 forloop.counter0 <Esc>i


	nnoremenu Django templates.Forloop.revcounter<Tab><F8>flr iforloop.revcounter <Esc>i	
	inoremenu Django templates.Forloop.revcounter<Tab><F8>flr forloop.revcounter <Esc>i


	nnoremenu Django templates.Forloop.revcounter0<Tab><F8>flr0 iforloop.revcounter0 <Esc>i	
	inoremenu Django templates.Forloop.revcounter0<Tab><F8>flr0 forloop.revcounter0 <Esc>i


	nnoremenu Django templates.Forloop.first<Tab><F8>flf iforloop.first <Esc>i	
	inoremenu Django templates.Forloop.first<Tab><F8>flf forloop.first <Esc>i


	nnoremenu Django templates.Forloop.last<Tab><F8>fll iforloop.last <Esc>i	
	inoremenu Django templates.Forloop.last<Tab><F8>fll forloop.last <Esc>i


	nnoremenu Django templates.Forloop.parentloop<Tab><F8>flp iforloop.parentloop <Esc>i	
	inoremenu Django templates.Forloop.parentloop<Tab><F8>flp forloop.parentloop <Esc>i

	:amenu Django templates.-Sep2-	:

	nnoremenu Django templates.If<Tab><F8>if i{% if  %}<CR><CR>{% endif %}<Esc>1ki
	inoremenu Django templates.If<Tab><F8>if {% if  %}<CR><CR>{% endif %}<Esc>1ki
	
	nnoremenu Django templates.If Else<Tab><F8>ife i{% if  %}<CR><CR>{% else %}<CR><CR>{% endif %}<Esc>3ki
	inoremenu Django templates.If Else<Tab><F8>ife {% if  %}<CR><CR>{% else %}<CR><CR>{% endif %}<Esc>3ki

	nnoremenu Django templates.Ifchanged<Tab><F8>ifc i{% ifchanged  %}<CR><CR>{% endif %}<Esc>1ki
	inoremenu Django templates.Ifchanged<Tab><F8>ifc {% ifchanged  %}<CR><CR>{% endif %}<Esc>1ki

	nnoremenu Django templates.Ifequal<Tab><F8>ifeq i{% ifequal  %}<CR><CR>{% endif %}<Esc>1ki
	inoremenu Django templates.Ifequal<Tab><F8>ifeq {% ifequal  %}<CR><CR>{% endif %}<Esc>1ki

	nnoremenu Django templates.Ifnotequal<Tab><F8>ifne i{% ifnotequal  %}<CR><CR>{% endif %}<Esc>1ki
	inoremenu Django templates.Ifnotequal<Tab><F8>ifne {% ifnotequal  %}<CR><CR>{% endif %}<Esc>1ki		

	nnoremenu Django templates.Include<Tab><F8>in i{% include  %}<Esc>2hi
	inoremenu Django templates.Include<Tab><F8>in {% include  %}<Esc>2hi

	nnoremenu Django templates.Load<Tab><F8>lo i{% load  %}<Esc>2hi
	inoremenu Django templates.Load<Tab><F8>lo {% load  %}<Esc>2hi

	nnoremenu Django templates.Now<Tab><F8>now i{% now  %}<Esc>2hi
	inoremenu Django templates.Now<Tab><F8>now {% now  %}<Esc>2hi

	nnoremenu Django templates.Regroup<Tab><F8>rg i{% regroup  %}<Esc>2hi
	inoremenu Django templates.Regroup<Tab><F8>rg {% regroup  %}<Esc>2hi

	nnoremenu Django templates.Spaceless<Tab><F8>sl i{% spaceless  %}<CR><CR>{% endspaceless %}<Esc>1ki
	inoremenu Django templates.Spaceless<Tab><F8>sl {% spaceless  %}<CR><CR>{% endspaceless %}<Esc>1ki

	nnoremenu Django templates.Url<Tab><F8>url i{% url  %}<Esc>2hi
	inoremenu Django templates.Url<Tab><F8>url {% url  %}<Esc>2hi

	nnoremenu Django templates.With<Tab><F8>with i{% with  as  %}<CR><CR>{% endwith %}<Esc>1ki
	inoremenu Django templates.With<Tab><F8>with {% with  as  %}<CR><CR>{% endwith %}<Esc>1ki

endif


