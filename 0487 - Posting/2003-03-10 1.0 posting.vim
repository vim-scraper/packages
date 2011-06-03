" GVim utility script "Posting"

"·
" *posting.txt*  Plugin for preparing USENET/eMail articles
"
" Version    : 1.0
" Author     : Tim Allen <firstlight@redneck.gacracker.org>
" Target     : GVim 6.x
" Last Change: 2003 Mar 09
"
" Posting is derived from a few mappings I once messed around with in
" order to ease preparing USENET/eMail articles with GVim. It now offers
" various facilities related to this, such as line-wrapping, pasting,
" footnote-management, box-quoting, quoting-markup-correction or signa-
" ture-management. To activate, have GVim read this (":so posting.vim"),
" resp. copy this file to your plugin directory, and then type in
" ":Post " followed by the number of charcters per line you want to
" permit. (A typical value for USENET posts would be 72.)
"
" Help to certain topics can be obtained by ":h posting.txt" resp.
" ":h posting<Ctrl-D>" (":ts /posting" when called from help window) to
" obtain an overview. Note: Posting makes use of Vim's inbuilt
" "helptags" routine to generate the tag file information you may later
" on access via Vim's ":help" system. This is done on start-up (strictly
" speaking when this file is source'd for the first time) and normally
" should work fine unless the directory structure in your local
" environment should be in some strange way different to the default.
" If it, despite this, does not (might happen when source'd from a
" non-Vim directory) or you nevertheless should think this might not
" work in your local environment, please take a look at the
" "Settings" section below! You'll find a variable "vim_doc_path"
" there you may use to explicitely point to the directory you expect
" help doc's to be stored.
"
"  Have fun!
"
" PS: To be able to correct bugs more quickly for the future I'd be
" interested in getting some feedback. Please, feel free to send bug
" reports, suggestions or simply questions on how to use this script!
"
" Documentation {{{1
"
"					      *posting-shape*
"
" This is how a typical Posting posting should look like:
"	
"		      | From: ...              | <-- optional
"		      | Newsgroups: ...        |
"	     Header --| Reply-To: ...          |
"		      | Date: ...              |
"		      | Subject: ...           |
"		        <empty line>           |
"		      | <quoted/unquoted text>
"	     Body ----| ~~                     | <-- optional
"		      | <unquoted text>        |
"
" (Any text behind the cutmark delimiter ("~~") will be ignored, thus
" this section may be used to insert annotations you don't want to be
" formated or sent. (use this for instance to manage predefined message
" templates))
"
"					      *posting-sections*
"
" Besides its pasting mechanisms, meant for inserting foreign pieces of
" text from the clipboard, Posting provides a simple macro processor
" which allows you to use hypertext markup to prepare your message. This
" means that you don't have to take care on how your message looks like
" any more when composing. Simply insert the appropriate macro at the
" right place and Posting will do this for you. (see |posting-commands|)
"
" These are the directives Posting currently recognizes:
"
"    a)	Footnote:     |... (FN: <text>#) ...
"
"	(A separate footnote section at the end of the message body
"	will be created showing footnote text arranged in the order it
"	appears in your message. Each item will be referenced by a
"	unique number.)
"
"    b)	Table:	      |...
"		      |(TB:
"		      |<text>
"		      |#)
"		      |...
"
"	(Sections of this type are meant for inserts that may not be
"	formated by Posting, i. e. tables, lists etc.. These passages
"	will appear in their original shape when final formatting is
"	done.)
"
"    c)	Box (Frame):  |...
"		      |(BX: xx
"		      |<title>                 | <-- optional
"		      |-                       |
"		      |<text>
"		      |#)
"		      |...
"
"	(Creates a box surrounding what has been written. The first line
"	in parenthesis when followed by a hyphen ("-") is regarded as
"	the title of the box and thus formatted differently to the main
"	content. Use this for instance to specify references. To prevent
"	boxes from being damaged, what might happen with very big boxes
"	when quoted over and over, I provided a way to set an upper
"	limit to the width of the box. Due to that this is done by
"	reformatting its content however this'll make sense with certain
"	types of text only. Default is to do no reformatting ("NR"). To
"	center the box add suffix '/c' to the value denoting the width
"	of the box.
"
"					      *posting-sigfile*
"
" Posting provides a simple signature import mechanism too, you might
" wish to use to automatically append signatures when composing. To do
" so simply create a file containing a list of signatures you like (each
" item has to be followed by a single number sign character ("#")
" located on a line of its own), and assign its path to "sig_file" (see
" below) so that Posting can access it. Then each time you do "final
" formatting" Posting will insert a new signature from the list into the
" message body. (The number of entries "sig_file" may contain isn't
" limited.)
"
"					      *posting-commands*
"
" To keep the GUI menu simple I didn't enclose all of the functions
" Posting offers. A few of them (the most powerful I'd say) are
" accessible only by typing a certain key combination. This might look a
" bit uncomfortable at first glance, but it isn't! Just try to get used
" to it and you'll find that it can make composing very simple and
" comfortable. (press "u" to undo a command)
"
" ,h			Opens window showing this section. Typing ",h"
"			a second time closes the window again.
"
" ,a			Slots in/out attribution lines.
"
" ,i			Turns on/off syntax highlighting.
"
" ,w			Toggles line wrapping. (let's you show/hide
"			lines longer than allowed) Enabling makes
"			statusline-flag 'WP' show up.
"
" ,v			Turns on/off 'virtualedit'. (Easier to build
"			tables or lists then! (see |virtualedit| for
"			details)) When turned on statusline-flag 'VE'
"			will be visible.
"
" ,p			Helps correcting Plenking.
"
" ,c			Centers visually selected area.
"
" qq			Pastes text from clipboard. (inserts quoting
"			character "|" but preserves shape)
"
" <C-q><C-q>		Like "qq" but reformats text to correct
"			linewidth
"
" nn			Pastes text from clipboard. (text will show up
"			indented, you'll be asked for the shiftwidth to
"			use)
"
" <C-n><C-n>		Like "nn" but text is reformated too to correct
"			linewidth
"
" <M-f>			Inserts footnote section. (text inside will
"			appear in separate footnote section (see
"			|posting-sections|))
"
" <M-t>			Inserts table section. (text inside is left
"			untouched (see |posting-sections|)) Please note
"			that this turns on 'virtualedit' too. (Press
"			<M-v> to turn it off again.)
"
" <M-b>			Inserts box (frame) section. (content will be
"			surrounded by a frame (see |posting-sections|))
"
" <M-r>			Reformats paragraph. (adjusts linewidth and
"			corrects bad quoting markup) This shortcut is
"			available in Insert, Normal and Visual mode.
"
" ll			Extracts text of quoting levels 1 to [count]. If
"			no [count] is given (same as [count] of zero)
"			the paragraph is deleted. Note: When [count] is
"			given and non-zero, "Level" also checks the
"			chronological order, that's to say unordered
"			parts are then removed thereby too even if their
"			quoting level might be within the given range!
"
" <C-l><C-l>		Like "ll", however the paragraph referred to is
"			reformated too after this.
"
" tt			This command works in Visual and Normal mode
"			only. Text selected is taken out and replaced by
"			a mark indicating what happened. You'll be asked
"			for a short comment describing what has been
"			removed from the message. (Each level has its
"			own mark, so, depending on the area you select,
"			you might be prompted several times...) If you
"			think this might not be necessary to keep your
"			message readable simply press "return".
"			(Answering "n" leaves the currently highlighted
"			area unchanged. This can be used to process a
"			whole paragraph showing different quoting levels
"			at a single blow. Type "tt" in normal mode to do
"			so.) Accidentally selected reply text will not
"			be altered. This command applies on quoted text
"			only, quoting markup is left untouched thereby.
"			Note: When [count] is given and non-zero, "Take
"			out" also checks the chronological order, that's
"			to say unordered parts are then removed thereby
"			too even if their quoting level might be within
"			the given range!
"
" <C-t><C-t>		Like "tt", however the paragraph referred to is
"			reformated too after this.
"
" <Space>  		This is to quickly step through most recent
"			reply-text, i. e. level 1 quotings. The cursor
"			stops at the end of the current sentence. Press
"			<S/C-Space> to enter a reply. (see below for
"			details to <S/C-Space> resp. "Split")
"
" <M-Space>		Like <Space> but opposite direction.
"
" <S-Space> 		Splits paragraph to the right of the cursor
"	 		and removes text of quoting levels greater than
"	 		[count]. Part below remains unchanged. Note:
"	 		When [count] is given, "Split" also checks the
"	 		chronological order, that's to say unordered
"	 		parts are then removed thereby too even if
"	 		their quoting level might be within the given
"	 		range!
"
" <C-Space>		Like <S-Space>, however the top of the paragraph
"			(to the current cursor position) is reformated
"			too. (1<C-Space> for instance would do both,
"			remove levels greater than 1 from above, and
"			reformat to adjust linewidth.)
"
" ff			Formats what has been written (reply text
"			including sections (table, box) and footnotes),
"			checks quoting markup and chronological order,
"			removes text elder than that of quoting level
"			[count] and text found after the last
"			non-quoted non-empty line. ("1ff" for instance
"			would remove everything except what has been
"			written and (chron. ordered) text of quoting
"			level 1.) A flag 'FF/x' will show up in the
"			statusline when final-formatting is done,
"			indicating your "verbosity-level". ;-) (i. e.
"			the quoting level of the eldest piece of text
"			found) (Please note that when formatting
"			numbered lists, the number has to be placed in
"			the first column, otherwise it's treated as
"			indented text which is left untouched.)
"
" <C-f><C-f>		Like "ff" however quoted text is reformated too.
"
" <M-LeftMouse>		Use this to mark/unmark attribution lines not
"			yet detected properly. (Please note that marking
"			levels already assigned won't work. You then'd
"			have to unmark first to be able to mark.)
"
" <M-C-LeftMouse>  	Like <M-LeftMouse>, however the line referred to
"			is shortened too. Applying it a second time
"			brings back the line in its original shape.
"			This also applies to lines shortened automa-
"			tically at start-up.
"
" <M-RightMouse>  	Deletes the line referred to.
"
" FF			This shortcut is available only after final
"			formatting is done. It restores the current
"			buffer. (use this for instance if something
"			unwanted happened)
"
" ,s			Like "FF" available only after final formatting
"			is done. Removes annotations (delimited by a
"			cutmark ("~~")), saves buffer to disk and exits.
"
"					      *posting-autocmd*
"
" To automatically call Posting when loading a mail message file add
" this to your vimrc-file: (see also |filetype|)
" 
"		autocmd FileType mail Post 72
"
"					      *posting-window*
"
" When using Vim as a replacement in mail/news applications such as
" Pine, Mutt or Xnews for instance you might find it pleasant to know
" the GUI window always appears in the same place. This can be done by
" using command-line options. (see also |winpos| resp. |-geom|)
"
"		gvim --cmd "winpos X Y" <file>	(Windows) resp.
"		gvim -geometry XxY <file>	(xterm)
"
" Alternatively you may use Posting's "window_position" variables too to
" achieve this. (see "Settings" folder) However, this may cause a
" certain delay to take place on start-up due to that the window is
" drawn prior to processing autocommands.
"
"					      *posting-quoting_style*
"
" Due to that there's no real universally valid standard on how quoting
" has to be done in electronic mail exchange the style as provided by
" Posting has been affected mainly by own notions and my personal
" taste. A few ideas however, the ones I found most valuable, are
" taken from other ressources, such as related RFC's or privately
" maintained sites providing general USENET guidelines.
"
" [1] "Netiquette Guidelines" (Intel Corp., '95) RFC 1855
"     ftp://ftp.demon.co.uk/pub/mirrors/internic/rfc/rfc1855.txt
"
" [2] "The Text/Plain Format Parameter" (R. Gellens, '99) RFC 2646
"     http://www.ietf.org/rfc/rfc2646.txt
"
" [3] Ivan Reid's excellent quoting guideline not just valuable to
"     motorcycle fans...
"     http://www.windfalls.net/ukrm/postinghelp.html
"
" [4] Dirk Nimmich's more general overview (which is not as exhaustive
"     however as far as quoting related aspects are concerned)
"     http://www.netmeister.org/news/learn2quote.html
"
" Settings {{{1
"					      *posting-current_settings*
"
" This section shows user definable variables. You may change the
" default settings to fit Posting to your needs. (Switch-items (1-7)
" may be either 0 or 1; 0 turns off the feature referred to and 1
" turns it on.):
"
let s:check_attribs_on_startup = 1
" When set to 1 attributions consisting of more than a single line of
" text are shortened.
"
let s:rem_sig_on_startup = 0
" When set to 1 sender signatures are removed.
"
let s:syn_high_on_startup = 1
" Set this to 0 if you don't want syntax highlighting. (You still can
" turn it on again afterwards with shortcut ",i".)
"
let s:always_reformat = 0
" This is to adjust the default behaviour of "Take out" and "Split"
" when accessed via the menu. (Does not affect mappings!)
"
let s:classic_quo_style = 1
" Here you can select the character string to use for requoting
" operations. (0 means ">", 1 means "> ")
"
let s:easy_vim_please = 0
" Set this to 1 if you want Vim to behave like a modeless editor. (see
" |insertmode| for details)
"
let s:turn_off_gui_maps = 0
" When set to 1 GUI related mappings are turned off. (You can use this
" to avoid conflicts with Posting's ALT key mappings. (see |winaltkeys|
" for details))
"
let s:window_geometry = 115
" (wind_height / wind_width) * 100 (Courier_New:h14:cANSI)
"
let s:scroll_ratio = 85
" (scroll_area / total_area) * 100 (Decrease to keep the cursor
" centered! Note however that centering is done (more intelligently)
" by Posting itself, so you probably won't need to change this.)
"
"let s:window_position_x = 0
" top left corner of window, pixels from left (see also |winpos|)
"
"let s:window_position_y = 0
" top left corner of window, pixels from top (see also |winpos|)
"
"let s:sig_file = ''
" This is where Posting expects to find the signatures to append.
" (see |posting-sigfile|)
"
"let s:vim_doc_path = ''
" Use this to explicitely point to the directory you expect doc.-files
" to be stored.
"
" History {{{1
"					      *posting-history*
"
" 2003 Mar 09:	- Modified "Format" and "final-format" to recognize
"		  numbered lists (see also comment to shortcut "ff")
"		- Added shortcut ",a" to slot in attribution lines
"		- Added shortcut ",p" to help fixing up "Plenking"
"		  (i. e. space between word and punctuation mark)
"		- Attribution lines: Added shortcuts <M-C-LeftMouse>
"		  (manual reformatting/undo-mechanism) and
"		  <M-RightMouse> (cutting)
"		- "Take out": changed meaning slightly (<C-t><C-t>
"		  reformats whole paragraph only in case that count
"		  is given; tables, lists etc. thus remain untouched),
"		  code partially rewritten (content check, improved
"		  performance)
"		- Added centering support (box-directive "/c" and
"		  shortcut ",c")
"		- Modified code for quoting-markup-check (improved
"		  performance, covered special cases)
"		- Added shortcuts <Space> and <M-Space> to step through
"		  most recent reply text (see also comments to
"		  <S-Space> and <C-Space>)
"
" 2003 Feb 07:	- Added routine to correct/mark broken attribution
"		  lines, i. e. attributions consisting of more than a
"		  single line of text (see also comments to
"		  check_attribs_on_startup (*posting-current_settings*)
"		  and shortcut <M-LeftMouse> (*posting-commands*))
"		- Attribution-lines detected are bundled now in a
"		  separate area on top of the message body. This should
"		  improve readability in case that many different
"		  quoting levels are present.
"		- "Format" (shortcut <M-r>) works in visual mode now
"		  too. You may use it to correct quoting-markup defects
"		  made while composing or with check_quotes_on_startup
"		  disabled.
"		- Fixed bug in quoting markup correction routine
"		  (certain types of defects caused bad quoting) and
"		  added code to fix up other types of defects not
"		  yet covered (e. g. broken attribution-lines)
"		- Other changes: statusline (flag 'FF/x'); changed
"		  shortcuts for "Paste quoted/formated" (<C-q><C-q>)
"		  and "Paste indented/formated" (<C-n><C-n>) so that
"		  pressing 'Ctrl' always means 'reformat'; added new
"		  attribution-line-redundancy-check-routine (;-)
"
" 2003 Jan 18:	- "Take out", "Split", "Format", "Level", "Paste":
"		  Cursor positioned more intelligently now after change
"		  was made. (text is scrolled to ensure cursor appears
"		  at a place suitable for composing)
"		- Added mappings ",h" (help) and ",v" ('virtualedit')
"		- Added statusline functionality with flags 'WP'
"		  (wrapping), 'VE' (virtualedit) and 'FF' (final
"		  formatting)
"		- Settings section: two new items (evim, winaltkeys)
"		- Changed shortcuts for "footnote", "table" and "frame"
"		  inserts (maybe easier to memorize now)
"		- "Box": title/reformat-sit. handled better now
"
" 2003 Jan 12:	- "Take out" works in normal mode now too
"		- Modified "Split" (rewritten, should work better now
"		  (<S-Space>, <S-C-Space>: split is done to the right
"		  of the cursor now))
"	        - Help doc. completed (included mappings from menu)
"		- Syntax highlighting can be turned off now (didn't
"		  work at all in UNIX like environments till now)
"		- Fixed bug in help doc. routine (didn't work properly
"		  in UNIX like environments)
"		- Turned filetype from "dos" into "unix" to avoid
"		  source'ing problems in non-DOS/Windows environments
"		  (Versions prior to this had been tested in
"		  DOS/Windows environments only. It might have been
"		  difficult therefore in the past (maybe even
"		  impossible) to use this plugin within a UNIX-like
"		  environment like Linux or BSD for instance. This
"		  release now should work properly in both "worlds".)
"
" 2003 Jan 04:	- Quoting markup correction: level transitions seamless
"		  now at slightly improved performance (18%), added
"		  semi-automatical routine to correct defects
"		  automatically not recoverable (words teared off and
"		  left on a line of its own (Outlook/Outlook-Express
"		  mail and news output))
"		- Added "Take out" command to ease partial removing of
"		  quoted text (inserts mark to indicate change but
"		  leaves quoting markup untouched (Indeed there's no
"		  need to care for quoting related stuff any more when
"		  replacing quoted text!))
"		- "Level" commands able now to handle optional [count]
"		  parameter (no limit for the level to use any more,
"		  accepts any non-negative number)
"		- "final format": added "chronology-check" (quoted text
"		  is checked whether it is chronological ordered,
"		  malicious parts are removed (There's more behind it as
"		  might seem at first glance! To save bandwidth resp.
"		  keep the message readable you normally had to manually
"		  delete those parts of the message you did not
"		  need/want to answer. This is no longer necessary now
"		  due to that Posting will detect parts like these
"		  automatically now by investigating their chronological
"		  position. You just have to insert text in those places
"		  now you intend to answer and let Posting do the rest.
"		  A clear relief in my opinion compared to the usual
"		  proceeding.))
"		- Added highlighting support (flags long lines)
"
" 2002 Dec 22:	- Window-geometry/scroll-area adjustable now
"		- Modified level commands l1/l2 (able now to handle
"		  reply-leadins)
"		- Modified "split" to extract and split at one go
"		  (quoting levels are taken from optional [count]
"		  parameter)
"		- Added word oriented version of "split"
"		  (press "Shift-Space" instead of just "Space")
"		- Added Ctrl'd versions of "split" and "final format"
"		  to enable optional reformatting
"		- Improved quoting markup correction (able now to
"		  handle nearly every situation)
"		- Cursor pos. restored now after reformatting is done
"		- "Paste indented/formated" more "intelligent" now
"		  (linewidth adjusted automatically)
"		- Annotations are removed now prior to sending
"		- ":help" to various topics available now
"
" 2002 Dec 08:	- Added new hypertext macro "Frame"
"		- Added signature importing mechanism 
"		- Added "split" feature, enabling to quickly insert
"		  text into paragraphs (sentence oriented)
"		- Level commands l1/l2 more "intelligent" now
"		- "Paste indented(/formated)": shift-width adjustable
"		- Quoting markup correction: able to handle initials
"		- Sender signatures are stripped on demand
"		- Posting made configurable
"
" 2002 Nov 28:	- Added reply level select feature
"		- No linewidth formatting on startup any more
"		  to prevent quoted tables from being damaged
"
" 2002 Nov 25:	  Added unit to correct bad quoting markup (special!)
"
" 2002 Nov 22:	- Added macro support
"		- Improved footnote mechanism
"		- Minor changes in pasting/reformatting mechanism
"               
" 2002 Nov 18:	  Added footnote support
"
" 2002 Nov 16:	- Improved pasting/reformatting mechanism
"		- Changed posting menu slightly
"
"  vim:ts=8:so=0:siso=0:ft=help:norl:nowrap:
"·
" Realization {{{1

let g:posting_loaded   = 1
let s:chk_quo_on_strt  = 1
let s:wrap_lin_on_strt = 0
let s:vim_plugin_path  = expand("<sfile>:p:h")
let s:vim_doc_path_def = expand("<sfile>:p:h:h") . '/doc'
let s:valid_headers    = 'Newsgroups:\|From:\|To:\|Cc:\|Bcc:\|Reply-To:\|Subject:\|Return-Path:\|Received:\|Date:\|Replied:'
let s:name             = substitute(substitute('\v%(\u\. )?\u\l{2,}%( x\u\l+x)?%( \u\l?\.)?%( %(\ux)?\u\l+%(\-\u\l+)?)?\m', '\\l', '[a-zäöü]', 'g'), 'x', "'", 'g')
let s:host             = '%(%(\[%(\d{1,3}\.){3})|%(%([a-zA-Z0-9\-]+\.)+))%(\a{2,4}|\d{1,3})\]?'
let s:eml_add          = '\v[< ]@<=[0-9A-Za-z_\-]+%(\.[0-9A-Za-z_\-]+)*\@' . s:host . '\_[> ]@=\m'
let s:verb             = '\%(wrote\|writes\|said\|says\|stated\|states\|typed\|opinion\|schrieb\|schreibt\|geschrieben\)'
let s:rep_led          = '\%(' . s:name . '\|' . s:eml_add . '\|' . s:verb . '\).*[a-z>]\@<=\%(:\|\.\.\.\)\_$'
let s:year             = strftime("%Y")
let s:rep_led_max      = 20
let s:max_quo_lev = 0
if s:classic_quo_style == 0
  let s:tpq_quote = '>'
else
  let s:tpq_quote = '> '
endif
if has('gui')
  let s:gui_sup = 1
endif

function! s:Inst_doc()
  function! s:Make_doc()
    function! s:Form_set()
      function! s:Set()
	. s/^"\?let s:\(\w\+\) *= *\(.*\)$/    \1 = \2/e
	exe 'norm 0R' . s:n . ')'
	let s:n = s:n + 1
      endfunction
      function! s:Com()
	norm jV/^$/-gqV'[:s/^/    /e
      endfunction
      let s:n = 1
      let &tw = 68
      0;/Settings {{
      let top = search('^"\?let s:', 'W')
      exe top . ',/History {{/-1 g/^"\?let/call s:Set()'
      exe top . ',/History {{/-1 g/^\d\+)/call s:Com()'
      let &tw = 0
    endfunction
    norm zR
    exe "norm gg/\<C-K>.M/+V/\<C-K>.M/-\"ay"
    % d
    norm "aPGdd
    % s/^" \?//e
    call s:Form_set()
    % g/{{{\|}}}/d
  endfunction
  sil! unlet s:help_doc
  if !exists("s:vim_doc_path")
    let s:vim_doc_path = s:vim_doc_path_def
  endif
  if !isdirectory(s:vim_plugin_path) || !isdirectory(s:vim_doc_path) || filewritable(s:vim_doc_path) != 2
    return
  endif
  let plugin_file = s:vim_plugin_path . '/posting.vim'
  let doc_file    = s:vim_doc_path . '/posting.txt'
  if bufnr(substitute(plugin_file, '[\/]', '*', 'g')) != -1
    return
  endif
  if filereadable(s:vim_plugin_path . '/.posting.vim.swp')
    return
  endif
  if filereadable(doc_file) && getftime(plugin_file) < getftime(doc_file)
    return
  endif
  if strlen(@%)
    let go_back = 'b ' . bufnr("%")
  else
    let go_back = 'enew'
  endif
  setl nomodeline
  exe 'e ' . plugin_file
  setl modeline
  let buf = bufnr("%")
  setl noswapfile modifiable
  call s:Make_doc()
  exe 'w! ' . doc_file
  let s:help_doc = 1
  exe go_back
  exe 'bw ' . buf
  exe 'helptags ' . s:vim_doc_path
endfunction

function! s:Input(prom, def)
  echohl Question
  let str = input(a:prom . ' [' . a:def . ']? ')
  echohl None
  return substitute(str, '^$', a:def, 'e')
endfunction

function! s:Frame(act)
  if a:act == "save"
    sil! unlet s:header
    sil! unlet s:footer
    exe 'sil! 1,5 g/^\%(' . s:valid_headers . '\)/let s:header = 1'
    if exists("s:header")
      norm ggV/^$"xcdd
    endif
    if search('^\~\~$', "w") > 0
      norm GV?^\~\~$"ycdd
      let s:footer = 1
    endif
  else
    if exists("s:header")
      norm gg"xP
    endif
    if exists("s:footer")
      norm G:+?."yp
    endif
  endif
endfunction

function! s:Top()
  1
  if exists("s:header")
    sil! norm '}/.
  endif
endfunction

function! s:Sep_lines() range
  exe 'sil! ' . a:firstline . ',' . a:lastline . ' g/^\(.\).*\n\1\@!./ put _'
endfunction

function! s:Trim_par_quo()
  function! s:RemLay() range
    if exists("s:tpq_initials")
      exe a:firstline . ',' . a:lastline . ' s/^> \{,2}\%(\a\+ \{,2}>\@=\)\?//e'
    else
      exe a:firstline . ',' . a:lastline . ' s/^> \?//e'
    endif
  endfunction
  function! s:InsLay() range
    exe a:firstline . ',' . a:lastline . ' s/^>\@=/\=s:tpq_quote/e'
    exe a:firstline . ',' . a:lastline . ' s/^>\@!/> /e'
    exe (a:lastline + 1) . ' s/.//'
  endfunction
  let top = line(".")
  sil! +/^·*$/ s/^/·/
  exe 'sil! ' . top . '+;/^·/- call s:RemLay()'
  if exists("s:tpq_manual")
    exe top . '+;/^·/- s/\v%(\_^\>.*)@<=\n%([^>·]%(\w*\W{,2}){,3}\n|%<' . (s:rep_led_max + 1) . 'l%(news:.*|\>@!.*\<.*\@.*))@=/ /ce'
  endif
  exe 'sil! ' . top . '+;/^·/- g/^>.*\n[^>]/ put _'
  exe 'sil! ' . top . '+;/^·/- g/^[^>].*\n>/ put _'
  exe 'sil! ' . top . '+;/^·/- s/\%(\_^>.*\n\)\@<= *\%(\n>\)\@=/>/e'
  exe top
  while search('^[>·]', 'W') > 0
    if getline(".")[0] == '·'
      sil! - v/./d
      break
    endif
    .- call s:Trim_par_quo()
    -
  endwhile
  exe 'sil! ' . top . '+;/^·/- call s:InsLay()'
endfunction

function! s:Bot(top)
  exe a:top
  return s:Min(search('^$\|\%$', 'W'), s:rep_led_max)
endfunction

function! s:Unify(act)
  let mid_pat = '\%(In \| in \| article \| message \| news:\)[^"(,@]*@[^ \t,]*'
  let  id_pat = '1:"\zs[^"]\+\ze"#2:(\zs[^@]*\ze)#3: \@<=' . s:name . '\%( \S\+@\S\+\| ' . s:verb . '\|:\)\@=#4:, \zs' . s:name . '#5:, \zs[^,<]\+\ze <#6:, \zs[^,@]\+\ze ' . s:verb . '#7:^[> ]\+\zs[^<>]\+\ze <#8:^[> ]\+\zs\S\+\ze ' . s:verb . '#'
  function! s:Matchstr(str, pat_list)
    let n = 1
    let pat = matchstr(a:pat_list, '1:\zs.\{-}\ze#')
    while pat != ''
      let res = matchstr(a:str, pat)
      if res != '' | break | endif
      let n = n + 1
      let pat = matchstr(a:pat_list, n . ':\zs.\{-}\ze#')
    endwhile
    return res
  endfunction
  let ln = getline('.') | let lv = s:Level(line('.'))
  if ln =~ '•' || match(@w, lv . ': ') > -1 || strlen(matchstr(ln, '>\zs *\ze[^> ]')) > 1
    return
  endif
  let ln = substitute(ln, '\w-\zs \ze\w\| \zs \+', '', 'g')
  let @W = lv . ': ' . ln . "\n"
  if (strlen(ln) > &tw && a:act == 'check') || a:act == 'force'
    let ln  = substitute(ln, mid_pat, '', '')
    let id  = s:Matchstr(ln, id_pat)
    let add = matchstr(ln, s:eml_add)
    if id != '' || add != ''
      let quo = matchstr(ln, '^\(> \?\)\+')
      if id  != '' | let id = '"' . id . '"' | endif
      if add != '' | let add = '<' . add . '>' | endif
      call setline('.', substitute(quo . ' ' . id . ' ' . add . ' wrote:', ' \{2,}', ' ', 'g'))
    endif
  endif
  . s/[^> ]/• &/e
endfunction

function! s:Deunify()
  let lv = s:Level(line('.'))
  if getline('.') =~ '•' && match(@w, lv . ': ') > -1
    call setline('.', matchstr(@w, lv . ": \\zs[^\n]*\\ze\n"))
    let @w = substitute(@w, lv . ": [^\n]*\n", '', '')
  endif
endfunction

function! s:Trim_par()
  let rep_led_prt = '%( %(article|message)%( .*)?|news:.+|" \<.*|\<\S+\@.*|\@\S+\>.*|\@\S+ \(.*| [1-3]?\d \u\l\l%( .*)?| ' . s:year . '%( .*)?|%(\@|\m' . s:verb . '\v).*%(:|\.{3})|[^\> ]\>.*\m' . s:verb . '\v)\_$'
  sil . g/./?^$?
  let top = line(".")
  sil! /^$/ s/^/·/e
  exe 'sil! ' . top . '+,.- s/^\_[> ]*$/>/e'
  sil! /^·/ s/.*//e
  exe 'sil! ' . top . '+ g/^>[> ]*$/d'
  exe top . 'call s:Trim_par_quo()'
  if top < s:rep_led_max
    if s:tp_attrib > -1
      if s:tp_attrib == 0
        let act = 'mark'
      elseif s:tp_attrib == 1
        let act = 'check'
      else
        let act = 'force'
      endif
      exe 'sil ' . top . '+,' . s:Bot(top) . ' g/\v^.*' . rep_led_prt . '/ s/\v%(\_^\1[^\>].*)@<=\n(%(\> ?)*)%(.*' . rep_led_prt . ')@=/ /e 2 | call s:Unify(act)'
      let s:rep_led = '•.*\_$'
    endif
    exe 'sil ' . top . '+,' . s:Bot(top) . ' g/^.*' . s:rep_led . '\n[> ]*\n.*' . s:rep_led . '/ + d'
  endif
  exe top . ";'}"
endfunction

function! s:Do_init_form()
  if search('^>', 'w') == 0 | return | endif
  sil! call s:Frame("save")
  let col = &tw
  let &tw = 0
  exe 'sil! 1,5 g/^[^>].*' . s:rep_led . '/ if getline(".") !~ "•" | s/\S/• &/e | endif'
  sil! g/^>.*\n[^>]/ put _
  sil! g/^[^>].*\n>/ put _
  if s:rem_sig_on_startup == 1
    sil! 0;/^> \?-- $/,$ g/^>/d
  endif
  let @w = ''
  sil! $ put _ | sil! 1 put! _
  let &tw = col
  while search("^>", "W") > 0
    let top = line(".")
    if s:chk_quo_on_strt == 1
      exe top . 'call s:Trim_par()'
    endif
    if s:wrap_lin_on_strt == 1
      exe top . 'norm gqap'
    endif
    exe top . ";'}"
  endwhile
  1 | let top = search('^>', 'W')
  if top != 0
    exe s:Bot(top)
    if search('^>.*' . s:rep_led, 'bW') > 0
      sil! .+g/[^> ]/norm k$a
    endif
  endif
  sil! unlet s:tpq_initials
  sil! unlet s:tpq_manual
  let s:tp_attrib = -1
  sil! 1;/./- v/./d
  sil! $?.?+,$ v/./d
  sil! %s/^\_[[:space:]]*$//e
  sil! call s:Frame("restore")
endfunction

function! s:Min(x, y)
  if a:x < a:y
    return a:x
  endif
  return a:y
endfunction

function! s:Max(a, b)
  if a:a > a:b
    return a:a
  endif
  return a:b
endfunction

function! s:Columns()
  let n    = 1
  let nmax = line('$')
  let max  = 0
  while n <= nmax
    if strlen(getline(n)) > max
      let max = strlen(getline(n))
    endif
    let n = n + 1
  endwhile
  if max > s:cols
    if max > s:cols + 10 | setl nowrap | endif
    return s:Min(max, s:cols + 10)
  else
    return s:cols
  endif
endfunction

function! s:Paste(quote, form, ...)
  let spaces = '                                                                                '
  let wl = winline()
  if strlen(getline(".")) != 0
    return
  endif
  if a:quote == 'ind'
    let quo_str = strpart(spaces, 0, a:1)
  else
    let quo_str = '| '
  endif
  norm mjO"*pOmi
  sil 'i,'j s/^\([>|] \?\)\+//e
  if a:form == 'wrap' || a:form == 'wrap_dot'
    let dist = strlen(quo_str)
    let col  = &tw
    let &tw  = &tw - dist
    sil 'i,'j s/\v^\s*(.{-})\s*$/\1/e
    if a:form == 'wrap_dot'
      sil 'i/./ s/^./... &/e
      sil 'j?.? s/.$/& .../e
      norm 'igq'j
      let n = 3
      'j?.?
      while getline(".") =~ '^ *\.\.\. *$'
        let &tw = &tw - 1
        norm 'igq'j
        let n = n - 1
        if n == 0
          break
        endif
        'j?.?
      endwhile
    else  " a:form == 'wrap'
      norm 'igq'j
    endif
    let &tw = col
  endif
  'i;/./- d | ma i
  sil 'j- v/./ ?.?+,. d
  sil 'i,'j- s/^/\=quo_str/e
  'j
  call s:Rest_cur(wl)
endfunction

function! s:Level(n)
  let str = getline(a:n)
  let i   = 1
  let pat = '^\%(> \{,2}\)\{1}'
  while match(str, pat) != -1
    let i = i + 1
    let pat = substitute('^\%(> \{,2}\)\{x}', 'x', i, '')
  endwhile
  return i - 1
endfunction

function! s:Lev(n)
  let v:statusmsg = 0
  exe 'sil ' . a:n . ' s/\%(\_^\%(> \{,2}\)*\)\@<=>/&/ge'
  return matchstr(v:statusmsg, '^\d*')
endfunction

function! s:Check_par_chron()
  if getline(".")[0] == '>' && s:chk_quo_on_strt == 1
    let top = line("'{") + 1
    let bot = line("'}") - 1
    let lev = s:Lev(bot)
    let n   = bot - 1
    while n >= top && s:Lev(n) >= lev
      let lev = s:Lev(n)
      let n   = n - 1
      if n <= s:rep_led_max && getline(n) =~ s:rep_led
	break
      endif
    endwhile
    let s:max_quo_lev = s:Max(lev, s:max_quo_lev)
    if n >= top
      if top > s:rep_led_max
	exe top . ',' . n . 'd'
      else
	if n > s:rep_led_max
	  exe (s:rep_led_max + 1) . ',' . n . 'd'
	endif
	" remove everything except rep.-leadins
	exe top . ',' . s:Min(n, s:rep_led_max) . ' v/' . s:rep_led . '/d'
      endif
    endif
    exe top
  endif
endfunction

function! s:Make_par()
  setl nowrapscan
  norm ma/\_^[^>]\|\_^\_$\|\%$
  if getline(".")[0] == '>'
    put _
  else
    .g/./put! _
  endif
  norm `a?\_^[^>]\|\_^\_$\|\%^
  if getline(".")[0] == '>'
    put! _
  else
    .g/./put _
  endif
  norm `a
  setl wrapscan
endfunction

function! s:Ref_par(form, rest)
  function! s:Ref(form, text)
    if a:text == 'q'
      exe '?\%(' . s:rep_led . '\n\|\_^\n\)\@<=\_^>? ma h'
      .- ma l
      if a:form == 'wrap'
        sil 'h,'l s/\v^%(%(\> ?)*)\zs *//e
      endif
      'h,'l call s:Ref_vis(a:form)
    else
      if a:form == 'wrap'
        norm '{jgqip'}
      endif
    endif
  endfunction
  function! s:Cur(act)
    if a:act == 'mark'
      let s:Cur_shift = ''
      if getline('.')[col('$') - 2] == '·'
        let s:Cur_shift = 'l'
      else
        norm h
      endif
      exe "norm \"_x\"as\<C-K>.M"
    else
      exe "norm '{/\<C-K>.M\"_s" . @a . "" . s:Cur_shift
    endif
  endfunction
  let wl = winline()
  if getline(".") =~ '^>\|^·>'
    . s/·//e
    sil! call s:Make_par()
    '} call s:Ref(a:form, "q")
  else
    if a:rest =~ 'f'
      call s:Cur('mark')
      call s:Ref(a:form, 'r')
      call s:Cur('restore')
    else
      . s/·//e
      call s:Ref(a:form, 'r')
    endif
  endif
  if a:rest =~ 'w'
    call s:Rest_cur(wl)
    redraw
  endif
endfunction

function! s:Ref_vis(form) range
  exe 'sil ' . a:firstline . ',' . a:lastline . ' v/^>/let noquo = 1'
  if exists("noquo")
    exe a:firstline
    return
  endif
  exe a:lastline
  sil . g/\n>\@=/ +?[^> ]?
  sil put _
  exe a:firstline
  sil . g/\%(\_^>.*\n\)\@<=./ -/[^> ]/
  sil put! _
  call s:Trim_par()
  if a:form == 'wrap'
    '{ norm gqap
  endif
  sil '{ d
  sil '} d
  call s:Rest_cur(winline())
endfunction

function! s:Quo_lev(n, form, ...)
  function! s:Found_lev(n)
    let pat = substitute('^\%(> \?\)\{1,x}[^>]*$', 'x', a:n, '')
    let top = line(".")
    let n   = line("'}") - 1
    while n >= top
      if getline(n) =~ pat
        return 1
      endif
      let n = n - 1
    endwhile
  endfunction
  function! s:Extr_lev(n)
    sil . g/./norm '{
    let top = line(".") + 1
    let bot = line("'}") - 1
    let s:pat = substitute('^\%(> \?\)\{x,}', 'x', a:n + 1, '')
    if top > s:rep_led_max
      exe 'sil! ' . top . ',' . bot . ' g/' . s:pat . '/d'
    else
      if bot > s:rep_led_max
        exe (s:rep_led_max + 1) . ',' . bot . ' g/' . s:pat . '/d'
      endif
      exe top . ',' . s:Min(bot, s:rep_led_max) . ' g/' . s:pat . '\%(.*' . s:rep_led . '\)\@!/d'
    endif
    exe top + 1
  endfunction
  if a:n > -3 && getline(".")[0] == ">"
    call s:Make_par()
    if a:n == -1
      call s:Check_par_chron()
      let a=1
    elseif a:n == 0
      norm '{jV/^$/./-d
      return
    elseif a:n > 0
      call s:Check_par_chron()
      if exists("a:1") || s:Found_lev(a:n) == 1
        call s:Extr_lev(a:n)
      endif
    endif
    if exists("a:1")
      if a:form == 'wrap'
        sil norm vap:s/\v^%(%(\> ?)*)\zs *//egvgq
      endif
      '}
    else
      call s:Ref_par(a:form, '-')
    endif
  endif
endfunction

function! s:Shrink(n, form)
  let wl = winline()
  call s:Quo_lev(a:n, a:form)
  call s:Rest_cur(wl)
endfunction

function! s:Count(defop)
  if v:count == 0
    if a:defop == 'chron'
      return -1
    elseif a:defop == 'nochron'
      return -2
    else
      return -3
    endif
  endif
  return v:count
endfunction

function! s:Set_cur(wl)
  let d = winline() - a:wl
  if d > 0
    exe 'norm ' . d . ''
  elseif d < 0
    exe 'norm ' . (-d) . ''
  endif
endfunction

function! s:Rest_cur(wl)
  if a:wl < (winheight(0) / 5)
    call s:Set_cur(winheight(0) / 5)
  elseif a:wl < ((winheight(0) * 3) / 4)
    call s:Set_cur(a:wl)
  else
    norm z.
  endif
endfunction

function! s:Split_par(form)
  if s:Level(line('.')) != 1
    return
  endif
  let wl = winline()
  if strpart(getline('.'), 0, col('.')) =~ '^[> ]*$'
    return
  endif
  if line('.') == line('$') && col('.') == strlen(getline('.'))
    norm o
  else
    norm a0
    norm OV/[^> ]/-d
    norm Ok
  endif
  norm mc?.
  if a:form == 'wrap' | let defop = 'nochron' | else | let defop = 'nop' | endif
  call s:Quo_lev(s:Count(defop), a:form)
  norm 'c
  call s:Rest_cur(wl)
  startinsert
endfunction

function! s:Take_out(form, ...) range
  function! s:Set_top(mk)
    exe 'norm `<m' . a:mk
    if v:count > 0
      let max = 0
      let top = s:last
      let lev = s:Lev(top)
      while top > s:first && lev <= v:count && lev >= max
        let max = lev
        let top = top - 1
        let lev = s:Lev(top)
      endwhile
      if lev > v:count || lev < max
        let top = top + 1
      endif
      if top > s:first
	exe top . 'ma ' . a:mk
      endif
    endif
  endfunction
  function! s:Insert_repmark(text)
    function! s:Rep(mk, ch, ...)
      exe 'norm `' . a:mk
      if exists("a:1") | let s = '\zs' | else | let s = '' | endif
      exe '. s/\%' . s:Max(col('.'), strlen(matchstr(getline('.'), '^\%(> \?\)*')) + 1) . 'c.' . s . '/\=a:ch/e'
    endfunction
    norm `<mh
    . g/^[> ]*$/ /[^> ]\|\%$/ ma h
    norm `>ml
    . g/^[> ]*$/ ?[^> ]\|\%^? norm $ml
    if line("'h") > line("'l") | return | endif
    call s:Rep('h', '')
    call s:Rep('l', '', 'right')
    'h,'l s/\zs\_.*\ze/\=a:text/ge
    'h s/\S\ze\|\ze\w/& /ge
    let s:repmark = 1
  endfunction
  function! s:Bot_line(mk)
    exe 'norm `' . a:mk
    let lev = s:Level(line('.'))
    let n = line('.') + 1
    while n <= s:last && s:Lev(n) == lev
      let n = n + 1
    endwhile
    exe 'norm `' . a:mk
    return n - 1
  endfunction
  function! s:Replace()
    sil! norm gvy
    if match(@", "[^>[:space:]\n]") > -1 | let text = 1 | endif
    exe 'sil! norm gv:g/^.*' . s:rep_led . '/ let leadin = 1'
    if exists("text") && !exists("leadin")
      norm gv
      redraw
      let str = s:Input('comment', '...')
      norm v
      if str != 'n'
        sil! call s:Insert_repmark(str)
      endif
    endif
  endfunction
  let wl = winline()
  let fl = line('.')
  if exists("a:1") && a:1 == 'n'
    norm vip
  endif
  let s:first = line("'<")
  let s:last  = line("'>")
  norm `>mb
  call s:Set_top('a')
  let bot = s:Bot_line('a')
  norm v
  while bot < s:last
    exe bot . 'norm $v0jma'
    call s:Replace()
    let s:last = line("'b")
    let bot = s:Bot_line('a')
    norm v
  endwhile 
  norm `bv
  call s:Replace()
  if exists("s:repmark")
    unlet s:repmark
    if v:count == 0
      sil! 1-//+?\%(\%^\|\_^[^>].*\n\|\_^[> ]*\n\)\@<=\_^>?,$+??-/^>.*\%(\n[> ]*\_$\|\n[^>]\|\%$\)/ call s:Ref_vis(a:form)
    else
      exe 'sil! ' . s:first . 'call s:Quo_lev(' . s:Count("nochron") . ', "' . a:form . '")'
    endif
    sil! 1-//,$+?? s/\(\_[^]*\)/[\1]/ge
  else
    exe fl
  endif
  call s:Rest_cur(wl)
endfunction

function! s:Ins_foot_sec()
  norm a(FN: #)h
  startinsert
endfunction

function! s:Ins_tab_sec()
  if strlen(getline('.')) == 0
    let wl = winline()
    norm i(TB:#)kk
    call s:Rest_cur(wl)
    setl virtualedit=insert
    startinsert
  endif
endfunction

function! s:Ins_frame_sec()
  if strlen(getline('.')) == 0
    let wl = winline()
    exe 'norm i(BX: ' . s:Input('width', 'NR') . '#)kk'
    call s:Rest_cur(wl)
    startinsert
  endif
endfunction

function! s:Virt_edit(act)
  if a:act == 'on'
    setl virtualedit=insert
  elseif a:act == 'off'
    setl virtualedit=
  else
    if &virtualedit!=''
      call s:Virt_edit('off')
    else
      call s:Virt_edit('on')
    endif
  endif
endfunction

function! s:Undo_ff()
  %d
  norm "zPGdd
  call s:Top()
  call s:Add_menu()
  sil! unlet s:ff_stat
endfunction

function! s:Send()
  sil norm G:+?^\~\~$VGd
  sil x!
endfunction

function! s:Syn_high(act)
  if a:act == 'on'
    setl syn=mail
    exe s:def_match
  elseif a:act == 'off'
    setl syn=OFF
    match none
  else
    if &syn!='OFF'
      call s:Syn_high('off')
    else
      call s:Syn_high('on')
    endif
  endif
endfunction

function! s:Help(act)
  if a:act == 'on'
    h posting-commands
    let s:help = 1
  elseif a:act == 'off'
    q
    sil! unlet s:help
  else
    if exists("s:help")
      call s:Help('off')
    else
      call s:Help('on')
    endif
  endif
endfunction

function! s:Unplenk()
  norm ma
  let wl = winline()
  %s/\v%(^\>.*)@<=%(\w|\)|\]|'|")@<= {1,2}%([\.:!\?]%( |$))@=//ge
  %s/\v%(^\>.*)@<=%(\w|\)|\]|'|")@<= *\n(\>[\> ]*)([\.:!\?])%( |$)/\2\1/e
  norm `a
  call s:Set_cur(wl)
endfunction

function! s:Att_win(act)
  if a:act == 'on'
    if !exists("s:attwin")
      norm ma
      let wl = winline()
      $ | let top = search(s:rep_led, 'w')
      if top > 0
        1 | let bot = search(s:rep_led, 'bw')
        let s:attwin = bot - top + 2
	setl ls=0
        exe (s:attwin - 1) . 'sp'
        exe top . 'norm z'
        wincmd j
	norm `a
	call s:Set_cur(wl - s:attwin)
      endif
    endif
  elseif a:act == 'off'
    if exists("s:attwin")
      let wl = winline()
      wincmd k
      q
      setl ls=2
      call s:Set_cur(wl + s:attwin)
      unlet s:attwin
    endif
  else
    if exists("s:attwin")
      call s:Att_win('off')
    else
      call s:Att_win('on')
    endif
  endif
endfunction

function! s:Edit(act)
  if getline('.')[0] == '>'
    if getline('.') =~ '•'
      call s:Deunify()
    else
      call s:Unify(a:act)
    endif
  endif
endfunction

function! s:Ce() range
  let n = 0
  sil! '<,'> g/./ if col('$') > n | let n = col('$') | endif
  if n < &tw
    let h=&sw
    let &sw=(&tw - n + 1) / 2 | '<,'>>
    let &sw=h
  endif
endfunction

function! s:Jump(dir)
  if search('\v%(\_^\> [^\>•].*)@<=%(%(\a\a|\)|\.\.|' . "'" . '|")@<=[\.!\?]\s|[\.!\?]$|%([\.!\?][' . "'" . '"\)]?)@<=["\)])', a:dir) == 0
    echohl ErrorMsg
    echo 'no level 1 match'
    echohl None
  endif
endfunction

function! s:Clean()
  unmenu Posting
  unmenu! Posting
  nunmap ,h
  nunmap ,a
  nunmap ,i
  nunmap ,w
  nunmap ,v
  nunmap ,p
  vunmap ,c
  nunmap ll
  nunmap <C-l><C-l>
  vunmap tt
  nunmap tt
  vunmap <C-t><C-t>
  nunmap <C-t><C-t>
  nunmap <Space>
  nunmap <M-Space>
  nunmap <S-Space>
  nunmap <C-Space>
  iunmap <M-r>
  nunmap <M-r>
  vunmap <M-r>
  nunmap qq
  nunmap <C-q><C-q>
  nunmap nn
  nunmap <C-n><C-n>
  iunmap <M-f>
  iunmap <M-t>
  iunmap <M-b>
  nunmap ff
  nunmap <C-f><C-f>
  nunmap FF
  nunmap ,s
  nunmap <M-LeftMouse>
  nunmap <M-C-LeftMouse>
  nunmap <M-RightMouse>
endfunction

function! s:Add_menu(...)
  sil! call s:Clean()
  if !exists("a:1")  " prepare for editing
    if exists("s:gui_sup")
      anoremenu	<silent> &Posting.&Display.&Help\ on/off<Tab>,h		:call <SID>Help('toggle')<Cr>
      anoremenu	<silent> &Posting.&Display.&Attribs\ on/off<Tab>,a	:sil call <SID>Att_win('toggle')<Cr>
      anoremenu	<silent> &Posting.&Display.H&ighlight\ on/off<Tab>,i	:call <SID>Syn_high('toggle')<Cr>
      anoremenu	<silent> &Posting.&Display.&Wrap\ on/off<Tab>,w		:setl invwrap<Cr>
      anoremenu	<silent> &Posting.&Display.&Virtual\ on/off<Tab>,v	:call <SID>Virt_edit('toggle')<Cr>
      menu	Posting.-Sep1- :
      if s:always_reformat == 0
        nnoremenu	<silent> &Posting.&Take\ out<Tab>tt		:call <SID>Take_out('nowrap', 'n')<Cr>
        vnoremenu	<silent> &Posting.&Take\ out<Tab>tt		:call <SID>Take_out('nowrap')<Cr>
        anoremenu	<silent> &Posting.&Split<Tab>Shift-_		:<C-U>sil! call <SID>Split_par('nowrap')<Cr>
      else
        nnoremenu	<silent> &Posting.&Take\ out<Tab>tt		:call <SID>Take_out('wrap', 'n')<Cr>
        vnoremenu	<silent> &Posting.&Take\ out<Tab>tt		:call <SID>Take_out('wrap')<Cr>
        anoremenu	<silent> &Posting.&Split<Tab>Ctrl-_		:<C-U>sil! call <SID>Split_par('wrap')<Cr>
      endif
      inoremenu	<silent> &Posting.&Format<Tab>Alt-r			<C-K>.M<C-O>:sil! call <SID>Ref_par('wrap', 'fw')<Cr>
      nnoremenu	<silent> &Posting.&Format<Tab>Alt-r			i<C-K>.M<C-O>:sil! call <SID>Ref_par('wrap', 'fw')<Cr>
      vnoremenu	<silent> &Posting.&Format<Tab>Alt-r			:call <SID>Ref_vis('wrap')<Cr>
      menu	Posting.-Sep2- :
      anoremenu	<silent> &Posting.&Paste.&quoted<Tab>qq			:sil! call <SID>Paste('quo', 'nowrap')<Cr>
      anoremenu	<silent> &Posting.&Paste.&indented<Tab>nn		:call <SID>Paste('ind', 'nowrap', <SID>Input('indent', 8))<Cr>
      anoremenu	<silent> &Posting.&Insert.&footnote<Tab>Alt-f		:sil! call <SID>Ins_foot_sec()<Cr>
      anoremenu	<silent> &Posting.&Insert.&table<Tab>Alt-t		:sil! call <SID>Ins_tab_sec()<Cr>
      anoremenu	<silent> &Posting.&Insert.&box<Tab>Alt-b		:call <SID>Ins_frame_sec()<Cr>
      menu	Posting.-Sep3- :
      anoremenu	<silent> &Posting.&Ready!<Tab>ff			:<C-U>sil! call <SID>Do_fin_form('nowrap')<Cr>
      tm &Posting.&Display.&Help\ on/off	  To take a look at the command list
      tm &Posting.&Display.&Attribs\ on/off	  Slots in attribution lines
      tm &Posting.&Display.H&ighlight\ on/off	  Turns on/off syntax highlighting
      tm &Posting.&Display.&Wrap\ on/off	  Let's you show/hide longer lines
      tm &Posting.&Display.&Virtual\ on/off	  Turns on/off virtual-editing
      tm &Posting.&Take\ out			  Takes out visually selected area
      tm &Posting.&Split			  Splits paragraph to enable reply
      tm &Posting.&Format			  Adjusts linewidth and corrects bad quoting markup
      tm &Posting.&Paste.&quoted		  Inserts text from clipboard
      tm &Posting.&Paste.&indented		  Inserts text from clipboard
      tm &Posting.&Insert.&footnote		  Will appear in separate footnote section
      tm &Posting.&Insert.&table		  Text inside will be disregarded
      tm &Posting.&Insert.&box			  Content will be surrounded by a frame
      tm &Posting.&Ready!			  Prepares message for sending
    endif
    nn	<unique> <silent> ,h		    :call <SID>Help('toggle')<Cr>
    nn	<unique> <silent> ,a		    :sil call <SID>Att_win('toggle')<Cr>
    nn	<unique> <silent> ,i		    :call <SID>Syn_high('toggle')<Cr>
    nn	<unique> <silent> ,w		    :setl invwrap<Cr>
    nn	<unique> <silent> ,v		    :call <SID>Virt_edit('toggle')<Cr>
    nn	<unique> <silent> ,p		    :sil! call <SID>Unplenk()<Cr>
    vn	<unique> <silent> ,c		    :call <SID>Ce()<Cr>
    nn	<unique> <silent> ll		    :<C-U>sil! call <SID>Shrink(v:count, 'nowrap')<Cr>
    nn	<unique> <silent> <C-l><C-l>	    :<C-U>sil! call <SID>Shrink(v:count, 'wrap')<Cr>
    vn	<unique> <silent> tt		    :call <SID>Take_out('nowrap')<Cr>
    nn	<unique> <silent> tt		    :call <SID>Take_out('nowrap', 'n')<Cr>
    vn	<unique> <silent> <C-t><C-t>	    :call <SID>Take_out('wrap')<Cr>
    nn	<unique> <silent> <C-t><C-t>	    :call <SID>Take_out('wrap', 'n')<Cr>
    nn	<unique> <silent> <Space>	    :call <SID>Jump('w')<Cr>
    nn	<unique> <silent> <M-Space>	    :call <SID>Jump('wb')<Cr>
    nn	<unique> <silent> <S-Space>	    :<C-U>sil! call <SID>Split_par('nowrap')<Cr>
    nn	<unique> <silent> <C-Space>	    :<C-U>sil! call <SID>Split_par('wrap')<Cr>
    ino	<unique> <silent> <M-r>		    <C-K>.M<C-O>:sil! call <SID>Ref_par('wrap', 'fw')<Cr>
    nn	<unique> <silent> <M-r>		    i<C-K>.M<C-O>:sil! call <SID>Ref_par('wrap', 'fw')<Cr>
    vn	<unique> <silent> <M-r>		    :call <SID>Ref_vis('wrap')<Cr>
    nn	<unique> <silent> qq		    :sil! call <SID>Paste('quo', 'nowrap')<Cr>
    nn	<unique> <silent> <C-q><C-q>	    :sil! call <SID>Paste('quo', 'wrap_dot')<Cr>
    nn	<unique> <silent> nn		    :call <SID>Paste('ind', 'nowrap', <SID>Input('indent', 8))<Cr>
    nn	<unique> <silent> <C-n><C-n>	    :call <SID>Paste('ind', 'wrap_dot', <SID>Input('indent', 8))<Cr>
    ino	<unique> <silent> <M-f>		    <C-O>:call <SID>Ins_foot_sec()<Cr>
    ino	<unique> <silent> <M-t>		    <C-O>:call <SID>Ins_tab_sec()<Cr>
    ino	<unique> <silent> <M-b>		    <C-O>:call <SID>Ins_frame_sec()<Cr>
    nn	<unique> <silent> ff		    :<C-U>sil! call <SID>Do_fin_form('nowrap')<Cr>
    nn	<unique> <silent> <C-f><C-f>	    :<C-U>sil! call <SID>Do_fin_form('wrap')<Cr>
    nn	<unique> <silent> <M-LeftMouse>	    <LeftMouse>:sil! call <SID>Edit('mark')<Cr>
    nn	<unique> <silent> <M-C-LeftMouse>   <LeftMouse>:sil! call <SID>Edit('force')<Cr>
    nn	<unique> <silent> <M-RightMouse>    <LeftMouse>dd
  else  " leave editing mode
    if exists("s:gui_sup")
      anoremenu	<silent> &Posting.&Restore<Tab>FF			:sil! call <SID>Undo_ff()<Cr>
      menu	Posting.-Sep1- :
      anoremenu	<silent> &Posting.&Send!<Tab>,s				:call <SID>Send()<Cr>
      tm Posting.&Restore			  Use this to correct typos...
      tm &Posting.&Send!			  Currently "save and exit"
    endif
    nn  <unique> <silent> FF		    :sil! call <SID>Undo_ff()<Cr>
    nn  <unique> <silent> ,s		    :call <SID>Send()<Cr>
  endif
endfunction

function! s:App_foot_sec()
  function! s:App()
    norm v/#)lc
    exe 'norm a[' . s:ind . ']'
    .s/\]\</\] /ge
    'd put "
    norm 0vG$:s/^(FN: *\(\_.*\)#)/\1/gv"*c
    call s:Paste('ind', 'wrap', 5)
    exe "'d norm j0R[" . s:ind . ']'
    $- ma d
    let s:ind = s:ind + 1
  endfunction
  1
  if search('(FN:', 'W') == 0
    sil! unlet s:footnote
    return
  endif
  let s:ind = 1
  let @" = "(TB:\n_________" | $ put "
  $ ma d
  1
  while search('(FN:', 'W') > 0
    let top = line('.') - 1
    call s:App()
    exe top
  endwhile
  $?.? put ='#)'
  let s:footnote = 1
endfunction

function! s:Make_box()
  let lb = '|                                                                                                   '
  let tb = ' ---------------------------------------------------------------------------------------------------'
  function! s:Format()
    let n = matchstr(getline("'a"), '\d\{2}')
    if n != ''
      let col = &tw
      let &tw = s:Max(n - 4, s:len_t + 3)
      norm 'a+V'b-"*c
      call s:Paste('ind', 'wrap_dot', 0)
      d | 'a+ d
      let &tw = col
    endif
  endfunction
  function! s:Down(n)
    if a:n > 0 | exe 'return "' . a:n . 'j"' | endif
    return ''
  endfunction
  ma a | /^#)/ ma b
  let s:len_t = 0
  'a+2
  if getline(".") =~ '^-$'
    d | -
    let title = getline(".")
    let s:len_t = col("$")
    d
  endif
  call s:Format()
  let len_b = 0
  'a+,'b- g/^/ if col('$') > len_b | let len_b = col('$') | endif
  let len   = s:Max(s:len_t + 6, len_b + 2)
  let d_len = (s:Max(s:len_t + 4, len_b) - len_b) / 2
  'a+,'b- s/^/\=strpart(lb, 0, d_len + 2)/
  let h=&ve
  let &ve='all' | exe "norm 'aj0" . len . 'l' . s:Down(line("'b") - line("'a") - 2) . 'r|'
  let &ve=h
  let @" = strpart(tb, 0, len)
  'a put "
  if exists("title")
    exe 'norm 0' . ((len - s:len_t - 2) / 2) . 'lR[ ' . title . ' ]'
  endif
  'b put! "
  'a g:/c: let h=&sw | let &sw=(s:cols - len - 1) / 2 | .+,'b-> | let &sw=h
  call setline("'a", '(TB:')
  'b
endfunction

function! s:App_sig()
  if exists("s:sig_file") && filereadable(s:sig_file)
    exe 'e ' . s:sig_file
    exe 'b! ' . s:sig_file
    0
    let n = 0
    while search('^#$', 'W') > 0 && n < 2
      let n = n + 1
    endwhile
    if n == 0
      bd!
      return
    elseif n == 1 || filewritable(s:sig_file) != 1
      norm ggV/^#$"*y
    else
      norm ggV/^#$"*dG"*p
      w!
    endif
    bw!
    norm G:+?.o-- "*gp
    norm G:+?^#VGd
  endif
endfunction

function! s:Do_fin_form(form)
  function! s:Make_rep_led_sec()
    if v:count == 0
      let s:lev = s:max_quo_lev
    else
      let s:lev = s:Min(v:count, s:max_quo_lev)
    endif
    1 | let top = search('^>', 'W')
    if top != 0
      let pat = substitute('^\%(> \?\)\{x,}', 'x', s:lev, '')
      exe 'sil ' . top . ',' . s:Bot(top) . ' g/' . pat . '.*' . s:rep_led . '/d'
      exe s:Bot(top)
      if search('^>.*' . s:rep_led, 'bW') > 0
	+
	sil .g/[^> ]/ put! _
	let n = s:Min(s:Level(line('.') - 1), s:Level(line('.') + 1))
	exe 'norm D' . n . 'i' . s:tpq_quote . ''
      endif
      exe top . 'g/^>[> ]*$/d'
    endif
  endfunction
  sil call s:Att_win('off')
  setl noshowmode noshowcmd noai
  %y z
  call s:Frame("save")
  1 | let top = search('^\%(\s.*\S\|[^> \t]\)', 'bw')
  if top > 0 && top < line('$') && search('^>', 'bW') > 0
    exe (top + 1) . ',$ d'
  endif
  1 | put! _
  call s:App_foot_sec()
  let col = &tw
  let &tw = 0
  1
  while search("^(BX:", "W") > 0
    call s:Make_box()
  endwhile
  sil g/^(TB:/ norm V/^#):s/^/@/gv:s/^@\%((TB:\|#)\)//
  % s/^ \@=/+/e
  % s/^[^>|@+]\@=/\*/e
  %call s:Sep_lines()
  let &tw = col
  sil g/^\*/ norm vap:s/^\*//egvgq
  let s:max_quo_lev = 0
  1
  while search("^>", "W") > 0
    call s:Quo_lev(s:Count('chron'), a:form, 'skipchecks')
  endwhile
  g/^>\@!.*\n>[> ]*$/+d
  g/^>[> ]*\n>\@!/d
  call s:Make_rep_led_sec()
  1 d
  sil %s/^\_[[:space:]]*$//e
  sil %s/^[+@]//e
  if exists("s:footnote")
    sil %s/\v\n+%(\n_{9}$)@=//e
  endif
  call s:App_sig()
  exe 'sil 1,3 g/^>\@<!.*' . s:rep_led . '\n[> ]*\n.*' . s:rep_led . '/ + d'
  if s:check_attribs_on_startup == 0
    exe 'sil 1,' . s:Min(line('$'), s:rep_led_max) . ' g/•/ s/• \?//e | norm gqq'
  else
    exe 'sil 1,' . s:Min(line('$'), s:rep_led_max) . ' s/• \?//e'
  endif
  sil 1?.?;$ v/./d
  call s:Frame("restore")
  call s:Top()
  call s:Add_menu("undo")
  setl showmode showcmd ai
  let s:ff_stat = 'FF/' . s:lev
endfunction

function! s:Init_post(columns)
  function! OpSet(var, val, flag)
    exe 'if &' . a:var . '=="' . a:val . '" | return "' . a:flag . '" | else | return "" | endif'
  endfunction
  function! VarEx(var, flag)
    if exists(a:var)
      if a:flag == '-'
	exe 'let val = ' . a:var
	return '[' . val . ']'
      else
	return a:flag
      endif
    else
      return ''
    endif
  endfunction
  setl noshowmode noshowcmd
  if exists("s:window_position_x") && exists("s:window_position_y")
    exe 'winpos ' . s:window_position_x . ' ' . s:window_position_y
  endif
  if has('win32')
    setl guifont=Courier_New:h14:cANSI
  endif
  setl history=20
  setl backspace=2
  setl linebreak
  let s:cols=a:columns
  let &lines=(a:columns * s:window_geometry) / 200
  let &so=((100 - s:scroll_ratio) * &lines) / 200
  let &siso=((100 - s:scroll_ratio) * a:columns) / 200
  setl wrap
  let &tw=a:columns
  setl com=n:>,n:|
  setl fo=tcrq1
  setl nojs
  setl cpoptions-=J
  setl whichwrap=b,s,<,>,[,]
  setl lazyredraw
  setl vb t_vb=
  setl mfd=100
  setl magic
  setl report=0
  setl laststatus=2
  setl statusline=%<%f%h%m%r%{OpSet('wrap','1','[WP]')}%{OpSet('ve','insert','[VE]')}%{VarEx('s:ff_stat','-')}%=%l,%c%V\ %P
  setl cmdheight=1
  setl shortmess-=T
  let s:def_match = 'match Search /.\%>' . (s:cols + 1) . 'v/'
  setl langmenu=none
  call s:Add_menu()
  let s:tpq_initials = 1
  let s:tpq_manual = 1
  let s:tp_attrib = s:check_attribs_on_startup
  if s:syn_high_on_startup == 1
    let s:high = 1
    setl syn=mail
    call s:Do_init_form()
    exe s:def_match
  else
    call s:Do_init_form()
  endif
  call s:Top()
  let &co = s:Columns()
  setl fo+=n ai
  setl ts=2
  setl et
  setl ws
  setl ul=2000
  if s:easy_vim_please == 1
    setl insertmode
  endif
  if s:turn_off_gui_maps == 1
    setl winaltkeys=no
  endif
  setl showmode showcmd
endfunction

command! -nargs=1 Post :call s:Init_post(<f-args>)

sil! call s:Inst_doc()

if exists("s:help_doc")
  echo "Posting v1.0: Installed help-documentation."
endif

" }}}1
" vim600:co=72:fo=tcrq2:com=n\:\":so=5:siso=15:nowrap:sts=2:sm:fdm=marker:
