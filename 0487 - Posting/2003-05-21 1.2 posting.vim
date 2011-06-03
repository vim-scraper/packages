" GVim utility script "Posting"

"·
" *posting.txt*  Plugin providing comprehensive USENET/eMail support
"
" Version    : 1.2
" Author     : Tim Allen <firstlight@redneck.gacracker.org>
" Target     : GVim 6.x
" Last Change: 2003 Mai 20
"
" Posting is derived from a few mappings I once messed around with in
" order to ease preparing USENET/eMail articles with GVim. It now offers
" various facilities related to this, such as line-wrapping, pasting,
" footnote-management, box-quoting, quoting-markup-correction or signa-
" ture-management. To activate, have GVim read this (":so posting.vim"),
" resp. copy this file to your plugin directory, and then type in
" ":Post " followed by the number of characters per line you want to
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
"		      |(BX: W:<width> T:<title>
"		      |<text>
"		      |#)
"		      |...
"
"	(Draws a frame around what has been written. You may use
"	optional parameters to define the shape of the frame: <width> (A
"	two digit number denoting the maximum number of characters the
"	frame may span. When found the content will be reformatted so
"	that the width of the box won't exceed this limit. The number
"	may be followed by a suffix '/c' which tells Vim that it should
"	be centered too after this. (Use <C-A>/<C-X> to in-/decrease the
"	number... (see |CTRL-A| resp. |CTRL-X|)), <title> (An optional
"	phrase denoting the title of the box - will appear centered at
"	the top. Use this for instance to specify references.)
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
" ,r			Reformats paragraph. (adjusts linewidth and
"			corrects bad quoting markup) This shortcut is
"			available in Visual and Normal mode. (Please
"			note that applying ",r" to non-quoted text
"			which is not part of a table-section turns off
"			formatting of non-quoted text via "ff". Note
"			also please that <Cr> will have a different
"			meaning in case that format_flowed is set to 1.
"			This is due to that according to RFC 2646
"			paragraphs need to be terminated by a so-called
"			"hard" line break to be formatted properly.
"			(A side effekt of this you might find useful is
"			that text terminated this way (i. e. by pressing
"			<Cr> in Insert mode) always will be treated as
"			an independent part, that's to say the overall
"			structure of what has been written is preserved
"			even if you won't insert empty lines to build
"			separated blocks of text. "Hard" line breaks
"			once entered will persist.))
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
" tt			This command works in Visual and Normal mode.
"			Text selected (whole paragraph in Normal mode)
"			is taken out and replaced by a mark indicating
"			what happened. You'll be asked for a short
"			comment describing what has been removed from
"			the message. (Each level has its own mark, so,
"			depending on the area you select, you might be
"			prompted several times.) If you think this
"			might not be necessary to keep your message
"			readable simply press "return". To prevent a
"			certain region from being taken out at all,
"			answer "n". When [count] is given and greater
"			than one, "Take out" has a slightly different
"			meaning. Instead of asking whether or not to
"			replace, all parts with level greater than or
"			equal to [count] will be removed automatically.
"			This way you can cut off all elder parts at a
"			single blow while still keeping the message's
"			logical structure intact.
"
" <C-t><C-t>		Like "tt", however the region selected will be
"			reformated too after this.
"
" <Space>  		This is to quickly step through most recent
"			reply-text, i. e. level 1 quotings. The cursor
"			will stop at places which might be suitable for
"			to enter a reply. Press <S/C-Space> to do so.
"			(see below for details on <S/C-Space> resp.
"			"Split")
"
" <M-Space>		Like <Space> just opposite direction.
"
" <S-Space> 		Splits paragraph to the right of the cursor
"	 		and removes text of quoting levels greater than
"	 		[count]; part below remains unchanged. Note:
"	 		When [count] is given, "Split" also checks the
"	 		chronological order, that's to say unordered
"	 		parts are then removed thereby too even if
"	 		their quoting level might be within the given
"	 		range!
"
" <C-Space>		Like <S-Space> but additionally turns on Scan
"			mode which allows to quickly reformat through
"			quoted text containing tables, lists and so on,
"			generally speaking indented structures. A few
"			keys have a special meaning then: j and <Space>
"			(step down), k (step up), <Down> (mark down), G
"			(mark to the end), . (reformat down), <C-Space>
"			(reformat to the end), r (reformat visually
"			selected area), t (take out visually selected
"			area), d (delete visually selected area), c
"			(chron.-check to the end), 2,3,4... (take out to
"			the end), <Esc> (leave Scan mode). <C-Space> may
"			be used in conjunction with an optional [count]
"			parameter denoting the maximum level of quoted
"			text one wants to keep.
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
"			have to unmark first to be able to mark again.)
"
" <M-C-LeftMouse>  	Like <M-LeftMouse>, however the line referred to
"			is shortened too. Applying it a second time
"			brings back the line in its original shape.
"			This also applies to lines shortened automa-
"			tically on start-up.
"
" <M-RightMouse>  	Deletes the line referred to.
"
" ,u			Restores buffer. You may use this to undo
"			commands ":Post" or "ff". Please note that all
"			changes made till then will be lost!
"
" ,s			"Save and exit". The meaning of ",s" depends on
"			where it's been invoked from. Prior to final
"			formatting the message is marked as draft to
"			ensure that it won't be formatted again when
"			opened a second time. Annotations (delimited by
"			a cutmark "~~") are left untouched thereby.
"			After final formatting annotations are removed
"			and the message is prepared for sending. Please
"			note that Vim won't terminate when Posting has
"			been called via "Post!"; instead it will be
"			"minimized" resp. "iconised" (depending on the
"			operating system being used).
"
"					      *posting-invocation*
"
" To automatically call Posting when loading a mail message file add
" this to your vimrc-file (see also |filetype|)
" 
"	autocmd FileType mail Post 72
"
" resp. this to your Mailer/News-Agent's config.-file if you want to
" call it from the application being used for mail/news transfer
"
"	gvim "+Post 72"
"
" To further reduce the time spent on start-up you can use Vim's inbuild
" client-server feature which is supported by Posting too now. Putting
" this in your Mailer/News-Agent's config.-file
"
"	gvim --servername MAIL --remote-silent "+Post! 72"
"
" for instance would have Vim start anew only in case that it can't find
" a server which is already running. "Send" (shortcut ",s") will then
" have the meaning of "minimize" resp. "iconise", in any case it won't
" stop an already running server. To stop it again one could add some-
" thing like
"
"	gvim --servername MAIL --remote-send ":qa!<Cr>"
"
" to the local logout/shutdown script for instance. (A disadvantage of
" this method however is that you won't be able to control the usual
" markup check on start-up, since Vim does not accept commands when
" running in server mode.)
"
"					      *posting-window*
"
" When using Vim as a replacement in mail/news applications such as
" Pine, Mutt or Xnews for instance you might find it pleasant to know
" the GUI window always appears in the same place. This can be done
" with certain command-line options. (see also |winpos| resp. |-geom|)
"
"	gvim --cmd "winpos X Y" <file>	(Windows) resp.
"	gvim -geometry XxY <file>	(xterm)
"
" Alternatively you may use Posting's "window_position" variables.
" ("Settings" folder) This may cause however a certain delay to take
" place on start-up due to that the window is drawn prior to processing
" autocommands.
"
"					      *posting-highlighting*
"
" These are the syntax groups Posting makes use of for highlighting. In
" parenthesis you'll find the highlighting groups linked to per default:
"
" postingHeaderKey (PreProc), postingHeader (Type), postingSubject
" (PreProc), postingQuotes (Delimiter), postingQuoted1 (Comment),
" postingQuoted2 (Constant), postingQuoted3 (Identifier),
" postingQuotedLong (Search), postingFootnote (Special), postingTable
" (Special), postingBox (Special), postingSignature(Error),
" postingAnnotations (Todo)
"
" To change the color of quoting level 1 you may either change it
" directly
" 
"		    hi postingQuoted1 guifg=green
"
" or, maybe more convenient, by changing the color of the highlighting
" group it's linked to per default
"
"		    hi Comment guifg=green
"
" The latter can be done also via Vim's :color-command, which affects
" not just a single syntax group, but all syntax groups currently
" declared. (see |:colorscheme| for details)
"
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
let s:easy_vim_please = 0
" Set this to 1 if you want Vim to behave like a modeless editor. (see
" |insertmode| for details)
"
let s:turn_off_gui_maps = 0
" When set to 1 GUI related mappings are turned off. (You may use this
" to avoid conflicts with Posting's ALT key mappings. (see |winaltkeys|
" for details))
"
let s:format_flowed = 1
" Turns on RFC 2646 conformable formatting. (You won't see a big
" difference to the default, at the recipients side however your message
" might be better readable when viewed with appropriate software. (see
" [2] for details on RFC 2646))
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
" 2003 Mai 20:	- Added highlighting support (Vim's own syntax file
"		  mail.vim no longer necessary now - take a look at
"		  *posting-highlighting*...)
"		- Scan mode: added commands 'k' (step up), 'd' (delete
"		  visually selected area), 'c' (chron.-check to the
"		  end), digit (take out to the end) (see also comments
"		  to shortcut <C-Space>), slightly improved stepping
"		  mechanism
"		- "Take out": Added new feature (see description to
"		  shortcut "tt")
"		- Improved attribution handling (attributions may span
"		  over up to 4 lines now and may contain empty lines as
"		  well)
"		- Added shortcuts ",u" to undo ":Post" and ",s" to save
"		  message as draft (see comments)
"		- Modified ",r" (Visual/Normal mode) and "r" (Scan mode)
"		  to take indent from second line instead of first (Use
"		  this to reformat quotations with embedded lists.)
"		- 'autoindent' works with format_flowed set to 1 too now
"		  (Sorry for inconveniences!)
"		- Box section: New shape (see *posting-sections*)
"		- Various minor enhancements/fixups
"
" 2003 Apr 06:	- Added RFC 2646 support (see comment to parameter
"		 'format_flowed' (Settings folder))
"		- Added support for Vim's inbuilt client-server
"		  facility (command "Post!", see also *posting-
"		  invocation*)
"		- Added Scan mode feature to ease reformatting of quoted
"		  text with tables, lists or the like inside (see also
"		  comment to shortcut <C-Space>)
"		- Added shortcut ",r" for manual reformatting (see also
"		  comment under *posting-commands*)
"		- Added shortcuts <Up>, <Down>, <Home>, <End> to
"		  enable navigating with line-wrap turned on
"		  (formatoption "t" turned off now per default, use
"		  line-wrap instead (resp. "Split" which turns it on
"		  automatically)) 
"		- Modified "Paste" to protect indented structures
"		  (tab's are replaced with spaces)
"		- Modified quoting-markup-correction slightly to keep
"		  indented structures readable
"		- Modified "Undo" to make it restore the initial state
"		- "Jump" (shortcut <Space>) more intelligent now
"		  (narrowed raster)
"		- Box sections with new shape now (flowed-friendly)
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
"		  reformats whole paragraphs only in case that count
"		  is given; tables, lists etc. thus remain untouched),
"		  code partially rewritten (content check, improved
"		  performance)
"		- Added centering support (box-directive "/c" and
"		  shortcut ",c")
"		- Modified quoting-markup-routine (improved performance,
"		  covered special cases)
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

let g:posting_loaded	= 1
let s:chk_quo_on_strt	= 1
let s:wrap_lin_on_strt	= 0
let s:classic_quo_style = 0
let s:vim_plugin_path	= expand("<sfile>:p:h")
let s:vim_doc_path_def	= expand("<sfile>:p:h:h") . '/doc'
let s:valid_headers	= 'Newsgroups:\|From:\|To:\|Cc:\|Bcc:\|Reply-To:\|Subject:\|Return-Path:\|Received:\|Date:\|Replied:'
let s:name		= substitute(substitute('\v%(\u\. )?\u\l{2,}%( x\u\l+x)?%( \u\l?\.)?%( %(\ux)?\u\l+%(\-\u\l+)?)?\m', '\\l', '[a-zäöü]', 'g'), 'x', "'", 'g')
let s:host		= '%(%(\[%(\d{1,3}\.){3})|%(%([a-zA-Z0-9-]+\.)+))%(\a{2,4}|\d{1,3})\]?'
let s:email		= '\v[< (]@<=[0-9A-Za-z_-]+%(\.[0-9A-Za-z_-]+)*\@' . s:host . '\_[> )]@=\m'
let s:verb		= '\%(wrote\|writes\|said\|says\|stated\|states\|typed\|opinion\|schrieb\|schreibt\|geschrieben\|scripsit\|écrit\)'
let s:year		= strftime("%Y")
let s:rep_led_max	= 20
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
	exe 'norm! 0R' . s:n . ')'
	let s:n = s:n + 1
      endfunction
      function! s:Com()
	norm! jV/^$/-gqV'[:s/^/    /e
      endfunction
      let s:n = 1
      let &l:tw = 68
      0;/Settings {{
      let top = search('^"\?let s:', 'W')
      exe top . ',/History {{/-1 g/^"\?let/call s:Set()'
      exe top . ',/History {{/-1 g/^\d\+)/call s:Com()'
      let &l:tw = 0
    endfunction
    norm! zR
    exe "norm! gg/\<C-K>.M/+V/\<C-K>.M/-\"ay"
    % d
    norm! "aPGdd
    % s/^" \?//e
    call s:Form_set()
    % g/{{{\|}}}/d
  endfunction
  unlet! s:help_doc
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
    unlet! s:header
    unlet! s:footer
    exe 'sil! 1,5 g/^\%(' . s:valid_headers . '\)/let s:header = 1'
    if exists("s:header")
      norm! ggV/^$"xcdd
    endif
    if search('^\~\~$', "w") > 0
      norm! GV?^\~\~$"ycdd
      let s:footer = 1
    endif
  else
    if exists("s:header")
      norm! gg"xP
    endif
    if exists("s:footer")
      norm! G:+?."yp
    endif
  endif
endfunction

function! s:Top()
  norm! gg
  sil! /^\%(>\+\)\@>•\@!.*\S
endfunction


function! s:Trim_par_quo()
  function! s:RemLay() range
    if exists("s:tpq_initials")
      exe a:firstline . ',' . a:lastline . ' s/^> \?\%(\a* \?>\@=\)\?//e'
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
  if s:tpq_repair == 'man'
    exe top . '+;/^·/- s/\v%(\_^\>.*)@<=\n%([^>·]%(\w*\W{,2}){,3}\n|%<' . (s:rep_led_max + 1) . 'l%(news:.*|\>@!.*\@.*[>(].*))@=/ /ce'
  elseif s:tpq_repair == 'auto'
    exe 'sil! ' . top . '+;/^·/- g/\v^\>.*\n%([^>·]%(\w*\W{,2}){,3}\n|%<' . (s:rep_led_max + 1) . 'l%(news:.*|\>@!.*\@.*[>(].*))/ join'
  endif
  exe 'sil! ' . top . '+;/^·/- g/^>.*\n[^>]/ put _'
  exe 'sil! ' . top . '+;/^·/- g/^[^>].*\n>/ put _'
  exe 'sil! ' . top . '+;/^·/- s/\%(\_^>.*\n\)\@<= *\%(\n>\)\@=/>/e'
  exe top
  while search('^[>·]', 'W') > 0
    if getline('.')[0] == '·'
      sil! - v/./d
      break
    endif
    .- call s:Trim_par_quo()
    -
  endwhile
  exe 'sil! ' . top . '+;/^·/- call s:InsLay()'
endfunction

function! s:ABot(top)
  exe (a:top - 1)
  return search('^>.*\%(\n>\@!\|\%' . s:rep_led_max . 'l\|\%$\)', 'W')
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
  let ln = getline('.') | let lv = s:Lev(line('.'))
  if ln =~ '•' || match(@w, lv . ': ') > -1 || strlen(matchstr(ln, '>\zs *\ze[^> ]')) > 1
    return
  endif
  let ln = substitute(ln, '\w-\zs \ze\w\| \zs \+', '', 'g')
  let @W = lv . ': ' . ln . "\n"
  if (strlen(ln) > &tw && a:act == 'check') || a:act == 'force'
    let ln  = substitute(ln, mid_pat, '', '')
    let id  = s:Matchstr(ln, id_pat)
    let add = matchstr(ln, s:email)
    if id != '' || add != ''
      let quo = matchstr(ln, '^>\+ \?')
      if id  != '' | let id = '"' . id . '"' | endif
      if add != '' | let add = '<' . add . '>' | endif
      call setline('.', substitute(quo . ' ' . id . ' ' . add . ' wrote:', ' \{2,}', ' ', 'g'))
    endif
  else
    call setline('.', ln)
  endif
  s/ *[^> ]\@=/•/e
endfunction

function! s:Deunify()
  let lv = s:Lev(line('.'))
  if getline('.') =~ '•' && match(@w, lv . ': ') > -1
    call setline('.', matchstr(@w, lv . ": \\zs[^\n]*\\ze\n"))
    let @w = substitute(@w, lv . ": [^\n]*\n", '', '')
  endif
endfunction

function! s:Trim_par()
  let s:rep_led_prt = '%( %(article|message)%( [<n].*)?|news:.+|" \<.*|[<(]\S+\@.*|\@\S+[>)].*|\@\S+ .*[("].*| [1-3]?\d \u\l\l%( .*)?| [+-][01]\d00([ ,].*)?| ' . s:year . '%( .*)?|%(\@|\m' . s:verb . '\v).*%(:|\.{3})|[^> ]\>.*\m' . s:verb . '\v)\s*\_$'
  function! s:Join()
    let i = 1
    let prt = '^' . matchstr(getline('.'), '^>*') . '>\@!\%(\s*\_$\|.*\v' . s:rep_led_prt . '\m\)'
    while i < 4 && getline(line('.') + 1) =~ prt
      s/\n>*/ / | let i = i + 1
    endwhile
  endfunction
  sil . g/./?^$?
  let top = line('.')
  sil! /^$/ s/^/·/e
  exe 'sil! ' . top . '+,.- s/^\_[> ]*$/>/e'
  sil! /^·/ s/.*//e
  exe 'sil! ' . top . '+ g/^>[> ]*$/d'
  exe top . 'call s:Trim_par_quo()'
  if top < s:rep_led_max && s:tp_attrib > -1
    if s:tp_attrib == 0
      let act = 'mark'
    elseif s:tp_attrib == 1
      let act = 'check'
    else
      let act = 'force'
    endif
    exe 'sil ' . top . '+,' . s:ABot(top) . ' g/\v^.*' . s:rep_led_prt . '/ call s:Join() | call s:Unify(act)'
  endif
  exe top . ";'}"
endfunction

function! s:Do_init_form()
  let @w = ''
  sil %y v
  if search('\%>' . s:Max((line('$') - 5), 1) . 'l\_^ - Posting v', 'w') > 0
    .-,. d | return
  elseif search('^>', 'w') == 0
    setl wrap | startinsert | return
  elseif search('\%<' . (s:rep_led_max + 1) . 'l•', 'w') > 0
    return
  endif
  sil! call s:Frame('save')
  if s:rem_sig_on_startup == 1
    sil! 0;/^> \?-- $/,$ g/^>/d
  endif
  sil! g/^>.*\n[^>]/ put _
  sil! g/^[^>].*\n>/ put _
  sil! $ put _ | sil! 1 put! _
  while search('^>', 'W') > 0
    let top = line('.')
    if s:chk_quo_on_strt == 1
      exe top . 'call s:Trim_par()'
    endif
    if s:wrap_lin_on_strt == 1
      exe top . 'norm vap,qq'
    endif
    exe top . ";'}"
  endwhile
  sil! 1;/./- v/./d
  sil! $?.?+,$ v/./d
  sil! %s/^\_[[:space:]]*$//e
  exe 'sil! 1,3 g/^>\@!.*' . s:rep_led . '/ s/ *\S\@=/•/e'
  exe 'sil! 1,' . s:Min(line('$'), s:rep_led_max) . ' g/•.*\n[> ]*$/ +d'
  exe 'sil! 1,' . s:Min(line('$'), s:rep_led_max) . ' g/•.*\n[^•]*$/ call s:Sep_rep_led(line("."))'
  sil! call s:Frame('restore')
  setl nowrap
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
  let max_cols = s:cols + 10
  let max = 1 | sil! g/^>\%(.*•\)\@!/ if col('$') > max | let max = col('$') | endif
  let max = max - 1
  if max > s:cols
    return s:Min(max, max_cols)
  else
    return s:cols
  endif
endfunction

function! s:Paste(quote, form, ...)
  let spaces = '                                                                                '
  if col('$') > 1 | return | endif
  if a:quote == 'ind'
    let quo_str = strpart(spaces, 0, a:1)
  else
    let quo_str = '| '
  endif
  ma j | put! =\"\n\" . @* | ma i
  sil 'i,'j s/^\([>|] \?\)\+//e
  if a:form == 'wrap' || a:form == 'wrap_dot'
    let dist = strlen(quo_str)
    let tw0  = &tw
    let &l:tw  = &tw - dist
    sil 'i,'j s/\v^\s*(.{-})\s*$/\1/e
    if a:form == 'wrap_dot'
      sil 'i/./ s/^./... &/e
      sil 'j?.? s/.$/& .../e
      norm! 'igq'j
      let n = 3
      'j?.?
      while getline('.') =~ '^ *\.\.\. *$'
        let &l:tw = &tw - 1
        norm! 'igq'j
        let n = n - 1
        if n == 0
          break
        endif
        'j?.?
      endwhile
    else
      norm! 'igq'j
    endif
    let &l:tw = tw0
  else
    setl et ts=8 | 'i,'j retab | setl ts=2
  endif
  'i;/./- d | ma i
  sil 'j- v/./ ?.?+,. d
  sil 'i,'j- s/^/\=quo_str/e
  'j
endfunction

function! s:Lev(n)
  return strlen(matchstr(getline(a:n), '^>*'))
endfunction

function! s:Down(n)
  if a:n > 0 | exe 'return "' . a:n . 'j"' | endif
  return ''
endfunction

function! s:Shft(n) range
  let sw0=&sw | let &l:sw=a:n
  exe a:firstline . ',' . a:lastline . ' s/ *$//'
  exe a:firstline . ',' . a:lastline . ' >'
  let &l:sw=sw0
endfunction

function! s:GQ(type) range
  function! s:Fmt()
    norm! gq'x
    let n = line('.')
    if n > line("'x")
     'x,.- s/.\zs$/ /e
    endif
    exe (n + 1) . 'ma x'
  endfunction
  setl et
  if a:type == 'txt'
    exe a:firstline . 'ma x'
    exe a:lastline . ' put _ | ma y'
    exe a:firstline . ",'y-" . ' g/\S$/ call s:Fmt()'
    'y s/$/·/
    exe a:firstline . ",'y-" . ' g/^[0-9-].*\n / .,.-/\S$/ call s:Shft(1)'
    exe a:firstline . ",'y-" . ' s/^\%(>\|From \)\@=/ /'
    'y- s/\n.*//
  else
    exe a:firstline . 'norm! V' . s:Down(a:lastline - a:firstline) . 'gq'
    '[,'] s/ *$// | '[,'] s/[^>]\@<=\n>.*[^> ]/ &/
    ']
  endif
endfunction

function! s:Ref_quo(form) range
  let spaces = '                                                                                '
  function! s:QIndent(line)
    return strlen(matchstr(getline(a:line), '^>*\zs *'))
  endfunction
  let top = a:firstline
  let bot = a:lastline
  if a:form == 'nowrap' || a:form == 'rebuild' || a:form == 'rebuild_2'
    exe bot
    sil . g/\n>\@=/ +?[^> ]?
    sil put _
    exe top
    sil . g/\%(\_^>.*\n\)\@<=./ -/[^> ]/
    sil put! _
    call s:Trim_par()
    let bot = line('.') - 2 | sil d
    sil '{ d | let top = line(".")
    exe bot
  endif
  if a:form != 'nowrap'
    let n = bot - top
    if (a:form == 'wrap_2' || a:form == 'rebuild_2') && n > 0
      exe 'sil ' . bot . 'put _'
      let ind = s:QIndent(top + 1)
      exe top . 'norm gqj'
      exe 'sil ' . (top + 1) . 's/^>*\zs */\=strpart(spaces, 0, ind)/e'
      exe (top + 1) . "norm V'}-,qqm'jdd''"
    else
      exe top . 'norm V' . s:Down(n) . ',qq'
    endif
  endif
endfunction

function! s:Scan(top, bot)
  let wrap0=&wrap | let ve0=&ve | let gcr0=&gcr | let so0=&so
  setl nowrap ve=block gcr=v:ver1-blinkon0-Normal so=0
  exe a:bot . 'ma d'
  exe a:top . '-/[^> ]/'
  let top = line('.')
  if top < s:fl_wtop
    call s:Rest_cur(0, 'top')
  else
    exe s:fl_wtop . 'norm! zt'
    exe top
  endif
  norm ,qV
  while line('.') <= line("'d")
    if exists("move") | call s:Rest_cur(winline(), 'top') | endif
    redraw
    let ch = getchar()
    if ch == 114					      " r - reformat
      norm :call s:Ref_quo('wrap_2'),qj,qV
    elseif ch == 116					      " t - take out
      norm! '<V'>V
      call s:Take_out('nowrap', 'v')
      norm ,qj,qV
    elseif ch == 106 || ch == 32			      " j, <Space> - step down
      if line('.') < line("'d")
        norm ,qj,qV
      endif
    elseif ch == 107					      " k - step up
      if line('.') > top
        norm ,qk,qV
      endif
    elseif ch == "\<Down>"				      " <Down> - select next line
      if line('.') < line("'d")
        norm ,qj
      endif
    elseif ch == 71					      " G - mark to the end
      norm! 'dgv
    elseif ch == 46					      " . - reformat to next line
      norm Vj:call s:Ref_quo('wrap'),qV
    elseif ch == "\<C-Space>"				      " <C-Space> - reformat to the end
      norm V'd:call s:Ref_quo('wrap'),qV
      break
    elseif ch == 100					      " d - delete
      norm '<d'>,qV
    elseif ch == 99					      " c - chron.-check
      norm! 
      let fl = line('.') | let wl = winline()
      exe fl . ",'d" . ' call s:Ext_quo(0, "chk")'
      exe s:QTop(fl, 1) . ",'d" . ' call s:Ref_quo("nowrap")'
      call cursor(fl, 1) | call s:Set_cur(wl)
      norm ,qV
    elseif ch >= 50 && ch <= 57				      " 2, 3, ..., 9 - extract
      norm! 
      let fl = line('.') | let wl = winline()
      norm! V'dV
      call s:Take_out('nowrap', 'v', nr2char(ch))
      call cursor(fl, 1) | call s:Set_cur(wl)
      norm ,qV
    elseif ch == 27					      " <Esc> - quit
      break
    endif
    let move = 1
  endwhile
  norm! 0
  let &l:wrap=wrap0 | let &ve=ve0 | let &gcr=gcr0 | let &so=so0
endfunction

function! s:QTop(line, min)
  if a:line <= s:rep_led_max
    exe (s:ABot(a:line) + 1)
  else
    exe (a:line + 1)
  endif
  return search('\%(•.*\n\|\_^>\@!.*\n\|\%^\)\@<=\_^>\|\%' . a:min . 'l\_^', 'bW')
endfunction

function! s:QBot(line)
  exe (a:line - 1)
  return search('^>.*\%(\n>\@!\|\%$\)', 'W')
endfunction

function! s:QCTop(line, min)
  exe (s:QBot(a:line) + 1)
  return search('^\%(\1.*\n\)\@<!\(>\+\)\|\%' . a:min . 'l\_^', 'bW')
endfunction

function! s:QLBot(line)
  exe (a:line - 1)
  return search('^\(>\+\)\@>.*\%(\n\1\@!\|\n\1>\|\%$\)', 'W')
endfunction

function! s:Reformat(mode) range
  if a:mode == 'v'
    let pos = a:firstline
    exe 'sil ' . a:firstline . ',' . a:lastline . ' v/^>/let noquo = 1'
    if exists("noquo")
      let top = a:firstline
      let bot = a:lastline
    else
      let bot = a:lastline
      let top = s:QTop(bot, pos)
    endif
  else
    let pos = line('.')
    sil . v/^>/let noquo = 1
    if exists("noquo")
      norm vip
      let top = line("'<")
      let bot = line("'>")
    else
      let n = line('.')
      let top = s:QTop(n, 1)
      let bot = s:QBot(n)
    endif
  endif
  if top > bot | call cursor(pos, 1) | return | endif
  if exists("noquo")
    setl fo+=t ai
    exe 'sil ' . top . 'norm V' . s:Down(bot - top) . ',qt'
    setl fo-=t
    if searchpair('^(TB:$', '', '^#)$', 'n') == 0
      let s:formatting = 1
    endif
  else
    exe 'sil ' . top . ',' . bot . 'call s:Ref_quo("rebuild_2")'
  endif
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

function! s:Sep_rep_led(line)
  if getline(a:line + 1) =~ '[^> ]' | call append(a:line, '') | endif
  let s1 = matchstr(getline(a:line), '^\%(> \?\)*')
  let s2 = matchstr(getline(a:line + 2), '^\%(> \?\)*')
  call setline(a:line + 1, s:Min(s1, s2))
endfunction

function! s:Ext_quo(max, act) range
  let top = s:QCTop(a:lastline, a:firstline)
  let s:max_quo_lev = s:Max(s:Lev(top), s:max_quo_lev)
  if a:act == 'chk'
    let bot = a:lastline
    let pat = '^>\{1,' . a:max . '}>\@!.*\S'
    while bot >= top
      if getline(bot) =~ pat
        let ok = 1 | break
      endif
      let bot = bot - 1
    endwhile
  endif
  if a:act == 'nochk' || exists("ok")
    exe 'sil ' . a:lastline . 'put _' | ma f
    if top > a:firstline
      exe 'sil ' . a:firstline . ',' . (top - 1) . 'd'
    endif
    if a:max > 0
      exe 'sil ' . a:firstline . ",'f-" . ' g/^>\{' . (a:max + 1) . ',}/d'
    endif
    exe 'sil ' . a:firstline . 'g/\%(\_^>\@!.*\n\)\@<=>[> ]*$/d'
    'f- ma f | sil 'f+ d | 'f
  else
    exe a:firstline
  endif
endfunction

function! s:Quo_lev(n, form, act)
  if a:n > -3 && getline('.')[0] == '>'
    let n = line('.')
    let top = s:QTop(n, 1)
    if s:QBot(n) == line('$') | put _ | let nl = 1 | else | + | endif
    ma e
    if a:n == -1					  " chron.-check
      exe top . ",'e-" . ' call s:Ext_quo(0, "nochk")'
    elseif a:n == 0					  " delete par.
      exe 'sil ' . top . ",'e- d"
      return
    elseif a:n > 0					  " extract
      exe top . ",'e-" . ' call s:Ext_quo(a:n, a:act)'
      if a:act == 'chk' && getline(top - 1) =~ '^>.*•'
        call s:Sep_rep_led(top - 1)
      endif
    endif
    if a:act == 'chk' || (a:act == 'nochk' && a:form == 'wrap')
      if a:form == 'wrapman'
	call s:Scan(top, line("'e") - 1)
      else
        exe top . ",'e-" . ' call s:Ref_quo(a:form)'
      endif
    endif
    'e
    if exists("nl") | sil d | endif
  endif
endfunction

function! s:Shrink(n, form)
  let wl = winline()
  call s:Quo_lev(a:n, a:form, 'chk')
  call s:Rest_cur(wl)
endfunction

function! s:Set_cur(wl)
  let d = winline() - a:wl
  if d > 0
    exe 'norm! ' . d . ''
  elseif d < 0
    exe 'norm! ' . (-d) . ''
  endif
endfunction

function! s:Rest_cur(wl, ...)
  if &so > 0
    let top = s:Min(&so + 1, winheight(0) / 5)
  else
    let top = winheight(0) / 10
  endif
  if a:wl < top
    call s:Set_cur(top)
  elseif a:wl < winheight(0) - top - 3
    call s:Set_cur(a:wl)
  else
    if exists("a:1")
      call s:Set_cur(top)
    else
      norm! z.
    endif
  endif
endfunction

function! s:Split_par(form)
  if s:Lev(line('.')) != 1 || strpart(getline('.'), 0, col('.')) =~ '^[> ]*$'
    return
  endif
  let s:fl_wtop = line('.') - winline() + 1
  let so0=&so | setl so=0
  if line('.') == line('$') && strpart(getline('.'), col('.')) =~ '^\s*$'
    norm! o
  else
    sil! norm! aOV/[^> ]\|\%$/-dOk
  endif
  ma c
  sil ?.? s/ *$//e
  let smd0=&smd | let sc0=&sc | setl nosmd nosc
  if a:form == 'nowrap' | let defop = 'nop' | else | let defop = 'nochron' | endif
  call s:Quo_lev(s:Count(defop), a:form, 'chk')
  'c call s:Rest_cur(winline())
  exe 'setl wrap smd sc so=' . so0
  startinsert
endfunction

function! s:Take_out(form, mode, ...) range
  function! s:Mark(text)
    let qcol = strlen(matchstr(getline("'<"), '^>* \?')) + 1
    exe "'<" . ' s/\%' . s:Max(col("'<"), qcol) . 'c.//e'
    exe "'>" . ' s/\%' . s:Max(col("'>"), qcol) . 'c.\zs//e'
    '<,'> s/\zs\_.*\ze/\=a:text/ge
    '< s/\S\ze\|\ze\w/& /ge
    let s:mark = 1
  endfunction
  function! s:Replace()
    sil 'h g/^[> ]*$/ /[^> ]\|\%$/ ma h
    sil 'l g/^[> ]*$/ ?[^> ]\|\%^? norm! $ml
    if line("'h") > line("'l") | return | endif
    sil norm! `hv`ly
    if match(@", "[^>[:space:]\n]") > -1 | let text = 1 | endif
    sil '<,'> g/•/ let leadin = 1
    if exists("text") && !exists("leadin")
      if exists("s:min_lev")
	if s:Lev(line("'<")) >= s:min_lev | sil! call s:Mark('...') | endif
      else
	norm! gvo
      	redraw
      	let str = s:Input('comment', '...')
      	norm! v
      	if str != 'n' | sil! call s:Mark(str) | endif
      endif
    endif
  endfunction
  let smd0=&smd | let &smd=0
  let sc0=&sc | let &sc=0
  if a:mode == 'n'
    let fl = line('.')
    if getline(fl)[0] != '>' | return | endif
    exe s:QTop(fl, 1) . 'ma a'
    exe s:QBot(fl) . 'norm! $mb'
  else
    let fl = line("'<")
    sil '<,'> v/^>/let noquo = 1
    if exists("noquo") | exe fl | return | endif
    norm! `<ma`>mb
  endif
  unlet! s:min_lev
  if exists("a:1") && a:1 > 1 | let s:min_lev = a:1 | endif
  let bot = s:QLBot(line("'a"))
  norm! `amh
  while bot < line("'b")
    exe bot . 'norm! $ml0jma'
    call s:Replace()
    let bot = s:QLBot(line("'a"))
    norm! 'amh
  endwhile 
  norm! `bml
  call s:Replace()
  if exists("s:mark")
    unlet s:mark
    sil! 1-//+?\%(\%^\|\_^[^>].*\n\|\_^[> ]*\n\)\@<=\_^>?,$+??-/^>.*\%(\n[> ]*\_$\|\n[^>]\|\%$\)/ call s:Ref_quo(a:form)
    sil! 1-//,$+?? s/\(\_[^]*\)/[\1]/ge
  else
    exe fl
  endif
  let &smd=smd0 | let &sc=sc0
endfunction

function! s:Ins_foot_sec()
  if getline('.')[col('$') - 2] == '·'
    norm! $xa(FN: #)h
  else
    norm! hxi(FN: #)h
  endif
endfunction

function! s:Ins_tab_sec()
  if col('$') == 1
    let wl = winline()
    norm! i(TB:#)kk
    call s:Rest_cur(wl)
    setl virtualedit=insert
    startinsert
  endif
endfunction

function! s:Ins_frame_sec()
  if col('$') == 1
    let wl = winline()
    exe 'norm! i(BX: W:' . s:Input('width', '-') . ' T:#)2k$'
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

function! s:Undo(com)
  if a:com == 'ff'
    sil! norm! :%d"zpkdd
    call s:Add_menu()
    unlet! s:ff_stat
    match none
    call s:Syn_high(s:cusy0)
    setl fo-=t
    let &l:wrap=s:wrap0
    call cursor(s:fl0, s:fc0)
    call s:Set_cur(s:wl0)
  else
    sil! norm! :%d"vpkdd
    match none
    syn clear
    sil! call s:Clean()
    setl all&
  endif
endfunction

function! s:Send(...)
  if exists("a:1")
    $ put =\"\n\" . ' - Posting v1.2/' . strftime(\"%c\") . ' -'
  else
    if search('^\~\~$', 'w') > 0
      .,$ d
    endif
  endif
  if exists("s:server")
    w
    bw
    unlet s:ff_stat
    stop
  else
    x!
  endif
endfunction

function! s:Syn_high(act)
  function! s:Var(grp, dif)
    exe 'redir @a | sil hi ' . a:grp . ' | redir END'
    while @a =~ 'links to'
      exe 'redir @a | sil hi ' . matchstr(@a, 'links to \zs.*') . ' | redir END'
    endwhile
    return substitute(matchstr(@a, 'xxx \zs.*'), matchstr(a:dif, '.*=\@=') . '=\S*\|$', ' ' . a:dif, '')
  endfunction
  function! s:Color()
    let id  = '<\?[^@<> \t]\+@[0-9A-Za-z-.]\+>\?'
    let url = '<\?\%(http\|https\|ftp\):\/\/[0-9A-Za-z-.]\+\.\a\{2,4}>\?\%(\/\S*\)\?'
    syn region	postingHeader		start='^From ' skip='^[ \t]' end='^[-A-Za-z0-9/]*[^-A-Za-z0-9/:]'me=s-1 end='^[^:]*$'me=s-1 end='^---*' contains=postingHeaderKey,postingSubject
    syn case ignore
    syn region	postingHeader		start='^\%(Newsgroups:\|From:\|To:\|Cc:\|Bcc:\|Reply-To:\|Subject:\|Return-Path:\|Received:\|Date:\|Replied:\)' skip='^[ \t]' end='^[-a-z0-9/]*[^-a-z0-9/:]'me=s-1 end='^[^:]*$'me=s-1 end='^---*' contains=postingHeaderKey,postingSubject
    syn region	postingHeaderKey	start='^\%(Newsgroups\|From\|To\|Cc\|Bcc\|Reply-To\|Subject\|Return-Path\|Received\|Date\|Replied\).*' skip=',$' end='$' contains=postingId contained
    syn match	postingHeaderKey	'^Date' contained
    syn match	postingSubject		'^Subject.*' contained
    exe 'syn match postingId		"' . id . '" contained'
    syn region	postingFootnote		start='(FN:' end='#)'
    syn region	postingTable		start='^(TB:$' end='^#)$'
    syn region	postingBox		start='^(BX:' end='^#)$' contains=postingBoxHeader
    syn region	postingBoxHeader	start='^(BX:.*' end='$' oneline contains=postingBoxWidth,postingBoxTitle contained
    syn match	postingBoxWidth		'W:\zs\S*' contained
    syn match	postingBoxTitle		'T:\zs.*' contained
    syn region	postingSignature	start='^-- $' end='^.*\%(\n\~\~$\)\@=' end='\%$'
    syn region	postingAnnotations	start='^\~\~$' end='\%$'
    syn match	postingQuotedAny	'^[> |].*$' contains=postingQuoted1,postingQuoted2,postingQuoted3,postingQuotedLong,postingQuotedEmpty1,postingQuotedEmpty2,postingQuotedEmpty3
    syn match	postingQuoted1		'^>\{1}>\@!.*$' contains=postingQuotedLong,postingAttrib2,postingId1,postingURL1,postingQuotes contained
    syn match	postingQuoted2		'^>\{2}>\@!.*$' contains=postingQuotedLong,postingAttrib3,postingId2,postingURL2,postingQuotes contained
    syn match	postingQuoted3		'^>\{3,}.*$' contains=postingQuotedLong,postingAttrib3,postingId3,postingURL3,postingQuotes contained
    syn match	postingQuotes		'^>\+' contained
    exe 'syn match postingId1		"' . id . '" contained'
    exe 'syn match postingId2		"' . id . '" contained'
    exe 'syn match postingId3		"' . id . '" contained'
    exe 'syn match postingURL1		"' . url . '" contained'
    exe 'syn match postingURL2		"' . url . '" contained'
    exe 'syn match postingURL3		"' . url . '" contained'
    exe 'syn match postingAttrib1	"\%<' . (s:rep_led_max + 1) . 'l\zs•.*$" contains=postingAttribMark'
    exe 'syn match postingAttrib2	"\%<' . (s:rep_led_max + 1) . 'l\zs•.*$" contains=postingAttribMark contained'
    exe 'syn match postingAttrib3	"\%<' . (s:rep_led_max + 1) . 'l\zs•.*$" contains=postingAttribMark contained'
    syn match	postingAttribMark	'•' contained
    syn match	postingQuotedEmpty1	'\%(\_^\1>.*\n\)\@<=\_^\(>\+\) *$' contained
    syn match	postingQuotedEmpty2	'^\(>\+\) *\_$\%(\n\1>\)\@=' contained
    exe 'syn match postingQuotedEmpty3	"\%(•.*\n\)\@<=\%<' . (s:rep_led_max + 1) . 'l\_^>\+ *$" contained'
    exe 'syn match postingQuotedLong	".\%>' . s:vcols1 . 'v" contained'
    syn sync minlines=50
    hi def link postingHeaderKey	PreProc
    hi def link postingHeader		Type
    exe 'hi postingId ' . s:Var('postingHeaderKey', 'gui=italic')
    hi def link postingSubject		PreProc
    hi def link postingQuotes		Delimiter
    hi def link postingQuoted1		Comment
    hi def link postingQuoted2		Constant
    hi def link postingQuoted3		Identifier
    exe 'hi postingId1 ' . s:Var('postingQuoted1', 'gui=italic')
    exe 'hi postingId2 ' . s:Var('postingQuoted2', 'gui=italic')
    exe 'hi postingId3 ' . s:Var('postingQuoted3', 'gui=italic')
    hi link postingURL1			postingId1
    hi link postingURL2			postingId2
    hi link postingURL3			postingId3
    exe 'hi postingAttrib1 ' . s:Var('postingQuoted1', 'gui=underline')
    exe 'hi postingAttrib2 ' . s:Var('postingQuoted2', 'gui=underline')
    exe 'hi postingAttrib3 ' . s:Var('postingQuoted3', 'gui=underline')
    hi link postingAttribMark		Ignore
    hi def link postingQuotedLong	Search
    hi link postingQuotedEmpty1		Ignore
    hi link postingQuotedEmpty2		Ignore
    hi link postingQuotedEmpty3		Ignore
    hi def link postingFootnote		Special
    hi def link postingTable		Special
    hi def link postingBox		Special
    hi link postingBoxHeader		postingBox
    exe 'hi postingBoxWidth ' . s:Var('postingBox', 'gui=italic')
    hi link postingBoxTitle		postingBoxWidth
    hi def link postingSignature	Error
    hi def link postingAnnotations	Todo
  endfunction
  function! s:Mono()
    exe 'syn region postingAttrib	start="\%<' . (s:rep_led_max + 1) . 'l•" end="$" oneline contains=postingAttribMark'
    syn match	postingAttribMark	'•' contained
    syn sync maxlines=5
    hi link postingAttrib		Underlined
    hi link postingAttribMark		Ignore
  endfunction
  if a:act == 'on'
    if !exists("b:current_syntax") || b:current_syntax != 'posting_color'
      syn clear
      call s:Color()
      let b:current_syntax = "posting_color"
    endif
  elseif a:act == 'off'
    if !exists("b:current_syntax") || b:current_syntax != 'posting_mono'
      syn clear
      call s:Mono()
      let b:current_syntax = "posting_mono"
    endif
  else
    if exists("b:current_syntax") && b:current_syntax != 'posting_mono'
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
    unlet! s:help
  else
    if exists("s:help")
      call s:Help('off')
    else
      call s:Help('on')
    endif
  endif
endfunction

function! s:Unplenk()
  norm! ma
  let wl = winline()
  %s/\v%(^\>.*)@<=%(\w|\)|\]|'|")@<= {1,2}%([.:!?]%( |$))@=//ge
  %s/\v%(^\>.*)@<=%(\w|\)|\]|'|")@<= *\n(\>[> ]*)([.:!?])%( |$)/\2\1/e
  norm! `a
  call s:Set_cur(wl)
endfunction

function! s:Att_win(act)
  if a:act == 'on'
    if !exists("s:attwin")
      norm! ma
      let wl = winline()
      $ | let top = search('•', 'w')
      if top > 0
        1 | let bot = search('•', 'bw')
        let s:attwin = bot - top + 2
	setl ls=0
        exe (s:attwin - 1) . 'sp'
        exe top . 'norm! z'
        wincmd j
	norm! `a
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
      unlet! s:attwin
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
    '<,'> call s:Shft((&tw - n + 1) / 2)
  endif
endfunction

function! s:Jump(dir)
  if search('\v%(\_^\> [^>|•].*)@<=%(%(\a\a|\)|\.\.|' . "'" . '|")@<=[.!?]\s|[.!?]$|%([.!?][' . "'" . '")]?)@<=[")]|.%(\n[> ]*\_$|%$)@=)', a:dir) == 0
    echohl ErrorMsg
    if exists("s:jp_match") && a:dir =~ 'b'
      echo 'top'
    elseif exists("s:jp_match")
      echo 'bottom'
    else
      echo 'no match'
    endif
    echohl None
  else
    let s:jp_match = 1
  endif
endfunction

function! s:CR()
  let n = line('.')
  if getline(n - 1) =~ '^ \+$'
    call setline(n - 1, '')
  endif
  s/[^ ]\@<= *·\@=//e
  call cursor(n, match(getline(n), '·') + 1)
  norm! x
endfunction

function! s:Clean()
  unmenu    Posting
  unmenu!   Posting
  mapclear  <buffer>
  mapclear! <buffer>
  nmapclear <buffer>
  vmapclear <buffer>
  imapclear <buffer>
  omapclear <buffer>
  nunmap    ,h
endfunction

function! s:Add_menu(...)
  sil! call s:Clean()
  if !exists("a:1")  " prepare for editing
    if exists("s:gui_sup")
      anoremenu	  <silent> &Posting.&Display.&Help\ on/off<Tab>,h	:call <SID>Help('toggle')<Cr>
      anoremenu	  <silent> &Posting.&Display.&Attribs\ on/off<Tab>,a	:sil call <SID>Att_win('toggle')<Cr>
      anoremenu	  <silent> &Posting.&Display.H&ighlight\ on/off<Tab>,i	:call <SID>Syn_high('toggle')<Cr>
      anoremenu	  <silent> &Posting.&Display.&Wrap\ on/off<Tab>,w	:setl invwrap<Cr>
      anoremenu	  <silent> &Posting.&Display.&Virtual\ on/off<Tab>,v	:call <SID>Virt_edit('toggle')<Cr>
      menu	  Posting.Display.-Sep1- :
      anoremenu	  <silent> &Posting.&Display.&Restore<Tab>,u		:call <SID>Undo('Post')<Cr>
      anoremenu	  <silent> &Posting.&Display.&Save!<Tab>,s		:sil! call <SID>Send('draft')<Cr>
      menu	  Posting.-Sep1- :
      if s:always_reformat == 0
        vnoremenu <silent> &Posting.&Take\ out<Tab>tt			:call <SID>Take_out('nowrap', 'v', v:count)<Cr>
        nnoremenu <silent> &Posting.&Take\ out<Tab>tt			:call <SID>Take_out('nowrap', 'n', v:count)<Cr>
        anoremenu <silent> &Posting.&Split<Tab>Shift-_			:<C-U>sil! call <SID>Split_par('nowrap')<Cr>
      else
        vnoremenu <silent> &Posting.&Take\ out<Tab>^t^t			:call <SID>Take_out('wrap', 'v', v:count)<Cr>
        nnoremenu <silent> &Posting.&Take\ out<Tab>^t^t			:call <SID>Take_out('wrap', 'n', v:count)<Cr>
        anoremenu <silent> &Posting.&Split<Tab>Ctrl-_			:<C-U>sil! call <SID>Split_par('wrap')<Cr>
      endif
      vnoremenu	  <silent> &Posting.Re&format<Tab>,r			:call <SID>Reformat('v')<Cr>
      nnoremenu	  <silent> &Posting.Re&format<Tab>,r			:call <SID>Reformat('n')<Cr>
      menu	  Posting.-Sep2- :
      anoremenu	  <silent> &Posting.&Paste.&quoted<Tab>qq		:sil! call <SID>Paste('quo', 'nowrap')<Cr>
      anoremenu	  <silent> &Posting.&Paste.&indented<Tab>nn		:call <SID>Paste('ind', 'nowrap', <SID>Input('indent', 8))<Cr>
      inoremenu	  <silent> &Posting.&Insert.&footnote<Tab>Alt-f		<C-K>.M<C-O>:call <SID>Ins_foot_sec()<Cr>
      anoremenu	  <silent> &Posting.&Insert.&table<Tab>Alt-t		:call <SID>Ins_tab_sec()<Cr>
      anoremenu	  <silent> &Posting.&Insert.&box<Tab>Alt-b		:call <SID>Ins_frame_sec()<Cr>
      menu	  Posting.-Sep3- :
      anoremenu	  <silent> &Posting.&Ready!<Tab>ff			:<C-U>sil! call <SID>Do_fin_form('nowrap')<Cr>
      tm &Posting.&Display.&Help\ on/off	    To take a look at the command list
      tm &Posting.&Display.&Attribs\ on/off	    Slots in attribution lines
      tm &Posting.&Display.H&ighlight\ on/off	    Turns on/off syntax highlighting
      tm &Posting.&Display.&Wrap\ on/off	    Let's you show/hide longer lines
      tm &Posting.&Display.&Virtual\ on/off	    Turns on/off virtual-editing
      tm &Posting.&Display.&Restore		    Use this to undo :Post
      tm &Posting.&Display.&Save!		    To resume writing later
      tm &Posting.&Take\ out			    Takes out visually selected area
      tm &Posting.&Split			    Splits paragraph to enable reply
      tm &Posting.Re&format			    Adjusts linewidth and corrects bad quoting markup
      tm &Posting.&Paste.&quoted		    Inserts text from clipboard
      tm &Posting.&Paste.&indented		    Inserts text from clipboard
      tm &Posting.&Insert.&footnote		    Will appear in separate footnote section
      tm &Posting.&Insert.&table		    Text inside will be disregarded
      tm &Posting.&Insert.&box			    Content will be surrounded by a frame
      tm &Posting.&Ready!			    Prepares message for sending
    endif
    nn	  <unique> <silent> ,h			    :call <SID>Help('toggle')<Cr>
    nn	  <buffer> <silent> ,a			    :sil call <SID>Att_win('toggle')<Cr>
    nn	  <buffer> <silent> ,i			    :call <SID>Syn_high('toggle')<Cr>
    nn	  <buffer> <silent> ,w			    :setl invwrap<Cr>
    nn	  <buffer> <silent> ,v			    :call <SID>Virt_edit('toggle')<Cr>
    nn	  <buffer> <silent> ,p			    :sil! call <SID>Unplenk()<Cr>
    vn	  <buffer> <silent> ,c			    :<C-U>sil! '<,'> call <SID>Ce()<Cr>
    nn	  <buffer> <silent> ,u			    :call <SID>Undo('Post')<Cr>
    nn	  <buffer> <silent> ,s			    :sil! call <SID>Send('draft')<Cr>
    nn	  <buffer> <silent> ll			    :<C-U>sil! call <SID>Shrink(v:count, 'nowrap')<Cr>
    nn	  <buffer> <silent> <C-l><C-l>		    :<C-U>sil! call <SID>Shrink(v:count, 'wrap')<Cr>
    vn	  <buffer> <silent> tt			    :call <SID>Take_out('nowrap', 'v', v:count)<Cr>
    nn	  <buffer> <silent> tt			    :call <SID>Take_out('nowrap', 'n', v:count)<Cr>
    vn	  <buffer> <silent> <C-t><C-t>		    :call <SID>Take_out('wrap', 'v', v:count)<Cr>
    nn	  <buffer> <silent> <C-t><C-t>		    :call <SID>Take_out('wrap', 'n', v:count)<Cr>
    nn	  <buffer> <silent> <Space>		    :call <SID>Jump('W')<Cr>
    nn	  <buffer> <silent> <M-Space>		    :call <SID>Jump('Wb')<Cr>
    nn	  <buffer> <silent> <S-Space>		    :<C-U>sil! call <SID>Split_par('nowrap')<Cr>
    nn	  <buffer> <silent> <C-Space>		    :<C-U>call <SID>Split_par('wrapman')<Cr>
    vn	  <buffer> <silent> ,r			    :call <SID>Reformat('v')<Cr>
    nn	  <buffer> <silent> ,r			    :call <SID>Reformat('n')<Cr>
    nn	  <buffer> <silent> qq			    :sil! call <SID>Paste('quo', 'nowrap')<Cr>
    nn	  <buffer> <silent> <C-q><C-q>		    :sil! call <SID>Paste('quo', 'wrap_dot')<Cr>
    nn	  <buffer> <silent> nn			    :call <SID>Paste('ind', 'nowrap', <SID>Input('indent', 8))<Cr>
    nn	  <buffer> <silent> <C-n><C-n>		    :call <SID>Paste('ind', 'wrap_dot', <SID>Input('indent', 8))<Cr>
    ino	  <buffer> <silent> <M-f>		    <C-K>.M<C-O>:call <SID>Ins_foot_sec()<Cr>
    ino	  <buffer> <silent> <M-t>		    <C-O>:call <SID>Ins_tab_sec()<Cr>
    ino	  <buffer> <silent> <M-b>		    <C-O>:call <SID>Ins_frame_sec()<Cr>
    nn	  <buffer> <silent> ff			    :<C-U>sil! call <SID>Do_fin_form('nowrap')<Cr>
    nn	  <buffer> <silent> <C-f><C-f>		    :<C-U>sil! call <SID>Do_fin_form('wrap')<Cr>
    nn	  <buffer> <silent> <M-LeftMouse>	    <LeftMouse>:sil! call <SID>Edit('mark')<Cr>
    nn	  <buffer> <silent> <M-C-LeftMouse>	    <LeftMouse>:sil! call <SID>Edit('force')<Cr>
    nn	  <buffer> <silent> <M-RightMouse>	    <LeftMouse>dd
    ino	  <buffer> <silent> <Up>		    <C-O>gk
    ino	  <buffer> <silent> <Down>		    <C-O>gj
    no	  <buffer> <silent> <Up>		    gk
    no	  <buffer> <silent> <Down>		    gj
    ino	  <buffer> <silent> <Home>		    <C-O>g0
    ino	  <buffer> <silent> <End>		    <C-O>g$
    no	  <buffer> <silent> <Home>		    g0
    no	  <buffer> <silent> <End>		    g$
    if s:format_flowed == 1
      vn  <buffer> <silent> ,qt			    :<C-U>sil! '<,'> call <SID>GQ('txt')<Cr>
      vn  <buffer> <silent> ,qq			    gq
      ino <buffer> <silent> <Cr>		    ·<C-O>:sil! call <SID>CR()<Cr><Cr>
    else
      vn  <buffer> <silent> ,qt			    gq
      vn  <buffer> <silent> ,qq			    gq
    endif
    nn	  <buffer> <silent> ,qV			    :echohl ModeMsg<Cr>:echo '-- SCAN --'<Cr>:echohl None<Cr>0r>:exe 'sil! norm! 0<C-V><C-V>' . &co . '<Bar>o'<Cr>
    nn	  <buffer> <silent> ,qj			    j:sil! .v/[^> ]/+<Cr>
    vm	  <buffer> <silent> ,qj			    <Esc>,qjm''<,qV''
    nn	  <buffer> <silent> ,qk			    k:sil! .v/[^> ]/-<Cr>
    nn	  <buffer> <silent> ,q_			    _
  else  " leave editing mode
    if exists("s:gui_sup")
      anoremenu	  <silent> &Posting.&Restore<Tab>,u			:sil! call <SID>Undo('ff')<Cr>
      anoremenu	  <silent> &Posting.&Send!<Tab>,s			:sil! call <SID>Send()<Cr>
      tm Posting.&Restore			    Use this to correct typos...
      tm &Posting.&Send!			    Currently "save and exit"
    endif
    nn	  <buffer> <silent> ,u			    :sil! call <SID>Undo('ff')<Cr>
    nn	  <buffer> <silent> ,s			    :sil! call <SID>Send()<Cr>
  endif
endfunction

function! s:App_foot_sec()
  function! s:App()
    norm! v/#)lc
    exe 'norm! a[' . s:ind . ']'
    .s/\]\</\] /ge
    'd put "
    norm! 0vG$:s/^(FN: *\(\_.*\)#)/\1/gv"*c
    call s:Paste('ind', 'wrap', 5)
    exe "'d norm! j0R[" . s:ind . ']'
    $- ma d
    let s:ind = s:ind + 1
  endfunction
  1
  if search('(FN:', 'W') == 0
    unlet! s:footnote
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

function! s:Make_box() range
  let lb = '|                                                                                                   '
  let tb = '----------------------------------------------------------------------------------------------------'
  function! s:Format(width)
    let tw0 = &tw
    let &l:tw = s:Max(a:width - 4, s:len_t + 4)
    'a+,'b- d * | put! _
    call s:Paste('ind', 'wrap_dot', 0) | d
    let &l:tw = tw0
  endfunction
  exe a:firstline . ' ma a'
  exe a:lastline . ' ma b'
  let s:len_t = 0
  if getline("'a") =~ 'T:.*\S.*$'
    let title = matchstr(getline("'a"), 'T:\zs.*')
    let s:len_t = strlen(title)
  endif
  let n = matchstr(getline("'a"), 'W:\zs\d\{2}')
  if n != ''
    call s:Format(n)
  endif
  let len_b = 0
  'a+,'b- g/^/ if col('$') > len_b | let len_b = col('$') | endif
  let len   = s:Max(s:len_t + 7, len_b + 2)
  let d_len = (s:Max(s:len_t + 5, len_b) - len_b) / 2
  let ve0=&ve | let &ve='all' | let wrap0=&wrap | let &l:wrap=0
  let move = s:Down(line("'b") - line("'a") - 2)
  exe "norm! 'aj0\<C-V>" . move . 'I' . strpart(lb, 0, d_len + 2) . '' . len . 'l' . move . 'r|'
  let &ve=ve0 | let &l:wrap=wrap0
  let @" = strpart(tb, 0, len - 1)
  'a put =',' . @\" . '.'
  if exists("title")
    exe 'norm! 0' . ((len - s:len_t - 3) / 2) . 'lR[ ' . title . ' ]'
  endif
  'b put! ='`' . @\" . '´'
  'a g@W:.\{,2}/c@ .+,'b- call s:Shft((s:cols - len - 2) / 2)
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
      norm! ggV/^#$"*y
    else
      norm! ggV/^#$"*dG"*p
      w!
    endif
    bw!
    norm! G:+?.o-- "*gp
    norm! G:+?^#VGd
  endif
endfunction

function! s:Do_fin_form(form)
  function! s:Sep_lines() range
    exe 'sil! ' . a:firstline . ',' . a:lastline . ' g/^\(.\).*\n\1\@!./ put _'
  endfunction
  function! s:Make_rep_led_sec()
    if v:count == 0
      let s:lev = s:max_quo_lev
    else
      let s:lev = s:Min(v:count, s:max_quo_lev)
    endif
    exe 'sil! 1,' . s:Min(line('$'), s:rep_led_max) . ' g/^>\{' . s:lev . ',}.*•/ d'
    exe 'sil! 1,' . s:Min(line('$'), s:rep_led_max) . ' g/•.*\n[> ]*$/ +d'
    exe 'sil! 1,' . s:Min(line('$'), s:rep_led_max) . ' g/•.*\n[^•]*$/ call s:Sep_rep_led(line("."))'
    exe 'sil! 1,' . s:Min(line('$'), s:rep_led_max) . ' g/•/ s/•/ / | s/^ *\| *$//g | norm! gqq'
  endfunction
  function! s:Space_stuff() range
    if s:format_flowed == 1
      exe a:firstline . '+,' . a:lastline . '- call s:Shft(1)'
    endif
  endfunction
  function! s:Proc_sec(type, action)
    while search('^(' . a:type . ':', 'w') > 0
      ma h | /^#)$/ ma l
      if line("'l") > line("'h") + 1
	exe "'h,'l call " . a:action
	'h+,'l- s/^/@/
      endif
      call setline("'h", '')
      call setline("'l", '')
    endwhile
  endfunction
  call s:Att_win('off')
  setl nosmd nosc noai
  let s:fl0 = line('.')
  let s:fc0 = col('.')
  let s:wl0 = winline()
  %y z
  call s:Frame('save')
  $ put _ | let top = search('^\%(\s.*\S\|[^> \t]\)', 'bw')
  if top > 0 && top < line('$') && search('^>', 'bW') > 0
    exe (top + 1) . ',$ d'
  endif
  1 | put! _
  call s:App_foot_sec()
  call s:Proc_sec('BX', 's:Make_box()')
  call s:Proc_sec('TB', 's:Space_stuff()')
  if !exists("s:formatting")
    if s:format_flowed == 1
      g/^ / s/\v^(.{-}) *$/+ \1/e
    else
      % s/^ \@=/+/e
    endif
  endif
  % s/^[^>|@+]\@=/\*/e
  % call s:Sep_lines()
  if !exists("s:formatting")
    setl fo+=t ai
    1
    while search("^\*", "W") > 0
      norm vap:s/^\*//egv,qt
    endwhile
    setl noai
  else
    % s/^\*//e
  endif
  let s:max_quo_lev = 0
  1
  while search('^>', 'W') > 0
    call s:Quo_lev(s:Count('chron'), a:form, 'nochk')
  endwhile
  g/^>\@!.*\n>[> ]*$/+d
  g/^>[> ]*\n>\@!/d
  if s:format_flowed == 1
    g/^>/ s/ *$/ /e
    v/[^> ]/ .-,. s/ $//e
  endif
  1 d
  % s/^\_[[:space:]]*$//e
  % s/^[+@]//e
  if exists("s:footnote")
    % s/\v\n+%(\n ?_{9}$)@=//e
  endif
  call s:Make_rep_led_sec()
  call s:App_sig()
  1?.?;$ v/./d
  call s:Frame('restore')
  1
  call s:Add_menu('undo')
  setl smd sc ai
  let s:ff_stat = 'FF/' . s:lev
  if exists("b:current_syntax") && b:current_syntax == 'posting_color'
    let s:cusy0 = 'on'
  else
    let s:cusy0 = 'off'
  endif
  call s:Syn_high('off')
  exe 'match Search /.\%>' . s:vcols2 . 'v/'
  let s:wrap0=&wrap | setl nowrap
endfunction

function! s:Init_post(columns, remote)
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
  setl nosmd nosc
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
  let &siso=0
  setl dy=lastline
  setl lcs=extends:>,precedes:<
  if s:format_flowed == 0
    let &l:tw    = a:columns
    let s:vcols1 = a:columns + 1
    let s:vcols2 = a:columns + 1
  else
    let &l:tw    = a:columns - 1
    let s:vcols1 = a:columns
    let s:vcols2 = a:columns + 1
  endif
  setl com=n:>,n:|,fb:-
  setl fo=crq1
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
  setl langmenu=none
  call s:Add_menu()
  let s:tpq_initials = 1
  if a:remote == "!"
    let s:server = 1
    let s:tpq_repair = 'auto'
  else
    let s:tpq_repair = 'man'
  endif
  let s:tp_attrib = s:check_attribs_on_startup
  let s:rep_led = '\%(' . s:name . '\|' . s:email . '\|' . s:verb . '\).*[a-z>]\@<=\%(:\|\.\.\.\)\_$'
  match Ignore /·/
  call s:Do_init_form()
  match none
  if s:syn_high_on_startup == 1
    call s:Syn_high('on')
  else
    call s:Syn_high('off')
  endif
  unlet s:tpq_initials
  let s:tpq_repair = 'off'
  let s:tp_attrib = -1
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
  setl smd sc
  unlet! s:formatting
  unlet! s:jp_match
  call s:Top()
endfunction

command! -nargs=1 -bang Post :call s:Init_post(<args>, "<bang>")

sil! call s:Inst_doc()

if exists("s:help_doc")
  echo "Posting v1.2: Installed help-documentation."
endif

" }}}1
" vim600:fo=crq2:com=n\:\":tw=72:so=5:siso=15:nowrap:sts=2:sm:fdm=marker:
