"
"           Posting v1.3 - Stand-alone USENET/e-mail environment
"                         Target: GVim version 6.2
"                         Last Change: Dec 01 2003
" 
"                     By Tim Allen <eden@hod.aarg.net>
" 
" 
" Posting is derived from a small set of mappings I once messed around
" with in order to ease preparing USENET/e-mail articles with GVim. It
" now offers various facilities related to this, such as line-wrapping,
" pasting, footnote-management, box-quoting, quoting-markup-correction,
" folding or attribution-management. To activate, have GVim read this
" (":so posting.vim") or copy this file to your local plugin directory,
" and then type in ":Post " followed by the number of characters per line
" you want to permit. (A typical value for USENET posts would be 72.)
"
" Help to certain topics can be obtained with ":h posting.txt" or
" ":h posting<Ctrl-D>" resp. (":ts /posting" when requested from help
" window) to obtain an overview. Note: Posting makes use of Vim's built-in
" :helptags routine to generate the tag file information you may later on
" access via Vim's :help system. This is done on start-up (strictly
" speaking when this file is source'd for the first time) and normally
" should work fine unless the directory structure in your local
" environment should be in some strange way different to the default. If
" it, despite this, does not (might happen when source'd from a non-Vim
" directory) or you nevertheless should think this might not work in your
" local environment, please take a look at the Settings folder below!
" You'll find a variable "s:vim_doc_path" there you may use to explicitly
" point to the directory you expect help doc's to be stored.
"
"  Have fun!
"   Tim Allen
"
" PS: Please, feel free to send bug reports, suggestions or simply
"     questions on how to use this script!
"
" Documentation {{{1
"=========================================================================
" Usage {{{2
"------------------------------------------------------------------------
"					      *posting-manual*
"
" 1.  Object			  |posting-shape|
" 2.  Usage			  |posting-commands|
" 3.  Macros			  |posting-sections|
" 4.  Invocation  		  |posting-invocation|
" 5.  Window			  |posting-window|
" 6.  Highlighting		  |posting-highlighting|
" 7.  Signature			  |posting-sigfile|
" 8.  Style  			  |posting-quoting_style|
" 9.  History			  |posting-history|
" 10. Copyright			  |posting-copyright|
" 11. Settings			  |posting-current_settings|
"
"------------------------------------------------------------------------
" 1. Object				      *posting-shape*
"
" This is how a typical Posting posting should look like:
"	
"		     | From: ...              | <-- optional
"		     | Newsgroups: ...        |
"	   Header¹ --| Reply-To: ...          |
"		     | Date: ...              |
"		     | Subject: ...           |
"		       <empty line>           |
"		     | <quoted/unquoted text>
"	    Body ----| ~~                     | <-- optional
"		     | <unquoted text>        |
" _________
" ¹ any number and any order
"
" Any text behind the cutmark delimiter ("~~") will be ignored, thus you
" may use this section to append annotations you don't want to be formated
" or sent.
"
"------------------------------------------------------------------------
" 2. Usage				      *posting-commands*
"
" To keep the GUI menu simple I did enclose a certain subset of commands
" only, the vast majority, especially those who take extra arguments or
" got more complex duties, is accessible via the keyboard only. (All you
" need to know at first is <Tab> to step forward, <Cr> to split when
" framing an answer and <M-z> to send..)
"
"					      *po_i_ALT-H*
" <M-h>			Opens window showing this section. To close
"			press <M-h> again.
"
"					      *po_i_ALT-W*
" <M-w>			Toggles line wrapping. (shows/hides lines
"			longer than allowed) When on flag [WP] will
"			be visible.
"
"					      *po_i_ALT-E*
" <M-e>			Turns on/off 'virtualedit'. (|virtualedit|)
"			When turned ob flag [VE] will be visible.
"
"					      *po_i_ALT-+*
" <M-+>			Zoom in. (increases font size) Zoom in/out
"			currently works with font Courier New only!
"
"					      *po_i_ALT--*
" <M-->			Zoom out. (increases visible area)
"
"					      *po_i_ALT-R*
" <M-r>			Fold in. (unhides one level more)
"
"					      *po_i_ALT-M*
"					      *po_n_ALT-M*
" <M-m>			Fold out. (hides one level more) When [count]
"			is given quotations with level greater than or
"			equal to [count] are folded out. This is just
"			as "[count]<M-a>" works (see below), except
"			that text selected isn't really taken out, but
"			just made invisible. Text displayed for a closed
"			fold shows the number of non-empty lines it
"			contains followed by a list of names repre-
"			senting the authors of that chronologically
"			ordered part which is nearest to the bottom of
"			the folder. Names are ordered from higher to
"			lower levels, i. e. the author text below is
"			addressed to always appears at the end of the
"			list. When there's no logical connexion between
"			text within the folder and text below, the list
"			will disappear.
"
"					      *po_i_ALT-Q*
" <M-q>			Show level-1-quotes only. (toggles folding)
"
"					      *po_i_ALT-Y*
" <M-y>	  		Turns on/off syntax highlighting.
"
"					      *po_i_ALT-X*
" <M-x>	  		Opens Context window with information on author
"			and logical structure. (Levels are represented
"			by uppercase letters, starting with 'A' which
"			always represents the author of the eldest piece
"			of text found. (To see what levels are present
"			at all take a look at the statusline.)
"			Horizontal bars will indicate chronological
"			inconsistencies.) This window is updated
"			continuously.
"
"					      *po_i_<M-LeftMouse>*
" <M-LeftMouse>		Use this to mark/unmark attribution-lines not
"			yet detected properly. (Please note that marking
"			levels already assigned won't work; you'd then
"			have to unmark first. Attributions may span up
"			to four lines maximum. To process a multi-line
"			attribution klick on its first line.)
"
"					      *po_i_<M-C-LeftMouse>*
" <M-C-LeftMouse>  	Like <M-LeftMouse>, however the line referred to
"			is shortened too. Applying it a second time
"			brings back the line in its original shape.
"			This also applies to lines shortened automa-
"			tically on start-up.
"
"					      *po_i_<M-RightMouse>*
" <M-RightMouse>  	Appends an empty line.
"
"					      *po_i_<C-RightMouse>*
" <C-RightMouse>  	Deletes the line referred to.
"
"					      *po_i_#{char}*
" #{0-e}  		Provides several facilities around attributions:
" ##			"#{0-e}" (unmark attribution with level given
" #u			(no need to move the cursor)), "##" (mark off
" #m			current line as attribution-line (This is like
" #t			<M-LeftMouse> or <M-C-LeftMouse> resp. depending
" #j			on s:attrib_default_action. (for details see
"			|posting-current_settings|))), "#u" (unmark all
"			attributions currently marked), "#m" (find and
"			mark attributions (unmarks first and then
"			searches whole file)), "#t" (convert top-posting
"			style references into attributions), "#j" (join
"			attributions (checks number and order)) Please
"			note, the leading number sign character _must_
"			be followed by a second character. (To insert
"			a number sign character type CTRL-V #.)
"
"					      *po_i_<Tab>*
"					      *po_n_<Tab>*
"					      *po_i_<C-Tab>*
" <Tab>	 		This is to quickly step through most recent
" <C-Tab>  		quotes, i. e. level-1-quotations. The cursor
"			will stop at places likely being suitable for
"			framing an answer. (Press <Cr> or <C-Cr> (see
"			below) to do so.) Please note, after scrolling
"			via Page-keys (see |po_i_<PageUp>|), search will
"			start from top of the screen regardless of where
"			the cursor is. On unquoted lines press <C-Tab>
"			instead.
"
"					      *po_i_<S-Tab>*
" <S-Tab>  		Like <Tab> just opposite direction.
"
"					      *po_i_ALT-L*
" <M-l>	  		Removes signatures and commercial ads.
"
"					      *po_i_ALT-T*
" <M-t>	  		Resizes/replaces tab's. Since the actual size of
"			tabulation characters is specific to the program
"			used for viewing, messages with tab's may look
"			strange. Via <M-t> you can readjust your local
"			setting and replace tab's found with an
"			appropriate number of spaces, so that your
"			message won't change appearance again when
"			sending. (Use +/- keys and hit <Enter> as soon
"			as you feel the value being used might be the
"			right one.) When highlighting is turned on tab's
"			found within quotations will appear highlighted
"			as well. (|posting-highlighting|) Your own tab
"			key won't insert tabulation characters, so you
"			may use it whenever necessary.
"
"					      *po_i_ALT-K*
" <M-k>	  		Corrects Plenking.
"
"					      *po_i_ALT-I*
" <M-i>	 		Converts top-posting style message into bottom-
"			posting style message (interleaved). You may use
"			this to convert TOFU's, which is the main
"			purpose of this command, as well as pure
"			top-postings with fullquote where text is not
"			prepended by angle brackets as usual when
"			quoting. This should work fine in most cases, it
"			might fail however for hybrid messages often
"			seen too, where both common styles are mixed up
"			to build something 'very special'. Please note
"			that this command should be applied prior to
"			framing an answer. To undo apply <M-i> again.
"
"					      *po_i_SHIFT-ALT-I*
" <S-M-i>  		Like <M-i>, however aittributions already marked
"			are left untouched. (<M-i> isn't entire
"			foolproof since it essentially depends on proper
"			attribution recognition. To accomplish
"			conversion anyway go ahead as follows: "#u" ->
"			"#t" -> "##" (<M-LeftMouse>, <M-C-LeftMouse>) ->
"			<S-M-i>)
"
"					      *po_i_ALT-D*
"					      *po_n_ALT-D*
"
" <M-u>	 		Dismiss. Use this to undo ":Post". (Changes made
"			till then will be lost!)
"
"					      *po_n_ALT-A*
"					      *po_v_ALT-A*
" <M-a>			This command works in Normal and Visual mode.
" {Visual}<M-a>		Text selected (whole paragraph when called from
"			Normal mode) is replaced by a mark indicating
"			what happened. You'll be asked for a short
"			comment describing what has been removed from
"			the message. (Each level has its own mark, so,
"			depending on the area you select, you might be
"			prompted several times.) To prevent a certain
"			region from being taken out press <Esc>. When
"			[count] is given <M-a> has a slightly different
"			meaning. Instead of asking whether or not to
"			replace, parts with level greater than [count]
"			are removed automatically. This way you can cut
"			off elder parts at a single blow while still
"			keeping the message's logical structure intact.
"
"					      *po_n_SHIFT-ALT-A*
"					      *po_v_SHIFT-ALT-A*
" <S-M-a> 		Like <M-a>, the region selected however will be
" {Visual}<S-M-a>  	reformated too.
"
"					      *po_i_<Cr>*
"					      *po_n_<Cr>*
" <Cr> 			Splits paragraph and removes quotes with level
"			greater than [count]; text below remains
"			unchanged. Note: When [count] is given, <Cr>
"			also checks the chronological order, that's to
"			say unordered parts are then removed thereby too
"			even if their quoting level might be within the
"			given range!
"
"					      *po_i_<C-Cr>*
"					      *po_n_<C-Cr>*
" <C-Cr>  		Like <Cr> but additionally turns on Scan mode
"			which allows to quickly reformat through quoted
"			text containing tables, lists and so on,
"			generally speaking indented structures. A few
"			keys have a special meaning then:
"
"					      *posting-scanmode*
"
"			n, <Tab>       go to next non-empty line
"			N              go to next paragraph
"			p, <Bs>        go to previous non-empty line
"			P              go to previous paragraph
"			+              mark to next non-empty line
"			j              justify visually selected area
"			.              justify to next line¹
"			<Cr>           justify to the end of paragraph²
"			<C-Cr>         justify to the end
"			a              abridge visually selected area
"			1, 2,..., E    abridge to the end
"			c              chron.-check to the end
"			d              delete visually selected area
"			u	       undo last change
"			q              turn on Visual mode (see below)
"			h              show this explanation
"			<Esc>          quit
"
"					      *posting-scanmode_VISUAL*
"
"			q              unmark one character (left)
"			+              mark to next non-empty line
"			<Bs>           unmark one character (right)
"			a	       abridge visually selected area
"			<Esc>          quit
"
"			You may issue this command from unquoted lines,
"			too. Scan mode then is turned on for the first
"			quotation found when searching backwards.
"			_________
"			¹ ignores leading numbers
"			² visually selected area in case it spans more
"			  than a single line
"
"					      *po_v_CTRL-Y*
" <C-y>			Yanks text selected into clipboard.
"
"					      *po_i_CTRL-P_<Space>*
" <C-p><Space> 	  	Puts text from clipboard after the cursor
"			(shortcuts <C-p>{char} are available in Insert
"			mode only)
"
"					      *po_i_CTRL-P_I*
" <C-p>i  		Inserts text from clipboard optionally shifted
"			to the right
"
"					      *po_i_CTRL-P_CTRL-I*
" <C-p><C-i>		Like "<C-p>i" but lines are wrapped too to
"			suit line length
"
"					      *po_i_CTRL-P_Q*
" <C-p>q  		Inserts text from clipboard '|'-quoted
"
"					      *po_i_CTRL-P_CTRL-Q*
" <C-p><C-q>		Like "<C-p>q" but lines are wrapped too to
"			suit line length
"
"					      *po_i_CTRL-P_B*
" <C-p>b  		Inserts text from clipboard surrounded by a
"			Frame section
"
"					      *po_i_CTRL-S_F*
"					      *po_v_CTRL-S_F*
" <C-s>f  		Inserts Footnote section. (Text inside will
" {Visual}<C-s>f  	appear in separate section at the end of your
"			message. (see |posting-sections|) Shortcuts
"			<C-s>{char} are available in Insert and Visual
"			mode.)
"
"					      *po_i_CTRL-S_T*
"					      *po_v_CTRL-S_T*
" <C-s>t  		Inserts Table section before current line.
" {Visual}<C-s>t  	(text inside will be left untouched (see
"			|posting-sections|))
"
"					      *po_i_CTRL-S_B*
"					      *po_v_CTRL-S_B*
" <C-s>b  		Inserts Box (Frame) section before current line.
" {Visual}<C-s>b  	(content will be surrounded by a frame (see
"			|posting-sections|))
"
"					      *po_i_ALT-C*
"					      *po_v_ALT-C*
" <M-c>			Centers visually selected area/paragraph.
" {Visual}<M-c>
"
"					      *po_i_ALT-N*
"					      *po_v_ALT-N*
" {Visual}<M-n>	  	Numbers lines within area selected. (use block-
"			wise Visual mode to select range and column)
"
"					      *po_i_ALT-J*
"					      *po_n_ALT-J*
"					      *po_v_ALT-J*
" <M-j>			Adjusts line-width and/or corrects bad quoting
" {Visual}<M-j>		markup. This shortcut is available in Normal,
"			Insert and Visual mode and may be used for text
"			typed in as well as for quotations. (Please note
"			that applying <M-j> to unquoted text which is
"			not part of a Table section turns off formatting
"			of unquoted text via <M-p> or <M-z>. (Flag
"			[FT] will show up then.) Note also please that
"			<Cr> will have a different meaning in case that
"			s:format_flowed is set to 1. This is due to that
"			according to RFC 2646 paragraphs need to be
"			terminated by a so-called Hard line break. (A
"			side effekt of this you may find useful is that
"			text terminated this way, i. e. by pressing <Cr>
"			in Insert mode, always will be treated as an
"			independent part, that's to say the overall
"			structure of what has been written is preserved
"			even if you won't insert empty lines to build
"			separated blocks of text. Hard line breaks once
"			entered will persist.) Lists which are not
"			numbered are expected to be subdivided via
"			list-bullets '-' (hyphen) or '*' (star).)
"
"					      *po_i_SHIFT-ALT-J*
"					      *po_n_SHIFT-ALT-J*
"					      *po_v_SHIFT-ALT-J*
" <S-M-j>  		Like <M-j>, leading numbers however won't be
" {Visual}<S-M-j>  	treated as if being part of a numbered list.
"
"					      *po_i_ALT-P*
"					      *po_n_ALT-P*
" <M-p>			Shows preview, i. e. formats what has been
"			written (text including sections (table, box)
"			and footnotes), checks quoting markup and
"			chronological order, removes text elder than
"			that of quoting level [count] and any text found
"			after the last unquoted non-empty line. Flag
"			[--P] will show up when final-formatting is
"			done. To leave Preview screen apply <M-p> again.
"			(Please note, when formatting numbered or
"			bullet-lists, the number/bullet has to be placed
"			in the first column, otherwise it'll be
"			considered as being indented and thus left
"			untouched.)
"
"					      *po_i_SHIFT-ALT-P*
"					      *po_n_SHIFT-ALT-P*
" <S-M-p>  		Like <M-p> quotes however are reformated too.
"
"					      *po_i_ALT-O*
"					      *po_n_ALT-O*
" <M-o>			Save and exit. Use this in case you intend to
"			break and resume writing later. The message is
"			marked as draft to ensure that it won't change
"			again when opened later. (Annotations are left
"			untouched.) When loading a draft again the
"			session is restored exactly as it was when
"			exiting. (This needs global restore to be
"			enabled. (":set vi+=!"))
"
"					      *po_i_ALT-Z*
"					      *po_n_ALT-Z*
" <M-z>			Send. You may use this command from all three
"			Screens, i. e. Repair screen ([R--]), Compose
"			screen ([-C-]) and Preview screen ([--P]). When
"			applied from Compose screen you may supply an
"			additional [count] parameter denoting the maximum
"			level of quoted text you want to keep. Please note
"			that Vim won't terminate when Posting has been
"			called via ":Post!"; instead it will be minimized
"			or iconised resp. depending on the operating
"			system being used.
"
"					      *po_i_SHIFT-ALT-Z*
"					      *po_n_SHIFT-ALT-Z*
" <S-M-z>  		Like <M-z>, quotes however are reformated too.
"			(available in Compose screen [-C-] only)
"
"					      *po_i_<PageUp>*
"					      *po_i_<PageDown>*
" <PageUp>		Slightly different to the default. When
" <PageDown>		scrolling the place the cursor is on is stored
"			(cursor stops blinking then) and restored as
"			soon as this position again becomes part of the
"			visible file area. Pressing it once means the
"			content of the window will move by a quarter
"			of its current height.
"
"					      *po_i_<S-PageUp>*
"					      *po_i_<S-PageDown>*
" <S-PageUp>		Like <PageUp>/<PageDown>, content will move a
" <S-PageDown>		whole page however.
"
"					      *po_i_<M-Up>*
"					      *po_i_<M-Down>*
" <M-Up>  		Moves cursor to next nearest reply above/beneath
" <M-Down>  		the current position.
"
"					      *po_n_,q*
" ,q			Helps correcting unusual quote-marks. This
"			shortcut is available for Repair screen [R--]
"			only, i. e. after pressing <M-u> to undo
"			":Post". (You won't hardly ever need it except
"			in those rare cases where something really fatal
"			happened on start-up. To further ease debugging
"			structures which are likely to become a problem
"			when mailing will appear highlighted.)
"
"					      *po_n_,m*
" ,m			Removes unusual characters. To apply place the
"			cursor on the character in question and type
"			",m". (available for Repair screen only)
"
"					      *po_n_,c*
" ,c			Cuts off invalid levels. (There's a fixed upper
"			limit on the level quotations found may have.)
"			(available for Repair screen only)
"
"					      *po_i_ALT-D*
"					      *po_n_ALT-D*
" <M-u>	 		Preformat. (reissues a previously reversed
"			":Post" command) (available for Repair screen
"			only)
"
"------------------------------------------------------------------------
" 3. Macros				      *posting-sections*
"
" To accomplish more complex changes use Posting's built-in macro
" processor. Directives currently implemented are
"
"    a)	Footnote:     |... {FN<opt> <text>} ...
"
"	(Makes Vim create a separate section at the end of your message
"	presenting text of this type arranged in the order it appears in
"	your message. Each item is referenced by a unique number. (valid
"	options: /1 (don't wrap))
"
"    b)	Table:	      |...
"		      |{TB
"		      |<text>
"		      |}
"		      |...
"
"	(Sections of this type are meant for text that may not be for-
"	mated by Posting, i. e. tables, lists and so on. These passages
"	will appear in their original shape when final formatting is
"	done.)
"
"    c)	Box (Frame):  |...
"		      |{BX<opt>
"		      |<text>
"		      |}
"		      |...
"
"	(Draws a frame around what has been written. (valid options:
"	/w<arg>	(textwidth to use for reformatting), /t<arg> (string to
"	use as title), /c (box will appear centered))
"
" Please note that Table and Box sections may contain Footnote sections,
" nesting of Table and Box sections however (among each other as well as
" alternating) normally won't work.
"
"------------------------------------------------------------------------
" 4. Invocation				      *posting-invocation*
"
" To automatically call Posting when loading a mail message file add
"
"	autocmd FileType mail Post 72
"
" to your .vimrc-file (see also |filetype|) or
"
"	gvim "+Post 72"
"
" resp. to your mailer/news-agent's config.-file if you want the
" application being used for mail/news transfer to invoke Posting. To
" further reduce the time spent on start-up make use of Vim's inbuild
" client-server feature.
"
"	gvim --servername MAIL --remote-silent "+Post! 72"
"
" for instance would have Vim start anew only in case that it couldn't
" find an instance which is already running. Shortcuts <M-z> (Send) and
" <M-o> (Postpone) will then have the meaning of "minimize" or "iconise"
" resp., in any case they won't stop an already running server. To ensure
" the server will terminate when leaving one might consider adding
" something like
"
"	gvim --servername MAIL --remote-send ":qa!<Cr>"
"
" to the local logout/shutdown script. (A slight disadvantage of this
" method is that you won't be able to control the usual markup check on
" start-up since Vim won't recognize commands issued manually when
" running in server mode.)
"
" (To avoid loading Posting at all, either remove it from your local
" plugin directory, or put "let loaded_posting = 1" in your .vimrc file.)
"
"------------------------------------------------------------------------
" 5. Window				      *posting-window*
"
" When using Vim as a replacement in mail/news applications such as Mutt
" or Xnews for instance you might find it pleasant to know the GUI window
" always appears in the same place. Supply additional options to do so:
" (see also |winpos| and |-geom|)
"
"	gvim --cmd "winpos X Y" <file>	(Windows)
"	gvim -geometry XxY <file>	(xterm)
"
" Alternatively you may use Posting's "s:window_position" variable as
" well. (see Settings folder)
"
"------------------------------------------------------------------------
" 6. Highlighting			      *posting-highlighting*
"
" Below you'll find a list of all syntax groups Posting makes use of for
" highlighting. Names in parenthesis denote highlighting groups linked to
" by default:
"
" postingHeader (Type), postingSubject (PreProc), postingQuotes
" (Delimiter), postingQuoted1 (Comment), postingQuoted2 (Constant),
" postingQuoted3 (Identifier), postingQuoted4 (Statement),
" postingQuotedTab (Error), postingQuotedLong (Search), postingSection
" (Special), postingTrailer (Error), postingAnnotation (Todo)
"
" To change the color of level-1-quotations you may either change it
" directly
" 
"		    hi postingQuoted1 guifg=green
"
" or, alternatively, by changing the color of the highlighting group it's
" linked to
"
"		    hi Comment guifg=green
"
" The latter can be done via Vim's built-in :color-command as well, which
" not just affects a single syntax group, but all syntax groups currently
" linked to default hightlighting groups. (see |:colorscheme| for details)
"
"------------------------------------------------------------------------
" 7. Signature				      *posting-sigfile*
"
" Posting provides a simple signature import mechanism too, you may use
" to automatically append signatures when composing. To do so simply
" create a file listing signatures you like (each item has to be followed
" by a single number sign character ('#') located on a line of its own),
" and assign its path to "s:sig_file" (see below) so that Posting can find
" it. Then each time you press <M-p> or <M-z> resp. (see above) Posting
" will append a different signature to the message-body. (The file may
" contain an arbitrary number of signatures.)
"
"------------------------------------------------------------------------
" 8. Style				      *posting-quoting_style*
"
" Due to that there's no real universally valid standard on how quoting
" has to be done in electronic mail exchange the style as provided by
" Posting mainly has been affected by own notions and my personal taste.
" A few ideas though are taken from other ressources, such as related
" RFC's or privately maintained sites providing general guidelines.
"
" [1] "Netiquette Guidelines" (Intel Corp., '95) RFC 1855
"     ftp://ftp.demon.co.uk/pub/mirrors/internic/rfc/rfc1855.txt
"
" [2] "The Text/Plain Format Parameter" (R. Gellens, '99) RFC 2646
"     http://www.ietf.org/rfc/rfc2646.txt
"
" [3] Ivan Reid's excellent quoting guideline not just valuable to
"     motorcycle fans... ;-)
"     http://www.windfalls.net/ukrm/postinghelp.html
"
" [4] Dirk Nimmich's more general overview (which is not that exhaustive
"     however as far as quoting related aspects are concerned)
"     http://www.netmeister.org/news/learn2quote.html
"
" [5] Daniel R. Tobias' more than exhaustive Mail Format Site
"     http://mailformat.dan.info
"
" History {{{2
"------------------------------------------------------------------------
" 9. History				      *posting-history*
"
" Dec 01 2003:	- Introduced Repair/Preview screen
"		- Added Context window feature (slot in names of authors)
"		- Added basic Folding support (hide elder levels)
"		- Shortcuts: Added <M-+>/<M--> (zoom in/out),
"		  <M-r>/<M-m> (fold in/out), <M-x> (Context window),
"		  <M-y> (highlighting), <M-RightMouse>, <C-RightMouse>
"		  (append/delete line), <M-l> (remove trailers), <M-t>
"		  (remove tab's), <M-k> (unplenk), <M-i> (convert
"		  top-posting), ",q" (requote), ",m" (remove control
"		  chars), ",c" (cut off invalid levels), <M-c> (center),
"		  <M-n> (number), <M-p> (preformat), "<M-s>{char}"
"		  (insert section), "<M-p>{char}" (paste text from
"		  clipboard), "#{char}" (edit attribution), <M-h> (help).
"		  (|posting-commands|)
"		- Scan mode: Added commands "N" (paragraph down), "P"
"		  (paragraph up), <Cr> (justify par.), <C-Cr> (justify
"		  block), "u" (undo last change), "q" (turn on Visual
"		  mode), "h" (help). (|posting-scanmode|)
"		- Menu slightly more extensive now
"
" 2003 Mai 20:	- Added comprehensive highlighting support (Vim's own
"		  syntax file mail.vim is no longer necessary now.
"		  Take a look at |posting-highlighting|...)
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
"		  to take indent from second line instead of first (use
"		  this to reformat quotations with embedded lists)
"		- 'autoindent' works with format_flowed set to 1 too now
"		  (Sorry for possible inconveniences!)
"		- Box section: New shape (see |posting-sections|)
"		- Various minor enhancements/fixups
"
" 2003 Apr 06:	- Added RFC 2646 support (see comment to parameter
"		 'format_flowed' (Settings folder))
"		- Added support for Vim's built-in client-server
"		  facility (command "Post!", see also *posting-
"		  invocation*)
"		- Added Scan mode feature to ease reformatting of quoted
"		  text with tables, lists or the like inside (see also
"		  comment to shortcut <C-Space>)
"		- Added shortcut ",r" for manual reformatting (see also
"		  comment under |posting-commands|)
"		- Added shortcuts <Up>, <Down>, <Home>, <End> to
"		  enable navigating with line-wrap turned on
"		  (formatoption "t" turned off now per default, use
"		  line-wrap instead (or "Split" which turns it on
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
"		  check_attribs_on_startup (|posting-current_settings|)
"		  and shortcut <M-LeftMouse> (|posting-commands|))
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
" Copyright {{{2
"------------------------------------------------------------------------
" 10. Copyright				      *posting-copyright*
"
" Copyright (C) 2002, 2003 Tim Allen
"
" This program is free software; you can redistribute it and/or modify it
" under the terms of the GNU General Public License as published by the
" Free Software Foundation; either version 2 of the License, or (at your
" option) any later version.
"
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
" Public License for more details.
"
" The exact wording can be found at http://www.gnu.org/copyleft/gpl.html.
" You may also write to the Free Software Foundation, Inc., 59 Temple
" Place - Suite 330, Boston, MA 02111-1307, USA to obtain a copy.
"
" }}}2
"=========================================================================
" Settings {{{1
"------------------------------------------------------------------------
" 11. Settings				      *posting-current_settings*
"
" Edit this section to fit Posting to your needs! (Please note that most
" of what you can find here can be done manually as well after start-up.
" (Corresponding shortcuts are shown in brackets.)):
"
let s:display_mode_on_startup = "color"
" May be either "color" (turns on syntax highlighting (<M-y>)) or "mono"
" (highlights essentials only) optionally followed by a second directive
" which may be either "context" (makes Vim open a second window presenting
" information on context and author (<M-x>)) or "folds" (quotations with
" level greater than one are folded out (<M-m>, <M-q>) (You may supply an
" additional parameter to explicitly denote the number of levels you'd
" like to see. The number has to be enclosed in brackets.). Valid settings
" for example are "mono", "mono,context", "color", "color,folds(2)".
"
let s:attrib_default_action = "check"
" Indicates what to do with attributions. You may select from among three
" possible choices: "mark" (don't touch (<M-LeftMouse>)), "check" (shorten
" in case that line length exceeds limit), "force" (convert to standard
" type (<M-C-LeftMouse>)).
"
let s:rem_trailers_on_startup = 0
" Set this to 1 to remove signatures and disclaimers found. (<M-l>)
"
let s:format_flowed = 0
" Set this to 1 to turn on RFC 2646 conformable formatting. (You won't see
" a great difference to the default, at the recipient's side however your
" message might be better readable when viewed with appropriate software.
" Please note that this requires an extra entry "format=flowed" to be
" added to the Content-Type header of your message when sending. (see also
" [2] (|posting-quoting_style|) for details on RFC 2646))
"
let s:window_geometry = 130
" (height / width) * 100
"
"let s:window_position = "0,0"
" top left corner of window (<pixels from left>,<pixels from top>)
"
"let s:vim_doc_path = ''
" Use this to explicitly point to the directory you expect doc.-files to
" be stored.
"
"let s:sig_file = ''
" This is where Posting expects to find signature templates. (see also
" |posting-sigfile|)
"
"  vim:ts=8:so=0:siso=0:ft=help:norl:nowrap:
"·
" Realization {{{1

if exists("loaded_posting")
  finish
endif

let loaded_posting   = 1
let s:cpo	     = &cpo | set cpo&vim
let s:vimPluginPath  = expand("<sfile>:p:h")
if !exists("s:vim_doc_path")
  let s:vim_doc_path = expand("<sfile>:p:h:h") . '/doc'
endif
let s:tpqQuote	     = '>'
let s:attribLineMax  = 20
let s:validHeaders   = 'Newsgroups:\|From:\|To:\|Cc:\|Bcc:\|Reply-To:\|Subject:\|Return-Path:\|Received:\|Date:\|Replied:'
let s:name	     = substitute('\v%([[:upper:]]\. )?[[:upper:]][[:lower:]]{2,}%(-[[:upper:]][[:lower:]]{2,})?%( x[[:upper:]][[:lower:]]+x)?%( [[:upper:]][[:lower:]]?\.)?%( %([[:upper:]]x|[[:upper:]][[:lower:]])?[[:upper:]][[:lower:]]+%(\-[[:upper:]][[:lower:]]+)?)?\m', 'x', "'", 'g')
let s:host	     = '%(%(\[%(\d{1,3}\.){3})|%(%([a-zA-Z0-9-]+\.)+))%(\a{2,4}|\d{1,3})\]?'
let s:email	     = '\v[< (]@<=[0-9A-Za-z_-]+%(\.[0-9A-Za-z_-]+)*\@' . s:host . '\_[> )]@=\m'
let s:verb	     = '\%(wrote\|writes\|said\|says\|stated\|states\|typed\|opinion\|schrieb\|schreibt\|geschrieben\|scripsit\|écrit\)'
let s:year	     = strftime("%Y")
let s:repLedPrt	     = '%( %(article|message)%( [<n].*)?|news:.+|" \<.*|[<(]\S+\@.*|\@\S+[>)].*|\@\S+ .*[("].*|%(\_^.*%(Sent:|Date:).*)@<!%( [1-3]?\d \u\l\l%( .*)?|[+\-][01]\d00(\D.*)?| ' . s:year . '%([, ].*)?)|%(\@|\m' . s:verb . '\v).*%(:|\.{3})|%([^> ]\>|\@).*\m' . s:verb . '\v)\s*\_$'
let s:repLed	     = '\%(' . s:name . '\|' . s:email . '\|' . s:verb . '\).*[a-z>]\@<=\%(:\|\.\.\.\)\_$'
let s:spaces	     = '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        '
let s:guiSup	     = has('gui')


function! s:SaveSett(...)
  let cmd = ''
  let i   = 1
  while i <= a:0
    exe 'let val = &l:' . a:{i}
    let cmd = cmd . (i > 1 ? ' | ' : '') . 'let &l:' . a:{i} . '="' . escape(val, '"') . '"'
    let i = i + 1
  endwhile
  return "let restSett = '" . cmd . "'"
endfunction

function! s:InstDoc()
  function! s:MakeDoc()
    % foldopen!
    1 d _ | 1/^"·/,$ d _
    % s/^\%("let \)\@!" \?//
    call setline(1, "*posting.txt*\tStand-alone USENET/e-mail environment (v1.3)")
    setl fo& tw=68 et sw=4 nojs
    let n = 1
    $?Settings {{?/^"\?let/;/^\n\%("\?let \)\@!/-1 g/^"\?let/ s/^\("\?\)let s:\(\w\+\) *= *\(.*\)$/\=n . (n > 9 ? ') ' : ')  ') . submatch(2) . (submatch(1) == '' ? ' = ' . submatch(3) : ' (disabled)')/
    \ | let n = n + 1
    \ | norm! jV/^$/-gqV'[>
    % g/{{{\|}}}\|=\{10,}/d
  endfunction
  unlet! s:helpDoc
  let pluginFile = s:vimPluginPath . '/posting.vim'
  let docFile = s:vim_doc_path . '/posting.txt'
  if !isdirectory(s:vimPluginPath) || !isdirectory(s:vim_doc_path)
  \ || filewritable(s:vim_doc_path) != 2
  \ || bufnr(pluginFile) != -1
  \ || filereadable(s:vimPluginPath . '/.posting.vim.swp')
  \ || (filereadable(docFile) && getftime(pluginFile) < getftime(docFile))
    return
  endif
  let restBuf = (strlen(@%) ? 'b ' . bufnr("%") : 'enew')
  exe s:SaveSett('ml', 'swf', 'ma', 'bk', 'fo', 'tw', 'et', 'sw', 'js', 'ei')
  setl noml noswf ma nobk ei=all
  exe 'e ' . pluginFile
  call s:MakeDoc()
  exe 'w! ' . docFile
  let s:helpDoc = 1
  bw
  exe restBuf . ' | ' . restSett
  exe 'helptags ' . s:vim_doc_path
endfunction

function! s:Val(name, valEsc, valCr, ...)
  cno <buffer> <Esc> <C-E><C-U>-<Cr>
  cno <buffer> <C-p> <C-R>*
  exe 'cnoreabb <buffer> no ' . (a:0 > 0 ? 'no' : '-')
  echohl Question | exe 'let rep = input(a:name . "? "' . (a:0 > 0 ? ', a:1' : ', "yes"') . ')' | echohl None
  cu  <buffer> <Esc>
  cu  <buffer> <C-p>
  cunabb <buffer> no
  if rep == '-'
    let rep = a:valEsc
  else
    exe 'let rep = a:valCr' . (a:0 > 0 ? ((a:valEsc[0] == '/' || a:valCr[0] == '/') ? '. escape(rep, "/ ")' : '. rep') : '')
  endif
  return rep
endfunction

function! s:Frame(act)
  if a:act == "save"
    unlet! s:header s:footer
    exe 'sil! 1,5 g/^\%(' . s:validHeaders . '\)/let s:header = 1'
    if exists("s:header")
      sil! 1;/^$/ d x
    endif
    if search('^\~\~$', 'w') > 0
      sil! .,$ d y
      let s:footer = 1
    endif
  else
    if exists("s:header")
      sil! 1 put! x
    endif
    if exists("s:footer")
      sil! $ put y
    endif
  endif
endfunction

function! s:Top()
  sil! norm! gg
endfunction


function! s:TrimParQuo()
  function! s:RemLay() range
    exe a:firstline . ',' . a:lastline . ' s/^> \?//e'
  endfunction
  function! s:InsLay() range
    exe a:firstline . ',' . a:lastline . ' s/^>\@=/\=s:tpqQuote/e'
    exe a:firstline . ',' . a:lastline . ' s/^>\@!/> /e'
    exe (a:lastline + 1) . ' s/.//'
  endfunction
  let top = line(".")
  sil! +/^·*$/ s/^/·/
  exe 'sil! ' . top . '+;/^·/- call s:RemLay()'
  if s:tpqRepair == 'man'
    exe top . '+;/^·/- s/\v%(\_^\>.*)@<=\n%([^>·]%(\w*\W{,2}){,3}\n|%<' . (s:repLedMax + 1) . 'l%(news:.*|\>@!.*\@.*[>(]))@=/ /ce'
  elseif s:tpqRepair == 'auto'
    exe 'sil! ' . top . '+;/^·/- g/\v^\>.*\n%([^>·]%(\w*\W{,2}){,3}\n|%<' . (s:repLedMax + 1) . 'l%(news:.*|\>@!.*\@.*[>(]))/ join'
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
    .- call s:TrimParQuo()
    -
  endwhile
  exe 'sil! ' . top . '+;/^·/- call s:InsLay()'
endfunction

function! s:TrimQuo()
  let top = search('^$', 'bW')
  if exists("s:tqInitials")
    let v:errmsg = ''
    while v:errmsg == ''
      exe 'sil! ' . top . '+;/^$/- s/\v^%(\> ?)*\zs\u{1,3} ?\ze\>//'
    endwhile
  endif
  /^$/ call setline('.', '·')
  exe 'sil! ' . top . '+,.- s/^\_[>[:blank:]]*$/>/'
  /^·/ call setline('.', '')
  exe 'sil! ' . top . '+ g/^>[>[:blank:]]*$/d'
  exe top . 'call s:TrimParQuo()'
endfunction

function! s:Join()
  let @W  = 'L' . getline('.')[0] . '[0]: ' . getline('.') . "\n"
  let prt = '\v^' . getline('.')[0] . '\>%(\s*\_$|.*' . s:repLedPrt . ')'
  let n	  = line('.') + 1
  let i	  = 1
  while i < 4 && getline(n) =~ prt
    let @W = 'L' . getline(n)[0] . '[' . i . ']: ' . getline(n) . "\n"
    sil! s/\n../ /
    let i = i + 1
  endwhile
  sil! s/\w-\zs\s\+\ze\w\|\s*\_$\| \zs\s\+//g
  let s:joinedLines = i
endfunction

function! s:Unify(act)
  let midPat  = '\%(In \| in \| article \| message \| news:\)[^"(,@[:blank:]]\+@[^,[:blank:]]\+'
  let namePat = '1:"\zs[^"]\+\ze"#
		\2:(\zs[ [:alpha:].\-_]\+\ze)#
		\3:\%(\%3v\| \)\zs' . s:name . '\ze\%( \S\+@\S\+\| ' . s:verb . '\|:\)#
		\4:, \zs' . s:name . '\%( \d\)\@!#
		\5:, \zs[^,<]\+\ze <#
		\6:, \zs[^,@]\+\ze ' . s:verb . '#
		\7:\%3v\%(\* \)\?\zs[^,<>]\+\ze <#
		\8:\%(\%3v\%(\* \)\?\| \)\zs\%([^, ]\+ \)\{,2}[^, ]\+\ze ' . s:verb . '#
		\9:' . s:verb . ' \zs' . s:name . '#
		\10: \zs\u\{3,}\ze[,: ]#'
  function! s:Matchstr(str, patList)
    let n = 1
    let pat = matchstr(a:patList, '1:\zs.\{-}\ze#')
    while pat != ''
      let res = matchstr(a:str, pat)
      if res != '' | break | endif
      let n = n + 1
      let pat = matchstr(a:patList, n . ':\zs.\{-}\ze#')
    endwhile
    return res
  endfunction
  call s:Join()
  let ln = getline('.')
  let idPrt = substitute(ln, midPat, '', '')
  let name  = s:Matchstr(idPrt, namePat)
  if name != ''
    let @W = 'N' . ln[0] . ': ' . name . "\n"
    let s:changedName = 1
  endif
  if (strlen(ln) > s:tw && a:act ==? 'check') || a:act ==? 'force'
    let add = matchstr(idPrt, s:email)
    if name != '' || add != ''
      call setline('.', substitute(strpart(ln, 0, 2)
			\ . (name != '' ? '"' . name . '" ' : '')
			\ . (add  != '' ? '<' . add . '> ' : '')
			\ . 'wrote:•', ' \{2,}', ' ', 'g'))
    else
      call setline('.', ln . '•')
    endif
  else
    call setline('.', ln . '•')
  endif
endfunction

function! s:MaxBufLev()
  exe s:SaveView()
  let n = 0 | sil! g/^\x>/ if ('0x' . getline('.')[0]) + 0 > n | let n = ('0x' . getline('.')[0]) + 0 | endif
  exe restView
  return n
endfunction

function! s:ConWin(act, ...)
  let s:widthMax = 20
  function! s:MkNames()
    let line   = '________________________________________'
    let lenMax = 0
    let i = 1
    while i <= b:levMax
      let s:name{i} = s:SMax(matchstr(@w, 'N' . '0123456789abcdef'[i - 1] . ": \\zs[^\n]*\\ze\n"), '?') . ' ' . 'ABCDEFGHIJKLMNOP'[b:levMax - i] . ':'
      let lenMax    = s:Max(strlen(s:name{i}), lenMax)
      let i = i + 1
    endwhile
    let s:width = s:Min(lenMax, s:widthMax)
    let i = 1
    while i <= b:levMax
      let len = strlen(s:name{i})
      let s:name{i} = (len <= s:width) 
	\? strpart(s:spaces, 0, s:width - len) . s:name{i} 
	\: '<' . strpart(s:name{i}, len - s:width + 1)
      let i = i + 1
    endwhile
    let s:line = strpart(line, 0, s:width)
  endfunction
  function! s:MkList(top, bot)
    let offset	  = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    let n	  = s:Max(a:top, 3)
    let @u	  = strpart(offset, 0, n - 1)
    let bot	  = s:Min(a:bot, line('$'))
    let lev0	  = 0
    let s:width0  = winwidth(0)
    function! s:Len(len)
      let p = a:len * s:width
      return p / s:width0 + (p % s:width0 > 0)
    endfunction
    if exists("s:changedName")
      call s:MkNames() | unlet s:changedName
    endif
    while n <= bot
      if getline(n) =~ '^0\@!\x>.*\S.*•\@<!$'
      \ && (getline(n - 1) =~ '^\%(\x>\)\@!' || (getline(n - 1) =~ '\%3c\s*$'
      \ && (getline(n - 2) =~ '^\x>'
      \ && (getline(n - 2)[0] != getline(n)[0] || getline(n - 2) =~ '•$'))))
	let lev = ('0x' . getline(n)[0]) + 0
	let @u = (lev > lev0) 
	  \? substitute(@u, ' *.$', s:line . "\n" . strpart(s:name{lev} . s:spaces, 0, s:Max(s:Len(strlen(getline(n))), s:width)) . "\n", '') 
	  \: @u . strpart(s:name{lev} . s:spaces, 0, s:Max(s:Len(strlen(getline(n))), s:width)) . "\n"
	let lev0 = lev
      else
        let @U = (strlen(getline(n)) > s:width0 ? strpart(s:spaces, 0, s:Len(strlen(getline(n)))) : '') . "\n"
      endif
      let n = n + 1	    
    endwhile
  endfunction
  function! s:MkBuf(top)
    sil $ put u
    sil 1,.- d _
    exe 'norm! ' . a:top . 'zt'
  endfunction
  if exists("s:fold") | return | endif
  if a:act == 'on'
    if !exists("s:authBuf")
      if !exists("b:levMax")
	let b:levMax = s:MaxBufLev()
      endif
      if b:levMax > 0
	let wl = winline()
      	sil! call s:MkList(1, line('$'))
      	setl wiw=1 sbo=ver,jump scb so=0
	let top = s:WTop()
      	let s:co0 = &co | let &co = &co + s:width + 1
	let wrap = &l:wrap
      	exe s:width . ' vnew CT'
      	setl scb bt=nofile bh=hide noswf
	let &l:wrap = wrap
	syn match postingLetter '\u\ze:'
	hi postingLetter gui=bold
	call s:MkBuf(top)
      	let s:authBuf = bufnr('%')
      	wincmd l
      	call s:SetCur(wl)
	let b:stlLevels = strpart('ABCDEFGHIJKLMNOP', 0, b:levMax)
	let &so = s:so
	set ut=1000
	exe 'au CursorHold ' . escape(substitute(expand("%:p"), '\', '/', 'g'), '[,{ ') . ' sil! call <SID>ConWin("update")'
      	syncbind
      endif
    endif
  elseif a:act == 'off'
    if exists("s:authBuf")
      exe 'bw! ' . s:authBuf
      let &co = s:co0
      unlet s:authBuf b:stlLevels
      exe 'au! CursorHold ' . escape(substitute(expand("%:p"), '\', '/', 'g'), '[,{ ')
    endif
  elseif a:act == 'update'
    if exists("s:authBuf") && bufnr('%') == s:mailBuf
      if !exists("b:levMax")
	let b:levMax = s:MaxBufLev()
      endif
      if b:levMax > 0
      	set so=0
      	if a:0 > 0
      	  sil! call s:MkList(a:1, a:2)
      	  wincmd h
      	  call s:MkBuf(s:WTop())
      	  wincmd l
      	else
      	  sil! call s:MkList(1, line('$'))
      	  let width0 = winwidth(1)
      	  wincmd h
      	  call s:MkBuf(s:WTop())
      	  if s:width != width0
      	    exe 'vertical resize ' . s:width
      	    let &co = &co - (width0 - s:width)
      	  endif
      	  wincmd l
      	endif
      	let &so = s:so
      else
	call s:ConWin('off')
      endif
    endif
  else
    call s:ConWin(exists("s:authBuf") ? 'off' : 'on')
  endif
endfunction

function! s:TrimAtt()
  function! s:Strip() range
    if (a:lastline - a:firstline) > 2
      exe 'sil! ' . a:firstline . ',' . a:lastline . ' s/^0>\%(.*•$\)\@!//'
    endif
  endfunction
  sil! 1;/^\x>/- s/^\%(\x>\)\@!/0>/
  let cnt    = 1
  let i	     = 1
  let next   = 0
  let levels = '0123456789abcde'
  while i <= line('$') && cnt <= s:repLedMax
    if getline(i) =~ '^[' . levels . ']>.*\v' . s:repLedPrt
      exe i
      let levels = substitute(levels, getline('.')[0], '', '')
      call s:Unify(s:attrib_default_action)
      let next = i + 1
      let cnt = cnt + s:joinedLines
    else
      if i == next && getline(i) =~ '^\x>'
        call s:DSep(i - 1)
        let i = i + s:appendedLines
      endif
      let cnt = cnt + 1
    endif
    let i = i + 1
  endwhile
  if i == next && getline(i) =~ '^\x>'
    call s:DSep(i - 1)
    let i = i + 1
  endif
  sil! 1;/^\%(0>\)\@!/- call s:Strip()
  let s:repLedMax = next + 4
endfunction

function! s:Digiquote(act) range
  if a:act == 'on'
    exe 'sil! ' . a:firstline . ',' . a:lastline . ' s/\v^(\>+) ?/\="0123456789abcdef"[strlen(submatch(1))] . ">"/'
    let s:digiquote = 1
  else
    exe 'sil! ' . a:firstline . ',' . a:lastline . ' s/\v^(\x)\>/\=strpart(">>>>>>>>>>>>>>> ", 15 - ("0x" . submatch(1)) + (submatch(1) == "0"))/'
    unlet! s:digiquote
  endif
endfunction

function! s:SetTitle(...)
  let &title = 1
  let &titlestring = ' ' . (a:0 == 0 ? s:SMax(matchstr(@w, "N0: \\zs[^\n]*\\ze\n"), '?') : a:1)
  let &titlelen = winwidth(0) - 20
endfunction

function! s:Clean(...)
  let leadins = '-- $|DISCLAIMER|Disclaimer|\*{10,}.*[Cc]onfidential'
  if a:0 > 0 | exe s:SaveView('mark') | endif
  exe 'sil! g/\v^%(\x\>\s*\n)?\x\>%(' . leadins . ')/ .;/\v^(\x\>).*%(\n\1\s*)?%(\n\1@!|%$)/d _'
  if a:0 > 0 | exe restView | endif
  call s:ConWin('update')
endfunction

function! s:DoInitForm()
  call s:Frame('save')
  sil! g/^>.*\n[^>]/ put _
  sil! g/^[^>].*\n>/ put _
  sil! $ put _ | sil! 1 put! _
  match Ignore /·/
  while search('^>', 'W') > 0
    call s:TrimQuo()
  endwhile
  match none
  sil! 1;/./- v/./d _
  sil! $?.?+,$ v/./d _
  sil! %s/^\_[[:blank:]]*$//e
  % call s:Digiquote('on')
  call s:TrimAtt()
  if s:rem_trailers_on_startup == 1 | call s:Clean() | endif
  call s:Frame('restore')
endfunction

function! s:Min(a, b)
  return (a:a > a:b ? a:b : a:a)
endfunction

function! s:Max(a, b)
  return (a:a > a:b ? a:a : a:b)
endfunction

function! s:SMin(a, b)
  return (strlen(a:a) > strlen(a:b) ? a:b : a:a)
endfunction

function! s:SMax(a, b)
  return (strlen(a:a) > strlen(a:b) ? a:a : a:b)
endfunction

function! s:Columns()
  let maxCols = s:cols + 10
  let max = 1 | sil! g/^\x>.*•\@<!$/ if col('$') > max | let max = col('$') | endif
  let max = max - 1
  return (max > s:cols ? s:Min(max, maxCols) : s:cols)
endfunction

function! s:Paste(reg, form, ...)
  exe 'if col("$") > 1 || @' . a:reg . ' == "' . (a:reg == '*' ? '\n' : '') . '" | return | endif'
  ma j | set paste | exe 'sil put! =\"\n\" . @' . a:reg | set nopaste | ma i
  setl et
  if a:form != 'nowrap'
    let fo0 = &l:fo | let &l:fo = 'tqn1'
    let &l:tw = s:tw - (a:0 > 0 ? a:1 : 2)
    sil 'i,'j s/^[>|[:blank:]]*\|\s*$//g
    if a:form == 'wrap_dot'
      sil 'i/./ s/^./... &/e
      sil 'j?.? s/.$/& .../e
      norm! 'igq'j
      let n = 3
      'j?.?
      while n > 0 && getline('.') =~ '^ *\.\{3} *$'
        let &l:tw = &l:tw - 1
        norm! 'igq'j
        let n = n - 1
        'j?.?
      endwhile
    else
      norm! 'igq'j
    endif
    let &l:fo = fo0
    let &l:tw = 0
  else
    setl ts=8 | 'i,'j retab 2
  endif
  sil 'i;/./- d | ma i
  sil 'j- v/./ ?.?+,. d
  if a:0 > 0 && a:1 > 0
    let &l:sw = a:1 | sil 'i,'j- > | setl sw&
  elseif a:0 == 0
    sil 'i,'j- s/^/\='| '/
  endif
  'j
endfunction

function! s:Lev(n)
  return strlen(matchstr(getline(a:n), '^>*'))
endfunction

function! s:DLev(n)
  let lev = matchstr(getline(a:n), '^\x>\@=')
  return (lev == '' ? 0 : ('0x' . lev) + 0)
endfunction

function! s:Down(n)
  return (a:n > 0 ? a:n . 'j' : '')
endfunction

function! s:Shft(n) range
  let sw0=&sw | let &l:sw=a:n
  exe a:firstline . ',' . a:lastline . ' s/ *$//'
  exe a:firstline . ',' . a:lastline . ' >'
  let &l:sw=sw0
endfunction

function! s:GQ(type, fo, ...) range
  function! s:Fmt()
    norm! gq'x
    let n = line('.')
    if n > line("'x")
     'x,.- s/.\zs$/ /e
    endif
    exe (n + 1) . 'ma x'
  endfunction
  exe s:SaveSett('et', 'fo')
  exe 'setl et fo=' . a:fo
  if a:type == 'txt'
    if a:0 > 0
      exe a:firstline
      while line('.') <= a:lastline
	if searchpair("^{TB$", "", "^}$", "n") == 0
	  let b:stlManFmt = 1 | break
	endif
	+
      endwhile
    endif
    let &l:tw = s:tw
    if s:format_flowed == 1
      exe a:firstline . 'ma x'
      exe 'sil! ' . a:lastline . ' put _' | ma y
      exe 'sil! ' . a:firstline . ",'y-" . ' g/\S$/ call s:Fmt()'
      call setline("'y", '·')
      exe 'sil! ' . a:firstline . ",'y-" . ' g/^[0-9-].*\n / .,.-/\S$/ call s:Shft(1)'
      exe 'sil! ' . a:firstline . ",'y-" . ' s/^\%(>\|From \)\@=/ /'
      sil! 'y- s/\n.*//
    else
      exe 'sil! ' . a:firstline . 'norm! V' . s:Down(a:lastline - a:firstline) . 'gq'
    endif
  else
    exe 'sil ' . a:lastline . ' put _' | ma y
    exe a:firstline
    while line('.') < line("'y")
      let lev = '0x' . getline('.')[0] + 0
      let &l:tw = s:tw - (lev > 0 ? lev + 1 : 0)
      exe 'norm! 0-/\v^(\x\>).*%(\n\1@!|%$)2|"_d' . "gq']0m''[0I" . getline('.')[0] . ">''+"
    endwhile
    sil 'y- s/\n//
  endif
  let &l:tw = 0
  exe restSett
endfunction

function! s:RefQuo(form, ...) range
  let top = a:firstline
  let bot = a:lastline
  if a:form == 'nowrap' || a:form == 'rebuild'
    exe bot
    sil . g/\n\x>/ +?^\x>.*\S?
    sil put _
    exe top
    sil . g/\%(^\x>.*\n\)\@<=\_^/ -/^\x>.*\S/
    sil put! _
    norm! mzjvip:call s:Digiquote("off")'z+
    call s:TrimQuo()
    let bot = line('.') - 2 | sil d
    sil '{ d | let top = line(".")
    exe top . ',' . bot . 'call s:Digiquote("on")'
  endif
  if a:form == 'wrap' || a:form == 'rebuild'
    exe 'sil ' . top . ',' . bot . 'call s:GQ("quo", (a:0 > 0 ? a:1 : "tqn1"))'
  endif
endfunction

function! s:Hl(act, ...)
  if a:act == 'on'
    if exists("s:hl")
      sil! syn clear postingVis postingVisTop postingVisMid postingVisBot
    else
      exe 'hi link postingVisTop ' . a:5
      exe 'hi link postingVisMid ' . a:5
      exe 'hi link postingVisBot ' . a:5
    endif
    exe 'syn match postingVis transparent "\%>' . (a:1 - 1) . 'l\%<' . (a:3 + 1) . 'l\%>2v.*$" containedin=postingQuoted[1234] contains=postingVisTop,postingVisMid,postingVisBot'
    if a:3 == a:1
      exe 'syn match postingVisTop "\%' . a:1 . 'l\%' . a:2 . 'v.*\%' . (a:4 + 1) . 'v" contained'
    else
      exe 'syn match postingVisTop "\%' . a:1 . 'l\%' . a:2 . 'v.*" contained'
      exe 'syn match postingVisBot "\%' . a:3 . 'l.*\%' . (a:4 + 1) . 'v" contained'
    endif
    exe 'syn match postingVisMid "\%>' . a:1 . 'l\%<' . a:3 . 'l.*" contained'
    call cursor(a:3, 1)
    redraw
    let s:hl = 1
  else
    sil! syn clear postingVis postingVisTop postingVisMid postingVisBot
    hi link postingVisTop NONE
    hi link postingVisMid NONE
    hi link postingVisBot NONE
    unlet! s:hl
  endif
endfunction

function! s:VisChar()
  let l1 = line('.')
  let wtop = line('.') - winline() + 1
  let c1 = 3
  let l2 = l1
  let c2 = strlen(getline(l2))
  let mv = ''
  while 1
    norm ,qe2
    call s:Hl('on', l1, c1, l2, c2, 'postingVisChar')
    let ch = getchar()
    if (ch == 113 || ch == "\<Right>") && c1 < strlen(getline(l1)) && mv !~ '[dl]'
      let c1 = c1 + 1
    elseif (ch == 43 || ch == "\<Down>") && l2 < line("'v") && mv !~ 'l'
      let l2 = l2 + 1
      let c2 = strlen(getline(l2))
      let mv = mv . 'd'
    elseif (ch == "\<Bs>" || ch == "\<Left>") && c2 > 3
      let c2 = c2 - 1
      let mv = mv . 'l'
    elseif ch == 97
      call s:Hl('off')
      exe l1 . ' norm! ' . c1 . '|v' . s:Down(l2 - l1) . c2 . '|v'
      call s:Abridge('wrap', 'v')
      exe "norm! m'" . wtop . "zt`'"
      break
    elseif ch == 27
      call s:Hl('off')
      break
    endif
    call s:RestCur(winline(), 'top')
  endwhile
endfunction

function! s:SaveView(...)
  norm! mr
  return 'let restView = "' . (a:0 == 0 ? 'call cursor(' . line('.') . ',' . virtcol('.') . ') | ' : 'sil! norm! `r:') . 'call s:SetCur(' . winline() . ')' . (a:0 == 0 ? '"' : '"')
endfunction

function! s:Scan() range
  function! s:UpdConWin()
    if line("'w") != s:bot
      let s:bot = line("'w")
      call s:ConWin('update', s:top - winheight(0), line("'w") + winheight(0))
      let s:update = 1
    endif
  endfunction
  exe s:SaveSett('ve', 'gcr', 'so', 'ws', 'sc', 'smd')
  setl ve=all gcr=v-n:ver1-blinkon0-Normal so=0 nows nosc nosmd
  let wrap = &l:wrap | call s:Setl('nowrap')
  exe a:lastline . 'ma v'
  exe a:firstline . '-/^\x>.*\S/'
  let s:top = line('.')
  exe (a:lastline + 1) . 'ma w' | let s:bot = line("'w")
  unlet! s:update
  exe 'norm! ' . s:Min(s:Max(s:top - s:so, 1), s:wtop) . 'zt' . s:top . 'G'
  norm ,qB
  while line('.') <= line("'v")
    if exists("move") | call s:RestCur(winline(), 'top') | endif
    syncbind
    norm ,qe1gv
    redraw
    let ch = getchar()
    if (ch == 110 || ch == 9 || ch == 32) && line('.') < line("'v")
      norm ,qj,qB
    elseif ch == 78 && line('.') < line("'v")
      exe 'sil! norm $/\%(\_^\x>\s*\n\)\@<=\%<' . (line("'v") + 1) . 'l\x>,qB'
    elseif (ch == 112 || ch == "\<S-Tab>" || ch == "\<Bs>") && line('.') > s:top
      norm ,qk,qB
    elseif ch == 80 && line('.') > s:top
      exe 'sil! norm 0?\%(\_^\x>\s*\n\|\_^\%(\x>\)\@!.*\n\|\%^\)\@<=\%>' . (s:top - 1) . 'l\_^\x>,qB'
    elseif (ch == 43 || ch == "\<Down>") && line('.') < line("'v")
      norm ,qj
    elseif ch == 106
      sil! norm :call s:RefQuo('wrap'),qj,qB
      call s:UpdConWin()
    elseif ch == 46
      sil! norm Vj:call s:RefQuo('wrap', 'tq12'):.g/^\x>\s*$/+,qB
      call s:UpdConWin()
    elseif ch == 13
      if line("'>") > line("'<")
	sil! norm :call s:RefQuo('wrap'),qj,qB
      else
	sil! norm :.,.-/^.*\n\%(\x>\)\?\s*$/ call s:RefQuo('wrap'),qj,qB
      endif
      call s:UpdConWin()
    elseif ch == "\<C-Cr>"
      sil! norm V'v:call s:RefQuo('wrap'),qB
      call s:UpdConWin()
      break
    elseif ch == 97
      norm! VV
      call s:Abridge('nowrap', 'v')
      norm ,qj,qB
      call s:UpdConWin()
    elseif nr2char(ch) =~ '[1-9A-E]'
      norm! 
      exe s:SaveView()
      norm! V'vV
      call s:Abridge('nowrap', 'v', '0x' . nr2char(ch) + 0)
      exe restView
      norm ,qB
      call s:UpdConWin()
    elseif ch == 99
      norm! 
      exe s:SaveView()
      exe 'sil! ' . ".,'v" . ' call s:DExtQuo(14)'
      exe restView
      norm ,qB
      call s:UpdConWin()
    elseif ch == 100
      norm '<d'>,qB
      call s:UpdConWin()
    elseif ch == 104
      norm! 0
      exe s:SaveView()
      let &hh = 28 | sil! h posting-scanmode | redraw
      echohl MoreMsg | echo 'Hit ENTER to continue' | echohl None
      call getchar()
      q | set hh&
      exe restView
      redraw
    elseif ch == 117
      norm! u
      norm ,qB
      redraw
    elseif (ch == 113 || ch == "\<Right>") && col('$') > 3
      norm! 
      call s:VisChar()
      norm ,qj,qB
      call s:UpdConWin()
    elseif ch == 27
      norm! 
      break
    else
      norm! 
    endif
    let move = 1
    let g:ch = ch
  endwhile
  if line("'w") != s:bot || exists("s:update") | call s:ConWin('update') | endif
  norm ,qe0
  exe restSett
  call s:Setl(wrap == 1 ? 'wrap' : 'nowrap')
endfunction

function! s:ABot(top)
  exe (a:top - 1)
  return search('^>.*\%(\n>\@!\|\%' . s:repLedMax . 'l\|\%$\)', 'W')
endfunction

function! s:DABot(top)
  exe (a:top - 1)
  return search('^\x>.*\%(\n\%(\x>\)\@!\|\%' . s:repLedMax . 'l\|\%$\)', 'W')
endfunction

function! s:QTop(line, min)
  if a:line <= s:repLedMax
    exe (s:DABot(a:line) + 1)
  else
    exe (a:line + 1)
  endif
  return search('\%(•\n\|\_^\%(\x>\)\@!.*\n\|\%^\)\@<=\_^\x>\|\%' . a:min . 'l\_^', 'bW')
endfunction

function! s:QBot(line)
  exe (a:line - 1)
  return search('^\x>.*\%(\n\%(\x>\)\@!\|\%$\)', 'W')
endfunction

function! s:DQCTop(line, min)
  let bot0 = s:QBot(a:line)
  let lev0 = s:DLev(bot0)
  let bot1 = bot0 - 1
  let lev1 = s:DLev(bot1)
  while lev1 >= lev0 && bot1 >= a:min
    let bot0 = bot1
    let lev0 = lev1
    let bot1 = bot0 - 1
    let lev1 = s:DLev(bot1)
  endwhile
  return bot0
endfunction

function! s:DQLBot(line)
  exe (a:line - 1)
  return search('^\(\x>\).*\%(\n\1\@!\|\%$\)', 'W')
endfunction

function! s:TSetRange()
  ma ' | +
  let top = search('\%(\_^{TB\n\|\_^{BX.*\n\|\_^}\n\|\_^\x>.*\n\|\_^\n\|\%^\)\@<=\_^', 'bW') | ''-
  let bot = search('\_^.*\%(\%$\|\n\_$\|\n{TB\_$\|\n{BX\|\n}\_$\|\n\x>\)\@=', 'W')
  return 'let top = ' . top . ' | let bot = ' . bot
endfunction

function! s:Justify(mode, ...) range
  exe s:SaveSett('smd', 'sc')
  set nosmd nosc
  let fo = (a:0 > 0 ? a:1 : 'tqn1')
  if a:mode == 'v'
    exe 'sil ' . a:lastline . ' put _' | ma w
    exe a:firstline
    while line('.') < line("'w")
      if getline('.') =~ '^\x>'
	sil! .;-/\v^\x\>.*%(\n%(\x\>)@!|%$)/ call s:RefQuo('rebuild', fo)
      else
	exe 'sil! .;-/\v\n\x\>|%' . (line("'w") - 1) . 'l/ call s:GQ("txt", fo, "chk")'
      endif
      +
    endwhile
    sil 'w- s/\n//
  else
    if getline('.') =~ '^\x>'
      let n = line('.')
      exe 'sil ' . s:QTop(n, 1) . ',' . s:QBot(n) . ' call s:RefQuo("rebuild", fo)'
    else
      exe s:TSetRange()
      exe 'sil ' . top . ',' . bot . ' call s:GQ("txt", fo, "chk")'
    endif
  endif
  exe restSett
endfunction

function! s:Sep(line)
  if getline(a:line + 1) =~ '[^> ]' | call append(a:line, '') | endif
  let s1 = matchstr(getline(a:line), '^\%(> \?\)*')
  let s2 = matchstr(getline(a:line + 2), '^\%(> \?\)*')
  call setline(a:line + 1, s:SMin(s1, s2))
endfunction

function! s:DSep(line)
  if getline(a:line + 1) =~ '^\x>.*\S'
    call append(a:line, '')
    let s:appendedLines = 1
  else
    let s:appendedLines = 0
  endif
  call setline(a:line + 1, '0123456789abcdef'[s:Min(s:DLev(a:line), s:DLev(a:line + 2))] . '>')
endfunction

function! s:DExtQuo(max) range
  exe 'sil ' . a:firstline . ' put! =\"f>\"'
  let l1 = 0
  let i  = a:lastline + 1
  let l2 = '0x' . getline(i)[0] + 0
  while l2 <= a:max && l2 >= l1
    let l1 = l2
    let i  = i - 1
    let l2 = '0x' . getline(i)[0] + 0
  endwhile
  exe 'sil ' . a:firstline . ',' . i . '-/\%3v\s*$\|\%' . (a:lastline + 1) . 'l/ d _'
endfunction

function! s:SetCur(wl)
  let d = winline() - a:wl
  if d > 0
    exe 'norm! ' . d . ''
  elseif d < 0
    exe 'norm! ' . (-d) . ''
  endif
  syncbind
endfunction

function! s:RestCur(wl, ...)
  let top = s:so + 1
  if a:wl < top
    call s:SetCur(top)
  elseif a:wl < winheight(0) - top - 3
    call s:SetCur(a:wl)
  else
    if exists("a:1")
      call s:SetCur(top)
    else
      norm! z.
    endif
  endif
endfunction

function! s:Abridge(form, mode, ...) range
  function! s:Mark(text)
    exe "'h" . ' s/\%' . s:Max(col("'h"), 3) . 'c.//e'
    exe "'l" . ' s/\%' . s:Max(col("'l"), 3) . 'c.\zs//e'
    'h,'l s/\zs\_.*\ze/\=a:text/ge
    let s:mark = 1
  endfunction
  function! s:Replace()
    function! s:Request()
      if col("'h") <= 3 && col("'l") == strlen(getline("'l"))
        set ve=all
        norm! 'h3|'l3|g$
        redraw
      	let str = s:Val('comment', 'abort', '', '...')
      	norm! 
        set ve=
      else
        call s:Hl('on', line("'h"), s:Max(col("'h"), 3), line("'l"), s:Max(col("'l"), 3), 'postingVisChar')
      	let str = s:Val('comment', 'abort', '', '...')
        call s:Hl('off')
      endif
      if str != 'abort' | sil! call s:Mark(str) | endif
    endfunction
    sil 'h g/^\x>\s*$/ /^\x>.*\S\|\%$/ ma h
    sil 'l g/^\x>\s*$/ ?^\x>.*\S\|\%^? norm! $ml
    if line("'h") > line("'l") | return | endif
    sil norm! `hv`ly
    if match(@", "\\%(\\%(\\_^\\|\n\\)\\@<=\\x>[^\n]*\\)\\?[^[:blank:]\n]") > -1 | let text = 1 | endif
    sil 'h,'l g/•$/ let leadin = 1
    if exists("text") && !exists("leadin")
      if exists("s:minLev")
	if s:DLev(line("'h")) >= s:minLev | sil! call s:Mark('...') | endif
      else
	call s:Request()
      endif
    endif
  endfunction
  exe s:SaveSett('smd', 'sc', 've', 'so')
  exe 'set nosmd nosc ve= so=' . s:so
  norm! zn
  if a:mode == 'n'
    let fl = line('.')
    if getline(fl) !~ '^\x>' | return | endif
    exe s:QTop(fl, 1) . 'ma a'
    exe s:QBot(fl) . 'norm! $mb'
  else
    let fl = line("'<")
    sil '<,'> v/^\x>/let noquo = 1
    if exists("noquo") | exe fl | return | endif
    norm! `<ma`>mb
  endif
  unlet! s:minLev
  if exists("a:1") && a:1 > 0 | let s:minLev = a:1 + 1 | endif
  let bot = s:DQLBot(line("'a"))
  norm! `amh
  while bot < line("'b")
    exe bot . 'norm! $ml0jma'
    call s:Replace()
    let bot = s:DQLBot(line("'a"))
    norm! 'amh
  endwhile 
  norm! `bml
  call s:Replace()
  if exists("s:mark")
    unlet s:mark
    if a:form == 'wrap'
      sil! 1-//+?\%(\%^\|\_^\%(\x>\)\@!.*\n\|\_^\x>\s*\n\)\@<=\_^\x>?,$+??-/^\x>.*\%(\n\x>\s*\_$\|\n\%(\x>\)\@!\|\%$\)/ call s:RefQuo(a:form)
    endif
    sil! 1-//,$+?? s/\(\_[^]*\)/[\1]/ge
  else
    exe fl
  endif
  call s:ConWin('update')
  exe restSett
  norm! zX
endfunction

function! s:WTop()
  exe s:SaveSett('wrap')
  setl nowrap
  let n = line('.') - winline() + 1
  exe restSett
  return n
endfunction

function! s:Cr(form, mode, ...)
  function! s:TrimQuote(count, form)
    norm! znjmt
    let top = s:QTop(line("'t") - 1, 1) + (getline('.') =~ '\%3v\s*$')
    if a:count > 0
      exe top . ",'t- call s:DExtQuo(v:count)"
    endif
    if a:form == 'wrapman'
      exe top . ",'t- call s:Scan()"
    endif      
  endfunction
  exe (a:mode == 'n' ? 'norm! a·' : '')
  if getline('.') =~ '^1>'
    setl so=0
    let s:wtop = s:WTop()
    sil s/\v^(..).{-}\zs\s*·\s*(.*)/\r\r\1\2/
    exe 'sil s/\v^%' . line('.') . 'l%(\x\>\s*$|\n)*//g 2'
    sil . g/./ put! =\"\n\n\"
    call s:ConWin('update')
    unlet! s:pageLine
    norm! ms
    if (v:count > 0 || a:form == 'wrapman') && search('^\x>', 'bW') > 0
      call s:TrimQuote(v:count, a:form)
      norm! `s
    endif
    call s:RestCur(winline())
    call s:Setl('wrap')
    exe (&l:fen == 1 ? 'norm! zX' : '')
    let &so = s:so
    syncbind
    start
  elseif getline('.') =~ '\v^·?\x·?\>'
    norm! "_x
    start
  elseif a:0 == 0
    exe 'sil s/\v^.{-}' . (s:format_flowed == 1 ? '\zs\s*' : '\zs') . '·(.*)/\r\1/'
    norm! 0
    unlet! s:pageLine | set gcr&
    call s:ConWin('update')
    start
  else
    setl so=0
    let s:wtop = s:WTop()
    let start = (getline('.')[col('$') - 2] == '·' ? 'start!' : 'start')
    norm! "_xms
    if (v:count > 0 || a:form == 'wrapman') && search('^\x>', 'bW') > 0
      call s:TrimQuote(v:count, a:form)
      norm! `s
    endif
    call s:RestCur(winline())
    norm! zX
    let &so = s:so
    syncbind
    exe (a:mode == 'i' ? start : '')
  endif
endfunction

function! s:Do(...)
  if getline('.')[col('$') - 2] == '·'
    norm! $"_x
    return (a:0 > 0 ? a:1 : '')
  else
    norm! h"_x
    return (a:0 > 0 ? a:2 : '')
  endif
endfunction


function! s:ManInsert(mode, ...) range
  let ch = (a:0 == 0 ? getchar() : char2nr(a:1))
  if ch == 102
    let opt = s:Val('wrap', '/1', '')
    set paste
    if a:mode == 'i'
      exe 'norm! ' . s:Do('a', 'i') . '{FN' . opt . ' }'
    else
      exe 'sil! norm! `>a·`<i{FN' . opt . ' /·r}h'
    endif
  elseif ch == 116
    if a:mode == 'i'
      exe 'norm! ' . s:Do() . 'OVV'
    endif
    norm! '<O{TB'>oi}k0
  elseif ch == 98
    let opt = s:Val('width', '', '/w', '60') . s:Val('title', '', '/t', '') . s:Val('center', '', '/c')
    set paste
    if a:mode == 'i'
      exe 'norm! ' . s:Do() . 'OVV'
    endif
    exe "norm! '<O{BX" . opt . "'>oi}k0"
  elseif a:mode == 'i'
    call s:Do()
  endif
  set nopaste
  call s:ConWin('update')
endfunction

function! s:Undo()
  if b:stlBufStat == '--P'
    sil $ put z | sil 1,.- d _
    let s:digiquote = 1
    exe s:restore
    call s:Interface('compose')
  else
    if s:Val('dismiss', 'abort', '', 'yes') == 'abort' | return | endif
    call s:ConWin('off')
    unlet! b:levMax | let s:changedName = 1
    let @w = '' | call s:SetTitle()
    match none | syn clear
    sil $ put v | sil 1,.- d _
    call s:Interface('correct')
    unlet! b:stlManFmt s:fold s:pageLine s:restTopView
    exe 'setl nowrap ve= ts=8 fdm& gcr& fo=tcqnr ai com=n:>,fb:-,fb:*,b:\\| tw=' . s:cols
    match Error /[·•[:cntrl:] ]\|\%(\_^[> :)|]*\)\@<=[:)|]\|\_^\%(> *\)\{16,}.*/
    norm! gg
  endif
endfunction

function! s:Send(act)
  if b:stlBufStat == '-C-'
    if a:act == 'draft'
      let time = strftime("%c")
      if &vi =~ '!'
        let stat0 = 'off' | let stat1 = 'on'
        let g:DRAFT{substitute(time, '\D', '', 'g')} = '
          \let @v = "' . escape(@v, '\"') . '" | 
          \let @w = "' . escape(@w, '\"') . '" | 
          \call s:SetTitle() | 
          \let &gfn = "' . &gfn . '" | 
          \let &co = ' . winwidth(0) . ' | 
          \let &lines = ' . &lines . ' | 
          \let s:so = ' . s:so . ' | 
          \let &so = ' . s:so . ' | 
          \let &l:wrap = ' . &l:wrap . ' | 
          \match none | 
          \call s:SynHigh("' . stat{(exists("b:current_syntax") && b:current_syntax == 'posting_color')} . '") | 
          \call s:ConWin("' . stat{exists("s:authBuf")} . '") | 
          \' . (exists("s:fold") ? 'call s:Fold("out", ' . (&l:fdl + 1) . ') | ' : "") . '
          \call cursor(' . line('.') . ',' . col('.') . ') | 
          \call s:SetCur(' . winline() . ')'
      endif
      sil $ put =\"\n\" . ' - Posting v1.3/' . time . ' -'
    else
      if s:Val('send', 'abort', '', 'yes') == 'abort' | return | endif
      sil! call s:DoFinForm(a:act, 'n')
      if search('^\~\~$', 'w') > 0 | .,$ d _ | endif
    endif
  else
    if s:Val('send', 'abort', '', 'yes') == 'abort' | return | endif
  endif
  call s:ConWin('off')
  exe substitute('uvwxyz', '.', 'let @\0 = "" | ', 'g')
  if s:server
    w
    exe 'bw! ' . s:mailBuf
    setl titlestring= fdm& nofen
    stop
  else
    x!
  endif
endfunction

function! s:HlSet(grp, att)
  let x = synIDattr(synIDtrans(hlID(a:grp)), a:att, 'gui')
  return (x != '' ? ' gui' . a:att . '=' . x . ' ' : '')
endfunction

function! s:SynHigh(act)
  let s:synRepLedMax = 50
  function! s:Col(grp)
    return s:HlSet(a:grp, 'fg') . s:HlSet(a:grp, 'bg')
  endfunction
  function! s:Color()
    let id  = '<\?[^@<> \t]\+@[0-9A-Za-z-.]\+>\?'
    let url = '<\?\%(http\|https\|ftp\):\/\/[0-9A-Za-z-.]\+\.\a\{2,4}>\?\%(\/\S*\)\?'
    syn match	postingReply		'^.*$' contains=postingFootnote,@Spell
    exe 'syn region postingHeader start="^\%<6l\%(' . s:validHeaders . '\) " end="^$"me=s-1 contains=postingSubject'
    syn match	postingSubject		'^Subject: \zs.*' contained
    syn region	postingFootnote		start='{FN' end='}' contains=postingSectSpec,@Spell contained
    syn region	postingTable		start='^{TB' end='^}$' contains=postingSectSpec
    syn region	postingBox		start='^{BX' end='^}$' contains=postingSectSpec
    syn region	postingSectSpec		oneline start='{\@<=\%(FN\|TB\|BX\)' end='\\\@<! \|$' contained
    syn region	postingAnnotation	start='^\~\~$' end='\%$'
    syn match	postingQuotedA		display '^\x>.*$' contains=postingQuoted0,postingQuoted1,postingQuoted2,postingQuoted3,postingQuoted4
    syn match	postingQuoted0		'\%<11l\_^0>.*$' contains=postingAttrib1,postingQuotesLocal contained
    syn match	postingQuoted1		'^1>.*$' contains=postingQuotedLong,postingAttrib2,postingId1,postingURL1,postingQuotesLocal,,postingQuotedTab,postingTrailer contained
    syn match	postingQuoted2		'^2>.*$' contains=postingQuotedLong,postingAttrib3,postingId2,postingURL2,postingQuotesLocal,postingQuotedTab,postingTrailer contained
    syn match	postingQuoted3		'^3>.*$' contains=postingQuotedLong,postingAttrib4,postingId3,postingURL3,postingQuotesLocal,postingQuotedTab,postingTrailer contained
    syn match	postingQuoted4		'^[456789abcdef]>.*$' contains=postingQuotedLong,postingAttrib4,postingId4,postingURL4,postingQuotesLocal,postingQuotedTab,postingTrailer contained
    syn match	postingQuotesLocal	'^\x>' contained
    syn match	postingQuotedTab	'\%>2v\t' contained
    exe 'syn match postingId1		"' . id . '" contained'
    exe 'syn match postingId2		"' . id . '" contained'
    exe 'syn match postingId3		"' . id . '" contained'
    exe 'syn match postingId4		"' . id . '" contained'
    exe 'syn match postingURL1		"' . url . '" contained'
    exe 'syn match postingURL2		"' . url . '" contained'
    exe 'syn match postingURL3		"' . url . '" contained'
    exe 'syn match postingURL4		"' . url . '" contained'
    exe 'syn match postingAttrib1	"\%<' . (s:synRepLedMax + 1) . 'l\%3c.*•$" contains=postingAttribMark contained'
    exe 'syn match postingAttrib2	"\%<' . (s:synRepLedMax + 1) . 'l\%3c.*•$" contains=postingAttribMark contained'
    exe 'syn match postingAttrib3	"\%<' . (s:synRepLedMax + 1) . 'l\%3c.*•$" contains=postingAttribMark contained'
    exe 'syn match postingAttrib4	"\%<' . (s:synRepLedMax + 1) . 'l\%3c.*•$" contains=postingAttribMark contained'
    syn match	postingAttribMark	'•' contained
    syn region	postingTrailer		start='\%3c-- $' start='\%3cDISCLAIMER:' start='\%3c\*\{10,}.*[Cc]onfidential'  end='^\(..\).*\%(\n\1\@!\|\%$\)\@=' contains=postingQuotesLocal contained keepend
    syn match	postingQuotedB		display '^[ |].*$' contains=postingQuotedLong,postingFootnote
    exe 'syn match postingQuotedLong	display ".\%>' . s:vcols1 . 'v" contained'
    syn sync minlines=20
    hi def link postingHeader		Type
    hi def link postingSubject		PreProc
    hi def link postingQuoted1		Comment
    hi def link postingQuoted2		Constant
    hi def link postingQuoted3		Identifier
    hi def link postingQuoted4		Statement
    hi def link postingQuotes		Delimiter
    exe 'hi postingQuotesLocal gui=inverse' . s:Col('postingQuotes')
    hi def link postingQuotedTab	Error
    exe 'hi postingId1 gui=italic' . s:Col('postingQuoted1')
    exe 'hi postingId2 gui=italic' . s:Col('postingQuoted2')
    exe 'hi postingId3 gui=italic' . s:Col('postingQuoted3')
    exe 'hi postingId4 gui=italic' . s:Col('postingQuoted4')
    hi link postingURL1			postingId1
    hi link postingURL2			postingId2
    hi link postingURL3			postingId3
    hi link postingURL4			postingId4
    exe 'hi postingAttrib1 gui=underline' . s:Col('postingQuoted1')
    exe 'hi postingAttrib2 gui=underline' . s:Col('postingQuoted2')
    exe 'hi postingAttrib3 gui=underline' . s:Col('postingQuoted3')
    exe 'hi postingAttrib4 gui=underline' . s:Col('postingQuoted4')
    hi link postingAttribMark		Ignore
    hi def link postingQuotedLong	Search
    hi def link postingSection		Special
    hi link postingFootnote		postingSection
    hi link postingTable		postingSection
    hi link postingBox			postingSection
    exe 'hi postingSectSpec gui=bold' . s:Col('postingSection')
    hi def link postingTrailer		Error
    hi def link postingAnnotation	Todo
  endfunction
  function! s:Mono()
    syn match	postingReply		'^[ |]\@!.*$' contains=@Spell
    exe 'syn region postingAttrib	display start="\%<' . (s:synRepLedMax + 1) . 'l\%3c" end="•$" oneline contains=postingAttribMark'
    syn match	postingAttribMark	'•' contained
    syn match	postingQuotesLocal	display '^\x>'
    syn sync maxlines=5
    hi link postingAttrib		Underlined
    hi link postingAttribMark		Ignore
    hi def link postingQuotes		Delimiter
    exe 'hi postingQuotesLocal gui=inverse' . s:Col('postingQuotes')
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
    call s:SynHigh(exists("b:current_syntax") && b:current_syntax != 'posting_mono' ? 'off' : 'on')
  endif
endfunction

function! s:Help(buf)
  if a:buf == 'help'
    bd
    let &so = s:so
    exe s:helpRest
  else
    let start = s:Do('start!', 'start')
    if bufloaded(bufnr('posting.txt')) == 0
      exe s:SaveView()
      let s:helpRest = restView . ' | ' . start
      h posting-commands
      nn <buffer> <silent> <M-h> :call <SID>Help('help')<Cr>
    else
      exe 'bd ' . bufnr('posting.txt')
      let &so = s:so
      exe start
    endif
  endif
endfunction

function! s:MHelp()
  if bufloaded(bufnr('posting.txt')) == 0
    h posting-commands
  else
    exe 'bd ' . bufnr('posting.txt')
  endif
endfunction

function! s:Unplenk()
  exe s:SaveView('mark')
  %s/\v%(^\x\>.*)@<=%(\w|\)|\]|'|")@<= {1,2}%([.:!?]%( |$))@=//ge
  %s/\v%(^\x\>.*)@<=%(\w|\)|\]|'|")@<= *\n(\x\>\s*)([.:!?])%( |$)/\2\1/e
  exe restView
endfunction

function! s:AEdit(act, ...)
  if getline('.') =~ '^\%(f>\)\@!\x>'
    let i = line('.')
    if getline(i) =~ '•$'
      let sN = 'N' . getline(i)[0] . ': '
      let sL = 'L' . getline(i)[0] . '\[\d\]: '
      let src = substitute(matchstr(@w, sL . '.\{-}' . "\n" . '\%(' . sL . '\)\@!'), sL, '', 'g')
      if src != ''
	s/.*\n\%(\x>\s*\n\)\?//
	put! =src
	if getline(i - 1) =~ '•$'
	  call s:DSep(i - 1)
	endif
	let @w = substitute(@w, '\%(' . sL . '\|' . sN . '\)' . "[^\n]*\n", '', 'g')
	let s:changedName = 1
      endif
    else
      if match(@w, 'L' . getline(i)[0] . '\[\d\]: ') == -1
	let j = col('.')
      	call s:Unify(a:act)
      	if getline(i + 1) =~ '^\x>.*•\@<!$'
      	  call s:DSep(i)
      	endif
      	if getline(i - 1) =~ '^\x>\s*$' && getline(i - 2) =~ '•$'
      	  let i = i - 1
      	  exe i . ' d _'
      	endif
	call cursor(i, j)
      	let s:repLedMax = s:Max(i + 5, s:repLedMax)
      endif
    endif
    if a:0 > 0 && exists('s:changedName')
      call s:ConWin('update') | call s:SetTitle()
    endif
  endif
endfunction

function! s:Attrib(act, ...)
  function! s:ConvertQuotes()
    let hdr1    = 'Original Message|Ursprüngliche Nachricht'
    let hdr2    = 'Date:|Gesendet:|From:|Von:'
    let hdrFrom = 'From:|Von:'
    function! s:Convert(hdr)
      exe 's/\v^\_.{-}\_^(\x\>)?\s*%(' . a:hdr . ')\s*(.*)\_$\_.{-}%(\n%(\x\>)?\s*$)@=/\=substitute(submatch(1) . substitute(submatch(2), "\\[.\\{-}:\\([^\\]]*\\)\\]", " <\\1>", "") . " wrote:", " \\{2,}", " ", "g")/'
    endfunction
    function! s:Requote()
      function! s:Add(n) range
        exe a:firstline . ',' . a:lastline . ' s/\v^((\x)\>)?/\=(submatch(1) + a:n) . ">"/'
      endfunction
      ma m | /\%>2v\S/ ma n
      if s:DLev(line("'m") - 1) < s:DLev(line("'m"))
        'm call s:Add(-1)
      elseif s:DLev(line("'n")) == s:DLev(line("'m"))
        'n,$ call s:Add(1)
      endif
    endfunction
    exe 'g/\v^.*%(' . hdr1 . '|-{4,}\n.*%(' . hdr2 . '))/ call s:Convert(hdrFrom) | call s:Requote()'
  endfunction
  if a:act == 'unmark'
    if match(@w, 'L' . a:1 . '\[\d\]: ') >= 0
      exe '0/^' . a:1 . '>.*•$/ call s:AEdit("mark")'
    endif
  elseif a:act == 'mark'
    call s:AEdit(s:attrib_default_action)
  elseif a:act == 'UNMARK'
    while search('•$', 'w') > 0 | call s:AEdit('mark') | endwhile
  elseif a:act == 'MARK'
    call s:Attrib('UNMARK')
    let s:repLedMax = line('$') | call s:TrimAtt()
  elseif a:act == 'convert'
    call s:ConvertQuotes()
  elseif a:act == 'store'
    let @s = '' | exe '1,' . s:Min(s:repLedMax, line('$')) . ' g/•$/ d S | if getline(".") =~ "^\\x>\\s*$" | d _ | endif'
  elseif a:act == 'restore'
    $
    if @s != '' && search('^\x>', 'w') > 0
      let i = 0
      let max = '0x' . a:1 + 0
      while i <= max
	let leadin = matchstr(@s, "\n" . '\zs' . '0123456789abcdef'[i] .'>.\{-}•\ze')
	if leadin != ''
	  call append(line('.') - 1, leadin)
	  let newLine = 1
	endif
	let i = i + 1
      endwhile
      if exists('newLine')
	let s:repLedMax = line('.') + 4
	call s:DSep(line('.') - 1)
      else
	let s:repLedMax = 1
      endif
    endif
  endif
endfunction

function! s:ManAEdit()
  let ch = nr2char(getchar())
  if ch =~ '[0123456789abcde]'
    call s:Attrib('unmark', ch)
  elseif ch == '#'
    call s:Attrib('mark')
  elseif ch == 'u'
    call s:Attrib('UNMARK')
  elseif ch == 'm'
    call s:Attrib('MARK')
  elseif ch == 't'
    call s:Attrib('convert') | unlet! b:levMax | let s:changedName = 1
  elseif ch == 'j'
    call s:Attrib('store')
    call s:Attrib('restore', s:MaxBufLev() - 1)
  endif
  exe (ch =~ '[umtj]' ? 'norm! gg' : '')
  call s:ConWin('update') | call s:SetTitle()
endfunction

function! s:Ce(mode) range
  if a:mode == 'v'
    let top = a:firstline
    let bot = a:lastline
  else
    exe s:TSetRange()
  endif
  let n = 0
  exe 'sil ' . top . ',' . bot . ' g/./ if col("$") > n | let n = col("$") | endif'
  if n < s:cols
    exe 'sil ' . top . ',' . bot . ' call s:Shft((s:cols - n + 1) / 2)'
  endif
endfunction

function! s:Jump(dir)
  exe (exists("s:jpPage") ? s:WTop() + &so : '')
  if search('\v%(\_^1\>%(\s*\|)@!.*)@<=%(%(\a\a|\)|\.\.|' . "'" . '|")@<=[.!?]\s|[.!?]$|%([.!?][' . "'" . '")]?)@<=[")]|[^•]%(\n\x\>\s*\_$|%$)@=)', (a:dir == 'up' ? 'Wb' : 'W')) > 0
    let s:jpMatch = 1
    unlet! s:pageLine | set gcr&
  else
    set gcr=n:block-Ignore
    echohl ErrorMsg
    if exists("s:jpMatch") && a:dir == 'up'
      echo 'top'
    elseif exists("s:jpMatch")
      echo 'bottom'
    else
      echo 'no match'
    endif
    echohl None
    sleep 500m
    echon "\r        "
    set gcr&
  endif
  unlet! s:jpPage
endfunction

function! s:Tab(dir, ...)
  exe s:SaveSett('ve')
  set ve=all
  if a:0 > 0 || getline('.') =~ '\v^·?\x·?\>'
    sil! norm! "_xh
    call s:Jump(a:dir)
    sil! norm! lze
  else
    sil! norm! r	l
  endif
  start
  exe restSett
endfunction


function! s:Bar()
  if getline('.') =~ '^\x>'
    norm! 2|r>lg$
  endif
endfunction

function! s:Grab()
  norm! gv
  sil! . v/\%>2v\S/ norm! j
endfunction

function! s:Zoom(dir)
  let fonts = 'Courier_New'
  function! s:Size(type, ...)
    let Courier_New = '10/56 12/50 14/42 16/40 18/34'
    return matchstr({s:f}, (a:type == 'lines' ? a:1 . '/\zs\d*' : 
			  \(a:type == 'h_min' ? '^\d*' : '\d*\%(/\S*$\)\@=')))
  endfunction
  if &gfn =~ fonts
    let s:f   = matchstr(&gfn, '^.\{-}:\@=')
    let h     = matchstr(&gfn, ':h\zs\d\+')
    let h_min = s:Size('h_min')
    let h_max = s:Size('h_max')
    if (a:dir == 'out' && h > h_min) || (a:dir == 'in' && h < h_max)
      if !exists('s:lines_' . s:f . '_' . h) || &lines != s:lines_{s:f}_{h}
        let i = h_min
        while i <= h_max
	  if s:Size('lines', i) != ''
	    let s:lines_{s:f}_{i} = s:Size('lines', i) * &lines / s:Size('lines', h)
	  endif
	  let i = i + 1
        endwhile
      endif
      let d = (a:dir == 'in' ? 1 : -1)
      let i = h + d
      while !exists('s:lines_' . s:f . '_' . i)
        let i = i + d
      endwhile
      if i > h
	let &lines = s:lines_{s:f}_{i}
	let &gfn   = substitute(&gfn, ':h\zs\d\+', '\=i', '')
      else
	let &gfn   = substitute(&gfn, ':h\zs\d\+', '\=i', '')
	let &lines = s:lines_{s:f}_{i}
      endif
    endif
  endif
endfunction

function! s:Fold(dir, ...)
  function! FLev(n)
    if getline(a:n) =~ '^\x>\s*$'
      return ('0x' . getline(a:n)[0]) + (getline(a:n - 1) =~ '•$')
    elseif getline(a:n) =~ '^\x>.*•$'
      return ('0x' . getline(a:n)[0]) + 1
    elseif getline(a:n) =~ '^\x>'
      return ('0x' . getline(a:n)[0]) + 0
    endif
    return 0
  endfunction
  function! FTxt()
    let levels = ''
    let lines  = 0
    let names  = ''
    let i = v:foldstart
    while i <= v:foldend
      if getline(i) =~ '\%3v.*\S.*•\@<!$'
        let levels = levels . getline(i)[0]
        let lines = lines + 1
      endif
      let i = i + 1
    endwhile
    let lines = strpart('   ' . lines, strlen('   ' . lines) - 3)
    let levels = substitute(matchstr(levels . '1', '\v%(f[fe1]@=)*%(e[ed1]@=)*%(d[dc1]@=)*%(c[cb1]@=)*%(b[ba1]@=)*%(a[a91]@=)*%(9[981]@=)*%(8[871]@=)*%(7[761]@=)*%(6[651]@=)*%(5[541]@=)*%(4[431]@=)*%(3[321]@=)*%(2[21]@=)*\ze1$'), '\%(\(.\)\1\@=\)\+', '', 'g')
    let names = (levels =~ (s:DLev(v:foldend + 2) + 1) . '$') 
    \? substitute(substitute(levels, '.', '\=s:SMax(matchstr(@w, "N" . "0123456789abcdef"[("0x" . submatch(0)) - 1] . ": \\zs[^\n]*\\ze\n"), "?") . ", "', 'g'), ', $', '', 'g') 
    \: ''
    if strlen(names) > winwidth(0) - 7
      let names = '<' . strpart(names, strlen(names) - (winwidth(0) - 7) + 1)
    endif
    return '+>' . lines . '  ' . names
  endfunction
  if exists("s:authBuf") | return | endif
  let wl = winline()
  if !exists("s:fold")
    if !exists("b:levMax")
      let b:levMax = s:MaxBufLev()
    endif
    setl fcs=vert:| fdm=expr fde=FLev(v:lnum) fdt=FTxt() fml=0 fdo& fen
    let &l:fdl = b:levMax
    exe 'hi Folded gui=inverse' . s:HlSet('postingQuotes', 'fg') . s:HlSet('postingQuotes', 'bg')
    let s:fold = 1
  endif
  if a:dir == 'out' && exists("a:1") && a:1 > 0
    if a:1 != (&l:fdl + 1) | let &l:fdl = (a:1 > 1 ? (a:1 <= b:levMax ? a:1 - 1 : b:levMax) : 1) | endif
  elseif a:dir == 'out' && &l:fdl > 1
    let &l:fdl = &l:fdl - 1
  elseif a:dir == 'in' && &l:fdl < b:levMax
    let &l:fdl = &l:fdl + 1
  endif
  if &l:fdl == b:levMax | unlet s:fold | setl fdm& nofen | endif
  call s:SetCur(wl)
endfunction

function! s:Setl(val)
  exe 'setl ' . a:val
  exe (exists('s:authBuf') ? 'wincmd h | setl ' . a:val . ' | wincmd l' : '')
endfunction

function! s:InvWrap()
  if &l:wrap == 0
    call s:Setl('wrap')
    let s:wrapLine = line('.')
    let s:wrapCol = col('.')
    if col('.') > wincol()
      exe 'norm! ' . (col('.') - wincol()) . 'zh'
    endif
  else
    call s:Setl('nowrap')
    if exists('s:wrapLine') && line('.') == s:wrapLine
      call cursor('.', s:wrapCol)
      unlet s:wrapLine s:wrapCol
    endif
  endif
endfunction

function! s:ManPaste(...)
  let ch = (a:0 == 0 ? getchar() : char2nr(a:1))
  if ch == 32
    exe 'norm! ' . s:Do('"*gp', '"*gP')
  else
    call s:Do()
    if ch == 105
      call s:Paste('*', 'nowrap', s:Val('indent', '0', '', 4))
    elseif ch == 9
      call s:Paste('*', 'wrap_dot', s:Val('indent', '0', '', 4))
    elseif ch == 113
      call s:Paste('*', 'nowrap')
    elseif ch == 17
      call s:Paste('*', 'wrap_dot')
    elseif ch == 98
      .- ma z
      call s:Paste('*', 'nowrap', 0)
      norm! kV'z+
      call s:ManInsert('v', 'b')
    endif
  endif
  call s:ConWin('update')
endfunction

function! s:Retab()
  cno <buffer> <Esc> <C-E><C-U>-<Cr>
  cno <buffer> <silent> - <C-E> - 1<Cr>
  cno <buffer> <silent> + <C-E> + 1<Cr>
  echohl Question
  let ts = input('width? ', &l:ts)
  while ts =~ ' 1$'
    exe 'let ts = ' . ts
    let &l:ts = (ts > 0 ? ts : &l:ts)
    redraw
    let ts = input('width? ', &l:ts)
  endwhile
  echohl None
  cu <buffer> <Esc>
  cu <buffer> -
  cu <buffer> +
  if ts != '-'
    let &l:ts = ts | retab 2
  else
    setl ts=2
  endif
endfunction

function! s:Requote()
  let m = escape(s:Val('quote-mark', '', '', ':'), '/$.')
  if m != '' | exe '%s/\%(^\%(>\| \|' . m . '\)*\)\@<=' . m . '/>/gc' | endif
endfunction

function! s:Scroll(dir, lin)
  exe s:SaveSett('sc', 'smd')
  set nosc nosmd
  let i = 0
  while i < a:lin
    exe 'norm! ' . (a:dir == 'up' ? '' : '')
    syncbind
    redraw
    let i = i + 1
  endwhile
  exe restSett
endfunction

function! s:Page(dir, ...)
  let start = s:Do('start!', 'start')
  if !exists("s:pageLine")
    let s:pageLine = line('.')
    let s:pageRest = 'call cursor(' . line('.') . ',' . virtcol('.') . ') | ' . start
  endif
  call s:Scroll(a:dir, winheight(0) / (a:0 > 0 ? 4 : 1))
  let wtop = s:WTop()
  if s:pageLine >= (wtop + &so) && s:pageLine < (wtop + winheight(0) - &so)
    exe s:pageRest
    unlet s:pageLine s:pageRest
    set gcr&
  else
    let &gcr = 'n-v-c:block-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i:ver25-Cursor/lCursor-blinkon0,ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175'
  endif
  let s:jpPage = 1
endfunction


function! s:Reply(dir)
  exe s:SaveSett('so', 'ws')
  setl so=0 nows
  let top1 = s:WTop()
  if a:dir == 'down'
    sil! norm! /\v^\x\>.*\n%(\x\>)@!/;/^.*\Smzz.
    let top2 = s:WTop()
    exe 'norm! ' . top1 . 'zt'
    call s:Scroll('down', top2 - top1)
  else
    sil! norm! ?\v^(\x\>)@!.*\n\x\>?;?^\x>?;/^.*\Smzz.
    let top2 = s:WTop()
    exe 'norm! ' . top1 . 'zt'
    call s:Scroll('up', top1 - top2)
  endif
  norm! `z
  exe restSett
endfunction

function! s:Invert(...)
  function! s:Reverse()
    1 put! _ | $ put _
    ma a
    g/•$/ co $
    let s:repLedMax = line('$') - line("'a") + 5
    set nows
    'a?\v^\x\>.*\S.*•@<!$|%1l? ma b
    while line("'b") > 1
      'b+?\v%(•\n|\_^%(\x\>)@!.*\n|%^)@<=\_^\x\>|%1l? ma t
      't,'b co $
      't?\v^\x\>.*\S.*•@<!$|%1l? ma b
    endwhile
    1,'a- d _
    $?\v•\_$|%1l? ma t
    't+,$ g/^0>\s*$/ d _
    't+,$ call s:RefQuo('nowrap')
    if getline("'t") =~ '•$' | call s:DSep(line("'t")) | endif
    1 d _
    set ws&
  endfunction
  setl nosmd nosc
  if !exists('s:restTopView')
    % y t | let s:restTopView = '$ put t | 1,.- d _ | let @w = "' . escape(@w, '\"') . '"'
    call s:Frame('save')
    if a:0 == 0
      call s:Clean()
      call s:Attrib('UNMARK')
      call s:Attrib('convert')
      call s:Attrib('MARK')
    endif
    call s:Reverse()
    call s:Frame('restore')
  else
    exe s:restTopView | unlet s:restTopView
  endif
  unlet! b:levMax | let s:changedName = 1 | call s:ConWin('update')
  call s:SetTitle()
  setl smd sc
endfunction

function! s:Number(mode) range
  function! s:N(n)
    return strpart('   ' . a:n, strlen('   ' . a:n) - 3, 6)
  endfunction
  if a:mode == 'v'
    let top = a:firstline
    let bot = a:lastline
  else
    exe s:TSetRange()
  endif
  exe top . ',' . bot . ' s/\%' . (a:mode == 'v' ? col("'<") : 1) . 'c/\=s:N(line(".") - ' . (top - 1) . ') . " "/'
endfunction

function! s:Interface(mode)
  function! s:Wipe()
    unmenu    Posting
    unmenu!   Posting
    mapclear  <buffer>
    mapclear! <buffer>
    nmapclear <buffer>
    vmapclear <buffer>
    imapclear <buffer>
    omapclear <buffer>
    nunmap    <M-h>
    iunmap    <M-h>
  endfunction
  sil! call s:Wipe()
  if a:mode == 'correct'
    if s:guiSup
      anoremenu <silent> &Posting.Re&quote<Tab>,q   :call <SID>Requote()<Cr>
      anoremenu <silent> &Posting.Re&move<Tab>,m    vy:exe '%s/' . escape(@", '/$.') . '/ /ge'<Cr>
      anoremenu <silent> &Posting.&Cut\ off<Tab>,c  :sil g/^\%(> *\)\{16,}/d<Cr>
      menu	Posting.-Sep1- :
      exe 'anoremenu <silent> &Posting.&Preformat<Tab>Alt+U :Post' . (s:server ? '! ' : ' ') . s:cols . '<Cr>'
      anoremenu	<silent> &Posting.&Send!<Tab>Alt+Z  :call <SID>Send('asis')<Cr>
      tm &Posting.Re&quote			    To replace unusual quote-marks
      tm &Posting.Re&move			    To remove unusual characters
      tm &Posting.&Cut\ off			    To cut off invalid levels
      exe 'tm &Posting.&Preformat :Post' . (s:server ? '! ' : ' ') . s:cols
      tm &Posting.&Send!			    Save and exit
    endif
    nn	<buffer> <silent> ,q			    :call <SID>Requote()<Cr>
    nn	<buffer> <silent> ,m			    vy:exe '%s/' . escape(@", '/') . '/ /ge'<Cr>
    nn	<buffer> <silent> ,c			    :sil g/^\%(> *\)\{16,}/d<Cr>
    exe 'nn <buffer> <M-u> :Post' . (s:server ? '! ' : ' ') . s:cols . '<Cr>'
    exe 'ino <buffer> <M-u> <C-O>:Post' . (s:server ? '! ' : ' ') . s:cols . '<Cr>'
    nn	<buffer> <silent> <M-z>			    :call <SID>Send('asis')<Cr>
    ino	<buffer> <silent> <M-z>			    <C-O>:call <SID>Send('asis')<Cr>
    let b:stlBufStat = 'R--'
  elseif a:mode == 'compose'
    if s:guiSup
      anoremenu	  <silent> &Posting.&Display.&Help\ on/off<Tab>Alt+H	:call <SID>MHelp()<Cr>
      anoremenu	  <silent> &Posting.&Display.&Wrap\ on/off<Tab>Alt+W	:call <SID>InvWrap()<Cr>
      anoremenu	  <silent> &Posting.&Display.&Virtual\ on/off<Tab>Alt+E	:exe 'setl ve=' . (&l:ve != 'all' ? 'all' : '')<Cr>
      menu	  Posting.Display.-Sep1- :
      anoremenu	  <silent> &Posting.&Display.&Zoom\ in<Tab>Alt++	:call <SID>Zoom('in')<Cr>
      anoremenu	  <silent> &Posting.&Display.Zoom\ &out<Tab>Alt+-	:call <SID>Zoom('out')<Cr>
      anoremenu	  <silent> &Posting.&Display.&Fold\ in<Tab>Alt+R	:call <SID>Fold('in')<Cr>
      anoremenu	  <silent> &Posting.&Display.Fold\ o&ut<Tab>Alt+M	:<C-U>call <SID>Fold('out', v:count)<Cr>
      menu	  Posting.Display.-Sep2- :
      anoremenu	  <silent> &Posting.&Display.&Syntax\ on/off<Tab>Alt+Y	:call <SID>SynHigh('toggle')<Cr>
      anoremenu	  <silent> &Posting.&Display.&Context\ on/off<Tab>Alt+X	:call <SID>ConWin('toggle')<Cr>
      anoremenu	  <silent> &Posting.&Quoting.&Clean<Tab>Alt+L		:call <SID>Clean('restView')<Cr>
      anoremenu	  <silent> &Posting.&Quoting.&Retab<Tab>Alt+T		:call <SID>Retab()<Cr>
      anoremenu	  <silent> &Posting.&Quoting.&Unplenk<Tab>Alt+K		:sil! call <SID>Unplenk()<Cr>
      anoremenu	  <silent> &Posting.&Quoting.&Invert<Tab>Alt+I		:sil! call <SID>Invert()<Cr>
      anoremenu	  <silent> &Posting.&Quoting.&Dismiss<Tab>Alt+U		:call <SID>Undo()<Cr>
      menu	  Posting.Quoting.-Sep1- :
      nnoremenu	  <silent> &Posting.&Quoting.&Abridge<Tab>Alt+A		:call <SID>Abridge('nowrap', 'n', v:count)<Cr>
      inoremenu	  <silent> &Posting.&Quoting.&Scan<Tab>Ctrl+Cr		<C-K>.M<Esc>:<C-U>call <SID>Cr('wrapman', 'i', 'scan')<Cr>
      menu	  Posting.-Sep1- :
      inoremenu	  <silent> &Posting.&Paste.&Indented<Tab>Ctrl+P\ i	<C-K>.M<C-O>:call <SID>ManPaste('i')<Cr>
      inoremenu	  <silent> &Posting.&Paste.&Quoted<Tab>Ctrl+P\ q	<C-K>.M<C-O>:call <SID>ManPaste('q')<Cr>
      inoremenu	  <silent> &Posting.&Paste.&Boxed<Tab>Ctrl+P\ b		<C-K>.M<C-O>:call <SID>ManPaste('b')<Cr>
      inoremenu	  <silent> &Posting.S&ections.&Footnote<Tab>Ctrl+S\ f	<C-K>.M<C-O>:call <SID>ManInsert('i', 'f')<Cr>
      inoremenu	  <silent> &Posting.S&ections.&Table<Tab>Ctrl+S\ t	<C-K>.M<C-O>:call <SID>ManInsert('i', 't')<Cr>
      inoremenu	  <silent> &Posting.S&ections.&Box<Tab>Ctrl+S\ b	<C-K>.M<C-O>:call <SID>ManInsert('i', 'b')<Cr>
      inoremenu	  <silent> &Posting.&Center<Tab>Alt+C			<C-O>:call <SID>Ce('n')<Cr>
      vnoremenu	  <silent> &Posting.&Center<Tab>Alt+C			:call <SID>Ce('v')<Cr>
      inoremenu	  <silent> &Posting.&Number<Tab>Alt+N			<C-O>:call <SID>Number('n')<Cr>
      vnoremenu	  <silent> &Posting.&Number<Tab>Alt+N			:call <SID>Number('v')<Cr>`>
      inoremenu	  <silent> &Posting.&Justify<Tab>Alt+J			<C-O>:call <SID>Justify('n')<Cr>
      vnoremenu	  <silent> &Posting.&Justify<Tab>Alt+J			:call <SID>Justify('v')<Cr>
      inoremenu	  <silent> &Posting.&Preview<Tab>Alt+P			<C-O>:<C-U>sil! call <SID>DoFinForm('nowrap', 'i')<Cr>
      menu	  Posting.-Sep2- :
      anoremenu	  <silent> &Posting.P&ostpone!<Tab>Alt+O		:call <SID>Send('draft')<Cr>
      anoremenu	  <silent> &Posting.&Send!<Tab>Alt+Z			:<C-U>call <SID>Send('nowrap')<Cr>
      tm &Posting.&Display.&Help\ on/off				Slots in *posting-commands*
      tm &Posting.&Display.&Wrap\ on/off	    			Let's you show/hide long lines
      tm &Posting.&Display.&Virtual\ on/off				Turns on/off 'virtualedit'
      tm &Posting.&Display.&Zoom\ in					Increases font size
      tm &Posting.&Display.Zoom\ &out					Increases visible area
      tm &Posting.&Display.&Fold\ in					Unhides one level more
      tm &Posting.&Display.Fold\ o&ut					Hides one level more
      tm &Posting.&Display.&Syntax\ on/off       			Turns on/off syntax highlighting
      tm &Posting.&Display.&Context\ on/off				Opens/closes Context window
      tm &Posting.&Quoting.&Clean					Removes signatures/disclaimers
      tm &Posting.&Quoting.&Retab					Use this to replace/resize tab's
      tm &Posting.&Quoting.&Unplenk					Corrects Plenking
      tm &Posting.&Quoting.&Invert					Changes quoting style from top- to bottom-posting
      tm &Posting.&Quoting.&Dismiss					Use this to undo :Post
      tm &Posting.&Quoting.&Abridge					Cut off levels greater than [count]
      tm &Posting.&Quoting.&Scan					Splits quotation and turns on Scan mode
      tm &Posting.&Paste.&Indented		    			Shifts to the right
      tm &Posting.&Paste.&Quoted		    			Prepends "\| "
      tm &Posting.&Paste.&Boxed						Encloses in frame section
      tm &Posting.S&ections.&Footnote		    			Will be appended
      tm &Posting.S&ections.&Table		    			Text inside won't change
      tm &Posting.S&ections.&Box			    		Will be surrounded by a frame
      tm &Posting.&Center						Centers area/paragraph
      tm &Posting.&Number						Numbers lines within area/paragraph
      tm &Posting.&Justify			    			Adjusts line length and corrects bad quoting markup
      tm &Posting.&Preview			    			Prepares message for sending
      tm &Posting.P&ostpone!		    			        To resume writing later
      tm &Posting.&Send!						Send message and exit
    endif
    ino	  <buffer> <silent> <M-h>		    <C-K>.M<C-O>:call <SID>Help('mail')<Cr>
    ino	  <buffer> <silent> <M-w>		    <C-O>:call <SID>InvWrap()<Cr>
    ino	  <buffer> <silent> <M-e>		    <C-O>:exe 'setl ve=' . (&l:ve != 'all' ? 'all' : '')<Cr>
    ino	  <buffer> <silent> <M-+>		    <C-O>:call <SID>Zoom('in')<Cr>
    ino	  <buffer> <silent> <M-->		    <C-O>:call <SID>Zoom('out')<Cr>
    ino	  <buffer> <silent> <M-r>		    <C-O>:call <SID>Fold('in')<Cr>
    nn	  <buffer> <silent> <M-m>		    :<C-U>call <SID>Fold('out', v:count)<Cr>
    ino	  <buffer> <silent> <M-m>		    <C-O>:call <SID>Fold('out', v:count)<Cr>
    ino	  <buffer> <silent> <M-q>		    <C-O>:call <SID>Fold('out', (&l:fen == 0 ? '1' : '99'))<Cr>
    ino	  <buffer> <silent> <M-y>		    <C-O>:call <SID>SynHigh('toggle')<Cr>
    ino	  <buffer> <silent> <M-x>		    <C-O>:call <SID>ConWin('toggle')<Cr>
    ino	  <buffer> <silent> <M-LeftMouse>	    <Esc><LeftMouse>:sil! call <SID>AEdit('mark', 'update')<Cr>i
    ino	  <buffer> <silent> <M-C-LeftMouse>	    <Esc><LeftMouse>:sil! call <SID>AEdit('force', 'update')<Cr>i
    ino	  <buffer> <silent> <M-RightMouse>	    <Esc><LeftMouse>:sil! call <SID>DSep(line('.'))<Cr>:call <SID>ConWin('update')<Cr>i
    ino	  <buffer> <silent> <C-RightMouse>	    <Esc><LeftMouse>dd:call <SID>ConWin('update')<Cr>i
    ino	  <buffer> <silent> #			    <C-O>:sil! call <SID>ManAEdit()<Cr>
    ino	  <buffer> <silent> <Tab>		    <C-K>.M<Esc>:call <SID>Tab('down')<Cr>
    nn	  <buffer> <silent> <Tab>		    a<C-K>.M<Esc>:call <SID>Tab('down', 'jump')<Cr>
    ino	  <buffer> <silent> <C-Tab>		    <C-K>.M<Esc>:call <SID>Tab('down', 'jump')<Cr>
    ino	  <buffer> <silent> <S-Tab>		    <C-K>.M<Esc>:call <SID>Tab('up', 'jump')<Cr>
    ino	  <buffer> <silent> <M-l>		    <C-O>:call <SID>Clean('restView')<Cr>
    ino	  <buffer> <silent> <M-t>		    <C-O>:call <SID>Retab()<Cr>
    ino	  <buffer> <silent> <M-k>		    <C-O>:sil! call <SID>Unplenk()<Cr>
    ino	  <buffer> <silent> <M-i>		    <C-O>:sil! call <SID>Invert()<Cr>
    ino	  <buffer> <silent> <S-M-i>		    <C-O>:sil! call <SID>Invert('skipAttProc')<Cr>
    ino	  <buffer> <silent> <M-u>		    <Esc>:call <SID>Undo()<Cr>
    nn	  <buffer> <silent> <M-u>		    :call <SID>Undo()<Cr>
    vn	  <buffer> <silent> <M-a>		    :call <SID>Abridge('nowrap', 'v', v:count)<Cr>
    nn	  <buffer> <silent> <M-a>		    :call <SID>Abridge('nowrap', 'n', v:count)<Cr>
    vn	  <buffer> <silent> <S-M-a>		    :call <SID>Abridge('wrap', 'v', v:count)<Cr>
    nn	  <buffer> <silent> <S-M-a>		    :call <SID>Abridge('wrap', 'n', v:count)<Cr>
    ino	  <buffer> <silent> <Cr>		    <C-K>.M<Esc>:<C-U>call <SID>Cr('nowrap', 'i')<Cr>
    ino	  <buffer> <silent> <C-Cr>		    <C-K>.M<Esc>:<C-U>call <SID>Cr('wrapman', 'i', 'scan')<Cr>
    nn	  <buffer> <silent> <Cr>		    :<C-U>call <SID>Cr('nowrap', 'n')<Cr>
    nn	  <buffer> <silent> <C-Cr>		    :<C-U>call <SID>Cr('wrapman', 'n', 'scan')<Cr>
    vn	  <buffer> <silent> <C-y>		    "*y
    ino	  <buffer> <silent> <C-p>		    <C-K>.M<C-O>:call <SID>ManPaste()<Cr>
    ino	  <buffer> <silent> <C-s>		    <C-K>.M<C-O>:call <SID>ManInsert('i')<Cr>
    vn	  <buffer> <silent> <C-s>		    :call <SID>ManInsert('v')<Cr>
    ino	  <buffer> <silent> <M-c>		    <C-O>:call <SID>Ce('n')<Cr>
    vn	  <buffer> <silent> <M-c>		    :call <SID>Ce('v')<Cr>
    ino	  <buffer> <silent> <M-n>		    <C-O>:call <SID>Number('n')<Cr>
    vn	  <buffer> <silent> <M-n>		    :call <SID>Number('v')<Cr>`>
    vn	  <buffer> <silent> <M-j>		    :call <SID>Justify('v')<Cr>
    nn	  <buffer> <silent> <M-j>		    :call <SID>Justify('n')<Cr>
    ino	  <buffer> <silent> <M-j>		    <C-O>:call <SID>Justify('n')<Cr>
    vn	  <buffer> <silent> <S-M-j>		    :call <SID>Justify('v', 'tq12')<Cr>
    nn	  <buffer> <silent> <S-M-j>		    :call <SID>Justify('n', 'tq12')<Cr>
    ino	  <buffer> <silent> <S-M-j>		    <C-O>:call <SID>Justify('n', 'tq12')<Cr>
    ino	  <buffer> <silent> <M-p>		    <C-K>.M<C-O>:<C-U>sil! call <SID>DoFinForm('nowrap', 'i')<Cr>
    nn	  <buffer> <silent> <M-p>		    :<C-U>sil! call <SID>DoFinForm('nowrap', 'n')<Cr>
    ino	  <buffer> <silent> <S-M-p>		    <C-K>.M<C-O>:<C-U>sil! call <SID>DoFinForm('wrap', 'i')<Cr>
    nn	  <buffer> <silent> <S-M-p>		    :<C-U>sil! call <SID>DoFinForm('wrap', 'n')<Cr>
    nn	  <buffer> <silent> <M-o>		    :call <SID>Send('draft')<Cr>
    ino	  <buffer> <silent> <M-o>		    <C-O>:call <SID>Send('draft')<Cr>
    nn	  <buffer> <silent> <M-z>		    :<C-U>call <SID>Send('nowrap')<Cr>
    ino	  <buffer> <silent> <M-z>		    <C-O>:<C-U>call <SID>Send('nowrap')<Cr>
    nn	  <buffer> <silent> <S-M-z>		    :<C-U>call <SID>Send('wrap')<Cr>
    ino	  <buffer> <silent> <S-M-z>		    <C-O>:<C-U>call <SID>Send('wrap')<Cr>
    ino	  <buffer> <silent> <Up>		    <C-O>gk
    ino	  <buffer> <silent> <Down>		    <C-O>gj
    no	  <buffer> <silent> <Up>		    gk
    no	  <buffer> <silent> <Down>		    gj
    ino	  <buffer> <silent> <Home>		    <C-O>g0
    ino	  <buffer> <silent> <End>		    <C-O>g$
    no	  <buffer> <silent> <Home>		    g0
    no	  <buffer> <silent> <End>		    g$
    ino	  <buffer> <silent> <PageUp>		    <C-K>.M<C-O>:call <SID>Page('up', 'step')<Cr>
    ino	  <buffer> <silent> <PageDown>		    <C-K>.M<C-O>:call <SID>Page('down', 'step')<Cr>
    ino	  <buffer> <silent> <S-PageUp>		    <C-K>.M<C-O>:call <SID>Page('up')<Cr>
    ino	  <buffer> <silent> <S-PageDown>	    <C-K>.M<C-O>:call <SID>Page('down')<Cr>
    ino	  <buffer> <silent> <M-Up>		    <C-O>:call <SID>Reply('up')<Cr>
    ino	  <buffer> <silent> <M-Down>		    <C-O>:call <SID>Reply('down')<Cr>
    nn	  <buffer> <silent> ,qB			    :call <SID>Bar()<Cr>
    nn	  <buffer> <silent> ,qe1		    :echohl ModeMsg <Bar> echo '-- SCAN --' <Bar> echohl None<Cr>
    nn	  <buffer> <silent> ,qe2		    :echohl ModeMsg <Bar> echo '-- (scan) VISUAL --' <Bar> echohl None<Cr>
    nn	  <buffer> <silent> ,qe0		    :echo<Cr>
    nn	  <buffer> <silent> ,qj			    j:sil! . v/\%>2v\S/+<Cr>
    nn	  <buffer> <silent> ,qk			    k:sil! . v/\%>2v\S/-<Cr>
    vn	  <buffer> <silent> ,qj			    j<Esc>:call <SID>Grab()<Cr>
    let b:stlBufStat = '-C-'
  else
    if s:guiSup
      anoremenu	  <silent> &Posting.&Restore<Tab>Alt+P	:sil! call <SID>Undo()<Cr>
      anoremenu	  <silent> &Posting.&Send!<Tab>Alt+Z	:call <SID>Send('asis')<Cr>
      tm Posting.&Restore			    Use this to correct typos...
      tm &Posting.&Send!			    Save and exit
    endif
    nn	  <buffer> <silent> <M-p>		    :sil! call <SID>Undo()<Cr>
    ino	  <buffer> <silent> <M-p>		    <Esc>:sil! call <SID>Undo()<Cr>
    nn	  <buffer> <silent> <M-z>		    :call <SID>Send('asis')<Cr>
    ino	  <buffer> <silent> <M-z>		    <C-O>:call <SID>Send('asis')<Cr>
    let b:stlBufStat = '--P'
  endif
endfunction

function! s:GetArgs()
  let s:args = matchstr(getline('.'), '\%' . (virtcol('.') + 3) . 'v.\{-}\ze\%(\\\@<! \|$\)')
endfunction

function! s:Arg(name)
  if match(s:args, '\\\@<!' . a:name) > -1
    let s:arg = substitute(matchstr(s:args, '\\\@<!' . a:name . '\zs.\{-}\ze\%(\\\@<!/\|$\)'), '\\\([ /]\)', '\1', 'g')
    return 1
  endif
  return 0
endfunction

function! s:AppFootSec()
  function! s:AppFoot()
    exe "norm! m':call searchpair('{', '', '}', 'W')v`'c[" . s:footnote . "]"
    let @" = matchstr(@", '\\\@<! \zs.*\ze}$')
    let top = line('$')
    $ call s:Paste('"', (s:Arg('/1') ? 'nowrap' : 'wrap'), 5)
    exe top . 'norm! 0R[' . s:footnote . ']'
    let s:footnote = s:footnote + 1
  endfunction
  unlet! s:footnote
  1
  while search('{FN', 'W') > 0
    call s:GetArgs()
    let byte = line2byte(line('.')) + col('.') - 1
    if !exists("s:footnote")
      let s:footnote = 1
      $ put =\"{TB\n_________\n\n\"
      exe 'goto ' . byte
    endif
    call s:AppFoot()
    exe 'goto ' . byte
  endwhile
  if exists("s:footnote")
    call setline('$', '}')
  endif
endfunction

function! s:MakeBox() range
  let lb = '|                                                                                                   '
  let tb = '----------------------------------------------------------------------------------------------------'
  function! s:Format(width)
    let tw = s:tw
    let s:tw = s:Max(a:width - 4, s:lenT + 4)
    'a+,'b- d | put! _
    call s:Paste('"', 'wrap_dot', 0) | d
    let s:tw = tw
  endfunction
  exe a:firstline . ' ma a'
  exe a:lastline . ' ma b'
  let s:lenT = 0
  if s:Arg('/t')
    let title  = s:arg
    let s:lenT = strlen(title)
  endif
  if s:Arg('/w')
    call s:Format(s:arg)
  endif
  let lenB = 0
  'a+,'b- g/^/ if col('$') > lenB | let lenB = col('$') | endif
  let len  = s:Max(s:lenT + 7, lenB + 2)
  let dLen = (s:Max(s:lenT + 5, lenB) - lenB) / 2
  let ve0=&ve | let &ve='all' | let wrap0=&wrap | let &l:wrap=0
  let move = s:Down(line("'b") - line("'a") - 2)
  exe "norm! 'aj0\<C-V>" . move . 'I' . strpart(lb, 0, dLen + 2) . '' . len . 'l' . move . 'r|'
  let &ve=ve0 | let &l:wrap=wrap0
  let @" = strpart(tb, 0, len - 1)
  'a put =',' . @\" . '.'
  if exists("title")
    exe 's/\%' . ((len - s:lenT - 3) / 2 + 1) . 'c.\{' . (s:lenT + 4) . '}/\="[ " . title . " ]"/'
  endif
  'b put! ='`' . @\" . '´'
  if s:Arg('/c') | 'a+,'b- call s:Shft((s:cols - len - 2) / 2) | endif
endfunction

function! s:AppSig()
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

function! s:DoFinForm(form, mode)
  let stat0 = 'off' | let stat1 = 'on'
  function! s:SepLines() range
    exe 'sil! ' . a:firstline . ',' . a:lastline . ' g/^\(.\).*\n\1\@!./ put _'
  endfunction
  function! s:SpaceStuff() range
    if s:format_flowed == 1
      exe a:firstline . '+,' . a:lastline . '- call s:Shft(1)'
    endif
  endfunction
  function! s:ProcSec(type, action)
    1
    while search('^{' . a:type, 'W') > 0
      call s:GetArgs()
      ma h | call searchpair('\_^{\%(TB\|BX\)', '', '^}$', 'W') | ma l
      if line("'l") > line("'h") + 1
	exe "'h,'l call " . a:action
	'h+,'l- s/^/@/
      endif
      call setline("'h", '')
      call setline("'l", '')
    endwhile
  endfunction
  function! s:DRefQuoFin(form)
    call s:Attrib('store')
    1 put! _ | $ put _
    let maxLev = 0
    while search('^\x>', 'Wb') > 0
      put _ | ma b
      ?\v^%(\x\>)@!? ma a
      'a+,'b- call s:DExtQuo(v:count > 0 ? v:count : 15)
      if getline('.') !~ '^\x>' | continue | endif
      let maxLev = s:Max('0x' . getline('.')[0] + 0, maxLev)
      if a:form == 'wrap'
        'a+,'b- call s:GQ('quo', 'tqn1')
      endif
      if s:format_flowed == 1
        'a+,'b- s/ *$/ /
        'a+,'b g/\v^%(..\s*)?$/ .-,. s/ $//
      endif
      'a
    endwhile
    call s:Attrib('restore', maxLev - 1)
  endfunction
  let s:restore = '
    \let s:repLedMax = ' . s:repLedMax . ' | 
    \let &l:wrap = ' . &l:wrap . ' | 
    \' . (exists("s:fold") ? 'call s:Fold("out", ' . (&l:fdl + 1) . ') | ' : "") . '
    \exe "' . (a:mode == 'i' ? s:Do('start!', 'start') : '') . '" | 
    \call cursor(' . line('.') . ',' . col('.') . ') | 
    \call s:SetCur(' . winline() . ') | 
    \match none | 
    \call s:SynHigh("' . stat{(exists("b:current_syntax") && b:current_syntax == 'posting_color')} . '") | 
    \call s:ConWin("' . stat{exists("s:authBuf")} . '")'
  unlet! s:fold | setl nosmd nosc fdm& nofen
  %y z
  call s:Frame('save')
  $ put _ | ?\v^%(\x\>)@!.*\S.*•@<!$?+,$ d _
  1 put! _
  call s:AppFootSec()
  call s:ProcSec('TB', 's:SpaceStuff()')
  call s:ProcSec('BX', 's:MakeBox()')
  if !exists("b:stlManFmt")
    if s:format_flowed == 1
      % s/\v^( .{-}) *$/+ \1/
    else
      % s/^ \@=/+/
    endif
  endif
  call s:DRefQuoFin(a:form)
  % call s:Digiquote('off')
  % s/^[>|@+]\@!/\*/
  % call s:SepLines()
  if !exists("b:stlManFmt")
    1
    while search('^\*', 'W') > 0
      norm vap:s/^\*//gv:call s:GQ('txt', 'tqn1')
    endwhile
  else
    % s/^\*//
  endif
  % s/^\_[[:blank:]]*$// | 1 d | $ d
  % s/^[@+]//
  if exists("s:footnote")
    % s/\v\n+%(\n ?_{9}$)@=//
  endif
  exe '1,' . s:Min(s:repLedMax, line('$')) . ' g/•$/ s/\v.$%(\n%(\n.*•$)@=)?//'
  call s:AppSig()
  1?.?;$ v/./d
  call s:Frame('restore')
  call s:Interface('send')
  setl nowrap
  call s:ConWin('off')
  syn clear | unlet! b:current_syntax
  exe 'match Search /.\%>' . s:vcols2 . 'v/'
  norm! gg
  setl smd sc
endfunction

function! s:InitPost(columns, remote)
  function! OpSet(var, val, flag)
    exe 'if exists("b:stlBufStat") && &' . a:var . '=="' . a:val . '" | return "' . a:flag . '" | else | return "" | endif'
  endfunction
  function! VarEx(var, flag)
    return (exists(a:var) ? (a:flag == '-' ? '[' . {a:var} . ']' : a:flag) : '')
  endfunction
  function! s:Prepare(type)
    sil %y v | let @w = ''
    let &lines = (s:cols * s:window_geometry) / 200
    let s:so = winheight(0) / 10
    let &so = s:so
    if a:type == 'new'
      call s:SetTitle('[New]')
      let &co = s:cols
      setl wrap
      call s:SynHigh(s:display_mode_on_startup =~? 'color' ? 'on' : 'off')
      call s:Top()
    else
      if a:type == 'followup'
	let s:tqInitials = 1
      	let s:tpqRepair = (s:server ? 'auto' : 'man')
      	call s:DoInitForm()
	unlet! s:tqInitials
	let s:tpqRepair = 'off'
      	call s:SetTitle()
      else
      	call s:SetTitle('[Postponed]')
      endif
      let &co = s:Columns()
      setl nowrap
      call s:SynHigh(s:display_mode_on_startup =~? 'color' ? 'on' : 'off')
      if s:display_mode_on_startup =~? 'context'
	call s:ConWin('on')
      elseif s:display_mode_on_startup =~? 'folds'
	let arg = matchstr(s:display_mode_on_startup, 'folds(\zs\d\+')
	call s:Fold('out', arg != '' ? arg + 1 : 2)
      endif
      call s:Top()
    endif
    if !s:server | start | endif
  endfunction
  if exists("s:window_position")
    exe 'winpos ' . matchstr(s:window_position, '^\d\+') . ' ' . matchstr(s:window_position, '\d\+$')
  endif
  let s:cols = a:columns
  if s:format_flowed == 0
    let s:tw	 = s:cols
    let s:vcols1 = s:cols + 1
    let s:vcols2 = s:cols + 1
  else
    let s:tw     = s:cols - 1
    let s:vcols1 = s:cols
    let s:vcols2 = s:cols + 1
  endif
  let s:repLedMax = s:attribLineMax
  let s:changedName = 1
  let s:server = (a:remote == '!' ? 1 : 0)
  unlet! b:stlManFmt
  unlet! s:jpMatch s:jpPage
  unlet! s:restTopView
  unlet! s:fold
  unlet! s:tqInitials
  let s:tpqRepair = 'off'
  let s:mailBuf = bufnr('%')
  set cpo&
  setl noswf
  setl ul=-1
  setl nosmd nosc
  set go-=r go-=R go-=l go-=L go-=T
  setl hi=20
  setl bs=2
  setl brk& lbr
  set siso=0
  setl dy=lastline
  setl lcs= nolist
  setl com=fb:-,fb:*,b:\|
  setl ai
  setl et
  setl nojs
  setl ts=2
  setl ww=b,s,<,>,[,]
  setl lz
  setl mfd=100
  setl magic
  setl ls=2
  exe "setl stl=%<%0." . s:Max(10, s:cols - 40) . "t\\ %h%{VarEx('b:stlBufStat','-')}%{VarEx('b:stlLevels','-')}%{VarEx('b:stlManFmt','[FT]')}%{OpSet('wrap','1','[WP]')}%{OpSet('ve','all','[VE]')}\\ %=%l,%c%V\\ %P"
  setl ch=1
  setl shm-=T
  setl lm=none
  setl ws
  set sbo=ver,jump
  setl wak=no
  setl nofen
  setl km= sel=inclusive
  exe 'hi postingVisChar gui=reverse,italic' . s:HlSet('Visual', 'fg') . s:HlSet('Visual', 'bg')
  call s:Interface('compose')
  match None
  if search('\%>' . s:Max((line('$') - 5), 1) . 'l\_^ - Posting v', 'w') > 0
    let id = substitute(matchstr(getline('.'), '/\zs.*\ze -'), '\D', '', 'g') | .-,. d _
    if exists('g:DRAFT' . id)
      exe g:DRAFT{id} | unlet g:DRAFT{id}
    else
      call s:Prepare('postponed')
    endif
  elseif search('^>', 'w') == 0
    call s:Prepare('new')
  else
    call s:Prepare('followup')
  endif
  if exists("s:authBuf") | syncbind | endif
  setl smd sc
  setl ul=1000
endfunction

command! -nargs=1 -bang Post :call s:InitPost(<args>, "<bang>")

sil! call s:InstDoc()

if exists("s:helpDoc")
  echo "Posting v1.3: Installed help-documentation."
endif

let &cpo = s:cpo

" }}}1
" vim600:co=74:fo=crq12:com=n\:\":tw=74:nojs:so=5:siso=15:nowrap:inde=:sts=2:sm:fdm=marker:
