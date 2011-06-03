" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/qfn.vim	[[[1
93
" qfn.vim
" Create file notes in quickfix format
" Last Changed: 2008 April 20
" Author: Will Drewry <redpig@dataspill.org>
" License: See qfn.txt packaged with this file.
" Changes:
" = 2008-April-20
" + Removed forced F8 mapping. Whoops.

if v:version < 700
  echo "qfn.vim requires version 7.0 or higher"
  finish
endif

" Don't double load
if exists("s:loaded")
  finish
endif
let s:loaded = 1

" Add support for W or E
function s:QFNAddQ()
  let txt = input("Enter note: ")
  call s:QFNAdd(txt)
endfunction

function s:QFNAdd(note)
  if a:note == ''
    return
  endif
  let entry = {}
  " may need expand() here.
  let entry["filename"] = bufname("%")
  let entry["lnum"] = line('.')
  let entry["col"] = col('.')
  let entry["vcol"] = ''
  let entry["text"] = a:note
  let entry["type"] = 'E'
  call setqflist([entry], 'a')
endfunction

" Allow saving in a parseable quickfix format
function s:QFNSave(fname)
  let qflist = getqflist()
  let output = []
  for entry in qflist
    let line = bufname(entry.bufnr) . ":" . entry.lnum . ":" . entry.col . ":" . entry.text
    call add(output, line)
  endfor
  call writefile(output, a:fname)
endfunction

" Map these local functions globally with the script id
noremap <unique> <script> <Plug>QuickFixNote <SID>QFNAddQ
noremap <unique> <script> <Plug>QuickFixSave <SID>QFNSave
noremap <SID>QFNAddQ :call <SID>QFNAddQ()<CR>
noremap <SID>QFNSave :call <SID>QFNSave("quickfix.err")<CR>

" Drop them on the menu too
noremenu <script> Plugin.QFN\ Add  <SID>QFNAddQ
noremenu <script> Plugin.QFN\ Save <SID>QFNSave

" ex commands
if !exists(":QFNAdd")
  command -nargs=1 QFNAdd :call s:QFNAdd(<f-args>)
endif

if !exists(":QFNAddQ")
  command -nargs=0 QFNAddQ :call s:QFNAddQ()
endif

if !exists(":QFNSave")
  command -nargs=1 -complete=file QFNSave :call s:QFNSave(<f-args>)
endif

" global commands
"function QFN_Add(text)
"  s:QFNAdd(text)
"endfunction
"
"function QFN_Save(file)
"  s:QFNSave(file)
"endfunction

" basic maps
if !hasmapto('<Plug>QuickFixNote')
  map <unique> <Leader>m <Plug>QuickFixNote
endif

if !hasmapto('<Plug>QuickFixSave')
  map <unique> <Leader>s <Plug>QuickFixSave quickfix.err
endif

doc/qfn.txt	[[[1
205
*qfn.txt*	Plugin for external file annotation

Author: Will Drewry  (redpig AT dataspill DOT org)
For Vim version 7.0 and above
Last change: 2008 April 20

1. Overview 					|qfn-intro|
2. Retrieving QFN				|qfn-online|
3. Requirements					|qfn-requirements|
4. Installation 				|qfn-install|
5. Usage 					|qfn-using|
6. Options 					|qfn-options|
7. Commands 					|qfn-commands|
8. Global functions 				|qfn-functions|
10. FAQ 					|qfn-faq|
11. License 					|qfn-license|
12. Todo					|qfn-todo|

==============================================================================
						*qfn-intro*
1. Overview~

QuickFixNotes, or QFN, is a plugin for Vim that allows file annotation
based on |quickfix| functionality.  In particular, this plugin allows the user
to tie comments to a specific source file, line number, and column number
without modifying the file being reviewed.  The goal of the plugin is to
provide a simple and useful mechanism for tracking notes when reviewing any
sort of text file -- from documents and book drafts to source code and
configuration files.  There are many instances where placing comments into the
file being reviewed is less practical.

Given its simplicity, this plugin only supplies a simple set of features:

    * Add a comment for the given line and column number to the |quickfix| list
    * Save the current |quickfix| list an expected format
    * Provide all functionality without external scripting dependencies

In the future, QFN may support more advanced interfaces to the |quickfix|
list.  Currently removal and modifications must be done on a saved |quickfix|
file.

==============================================================================
						*qfn-online*
2. Retrieving QFN

The home page of the qfn plugin is at:
>
	http://redpig.dataspill.org
<
You can subscribe to the qfn mailing list to post your questions or
suggestions for improvement or to send bug reports. Visit the following page
for subscribing to the mailing list:

==============================================================================
						*qfn-requirements*
3. Requirements~

The qfn plugin requires the following:

    * Vim version 7.0 and above

The qfn plugin will work on all the platforms where Vim is supported and
|quickfix| support is enabled.


==============================================================================
						*qfn-install*
4. Installation~

1. Download the |vimball| from http://vim.org.
2. Run:
>
   vim qfn.vba
<
3. Execute the following ex command:
>
   :so %
<
4. You should now be able to bind <Plug>QuickFixNote and <Plug>QuickFixSave to
   your preferred key combinations.  You can use the ":help qfn" command  at any time
   to get more information.

To uninstall the qfn plugin, you only need to remove the plugin/qfn.vim and
doc/qfn.txt files.

==============================================================================
						*qfn-using*
5. Usage~

The qfn plugin is meant to be used during some sort of file review.  If you
are reading a document, reviewing source code, or anything else where having a
scratch pad for notes associated with a file and line number is useful.


Creating a new note~

Creating notes using qfn is simple.  First, bind the QuickFixNote command to
some keys you prefer.  I've added the following to my $HOME/.vimrc file:
>
	map mm <Plug>QuickFixNote
<
Once you have a mapping in place, you can get started!  Open a file, move to a
line you'd like to comment on, and type 'mm'.  You'll be prompted for your
note. Type it in, press enter, and you're done.

Viewing your notes~
If you'd like to see the notes as they are being taken, simply use the normal
|quickfix| commands.  For example, |copen| will open the |quickfix| window.
You can customize it in any way that is normally allowed.  It is also possible
to map keys to these commands:
>
	noremap <silent> <F8> :copen<CR>
<
You can also open the qfn/|quickfix| window on startup using the following command
line:
>
	$ vim +copen
<

Saving your notes~

If you're taking the time to make these notes, you may also want to keep them
for longer than just the time Vim is running.  If so, you'll need to use the
QuickFixSave command.  This command saves your |quickfix| list, or your notes,
to a file in your local directory named 'quickfix.err'.  I've added the
following to my $HOME/.vimrc for this:
>
	map ms <Plug>QuickFixSave
<
If you add this to your .vimrc, you can save your notes to $PWD/quickfix.err
by hitting 'ms'.

==============================================================================
						*qfn-options*
6. Options~

Currently, nothing has been made configurable. TODO!

==============================================================================
						*qfn-commands*
7. Commands~

The qfn plugin provides the following ex-mode commands:

|:QFNSave|	Saves the |quickfix| window contents
|:QFNAdd|	Adds a new note for the current line
|:QFNAddQ|	Adds a new note for the current line

						*:QFNSave*
:QFNSave {file}
		Saves the |quickfix| list to the specified {file}.
		Examples:
>
		    :QFNSave

						*:QFNAdd*
:QFNAdd {text}
		Add a quickfix entry with the given text
		for the current file, line, and colum.

						*:QFNAddQ*
:QFNAddQ
		Calls QFNAdd() with text derived from
		prompting the user.


==============================================================================
						*qfn-functions*
8. Global functions~

Currently, there are none.

==============================================================================
						*qfn-faq*
10. Frequently Asked Questions~

Q. Uh, hi?
A. Yea. nothing here yet.

==============================================================================
						*qfn-license*
11. License~

Permission is hereby granted to use and distribute the qfn plugin, with or
without modifications, provided that this copyright notice is copied with it
and attribution is included.  Like anything else that's free, qfn.vim is
provided *as is* and comes with no warranty of any kind, either expressed or
implied. In no event will the copyright holder be liable for any damamges
resulting from the use of this software.

==============================================================================
						*qfn-todo*
12. Todo~

* Absolute pathname resolution
* Comment/note deletion
* Comment editing
* Automatic saves
* Customizable save file
* Global functions
* ?

==============================================================================

vim:tw=78:ts=8:noet:ft=help:
