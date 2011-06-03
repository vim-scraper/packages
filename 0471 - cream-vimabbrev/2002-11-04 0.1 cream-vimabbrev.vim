"=====================================================================
" cream-vimabbrev.vim -- Convert Vim script command and option names
"                        between full and abbreviated forms
"
" Cream -- An easy-to-use configuration of the famous Vim text editor
" Copyright (C) 2002  Steve Hall
"
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
"
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
" General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
" 02111-1307, USA.
"
" **********************************************************************
" WARNING:
" This script is very much a work in progress and has several known
" bugs and likely many unknown ones. You will find little automatic
" success with it. It is being provided here for information only. Be
" hereby warned that using these functions will tear valid Vim script
" limb from limb leaving a trail of crying and gnashing of teeth
" behind.
" 
" That said, we would very much appreciate your feedback on correcting
" said anamolies. Feel free to forward suggestions or fixes to:
"   digitect (at) mindspring • com
" **********************************************************************
"
" Description:
" This script is currently one half of a set of functions to convert
" Vim command and option names between short/abbreviated and long/full
" name forms. (Ex. "fu" => "function", "e" => "edit") These are
" sometimes indicated like "fu[nction]" or  "e[dit]".
"
" The script currently only converts from short to long. Based on my
" own preferences, I am less motivated to do the other side. ;) But
" I conceed it would be useful to have a polished routine to help
" going back and forth between the two for those with preferences for
" either readability or bandwidth conservation.
"
" The script contains plenty of working debris at the bottom which
" shouldn't impact the code above. I have tried to be as complete as
" possible by using Vim's own syntax and option lists to compile the
" substitutions from. However you may notice omissions.
"
" Date:    2002-11-04
" Version: 0.1
" Source:  http://vim.sourceforge.net/scripts/script.php?script_id=471
" Author:  Steve Hall  [ digitect@mindspring.com ]
" License: GPL (http://www.gnu.org/licenses/gpl.html)
"
" Dependancies:
" multvals.vim (http://www.vim.org/script.php?script_id=171)
"   by Hari Krishna Dara, for the arrays of items to be substituted.
" 
" Instructions:
" Just drop this file into your plugins directory, make sure you have
" the required dependencies (multvals.vim) also there, and start Vim.
" Then call the functions Cream_vimshort2long() and
" Cream_vimlong2short() from within the file to be converted.

" ToDo:
" * Avoid mapping syntaxes.
" * Avoid heinous "normal af|g^" => "normal af|global^" error
" * Avoid heinous "normal j+r" => "normal j+read" error
" * Add whitespace separators around evaluation operators and
"   concatenation symbols.
"
" Design Requirements:
" * File being converted (current buffer) must have &filetype == "vim"
" * Two functions:
"   » long2short()
"   » short2long()
" * Must handle strings appropriately:
"   » Must be able to search strings, due to execute forms, yet...
"   » Must not modify user strings.
" * Must be case sensitive
" * Should consider Vim keywords only.
"   » Patterns use syntax keywords to demark?
"     - vimCommand
"     - vimOptions
"   » Patterns use whole word to demark?
" * Could consider preceeding ":" character
" * Could be able to handle an entire file and only a selection.
" * Could handle modifiers
"   » !
"
" ChangeLog:
"


function! Cream_vimshort2long()
" convert Vim script abbreviated command and option names to long/complete forms

	let s:save_cpo = &cpo
	set cpoptions&vim

	" restrict to only vim filetypes
	if &filetype != "vim"
		call confirm("Cannot convert a file without a filetype of \"vim\". Please save file with .vim extension. \n\nQuitting...", "&Ok", 1, "Info")
	endif

	" warning
	let n = confirm(
		\ "Warning!\n" .
		\ "\n" .
		\ "Preparing to perform over 2100 complex substitutions. This\n" .
		\ "may take some time. Continue?\n" .
		\ "\n" .
		\ "\n", "&Ok\n&Cancel", 1, "Info")
	if n != 1
		return
	endif

	"----------------------------------------------------------------------
	" commands, special first
	" * prepares later requirements for well-formed "set" or "normal"
	"   precedents
	" * skip if preceded by "normal\s\+"
	" * skip if in or after string (not between a quote and beginning of line)
	" * skip if followed by an open parenthesis

	" :norm[al]
	silent! %substitute/\(^.*["']\{1}.*\)\@<!\(norm\%[al]\s\+\)\@<!\<norm\%[al]\>\(\s*(\)\@!/normal/geI
	" :exe[cute]
	silent! %substitute/\(^.*["']\{1}.*\)\@<!\(norm\%[al]\s\+\)\@<!\<exe\%[cute]\>\(\s*(\)\@!/execute/geI
	" :!{cmd}
	" hmmm...

	"······································································
	" commands, special precedents next
	" * skip if preceded by "normal\s\+"
	" * skip if in or after string (not between a quote and beginning of line)
	"   - except when preceded by  execute "  or  execute '
	" * skip if followed by an open parenthesis

	" :sil[ent]
	silent! %substitute/\(^.*["']\{1}.*\)\@<!\(norm\%[al]\s\+\)\@<!\<sil\%[ent]\>\(\s*(\)\@!/silent/geI
	" :sil[ent] when preceded by  execute "  or  execute '
	silent! %substitute/\(^\s*execute\s\+["']\{1}\)\<sil\%[ent]\>\(\s*(\)\@<!/\1silent/geI
	" :se[t]
	silent! %substitute/\(^.*["']\{1}.*\)\@<!\(norm\%[al]\s\+\)\@<!\<se\%[t]\>\(\s*(\)\@!/set/geI
	" :se[t] when preceded by  execute " , execute ' , execute "silent , or execute 'silent!
	silent! %substitute/\(^\s*execute\s\+["']\{1}\)\(silent\%[!]\=\s\+\)\=\<se\%[t]\>\(\s*(\)\@!/\1\2set/geI
	" :setl[ocal]
	silent! %substitute/\(^.*["']\{1}.*\)\@<!\(norm\%[al]\s\+\)\@<!\<setl\%[ocal]\>\(\s*(\)\@!/setlocal/geI
	" :setl[ocal] when preceded by  execute " , execute ' , execute "silent , or execute 'silent!
	silent! %substitute/\(^\s*execute\s\+["']\{1}\)\(silent\%[!]\=\s\+\)\=\<setl\%[ocal]\>\(\s*(\)\@!/\1\2setlocal/geI

	"······································································
	" commands
	" * must *not* be preceded by "set\s\+" or "setlocal\s\+"
	" * skip if preceded by "normal\s\+"
	" * skip if in or after string (not between a quote and beginning
	"   of line)
	"   - except when preceded by  silent[!]
	" * skip if followed by an open parenthesis
	" * skip if followed by a colon, per buffer/script-scoping prefixes

	" get array of long/short name pairs
	let myvimpairs = Cream_vimabbrev_makepairs_commands()

	" itterate through each pair and do substitutions over file
	let i = 0
	let total = MvNumberOfElements(myvimpairs, "\n")

	" modify "ab[breviate]" form to "ab\%[breviate]"
	let myvimpairs = substitute(myvimpairs, '[', '\\\%[', 'g')

	while i < total
		" get pair
		let mypair = MvElementAt(myvimpairs, "\n", i)
		" get long (before <tab>)
		let mylong = MvElementAt(mypair, "\t", 0)
		" get short (after <tab>)
		let myshort = MvElementAt(mypair, "\t", 1)

		" substitute when not in string or when not preceded by "normal " or "set "
		execute 'silent! %substitute/\(^.*["' . nr2char(39) . ']\{1}.*\)\@<!\(norm\%[al]\s\+\)\@<!\(set\s\+\)\@<!\(setlocal\s\+\)\@<!\<' . myshort . '\>\(:\)\@!\(\s*(\)\@!/' . mylong . '/geI'
		" substitute when in string only if preceded by "execute " and at beginning or also preceded by "silent[!]", as long as no "set " or "setlocal "
		execute 'silent! %substitute/\(^\s*execute\%[!]\s\+\)\{1}\(["' . nr2char(39) . ']\{1}\)\(silent\%[!]\s\+\)\=\(set\s\+\)\@<!\(setlocal\s\+\)\@!\<' . myshort . '\>\(:\)\@!\(\s*(\)\@<!/\1\2\3' . mylong . '/geI'

		let i = i + 1
	endwhile


	"----------------------------------------------------------------------
	" options
	" * *must* be preceded by "set\s\+" or "setlocal\s\+"
	" * skip if followed by an open parenthesis
	" * skip if followed by a colon, per buffer/script-scoping prefixes

	" get array of long/short name pairs
	let myvimpairs = Cream_vimabbrev_makepairs_options()

    " itterate through each pair and do substitutions over file
	let i = 0
	let total = MvNumberOfElements(myvimpairs, "\n")

	" modify "ab[breviate]" form to "ab\%[breviate]"
	let myvimpairs = substitute(myvimpairs, '[', '\\\%[', 'g')

	while i < total
		" get pair
		let mypair = MvElementAt(myvimpairs, "\n", i)
		" get long (before <tab>)
		let mylong = MvElementAt(mypair, "\t", 0)
		" get short (after <tab>)
		let myshort = MvElementAt(mypair, "\t", 1)

		" substitute options preceded by "set " or "setlocal "
		execute 'silent! %substitute/\(\<set\%[local]\>\s\+\)\{1}\<' . myshort . '\>\(:\)\@!\(\s*(\)\@!/\1' . mylong . '/geI'

		let i = i + 1
	endwhile

	let &cpo = s:save_cpo

endfunction

function! Cream_vimlong2short()
	call confirm("Sorry, this function not complete yet!", "&Ok", 1, "Info")
endfunction

" function! Cream_vimabbrev_makepairs_commands() {{{1
function! Cream_vimabbrev_makepairs_commands()
" create a two-dimensional array 'VimPairs', in the form 'long\tshort\n'
" *** Currently 43 pairs of dupes (86 total) commented out ***

	let VimCommandsPairs = ""
	let tab = nr2char(9)

     let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "abbreviate"     . tab .   "ab[breviate]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "abclear"        . tab .   "abc[lear]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "aboveleft"      . tab .   "abo[veleft]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "all"            . tab .   "al[l]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "amenu"          . tab .   "am[enu]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "anoremenu"      . tab .   "an[oremenu]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "argadd"         . tab .   "arga[dd]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "argdelete"      . tab .   "argd[elete]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "argedit"        . tab .   "arge[dit]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "argglobal"      . tab .   "argg[lobal]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "arglocal"       . tab .   "argl[ocal]"   )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "args"           . tab .   "ar[gs]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "argument"       . tab .   "argu[ment]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ascii"          . tab .   "as[cii]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "augroup"        . tab .   "aug[roup]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "aunmenu"        . tab .   "aun[menu]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "autocmd"        . tab .   "au[tocmd]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "badd"           . tab .   "bad[d]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ball"           . tab .   "ba[ll]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bdelete"        . tab .   "bd[elete]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "belowright"     . tab .   "bel[owright]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bfirst"         . tab .   "bf[irst]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "blast"          . tab .   "bl[ast]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bmodified"      . tab .   "bm[odified]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bNext"          . tab .   "bN[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bnext"          . tab .   "bn[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "botright"       . tab .   "bo[tright]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bprevious"      . tab .   "bp[revious]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "break"          . tab .   "brea[k]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "breakadd"       . tab .   "breaka[dd]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "breakdel"       . tab .   "breakd[el]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "breaklist"      . tab .   "breakl[ist]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "brewind"        . tab .   "br[ewind]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "browse"         . tab .   "bro[wse]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "buffer"         . tab .   "b[uffer]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "buffer"         . tab .   "buf[fer]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "buffers"        . tab .   "ls"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bunload"        . tab .   "bun[load]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "bwipeout"       . tab .   "bw[ipeout]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cabbrev"        . tab .   "ca[bbrev]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cabclear"       . tab .   "cabc[lear]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "call"           . tab .   "cal[l]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cclose"         . tab .   "ccl[ose]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "center"         . tab .   "ce[nter]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cfile"          . tab .   "cf[ile]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cfirst"         . tab .   "cfir[st]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "change"         . tab .   "c[hange]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "chdir"          . tab .   "chd[ir]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "checkpath"      . tab .   "che[ckpath]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "checktime"      . tab .   "checkt[ime]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "clast"          . tab .   "cla[st]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "clist"          . tab .   "cl[ist]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "close"          . tab .   "clo[se]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cmap"           . tab .   "cm[ap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cmapclear"      . tab .   "cmapc[lear]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cmenu"          . tab .   "cme[nu]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cnewer"         . tab .   "cnew[er]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cnext"          . tab .   "cn[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cNext"          . tab .   "cN[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cnfile"         . tab .   "cnf[ile]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cnoreabbrev"    . tab .   "cnorea[bbrev]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cnoremap"       . tab .   "cno[remap]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cnoremenu"      . tab .   "cnoreme[nu]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "colder"         . tab .   "col[der]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "colorscheme"    . tab .   "colo[rscheme]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "comclear"       . tab .   "comc[lear]"   )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "command"        . tab .   "com[mand]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "confirm"        . tab .   "cf"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "confirm"        . tab .   "conf[irm]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "continue"       . tab .   "con[tinue]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "copen"          . tab .   "cope[n]"   )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "copy"           . tab .   "co[py]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "copy"           . tab .   "t"      )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cprevious"      . tab .   "cp"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cquit"          . tab .   "cq[uit]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "crewind"        . tab .   "cr[ewind]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cunabbrev"      . tab .   "cuna[bbrev]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cunmap"         . tab .   "cu[nmap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cunmenu"        . tab .   "cunme[nu]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "cwindow"        . tab .   "cw[indow]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "delcommand"     . tab .   "delc[ommand]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "delete"         . tab .   "d[elete]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "delfunction"    . tab .   "delf[unction]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "diffget"        . tab .   "diffg[et]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "diffpatch"      . tab .   "diffp[atch]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "diffput"        . tab .   "diffpu[t]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "diffthis"       . tab .   "difft[his]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "digraphs"       . tab .   "dig[raphs]"    )
     let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "display"        . tab .   "di[splay]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "djump"          . tab .   "dj[ump]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "dlist"          . tab .   "dl[ist]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "doautoall"      . tab .   "doautoa[ll]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "doautocmd"      . tab .   "do[autocmd]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "drop"           . tab .   "dr[op]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "dsearch"        . tab .   "ds[earch]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "dsplit"         . tab .   "dsp[lit]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "echo"           . tab .   "ec[ho]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "echoerr"        . tab .   "echoe[rr]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "echomsg"        . tab .   "echom[sg]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "edit"           . tab .   "e[dit]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "else"           . tab .   "el[se]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "elseif"         . tab .   "elsei[f]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "emenu"          . tab .   "em[enu]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "endfunction"    . tab .   "endf[unction]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "endif"          . tab .   "en[dif]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "endwhile"       . tab .   "endw[hile]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "enew"           . tab .   "ene[w]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "exit"           . tab .   "exi[t]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "file"           . tab .   "f[ile]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "filetype"       . tab .   "filet[ype]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "filetype"       . tab .   "ft"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "find"           . tab .   "fin[d]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "finish"         . tab .   "fini[sh]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "first"          . tab .   "fir[st]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "fixdel"         . tab .   "fix[del]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "fold"           . tab .   "fo"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "foldclose"      . tab .   "foldc[lose]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "folddoclosed"   . tab .   "folddoc[losed]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "folddoopen"     . tab .   "foldd[oopen]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "foldopen"       . tab .   "foldo[pen]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "function"       . tab .   "fu[nction]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "global"         . tab .   "g[lobal]"      )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "goto"           . tab .   "go[to]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "grep"           . tab .   "gr[ep]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "grepadd"        . tab .   "grepa[dd]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "hardcopy"       . tab .   "ha[rdcopy]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "help"           . tab .   "h[elp]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "helpfind"       . tab .   "helpf[ind]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "helptags"       . tab .   "helpt[ags]"  )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "hide"           . tab .   "hid[e]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "highlight"      . tab .   "hi[ghlight]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "history"        . tab .   "his[tory]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "iabbrev"        . tab .   "ia[bbrev]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "iabclear"       . tab .   "iabc[lear]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ijump"          . tab .   "ij[ump]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ilist"          . tab .   "il[ist]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "imap"           . tab .   "im[ap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "imapclear"      . tab .   "imapc[lear]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "imenu"          . tab .   "ime[nu]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "inoreabbrev"    . tab .   "inorea[bbrev]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "inoremap"       . tab .   "ino[remap]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "inoremenu"      . tab .   "inoreme[nu]")
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "isearch"        . tab .   "is[earch]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "isplit"         . tab .   "isp[lit]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "iunabbrev"      . tab .   "iuna[bbrev]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "iunmap"         . tab .   "iu[nmap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "iunmenu"        . tab .   "iunme[nu]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "join"           . tab .   "j[oin]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "jumps"          . tab .   "ju[mps]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "language"       . tab .   "lan[guage]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "last"           . tab .   "la[st]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "lcd"            . tab .   "lc[d]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "lchdir"         . tab .   "lch[dir]"    )
                                                             "      let (no abbreviation)
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "left"           . tab .   "le[ft]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "leftabove"      . tab .   "lefta[bove]"  )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "lmap"           . tab .   "lm[ap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "lmapclear"      . tab .   "lmapc[lear]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "lnoremap"       . tab .   "lno[remap]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "loadview"       . tab .   "lo[adview]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "lunmap"         . tab .   "lu[nmap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "make"           . tab .   "mak[e]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mapclear"       . tab .   "mapc[lear]"   )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mark"           . tab .   "ma[rk]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "match"          . tab .   "mat[ch]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "menu"           . tab .   "me[nu]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "menutranslate"  . tab .   "menut[ranslate]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mkexrc"         . tab .   "mk[exrc]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mksession"      . tab .   "mks[ession]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mkview"         . tab .   "mkvie[w]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mkvimrc"        . tab .   "mkv[imrc]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "mode"           . tab .   "mod[e]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "move"           . tab .   "m[ove]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "next"           . tab .   "n[ext]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "Next"           . tab .   "N[ext]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nmap"           . tab .   "nm[ap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nmapclear"      . tab .   "nmapc[lear]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nmenu"          . tab .   "nme[nu]"    )
     " normal (removed)
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nnoremap"       . tab .   "nn[oremap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nnoremenu"      . tab .   "nnoreme[nu]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nunmap"         . tab .   "nun[map]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "nunmenu"        . tab .   "nunme[nu]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "omap"           . tab .   "om[ap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "omapclear"      . tab .   "omapc[lear]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "omenu"          . tab .   "ome[nu]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "only"           . tab .   "on[ly]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "onoremap"       . tab .   "ono[remap]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "onoremenu"      . tab .   "onoreme[nu]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "open"           . tab .   "o[pen]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "options"        . tab .   "opt[ions]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ounmap"         . tab .   "ou[nmap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ounmenu"        . tab .   "ounme[nu]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "pclose"         . tab .   "pc[lose]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "pedit"          . tab .   "ped[it]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "perl"           . tab .   "pe[rl]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "perldo"         . tab .   "perld[o]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "pop"            . tab .   "po[p]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "popup"          . tab .   "pop[up]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ppop"           . tab .   "pp[op]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "preserve"       . tab .   "pre[serve]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "previous"       . tab .   "prev[ious]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "Print"          . tab .   "P[rint]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "print"          . tab .   "p[rint]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "promptfind"     . tab .   "promptf[ind]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "promptrepl"     . tab .   "promptr[epl]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "psearch"        . tab .   "ps[earch]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptag"           . tab .   "pta[g]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptfirst"        . tab .   "ptf[irst]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptjump"         . tab .   "ptj[ump]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptlast"         . tab .   "ptl[ast]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptNext"         . tab .   "ptN[ext]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptnext"         . tab .   "ptn[ext]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptprevious"     . tab .   "ptp[revious]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptrewind"       . tab .   "ptr[ewind]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ptselect"       . tab .   "pts[elect]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "put"            . tab .   "pu[t]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "pwd"            . tab .   "pw[d]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "pyfile"         . tab .   "pyf[ile]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "python"         . tab .   "py[thon]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "qall"           . tab .   "qa[ll]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "quit"           . tab .   "q[uit]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "quitall"        . tab .   "quita[ll]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n", "noreabbrev"       . tab . "norea[bbrev]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "read"           . tab .   "r[ead]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "recover"        . tab .   "rec[over]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "redir"          . tab .   "redi[r]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "redo"           . tab .   "red[o]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "redraw"         . tab .   "redr[aw]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "registers"      . tab .   "reg[isters]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n", "noremenu"         . tab . "noreme[nu]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n", "noremap"          . tab . "no[remap]"       )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "resize"         . tab .   "res[ize]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "retab"          . tab .   "ret[ab]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "return"         . tab .   "retu[rn]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "rewind"         . tab .   "rew[ind]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "right"          . tab .   "ri[ght]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "rightbelow"     . tab .   "rightb[elow]" )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "ruby"           . tab .   "rub[y]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "rubydo"         . tab .   "rubyd[o]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "rubyfile"       . tab .   "rubyf[ile]"  )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "runtime"        . tab .   "ru[ntime]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "rviminfo"       . tab .   "rv[iminfo]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sall"           . tab .   "sal[l]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sargument"      . tab .   "sa[rgument]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sargument"      . tab .   "sar[gument]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "saveas"         . tab .   "sav[eas]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sball"          . tab .   "sba[ll]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbfirst"        . tab .   "sbf[irst]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sblast"         . tab .   "sbl[ast]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbmodified"     . tab .   "sbm[odified]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbNext"         . tab .   "sbN[ext]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbnext"         . tab .   "sbn[ext]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbprevious"     . tab .   "sbp[revious]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbrewind"       . tab .   "sbr[ewind]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sbuffer"        . tab .   "sb[uffer]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "scriptencoding" . tab .   "scripte[ncoding]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "scriptnames"    . tab .   "scrip[tnames]"  )
	" set (removed)
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "setfiletype"    . tab .   "setf[iletype]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "setglobal"      . tab .   "setg[lobal]"   )
	" setlocal (removed)
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sfind"          . tab .   "sf[ind]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sfirst"         . tab .   "sfir[st]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "silent"         . tab .   "sil[ent]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "simalt"         . tab .   "si[malt]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "slast"          . tab .   "sla[st]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sleep"          . tab .   "sl[eep]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "smagic"         . tab .   "sm[agic]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sNext"          . tab .   "sN[ext]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "snext"          . tab .   "sn[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sniff"          . tab .   "sni[ff]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "snomagic"       . tab .   "sno[magic]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "source"         . tab .   "so[urce]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "split"          . tab .   "sp[lit]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sprevious"      . tab .   "spr[evious]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "srewind"        . tab .   "sr[ewind]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "stag"           . tab .   "sta[g]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "startinsert"    . tab .   "star[tinsert]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "stjump"         . tab .   "stj[ump]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "stop"           . tab .   "st[op]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "stselect"       . tab .   "sts[elect]"    )
     let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "substitute"     . tab .   "s[ubstitute]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sunhide"        . tab .   "sun[hide]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "suspend"        . tab .   "sus[pend]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "sview"          . tab .   "sv[iew]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "syntax"         . tab .   "sy[ntax]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tag"            . tab .   "ta[g]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tcl"            . tab .   "tc[l]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tcldo"          . tab .   "tcld[o]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tclfile"        . tab .   "tclf[ile]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tearoff"        . tab .   "te[aroff]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tfirst"         . tab .   "tf[irst]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tjump"          . tab .   "tj[ump]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tlast"          . tab .   "tl[ast]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tmenu"          . tab .   "tm[enu]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tnext"          . tab .   "tn[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tNext"          . tab .   "tN[ext]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "topleft"        . tab .   "to[pleft]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tprevious"      . tab .   "tp[revious]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "trewind"        . tab .   "tr[ewind]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tselect"        . tab .   "ts[elect]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "tunmenu"        . tab .   "tu[nmenu]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "unabbreviate"   . tab .   "una[bbreviate]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "undo"           . tab .   "u[ndo]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "unhide"         . tab .   "unh[ide]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "unlet"          . tab .   "unl[et]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "unmap"          . tab .   "unm[ap]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "unmenu"         . tab .   "unme[nu]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "update"         . tab .   "up[date]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "verbose"        . tab .   "verb[ose]"   )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "version"        . tab .   "ve[rsion]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vertical"       . tab .   "vert[ical]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vglobal"        . tab .   "v[global]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "view"           . tab .   "vie[w]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "visual"         . tab .   "vi[sual]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vmap"           . tab .   "vm[ap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vmapclear"      . tab .   "vmapc[lear]"  )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vmenu"          . tab .   "vme[nu]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vnew"           . tab .   "vne[w]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vnoremap"       . tab .   "vn[oremap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vnoremenu"      . tab .   "vnoreme[nu]")
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vsplit"         . tab .   "vs[plit]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vunmap"         . tab .   "vu[nmap]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "vunmenu"        . tab .   "vunme[nu]"  )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wall"           . tab .   "wa[ll]"     )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "while"          . tab .   "wh[ile]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wincmd"         . tab .   "winc[md]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "winpos"         . tab .   "winp[os]"   )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "winsize"        . tab .   "win[size]"    )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wnext"          . tab .   "wn[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wNext"          . tab .   "wN[ext]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wprevous"       . tab .   "wp[revous]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wqall"          . tab .   "wqa[ll]"    )
	 " dupe
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wsverb"         . tab .   "ws[verb]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "wviminfo"       . tab .   "wv[iminfo]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "xall"           . tab .   "xa[ll]"     )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "xit"            . tab .   "x[it]"      )
	 let VimCommandsPairs = MvAddElement(VimCommandsPairs, "\n",   "yank"           . tab .   "y[ank]"      )

	return VimCommandsPairs

endfunction

" function! Cream_vimabbrev_makepairs_options() {{{1
function! Cream_vimabbrev_makepairs_options()

	let VimOptionsPairs = ""
	let tab = nr2char(9)

"*** Please don't re-tab this! ***
    "let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "aleph"          . tab .   "al[eph]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "allowrevins"    . tab .   "ari"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noallowrevins"    . tab . "noari"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "altkeymap"      . tab .   "akm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noaltkeymap"      . tab . "noakm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "autoindent"     . tab .   "ai"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noautoindent"     . tab . "noai"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "autoread"       . tab .   "ar"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noautoread"       . tab . "noar"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "autowrite"      . tab .   "aw"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noautowrite"      . tab . "noaw"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "autowriteall"   . tab .   "awa"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noautowriteall"   . tab . "noawa"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "background"     . tab .   "bg"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "backspace"      . tab .   "bs"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "backup"         . tab .   "bk"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nobackup"         . tab . "nobk"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "backupcopy"     . tab .   "bkc"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "backupdir"      . tab .   "bdir"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "backupext"      . tab .   "bex"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "backupskip"     . tab .   "bsk"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "balloondelay"   . tab .   "bdlay"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ballooneval"    . tab .   "beval"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noballooneval"    . tab . "nobeval"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "binary"         . tab .   "bin[ary]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nobinary"         . tab . "nobin[ary]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "bioskey"        . tab .   "biosk[ey]"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nobioskey"        . tab . "nobiosk[ey]"  )
                                                           "      bomb
                                                           "    nobomb
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "breakat"        . tab .   "brk"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "browsedir"      . tab .   "bsdir"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "bufhidden"      . tab .   "bh"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "buflisted"      . tab .   "bl"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "buftype"        . tab .   "bt"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cdpath"         . tab .   "cd[path]"     )
                                                           "      cedit
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "charconvert"    . tab .   "ccv"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cindent"        . tab .   "cin[dent]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nocindent"        . tab . "nocin[dent]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cinkeys"        . tab .   "cink[eys]"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cinoptions"     . tab .   "cino[ptions]"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cinwords"       . tab .   "cinw[ords]"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "clipboard"      . tab .   "cb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cmdheight"      . tab .   "ch"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cmdwinheight"   . tab .   "cwh"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "columns"        . tab .   "co[lumns]"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "comments"       . tab .   "com"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "commentstring"  . tab .   "cms"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "compatible"     . tab .   "cp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nocompatible"     . tab . "nocp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "complete"       . tab .   "cpt"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "confirm"        . tab .   "conf[irm]"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noconfirm"        . tab . "nocf"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "conskey"        . tab .   "consk[ey]"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noconskey"        . tab . "noconsk[ey]"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cpoptions"      . tab .   "cpo[ptions]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cscopepathcomp" . tab .   "cspc"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cscopeprg"      . tab .   "csprg"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cscopetag"      . tab .   "cst"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nocscopetag"      . tab . "nocst"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cscopetagorder" . tab .   "csto"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "cscopeverbose"  . tab .   "csverb" )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nocscopeverbose"  . tab . "nocsverb" )
                                                           "      debug
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "define"         . tab .   "def[ine]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "delcombine"     . tab .   "deco"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "dictionary"     . tab .   "dict[ionary]"   )
                                                           "      diff
                                                           "    nodiff
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "diffexpr"       . tab .   "dex"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "diffopt"        . tab .   "dip"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "digraph"        . tab .   "dg"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nodigraph"        . tab . "nodg"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "directory"      . tab .   "dir[ectory]"    )
                                                           "    nodisable
	 let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "display"        . tab .   "dy"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "eadirection"    . tab .   "ead[irection]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "edcompatible"   . tab .   "ed[compatible]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noedcompatible"   . tab . "noed[compatible]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "encoding"       . tab .   "enc[oding]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "endofline"      . tab .   "eol"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noendofline"      . tab . "noeol"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "equalalways"    . tab .   "ea"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noequalalways"    . tab . "noea"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "equalprg"       . tab .   "ep"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "errorbells"     . tab .   "eb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noerrorbells"     . tab . "noeb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "errorfile"      . tab .   "ef"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "errorformat"    . tab .   "efm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "esckeys"        . tab .   "ek"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noesckeys"        . tab . "noek"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "eventignore"    . tab .   "ei"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "expandtab"      . tab .   "et"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noexpandtab"      . tab . "noet"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "exrc"           . tab .   "ex"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noexrc"           . tab . "noex"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "fileencoding"   . tab .   "fenc"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "fileencodings"  . tab .   "fencs"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "fileformat"     . tab .   "ff"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "fileformats"    . tab .   "ffs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "filetype"       . tab .   "fi"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "fillchars"      . tab .   "fcs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "fkmap"          . tab .   "fk[map]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nofkmap"          . tab . "nofk[map]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldclose"      . tab .   "fcl"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldcolumn"     . tab .   "fdc"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldenable"     . tab .   "fen"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nofoldenable"     . tab . "nofen"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldexpr"       . tab .   "fde"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldignore"     . tab .   "fdi"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldlevel"      . tab .   "fdl"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldlevelstart" . tab .   "fdls"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldmarker"     . tab .   "fmr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldmethod"     . tab .   "fdm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldminlines"   . tab .   "fml"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldnestmax"    . tab .   "fdn"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldopen"       . tab .   "fdo"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "foldtext"       . tab .   "fdt"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "formatoptions"  . tab .   "fo[rmatoptions]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "formatprg"      . tab .   "fp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "gdefault"       . tab .   "gd[efault]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nogdefault"       . tab . "nogd[efault]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "grepformat"     . tab .   "gfm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "grepprg"        . tab .   "gp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "guicursor"      . tab .   "gcr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "guifont"        . tab .   "gfn"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "guifontset"     . tab .   "gfs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "guifontwide"    . tab .   "gfw"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "guiheadroom"    . tab .   "ghr"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "guioptions"     . tab .   "go"     )
                                                           "      guipty
                                                           "    noguipty
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "helpfile"       . tab .   "hf"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "helpheight"     . tab .   "hh"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "hidden"         . tab .   "hid[den]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nohidden"         . tab . "nohid[den]"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "highlight"      . tab .   "hl"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "history"        . tab .   "hi[story]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "hkmap"          . tab .   "hk[map]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nohkmap"          . tab . "nohk[map]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "hkmapp"         . tab .   "hkp"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nohkmapp"         . tab . "nohkp"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "hlsearch"       . tab .   "hls[earch]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nohlsearch"       . tab . "nohls[earch]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ignorecase"     . tab .   "ic"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noignorecase"     . tab . "noic"     )
                                                           "      icon
                                                           "    noicon
                                                           "      iconstring
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "imactivatekey"  . tab .   "imak"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "imcmdline"      . tab .   "imc[mdline]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noimcmdline"      . tab . "noimc[mdline]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "imdisable"      . tab .   "imd[isable]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noimdisable"      . tab . "noimd[isable]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "iminsert"       . tab .   "imi[nsert]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "imsearch"       . tab .   "ims[earch]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "include"        . tab .   "inc[lude]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "includeexpr"    . tab .   "ine"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "includeexpr"    . tab .   "inex"   )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "incsearch"      . tab .   "is"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noincsearch"      . tab . "nois"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "indentexpr"     . tab .   "inde[ntexpr]"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "indentkeys"     . tab .   "indk"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "infercase"      . tab .   "inf[ercase]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noinfercase"      . tab . "noinf[ercase]"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "insertmode"     . tab .   "im"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noinsertmode"     . tab . "noim"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "isfname"        . tab .   "isf[name]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "isident"        . tab .   "isi[dent]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "iskeyword"      . tab .   "isk[eyword]"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "isprint"        . tab .   "isp[rint]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "joinspaces"     . tab .   "js"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nojoinspaces"     . tab . "nojs"     )
                                                           "      key
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "keymap"         . tab .   "kmp"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "keymodel"       . tab .   "km"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "keywordprg"     . tab .   "kp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "langmap"        . tab .   "lmap"   )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "langmenu"       . tab .   "lm"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "laststatus"     . tab .   "ls"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "lazyredraw"     . tab .   "lz"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nolazyredraw"     . tab . "nolz"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "linebreak"      . tab .   "lbr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nolinebreak"      . tab . "nolbr"    )
                                                           "      lines
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "linespace"      . tab .   "lsp"    )
                                                           "      lisp
                                                           "    nolisp
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "lispwords"      . tab .   "lw"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "list"           . tab .   "l[ist]"      )
                                                           "    nolist
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "listchars"      . tab .   "lcs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "loadplugins"    . tab .   "lpl"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noloadplugins"    . tab . "nolpl"    )
                                                           "      magic
                                                           "    nomagic
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "makeef"         . tab .   "mef"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "makeprg"        . tab .   "mp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "matchpairs"     . tab .   "mps"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "matchtime"      . tab .   "mat[chtime]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "maxfuncdepth"   . tab .   "mfd"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "maxmapdepth"    . tab .   "mmd"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "maxmem"         . tab .   "mm"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "maxmemtot"      . tab .   "mmt"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "menuitems"      . tab .   "mis"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "modeline"       . tab .   "ml"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nomodeline"       . tab . "noml"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "modelines"      . tab .   "mls"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "modifiable"     . tab .   "ma"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nomodifiable"     . tab . "noma"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "modified"       . tab .   "mod[ified]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nomodified"       . tab . "nomod[ified]"    )
                                                           "      more
                                                           "    nomore
                                                           "      mouse
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "mousefocus"     . tab .   "mousef[ocus]" )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nomousefocus"     . tab . "nomousef[ocus]" )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "mousehide"      . tab .   "mh"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nomousehide"      . tab . "nomh"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "mousemodel"     . tab .   "mousem[odel]" )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "mouseshape"     . tab .   "mouses[hape]" )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "mousetime"      . tab .   "mouset[ime]" )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "nrformats"      . tab .   "nf"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "number"         . tab .   "nu[mber]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nonumber"         . tab . "nonu[mber]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "osfiletype"     . tab .   "oft"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "paragraphs"     . tab .   "para[graphs]"   )
                                                           "      paste
                                                           "    nopaste
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "pastetoggle"    . tab .   "pt"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "patchexpr"      . tab .   "pex"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "patchmode"      . tab .   "pm"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "path"           . tab .   "pa[th]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "previewheight"  . tab .   "pvh"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "previewwindow"  . tab .   "pvw"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nopreviewwindow"  . tab . "nopvw"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "printdevice"    . tab .   "pdev"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "printexpr"      . tab .   "pexpr"  )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "printfont"      . tab .   "pfn"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "printheader"    . tab .   "pheader")
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "printoptions"   . tab .   "popt"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "readonly"       . tab .   "ro"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noreadonly"       . tab . "noro"     )
                                                           "      remap
                                                           "      report
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "restorescreen"  . tab .   "rs"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "norestorescreen"  . tab . "nors"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "revins"         . tab .   "ri"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "norevins"         . tab . "nori"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "rightleft"      . tab .   "rl"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "norightleft"      . tab . "norl"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ruler"          . tab .   "ru[ler]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noruler"          . tab . "noru[ler]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "rulerformat"    . tab .   "ruf"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "runtimepath"    . tab .   "rtp"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "scroll"         . tab .   "scr[oll]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "scrollbind"     . tab .   "scb"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noscrollbind"     . tab . "noscb"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "scrolljump"     . tab .   "sj"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "scrolloff"      . tab .   "so"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "scrollopt"      . tab .   "sbo"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "sections"       . tab .   "sect[ions]"   )
                                                           "      secure
                                                           "    nosecure
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "selection"      . tab .   "sel[ection]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "selectmode"     . tab .   "slm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "sessionoptions" . tab .   "ssop"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shell"          . tab .   "sh[ell]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shellcmdflag"   . tab .   "shcf"   )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shellpipe"      . tab .   "sp"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shellquote"     . tab .   "shq"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shellredir"     . tab .   "srr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shellslash"     . tab .   "ssl"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshellslash"     . tab . "nossl"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shelltype"      . tab .   "st"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shellxquote"    . tab .   "sxq"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shiftround"     . tab .   "sr"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshiftround"     . tab . "nosr"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shiftwidth"     . tab .   "sw"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shortmess"      . tab .   "shm"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "shortname"      . tab .   "sn"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshortname"      . tab . "nosn"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "showbreak"      . tab .   "sbr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "showcmd"        . tab .   "sc"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshowcmd"        . tab . "nosc"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "showfulltag"    . tab .   "sft"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshowfulltag"    . tab . "nosft"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "showmatch"      . tab .   "sm"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshowmatch"      . tab . "nosm"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "showmode"       . tab .   "smd"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noshowmode"       . tab . "nosmd"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "sidescroll"     . tab .   "ss"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "sidescrolloff"  . tab .   "siso"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "smartcase"      . tab .   "scs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nosmartcase"      . tab . "noscs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "smartindent"    . tab .   "si"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nosmartindent"    . tab . "nosi"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "smarttab"       . tab .   "sta"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nosmarttab"       . tab . "nosta"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "softtabstop"    . tab .   "sts"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "splitbelow"     . tab .   "sb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nosplitbelow"     . tab . "nosb"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "splitright"     . tab .   "spr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nosplitright"     . tab . "nospr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "startofline"    . tab .   "sol"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nostartofline"    . tab . "nosol"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "statusline"     . tab .   "stl"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "suffixes"       . tab .   "su[ffixes]"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "suffixesadd"    . tab .   "sua"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "swapfile"       . tab .   "swf"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noswapfile"       . tab . "noswf"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "swapsync"       . tab .   "sws"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "switchbuf"      . tab .   "swb"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "syntax"         . tab .   "syn[tax]"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "tabstop"        . tab .   "ts"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "tagbsearch"     . tab .   "tbs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notagbsearch"     . tab . "notbs"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "taglength"      . tab .   "tl"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "tagrelative"    . tab .   "tr"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notagrelative"    . tab . "notr"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "tags"           . tab .   "tag[s]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "tagstack"       . tab .   "tgst"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notagstack"       . tab . "notgst"   )
                                                           "      term
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "termencoding"   . tab .   "tenc"   )
                                                           "      terse
                                                           "    noterse
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "textauto"       . tab .   "ta"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notextauto"       . tab . "nota"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "textmode"       . tab .   "tx"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notextmode"       . tab . "notx"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "textwidth"      . tab .   "tw"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "thesaurus"      . tab .   "tsr"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "tildeop"        . tab .   "top"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notildeop"        . tab . "notop"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "timeout"        . tab .   "to"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "notimeout"        . tab . "noto"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "timeoutlen"     . tab .   "tm"     )
                                                           "      title
                                                           "    notitle
                                                           "      titlelen
                                                           "      titleold
                                                           "      titlestring
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "toolbar"        . tab .   "tb"     )
                                                           "      ttimeout
                                                           "    nottimeout
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ttimeoutlen"    . tab .   "ttm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ttybuiltin"     . tab .   "tbi"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nottybuiltin"     . tab . "notbi"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ttyfast"        . tab .   "tf"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nottyfast"        . tab . "notf"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ttymouse"       . tab .   "ttym[ouse]"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ttyscroll"      . tab .   "tsl"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "ttytype"        . tab .   "tty[type]"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "undolevels"     . tab .   "ul"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "updatecount"    . tab .   "uc"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "updatetime"     . tab .   "ut"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "verbose"        . tab .   "vbs"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "viewdir"        . tab .   "vdir"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "viewoptions"    . tab .   "vop"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "viminfo"        . tab .   "vi[minfo]"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "virtualedit"    . tab .   "ve"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "visualbell"     . tab .   "vb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "novisualbell"     . tab . "novb"     )
                                                           "      warn
                                                           "    nowarn
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "weirdinvert"    . tab .   "wiv"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "noweirdinvert"    . tab . "nowiv"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "whichwrap"      . tab .   "ww"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wildchar"       . tab .   "wc"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wildcharm"      . tab .   "wcm"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wildignore"     . tab .   "wig"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wildmenu"       . tab .   "wmnu"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nowildmenu"       . tab . "nowmnu"   )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wildmode"       . tab .   "wim"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "winaltkeys"     . tab .   "wak"    )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "winheight"      . tab .   "wh"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "winminheight"   . tab .   "wmh"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "winminwidth"    . tab .   "wmw"    )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "winwidth"       . tab .   "wiw"    )
                                                           "      wrap
                                                           "    nowrap
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wrapmargin"     . tab .   "wm"     )
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "wrapscan"       . tab .   "ws"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nowrapscan"       . tab . "nows"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "write"          . tab .   "w[rite]"      )
                                                           "    nowrite
	 " dupe
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "writeany"       . tab .   "wa"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nowriteany"       . tab . "nowa"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "writebackup"    . tab .   "wb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n", "nowritebackup"    . tab . "nowb"     )
     let VimOptionsPairs = MvAddElement(VimOptionsPairs, "\n",   "writedelay"     . tab .   "wd"     )

	return VimOptionsPairs

endfunction 1}}}

" Test suite {{{1

fun TestCow0()
" yikes, not really!
fini

	" strings (these shouldn't be changed)
	let test = "normal fu"
	let test = "norm fu"
	"normal fu"

	" second words should *not* get lengthened
	normal se
	norm se
	normal is
	norm is
	normal norm
	norm norm

	" commented should not change
	"normal exe
	"norm exe

	" silent in execute strings
	exe "sil au VimEnter * call TestCow()"
	exe "sil! au VimEnter * call TestCow()"
	exe 'sil au VimEnter * call TestCow()'
	exe 'sil! au VimEnter * call TestCow()'
	sil cow
	sil! cow
	sile cow
	silen cow
	silent cow

	" commands
	ec "fu"
	let fu="fu fun func funct functi functio function"
	let fun="is ise isea isear isearc isearch"
	ech fu
	execute "function TestCowBob()|endfunction"
	execute "sil fu TestCowBob()|endfunction"
	execute "sil! fun TestCowBob()|endfunction"
	execute "silen! func TestCowBob()|endfunction"

	" errors, don't lengthen
	execute " sil "
	execute "silent! sil"
	execute "set fu"
	execute "set norm"
	execute "silent set fu"
	execute "silent! set fu"
	execute "silent set norm"

	" set
	se noinsertmode
	se noim
	se insertmode
	se im

	" options in execute strings
	execute "se ai"
	execute "se go+=a"
	execute 'se ai'
	execute 'se go+=a'
	execute "sil se go+=a"
	execute "sil! se go+=a"
	execute 'sil se go+=a'
	execute 'sil! se go+=a'

endf

" 1}}}

"======================================================================
" STOP!!! Debris beyond...
finish
"======================================================================

"" commands {{{1

"abbreviate            ab
"abclear               abc
"aboveleft             abo
"all                   al
"argadd                arga
"argdelete             argd
"argdo
"argedit               arge
"argglobal             argg
"arglocal              argl
"args                  ar
"argument              argu
"ascii                 as
"badd                  bad
"ball                  ba
"bdelete               bd
"belowright            bel
"bfirst                bf
"blast                 bl
"bmodified             bm
"bmodified             bmod
"bnext                 bn
"bNext                 bN
"botright              bo
"bprevious             bp
"break                 brea
"breakadd              breaka
"breakdel              breakd
"breaklist             breakl
"brewind               br
"browse                bro
"bufdo
"buffer                b
"buffer                buf
"buffers
"buffers               ls
"bunload               bun
"bwipeout              bw
"cabbrev               ca
"cabclear              cabc
"call                  cal
"cc
"cclose                ccl
"cd
"center                ce
"cfile                 cf
"cfirst                cfir
"change                c
"chdir                 chd
"checkpath             che
"checktime             checkt
"clast                 cla
"clist                 cl
"close                 clo
"cmapclear             cmapc
"cnewer                cnew
"cnext                 cn
"cNext                 cN
"cnfile                cnf
"cnoreabbrev           cnorea
"colder                col
"colorscheme           colo
"comclear              comc
"command               com
"compiler
"confirm               conf
"continue              con
"copen                 cope
"copy                  co
"copy                  t
"cprevious             cp
"cquit                 cq
"crewind               cr
"cunabbrev             cuna
"cunmap                cu
"cwindow               cw
"delcommand            delc
"delcommand            delcom
"delete                d
"delete                de
"delete                del
"DeleteFirst
"delfunction           delf
"diffget               diffg
"diffpatch             diffp
"diffput               diffpu
"diffsplit
"diffthis              difft
"digraphs              dig
"display               di
"djump                 dj
"dlist                 dl
"drop                  dr
"dsearch               ds
"dsplit                dsp
"echoerr               echoe
"echomsg               echom
"echon
"edit                  e
"else                  el
"elseif                elsei
"emenu
"emenu                 em
"endfunction           endf
"endif                 en
"endwhile              endw
"enew                  ene
"ex
"exit                  exi
"file                  f
"files
"filetype              fi
"filetype              filet
"find                  fin
"finish                fini
"first                 fir
"fixdel                fix
"fold                  fo
"foldclose             foldc
"folddoclosed          folddoc
"folddoopen            foldd
"foldopen              foldo
"function              fu
"global                g
"goto                  go
"grep                  gr
"grepadd               grepa
"hardcopy              ha
"help                  h
"helpfind              helpf
"helptags              helpt
"hide                  hid
"highlight             hi
"history               his
"iabbrev               ia
"iabclear              iabc
"if
"ijump                 ij
"ilist                 il
"imapclear             imapc
"inoreabbrev           inorea
"isearch               is
"isplit                isp
"iunabbrev             iuna
"iunmap                iu
"join                  j
"jumps                 ju
"k
"language              lan
"last                  la
"lcd                   lc
"lchdir                lch
"left                  le
"leftabove             lefta
"list                  l
"lmap                  lm
"lmapclear             lmapc
"lnoremap              lno
"loadview              lo
"ls
"lunmap                lu
"make                  mak
"mark                  ma
"marks
"match                 mat
"menutranslate         menut
"mkexrc                mk
"mksession             mks
"mkview                mkvie
"mkvimrc               mkv
"mode                  mod
"move                  m
"new
"next                  n
"Next                  N
"nmapclear             nmapc
"nohlsearch            noh
"noreabbrev            norea
"normal                norm
"Nread
"number                nu
"nunmap                nun
"Nw
"omapclear             omapc
"only                  on
"open                  o
"options               opt
"ounmap                ou
"pclose                pc
"pedit                 ped
"perl                  pe
"perldo                perld
"pop                   po
"popup                 pop
"ppop                  pp
"preserve              pre
"previous              prev
"print                 p
"Print                 P
"prompt
"promptfind            promptf
"promptrepl            promptr
"psearch               ps
"ptag                  pta
"ptfirst               ptf
"ptjump                ptj
"ptlast                ptl
"ptnext                ptn
"ptNext                ptN
"ptprevious            ptp
"ptrewind              ptr
"ptselect              pts
"put                   pu
"pwd                   pw
"pyfile                pyf
"python                py
"qall                  qa
"quit                  q
"quitall               quita
"read                  r
"recover               rec
"redir                 redi
"redo                  red
"redraw                redr
"registers             reg
"resize                res
"retab                 ret
"return                retu
"rewind                rew
"right                 ri
"rightbelow            rightb
"ruby                  rub
"rubydo                rubyd
"rubyfile              rubyf
"runtime               ru
"rviminfo              rv
"sall                  sal
"sargument             sa
"sargument             sar
"saveas                sav
"sball                 sba
"sbfirst               sbf
"sblast                sbl
"sbmodified            sbm
"sbmodified            sbmod
"sbnext                sbn
"sbNext                sbN
"sbprevious            sbp
"sbrewind              sbr
"sbuffer               sb
"scriptencoding        scripte
"scriptnames           scrip
"set                   se
"setfiletype           setf
"setglobal             setg
"setlocal              setl
"sfind                 sf
"sfirst                sfir
"shell                 sh
"sign
"silent                sil
"simalt                si
"slast                 sla
"sleep                 sl
"smagic                sm
"snext                 sn
"sNext                 sN
"sniff                 sni
"snomagic              sno
"source                so
"split                 sp
"sprevious             spr
"srewind               sr
"stag                  sta
"startinsert           star
"stjump                stj
"stop                  st
"stselect              sts
"sunhide               sun
"suspend               sus
"sview                 sv
"syncbind
"tag                   ta
"tags
"tcl                   tc
"tcldo                 tcld
"tclfile               tclf
"tearoff               te
"tfirst                tf
"tjump                 tj
"tlast                 tl
"tmenu                 tm
"tnext                 tn
"tNext                 tN
"topleft               to
"tprevious             tp
"trewind               tr
"tselect               ts
"tunmenu               tu
"unabbreviate          una
"undo                  u
"unhide                unh
"unmap                 unm
"update                up
"U
"verbose               verb
"version               ve
"vertical              vert
"vglobal               v
"view                  vie
"visual                vi
"vmapclear             vmapc
"vnew                  vne
"vsplit                vs
"vunmap                vu
"wall                  wa
"while                 wh
"win
"wincmd                winc
"windo
"winpos
"winpos                winp
"winsize               win
"wNext                 wN
"wnext                 wn
"wprevous              wp
"wq
"wqall                 wqa
"write                 w
"wsverb                ws
"wviminfo              wv
"X
"xall                  xa
"xit                   x
"yank                  y
" 1}}}

"" let {{{1

"let
"unlet                 unl

"" autocommands {{{1

"autocmd               au
"augroup               aug
"doautocmd             do
"doautoall             doautoa

"" options {{{1

"  aleph               al
"  allowrevins         ari
"noallowrevins       noari
"  altkeymap           akm
"noaltkeymap         noakm
"  autoindent          ai
"noautoindent        noai
"  autoread            ar
"noautoread          noar
"  autowrite           aw
"noautowrite         noaw
"  autowriteall        awa
"noautowriteall      noawa
"  background          bg
"  backspace           bs
"  backup              bk
"nobackup            nobk
"  backupcopy          bkc
"  backupdir           bdir
"  backupext           bex
"  backupskip          bsk
"  balloondelay        bdlay
"  ballooneval         beval
"noballooneval       nobeval
"  binary              bin
"nobinary            nobin
"  bioskey             biosk
"nobioskey           nobiosk
"  bomb
"nobomb
"  breakat             brk
"  browsedir           bsdir
"  bufhidden           bh
"  buflisted           bl
"  buftype             bt
"  cdpath              cd
"  cedit
"  charconvert         ccv
"  cindent             cin
"nocindent           nocin
"  cinkeys             cink
"  cinoptions          cino
"  cinwords            cinw
"  clipboard           cb
"  cmdheight           ch
"  cmdwinheight        cwh
"  columns             co
"  comments            com
"  commentstring       cms
"  compatible          cp
"nocompatible        nocp
"  complete            cpt
"  confirm             cf
"noconfirm           nocf
"  conskey             consk
"noconskey           noconsk
"  cpoptions           cpo
"  cscopepathcomp      cspc
"  cscopeprg           csprg
"  cscopetag           cst
"nocscopetag         nocst
"  cscopetagorder      csto
"  cscopeverbose       csverb
"nocscopeverbose     nocsverb
"  debug
"  define              def
"  delcombine          deco
"  dictionary          dict
"  diff
"nodiff
"  diffexpr            dex
"  diffopt             dip
"  digraph             dg
"nodigraph           nodg
"  directory           dir
"nodisable
"  display             dy
"  eadirection         ead
"  edcompatible        ed
"noedcompatible      noed
"  encoding            enc
"  endofline           eol
"noendofline         noeol
"  equalalways         ea
"noequalalways       noea
"  equalprg            ep
"  errorbells          eb
"noerrorbells        noeb
"  errorfile           ef
"  errorformat         efm
"  esckeys             ek
"noesckeys           noek
"  eventignore         ei
"  expandtab           et
"noexpandtab         noet
"  exrc                ex
"noexrc              noex
"  fileencoding        fenc
"  fileencodings       fencs
"  fileformat          ff
"  fileformats         ffs
"  filetype            ft
"  fillchars           fcs
"  fkmap               fk
"nofkmap             nofk
"  foldclose           fcl
"  foldcolumn          fdc
"  foldenable          fen
"nofoldenable        nofen
"  foldexpr            fde
"  foldignore          fdi
"  foldlevel           fdl
"  foldlevelstart      fdls
"  foldmarker          fmr
"  foldmethod          fdm
"  foldminlines        fml
"  foldnestmax         fdn
"  foldopen            fdo
"  foldtext            fdt
"  formatoptions       fo
"  formatprg           fp
"  gdefault            gd
"nogdefault          nogd
"  grepformat          gfm
"  grepprg             gp
"  guicursor           gcr
"  guifont             gfn
"  guifontset          gfs
"  guifontwide         gfw
"  guiheadroom         ghr
"  guioptions          go
"  guipty
"noguipty
"  helpfile            hf
"  helpheight          hh
"  hidden              hid
"nohidden            nohid
"  highlight           hl
"  history             hi
"  hkmap               hk
"nohkmap             nohk
"  hkmapp              hkp
"nohkmapp            nohkp
"  hlsearch            hls
"nohlsearch          nohls
"  icon
"noicon
"  iconstring
"  ignorecase          ic
"noignorecase        noic
"  imactivatekey       imak
"  imcmdline           imc
"noimcmdline         noimc
"  imdisable           imd
"noimdisable         noimd
"  iminsert            imi
"  imsearch            ims
"  include             inc
"  includeexpr         ine
"  includeexpr         inex
"  incsearch           is
"noincsearch         nois
"  indentexpr          inde
"  indentkeys          indk
"  infercase           inf
"noinfercase         noinf
"  insertmode          im
"noinsertmode        noim
"  isfname             isf
"  isident             isi
"  iskeyword           isk
"  isprint             isp
"  joinspaces          js
"nojoinspaces        nojs
"  key
"  keymap              kmp
"  keymodel            km
"  keywordprg          kp
"  langmap             lmap
"  langmenu            lm
"  laststatus          ls
"  lazyredraw          lz
"nolazyredraw        nolz
"  linebreak           lbr
"nolinebreak         nolbr
"  lines
"  linespace           lsp
"  lisp
"nolisp
"  lispwords           lw
"  list
"nolist
"  listchars           lcs
"  loadplugins         lpl
"noloadplugins       nolpl
"  magic
"nomagic
"  makeef              mef
"  makeprg             mp
"  matchpairs          mps
"  matchtime           mat
"  maxfuncdepth        mfd
"  maxmapdepth         mmd
"  maxmem              mm
"  maxmemtot           mmt
"  menuitems           mis
"  modeline            ml
"nomodeline          noml
"  modelines           mls
"  modifiable          ma
"nomodifiable        noma
"  modified            mod
"nomodified          nomod
"  more
"nomore
"  mouse
"  mousefocus          mousef
"nomousefocus        nomousef
"  mousehide           mh
"nomousehide         nomh
"  mousemodel          mousem
"  mouseshape          mouses
"  mousetime           mouset
"  nrformats           nf
"nonumber            nonu
"  number              nu
"  osfiletype          oft
"  paragraphs          para
"  paste
"nopaste
"  pastetoggle         pt
"  patchexpr           pex
"  patchmode           pm
"  path                pa
"  previewheight       pvh
"nopreviewwindow     nopvw
"  previewwindow       pvw
"  printdevice         pdev
"  printexpr           pexpr
"  printfont           pfn
"  printheader         pheader
"  printoptions        popt
"  readonly            ro
"noreadonly          noro
"  remap
"  report
"  restorescreen       rs
"norestorescreen     nors
"  revins              ri
"norevins            nori
"  rightleft           rl
"norightleft         norl
"  ruler               ru
"noruler             noru
"  rulerformat         ruf
"  runtimepath         rtp
"  scroll              scr
"  scrollbind          scb
"noscrollbind        noscb
"  scrolljump          sj
"  scrolloff           so
"  scrollopt           sbo
"  sections            sect
"  secure
"nosecure
"  selection           sel
"  selectmode          slm
"  sessionoptions      ssop
"  shell               sh
"  shellcmdflag        shcf
"  shellpipe           sp
"  shellquote          shq
"  shellredir          srr
"  shellslash          ssl
"noshellslash        nossl
"  shelltype           st
"  shellxquote         sxq
"  shiftround          sr
"noshiftround        nosr
"  shiftwidth          sw
"  shortmess           shm
"  shortname           sn
"noshortname         nosn
"  showbreak           sbr
"  showcmd             sc
"noshowcmd           nosc
"  showfulltag         sft
"noshowfulltag       nosft
"  showmatch           sm
"noshowmatch         nosm
"  showmode            smd
"noshowmode          nosmd
"  sidescroll          ss
"  sidescrolloff       siso
"  smartcase           scs
"nosmartcase         noscs
"  smartindent         si
"nosmartindent       nosi
"  smarttab            sta
"nosmarttab          nosta
"  softtabstop         sts
"  splitbelow          sb
"nosplitbelow        nosb
"  splitright          spr
"nosplitright        nospr
"  startofline         sol
"nostartofline       nosol
"  statusline          stl
"  suffixes            su
"  suffixesadd         sua
"  swapfile            swf
"noswapfile          noswf
"  swapsync            sws
"  switchbuf           swb
"  syntax              syn
"  tabstop             ts
"  tagbsearch          tbs
"notagbsearch        notbs
"  taglength           tl
"  tagrelative         tr
"notagrelative       notr
"  tags                tag
"  tagstack            tgst
"notagstack          notgst
"  term
"  termencoding        tenc
"  terse
"noterse
"  textauto            ta
"notextauto          nota
"  textmode            tx
"notextmode          notx
"  textwidth           tw
"  thesaurus           tsr
"  tildeop             top
"notildeop           notop
"  timeout             to
"notimeout           noto
"  timeoutlen          tm
"  title
"notitle
"  titlelen
"  titleold
"  titlestring
"  toolbar             tb
"  ttimeout
"nottimeout
"  ttimeoutlen         ttm
"  ttybuiltin          tbi
"nottybuiltin        notbi
"  ttyfast             tf
"nottyfast           notf
"  ttymouse            ttym
"  ttyscroll           tsl
"  ttytype             tty
"  undolevels          ul
"  updatecount         uc
"  updatetime          ut
"  verbose             vbs
"  viewdir             vdir
"  viewoptions         vop
"  viminfo             vi
"  virtualedit         ve
"  visualbell          vb
"novisualbell        novb
"  warn
"nowarn
"  weirdinvert         wiv
"noweirdinvert       nowiv
"  whichwrap           ww
"  wildchar            wc
"  wildcharm           wcm
"  wildignore          wig
"  wildmenu            wmnu
"nowildmenu          nowmnu
"  wildmode            wim
"  winaltkeys          wak
"  winheight           wh
"  winminheight        wmh
"  winminwidth         wmw
"  winwidth            wiw
"  wrap
"nowrap
"  wrapmargin          wm
"  wrapscan            ws
"nowrapscan          nows
"  write
"nowrite
"  writeany            wa
"nowriteany          nowa
"  writebackup         wb
"nowritebackup       nowb
"  writedelay          wd

"" menus {{{1

"amenu                 am
"anoremenu             an
"aunmenu               aun
"cmenu                 cme
"cnoremenu             cnoreme
"cunmenu               cunme
"imenu                 ime
"inoremenu             inoreme
"iunmenu               iunme
"menu                  me
"nmenu                 nme
"nnoremenu             nnoreme
"noremenu              noreme
"nunmenu               nunme
"omenu                 ome
"onoremenu             onoreme
"ounmenu               ounme
"unmenu                unme
"vmenu                 vme
"vnoremenu             vnoreme
"vunmenu               vunme

"" maps {{{1

" map
"cmap                  cm
"imap                  im
"nmap                  nm
"omap                  om
"vmap                  vm

" noremap              no
"cnoremap              cno
"inoremap              ino
"nnoremap              nn
"onoremap              ono
"vnoremap              vn

" unmap                unm
"nunmap                nun
"vunmap                vu
"ounmap                ou
"iunmap                iu
"lunmap                lu
"cunmap                cu

" mapclear             mapc
"nmapclear             nmapc
"vmapclear             vmapc
"omapclear             omapc
"imapclear             imapc
"lmapclear             lmapc
"cmapclear             cmapc
"1}}}

"" These below unlikely candidates

"" Missing options {{{1

"autoprint           ap
"beautify            bf
"flash               fl
"graphic             gr
"hardtabs            ht
"mesg
"novice
"open
"optimize            op
"prompt
"redraw
"slowopen            slow
"sourceany
"window              wi
"w300
"w1200               
"w9600

"1}}}
" vim:foldmethod=marker
