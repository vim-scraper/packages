" Vim ftplugin for PO file (GNU gettext) editing.
" Maintainer:	Aleksandar Jelenak <ajelenak AT yahoo.com>
" Last Change:	Thu, 10 Jul 2003 15:33:08 -0400
"
" *** Latest version: http://www.vim.org/scripts/script.php?script_id=695 ***
"
" DESCRIPTION
"     This file is a Vim ftplugin for editing PO files (GNU Gettext -- the GNU
"     i18n and l10n system). It automates over a dozen of frequent tasks that
"     occur while editing files of this type.
"
"                                                      Key mappings
"     Action (Insert mode)                            GUI Vim     Vim
"     ===============================================================
"     Move to an untransl. string forward             <S-F1>      \n
"     Move to an untransl. string backward            <S-F2>      \p
"     Copy the msgid string to msgstr                 <S-F3>      \c
"     Delete the msgstr string                        <S-F4>      \d
"     Move to the next fuzzy translation              <S-F5>      \f
"     Go back to the first fuzzy translation          <S-F6>      \b
"     Label the translation fuzzy                     <S-F7>      \z
"     Remove the fuzzy label                          <S-F8>      \r
"     Show msgfmt statistics for the file(*)          <S-F11>     \s
"     Browse through msgfmt errors for the file(*)    <S-F12>     \m
"     Put the translator info in the header           \t          \t
"     Put the lang. team info in the header           \l          \l
"     ---------------------------------------------------------------
"     Action (Normal mode)
"     ===============================================================
"     Split-open the file under cursor                gf          gf
"     Show msgfmt statistics for the file(*)          <S-F11>     \s
"     Browse through msgfmt errors for the file(*)    <S-F12>     \m
"     ---------------------------------------------------------------
"     (*) Only available on UNIX computers.
"
"     Remarks:
"     - "S" in the above key mappings stands for the <Shift> key and "\" in
"       fact means "<LocalLeader>" (:help <LocalLeader>), which is "\" by
"       Vim's default.
"     - Information about the translator and language team is supplied by two
"       global variables: 'g:po_translator' and 'g:po_lang_team'. They should
"       be defined in the ".vimrc" (UNIX) or "_vimrc" (Windows) file. If they
"       are not defined, the default values (descriptive strings) are put
"       instead.
"     - Vim's "gf" Normal mode command is remapped (local to the PO buffer, of
"       course). It will only function on lines starting with "#: ". Search
"       for the file is performed in the directories specified by the 'path'
"       option. The user can supply its own addition to this option via the
"       'g:po_path' global variable. Its default value for PO files can be
"       found by typing ":set path?" from within a PO buffer. For the correct
"       format please see ":help 'path'". Warning messages are printed if no
"       or more than one file is found.
"     - Vim's Quickfix mode (see ":help quickfix") is used for browsing
"       through msgfmt-reported errors for the file. No MO file is created
"       when running the msgfmt program since its output is directed to
"       "/dev/null". The user can supply command-line arguments to the msgfmt
"       program via the global variable 'g:po_msgfmt_args'. All arguments are
"       allowed except the "-o" for output file. The default value is
"       "-vv -c".
"
"     But there's even more!
"
"     Every time the PO file is saved, a PO-formatted time stamp is
"     automatically added to the file header.
"
" INSTALLATION
"     Put this file in a Vim ftplugin directory. On UNIX computers it is
"     usually either "~/.vim/ftplugin" or "~/.vim/after/ftplugin". On Windows
"     computers, the defaults are "$VIM\vimfiles\ftplugin" or
"     "$VIM\vimfiles\after\ftplugin". For more information consult the Vim
"     help, ":help 'ftplugin'" and ":help 'runtimepath'".
"
" REMOVAL
"     Just delete the bloody file!

" Only do this when not done yet for this buffer.
if exists("b:did_po_mode_ftplugin")
   finish
endif
let b:did_po_mode_ftplugin = 1

setlocal comments=
setlocal errorformat=%f:%l:\ %m
setlocal makeprg=msgfmt

let po_path = '.,..,../src,../src/*'
if exists("g:po_path")
   let po_path = po_path . ',' . g:po_path
endif
exe "setlocal path=" . po_path
unlet po_path

" Check if GUI Vim is running.
if has("gui_running")
   let gui = 1
else
   let gui = 0
endif

" Move to the first untranslated msgstr string forward.
if !hasmapto('<Plug>NextTransFwd')
   if gui
      imap <buffer> <unique> <S-F1> <Plug>NextTransFwd
   else
      imap <buffer> <unique> <LocalLeader>n <Plug>NextTransFwd
   endif
endif
inoremap <buffer> <unique> <Plug>NextTransFwd <ESC>/^msgstr\s*""\n\n<CR>:call histdel("/", -1)<CR>z.f"a

" Move to the first untranslated msgstr string backward.
if !hasmapto('<Plug>NextTransBwd')
   if gui
      imap <buffer> <unique> <S-F2> <Plug>NextTransBwd
   else
      imap <buffer> <unique> <LocalLeader>p <Plug>NextTransBwd
   endif
endif
inoremap <buffer> <unique> <Plug>NextTransBwd <ESC>{?^msgstr\s*""\n\n<CR>:call histdel("/", -1)<CR>z.f"a

" Copy original msgid string into msgstr string.
if !hasmapto('<Plug>CopyMsgid')
   if gui
      imap <buffer> <unique> <S-F3> <Plug>CopyMsgid
   else
      imap <buffer> <unique> <LocalLeader>c <Plug>CopyMsgid
   endif
endif
inoremap <buffer> <unique> <Plug>CopyMsgid <ESC>}?^msgid<CR>:call histdel("/", -1)<CR>f"y/^msgstr<CR>/^msgstr<CR>:call histdel("/", -1)<CR>f""_d$pa

" Erase the translation string.
if !hasmapto('<Plug>DeleteTrans')
   if gui
      imap <buffer> <unique> <S-F4> <Plug>DeleteTrans
   else
      imap <buffer> <unique> <LocalLeader>d <Plug>DeleteTrans
   endif
endif
inoremap <buffer> <unique> <Plug>DeleteTrans <ESC>}?^msgstr<CR>:call histdel("/", -1)<CR>f"lc}"<ESC>i

" Move to the first fuzzy translation forward.
if !hasmapto('<Plug>NextFuzzy')
   if gui
      imap <buffer> <unique> <S-F5> <Plug>NextFuzzy
   else
      imap <buffer> <unique> <LocalLeader>f <Plug>NextFuzzy
   endif
endif
inoremap <buffer> <unique> <Plug>NextFuzzy <ESC>/^#,\s*fuzzy<CR>:call histdel("/", -1)<CR>/^msgstr<CR>:call histdel("/", -1)<CR>$i

" Move to the first fuzzy descriptor backward.
if !hasmapto('<Plug>PreviousFuzzy')
   if gui
      imap <buffer> <unique> <S-F6> <Plug>PreviousFuzzy
   else
      imap <buffer> <unique> <LocalLeader>b <Plug>PreviousFuzzy
   endif
endif
inoremap <buffer> <unique> <Plug>PreviousFuzzy <ESC>{?^#,\s*fuzzy<CR>:call histdel("/", -1)<CR>/^msgstr<CR>:call histdel("/", -1)<CR>$i

" Insert fuzzy description for a translation.
if !hasmapto('<Plug>InsertFuzzy')
   if gui
      imap <buffer> <unique> <S-F7> <Plug>InsertFuzzy
   else
      imap <buffer> <unique> <LocalLeader>z <Plug>InsertFuzzy
   endif
endif
inoremap <buffer> <unique> <Plug>InsertFuzzy <ESC>{/^msgid<CR>:call histdel("/", -1)<CR>O#, fuzzy

" Remove fuzzy description from a translation.
if !hasmapto('<Plug>RemoveFuzzy')
   if gui
      imap <buffer> <unique> <S-F8> <Plug>RemoveFuzzy
   else
      imap <buffer> <unique> <LocalLeader>r <Plug>RemoveFuzzy
   endif
endif
inoremap <buffer> <unique> <Plug>RemoveFuzzy <ESC>}?^#,\s*fuzzy<CR>:call histdel("/", -1)<CR>ddi

" Show PO translation statistics. (Only available on UNIX computers for now.)
if has("unix")
   if !hasmapto('<Plug>MsgfmtStats')
      if gui
         imap <buffer> <unique> <S-F11> <Plug>MsgfmtStats
         map <buffer> <unique> <S-F11> <Plug>MsgfmtStats
      else
         imap <buffer> <unique> <LocalLeader>s <Plug>MsgfmtStats
         map <buffer> <unique> <LocalLeader>s <Plug>MsgfmtStats
      endif
   endif
   inoremap <buffer> <unique> <Plug>MsgfmtStats <ESC>:call <SID>Msgfmt('stats')<CR>
   noremap <buffer> <unique> <Plug>MsgfmtStats :call <SID>Msgfmt('stats')<CR>

   if !hasmapto('<Plug>MsgfmtTest')
      if gui
         imap <buffer> <unique> <S-F12> <Plug>MsgfmtTest
         map <buffer> <unique> <S-F12> <Plug>MsgfmtTest
      else
         imap <buffer> <unique> <LocalLeader>m <Plug>MsgfmtTest
         map <buffer> <unique> <LocalLeader>m <Plug>MsgfmtTest
      endif
   endif
   inoremap <buffer> <unique> <Plug>MsgfmtTest <ESC>:call <SID>Msgfmt('test')<CR>
   noremap <buffer> <unique> <Plug>MsgfmtTest :call <SID>Msgfmt('test')<CR>

   fu! <SID>Msgfmt(action)
      " Check if the file needs to be saved first.
      exe "if &modified | w | endif"
      if a:action == 'stats'
         exe "!msgfmt --statistics -o /dev/null %"
      elseif a:action == 'test'
         if exists("g:po_msgfmt_args")
            let args = g:po_msgfmt_args
         else
            let args = '-vv -c'
         endif
         exe "make! " . args . " -o /dev/null %"
         copen
      endif
   endf
endif

" Add translator info in the file header.
if !hasmapto('<Plug>TranslatorInfo')
   imap <buffer> <unique> <LocalLeader>t <Plug>TranslatorInfo
endif
inoremap <buffer> <unique> <Plug>TranslatorInfo <ESC>:call <SID>AddHeaderInfo('person')<CR>i

" Add language team info in the file header.
if !hasmapto('<Plug>LangTeamInfo')
   imap <buffer> <unique> <LocalLeader>l <Plug>LangTeamInfo
endif
inoremap <buffer> <unique> <Plug>LangTeamInfo <ESC>:call <SID>AddHeaderInfo('team')<CR>i

fu! <SID>AddHeaderInfo(action)
   if a:action == 'person'
      let search_for = 'Last-Translator'
      if exists("g:po_translator")
         let add = g:po_translator
      else
         let add = 'YOUR NAME <E-MAIL@ADDRESS>'
      endif
   elseif a:action == 'team'
      let search_for = 'Language-Team'
      if exists("g:po_lang_team")
         let add = g:po_lang_team
      else
         let add = 'LANGUAGE TEAM <E-MAIL@ADDRESS or HOME PAGE>'
      endif
   else
      " Undefined action -- just do nothing.
      return
   endif
   let search_for = '"' . search_for . ':'
   let add = add . '\\n"'

   normal! 1G
   if search('^' . search_for)
      silent! exe 's/^\(' . search_for . '\).*$/\1 ' . add
   endif
   call histdel("/", -1)
endf

" Write automagically PO-formatted time stamp every time the file is saved.
augroup PoFileTimestamp
   au!
   au BufWrite *.po call <SID>PoFileTimestamp()
augroup END

fu! <SID>PoFileTimestamp()
   " Prepare for cleanup at the end of this function.
   let hist_search = histnr("/")
   let old_report = 'set report='.&report
   let &report = 100
   let cursor_pos_cmd = line(".").'normal! '.virtcol(".").'|'
   normal! H
   let scrn_pos = line(".").'normal! zt'

   " Put in time stamp.
   normal! 1G
   if search('^"PO-Revision-Date:')
      silent! exe 's/^\("PO-Revision-Date:\).*$/\1 ' . strftime("%Y-%m-%d %H:%M%z") . '\\n"'
   endif

   " Cleanup and restore old cursor position.
   while histnr("/") > hist_search && histnr("/") > 0
      call histdel("/", -1)
   endwhile
   exe scrn_pos
   exe cursor_pos_cmd
   exe old_report
endf

" On "gf" Normal mode command, split window and open the file under the
" cursor.
if !hasmapto('<Plug>OpenSourceFile')
   map <buffer> <unique> gf <Plug>OpenSourceFile
endif
noremap <buffer> <unique> <Plug>OpenSourceFile :call <SID>OpenSourceFile()<CR>

" This opens the file under the cursor in a split-window.
fu! <SID>OpenSourceFile()
   " Check if we're at the right line. Return if not.
   if getline(".") !~ '^#:\s\+' | return | endif
   
   " Get the reference, check it, and return if it doesn't have the assumed
   " format.
   let ref = expand("<cWORD>")
   if ref !~ ':\d\+$' | return | endif

   " Split the reference into the file name and the line number parts.
   let d = match(ref, ':')
   let flnm = strpart(ref, 0, d)
   let lnr = strpart(ref, d+1, 100)

   " Start searching for the file in the directories specified with the 'path'
   " option.
   let ff = globpath(&path, flnm)

   " Check what's been found. Report if no or more than one file found and
   " return.
   if ff == ''
      echohl WarningMsg | echo "No file found in the path."
      echohl None
      exe "normal \<Esc>"
   elseif match(ff, "\n") > 0
      echohl WarningMsg | echo "More than one file found: " . ff . "\nAborting."
      echohl None
      exe "normal \<Esc>"
   else
      " Split the window and open the file at the correct line.
      execute "silent sp +" . lnr . " " . ff
   endif
endf

unlet gui
