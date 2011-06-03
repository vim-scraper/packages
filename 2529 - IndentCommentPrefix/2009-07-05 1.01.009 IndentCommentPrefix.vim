" IndentCommentPrefix.vim: Keep comment prefix in column 1 when indenting. 
"
" DESCRIPTION:
"   Indent commands like >>, << and <C-T>/<C-D> in insert mode indent the entire
"   line. For some kinds of comments, like the big boilerplate at the file
"   header etc., the comment prefix (e.g. # for Perl scripts) should remain at
"   the first column, though. 
"   This plugin modifies these indent commands so that the comment prefix
"   remains in the first column, and the indenting takes place between the
"   comment prefix and the comment text. For that, it uses the comment
"   configuration provided by the buffer's 'comment' option, which is set by
"   most filetype plugins. 
"
" USAGE:
"   On a line like this:
"   # My comment. 
"   The >> command now keeps the # prefix in column 1, and just indents the
"   comment text:
"   #       My comment. 
"   This only works if there is at least one whitespace character after the
"   prefix (so that comments like ###### do not become #       ######). 
"   Progressive de-indenting will remove all whitespace between prefix and
"   comment text, or leave a single space in between if the 'comments' setting
"   requires a blank after the comment prefix. 
"
"   An optional [count] of lines can be supplied to the >> and << commands, as
"   before.
"   In visual mode, the optional [count] specifies how many 'shiftwidth's should
"   be indented; the > and < commands operate on all highlighted lines. 
"
"   With the optional repeat.vim script, the commands can also be repeated via '.'. 
"   
"   The same behavior is available in insert mode via the <C-T>/<C-D> mappings. 
"
"   In case you want to indent lines including the comment prefix, the original
"   indent behavior is mapped to 'g>>' in normal mode and 'g>' in visual mode.
"   (There's no need for the corresponding g<< dedent mappings, just stop
"   dedenting when the comment prefix reaches column 1.)
"   Alternatively, you could also use the >{motion} command, as the > and <
"   operators aren't modified by this script. 
"
" INSTALLATION:
"   Put the script into your user or system Vim plugin directory (e.g.
"   ~/.vim/plugin). 
"
" DEPENDENCIES:
"   - Requires Vim 7.0 or higher. 
"   - vimscript #2136 repeat.vim autoload script (optional). 
"
" CONFIGURATION:
"   If you don't want the alternative g>> mappings for the original indent
"   commands, set the following variable _before_ sourcing this script (e.g. in
"   your vimrc file (see :help vimrc)). 
"	let g:IndentCommentPrefix_alternativeOriginalCommands = 0
"
" INTEGRATION:
" LIMITATIONS:
" ASSUMPTIONS:
" KNOWN PROBLEMS:
"   - When indenting in insert mode via <C-T>/<C-D>, the cursor position may be
"     off if there are <Tab> characters in the indented text itself (not just
"     between the prefix and the indented text), and the cursor is positioned
"     somewhere behind such a <Tab> character. The changing virtual width of
"     these <Tab> characters isn't considered when calculating the new virtual
"     cursor column. 
"   - With ':set list' and if ':set listchars' does not include a 'tab:xy' item,
"     tabs show up as ^I and do not occupy the full width (up to 'tabstop'
"     characters). This shortened representation throws off the cursor position
"     when indenting in insert mode via <C-T>/<C-D>. 
"   - If a visual mode '.' repeat command is defined to repeat the last change
"     on all highlighted lines, and the previous indent operation used a [count]
"     greater than 1, the highlighted lines will be indented multiple times and
"     lines after the current visual selection will be erroneously indented,
"     too. (So it's a big mess up, don't do this.) This is because the previous
"     [count] will now be used repeatedly to select multiple lines. 
"
" TODO:
"   - Does it make sense to also modify the >{motion} operators? 
"
" Copyright: (C) 2008-2009 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
"   1.01.009	03-Jul-2009	BF: When 'report' is less than the default 2,
"				the :substitute and << / >> commands created
"				additional messages, causing a hit-enter prompt. 
"				Now also reporting a single-line change when
"				'report' is 0 (to be consistent with the
"				built-in indent commands). 
"   1.00.008	23-Feb-2009	BF: Fixed "E61: Nested *" that occurred when
"				shifting a line with a comment prefix containing
"				multiple asterisks in a row (e.g. '**'). This
"				was caused by a mixed up argument escaping in
"				s:IsMatchInComments() and one missed escaping
"				elsewhere. 
"				BF: Info message (given when indenting multiple
"				lines) always printed "1 time" even when a
"				[count] was specified in visual mode. 
"   1.00.007	29-Jan-2009	BF: Test whether prefix is a comment was too
"				primitive and failed to distinguish between ':'
"				(label) and '::' (comment) in dosbatch filetype.
"				Now using exact regexp factored out into a
"				function, also for the blank-required check. 
"	006	22-Jan-2009	Added visual mode mappings. 
"				Enhanced implementation to deal with the
"				optional [count] 'shiftwidth's that can be
"				specified in visual mode. 
"	005	04-Jan-2009	BF: Fixed changes of vertical window position by
"				saving and restoring window view. 
"				ENH: The >> and << (range) commands now position
"				the cursor on the first non-blank character
"				after the comment prefix; this makes more sense. 
"				Now avoiding superfluous cursor positioning when
"				indenting ranges. (Side effect from the changes
"				due to restore of window position.) 
"	004	21-Aug-2008	BF: Didn't consider that removing the comment
"				prefix could cause changes in folding (e.g. in
"				vimscript if the line ends with "if"), which
"				then affects all indent operations, which now
"				work on the closed fold instead of the current
"				line. Now temporarily disabling folding. 
"				BF: The looping over the passed range in
"				s:IndentKeepCommentPrefixRange() didn't consider
"				closed folds, so those (except for a last-line
"				fold) would be processed multiple times. Now
"				that folding is temporarily disabling, need to
"				account for the net end of the range. 
"				Added echo message when operating on more than
"				one line, like the original >> commands. 
"	003	19-Aug-2008	BF: Indenting/detenting at the first shiftwidth
"				caused cursor to move to column 1; now adjusting
"				for the net reduction caused by the prefix. 
"	002	12-Aug-2008	Do not clobber search history with :s command. 
"				If a blank is required after the comment prefix,
"				make sure it still exists when dedenting. 
"	001	11-Aug-2008	file creation

" Avoid installing twice or when in unsupported Vim version. 
if exists('g:loaded_IndentCommentPrefix') || (v:version < 700)
    finish
endif
let g:loaded_IndentCommentPrefix = 1

"- configuration --------------------------------------------------------------
if ! exists('g:IndentCommentPrefix_alternativeOriginalCommands')
    let g:IndentCommentPrefix_alternativeOriginalCommands = 1
endif

"------------------------------------------------------------------------------
function! s:Literal( string )
" Helper: Make a:string a literal search expression. 
    return '\V' . escape(a:string, '\') . '\m'
endfunction

function! s:IsMatchInComments( flag, prefix )
    return &l:comments =~# '\%(^\|,\)[^:]*' . a:flag . '[^:]*:' . s:Literal(a:prefix) . '\%(,\|$\)'
endfunction
function! s:IsComment( prefix )
    return s:IsMatchInComments('', a:prefix)
endfunction
function! s:IsBlankRequiredAfterPrefix( prefix )
    return s:IsMatchInComments('b', a:prefix)
endfunction

"------------------------------------------------------------------------------
function! s:DoIndent( isDedent, isInsertMode, count )
    if a:isInsertMode
	call feedkeys( repeat((a:isDedent ? "\<C-d>" : "\<C-t>"), a:count), 'n' )
    else
	" Use :silent to suppress reporting of changed line (when 'report' is
	" 0). 
	execute 'silent normal!' repeat((a:isDedent ? '<<' : '>>'), a:count)
    endif
endfunction
function! s:SubstituteHere( substituitionCmd )
    " Use :silent! to suppress any error messages or reporting of changed line
    " (when 'report' is 0). 
    " Use :keepjumps to avoid modification of jump list. 
    execute 'silent! keepjumps s' . a:substituitionCmd
    call histdel('search', -1)
endfunction
function! s:IndentKeepCommentPrefix( isDedent, isInsertMode, count )
"*******************************************************************************
"* PURPOSE:
"   Enhanced indent / dedent replacement for >>, <<, i_CTRL-D, i_CTRL-T
"   commands. 
"* ASSUMPTIONS / PRECONDITIONS:
"   "Normal" prefix characters (i.e. they have screen width of 1 and are encoded
"   by one byte); as we're using len(l:prefix) to calculate screen width. 
"   Folding should be turned off (:setlocal nofoldenable); otherwise, the
"   modifications of the line (i.e. removing and re-adding the comment prefix)
"   may result in creation / removal of folds, and suddenly the function
"   operates on multiple lines!
"* EFFECTS / POSTCONDITIONS:
"   Modifies current line. 
"* INPUTS:
"   a:isDedent	    Flag whether indenting or dedenting. 
"   a:isInsertMode  Flag whether normal mode or insert mode replacement. 
"   a:count	    Number of 'shiftwidth' that should be indented (i.e. number
"		    of repetitions of the indent command). 
"* RETURN VALUES: 
"   New virtual cursor column, taking into account a single (a:count == 1)
"   indent operation. 
"   Multiple repetitions are not supported here, because the virtual cursor
"   column is only consumed by the insert mode operation, which is always a
"   single indent. The (possibly multi-indent) visual mode operation discards
"   this return value, anyway. 
"*******************************************************************************
    let l:line = line('.')
    let l:matches = matchlist( getline(l:line), '\(^\S\+\)\(\s*\)' )
    let l:prefix = get(l:matches, 1, '')
    let l:indent = get(l:matches, 2, '')
    let l:isSpaceIndent = (l:indent =~# '^ ')

    if empty(l:prefix) || ! s:IsComment(l:prefix)
	" No prefix in this line or the prefix is not registered as a comment. 
	call s:DoIndent( a:isDedent, a:isInsertMode, a:count )
	" The built-in indent commands automatically adjust the cursor column. 
	return virtcol('.')
    endif



"****D echomsg l:isSpaceIndent ? 'spaces' : 'tab'
    let l:virtCol = virtcol('.')

    " If the actual indent is a <Tab>, remove the prefix. If it is <Space>,
    " replace prefix with spaces so that the overall indentation remains fixed. 
    " Note: We have to decide based on the actual indent, because with the
    " softtabstop setting, there may be spaces though the overall indenting is
    " done with <Tab>. 
    call s:SubstituteHere('/^\C\V' . escape(l:prefix, '/\') . '/' . (l:isSpaceIndent ? repeat(' ', len(l:prefix)) : '') . '/')

    call s:DoIndent( a:isDedent, 0, a:count )

    " If the first indent is a <Tab>, re-insert the prefix. If it is <Space>,
    " replace spaces with prefix so that the overall indentation remains fixed. 
    " Note: We have to re-evaluate because the softtabstop setting may have
    " changed <Tab> into spaces and vice versa. 
    let l:newIndent = matchstr( getline(l:line), '^\s' )
    " Dedenting may have eaten up all indent spaces. In that case, just
    " re-insert the comment prefix as is done with <Tab> indenting. 
    call s:SubstituteHere('/^' . (l:newIndent == ' ' ? '\%( \{' . len(l:prefix) . '}\)\?' : '') . '/' . escape(l:prefix, '/\&~') . '/')

    " If a blank is required after the comment prefix, make sure it still exists
    " when dedenting. 
    if s:IsBlankRequiredAfterPrefix(l:prefix) && a:isDedent
	call s:SubstituteHere('/^' . escape(l:prefix, '/\') . '\ze\S/\0 /e')
    endif
    

    " Adjust cursor column based on the _virtual_ column. (Important since we're
    " dealing with <Tab> characters here!) 
    " Note: This calculation ignores a:count, see note in function
    " documentation. 
    let l:newVirtCol = l:virtCol
    if ! a:isDedent && l:isSpaceIndent && len(l:prefix . l:indent) < &l:sw
	" If the former indent was less than one shiftwidth and indenting was
	" done via spaces, this reduces the net change of cursor position. 
	let l:newVirtCol -= len(l:prefix . l:indent)
    elseif a:isDedent && l:isSpaceIndent && len(l:prefix . l:indent) <= &l:sw
	" Also, on the last possible dedent, the prefix (and one <Space> if blank
	" required) will reduce the net change of cursor position. 
	let l:newVirtCol += len(l:prefix) + (s:IsBlankRequiredAfterPrefix(l:prefix) ? 1 : 0)
    endif
    " Calculate new cursor position based on indent/dedent of shiftwidth,
    " considering the adjustments made before. 
    let l:newVirtCol += (a:isDedent ? -1 : 1) * &l:sw

"****D echomsg '****' l:virtCol l:newVirtCol len(l:prefix . l:indent)
    return l:newVirtCol

    " Note: The cursor column isn't updated here anymore, because the window
    " view had to be saved and restored by the caller of this function, anyway. 
    " (Due to the temporary disabling of folding.) As the window position
    " restore also restores the old cursor position, the setting here would be
    " overwritten, anyway.
    " Plus, the s:IndentKeepCommentPrefixRange() functionality sets the cursor
    " position in a different way, anyway, and only for the first line in the
    " range, so the cursor movement here would be superfluous, too. 
    "call cursor(l:line, 1)
    "if l:newVirtCol > 1
    "	call search('\%>' . (l:newVirtCol - 1) . 'v', 'c', l:line)
    "endif
endfunction

function! s:IndentKeepCommentPrefixInsertMode( isDedent )
    " The temporary disabling of folding below may result in a change of the
    " viewed lines, which would be irritating for a command that only modified
    " the current line. Thus, save and restore the view, but afterwards take
    " into account that the indenting changes the cursor column. 
    let l:save_winview = winsaveview()

    " Temporarily turn off folding while indenting the line. 
    let l:save_foldenable = &l:foldenable
    setlocal nofoldenable

    let l:newVirtCol = s:IndentKeepCommentPrefix(a:isDedent, 1, 1)

    let &l:foldenable = l:save_foldenable
    call winrestview(l:save_winview)

    " Set new cursor position after indenting; the saved view has reset the
    " position to before indent. 
    call cursor('.', 1)
    if l:newVirtCol > 1
	call search('\%>' . (l:newVirtCol - 1) . 'v', 'c', line('.'))
    endif
endfunction
inoremap <silent> <C-t> <C-o>:call <SID>IndentKeepCommentPrefixInsertMode(0)<CR>
inoremap <silent> <C-d> <C-o>:call <SID>IndentKeepCommentPrefixInsertMode(1)<CR>

function! s:IndentKeepCommentPrefixRange( isDedent, count ) range
    " The temporary disabling of folding below may result in a change of the
    " viewed lines, which would be irritating for a command that only modified
    " the current line. Thus, save and restore the view. 
    let l:save_winview = winsaveview()

    " Determine the net last line (different if last line is folded) and
    " temporarily turn off folding while indenting the lines. 
    let l:netLastLine = (foldclosedend(a:lastline) == -1 ? a:lastline : foldclosedend(a:lastline))
    let l:save_foldenable = &l:foldenable
    setlocal nofoldenable

    for l in range(a:firstline, l:netLastLine)
	execute l . 'call s:IndentKeepCommentPrefix(' . a:isDedent . ', 0'. ', ' . a:count . ')'
    endfor

    let &l:foldenable = l:save_foldenable
    call winrestview(l:save_winview)

    " Go back to first line, like the default >> indent commands. 
    " But put the cursor on the first non-blank character after the comment
    " prefix, not on first overall non-blank character, as the default >> indent
    " commands would do. This makes more sense, since we're essentially ignoring
    " the comment prefix during indenting. 
    execute a:firstline
    let l:matches = matchlist( getline(a:firstline), '\(^\S\+\)\s*' )
    let l:prefix = get(l:matches, 1, '')
    if ! empty(l:prefix) && &l:comments =~# s:Literal(l:prefix)
	" Yes, the first line was a special comment prefix indent, not a normal
	" one. 
	call search('^\S\+\s*\%(\S\|$\)', 'ce', a:firstline)
    endif

    " Integration into repeat.vim. 
    let l:netIndentedLines = l:netLastLine - a:firstline + 1
    " Passing the net number of indented lines is necessary to correctly repeat
    " (in normal mode) indenting of a visual selection. Otherwise, only the
    " current line would be indented because v:count was 1 during the visual
    " indent operation. 
    silent! call repeat#set("\<Plug>IndentCommentPrefix" . a:isDedent, l:netIndentedLines)

    let l:lineNum = l:netLastLine - a:firstline + 1
    " Vim reports the change if more than one line is indented (unless 'report'
    " is 0). 
    if l:lineNum > (&report == 0 ? 0 : 1)
	echo printf('%d line%s %sed %d time%s', l:lineNum, (l:lineNum == 1 ? '' : 's'), (a:isDedent ? '<' : '>'), a:count, (a:count == 1 ? '' : 's'))
    endif
endfunction
nnoremap <silent> <Plug>IndentCommentPrefix0 :call <SID>IndentKeepCommentPrefixRange(0,1)<CR>
vnoremap <silent> <Plug>IndentCommentPrefix0 :call <SID>IndentKeepCommentPrefixRange(0,v:count1)<CR>
nnoremap <silent> <Plug>IndentCommentPrefix1 :call <SID>IndentKeepCommentPrefixRange(1,1)<CR>
vnoremap <silent> <Plug>IndentCommentPrefix1 :call <SID>IndentKeepCommentPrefixRange(1,v:count1)<CR>
if ! hasmapto('<Plug>IndentCommentPrefix0', 'n')
    nmap <silent> >> <Plug>IndentCommentPrefix0
endif
if ! hasmapto('<Plug>IndentCommentPrefix0', 'v')
    vmap <silent> > <Plug>IndentCommentPrefix0
endif
if ! hasmapto('<Plug>IndentCommentPrefix1', 'n')
    nmap <silent> << <Plug>IndentCommentPrefix1
endif
if ! hasmapto('<Plug>IndentCommentPrefix1', 'v')
    vmap <silent> < <Plug>IndentCommentPrefix1
endif

if g:IndentCommentPrefix_alternativeOriginalCommands
    nnoremap g>> >>
    vnoremap g> >
endif

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
