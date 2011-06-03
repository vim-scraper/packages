" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/vimchant.txt	[[[1
104
*vimchant.txt*          Vimchant - spell-checker plugin             2008-08-16


Description     Vimchant is a simple but efficient spell-checker plugin. It's 
                not bundled with many features but it provides fast real-time 
                spell-checking while typing and editing text. Vimchant uses 
                Enchant as its back-end program. Enchant, in turn, is 
                a front-end for several different spell-checkers, including 
                Ispell, Myspell and Voikko, for example. This plugin does not 
                provide Enchant itself; it's a separate component included in 
                modern GNU/Linux systems.

Author          Teemu Likonen <tlikonen@iki.fi>


==============================================================================
COMMANDS                                              *:VimchantSpellCheckOn*
                                                      *:VimchantSpellCheckOff*

The spell-checker can be turned on and off with the following ex commands:

        :VimchantSpellCheckOn
        :VimchantSpellCheckOff

There are also two keymappings available in normal mode:

        <Leader>ss      Spell-checking on/off
        <Leader>sl      Change the language (prompts for language code)

By default, <Leader> is the backslash key but it can be configured with 
g:mapleader variable. Consult the Vim reference manual for more information on 
<Leader>.

Spell-checker's state, whether it's on or off, as well as the language 
settings are buffer-specific. That is, you can have spell-checker active only 
in certain buffers and you can use different languages in different buffers.
Settings are preserved while navigating through buffers but only the text in 
active window is actually checked for spelling.

==============================================================================
CONFIGURATION                                                *vimchant-config*


Keymaps ~

The default keymaps <Leader>ss and <Leader>sl are available only if they have 
not been taken by some other plugin or defined by user. You can always bind 
them to other keys or key combinations with Vim's |:map| commands. For 
example, you could put these lines to your ~/.vimrc file:
>
        nmap <F7> <Plug>VimchantSpellCheckSwitch
        nmap <F8> <Plug>VimchantChangeLanguage

This would make <F7> the switching key and <F8> the language selector key.


Update interval ~

When the spell-checker is turned on the check is run every time when user 
stops moving the cursor and editing text. There's a short interval period 
which is determined by 'updatetime' option. It's 4000 milliseconds (i.e., 
4 seconds) by default but you might want to reduce the value a bit so that the 
spell-checker responds faster. For example, you could add the following line 
to your ~/.vimrc file:
>
        set updatetime=1000


Language ~

The LANG environment variable in GNU/Linux systems determines the default 
language used in spell-checking. Apart from changing the LANG variable there 
are two options for configuring languages from Vim. First, you can define 
a global Vim variable and put the preferred language code there:
>
        let g:vimchant_spellcheck_lang = 'fi'

You can also use local-to-buffer variables, for example:
>
        let b:vimchant_spellcheck_lang = 'en_GB'

As the names suggest, the global variable has global effect (all buffers) and 
local variables have local effect (current buffer). Local variables (if set 
and not empty) take preference over other settings; the global variable takes 
preference over the LANG environment variable.

Note: The <Leader>sl keymap actually just changes the local-to-buffer 
variable. It's provided for quick and convenient language switching.


Highlight ~

Misspelled words are highlighted with the same highlight color group which the 
Vim's internal spell-checker uses. The group is called "SpellBad". If you are 
unhappy with the default colors you can change them with |:highlight| command. 
An example:
>
        highlight SpellBad cterm=underline ctermbg=bg ctermfg=Red gui=undercurl guisp=Red


That's pretty much it. Happy typing! :-)

==============================================================================
vim: ft=help tw=78 ts=8 sts=8 et norl fo+=2aw
plugin/vimchant.vim	[[[1
174
" Vimchant - An Enchant-based spell-checker for Vim text editor
"
" Version:	1.0 (2008-08-19)
" Maintainer:	Teemu Likonen <tlikonen@iki.fi>
" License:	Public domain
"
" GetLatestVimScripts: 2345 1 :AutoInstall: Vimchant

" {{{1 The Beginning
if &compatible || !executable('enchant') || exists('g:loaded_vimchant')
	finish
endif
if v:version < 701 || (v:version == 701 && !has('patch040'))
	echohl WarningMsg
	echo 'Vimchant spell-checker plugin requires Vim version 7.1.040 or later. Sorry.'
	echohl None
	finish
endif

let s:save_cpo = &cpo
set cpo&vim

let s:spellcheck_prg = 'enchant -l'

if !hlexists('SpellBad')
	highlight SpellBad term=reverse ctermbg=1 gui=undercurl guisp=Red
endif
if !hlexists('ErrorMsg')
	highlight ErrorMsg term=standout cterm=bold ctermfg=7 ctermbg=1 guifg=White guibg=Red
endif
if !hlexists('WarningMsg')
	highlight WarningMsg term=standout cterm=bold ctermfg=1 guifg=Red
endif
if !hlexists('Question')
	highlight Question term=standout cterm=bold ctermfg=2 gui=bold guifg=Green
endif

function! s:SpellCheckSwitch(switch, ...) "{{{1
	if a:switch ==? 'on'
		let switch = 1
	elseif a:switch ==? 'off'
		let switch = 0
	elseif a:switch ==? 'switch'
		if exists('#VimchantSpellCheck#CursorHold#<buffer>')
			let switch = 0
		else
			let switch = 1
		endif
	else
		return
	endif
	if exists('a:1') && a:1 == 1
		let silence = 1
	else
		let silence = 0
	endif

	if switch
		if !exists('b:vimchant_spellcheck_save_isk')
			let b:vimchant_spellcheck_save_isk = &l:isk
		endif
		setlocal isk=@,48-57,192-255
		if s:CheckSpelling()
			" There was an error so don't start autocmds.
			return
		endif
		augroup VimchantSpellCheck
			autocmd! * <buffer>
			autocmd CursorHold,CursorHoldI <buffer> call s:CheckSpelling()
			autocmd BufDelete <buffer> call s:RemoveAutoCmds(expand('<abuf>'))
			autocmd BufLeave,WinLeave,TabLeave <buffer> call clearmatches()
		augroup END
		if !silence | echo 'Spell-checking turned on' | endif
	else
		augroup VimchantSpellCheck
			autocmd! * <buffer>
		augroup END
		call clearmatches()
		if exists('b:vimchant_spellcheck_save_isk')
			let &l:isk = b:vimchant_spellcheck_save_isk
			unlet b:vimchant_spellcheck_save_isk
		endif
		if &term != 'builtin_gui' | redraw! | endif
		if !silence | echo 'Spell-checking turned off' | endif
	endif
endfunction

function! s:CheckSpelling() "{{{1
	if exists('b:vimchant_spellcheck_lang') && b:vimchant_spellcheck_lang != ''
		let lang = 'LANG='.split(b:vimchant_spellcheck_lang)[0].' '
	elseif exists('g:vimchant_spellcheck_lang') && g:vimchant_spellcheck_lang != ''
		let lang = 'LANG='.split(g:vimchant_spellcheck_lang)[0].' '
	else
		let lang = ''
	endif

	call clearmatches()

	let line = line('w0')
	let last_line = line('w$')
	let content_string = ' '
	while line <= last_line
		let fold_end = foldclosedend(line)
		if fold_end > 0
			let line = fold_end
		else
			let content_string .= getline(line).' '
		endif
		let line += 1
	endwhile

	let content_string = tr(content_string,'_',' ')
	let content_string = substitute(content_string,'\v\s[[:punct:]]*''(\k)',' \1','g')
	let content_string = substitute(content_string,'\v(\k)''[[:punct:]]*\s','\1 ','g')
	let spelling_errors = system(lang.s:spellcheck_prg,content_string)
	if v:shell_error != 0
		if spelling_errors =~ '\cCouldn''t create a dictionary for'
			call s:SpellCheckSwitch('Off',1)
			echohl WarningMsg
			echo 'No dictionary available for language "'.substitute(lang,'\v\C^LANG\=(\S*)\s*$','\1','').
						\'". Spell-checking turned off.'
			echohl None
		else
			call s:SpellCheckSwitch('Off',1)
			echohl ErrorMsg
			echo 'Error in executing spell-checking program. Spell-checking turned off.'
			echohl None
		endif
		return 1
	endif
	for word in split(spelling_errors)
		call matchadd('SpellBad','\V\C\<'.word.'\>')
	endfor
	if &term != 'builtin_gui' | redraw! | endif
	return 0
endfunction

function! s:RemoveAutoCmds(buffer) "{{{1
	call clearmatches()
	execute 'autocmd! VimchantSpellCheck * <buffer='.a:buffer.'>'
endfunction

function! s:ChangeLanguage() "{{{1
	echohl Question
	let b:vimchant_spellcheck_lang = input('Language code: ',
				\(exists('b:vimchant_spellcheck_lang') && b:vimchant_spellcheck_lang != '') ?
				\split(b:vimchant_spellcheck_lang)[0] : '')
	echohl None
	if b:vimchant_spellcheck_lang != ''
		let b:vimchant_spellcheck_lang = split(b:vimchant_spellcheck_lang)[0]
	endif
endfunction

" {{{1 Commands and key mappings

if !exists(':VimchantSpellCheckOn') && !exists(':VimchantSpellCheckOff')
	command VimchantSpellCheckOn  call s:SpellCheckSwitch('On')
	command VimchantSpellCheckOff call s:SpellCheckSwitch('Off')
endif

nnoremap <silent> <Plug>VimchantSpellCheckSwitch :call <SID>SpellCheckSwitch('Switch')<CR>
nnoremap <silent> <Plug>VimchantChangeLanguage :call <SID>ChangeLanguage()<CR>

if maparg('<Leader>ss') == '' && !hasmapto('<Plug>VimchantSpellCheckSwitch')
	nmap <Leader>ss <Plug>VimchantSpellCheckSwitch
endif
if maparg('<Leader>sl') == '' && !hasmapto('<Plug>VimchantChangeLanguage')
	nmap <Leader>sl <Plug>VimchantChangeLanguage
endif

" {{{1 The End
let g:loaded_vimchant = 1
let &cpo = s:save_cpo
" vim600: fdm=marker
