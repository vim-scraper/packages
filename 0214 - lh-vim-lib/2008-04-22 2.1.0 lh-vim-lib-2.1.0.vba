" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/lh/askvim.vim	[[[1
125
"=============================================================================
" $Id: askvim.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		autoload/lh/askvim.vim                                    {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	17th Apr 2007
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $ (17th Apr 2007)
"------------------------------------------------------------------------
" Description:	
" 	Defines functions that asks vim what its is relinquish to tell us
" 	- menu
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
" 	v2.0.0:
" TODO:		«missing features»
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
" Functions {{{1

" Function: lh#askvim#exe(command) {{{2
function! lh#askvim#Exe(command)
  let save_a = @a
  try 
    silent! redir @a
    silent! exe a:command
    redir END
  finally
    " Always restore everything
    let res = @a
    let @a = save_a
    return res
  endtry
endfunction


" Function: lh#askvim#menu(menuid) {{{2
function! s:AskOneMenu(menuact, res)
  let sKnown_menus = lh#askvim#Exe(a:menuact)
  let lKnown_menus = split(sKnown_menus, '\n')
  " echo string(lKnown_menus)

  " 1- search for the menuid
  " todo: fix the next line to correctly interpret "stuff\.stuff" and
  " "stuff\\.stuff".
  let menuid_parts = split(a:menuact, '\.')

  let simplifiedKnown_menus = deepcopy(lKnown_menus)
  call map(simplifiedKnown_menus, 'substitute(v:val, "&", "", "g")')
  " let idx = lh#list#Match(simplifiedKnown_menus, '^\d\+\s\+'.menuid_parts[-1])
  let idx = match(simplifiedKnown_menus, '^\d\+\s\+'.menuid_parts[-1])
  if idx == -1
    " echo "not found"
    return
  endif
  " echo "l[".idx."]=".lKnown_menus[idx]

  if empty(a:res)
    let a:res.priority = matchstr(lKnown_menus[idx], '\d\+\ze\s\+.*')
    let a:res.name     = matchstr(lKnown_menus[idx], '\d\+\s\+\zs.*')
    let a:res.actions  = {}
  " else
  "   what if the priority isn't the same?
  endif

  " 2- search for the menu definition
  let idx += 1
  while idx != len(lKnown_menus)
    echo "l[".idx."]=".lKnown_menus[idx]
    " should not happen
    if lKnown_menus[idx] =~ '^\d\+' | break | endif

    " :h showing-menus
    " -> The format of the result of the call to Exe() seems to be:
    "    ^ssssMns-sACTION$
    "    s == 1 whitespace
    "    M == mode (inrvcs)
    "    n == noremap(*)/script(&)
    "    - == disable(-)/of not
    let act = {}
    let menu_def = matchlist(lKnown_menus[idx],
	  \ '^\s*\([invocs]\)\([&* ]\) \([- ]\) \(.*\)$')
    if len(menu_def) > 4
      let act.mode        = menu_def[1]
      let act.nore_script = menu_def[2]
      let act.disabled    = menu_def[3]
      let act.action      = menu_def[4]
    else
      echomsg string(menu_def)
      echoerr "lh#askvim#Menu(): Cannot decode ``".lKnown_menus[idx]."''"
    endif
    
    let a:res.actions["mode_" . act.mode] = act

    let idx += 1
  endwhile

  " n- Return the result
  return a:res
endfunction

function! lh#askvim#Menu(menuid, modes)
  let res = {}
  let i = 0
  while i != strlen(a:modes)
    call s:AskOneMenu(a:modes[i].'menu '.a:menuid, res)
    let i = i + 1
  endwhile
  return res
endfunction
" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/buffer.vim	[[[1
57
"=============================================================================
" $Id: buffer.vim 12 2008-02-14 00:06:48Z luc.hermitte $
" File:		buffer.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	23rd Jan 2007
" Last Update:	$Date: 2008-02-14 01:06:48 +0100 (jeu., 14 fÃ©vr. 2008) $
"------------------------------------------------------------------------
" Description:	
" 	Defines functions that help finding windows.
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
"	v 1.0.0 First Version
" 	(*) Functions moved from searchInRuntimeTime  
" TODO:		«missing features»
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------

" Function: lh#buffer#Find({filename}) {{{3
" If {filename} is opened in a window, jump to this window, otherwise return -1
" Moved from searchInRuntimeTime.vim
function! lh#buffer#Find(filename)
  let b = bufwinnr(a:filename)
  if b == -1 | return b | endif
  exe b.'wincmd w'
  return b
endfunction

" Function: lh#buffer#Jump({filename},{cmd}) {{{3
function! lh#buffer#Jump(filename, cmd)
  if lh#buffer#Find(a:filename) != -1 | return | endif
  exe a:cmd . ' ' . a:filename
endfunction

function! lh#buffer#Scratch(bname, where)
  try
    silent exe a:where.' sp '.a:bname
  catch /.*/
    throw "Can't open a buffer named '".a:bname."'!"
  endtry
  setlocal bt=nofile bh=wipe nobl noswf ro
endfunction
"=============================================================================
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/buffer/dialog.vim	[[[1
225
"=============================================================================
" $Id: dialog.vim 37 2008-02-19 02:11:08Z luc.hermitte $
" File:		autoload/lh/buffer/dialog.vim                            {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.6
" Created:	21st Sep 2007
" Last Update:	$Date: 2008-02-19 03:11:08 +0100 (mar., 19 fÃ©vr. 2008) $
"------------------------------------------------------------------------
" Description:	«description»
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
"	v 1.0.0 First Version
" 	(*) Functions imported from Mail_mutt_alias.vim
" TODO:		
" 	(*) --abort-- line
" 	(*) custom messages
" 	(*) do not mess with search history
" 	(*) support any &magic
" 	(*) syntax
" 	(*) add number/letters
" 	(*) tag with '[x] ' instead of '* '
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim

let s:LHdialog = {}
"------------------------------------------------------------------------
function! s:Mappings(abuffer)
  " map <enter> to edit a file, also dbl-click
  exe "nnoremap <silent> <buffer> <esc>         :silent call ".a:abuffer.action."(-1, ".a:abuffer.id.")<cr>"
  exe "nnoremap <silent> <buffer> q             :call <sid>Select(-1, ".a:abuffer.id.")<cr>"
  exe "nnoremap <silent> <buffer> <cr>          :call <sid>Select(line('.'), ".a:abuffer.id.")<cr>"
  " nnoremap <silent> <buffer> <2-LeftMouse> :silent call <sid>GrepEditFileLine(line("."))<cr>
  " nnoremap <silent> <buffer> Q	  :call <sid>Reformat()<cr>
  " nnoremap <silent> <buffer> <Left>	  :set tabstop-=1<cr>
  " nnoremap <silent> <buffer> <Right>	  :set tabstop+=1<cr>
  if a:abuffer.support_tagging
    nnoremap <silent> <buffer> t	  :silent call <sid>ToggleTag(line("."))<cr>
    nnoremap <silent> <buffer> <space>	  :silent call <sid>ToggleTag(line("."))<cr>
  endif
  nnoremap <silent> <buffer> <tab>	  :silent call <sid>NextChoice('')<cr>
  nnoremap <silent> <buffer> <S-tab>	  :silent call <sid>NextChoice('b')<cr>
  exe "nnoremap <silent> <buffer> h	  :silent call <sid>ToggleHelp(".a:abuffer.id.")<cr>"
endfunction

"----------------------------------------
" Tag / untag the current choice {{{
function! s:ToggleTag(lineNum)
   if a:lineNum > s:Help_NbL()
      " If tagged
      if (getline(a:lineNum)[0] == '*')
	let b:NbTags = b:NbTags - 1
	silent exe a:lineNum.'s/^\* /  /e'
      else
	let b:NbTags = b:NbTags + 1
	silent exe a:lineNum.'s/^  /* /e'
      endif
      " Move after the tag ; there is something with the two previous :s. They
      " don't leave the cursor at the same position.
      silent! normal! 3|
      call s:NextChoice('') " move to the next choice
    endif
endfunction
" }}}

function! s:Help_NbL()
  " return 1 + nb lines of BuildHelp
  return 2 + len(b:dialog['help_'.b:dialog.help_type])
endfunction
"----------------------------------------
" Go to the Next (/previous) possible choice. {{{
function! s:NextChoice(direction)
  " echomsg "next!"
  call search('^[ *]\s*\zs\S\+', a:direction)
endfunction
" }}}

"------------------------------------------------------------------------

function! s:RedisplayHelp(dialog)
  silent! 2,$g/^@/d_
  normal! gg
  for help in a:dialog['help_'.a:dialog.help_type]
    silent put=help
  endfor
endfunction

function! lh#buffer#dialog#Update(dialog)
  set noro
  exe (s:Help_NbL()+1).',$d_'
  for choice in a:dialog.choices
    silent $put='  '.choice
  endfor
  set ro
endfunction

function! s:Display(dialog, atitle)
  set noro
  0 put = a:atitle
  call s:RedisplayHelp(a:dialog)
  for choice in a:dialog.choices
    silent $put='  '.choice
  endfor
  set ro
  exe s:Help_NbL()+1
endfunction

function! s:ToggleHelp(bufferId)
  call lh#buffer#Find(a:bufferId)
  call b:dialog.toggle_help()
endfunction

function! lh#buffer#dialog#toggle_help() dict
  let self.help_type 
	\ = (self.help_type == 'short')
	\ ? 'long'
	\ : 'short'
  call s:RedisplayHelp(self)
endfunction

function! lh#buffer#dialog#new(bname, title, where, support_tagging, action, choices)
  " The ID will be the buffer id
  let res = {}

  try
    call lh#buffer#Scratch(a:bname, a:where)
  catch /.*/
    echoerr v:exception
    return res
  endtry
  let res.id              = bufnr('%')
  let b:NbTags            = 0
  let b:dialog            = res
  let s:LHdialog[res.id]  = res
  let res.help_long       = []
  let res.help_short      = []
  let res.help_type       = 'short'
  let res.support_tagging = a:support_tagging
  let res.action	  = a:action
  let res.choices	  = a:choices

  " Long help
  call lh#buffer#dialog#add_help(res, '@| <cr>, <double-click>    : select this', 'long')
  call lh#buffer#dialog#add_help(res, '@| <esc>, q                : Abort', 'long')
  if a:support_tagging
    call lh#buffer#dialog#add_help(res, '@| <t>, <space>            : Tag/Untag the current item', 'long')
  endif
  call lh#buffer#dialog#add_help(res, '@| <up>/<down>, <tab>, +/- : Move between entries', 'long')
  call lh#buffer#dialog#add_help(res, '@|', 'long')
  " call lh#buffer#dialog#add_help(res, '@| h                       : Toggle help', 'long')
  call lh#buffer#dialog#add_help(res, '@+'.repeat('-', winwidth(bufwinnr(res.id))-3), 'long')
  " Short Help
  " call lh#buffer#dialog#add_help(res, '@| h                       : Toggle help', 'short')
  call lh#buffer#dialog#add_help(res, '@+'.repeat('-', winwidth(bufwinnr(res.id))-3), 'short')

  let res.toggle_help = function("lh#buffer#dialog#toggle_help")
  let title = '@  ' . a:title
  let helpstr = '| Toggle (h)elp'
  let title = title 
	\ . repeat(' ', winwidth(bufwinnr(res.id))-strlen(title)-strlen(helpstr)-1)
	\ . helpstr
  call s:Display(res, title)
 
  call s:Mappings(res)
  return res
endfunction

function! lh#buffer#dialog#add_help(abuffer, text, help_type)
  call add(a:abuffer['help_'.a:help_type],a:text)
endfunction

"=============================================================================
function! lh#buffer#dialog#Quit()
  echohl WarningMsg
  echo "Abort"
  echohl None
  quit
endfunction

function! s:Select(line, bufferId)
  if a:line == -1
    call lh#buffer#dialog#Quit()
    return
  " elseif a:line <= s:Help_NbL() + 1
  elseif a:line <= s:Help_NbL() 
    echoerr "Unselectable item"
    return 
  else
    let dialog = s:LHdialog[a:bufferId]
    let results = { 'dialog' : dialog, 'selection' : []  }

    if b:NbTags == 0
      " -1 because first index is 0
      " let results = [ dialog.choices[a:line - s:Help_NbL() - 1] ]
      let results.selection = [ a:line - s:Help_NbL() - 1 ]
    else
      silent g/^* /call add(results.selection, line('.')-s:Help_NbL()-1)
    endif
  endif

  exe 'call '.dialog.action.'(results)'
endfunction

function! Action(results)
  let dialog = a:results.dialog
  let choices = dialog.choices
  for r in a:results.selection
    echomsg '-> '.choices[r]
  endfor
endfunction


"=============================================================================
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/command.vim	[[[1
208
"=============================================================================
" $Id: command.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		command.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	08th Jan 2007
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $ (08th Jan 2007)
"------------------------------------------------------------------------
" Description:	
" 	Helpers to define commands that:
" 	- support subcommands
" 	- support autocompletion
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
" 	v2.0.0:
" 		Code move from other plugins
" TODO:		«missing features»
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
" Functions {{{1

" Tool functions {{{2
" Function: lh#command#Fargs2String(aList) {{{3
" @param[in,out] aList list of params from <f-args>
" @see tests/lh/test-Fargs2String.vim
function! lh#command#Fargs2String(aList)
  if empty(a:aList) | return '' | endif

  let quote_char = a:aList[0][0] 
  let res = a:aList[0]
  call remove(a:aList, 0)
  if quote_char !~ '["'."']"
    return res
  endif
  " else
  let end_string = '[^\\]\%(\\\\\)*'.quote_char.'$'
  while !empty(a:aList) && res !~ end_string 
    let res .= ' ' . a:aList[0]
    call remove(a:aList, 0)
  endwhile
  return res
endfunction

"------------------------------------------------------------------------
" Experimental Functions {{{1

" Internal functions        {{{2
" Function: s:SaveData({Data})                             {{{3
" @param Data Command definition
" Saves {Data} as s:Data{s:data_id++}. The definition will be used by
" automatically generated commands.
" @return s:data_id
let s:data_id = 0
function! s:SaveData(Data)
  if has_key(a:Data, "command_id")
    " Avoid data duplication
    return a:Data.command_id
  else
    let s:Data{s:data_id} = a:Data
    let id = s:data_id
    let s:data_id += 1
    let a:Data.command_id = id
    return id
  endif
endfunction

" BTWComplete(ArgLead, CmdLine, CursorPos):      Auto-complete {{{3
function! lh#command#Complete(ArgLead, CmdLine, CursorPos)
  let tmp = substitute(a:CmdLine, '\s*\S*', 'Z', 'g')
  let pos = strlen(tmp)
  if 0
    call confirm( "AL = ". a:ArgLead."\nCL = ". a:CmdLine."\nCP = ".a:CursorPos
	  \ . "\ntmp = ".tmp."\npos = ".pos
	  \, '&Ok', 1)
  endif

  if     2 == pos
    " First argument: a command
    return s:commands
  elseif 3 == pos
    " Second argument: first arg of the command
    if     -1 != match(a:CmdLine, '^BTW\s\+echo')
      return s:functions . "\n" . s:variables
    elseif -1 != match(a:CmdLine, '^BTW\s\+\%(help\|?\)')
    elseif -1 != match(a:CmdLine, '^BTW\s\+\%(set\|add\)\%(local\)\=')
      " Adds a filter
      " let files =         globpath(&rtp, 'compiler/BT-*')
      " let files = files . globpath(&rtp, 'compiler/BT_*')
      " let files = files . globpath(&rtp, 'compiler/BT/*')
      let files = s:FindFilter('*')
      let files = substitute(files,
	    \ '\(^\|\n\).\{-}compiler[\\/]BTW[-_\\/]\(.\{-}\)\.vim\>\ze\%(\n\|$\)',
	    \ '\1\2', 'g')
      return files
    elseif -1 != match(a:CmdLine, '^BTW\s\+remove\%(local\)\=')
      " Removes a filter
      return substitute(s:FiltersList(), ',', '\n', 'g')
    endif
  endif
  " finally: unknown
  echoerr 'BTW: unespected parameter ``'. a:ArgLead ."''"
  return ''
endfunction

function! s:BTW(command, ...)
  " todo: check a:0 > 1
  if     'set'      == a:command | let g:BTW_build_tool = a:1
    if exists('b:BTW_build_tool')
      let b:BTW_build_tool = a:1
    endif
  elseif 'setlocal'     == a:command | let b:BTW_build_tool = a:1
  elseif 'add'          == a:command | call s:AddFilter('g', a:1)
  elseif 'addlocal'     == a:command | call s:AddFilter('b', a:1)
    " if exists('b:BTW_filters_list') " ?????
    " call s:AddFilter('b', a:1)
    " endif
  elseif 'remove'       == a:command | call s:RemoveFilter('g', a:1)
  elseif 'removelocal'  == a:command | call s:RemoveFilter('b', a:1)
  elseif 'rebuild'      == a:command " wait for s:ReconstructToolsChain()
  elseif 'echo'         == a:command | exe "echo s:".a:1
    " echo s:{a:f1} ## don't support «echo s:f('foo')»
  elseif 'reloadPlugin' == a:command
    let g:force_reload_BuildToolsWrapper = 1
    let g:BTW_BTW_in_use = 1
    exe 'so '.s:sfile
    unlet g:force_reload_BuildToolsWrapper
    unlet g:BTW_BTW_in_use
    return
  elseif a:command =~ '\%(help\|?\)'
    call s:Usage()
    return
  endif
  call s:ReconstructToolsChain()
endfunction

" ##############################################################
" Public functions          {{{2

function! s:FindSubcommand(definition, subcommand)
  for arg in a:definition.arguments
    if arg.name == a:subcommand
      return arg
    endif
  endfor
  throw "NF"
endfunction

function! s:execute_function(definition, params)
    if len(a:params) < 1
      throw "(lh#command) Not enough arguments"
    endif
  let l:Fn = a:definition.action
  echo "calling ".string(l:Fn)
  echo "with ".string(a:params)
  " call remove(a:params, 0)
  call l:Fn(a:params)
endfunction

function! s:execute_sub_commands(definition, params)
  try
    if len(a:params) < 1
      throw "(lh#command) Not enough arguments"
    endif
    let subcommand = s:FindSubcommand(a:definition, a:params[0])
    call remove(a:params, 0)
    call s:int_execute(subcommand, a:params)
  catch /NF.*/
    throw "(lh#command) Unexpected subcommand `".a:params[0]."'."
  endtry
endfunction

function! s:int_execute(definition, params)
  echo "params=".string(a:params)
  call s:execute_{a:definition.arg_type}(a:definition, a:params)
endfunction

function! s:execute(definition, ...)
  try
    let params = copy(a:000)
    call s:int_execute(a:definition, params)
  catch /(lh#command).*/
    echoerr v:exception . " in `".a:definition.name.' '.join(a:000, ' ')."'"
  endtry
endfunction

function! lh#command#New(definition)
  let cmd_name = a:definition.name
  " Save the definition as an internal script variable
  let id = s:SaveData(a:definition)
  exe "command! -nargs=* ".cmd_name." :call s:execute(s:Data".id.", <f-args>)"
endfunction

" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/common.vim	[[[1
65
"=============================================================================
" $Id: common.vim 10 2008-02-14 00:02:10Z luc.hermitte $
" File:		common.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	07th Oct 2006
" Last Update:	$Date: 2008-02-14 01:02:10 +0100 (jeu., 14 fÃ©vr. 2008) $ (08th Feb 2008)
"------------------------------------------------------------------------
" Description:	
" 	Some common functions for:
" 	- displaying error messages
" 	- checking dependencies
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
" 	v2.0.0:
" 		Code move from other plugins
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
" Functions {{{1

" Function: lh#common#ErrorMsg {{{2
function! lh#common#ErrorMsg(text)
  if has('gui_running')
    call confirm(a:text, '&Ok', '1', 'Error')
  else
    " echohl ErrorMsg
    echoerr a:text
    " echohl None
  endif
endfunction 

" Function: lh#common#WarningMsg {{{2
function! lh#common#WarningMsg(text)
  echohl WarningMsg
  echomsg a:text
  echohl None
endfunction 

" Dependencies {{{2
function! lh#common#CheckDeps(Symbol, File, path, plugin) " {{{3
  if !exists(a:Symbol)
    exe "runtime ".a:path.a:File
    if !exists(a:Symbol)
      call lh#common#ErrorMsg( a:plugin.': Requires <'.a:File.'>')
      return 0
    endif
  endif
  return 1
endfunction " }}}4
" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/event.vim	[[[1
46
"============================================================================= "=============================================================================
" $Id: event.vim 36 2008-02-19 02:09:28Z luc.hermitte $
" File:		event.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.6
" Created:	15th Feb 2008
" Last Update:	$Date: 2008-02-19 03:09:28 +0100 (mar., 19 fÃ©vr. 2008) $
"------------------------------------------------------------------------
" Description:	
" 	Function to help manage vim |autocommand-events|
" 
"------------------------------------------------------------------------
" Installation:
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:
" 	v2.0.6:
" 		Creation
" TODO:		
" }}}1
"=============================================================================

let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
function! s:RegisteredOnce(cmd, group)
  " We can't delete the current augroup autocommand => increment a counter
  if !exists('s:'.a:group) || s:{a:group} == 0 
    let s:{a:group} = 1
    exe a:cmd
  endif
endfunction

function! lh#event#RegisterForOneExecutionAt(event, cmd, group)
  let group = a:group.'_once'
  let s:{group} = 0
  exe 'augroup '.group
  au!
  exe 'au '.a:event.' '.expand('%:p').' call s:RegisteredOnce('.string(a:cmd).','.string(group).')'
  augroup END
endfunction
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/encoding.vim	[[[1
49
"=============================================================================
" $Id: encoding.vim 42 2008-02-21 23:25:02Z luc.hermitte $
" File:		encoding.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.7
" Created:	21st Feb 2008
" Last Update:	$Date: 2008-02-22 00:25:02 +0100 (ven., 22 fÃ©vr. 2008) $
"------------------------------------------------------------------------
" Description:	
" 	Defines functions that help managing various encodings
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
" 	v2.0.7:
" 	(*) lh#encoding#Iconv() copied from map-tools
" TODO:		«missing features»
" }}}1
"=============================================================================

let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------

" Function: lh#encoding#Iconv(expr, from, to)  " {{{3
" Unlike |iconv()|, this wrapper returns {expr} when we know no convertion can
" be acheived.
function! lh#encoding#Iconv(expr, from, to)
  " call Dfunc("s:ICONV(".a:expr.','.a:from.','.a:to.')')
  if has('multi_byte') && 
	\ ( has('iconv') || has('iconv/dyn') ||
	\ ((a:from=~'latin1\|utf-8') && (a:to=~'latin1\|utf-8')))
    " call confirm('encoding: '.&enc."\nto:".a:to, "&Ok", 1)
    " call Dret("s:ICONV convert=".iconv(a:expr, a:from, a:to))
    return iconv(a:expr,a:from,a:to)
  else
    " Cannot convert
    " call Dret("s:ICONV  no convert=".a:expr)
    return a:expr
  endif
endfunction

"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/list.vim	[[[1
83
"=============================================================================
" $Id: list.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		autoload/lh/list.vim                                      {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.7
" Created:	17th Apr 2007
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $ (17th Apr 2007)
"------------------------------------------------------------------------
" Description:	
" 	Defines functions related to |Lists|
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	
" 	v2.0.7:
" 	(*) Bug fix: lh#list#Match()
" 	v2.0.6:
" 	(*) lh#list#Find_if() supports search predicate, and start index
" 	(*) lh#list#Match() supports start index
" 	v2.0.0:
" TODO:		«missing features»
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim

"------------------------------------------------------------------------
" Functions {{{1

" Function: lh#list#Match(list, to_be_matched [, idx]) {{{2
function! lh#list#Match(list, to_be_matched, ...)
  let idx = (a:0>0) ? a:1 : 0
  while idx < len(a:list)
    if match(a:list[idx], a:to_be_matched) != -1
      return idx
    endif
    let idx += 1
  endwhile
  return -1
endfunction

" Function: lh#list#Find_if(list, predicate [, predicate-arguments] [, start-pos]) {{{2
function! lh#list#Find_if(list, predicate, ...)
  " Parameters
  let idx = 0
  let args = []
  if a:0 == 2
    let idx = a:2
    let args = a:1
  elseif a:0 == 1
    if type(a:1) == type([])
      let args = a:1
    elseif type(a:1) == type(42)
      let idx = a:1
    else
      throw "lh#list#Match_if: unexpected argument type"
    endif
  elseif a:0 != 0
      throw "lh#list#Match_if: unexpected number of arguments: lh#list#Match_if(list, predicate [, predicate-arguments] [, start-pos])"
  endif

  " The search loop
  while idx != len(a:list)
    let predicate = substitute(a:predicate, 'v:val', 'a:list['.idx.']', 'g')
    let predicate = substitute(predicate, 'v:\(\d\+\)_', 'args[\1-1]', 'g')
    let res = eval(predicate)
    if res | return idx | endif
    let idx += 1
  endwhile
  return -1
endfunction

" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/menu.vim	[[[1
322
"=============================================================================
" $Id: menu.vim 12 2008-02-14 00:06:48Z luc.hermitte $
" File:		autoload/lh/menu.vim                               {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	13th Oct 2006
" Last Update:	$Date: 2008-02-14 01:06:48 +0100 (jeu., 14 fÃ©vr. 2008) $ (28th Aug 2007)
"------------------------------------------------------------------------
" Description:	
" 	Defines the global function lh#menu#DefMenu
" 	Aimed at (ft)plugn writers.
" 
"------------------------------------------------------------------------
" Installation:	
" 	Drop this file into {rtp}/autoload/lh/
" 	Requires Vim 7+
" History:	
" 	v2.0.0:	Moving to vim7
" TODO:		«missing features»
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
" Functions {{{1

"------------------------------------------------------------------------
" Common stuff       {{{2
" Function: lh#menu#Text({text})                             {{{3
" @return a text to be used in menus where "\" and spaces have been escaped.
function! lh#menu#Text(text)
  return escape(a:text, '\ ')
endfunction

" Toggling menu item {{{2
" Function: s:Fetch({Data},{key})                          {{{3
" @param[in] Data Menu-item definition
" @param[in] key  Table table from which the result will be fetched
" @return the current value, or text, whose index is Data.idx_crt_value.
function! s:Fetch(Data, key)
  let len = len(a:Data[a:key])
  if a:Data.idx_crt_value >= len | let a:Data.idx_crt_value = 0 | endif
  let value = a:Data[a:key][a:Data.idx_crt_value]
  return value
endfunction

" Function: s:Search({Data},{value})                       {{{3
" Searches for the index of {value} in {Data.values} list. Return 0 if not
" found.
function! s:Search(Data, value)
  let idx = 0
  while idx != len(a:Data.values)
    if a:value == a:Data.values[idx]
      " echo a:Data.variable . "[".idx."] == " . a:value
      return idx
    endif
    let idx = idx + 1
  endwhile
  " echo a:Data.variable . "[".-1."] == " . a:value
  return 0 " default is first element
endfunction

" Function: s:Set({Data})                                  {{{3
" @param[in,out] Data Menu item definition
"
" Sets the global variable associated to the menu item according to the item's
" current value.
function! s:Set(Data)
  let value = a:Data.values[a:Data.idx_crt_value]
  let variable = a:Data.variable
  let g:{variable} = value
  if has_key(a:Data, "actions")
    let l:Action = a:Data.actions[a:Data.idx_crt_value]
    if type(l:Action) == type(function('tr'))
      call l:Action()
    else
      exe l:Action
    endif
  endif
endfunction

" Function: s:MenuKey({Data})                              {{{3
" @return the table name from which the current value name (to dsplay in the
" menu) must be fetched. 
" Priority is given to the optional "texts" table over the madatory "values" table.
function! s:MenuKey(Data)
  if has_key(a:Data, "texts")
    let menu_id = "texts"
  else
    let menu_id = "values"
  endif
  return menu_id
endfunction

" Function: s:NextValue({Data})                            {{{3
" Change the value of the variable to the next in the list of value.
" The menu, and the variable are updated in consequence.
function! s:NextValue(Data)
  " Where the texts for values must be fetched
  let labels_key = s:MenuKey(a:Data)
  " Fetch the old current value 
  let old = s:Fetch(a:Data, labels_key)

  " Remove the entry from the menu
  call s:ClearMenu(a:Data.menu, old)

  " Cycle/increment the current value
  let a:Data.idx_crt_value += 1
  " Fetch it
  let new = s:Fetch(a:Data,labels_key)
  " Add the updated entry in the menu
  call s:UpdateMenu(a:Data.menu, new, a:Data.command)
  " Update the binded global variable
  call s:Set(a:Data)
endfunction

" Function: s:ClearMenu({Menu}, {text})                    {{{3
" Removes a menu item
"
" @param[in] Menu.priority Priority of the new menu-item
" @param[in] Menu.name     Name of the new menu-item
" @param[in] text          Text of the previous value of the variable binded
function! s:ClearMenu(Menu, text)
  if has('gui_running')
    let name = substitute(a:Menu.name, '&', '', 'g')
    let cmd = 'unmenu '.lh#menu#Text(name.'<tab>('.a:text.')')
    silent! exe cmd
  endif
endfunction

" Function: s:UpdateMenu({Menu}, {text}, {command})        {{{3
" Adds a new menu item, with the text associated to the current value in
" braces.
"
" @param[in] Menu.priority Priority of the new menu-item
" @param[in] Menu.name     Name of the new menu-item
" @param[in] text          Text of the current value of the variable binded to
"                          the menu-item
" @param[in] command       Toggle command to execute when the menu-item is selected
function! s:UpdateMenu(Menu, text, command)
  if has('gui_running')
    let cmd = 'nnoremenu <silent> '.a:Menu.priority.' '.
	  \ lh#menu#Text(a:Menu.name.'<tab>('.a:text.')').
	  \ ' :silent '.a:command."\<cr>"
    silent! exe cmd
  endif
endfunction

" Function: s:SaveData({Data})                             {{{3
" @param Data Menu-item definition
" Saves {Data} as s:Data{s:data_id++}. The definition will be used by
" automatically generated commands.
" @return s:data_id
let s:data_id = 0
function! s:SaveData(Data)
  let s:Data{s:data_id} = a:Data
  let id = s:data_id
  let s:data_id += 1
  return id
endfunction

" Function: lh#menu#DefToggleItem({Data})                  {{{3
" @param Data.idx_crt_value
" @param Data.definitions == [ {value:, menutext: } ]
" @param Data.menu        == { name:, position: }
"
" Sets a toggle-able menu-item defined by {Data}.
"
function! lh#menu#DefToggleItem(Data)
  " Save the menu data as an internal script variable
  let id = s:SaveData(a:Data)

  " If the index of the current value hasn't been set, fetch it from the
  " associated variable
  if !has_key(a:Data, "idx_crt_value")
    " Fetch the value of the associated variable
    let value = lh#option#Get(a:Data.variable, 0, 'g')
    " echo a:Data.variable . " <- " . value
    " Update the index of the current value
    let a:Data.idx_crt_value  = s:Search(a:Data, value)
  endif

  " Name of the auto-matically generated toggle command
  let cmdName = 'Toggle'.substitute(a:Data.menu.name, '[^a-zA-Z_]', '', 'g')
  " Defines the command
  silent exe 'command! -nargs=0 '.cmdName.' :call s:NextValue(s:Data'.id.')'
  let a:Data["command"] = cmdName

  " Add the menu entry according to the current value
  call s:UpdateMenu(a:Data.menu, s:Fetch(a:Data, s:MenuKey(a:Data)), cmdName)
  " Update the associated global variable
  call s:Set(a:Data)
endfunction

"------------------------------------------------------------------------
" IVN Menus          {{{2
" Function: s:CTRL_O({cmd})                                {{{3
" Build the command (sequence of ':ex commands') to be executed from
" INSERT-mode.
function! s:CTRL_O(cmd)
  return substitute(a:cmd, '\(^\|<CR>\):', '\1\<C-O>:', 'g')
endfunction

" Function: lh#menu#CMD_and_clear_v({cmd})                 {{{3
" execute the command and then clear the @v buffer
function! lh#menu#CMD_and_clear_v(cmd)
  exe a:cmd
  let @v=''
endfunction

" Function: s:Build_CMD({prefix},{cmd})                    {{{3
" build the exact command to execute regarding the mode it is dedicated
function! s:Build_CMD(prefix, cmd)
  if a:cmd[0] != ':' | return ' ' . a:cmd
  endif
  if     a:prefix[0] == "i"  | return ' ' . <SID>CTRL_O(a:cmd)
  elseif a:prefix[0] == "n"  | return ' ' . a:cmd
  elseif a:prefix[0] == "v" 
    if match(a:cmd, ":VCall") == 0
      return substitute(a:cmd, ':VCall', ' :call', ''). "\<cr>gV"
      " gV exit select-mode if we where in it!
    else
      return
	    \ " \"vy\<C-C>:call CMD_and_clear_v('" . 
	    \ substitute(a:cmd, "<CR>$", '', '') ."')\<cr>"
    endif
  elseif a:prefix[0] == "c"  | return " \<C-C>" . a:cmd
  else                       | return ' ' . a:cmd
  endif
endfunction

" Function: lh#menu#Map_all({map_type}, [{menu args}...)   {{{3
" map the command to all the modes required
function! lh#menu#Map_all(map_type,...)
  let nore   = (match(a:map_type, '[aincv]*noremap') != -1) ? "nore" : ""
  let prefix = matchstr(substitute(a:map_type, nore, '', ''), '[aincv]*')
  if a:1 == "<buffer>" | let i = 3 | let binding = a:1 . ' ' . a:2
  else                 | let i = 2 | let binding = a:1
  endif
  let binding = '<silent> ' . binding
  let cmd = a:{i}
  let i = i + 1
  while i <= a:0
    let cmd = cmd . ' ' . a:{i}
    let i = i + 1
  endwhile
  let build_cmd = nore . 'map ' . binding
  while strlen(prefix)
    if prefix[0] == "a" | let prefix = "incv"
    else
      execute prefix[0] . build_cmd . <SID>Build_CMD(prefix[0],cmd)
      let prefix = strpart(prefix, 1)
    endif
  endwhile
endfunction

" Function: lh#menu#Make({prefix},{code},{text},{binding},...) {{{3
" Build the menu and map its associated binding to all the modes required
function! lh#menu#Make(prefix, code, text, binding, ...)
  let nore   = (match(a:prefix, '[aincv]*nore') != -1) ? "nore" : ""
  let prefix = matchstr(substitute(a:prefix, nore, '', ''), '[aincv]*')
  let b = (a:1 == "<buffer>") ? 1 : 0
  let i = b + 1 
  let cmd = a:{i}
  let i = i + 1
  while i <= a:0
    let cmd = cmd . ' ' . a:{i}
    let i = i + 1
  endwhile
  let build_cmd = nore . "menu <silent> " . a:code . ' ' . lh#menu#Text(a:text) 
  if strlen(a:binding) != 0
    let build_cmd = build_cmd . '<tab>' . 
	  \ substitute(lh#menu#Text(a:binding), '&', '\0\0', 'g')
    if b != 0
      call lh#menu#Map_all(prefix.nore."map", ' <buffer> '.a:binding, cmd)
    else
      call lh#menu#Map_all(prefix.nore."map", a:binding, cmd)
    endif
  endif
  if has("gui_running")
    while strlen(prefix)
      execute <SID>BMenu(b).prefix[0].build_cmd.<SID>Build_CMD(prefix[0],cmd)
      let prefix = strpart(prefix, 1)
    endwhile
  endif
endfunction

" Function: s:BMenu({b})                                   {{{3
" If <buffermenu.vim> is installed and the menu should be local, then the
" apropriate string is returned.
function! s:BMenu(b)
  let res = (a:b && exists(':Bmenu') 
	\     && (1 == lh#option#Get("want_buffermenu_or_global_disable", 1, "bg"))
	\) ? 'B' : ''
  " call confirm("BMenu(".a:b.")=".res, '&Ok', 1)
  return res
endfunction

" Function: lh#menu#IVN_Make(...)                          {{{3
function! lh#menu#IVN_Make(code, text, binding, i_cmd, v_cmd, n_cmd, ...)
  " nore options
  let nore_i = (a:0 > 0) ? ((a:1 != 0) ? 'nore' : '') : ''
  let nore_v = (a:0 > 1) ? ((a:2 != 0) ? 'nore' : '') : ''
  let nore_n = (a:0 > 2) ? ((a:3 != 0) ? 'nore' : '') : ''
  " 
  call lh#menu#Make('i'.nore_i,a:code,a:text, a:binding, '<buffer>', a:i_cmd)
  call lh#menu#Make('v'.nore_v,a:code,a:text, a:binding, '<buffer>', a:v_cmd)
  if strlen(a:n_cmd) != 0
    call lh#menu#Make('n'.nore_n,a:code,a:text, a:binding, '<buffer>', a:n_cmd)
  endif
endfunction

"
" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/option.vim	[[[1
83
"=============================================================================
" $Id: option.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		autoload/lh/option.vim                                    {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	24th Jul 2004
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $ (07th Oct 2006)
"------------------------------------------------------------------------
" Description:
" 	Defines the global function lh#option#get().
"       Aimed at (ft)plugin writers.
" 
"------------------------------------------------------------------------
" Installation:
" 	Drop this file into {rtp}/autoload/lh/
" 	Requires Vim 7+
" History:	
" 	v2.0.5
" 	(*) lh#option#GetNonEmpty() manages Lists and Dictionaries
" 	(*) lh#option#Get() doesn't test emptyness anymore
" 	v2.0.0
" 		Code moved from {rtp}/macros/ 
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
" Functions {{{1

" Function: lh#option#Get(name, default [, scope])            {{{2
" @return b:{name} if it exists, or g:{name} if it exists, or {default}
" otherwise
" The order of the variables checked can be specified through the optional
" argument {scope}
function! lh#option#Get(name,default,...)
  let scope = (a:0 == 1) ? a:1 : 'bg'
  let name = a:name
  let i = 0
  while i != strlen(scope)
    if exists(scope[i].':'.name)
      " \ && (0 != strlen({scope[i]}:{name}))
      return {scope[i]}:{name}
    endif
    let i = i + 1
  endwhile 
  return a:default
endfunction

function! s:IsEmpty(variable)
  if     type(a:variable) == type('string') | return 0 == strlen(a:variable)
  elseif type(a:variable) == type(42)       | return 0 == a:variable
  elseif type(a:variable) == type([])       | return 0 == len(a:variable)
  elseif type(a:variable) == type({})       | return 0 == len(a:variable)
  else                                      | return false
  endif
endfunction

" Function: lh#option#GetNonEmpty(name, default [, scope])            {{{2
" @return of b:{name}, g:{name}, or {default} the first which exists and is not empty 
" The order of the variables checked can be specified through the optional
" argument {scope}
function! lh#option#GetNonEmpty(name,default,...)
  let scope = (a:0 == 1) ? a:1 : 'bg'
  let name = a:name
  let i = 0
  while i != strlen(scope)
    if exists(scope[i].':'.name) && !s:IsEmpty({scope[i]}:{name})
      return {scope[i]}:{name}
    endif
    let i = i + 1
  endwhile 
  return a:default
endfunction

" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/path.vim	[[[1
170
"=============================================================================
" $Id: path.vim 11 2008-02-14 00:04:43Z luc.hermitte $
" File:		path.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	23rd Jan 2007
" Last Update:	11th Feb 2008
"------------------------------------------------------------------------
" Description:	«description»
" 
"------------------------------------------------------------------------
" Installation:	«install details»
" History:	
"	v 1.0.0 First Version
" 	(*) Functions moved from searchInRuntimeTime  
" 	v 2.0.1
" 	(*) lh#path#Simplify() becomes like |simplify()| except for trailing
" 	v 2.0.2
" 	(*) lh#path#SelectOne() 
" 	(*) lh#path#ToRelative() 
" 	v 2.0.3
" 	(*) lh#path#GlobAsList() 
" 	v 2.0.4
" 	(*) lh#path#StripStart()
" 	v 2.0.5
" 	(*) lh#path#StripStart() interprets '.' as getcwd()
" TODO:		«missing features»
" }}}1
"=============================================================================


"=============================================================================
" Avoid global reinclusion {{{1
let s:cpo_save=&cpo
set cpo&vim
"=============================================================================

" Function: lh#path#Simplify({pathname}) {{{3
" Like |simplify()|, but also strip the leading './'
function! lh#path#Simplify(pathname)
  let pathname = simplify(a:pathname)
  let pathname = substitute(a:pathname, '^\%(\./\)\+', '', '')
  return pathname
endfunction
"
" Function: lh#path#StripCommon({pathnames}) {{{3
" Find the common leading path between all pathnames, and strip it
function! lh#path#StripCommon(pathnames)
  " assert(len(pathnames)) > 1
  let common = a:pathnames[0]
  let i = 1
  while i < len(a:pathnames)
    let fcrt = a:pathnames[i]
    " pathnames should not contain @
    let common = matchstr(common.'@@'.fcrt, '^\zs\(.*[/\\]\)\ze.\{-}@@\1.*$')
    if strlen(common) == 0
      " No need to further checks
      return a:pathnames
    endif
    let i = i + 1
  endwhile
  let l = strlen(common)
  let pathnames = a:pathnames
  call map(pathnames, 'strpart(v:val, '.l.')' )
  return pathnames
endfunction

" Function: lh#path#IsAbsolutePath({path}) {{{3
function! lh#path#IsAbsolutePath(path)
  return a:path =~ '^/'
	\ . '\|^[a-zA-Z]:%\(/\|\\\)'
	\ . '\|^[/\\]\{2}'
  "    Unix absolute path 
  " or Windows absolute path
  " or UNC path
endfunction

" Function: lh#path#IsURL({path}) {{{3
function! lh#path#IsURL(path)
  " todo: support UNC paths and other urls
  return a:path =~ '^\%(https\=\|s\=ftp\|dav\|fetch\|file\|rcp\|rsynch\|scp\)://'
endfunction

" Function: lh#path#SelectOne({pathnames},{prompt}) {{{3
function! lh#path#SelectOne(pathnames, prompt)
  if len(a:pathnames) > 1
    let simpl_pathnames = deepcopy(a:pathnames) 
    let simpl_pathnames = lh#path#StripCommon(simpl_pathnames)
    let simpl_pathnames = [ '&Cancel' ] + simpl_pathnames
    " Consider guioptions+=c is case of difficulties with the gui
    let selection = confirm(a:prompt, join(simpl_pathnames,"\n"), 1, 'Question')
    let file = (selection == 1) ? '' : a:pathnames[selection-2]
    return file
  elseif len(a:pathnames) == 0
    return ''
  else
    return a:pathnames[0]
  endif
endfunction

" Function: lh#path#ToRelative({pathname}) {{{3
function! lh#path#ToRelative(pathname)
  let newpath = fnamemodify(a:pathname, ':p:.')
  return newpath
endfunction

" Function: lh#path#GlobAsList({pathslist}, {expr}) {{{3
function! s:GlobAsList(pathslist, expr)
  let sResult = globpath(a:pathslist, a:expr)
  let lResult = split(sResult, '\n')
  return lResult
endfunction

function! lh#path#GlobAsList(pathslist, expr)
  if type(a:expr) == type('string')
    return s:GlobAsList(a:pathslist, a:expr)
  elseif type(a:expr) == type([])
    let res = []
    for expr in a:expr
      call extend(res, s:GlobAsList(a:pathslist, expr))
    endfor
    return res
  else
    throw "Unexpected type for a:expression"
  endif
endfunction

" Function: lh#path#StripStart({pathname}, {pathslist}) {{{3
" Strip occurrence of paths from {pathslist} in {pathname}
" @param[in] {pathname} name to simplify
" @param[in] {pathslist} list of pathname (can be a |string| of pathnames
" separated by ",", of a |List|).
function! lh#path#StripStart(pathname, pathslist)
  if type(a:pathslist) == type('string')
    " let strip_re = escape(a:pathslist, '\\.')
    " let strip_re = '^' . substitute(strip_re, ',', '\\|^', 'g')
    let pathslist = split(a:pathslist, ',')
  elseif type(a:pathslist) == type([])
    let pathslist = deepcopy(a:pathslist)
  else
    throw "Unexpected type for a:pathname"
  endif

  " apply a realpath like operation
  let nb_paths = len(pathslist) " set before the loop
  let i = 0
  while i != nb_paths
    if pathslist[i] =~ '^\.\%(/\|$\)'
      let path2 = getcwd().pathslist[i][1:]
      call add(pathslist, path2)
    endif
    let i = i + 1
  endwhile
  " replace path separators by a regex that can match them
  call map(pathslist, 'substitute(v:val, "[\\\\/]", "[\\\\/]", "g")')
  " echomsg string(pathslist)
  " escape .
  call map(pathslist, '"^".escape(v:val, ".")')
  " build the strip regex
  let strip_re = join(pathslist, '\|')
  " echomsg strip_re
  let res = substitute(a:pathname, '\%('.strip_re.'\)[/\\]\=', '', '')
  return res
endfunction

"=============================================================================
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/position.vim	[[[1
67
"=============================================================================
" $Id: position.vim 12 2008-02-14 00:06:48Z luc.hermitte $
" File:		position.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	05th Sep 2007
" Last Update:	$Date: 2008-02-14 01:06:48 +0100 (jeu., 14 fÃ©vr. 2008) $ (05th Sep 2007)
"------------------------------------------------------------------------
" Description:	«description»
" 
"------------------------------------------------------------------------
" Installation:
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	«history»
" 	v1.0.0:
" 		Creation
" TODO:		
" }}}1
"=============================================================================


"=============================================================================
let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
" Functions {{{1

" Function: lh#position#IsBefore {{{2
" @param[in] positions as those returned from |getpos()|
" @return whether lhs_pos is before rhs_pos
function! lh#position#IsBefore(lhs_pos, rhs_pos)
  if a:lhs_pos[0] != a:rhs_pos[0]
    throw "Positions from incompatible buffers can't be ordered"
  endif
  "1 test lines
  "2 test cols
  let before 
	\ = (a:lhs_pos[1] == a:rhs_pos[1])
	\ ? (a:lhs_pos[2] < a:rhs_pos[2])
	\ : (a:lhs_pos[1] < a:rhs_pos[1])
  return before
endfunction


" Function: lh#position#CharAtMark {{{2
" @return the character at a given mark (|mark|)
function! lh#position#CharAtMark(mark)
  let c = getline(a:mark)[col(a:mark)-1]
  return c
endfunction

" Function: lh#position#CharAtPos {{{2
" @return the character at a given position (|getpos()|)
function! lh#position#CharAtPos(pos)
  let c = getline(a:pos[1])[col(a:pos[2])-1]
  return c
endfunction



" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/syntax.vim	[[[1
107
"=============================================================================
" $Id: syntax.vim 12 2008-02-14 00:06:48Z luc.hermitte $
" File:		syntax.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.5
" Created:	05th Sep 2007
" Last Update:	$Date: 2008-02-14 01:06:48 +0100 (jeu., 14 fÃ©vr. 2008) $ (05th Sep 2007)
"------------------------------------------------------------------------
" Description:	«description»
" 
"------------------------------------------------------------------------
" Installation:
" 	Drop it into {rtp}/autoload/lh/
" 	Vim 7+ required.
" History:	«history»
" 	v1.0.0:
" 		Creation ;
" 		Functions moved from lhVimSpell
" TODO:
" 	function, to inject "contained", see lhVimSpell approach
" }}}1
"=============================================================================


"=============================================================================
" Avoid global reinclusion {{{1
let s:cpo_save=&cpo
set cpo&vim
" Avoid global reinclusion }}}1
"------------------------------------------------------------------------
" Functions {{{1

" Functions: Show name of the syntax kind of a character {{{2
function! lh#syntax#NameAt(l,c, ...)
  let what = a:0 > 0 ? a:1 : 0
  return synIDattr(synID(a:l, a:c, what),'name')
endfunction

function! lh#syntax#NameAtMark(mark, ...)
  let what = a:0 > 0 ? a:1 : 0
  return lh#syntax#NameAt(line(a:mark), col(a:mark), what)
endfunction

" Functions: skip string, comment, character, doxygen {{{2
func! lh#syntax#SkipAt(l,c)
  return lh#syntax#NameAt(a:l,a:c) =~? 'string\|comment\|character\|doxygen'
endfun

func! lh#syntax#Skip()
  return lh#syntax#SkipAt(line('.'), col('.'))
endfun

func! lh#syntax#SkipAtMark(mark)
  return lh#syntax#SkipAt(line(a:mark), col(a:mark))
endfun

" Function: Show current syntax kind {{{2
command! SynShow echo 'hi<'.lh#syntax#NameAtMark('.',1).'> trans<'
      \ lh#syntax#NameAtMark('.',0).'> lo<'.
      \ synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'name').'>'


" Function: lh#syntax#SynListRaw(name) : string                     {{{2
function! lh#syntax#SynListRaw(name)
  let a_save = @a
  try
    redir @a
    exe 'silent! syn list '.a:name
    redir END
    let res = @a
  finally
    let @a = a_save
  endtry
  return res
endfunction

function! lh#syntax#SynList(name)
  let raw = lh#syntax#SynListRaw(a:name)
  let res = [] 
  let lines = split(raw, '\n')
  let started = 0
  for l in lines
    if started
      let li = (l =~ 'links to') ? '' : l
    elseif l =~ 'xxx'
      let li = matchstr(l, 'xxx\s*\zs.*')
      let started = 1
    else
      let li = ''
    endif
    if strlen(li) != 0
      let li = substitute(li, 'contained\S*\|transparent\|nextgroup\|skipwhite\|skipnl\|skipempty', '', 'g')
      let kinds = split(li, '\s\+')
      call extend(res, kinds)
    endif
  endfor
  return res
endfunction



" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
autoload/lh/graph/tsort.vim	[[[1
160
"=============================================================================
" $Id: tsort.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		tsort.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.1.0
" Created:	21st Apr 2008
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $
"------------------------------------------------------------------------
" Description:	Library functions for Topological Sort
" 
"------------------------------------------------------------------------
" 	Drop the file into {rtp}/autoload/lh/graph
" History:	«history»
" TODO:		«missing features»
" }}}1
"=============================================================================

let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------
"## Helper functions                         {{{1
"# s:Successors_fully_defined(node)          {{{2
function! s:Successors_fully_defined(node) dict
  if has_key(self.table, a:node)
    return self.table[a:node]
  else
    return []
  endif
endfunction

"# s:Successors_lazy(node)                   {{{2
function! s:Successors_lazy(node) dict
  if !has_key(self.table, a:node)
    let nodes = self.fetch(a:node)
    let self.table[a:node] = nodes
    " if len(nodes) > 0
      " let self.nb += 1
    " endif
    return nodes
  else
    return self.table[a:node]
  endif
endfunction

"# s:PrepareDAG(dag)                         {{{2
function! s:PrepareDAG(dag)
  if type(a:dag) == type(function('has_key'))
    let dag = { 
	  \ 'successors': function('s:Successors_lazy'),
	  \ 'fetch'     : a:dag,
	  \ 'table' 	: {}
	  \}
  else
    let dag = { 
	  \ 'successors': function('s:Successors_fully_defined'),
	  \ 'table' 	: deepcopy(a:dag)
	  \}
  endif
  return dag
endfunction

"## Depth-first search (recursive)           {{{1
" Do not detect cyclic graphs

"# lh#graph#tsort#depth(dag, start_nodes)    {{{2
function! lh#graph#tsort#depth(dag, start_nodes)
  let dag = s:PrepareDAG(a:dag)
  let results = []
  let visited_nodes = { 'Visited':function('s:Visited')}
  call s:RecursiveDTSort(dag, a:start_nodes, results, visited_nodes)
  call reverse(results)
  return results
endfunction

"# The real, recursive, T-Sort               {{{2
"see boost.graph for a non recursive implementation
function! s:RecursiveDTSort(dag, start_nodes, results, visited_nodes)
  for node in a:start_nodes
    let visited = a:visited_nodes.Visited(node)
    if     visited == 1 | continue " done
    elseif visited == 2 | throw "Tsort: cyclic graph detected: ".node
    endif
    let a:visited_nodes[node] = 2 " visiting
    let succs = a:dag.successors(node)
    try
      call s:RecursiveDTSort(a:dag, succs, a:results, a:visited_nodes)
    catch /Tsort:/
      throw v:exception.'>'.node
    endtry
    let a:visited_nodes[node] = 1 " visited
    call add(a:results, node)
  endfor
endfunction

function! s:Visited(node) dict 
  return has_key(self, a:node) ? self[a:node] : 0
endfunction

"## Breadth-first search (non recursive)     {{{1
"# lh#graph#tsort#breadth(dag, start_nodes)  {{{2
" warning: This implementation does not work with lazy dag, but only with fully
" defined ones
function! lh#graph#tsort#breadth(dag, start_nodes)
  let result = []
  let dag = s:PrepareDAG(a:dag)
  let queue = deepcopy(a:start_nodes)

  while len(queue) > 0
    let node = remove(queue, 0)
    " echomsg "result <- ".node
    call add(result, node)
    let successors = dag.successors(node)
    while len(successors) > 0
      let m = s:RemoveEdgeFrom(dag, node)
      " echomsg "graph loose ".node."->".m
      if !s:HasIncomingEgde(dag, m)
	" echomsg "queue <- ".m
        call add(queue, m)
      endif
    endwhile
  endwhile
  if !s:Empty(dag)
    throw "Tsort: cyclic graph detected: "
  endif
  return result
endfunction

function! s:HasIncomingEgde(dag, node)
  for node in keys(a:dag.table)
    if type(a:dag.table[node]) != type([])
      continue
    endif
    if index(a:dag.table[node], a:node) != -1
      return 1
    endif
  endfor
  return 0
endfunction
function! s:RemoveEdgeFrom(dag, node)
  let successors = a:dag.successors(a:node)
  if len(successors) > 0
    let successor = remove(successors, 0)
    if len(successors) == 0
      " echomsg "finished with ->".a:node
      call remove(a:dag.table, a:node)
    endif
    return successor
  endif
  throw "No more edges from ".a:node
endfunction
function! s:Empty(dag)
  " echomsg "len="len(a:dag.table)
  return len(a:dag.table) == 0
endfunction
" }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker
doc/lh-vim-lib.txt	[[[1
671
*lh-vim-lib.txt*        Vim common libraries (v2.1.0)
                        For Vim version 7+Last change: $Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $

                        By Luc Hermitte
                        hermitte {at} free {dot} fr


==============================================================================
CONTENTS                                      *lhvl-contents*      {{{1
|lhvl-presentation|     Presentation
|lhvl-functions|        Functions
    
|add-local-help|        Instructions on installing this help file


------------------------------------------------------------------------------
PRESENTATION                                  *lhvl-presentation*  {{{1

|lh-vim-lib| is a library that defines some common VimL functions I use in my
various plugins and ftplugins.
This library has been conceived as a suite of |autoload| plugins, and a few
|macros| plugins. As such, it requires Vim 7+.


==============================================================================
FUNCTIONS                                     *lhvl-functions*     {{{1
{{{3Functions list~
Miscellanous functions:                                 |lhvl#misc|
- |lh#common#ErrorMsg()|
- |lh#common#WarningMsg()|
- |lh#common#CheckDeps()|
- |lh#option#Get()|
- |lh#option#GetNonEmpty()|
- |lh#list#Match()|
- |lh#list#Find_if()|
- |lh#askvim#Exe()|
- |lh#position#IsBefore()|
- |lh#position#CharAtMark()|
- |lh#position#CharAtPos()|
- |lh#event#RegisterForOneExecutionAt()|
- |lh#enconding#Iconv()|
Paths related functions:                                |lhvl#path|
- |lh#path#Simplify()|
- |lh#path#StripCommon()|
- |lh#path#StripStart()|
- |lh#path#isAbsolutePath()|
- |lh#path#isURL()|
- |lh#path#SelectOne()|
- |lh#path#ToRelative()|
- |lh#path#GlobAsList()|
Commands related functions:                             |lhvl#command|
- |lh#command#New()| (alpha version)
- |lh#command#Fargs2String()| (alpha version)
Menus related functions:                                |lhvl#menu|
- |lh#menu#DefToggleItem()|
- |lh#menu#Text()|
- |lh#menu#Make()|
- |lh#menu#IVN_Make()|
- |lh#menu#CMD_andclear_v()| (to document)
- |lh#menu#Map_all()| (to document)
- |lh#askvim#Menu()| (beta version)
Buffers related functions:                              |lhvl#buffer|
- |lh#buffer#Find()|
- |lh#buffer#Jump()|
- |lh#buffer#Scratch()|
- |lh#buffer#dialog#toggle_help()| (beta version ; to document)
- |lh#buffer#dialog#add_help()| (beta version ; to document)
- |lh#buffer#dialog#new()| (beta version ; to document)
Syntax related functions:				|lhvl#syntax|
- |lh#syntax#NameAt()|
- |lh#syntax#NameAtMark()|
- |lh#syntax#Skip()|
- |lh#syntax#SkipAt()|
- |lh#syntax#SkipAtMark()|
- |lh#syntax#SynListRaw()|
- |lh#syntax#SynList()| (to document)
Graphs related functions:				|lhvl#graph|
- |lh#graph#tsort#depth()|
- |lh#graph#tsort#breadth|

}}}2
------------------------------------------------------------------------------
MISCELLANOUS FUNCTIONS                                *lhvl#misc*       {{{2


                                                *lh#common#ErrorMsg()*  {{{3
lh#common#ErrorMsg({text})~
@param  {text}    Error message to display
@return nothing

This function displays an error message in a |confirm()| box if gvim is being
used, or as a standard vim error message through |:echoerr| otherwise. 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                               *lh#common#WarningMsg()* {{{3
lh#common#WarningMsg({text})~
@param  {text}    Error message to display
@return nothing

This function displays a warning message highlighted with |WarningMsg| syntax.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#common#CheckDeps()* {{{3
lh#common#CheckDeps({symbol},{file},{path},{requester})~
@param  {symbol}    Symbol required, see |exists()| for symbol format.
@param  {file}      File in which the symbol is expected to be defined
@param  {path}      Path where the file can be found
@param  {requester} Name of the script in need of this symbol
@return 0/1 whether the {symbol} exists

Checks if {symbol} exists in vim. If not, this function first tries
to |:source| the {file} in which the {symbol} is expected to be defined. If the
{symbol} is still not defined, an error message is issued (with
|lh#common#ErrorMsg()|, and 0 is returned.

Example: >
    if   
          \    !lh#common#CheckDeps('*Cpp_CurrentScope', 
          \                     'cpp_FindContextClass.vim', 'ftplugin/cpp/',
          \                     'cpp#GotoFunctionImpl.vim')
          \ || !lh#common#CheckDeps(':CheckOptions',
          \                     'cpp_options-commands.vim', 'ftplugin/cpp/',
          \                     'cpp#GotoFunctionImpl.vim')
      let &cpo=s:cpo_save
      finish
    endif

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#option#Get()*       {{{3
lh#option#Get({name},{default}[,{scopes}])~
@param {name}       Name of the option to fetch
@param {default}    Default value in case the option is not defined
@param {scopes}     Vim scopes in which the options must be searched,
                    default="bg".
@return b:{name} if it exists, of g:{name} if it exists, or {default}
otherwise.

This function fetches the value of an user defined option (not Vim |options|).
The option can be either a |global-variable|, a |buffer-variable|, or even
a|window-variable|.

The order of the variables checked can be specified through the optional
argument {scopes}. By default, buffer-local options have the priority over
global options.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                              *lh#option#GetNonEmpty()* {{{3
lh#option#GetNonEmpty({name},{default}[,{scopes}])~
@param {name}       Name of the option to fetch
@param {default}    Default value in case the option is not defined, nor empty
@param {scopes}     Vim scopes in which the options must be searched,
                    default="bg".
@return b:{name} if it exists, of g:{name} if it exists, or {default}
otherwise.

This function works exactly like |lh#option#Get()| except that a defined
variable with an empty value will be ignored as well.
An |expr-string| will be considered empty if its |strlen()| is 0, an
|expr-number| when it values 0, |Lists| and |Dictionaries| when their |len()|
is 0.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#list#Match()*       {{{3
lh#list#Match({list},{pattern}[, {start-pos}])~
@param {list}     |List| 
@param {pattern}  |expr-string|
@param {start-pos} First index to check
@return the lowest index, >= {start-pos}, in |List| {list} where the item
matches {pattern}.
@returns -1 if no item matches {pattern}.
@see |index()|, |match()|

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#list#Find_if()*     {{{3
lh#list#Find_if({list},{predicate} [, {pred-parameters}][, {start-pos}])~
@param {list}             |List| 
@param {predicate}         Predicate to evaluate
@param {pred-parameters}] |List| of Parameters to bind to special arguments in
                           the {predicate}.
@param {start-pos}         First index to check
@return the lowest index, >= {start-pos}, in |List| {list} where the
{predicate} evals to true.
@returns -1 if no item matches {pattern}.
@see |index()|, |eval()|

The predicate recognizes some special arguments:
- |v:val| is subtituted with the current element being evaluated in the list
- *v:1_* *v:2_* , ..., are substituted with the i-th elements from
  {pred-parameters}.
  NB: the "v:\d\+_" are 1-indexed while {pred-parameters} is indeed seen as
  0-indexed by Vim. 
  This particular feature permits to pass any type of variable to the
  predicate: a |expr-string|, a |List|, a |Dictionary|, ...

e.g.: >
    :let b = { 'min': 12, 'max': 42 }
    :let l = [ 1, 5, 48, 25, 5, 28, 6]
    :let i = lh#list#Find_if(l, 'v:val>v:1_.min  && v:val<v:1_.max && v:val%v:2_==0', [b, 2] )
    :echo l[i]
    28

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#askvim#Exe()*       {{{3
lh#askvim#Exe({command})~
@param {command}  Command to execute from vim.
@return what the command echos while executed.
@note This function encapsultates |redir| without altering any register.

Some informations aren't directly accessible through vim API (|functions|).
However, they can be obtained by executing some commands, and redirecting the
result of these commands.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#askvim#Menu()*      {{{3
lh#askvim#Menu({menuid},{modes})~
@param {menuid}  Menu identifier.
@param {modes}   List of modes
@return Information related to the {menuid}
@todo still bugged

This function provides a way to obtain information related to a menu entry in
Vim.

The format of the result being «to be stabilized»

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                               *lh#position#IsBefore()* {{{3
lh#position#IsBefore({lhs_pos},{rhs_pos})~
@param[in] positions as those returned from |getpos()|
@return whether {lhs_pos} is before {rhs_pos}

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                             *lh#position#CharAtMark()* {{{3
lh#position#CharAtMark({mark})~
@return the character at a given |mark|.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                              *lh#position#CharAtPos()* {{{3
lh#position#CharAtPos({pos})~
@return the character at a position (see |getpos()|).


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                 *lh#event#RegisterForOneExecutionAt()* {{{3
lh#event#RegisterForOneExecutionAt({event}, {cmd}, {group})~
Registers a command to be executed once (and only once) when {event} is
triggered on the current file.

@param {event}  Event that will trigger the execution of {cmd}|autocmd-events|
@param {cmd}   |expression-command| to execute
@param {group} |autocmd-groups| under which the internal autocommand will be
                registered.
@todo possibility to specify the file pattern

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
lh#enconding#Iconv({expr}, {from}, {to})~
This function just calls |inconv()| with the same arguments. The only
difference is that it return {expr} when we know that |iconv()| will return an
empty string.


------------------------------------------------------------------------------
PATHS RELATED FUNCTIONS                               *lhvl#path*       {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#Simplify()*    {{{3
lh#path#Simplify({pathname})~
@param {pathname}  Pathname to simplify
@return the simplified pathname

This function works like |simplify()|, except that it also strips the leading
"./".

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#StripCommon()* {{{3
lh#path#StripCommon({pathnames})~
@param[in,out] {pathnames} |List| of pathnames to simplify
@return the simplified pathnames

This function strips all pathnames from their common leading part.
e.g.: >
 :echo lh#path#StripCommon(['foo/bar/file','foo/file', 'foo/foo/file'])
echoes >
 ['bar/file','file', 'foo/file']

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                 *lh#path#StripStart()* {{{3
lh#path#StripStart({pathname}, {pathslist})~
@param[in] {pathname} name to simplify
@param[in] {pathslist} list of pathname (can be a |string| of pathnames
separated by ",", of a |List|).
Strip, {pathname}, the occurrence of any path from {pathslist}.

This function strips all pathnames from their common leading part.
e.g.: >
 :echo lh#path#StripStart($HOME.'/.vim/template/template.bar',
   \ ['/home/foo/.vim', '/usr/local/share/vim/'])
 :echo lh#path#StripStart($HOME.'/.vim/template/template.bar',&rtp)
echoe >
 template/template.bar

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#isAbsolutePath()* {{{3
lh#path#isAbsolutePath({path})~
@return {path} Path to test
@return whether the path is an absolute path
@note Supports Unix absolute paths, Windows absolute paths, and UNC paths

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#isURL()*       {{{3
lh#path#isURL({path})~
@return {path} Path to test
@return whether the path is an URL
@note Supports http(s)://, (s)ftp://, dav://, fetch://, file://, rcp://,
rsynch://, scp://

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#SelectOne()*   {{{3
lh#path#SelectOne({pathnames},{prompt})~
@param[in] {pathnames} |List| of pathname
@param     {prompt}     Prompt for the dialog box

@return "" if len({pathnames}) == 0
@return {pathnames}[0] if len({pathnames}) == 1
@return the selected pathname otherwise

Asks the end-user to choose a pathname among a list of pathnames.
The pathnames displayed will be simplified thanks to |StripCommon()| -- the
pathname returned is the "full" original pathname matching the simplified
pathname selected.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#ToRelative()*  {{{3
lh#path#ToRelative({pathname})~
@param {pathname} Pathname to convert
@return the pathname in it relative form as it would be seen from the current
directory.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#path#GlobAsList()*  {{{3
lh#path#GlobAsList({pathslist}, {expr})~
@return |globpath()|'s result, but formatted as a list of matching pathnames.
In case {expr} is a |List|, |globpath()| is applied on each expression in
{expr}.


------------------------------------------------------------------------------
MENUS RELATED FUNCTIONS                               *lhvl#menu*       {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                             *lh#menu#DefToggleItem()*  {{{3
lh#menu#DefToggleItem({Data})~
@param[in,out] {Data} Definition of a |menu| item.

This function defines a |menu| entry that will be associated to a
|global-variable| whose values can be cycled and explored from the menu. This
global variable can be seen as an enumerate whose value can be cyclically
updated through a menu.

{Data} is a |Dictionary| whose keys are:
- "variable": name of the |global-variable| to bind to the menu entry
  Mandatory.
- "values": associated values of string or integers (|List|)
  Mandatory.
- "menu": describes where the menu entry must be placed (|Dictionary|)
    - "priority": complete priority of the entry (see |sub-menu-priority|)
    - "name": complete name of the entry -- ampersand (&) can be used to define
      shortcut keys
  Mandatory.
- "idx_crt_value": index of the current value for the option (|expr-number|)
  This is also an internal variable that will be automatically updated to
  keep the index of the current value of the "variable" in "values".
  Optional ; default value is 1, or the associated index of the initial value
  of the variable (in "values") before the function call.
- "texts": texts to display according to the variable value (|List|)
  Optional, "values" will be used by default. This option is to be used to
  distinguish the short encoded value, from the long self explanatory name.

Warning:
    If the variable is changed by hand without using the menu, then the menu
    and the variable will be out of synch. Unless the automatically generated
    command *lhvl-:Toggle{Varname}* is used to change the value of the options
    (and keep the menu synchronized)

Examples:
   See tests/lh/test-toggle-menu.vim

Todo:
    Propose a *lhvl-:Set{vaName}* command.
    Is it really a good function name, won't ToggleMenu() be better ?

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                      *lh#menu#Text()*  {{{3
Function: lh#menu#Text({text})~
@param[in] {text} Text to send to |:menu| commands
@return a text to be used in menus where "\" and spaces have been escaped.

This helper function transforms a regular text into a text that can be
directly used with |:menu| commands.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                      *lh#menu#Make()*  {{{3
option: *[gb]:want_buffermenu_or_global_disable*

lh#menu#Make({modes}, {menu-priority}, {menu-text}, {key-binding}, [<buffer>,]  {action})~
@param[in] {modes} Vim modes the menus and maps will be provided for
@param[in] {menu-priority} |sub-menu-priority| for the new menu entry
@param[in] {menu-text}      Name of the new menu entry
@param[in] {key-binding}    Sequence of keys to execute to associated action
@param[in] "<buffer>"       If the string "<buffer>" is provided, then the 
                            associated mapping will be a |map-<buffer>|, and
                            the menu will be available to the current buffer
                            only. See |[gb]:want_buffermenu_or_global_disable|
                            When "<buffer>" is set, the call to lh#menu#Make()
                            must be done in the buffer-zone of a |ftplugin|.
@param[in] {action}         Action to execute when {key-binding} is typed, or
                            when the menu entry is selected.

First example: the following call will add the menu "LaTeX.Run LaTeX once
<C-L><C-O>", with the priority (placement) 50.305, for the NORMAL, INSERT and
COMMAND modes. The action associated first saves all the changed buffers and
then invokes LaTeX. The same action is also binded to <C-L><C-O> for the same
modes, with the nuance that the maps will be local to the buffer ; I haven't
tried yet to integrate Michael Geddes's Buffer-menus plugin.
>
  call lh#menu#Make("nic", '50.305', '&LaTeX.Run LaTeX &once', "<C-L><C-O>",
          \ '<buffer>', ":wa<CR>:call TKMakeDVIfile(1)<CR>")

The second example demonstrates an hidden, but useful, behavior: if the mode
is the visual one, then the register v is filled with the text of the visual
area. This text can then be used in the function called. Here, it will be
proposed as a default name for the section to insert:
>
  function! TKinsertSec()
    " ...
    if (strlen(@v) != 0) && (visualmode() == 'v')
      let SecName = input("name of ".SecType.": ", @v)
    else
      let SecName = input("name of ".SecType.": ")
    endif
    " ...
  endfunction
  
  call lh#menuMake("nic", '50.360.100', '&LaTeX.&Insert.&Section',
          \ "<C-L><C-S>", '<buffer>', ":call TKinsertSec()<CR>")

We have to be cautious to one little thing: there is a side effect: the visual
mode vanishes when we enter the function. If you don't want this to happen,
use the non-existant command: |:VCall| ...

Third: if it is known that a function will be called only under |VISUAL-mode|,
and that we don't want of the previous behavior, we can explicitly invoke the
function with |:VCall| -- command that doesn't have to exist. Check
|s:MapMenu4Env| for such an example.

Fourth thing: actually, lh#menu#Make() is not restricted to commands. The
action can be anything that could come at the right hand side of any |:map| or
|:menu| action. But this time, you have to be cautious with the modes you
dedicate your map to. I won't give any related example ; this is the
underlying approach in |IVN_MenuMake()|. 

Examples:
   See tests/lh/test-menu-map.vim

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                  *lh#menu#IVN_Make()*  {{{3
Mappings & menus inserting text~
lh#menu#IVN_Make(<priority>, {text}, {key}, {IM-action}, {VM-action}, {NM-action} [, {nore-IM}, {nore-VM}, {nore-NM}])~

lh#menu#IVN_MenuMake() accepts three different actions for the three modes:
INSERT, VISUAL and NORMAL. The mappings defined will be relative to the
current buffer -- this function is addressed to ftplugins writers. The last
arguments specify the inner mappings and abbreviations embedded within the
actions should be expanded or not ; i.e. are we defining «noremaps/noremenus» ?

You could find very simple examples of what could be done at the end of
menu-map.vim. Instead, I'll show here an extract of my TeX ftplugin: it
defines complex functions that will help to define very simply the different
mappings I use. You could find another variation on this theme in
html_set.vim.

>
  :MapMenu 50.370.300 &LaTeX.&Fonts.&Emphasize ]em emph
  call <SID>MapMenu4Env("50.370.200", '&LaTeX.&Environments.&itemize',
        \ ']ei', 'itemize', '\item ')
  

The first command binds ]em to \emph{} for the three different modes. In
INSERT mode, the cursor is positioned between the curly brackets, and a marker
is added after the closing bracket -- cf. my bracketing system. In VISUAL
mode, the curly brackets are added around the visual area. In NORMAL mode, the
area is considered to be the current word.

The second call binds for the three modes: ]ei to:
>
      \begin{itemize}
          \item
      \end{itemize}
    
The definition of the different functions and commands involved just follows.
>
  command -nargs=1 -buffer MapMenu :call <SID>MapMenu(<f-args>)
  
  function! s:MapMenu(code,text,binding, tex_cmd, ...)
    let _2visual = (a:0 > 0) ? a:1 : "viw"
    " If the tex_cmd starts with an alphabetic character, then suppose the
    " command must begin with a '\'.
    let texc = ((a:tex_cmd[0] =~ '\a') ? '\' : "") . a:tex_cmd
    call lh#menu#IVN_Make(a:code, a:text.'     --  ' . texc .'{}', a:binding,
          \ texc.'{',
          \ '<ESC>`>a}<ESC>`<i' . texc . '{<ESC>%l',
          \ ( (_2visual=='0') ? "" : _2visual.a:binding),
          \ 0, 1, 0)
  endfunction

  " a function and its map to close a "}", and that works whatever the
  " activation states of the brackets and marking features are.
  function! s:Close()
    if strlen(maparg('{')) == 0                    | exe "normal a} \<esc>"
    elseif exists("b:usemarks") && (b:usemarks==1) | exe "normal ¡jump! "
    else                                           | exe "normal a "
    endif
  endfunction

  imap <buffer> ¡close! <c-o>:call <SID>Close()<cr>

  function! s:MapMenu4Env(code,text,binding, tex_env, middle, ...)
    let _2visual = (a:0 > 0) ? a:1 : "vip"
    let b = "'" . '\begin{' . a:tex_env . '}' . "'"
    let e = "'" . '\end{' . a:tex_env . '}' . "'"
    call IVN_MenuMake(a:code, a:text, a:binding,
          \ '\begin{'.a:tex_env.'¡close!<CR>'.a:middle.' <CR>\end{'.a:tex_env.'}<C-F><esc>ks',
          \ ':VCall MapAroundVisualLines('.b. ',' .e.',1,1)',
          \ _2visual.a:binding,
          \ 0, 1, 0)
  endfunction

Examples:
   See tests/lh/test-menu-map.vim


------------------------------------------------------------------------------
                                            *lh#menu#CMD_andclear_v()*  {{{3
                                                   *lh#menu#Map_all()*  {{{3

------------------------------------------------------------------------------
???? RELATED FUNCTIONS                                *lh#????*         {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                     *lh#analysis#IsKindOfEmptyLine()*  {{{3


------------------------------------------------------------------------------
COMMANDS RELATED FUNCTIONS                            *lhvl#command*    {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#command#New()*      {{{3
Highly Experimental.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                       *lh#command#Fargs2String()*      {{{3
lh#command#Fargs2String({aList})~
@param[in,out] aList list of params from <f-args>
@see tests/lh/test-Fargs2String.vim


------------------------------------------------------------------------------
BUFFERS RELATED FUNCTIONS                             *lhvl#buffer*     {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#buffer#Find()*      {{{3
lh#buffer#Find({filename})~
@param {filename}
@return The number of the first window found, in which {filename} is opened.

If {filename} is opened in a window, jump to this window. Otherwise, return
-1.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#buffer#Jump()*      {{{3
lh#buffer#Jump({filename}, {cmd})~
@param {filename}
@param {cmd}
@return Nothing.

If {filename} is opened in a window, jump to this window. 
Otherwise, execute {cmd} with {filename} as a parameter. Typical values for
the command will be "sp" or "vsp". (see |:split|, |:vsplit|)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#buffer#Scratch()*   {{{3
lh#buffer#Scratch({bname},{where})~
@param {bname} Name for the new scratch buffer
@param {where} Where the new scratch buffer will be opened ('', or 'v')

This function opens a new scratch buffer.


------------------------------------------------------------------------------
SYNTAX RELATED FUNCTIONS                              *lhvl#syntax*     {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#syntax#NameAt()*    {{{3
lh#syntax#NameAt({lnum},{col}[,{trans}])~
@param {lnum}  line of the character
@param {col}   column of the character
@param {trans} see |synID()|, default=0
@return the syntax kind of the given character at {lnum}, {col}

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                               *lh#syntax#NameAtMark()* {{{3
lh#syntax#NameAtMark({mark}[,{trans}])~
@param {mark}  position of the character
@param {trans} see |synID()|, default=0
@return the syntax kind of the character at the givenn |mark|.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       *lh#syntax#Skip()* *lh#syntax#SkipAt()* *lh#syntax#SkipAtMark()* {{{3
lh#syntax#Skip()~
lh#syntax#SkipAt({lnum},{col})~
lh#syntax#SkipAtMark({mark})~

Functions to be used with |searchpair()| functions in order to search for a
pair of elements, without taking comments, strings, characters and doxygen
(syntax) contexts into account while searching.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                               *lh#syntax#SynListRaw()* {{{3
lh#syntax#SynListRaw({name})~
@param {group-name} 
@return the result of "syn list {group-name}" as a string

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                *lh#syntax#SynList()*   {{{3
lh#syntax#SynList()~


------------------------------------------------------------------------------
GRAPH RELATED FUNCTIONS                               *lhvl#syntax*     {{{2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                             *lh#graph#tsort#depth()*   {{{3
                                             *lh#graph#tsort#breadth()* {{{3
lh#graph#tsort#depth({dag}, {start-nodes})~
lh#graph#tsort#breadth({dag}, {start-nodes})~
@param {dag} is either 
	     - a |Dictionnary| that associates to each node, the |List| of all
	       its successors
	     - or a /fetch/ |function()| that returns the |List| of the
	       successors of a given node -- works only with depth() which
	       takes care of not calling this function more than once for each
	       given node.
@param {start-nodes} is a |List| of start nodes with no incoming edge
@throw "Tsort: cyclic graph detected:" if {dag} is not a DAG.
@see http://en.wikipedia.org/wiki/Topological_sort
@since Version 2.1.0
@test tests/lh/topological-sort.vim

These two functions implement a topological sort on the Direct Acyclic Graph
{dag}. 
- depth() is a recursive implementation of a depth-first search. 
- breadth() is a non recursive implementation of a breadth-first search.

------------------------------------------------------------------------------
                                                                     }}}1
==============================================================================
 © Luc Hermitte, 2001-2008, <http://hermitte.free.fr/vim/>           {{{1
 $Id: lh-vim-lib.txt 101 2008-04-23 00:22:05Z luc.hermitte $
 VIM: let b:VS_language = 'american' 
 vim:ts=8:sw=4:tw=80:fo=tcq2:isk=!-~,^*,^\|,^\":ft=help:
 vim600:fdm=marker:
macros/menu-map.vim	[[[1
83
"===========================================================================
" $Id: menu-map.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		macros/menu-map.vim
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
" 		<URL:http://hermitte.free.fr/vim/>
"
" Purpose:	Define functions to build mappings and menus at the same time
"
" Version:	2.0.0
" Last Update:  $Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $ (02nd Dec 2006)
"
" Last Changes: {{{
" 	Version 2.0.0:
" 		Moved to vim7, 
" 		Functions moved to {rtp}/autoload/
" 	Version 1.6.2: 
" 		(*) Silent mappings and menus
" 	Version 1.6. : 
" 		(*) Uses has('gui_running') instead of has('gui') to check if
" 		we can generate the menu.
" 	Version 1.5. : 
" 		(*) visual mappings launched from select-mode don't end with
" 		    text still selected -- applied to :VCalls
" 	Version 1.4. : 
" 		(*) address obfuscated for spammers
" 		(*) support the local option 
" 		    b:want_buffermenu_or_global_disable if we don't want
" 		    buffermenu to be used systematically.
" 		    0 -> buffer menu not used
" 		    1 -> buffer menu used
" 		    2 -> the VimL developper will use a global disable.
" 		    cf.:   tex-maps.vim:: s:SimpleMenu()
" 		       and texmenus.vim
" 	Version 1.3. :
"		(*) add continuation lines support ; cf 'cpoptions'
" 	Version 1.2. :
" 		(*) Code folded.
" 		(*) Take advantage of buffermenu.vim if present for local
" 		    menus.
" 		(*) If non gui is available, the menus won't be defined
" 	Version 1.1. :
"               (*) Bug corrected : 
"                   vnore(map\|menu) does not imply v+n(map\|menu) any more
" }}}
"
" Inspired By:	A function from Benji Fisher
"
" TODO:		(*) no menu if no gui.
"
"===========================================================================

if exists("g:loaded_menu_map") | finish | endif
let g:loaded_menu_map = 1  

"" line continuation used here ??
let s:cpo_save = &cpo
set cpo&vim

"=========================================================================
" Commands {{{
command! -nargs=+ -bang      MAP      map<bang> <args>
command! -nargs=+           IMAP     imap       <args>
command! -nargs=+           NMAP     nmap       <args>
command! -nargs=+           CMAP     cmap       <args>
command! -nargs=+           VMAP     vmap       <args>
command! -nargs=+           AMAP
      \       call lh#menu#Map_all('amap', <f-args>)

command! -nargs=+ -bang  NOREMAP  noremap<bang> <args>
command! -nargs=+       INOREMAP inoremap       <args>
command! -nargs=+       NNOREMAP nnoremap       <args>
command! -nargs=+       CNOREMAP cnoremap       <args>
command! -nargs=+       VNOREMAP vnoremap       <args>
command! -nargs=+       ANOREMAP
      \       call lh#menu#Map_all('anoremap', <f-args>)
" }}}

" End !
let &cpo = s:cpo_save
finish

"=========================================================================
" vim600: set fdm=marker:
plugin/ui-functions.vim	[[[1
466
"=============================================================================
" File:		ui-functions.vim					{{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" URL: http://hermitte.free.fr/vim/ressources/vimfiles/plugin/ui-functions.vim
" 
" Version:	0.06
" Created:	18th nov 2002
" Last Update:	28th Nov 2007
"------------------------------------------------------------------------
" Description:	Functions for the interaction with a User Interface.
" 		The UI can be graphical or textual.
" 		At first, this was designed to ease the syntax of
" 		mu-template's templates.
"
" Option:	{{{2
"	{[bg]:ui_type} 
" 		= "g\%[ui]", 
" 		= "t\%[ext]" ; the call must not be |:silent|
" 		= "f\%[te]"
" }}}2
"------------------------------------------------------------------------
" Installation:	Drop this into one of your {rtp}/plugin/ directories.
" History:	{{{2
"    v0.01 Initial Version
"    v0.02
"	(*) Code "factorisations" 
"	(*) Help on <F1> enhanced.
"	(*) Small changes regarding the parameter accepted
"	(*) Function SWITCH
"    v0.03
"	(*) Small bug fix with INPUT()
"    v0.04
"	(*) New function: WHICH()
"    v0.05
"       (*) In vim7e, inputdialog() returns a trailing '\n'. INPUT() strips the
"           NL character.
"    v0.06
"       (*) :s/echoerr/throw/ => vim7 only
" 
" TODO:		{{{2
" 	(*) Save the hl-User1..9 before using them
" 	(*) Possibility other than &statusline:
" 	    echohl User1 |echon "bla"|echohl User2|echon "bli"|echohl None
" 	(*) Wraps too long choices-line (length > term-width)
" 	(*) Add to the documentation: "don't use CTRL-C to abort !!"
" 	(*) Look if I need to support 'wildmode'
" 	(*) 3rd mode: return string for FTE
" 	(*) 4th mode: interaction in a scratch buffer
"
" }}}1
"=============================================================================
" Avoid reinclusion {{{1
" 
if exists("g:loaded_ui_functions") && !exists('g:force_reload_ui_functions')
  finish 
endif
let g:loaded_ui_functions = 1
let s:cpo_save=&cpo
set cpo&vim
" }}}1
"------------------------------------------------------------------------
" External functions {{{1
" Function: IF(var, then, else) {{{2
function! IF(var,then, else)
  let o = s:Opt_type() " {{{3
  if     o =~ 'g\%[ui]\|t\%[ext]' " {{{4
    return a:var ? a:then : a:else
  elseif o =~ 'f\%[te]'           " {{{4
    return s:if_fte(a:var, a:then, a:else)
  else                    " {{{4
    throw "UI-Fns::IF(): Unkonwn user-interface style (".o.")"
  endif
  " }}}3
endfunction

" Function: SWITCH(var, case, action [, case, action] [default_action]) {{{2
function! SWITCH(var, ...)
  let o = s:Opt_type() " {{{3
  if     o =~ 'g\%[ui]\|t\%[ext]' " {{{4
    let explicit_def = ((a:0 % 2) == 1)
    let default      = explicit_def ? a:{a:0} : ''
    let i = a:0 - 1 - explicit_def
    while i > 0
      if a:var == a:{i}
	return a:{i+1}
      endif
      let i = i - 2
    endwhile
    return default
  elseif o =~ 'f\%[te]'           " {{{4
    return s:if_fte(a:var, a:then, a:else)
  else                    " {{{4
    throw "UI-Fns::SWITCH(): Unkonwn user-interface style (".o.")"
  endif
  " }}}3
endfunction

" Function: CONFIRM(text [, choices [, default [, type]]]) {{{2
function! CONFIRM(text, ...)
  " 1- Check parameters {{{3
  if a:0 > 4 " {{{4
    throw "UI-Fns::CONFIRM(): too many parameters"
    return 0
  endif
  " build the parameters string {{{4
  let i = 1
  while i <= a:0
    if i == 1 | let params = 'a:{1}'
    else      | let params = params. ',a:{'.i.'}'
    endif
    let i = i + 1
  endwhile
  " 2- Choose the correct way to execute according to the option {{{3
  let o = s:Opt_type()
  if     o =~ 'g\%[ui]'  " {{{4
    exe 'return confirm(a:text,'.params.')'
  elseif o =~ 't\%[ext]' " {{{4
    if !has('gui_running') && has('dialog_con')
      exe 'return confirm(a:text,'.params.')'
    else
      exe 'return s:confirm_text("none", a:text,'.params.')'
    endif
  elseif o =~ 'f\%[te]'  " {{{4
      exe 'return s:confirm_fte(a:text,'.params.')'
  else               " {{{4
    throw "UI-Fns::CONFIRM(): Unkonwn user-interface style (".o.")"
  endif
  " }}}3
endfunction

" Function: INPUT(prompt [, default ]) {{{2
function! INPUT(prompt, ...)
  " 1- Check parameters {{{3
  if a:0 > 4 " {{{4
    throw "UI-Fns::INPUT(): too many parameters"
    return 0
  endif
  " build the parameters string {{{4
  let i = 1 | let params = ''
  while i <= a:0
    if i == 1 | let params = 'a:{1}'
    else      | let params = params. ',a:{'.i.'}'
    endif
    let i = i + 1
  endwhile
  " 2- Choose the correct way to execute according to the option {{{3
  let o = s:Opt_type()
  if     o =~ 'g\%[ui]'  " {{{4
    exe 'return matchstr(inputdialog(a:prompt,'.params.'), ".\\{-}\\ze\\n\\=$")'
  elseif o =~ 't\%[ext]' " {{{4
    exe 'return input(a:prompt,'.params.')'
  elseif o =~ 'f\%[te]'  " {{{4
      exe 'return s:input_fte(a:prompt,'.params.')'
  else               " {{{4
    throw "UI-Fns::INPUT(): Unkonwn user-interface style (".o.")"
  endif
  " }}}3
endfunction

" Function: COMBO(prompt, choice [, ... ]) {{{2
function! COMBO(prompt, ...)
  " 1- Check parameters {{{3
  if a:0 > 4 " {{{4
    throw "UI-Fns::COMBO(): too many parameters"
    return 0
  endif
  " build the parameters string {{{4
  let i = 1
  while i <= a:0
    if i == 1 | let params = 'a:{1}'
    else      | let params = params. ',a:{'.i.'}'
    endif
    let i = i + 1
  endwhile
  " 2- Choose the correct way to execute according to the option {{{3
  let o = s:Opt_type()
  if     o =~ 'g\%[ui]'  " {{{4
    exe 'return confirm(a:prompt,'.params.')'
  elseif o =~ 't\%[ext]' " {{{4
    exe 'return s:confirm_text("combo", a:prompt,'.params.')'
  elseif o =~ 'f\%[te]'  " {{{4
    exe 'return s:combo_fte(a:prompt,'.params.')'
  else               " {{{4
    throw "UI-Fns::COMBO(): Unkonwn user-interface style (".o.")"
  endif
  " }}}3
endfunction

" Function: WHICH(function, prompt, choice [, ... ]) {{{2
function! WHICH(fn, prompt, ...)
  " 1- Check parameters {{{3
  " build the parameters string {{{4
  let i = 1
  while i <= a:0
    if i == 1 | let params = 'a:{1}'
    else      | let params = params. ',a:{'.i.'}'
    endif
    let i = i + 1
  endwhile
  " 2- Execute the function {{{3
  exe 'let which = '.a:fn.'(a:prompt,'.params.')'
  if     0 >= which | return ''
  elseif 1 == which
    return substitute(matchstr(a:{1}, '^.\{-}\ze\%(\n\|$\)'), '&', '', 'g')
  else
    return substitute(
	  \ matchstr(a:{1}, '^\%(.\{-}\n\)\{'.(which-1).'}\zs.\{-}\ze\%(\n\|$\)')
	  \ , '&', '', 'g')
  endif
  " }}}3
endfunction

" Function: CHECK(prompt, choice [, ... ]) {{{2
function! CHECK(prompt, ...)
  " 1- Check parameters {{{3
  if a:0 > 4 " {{{4
    throw "UI-Fns::CHECK(): too many parameters"
    return 0
  endif
  " build the parameters string {{{4
  let i = 1
  while i <= a:0
    if i == 1 | let params = 'a:{1}'
    else      | let params = params. ',a:{'.i.'}'
    endif
    let i = i + 1
  endwhile
  " 2- Choose the correct way to execute according to the option {{{3
  let o = s:Opt_type()
  if     o =~ 'g\%[ui]'  " {{{4
    exe 'return s:confirm_text("check", a:prompt,'.params.')'
  elseif o =~ 't\%[ext]' " {{{4
    exe 'return s:confirm_text("check", a:prompt,'.params.')'
  elseif o =~ 'f\%[te]'  " {{{4
      exe 'return s:check_fte(a:prompt,'.params.')'
  else               " {{{4
    throw "UI-Fns::CHECK(): Unkonwn user-interface style (".o.")"
  endif
  " }}}3
endfunction

" }}}1
"------------------------------------------------------------------------
" Internal functions {{{1
function! s:Option(name, default) " {{{2
  if     exists('b:ui_'.a:name) | return b:ui_{a:name}
  elseif exists('g:ui_'.a:name) | return g:ui_{a:name}
  else                          | return a:default
  endif
endfunction


function! s:Opt_type() " {{{2
  return s:Option('type', 'gui')
endfunction

"
" Function: s:status_line(current, hl [, choices] ) {{{2
"     a:current: current item
"     a:hl     : Generic, Warning, Error
function! s:status_line(current, hl, ...)
  " Highlightning {{{3
  if     a:hl == "Generic"  | let hl = '%1*'
  elseif a:hl == "Warning"  | let hl = '%2*'
  elseif a:hl == "Error"    | let hl = '%3*'
  elseif a:hl == "Info"     | let hl = '%4*'
  elseif a:hl == "Question" | let hl = '%5*'
  else                      | let hl = '%1*'
  endif
  
  " Build the string {{{3
  let sl_choices = '' | let i = 1
  while i <= a:0
    if i == a:current
      let sl_choices = sl_choices . ' '. hl . 
	    \ substitute(a:{i}, '&\(.\)', '%6*\1'.hl, '') . '%* '
    else
      let sl_choices = sl_choices . ' ' . 
	    \ substitute(a:{i}, '&\(.\)', '%6*\1%*', '') . ' '
    endif
    let i = i + 1
  endwhile
  " }}}3
  return sl_choices
endfunction


" Function: s:confirm_text(box, text [, choices [, default [, type]]]) {{{2
function! s:confirm_text(box, text, ...)
  let help = "/<esc>/<s-tab>/<tab>/<left>/<right>/<cr>/<F1>"
  " 1- Retrieve the parameters       {{{3
  let choices = ((a:0>=1) ? a:1 : '&Ok')
  let default = ((a:0>=2) ? a:2 : (('check' == a:box) ? 0 : 1))
  let type    = ((a:0>=3) ? a:3 : 'Generic')
  if     'none'  == a:box | let prefix = ''
  elseif 'combo' == a:box | let prefix = '( )_'
  elseif 'check' == a:box | let prefix = '[ ]_'
    let help = '/ '.help
  else                    | let prefix = ''
  endif


  " 2- Retrieve the proposed choices {{{3
  " Prepare the hot keys
  let i = 0
  while i != 26
    let hotkey_{nr2char(i+64)} = 0
    let i = i + 1
  endwhile
  let hotkeys = '' | let help_k = '/'
  " Parse the choices
  let i = 0
  while choices != ""
    let i = i + 1
    let item    = matchstr(choices, "^.\\{-}\\ze\\(\n\\|$\\)")
    let choices = matchstr(choices, "\n\\zs.*$")
    " exe 'anoremenu ]'.a:text.'.'.item.' :let s:choice ='.i.'<cr>'
    if ('check' == a:box) && (strlen(default)>=i) && (1 == default[i-1])
      " let choice_{i} = '[X]' . substitute(item, '&', '', '')
      let choice_{i} = '[X]_' . item
    else
      " let choice_{i} = prefix . substitute(item, '&', '', '')
      let choice_{i} = prefix . item
    endif
    if i == 1
      let list_choices = 'choice_{1}'
    else
      let list_choices = list_choices . ',choice_{'.i.'}'
    endif
    " Update the hotkey.
    let key = toupper(matchstr(choice_{i}, '&\zs.\ze'))
    let hotkey_{key} = i
    let hotkeys = hotkeys . tolower(key) . toupper(key)
    let help_k = help_k . tolower(key)
  endwhile
  let nb_choices = i
  if default > nb_choices | let default = nb_choices | endif

  " 3- Run an interactive text menu  {{{3
  " Note: emenu can not be used through ":exe" {{{4
  " let wcm = &wcm
  " set wcm=<tab>
  " exe ':emenu ]'.a:text.'.'."<tab>"
  " let &wcm = wcm
  " 3.1- Preparations for the statusline {{{4
  " save the statusline
  let sl = &l:statusline
  " Color schemes for selected item {{{5
  :hi User1 term=inverse,bold cterm=inverse,bold ctermfg=Yellow 
	\ guifg=Black guibg=Yellow
  :hi User2 term=inverse,bold cterm=inverse,bold ctermfg=LightRed
	\ guifg=Black guibg=LightRed
  :hi User3 term=inverse,bold cterm=inverse,bold ctermfg=Red 
	\ guifg=Black guibg=Red
  :hi User4 term=inverse,bold cterm=inverse,bold ctermfg=Cyan
	\ guifg=Black guibg=Cyan
  :hi User5 term=inverse,bold cterm=inverse,bold ctermfg=LightYellow
	\ guifg=Black guibg=LightYellow
  :hi User6 term=inverse,bold cterm=inverse,bold ctermfg=LightGray
	\ guifg=DarkRed guibg=LightGray
  " }}}5

  " 3.2- Interactive loop                {{{4
  let help =  "\r-- Keys available (".help_k.help.")"
  " item selected at the start
  let i = ('check' != a:box) ? default : 1
  let direction = 0 | let toggle = 0
  while 1
    if 'combo' == a:box
      let choice_{i} = substitute(choice_{i}, '^( )', '(*)', '')
    endif
    " Colored statusline
    " Note: unfortunately the 'statusline' is a global option, {{{
    " not a local one. I the hope that may change, as it does not provokes any
    " error, I use '&l:statusline'. }}}
    exe 'let &l:statusline=s:status_line(i, type,'. list_choices .')'
    if has(':redrawstatus')
      redrawstatus!
    else
      redraw!
    endif
    " Echo the current selection
    echo "\r". a:text.' '.substitute(choice_{i}, '&', '', '')
    " Wait the user to hit a key
    let key=getchar()
    let complType=nr2char(key)
    " If the key hit matched awaited keys ...
    if -1 != stridx(" \<tab>\<esc>\<enter>".hotkeys,complType) ||
	  \ (key =~ "\<F1>\\|\<right>\\|\<left>\\|\<s-tab>")
      if key           == "\<F1>"                       " Help      {{{5
	redraw!
	echohl StatusLineNC
	echo help
	echohl None
	let key=getchar()
	let complType=nr2char(key)
      endif
      " TODO: support CTRL-D
      if     complType == "\<enter>"                    " Validate  {{{5
	break
      elseif complType == " "                           " check box {{{5
	let toggle = 1
      elseif complType == "\<esc>"                      " Abort     {{{5
	let i = -1 | break
      elseif complType == "\<tab>" || key == "\<right>" " Next      {{{5
	let direction = 1
      elseif key =~ "\<left>\\|\<s-tab>"                " Previous  {{{5
	let direction = -1
      elseif -1 != stridx(hotkeys, complType )          " Hotkeys     {{{5
	if '' == complType  | continue | endif
	let direction = hotkey_{toupper(complType)} - i
	let toggle = 1
      " else
      endif
      " }}}5
    endif
    if direction != 0 " {{{5
      if 'combo' == a:box
	let choice_{i} = substitute(choice_{i}, '^(\*)', '( )', '')
      endif
      let i = i + direction
      if     i > nb_choices | let i = 1 
      elseif i == 0         | let i = nb_choices
      endif
      let direction = 0
    endif
    if toggle == 1    " {{{5
      if 'check' == a:box
	let choice_{i} = ((choice_{i}[1] == ' ')? '[X]' : '[ ]') 
	      \ . strpart(choice_{i}, 3)
      endif
      let toggle = 0
    endif
  endwhile " }}}4
  " 4- Terminate                     {{{3
  " Clear screen
  redraw!

  " Restore statusline
  let &l:statusline=sl
  " Return
  if (i == -1) || ('check' != a:box)
    return i
  else
    let r = '' | let i = 1
    while i <= nb_choices
      let r = r . ((choice_{i}[1] == 'X') ? '1' : '0')
      let i = i + 1
    endwhile
    return r
  endif
endfunction
" }}}1
"------------------------------------------------------------------------
" Functions that insert fte statements {{{1
" Function: s:if_fte(var, then, else) {{{2
" Function: s:confirm_fte(text, [, choices [, default [, type]]]) {{{2
" Function: s:input_fte(prompt [, default]) {{{2
" Function: s:combo_fte(prompt, choice [, ...]) {{{2
" Function: s:check_fte(prompt, choice [, ...]) {{{2
" }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
plugin/words_tools.vim	[[[1
104
" File:		words_tools.vim
" Author:	Luc Hermitte <hermitte {at} free {dot} fr>
" 		<URL:http://hermitte.free.fr/vim>
" URL: http://hermitte.free.fr/vim/ressources/vim_dollar/plugin/words_tools.vim
"
" Last Update:	14th nov 2002
" Purpose:	Define functions better than expand("<cword>")
"
" Note:		They are expected to be used in insert mode (thanks to <c-r>
"               or <c-o>)
"
"===========================================================================

" Return the current keyword, uses spaces to delimitate {{{1
function! GetNearestKeyword()
  let c = col ('.')-1
  let ll = getline('.')
  let ll1 = strpart(ll,0,c)
  let ll1 = matchstr(ll1,'\k*$')
  let ll2 = strpart(ll,c,strlen(ll)-c+1)
  let ll2 = matchstr(ll2,'^\k*')
  " let ll2 = strpart(ll2,0,match(ll2,'$\|\s'))
  return ll1.ll2
endfunction

" Return the current word, uses spaces to delimitate {{{1
function! GetNearestWord()
  let c = col ('.')-1
  let l = line('.')
  let ll = getline(l)
  let ll1 = strpart(ll,0,c)
  let ll1 = matchstr(ll1,'\S*$')
  let ll2 = strpart(ll,c,strlen(ll)-c+1)
  let ll2 = strpart(ll2,0,match(ll2,'$\|\s'))
  ""echo ll1.ll2
  return ll1.ll2
endfunction

" Return the word before the cursor, uses spaces to delimitate {{{1
" Rem : <cword> is the word under or after the cursor
function! GetCurrentWord()
  let c = col ('.')-1
  let l = line('.')
  let ll = getline(l)
  let ll1 = strpart(ll,0,c)
  let ll1 = matchstr(ll1,'\S*$')
  if strlen(ll1) == 0
    return ll1
  else
    let ll2 = strpart(ll,c,strlen(ll)-c+1)
    let ll2 = strpart(ll2,0,match(ll2,'$\|\s'))
    return ll1.ll2
  endif
endfunction

" Return the keyword before the cursor, uses \k to delimitate {{{1
" Rem : <cword> is the word under or after the cursor
function! GetCurrentKeyword()
  let c = col ('.')-1
  let l = line('.')
  let ll = getline(l)
  let ll1 = strpart(ll,0,c)
  let ll1 = matchstr(ll1,'\k*$')
  if strlen(ll1) == 0
    return ll1
  else
    let ll2 = strpart(ll,c,strlen(ll)-c+1)
    let ll2 = matchstr(ll2,'^\k*')
    " let ll2 = strpart(ll2,0,match(ll2,'$\|\s'))
    return ll1.ll2
  endif
endfunction

" Extract the word before the cursor,  {{{1
" use keyword definitions, skip latter spaces (see "bla word_accepted ")
function! GetPreviousWord()
  let lig = getline(line('.'))
  let lig = strpart(lig,0,col('.')-1)
  return matchstr(lig, '\<\k*\>\s*$')
endfunction

" GetLikeCTRL_W() retrieves the characters that i_CTRL-W deletes. {{{1
" Initial need by Hari Krishna Dara <hari_vim@yahoo.com>
" Last ver:
" Pb: "if strlen(w) ==  " --> ") ==  " instead of just "==  ".
" There still exists a bug regarding the last char of a line. VIM bug ?
function! GetLikeCTRL_W()
  let lig = getline(line('.'))
  let lig = strpart(lig,0,col('.')-1)
  " treat ending spaces apart.
  let s = matchstr(lig, '\s*$')
  let lig = strpart(lig, 0, strlen(lig)-strlen(s))
  " First case : last characters belong to a "word"
  let w = matchstr(lig, '\<\k\+\>$')
  if strlen(w) == 0
    " otherwise, they belong to a "non word" (without any space)
    let w = substitute(lig, '.*\(\k\|\s\)', '', 'g')
  endif
  return w . s
endfunction

" }}}1
"========================================================================
" vim60: set fdm=marker:
tests/lh/test-Fargs2String.vim	[[[1
83
"=============================================================================
" $Id: test-Fargs2String.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		test-Fargs2String.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	«version»
" Created:	16th Apr 2007
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $
"------------------------------------------------------------------------
" Description:	Tests for lh-vim-lib . lh#command#Fargs2String
" 
"------------------------------------------------------------------------
" Installation:	
" 	Relies on the version «patched by myself|1?» of vim_units
" History:	«history»
" TODO:		«missing features»
" }}}1
"=============================================================================

function! s:TestEmpty()
  let empty = []
  let res = lh#command#Fargs2String(empty)
  call VUAssertEquals(len(empty), 0, 'Expected empty', 22)
  call VUAssertEquals(res, '', 'Expected empty result', 23)
endfunction

function! s:TestSimpleText1()
  let expected = 'text'
  let one = [ expected ]
  let res = lh#command#Fargs2String(one)
  call VUAssertEquals(len(one), 0, 'Expected empty', 27)
  call VUAssertEquals(res, expected, 'Expected a simple result', 28)
endfunction

function! s:TestSimpleTextN()
  let expected = 'text'
  let list = [ expected , 'stuff1', 'stuff2']
  let res = lh#command#Fargs2String(list)
  call VUAssertEquals(len(list), 2, 'Expected not empty', 38)
  call VUAssertEquals(res, expected, 'Expected a simple result', 39)
endfunction

function! s:TestComposedN()
  let expected = '"a several tokens string"'
  let list = [ '"a', 'several', 'tokens', 'string"', 'stuff1', 'stuff2']
  let res = lh#command#Fargs2String(list)
  call VUAssertEquals(len(list), 2, 'Expected not empty', 46)
  call VUAssertEquals(res, expected, 'Expected a composed string', 47)
  call VUAssertEquals(list, ['stuff1', 'stuff2'], 'Expected a list', 48)
  call VUAssertNotSame(list, ['stuff1', 'stuff2'], 'Expected different lists', 49)
endfunction

function! s:TestComposed1()
  let expected = '"string"'
  let list = [ '"string"', 'stuff1', 'stuff2']
  let res = lh#command#Fargs2String(list)
  call VUAssertEquals(len(list), 2, 'Expected not empty', 56)
  call VUAssertEquals(res, expected, 'Expected a string', 57)
  call VUAssertEquals(list, ['stuff1', 'stuff2'], 'Expected a list', 58)
  call VUAssertNotSame(list, ['stuff1', 'stuff2'], 'Expected different lists', 59)
endfunction

function! s:TestInvalidString()
  let expected = '"a string'
  let list = [ '"a', 'string']
  let res = lh#command#Fargs2String(list)
  call VUAssertEquals(len(list), 0, 'Expected empty', 66)
  call VUAssertEquals(res, expected, 'Expected an invalid string', 67)
endfunction

function! AllTests()
  call s:TestEmpty()
  call s:TestSimpleText1()
  call s:TestSimpleTextN()
  call s:TestComposed1()
  call s:TestComposedN()
endfunction

" call VURunnerRunTest('AllTests')
VURun % AllTests

"=============================================================================
" vim600: set fdm=marker:
tests/lh/test-askmenu.vim	[[[1
66
"=============================================================================
" $Id: test-askmenu.vim 6 2008-02-13 01:56:50Z luc.hermitte $
" File:		test-buffer-menu.vim                                      {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.0
" Created:	18th Apr 2007
" Last Update:	$Date: 2008-02-13 02:56:50 +0100 (mer., 13 fÃ©vr. 2008) $
"------------------------------------------------------------------------
" Description:	
" 	Test units for buffermenu.vim
" 
"------------------------------------------------------------------------
" Installation:	Requires:
" 	(*) Vim 7.0+
" 	(*) vim_units.vim v0.2/1.0?
" 	    Vimscript # «???»
" 	(*) lh-vim-lib (lh#ask#Menu)
" 	    <http://hermitte.free.fr/vim/ressources/lh-vim-lib.tar.gz>
"
" User Manual:
" 	Source this file.
"
" History:	
" (*) 17th Apr 2007: First version 
" TODO:		«missing features»
" }}}1
"=============================================================================



"=============================================================================
let s:cpo_save=&cpo
"------------------------------------------------------------------------
" Functions {{{1

function! TestAskMenu()
  imenu          42.40.10 &LH-Tests.&Menu.&ask.i       iask
  inoremenu      42.40.10 &LH-Tests.&Menu.&ask.inore   inoreask
  nmenu          42.40.10 &LH-Tests.&Menu.&ask.n       nask
  nnoremenu      42.40.10 &LH-Tests.&Menu.&ask.nnore   nnoreask
  nmenu <script> 42.40.10 &LH-Tests.&Menu.&ask.nscript nscriptask
  nnoremenu <script> 42.40.10 &LH-Tests.&Menu.&ask.nnnscript nnscriptask

  vmenu          42.40.10 &LH-Tests.&Menu.&ask.v     vask
  vnoremenu      42.40.10 &LH-Tests.&Menu.&ask.vnore vnoreask

  call s:CheckInMode('i', 'i')

endfunction

function! s:CheckInMode(mode, name)
  let g:menu = lh#askvim#Menu('LH-Tests.Menu.ask.'.a:name, a:mode)
  let g:name = a:name
  " VUAssert 55 Equals g:menu.name     g:name     "Name mismatch"
  " VUAssert 56 Equals g:menu.priority '42.40.10' "Priority mismatch"
  " VUAssert 57 Fail "parce qu'il le faut bien"
  echomsg "name= ".g:menu.name
  echomsg "prio= ".g:menu.priority
endfunction

" Functions }}}1
"------------------------------------------------------------------------
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:
tests/lh/test-command.vim	[[[1
69
" $Id: test-command.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" Tests for lh-vim-lib . lh#command

" FindFilter(filter):                            Helper {{{3
function! s:FindFilter(filter)
  let filter = a:filter . '.vim'
  let result =globpath(&rtp, "compiler/BTW-".filter) . "\n" .
	\ globpath(&rtp, "compiler/BTW_".filter). "\n" .
	\ globpath(&rtp, "compiler/BTW/".filter)
  let result = substitute(result, '\n\n', '\n', 'g')
  let result = substitute(result, '^\n', '', 'g')
  return result
endfunction

function! s:ComplFilter(filter)
  let files = s:FindFilter('*')
  let files = substitute(files,
	\ '\(^\|\n\).\{-}compiler[\\/]BTW[-_\\/]\(.\{-}\)\.vim\>\ze\%(\n\|$\)',
	\ '\1\2', 'g')
  return files
endfunction

function! s:Add()
endfunction

let s:v1 = 'v1'
let s:v2 = 2

function! s:Foo(i)
  return a:i*a:i
endfunction

function! s:echo(params)
  echo s:{join(a:params, '')}
endfunction

function! Echo(params)
  " echo "Echo(".string(a:params).')'
  let expr = 's:'.join(a:params, '')
  " echo expr
  exe 'echo '.expr
endfunction

let TBTWcommand = {
      \ "name"      : "TBT",
      \ "arg_type"  : "sub_commands",
      \ "arguments" :
      \     [
      \       { "name"      : "echo",
      \		"arg_type"  : "function",
      \         "arguments" : "v1,v2",
      \         "action": function("\<sid>echo") },
      \       { "name"      : "Echo",
      \		"arg_type"  : "function",
      \         "arguments" : "v1,v2",
      \         "action": function("Echo") },
      \       { "name"  : "help" },
      \       { "name"  : "add",
      \         "arguments": function("s:ComplFilter"),
      \         "action" : function("s:Add") }
      \     ]
      \ }

call lh#command#New(TBTWcommand)

nnoremap µ :call lh#command#New(TBTWcommand)<cr>

"=============================================================================
" vim600: set fdm=marker:
tests/lh/test-menu-map.vim	[[[1
54
"=============================================================================
" $Id: test-menu-map.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:		test-menu-map.vim                                           {{{1
" Author:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"		<URL:http://hermitte.free.fr/vim/>
" Version:	2.0.0
" Created:	05th Dec 2006
" Last Update:	$Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $
"------------------------------------------------------------------------
" Description:	Tests for lh-vim-lib . lh#menu#
" 
"------------------------------------------------------------------------
" Installation:	«install details»
" History:	«history»
" TODO:		«missing features»
" }}}1
"=============================================================================


" let g:want_buffermenu_or_global_disable = 1
" let b:want_buffermenu_or_global_disable = 1
" echo lh#option#Get("want_buffermenu_or_global_disable", 1, "bg")

" Call a command (':Command')
call lh#menu#Make("nic", '42.50.340',
      \ '&LH-Tests.&Menu-Make.Build Ta&gs', "<C-L>g",
      \ '<buffer>',
      \ ":echo 'TeXtags'<CR>")

" With '{' expanding to '{}××', or '{}' regarding the mode
call lh#menu#IVN_Make('42.50.360.200',
      \ '&LH-Tests.&Menu-Make.&Insert.\toto{}', ']toto',
      \ '\\toto{',
      \ '{%i\\toto<ESC>%l',
      \ "viw]toto")

" Noremap for the visual maps
call lh#menu#IVN_Make('42.50.360.200',
      \ '&LH-Tests.&Menu-Make.&Insert.\titi{}', ']titi',
      \ '\\titi{',
      \ '<ESC>`>a}<ESC>`<i\\titi{<ESC>%l',
      \ "viw]titi",
      \ 0, 1, 0)

" Noremap for the insert and visual maps
call lh#menu#IVN_Make('42.50.360.200',
      \ '&LH-Tests.&Menu-Make.&Insert.<tata></tata>', ']tata',
      \ '<tata></tata><esc>?<<CR>i', 
      \ '<ESC>`>a</tata><ESC>`<i<tata><ESC>/<\\/tata>/e1<CR>',
      \ "viw]tata", 
      \ 1, 1, 0)

"=============================================================================
" vim600: set fdm=marker:
tests/lh/test-toggle-menu.vim	[[[1
59
" $Id: test-toggle-menu.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" Tests for lh-vim-lib . lh#menu#DefToggleItem()

let Data = {
      \ "variable": "bar",
      \ "idx_crt_value": 1,
      \ "values": [ 'a', 'b', 'c', 'd' ],
      \ "menu": { "priority": '42.50.10', "name": '&LH-Tests.&TogMenu.&bar'}
      \}

call lh#menu#DefToggleItem(Data)

let Data2 = {
      \ "variable": "foo",
      \ "idx_crt_value": 3,
      \ "texts": [ 'un', 'deux', 'trois', 'quatre' ],
      \ "values": [ 1, 2, 3, 4 ],
      \ "menu": { "priority": '42.50.11', "name": '&LH-Tests.&TogMenu.&foo'}
      \}

call lh#menu#DefToggleItem(Data2)

" No default
let Data3 = {
      \ "variable": "nodef",
      \ "texts": [ 'one', 'two', 'three', 'four' ],
      \ "values": [ 1, 2, 3, 4 ],
      \ "menu": { "priority": '42.50.12', "name": '&LH-Tests.&TogMenu.&nodef'}
      \}
call lh#menu#DefToggleItem(Data3)

" No default
let g:def = 2
let Data4 = {
      \ "variable": "def",
      \ "values": [ 1, 2, 3, 4 ],
      \ "menu": { "priority": '42.50.13', "name": '&LH-Tests.&TogMenu.&def'}
      \}
call lh#menu#DefToggleItem(Data4)

function! s:Yes()
  echo "Yes"
endfunction


" What follows does not work because we can build an exportable FuncRef on top
" of a script local function
finish
function! s:No()
  echo "No"
endfunction
let Data4 = {
      \ "variable": "yesno",
      \ "values": [ 1, 2 ],
      \ "text": [ "No", "Yes" ],
      \ "actions": [ function("s:No"), function("s:Yes") ],
      \ "menu": { "priority": '42.50.20', "name": '&LH-Tests.&TogMenu.&yesno'}
      \}
call lh#menu#DefToggleItem(Data4)
tests/lh/topological-sort.vim	[[[1
65
"=============================================================================
" $Id: topological-sort.vim 101 2008-04-23 00:22:05Z luc.hermitte $
" File:                topological-sort.vim                                           {{{1
" Author:        Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
"                <URL:http://hermitte.free.fr/vim/>
" Version:        2.1.0
" Created:        17th Apr 2008
" Last Update:        $Date: 2008-04-23 02:22:05 +0200 (mer., 23 avr. 2008) $
"------------------------------------------------------------------------
" Description:        «description»
"
"------------------------------------------------------------------------
" Installation:        «install details»
" History:        «history»
" TODO:                «missing features»
" }}}1
"=============================================================================

let s:cpo_save=&cpo
set cpo&vim
"------------------------------------------------------------------------

" Fully defineds DAGs {{{1

" A Direct Acyclic Graph {{{2
let dag1 = {}
let dag1[7] = [11, 8]
let dag1[5] = [11]
let dag1[3] = [8, 10]
let dag1[11] = [2, 9, 10]
let dag1[8] = [9]

" A Direct Cyclic Graph {{{2
let dcg1 = deepcopy(dag1)
let dcg1[9] = [11]

" Test DAG1 {{{2
echo "D(dag1)=".string(lh#graph#tsort#depth(dag1, [3, 5,7]))
echo "B(dag1)=".string(lh#graph#tsort#breadth(dag1, [3, 5,7]))

" Test DCG1 {{{2
" echo "D(dcg1)=".string(lh#graph#tsort#depth(dcg1, [3, 5, 7]))
" echo "B(dcg1)=".string(lh#graph#tsort#breadth(dcg1, [3, 5, 7]))

" Lazzy Evaluated DAGs {{{1

" Emulated lazzyness {{{2
" The time-consumings evaluation function
let s:called = 0
function! Fetch(node)
  let s:called += 1
  return has_key(g:dag1, a:node) ? (g:dag1[a:node]) : []
endfunction

" Test Fetch on a DAG {{{2
echo "D(fetch)=".string(lh#graph#tsort#depth(function('Fetch'), [3,5,7]))
echo "Fetch has been evaluated ".s:called." times"



" }}}1
let &cpo=s:cpo_save
"=============================================================================
" vim600: set fdm=marker:

