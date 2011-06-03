" Buffer Menus - Add menus to the current buffer only.
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 1.9
"
" Contributions by Luc Hermitte

" Usage -
" :Bmenu[!] ["<silent>"] ["<unmenu>"|"<disable>"] [<modes>] [<priority>] <Menuname> <Mapping>
"  		Add menus to different modes for the current buffer.  (Bang is used to
"	     specify 'noremenu')
" :B[ivacn]menu[!]["<silent>"] ["<unmenu>"|"<disable>"]  [<priority>] <Menuname> <Mapping>
" 	    Add menu for one mode (or 'a' for all modes) for the current buffer.
"       (Bang is used to specify 'noremenu')
"
":B[ivacn]noremenu ["<silent>"] ["<unmenu>"|"<disable>"] [<modes>] [<priority>] <Menuname> <Mapping>
"       Adds a 'norecursive' menu.
"
" The above commands accept '<silent>' as a flag to do a silent mapping.
" They also allow <SID> (providing you have done the "exe FindBufferSID()"
" at the beginning of the script), and will expand <leader> and <localleader>
" in menu names.
"
" :Bunmenuall
"       Remove all menus for the current buffer
"
" BufferOneShot( <ident> )
"  		Use this to make sure Bmenus only get added ONCE per buffer.
"  		eg:
"  		if BufferOneShot('MyProgram')
"  			Bmenu 10.20 Test.Test iHello<esc>
"  		endif
"  		A buffer local-variable called b:buffer_oneshot_MyProgram will be
"  		created (just in case you want to unlet it when testing).
"
" exe FindBufferSID() 
" 		:exe the result of this function when using <SID> in menu commands so that the correct
" 		function gets called.
"


" TODO: 
"  * Add a 'Bunmenu' for one menu.
"  * If the same menu is added twice to the same mode, update the first entry rather than just
"    adding it twice (there _will_ be an error on leaving the buffer if this
"    happens, as there will be two unmenus for the same mode).

" Examples -
"  Bmenu ni 10.30 Test.It Test
"  Bimenu 10.40 Test.More Hello<esc>
"  Bamenu Test.Ignore :set ic<cr>

com! -nargs=+ -bang -complete=menu Bmenu call <SID>DoBufferMenu(<q-bang>, <f-args> )
com! -nargs=+ -bang -complete=menu Bimenu call <SID>DoBufferMenu(<q-bang>, 'i', <f-args> )
com! -nargs=+ -bang -complete=menu Bvmenu call <SID>DoBufferMenu(<q-bang>, 'v', <f-args> )
com! -nargs=+ -bang -complete=menu Bamenu call <SID>DoBufferMenu(<q-bang>, 'a', <f-args> )
com! -nargs=+ -bang -complete=menu Bcmenu call <SID>DoBufferMenu(<q-bang>, 'c', <f-args> )
com! -nargs=+ -bang -complete=menu Bnmenu call <SID>DoBufferMenu(<q-bang>, 'n', <f-args> )
com! -nargs=0 Bunmenuall call <SID>ClearBufferMenus()

com! -nargs=+ -complete=menu Bnoremenu
      \ call <SID>DoBufferMenu('!', <f-args> )
com! -nargs=+ -complete=menu Binoremenu
      \ call <SID>DoBufferMenu('!', 'i', <f-args> )
com! -nargs=+ -complete=menu Bvnoremenu
      \ call <SID>DoBufferMenu('!', 'v', <f-args> )
com! -nargs=+ -complete=menu Banoremenu
      \ call <SID>DoBufferMenu('!', 'a', <f-args> )
com! -nargs=+ -complete=menu Bcnoremenu
      \ call <SID>DoBufferMenu('!', 'c', <f-args> )
com! -nargs=+ -complete=menu Bnnoremenu
      \ call <SID>DoBufferMenu('!', 'n', <f-args> )


" s:ClearBufferMenus
" Clear out all the menus
fun! s:ClearBufferMenus()
	call s:RestoreMenus()
	let b:bufferMenuStack=""
	let b:bufferUnmenuStack=""
endfun

" Works out what arguments were passed to the command.
"  Use ! for 'noremenu'
fun! s:DoBufferMenu( bang, ...)
	let n=1
	let modes=''
	if a:{n} =~ '^[anvoic]*$' 
		let modes=a:{n}
		let n=n+1
	endif
	let silent = ''
	let useenable= exists('g:buffermenu_use_disable') && g:buffermenu_use_disable
	while 1
		if a:{n} ==? '<silent>'
			let silent = '<silent>'
			let n = n + 1
		elseif a:{n} ==? '<unmenu>'
			let useenable=0
		elseif a:{n} ==? '<disable>'
			let useenable=1
		else
			break
		endif
	endwhile
	let menunumber=''
	if a:{n} =~ '^\s*[0-9]\+\>\(\.[0-9]\+\)*'
		let menunumber=a:{n}.' '
		let n=n+1
	endif
	if a:0 >= (n+1)
		let menuname=escape(a:{n},' ')
		let menucmd=a:{n+1}
		let n=n+2
		while n <= a:0
			let menucmd=menucmd.' '.a:{n}
			let n=n+1
		endwhile
		call s:BufferMenu( a:bang=='!', useenable, modes, silent.menunumber.menuname, menucmd )
	else
		let cmd='('.modes.')menu'.a:bang.' '.menunumber.' ^^ '
		let x=n
		while x<= a:0
			let cmd=cmd.' {'.escape(a:{x},' ').'}'
			let x=x+1
		endwhile
		echoerr 'Invalid arguments: '.cmd
	endif
endfun

" s:BufferMenu - Do the work of adding the menu to the buffer.
" dontremap - use noremenu
" useenable - use menu disable instead of unmenu
" modes - list of modes 
" menuname - name of menu (including priority)
" mapping - command to map menu to
fun! s:BufferMenu( dontremap, useenable, modes, menuname, mapping )
	let sep="\n"
	if !exists("b:bufferUnmenuStack") 
	  let b:bufferUnmenuStack=sep
	endif
	if !exists("b:bufferMenuStack")
	  let b:bufferMenuStack=sep
	endif

	" We can't silently get the old menus, which is a problem at the moment.

    let noRe = a:dontremap ? 'nore' : ''
	let mll=escape(exists('maplocalleader')?maplocalleader : "\\","\\|")
	let ml=escape(exists('mapleader')?mapleader : "\\","\\|")

	let menuname=substitute(a:menuname, '\c<localleader>',escape(mll,"\\") , 'g')
	let menuname=substitute(menuname, '\c<leader>',escape(ml,"\\") , 'g') 
	if exists('b:bmenu_sid_t')
		let mapping=substitute(a:mapping, '\c<sid>',b:bmenu_sid_t, 'g')
	else
		if match(a:mapping, '\c<sid>') >= 0
			echoerr 'You must have "exe FindBufferSID()" before adding buffer menus with <SID>!'
			return
		endif
		let mapping=a:mapping
	endif


	" Get the modes - if nothing, use the default.
	let ma=0
	let modes=a:modes
	if modes==''
		let modes=' ' " Execute a 'menu' command without prefix
	endif

	" Execute 
	while ma < strlen(modes)
		let cmd=(modes[ma]).noRe. 'menu '.menuname.' '.mapping
		let erm=v:errmsg
		let v:errmsg=""
		exe cmd
		if v:errmsg!="" 
			echoerr 'In command: '.cmd
			break
		else
			let stripname=substitute(menuname,'^\s*[0-9][0-9.]*','','')
			if a:useenable 
				let cmd=(modes[ma]).'menu enable '.stripname
				let undocmd=(modes[ma]).'menu disable '.stripname
			else
				let undocmd=(modes[ma]).'unmenu '.stripname
			endif
			let v:errmsg=erm
			let b:bufferUnmenuStack=undocmd.sep.b:bufferUnmenuStack
			let b:bufferMenuStack=b:bufferMenuStack.cmd.sep
		endif
		let ma=ma+1
	endwhile
endfun

" Execute '\n' separated items in such a way that errors can be reported with
" the command executed.
fun! s:CallStack(stack,stackname)
	let mx="^\n\\=[^\n]*"
	let here=0
	let last=strlen(a:stack)
	while here < last 
	  let back=matchend(a:stack,mx,here)
	  if back<0 | return | endif
	  let cmd=strpart(a:stack,here, back-here)
	  let erm=v:errmsg
	  let v:errmsg=""
	  exe cmd
	  if v:errmsg!="" 
		echoerr 'Error processing stack "'.a:stackname.'" command: '.cmd
	  else
		let v:errmsg=erm
	  endif
	  let here=back
	endwhile
endfun

" Restore the menus for the buffer
fun! s:RestoreMenus()
	if exists("b:bufferUnmenuStack") && exists('b:buffer_did_menu_stack')
		unlet b:buffer_did_menu_stack
		call s:CallStack(b:bufferUnmenuStack, 'Buffer Unmenu')
	endif
endfun
" Remove the menus for the buffer
fun! s:BufferMenus()
	if exists("b:bufferMenuStack")
		let b:buffer_did_menu_stack=1
		call s:CallStack(b:bufferMenuStack, 'Buffer Menu')
	endif
endfun

fun! BufferOneShot(desc)
	if exists('b:buffer_oneshot_'.a:desc)
		return 0
	endif
 	let b:buffer_oneshot_{a:desc}=1
	return 1
endfun

aug MRGBufferMenuEnter
	au!
	au BufEnter * call <SID>BufferMenus() 
	au BufLeave,BufUnload * call <SID>RestoreMenus() 
aug END

" Execute the result of this to make sure that <SID> works when creating menus
	" Example:
	" exe FindBufferSID() 
fun! FindBufferSID()
	return 'imap zzzzzzz <SID>|let t_z=@z|redir @z|silent imap zzzzzzz |let b:bmenu_sid_t=matchstr(@z,"<SNR>\\d\\+_$")|let @z=t_z|unlet t_z|iunmap zzzzzzz'
endfun

" vim600: set ts=4 sw=4 noet fdm=indent:
