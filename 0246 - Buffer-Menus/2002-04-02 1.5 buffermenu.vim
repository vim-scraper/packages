" Buffer Menus - Add menus to the current buffer only.
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 1.5

" Usage -
" Bmenu[!] [<modes>] [<priority>] <Menuname> <Mapping>
"  		Add menus to different modes for the current buffer.  (Bang is used to
"	     specify 'noremenu')
" B[ivacn]menu[!] [<priority>] <Menuname> <Mapping>
" 	    Add menu for one mode (or 'a' for all modes) for the current buffer.
"       (Bang is used to specify 'noremenu')
" Bunmenuall
"       Remove all menus for the current buffer


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
	let menunumber=''
	if a:{n} =~ '^\s*[0-9]\+\>\(\.[0-9]\+\)*'
		let menunumber=a:{n}.' '
		let n=n+1
	endif
	if a:0 == (n+1)
		call s:BufferMenu( a:bang=='!', modes, menunumber.escape(a:{n},' '), a:{n+1} )
	else
		let cmd='('.modes.')menu'.a:bang.' '.menunumber.' ^^ '
		let x=n
		while x<= a:0
			let cmd=cmd.' {'.escape(a:{x},' ').'}'
			let x=x+1
		endwhile
		"echoerr (a:0) . '!='. (n+1)
		echoerr 'Invalid arguments: '.cmd
	endif
endfun

" Do the work of addimg the menu to the buffer.
fun! s:BufferMenu( dontremap, modes, menuname, mapping )
	let sep="\n"
	if !exists("b:bufferUnmenuStack") 
	  let b:bufferUnmenuStack=sep
	endif
	if !exists("b:bufferMenuStack")
	  let b:bufferMenuStack=sep
	endif

	" We can't silently get the old menus, which is a problem at the moment.

	if a:dontremap
	  let noRe='nore'
	else
	  let noRe=''
	endif
	let mll=escape(exists('maplocalleader')?maplocalleader : "\\","\\|")
	let ml=escape(exists('mapleader')?mapleader : "\\","\\|")

	let menuname=substitute(a:menuname, '\c<localleader>',escape(mll,"\\") , 'g')
	let menuname=substitute(menuname, '\c<leader>',escape(ml,"\\") , 'g') 

	" Get the modes - if nothing, use the default.
	let ma=0
	let modes=a:modes
	if modes==''
		let modes=' ' " Execute a 'menu' command without prefix
	endif

	" Execute 
	while ma < strlen(modes)
	  let cmd=(modes[ma]).noRe. 'menu '.menuname.' '.a:mapping
	  let erm=v:errmsg
	  let v:errmsg=""
	  exe cmd
	  if v:errmsg!="" 
		echoerr 'In command: '.cmd
		break
	  else
		let v:errmsg=erm
		let b:bufferUnmenuStack=modes[ma].'unmenu '.substitute(menuname,'^\s*[0-9][0-9.]*','','').sep.b:bufferUnmenuStack
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

