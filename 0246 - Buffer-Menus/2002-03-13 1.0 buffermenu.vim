" Buffer Menus - Add menus to the current buffer only.
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 1.0

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
com! -nargs=+ -bang -complete=menu Bamenu call <SID>DoBufferMenu(<q-bang>, ,'a', <f-args> )
com! -nargs=+ -bang -complete=menu Bcmenu call <SID>DoBufferMenu(<q-bang>, ,'c', <f-args> )
com! -nargs=+ -bang -complete=menu Bnmenu call <SID>DoBufferMenu(<q-bang>, ,'n', <f-args> )
com! -nargs=0 Bunmenuall call <SID>ClearBufferMenus()

" Clear out all the menus
fun! s:ClearBufferMenus()
	call s:RestoreOptions()
	let b:bufferMenuStack=""
	let b:unmenuStack=""
endfun

" Works out what arguments were passed to the command.
"  Use ! for 'noremenu'
fun! s:DoBufferMenu( bang,...)
	let n=1
	let modes='nvo'
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
		call s:BufferMenu( a:bang=='!', modes, menunumber.a:{n}, a:{n+1} )
	else
		call confirm('Invalid arguments')
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

	" Get the modes - if nothing, use the default.
	let ma=0
	let modes=a:modes
	if modes==''
		let modes=' ' " Execute a 'menu' command without prefix
	endif

	" Execute 
	while ma < strlen(modes)
	  let b:bufferUnmenuStack=modes[ma].'unmenu '.substitute(a:menuname,'^\s*[0-9][0-9.]*','','').sep.b:bufferUnmenuStack
	  let cmd=(modes[ma]).noRe. 'menu '.a:menuname.' '.a:mapping
	  exe cmd
	  let b:bufferMenuStack=b:bufferMenuStack.cmd.sep
	  let ma=ma+1
	endwhile
endfun

" Execute '\n' separated items in such a way that errors can be reported with
" the command executed.
fun! s:CallStack(stack)
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
		echohl ErrorMsg
		echo 'In Line: '.cmd
		echohl None
	  else
		let v:errmsg=erm
	  endif
	  let here=back
	endwhile
endfun
" Restore the menus for the buffer
fun! s:RestoreMenus()
  if exists("b:bufferUnmenuStack")
  	call s:CallStack(b:bufferUnmenuStack)
  endif
endfun
" Remove the menus for the buffer
fun! s:BufferMenus()
	if exists("b:bufferMenuStack")
		call s:CallStack(b:bufferMenuStack)
	endif
endfun

aug MRGBufferMenuEnter
au!
au BufEnter * call <SID>BufferMenus() 
au BufLeave,BufUnload * call <SID>RestoreMenus() 
aug END

