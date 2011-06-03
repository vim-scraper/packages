" mrswin.vim: allows one to toggle mswin on and off
" Based On: <mswin.vim> by Bram Moolenaar
" Author:   Charles E. Campbell, Jr.
" Version:  1
"
" Purpose:
"   To set options and add mapping such that Vim behaves a lot like MS-Windows
"   To unset options and remove mappings such that Vim behaves normally
"
"   Just source the file in to toggle the behavior

" set the 'cpoptions' to its Vim default
if 1	" only do this when compiled with expression evaluation
  let s:save_cpo = &cpoptions
endif
set cpo&vim

if !exists("g:mrswin")
 " Initial loading of <mrswin.vim>
 let g:mrswin= 0

 " SaveMap: this function sets up a buffer-variable (s:restoremap)
 "          which will be used to restore user maps
 "          mapchx: either <something>  which is handled as one map item
 "                  or a string of single letters which are multiple maps
 "                  ex.  mapchx="abc" and maplead='\': \a \b and \c are saved
 fu! <SID>SaveMap(mapmode,maplead,mapchx)
   if strpart(a:mapchx,0,1) == '<'
 	" save single map <something>
 	if maparg(a:mapchx,a:mapmode) != ""
 	  let s:restoremap= a:mapmode."map ".a:mapchx." ".maparg(a:mapchx,a:mapmode)."|".s:restoremap
 	  exe a:mapmode."unmap ".a:mapchx
 	 endif
   else
 	" save multiple maps
 	let i= 1
 	while i <= strlen(a:mapchx)
 	 let amap=a:maplead.strpart(a:mapchx,i-1,1)
 	 if maparg(amap,a:mapmode) != ""
 	  let s:restoremap= a:mapmode."map ".amap." ".maparg(amap,a:mapmode)."|".s:restoremap
 	  exe a:mapmode."unmap ".amap
 	 endif
 	 let i= i + 1
 	endwhile
   endif
 endfunction
endif

if g:mrswin == 0 " Turn mswin mode on
 let g:mrswin= 1

 " Option keeping
 let s:keep_selection   = &selection
 let s:keep_selectmode  = &selectmode
 let s:keep_mousemodel  = &mousemodel
 let s:keep_keymodel    = &keymodel
 let s:keep_backspace   = &backspace
 let s:keep_whichwrap   = &whichwrap
 let s:keep_virtualedit = &virtualedit
 let s:keep_guioptions  = &guioptions

 " Save previous settings of maps
 call s:SaveMap("n","","<C-A>")
 call s:SaveMap("n","","<C-F4>")
 call s:SaveMap("n","","<C-Q>")
 call s:SaveMap("n","","<C-S>")
 call s:SaveMap("n","","<C-Tab>")
 call s:SaveMap("n","","<C-V>")
 call s:SaveMap("n","","<C-Y>")
 call s:SaveMap("n","","<C-Z>")
 call s:SaveMap("n","","<M-Space>")
 call s:SaveMap("n","","<S-Insert>")
 call s:SaveMap("c","","<C-A>")
 call s:SaveMap("c","","<C-F4>")
 call s:SaveMap("c","","<C-Tab>")
 call s:SaveMap("c","","<C-V>")
 call s:SaveMap("c","","<M-Space>")
 call s:SaveMap("c","","<S-Insert>")
 call s:SaveMap("i","","<C-A>")
 call s:SaveMap("i","","<C-F4>")
 call s:SaveMap("i","","<C-S>")
 call s:SaveMap("i","","<C-Tab>")
 call s:SaveMap("i","","<C-V>")
 call s:SaveMap("i","","<C-Y>")
 call s:SaveMap("i","","<C-Z>")
 call s:SaveMap("i","","<M-Space>")
 call s:SaveMap("i","","<S-Insert>")
 call s:SaveMap("v","","<BS>")
 call s:SaveMap("v","","<C-C>")
 call s:SaveMap("v","","<C-Insert>")
 call s:SaveMap("v","","<C-S>")
 call s:SaveMap("v","","<C-V>")
 call s:SaveMap("v","","<C-X>")
 call s:SaveMap("v","","<S-Del>")
 call s:SaveMap("v","","<S-Insert>")

 " ---------------------------------------------------------------------
 "  MSWIN begins here...

 " set 'selection', 'selectmode', 'mousemodel' and 'keymodel' for MS-Windows
 behave mswin
 
 " backspace and cursor keys wrap to previous/next line
 set backspace=2 whichwrap+=<,>,[,]
 
 " backspace in Visual mode deletes selection
 vnoremap <BS> d
 
 " CTRL-X and SHIFT-Del are Cut
 vnoremap <C-X> "+x
 vnoremap <S-Del> "+x
 
 " CTRL-C and CTRL-Insert are Copy
 vnoremap <C-C> "+y
 vnoremap <C-Insert> "+y
 
 " CTRL-V and SHIFT-Insert are Paste
 map <C-V>		"+gP
 map <S-Insert>		"+gP
 
 cmap <C-V>		<C-R>+
 cmap <S-Insert>		<C-R>+
 
 " Pasting blockwise and linewise selections is not possible in Insert and
 " Visual mode without the +virtualedit feature.  They are pasted as if they
 " were characterwise instead.
 if has("virtualedit")
   nnoremap <silent> <SID>Paste :call <SID>Paste()<CR>
   func! <SID>Paste()
     let ove = &ve
     set ve=all
     normal `^"+gPi
     let &ve = ove
   endfunc
   imap <C-V>		<Esc><SID>Pastegi
   vmap <C-V>		"-c<Esc><SID>Paste
 else
   nnoremap <silent> <SID>Paste "=@+.'xy'<CR>gPFx"_2x
   imap <C-V>		x<Esc><SID>Paste"_s
   vmap <C-V>		"-c<Esc>gix<Esc><SID>Paste"_x
 endif
 imap <S-Insert>      	<C-V>
 vmap <S-Insert>      	<C-V>
 
 " Use CTRL-Q to do what CTRL-V used to do
 noremap <C-Q>		<C-V>
 
 " Use CTRL-S for saving, also in Insert mode
 noremap <C-S>		:update<CR>
 vnoremap <C-S>		<C-C>:update<CR>
 inoremap <C-S>		<C-O>:update<CR>
 
 " For CTRL-V to work autoselect must be off.
 " On Unix we have two selections, autoselect can be used.
 if !has("unix")
   set guioptions-=a
 endif
 
 " CTRL-Z is Undo; not in cmdline though
 noremap <C-Z> u
 inoremap <C-Z> <C-O>u
 
 " CTRL-Y is Redo (although not repeat); not in cmdline though
 noremap <C-Y> <C-R>
 inoremap <C-Y> <C-O><C-R>
 
 " Alt-Space is System menu
 if has("gui")
   noremap <M-Space> :simalt ~<CR>
   inoremap <M-Space> <C-O>:simalt ~<CR>
   cnoremap <M-Space> <C-C>:simalt ~<CR>
 endif
 
 " CTRL-A is Select all
 noremap <C-A> gggH<C-O>G
 inoremap <C-A> <C-O>gg<C-O>gH<C-O>G
 cnoremap <C-A> <C-C>gggH<C-O>G
 
 " CTRL-Tab is Next window
 noremap <C-Tab> <C-W>w
 inoremap <C-Tab> <C-O><C-W>w
 cnoremap <C-Tab> <C-C><C-W>w
 
 " CTRL-F4 is Close window
 noremap <C-F4> <C-W>c
 inoremap <C-F4> <C-O><C-W>c
 cnoremap <C-F4> <C-C><C-W>c

 " MSWIN ends here
 " ---------------------------------------------------------------------

else " -----------------------------------------------------------------
 " Turn <mswin.vim> off
 let g:mrswin= 0

 " Option restoration
 let &selection   = s:keep_selection
 let &selectmode  = s:keep_selectmode
 let &mousemodel  = s:keep_mousemodel
 let &keymodel    = s:keep_keymodel
 let &backspace   = s:keep_backspace
 let &whichwrap   = s:keep_whichwrap
 let &virtualedit = s:keep_virtualedit
 let &guioptions  = s:keep_guioptions

 " Unmapping
 nunmap <C-A>
 nunmap <C-F4>
 nunmap <C-Q>
 nunmap <C-S>
 nunmap <C-Tab>
 nunmap <C-V>
 nunmap <C-Y>
 nunmap <C-Z>
 nunmap <M-Space>
 nunmap <S-Insert>
 cunmap <C-A>
 cunmap <C-F4>
 cunmap <C-Tab>
 cunmap <C-V>
 cunmap <M-Space>
 cunmap <S-Insert>
 iunmap <C-A>
 iunmap <C-F4>
 iunmap <C-S>
 iunmap <C-Tab>
 iunmap <C-V>
 iunmap <C-Y>
 iunmap <C-Z>
 iunmap <M-Space>
 iunmap <S-Insert>
 vunmap <BS>
 vunmap <C-C>
 vunmap <C-Insert>
 vunmap <C-S>
 vunmap <C-V>
 vunmap <C-X>
 vunmap <S-Del>
 vunmap <S-Insert>

 " Map restoration
 if exists("s:restoremap")
  if s:restoremap != ""
   exe s:restoremap
   unlet s:restoremap
  endif
 endif

endif

" restore 'cpoptions'
set cpo&
if 1
  let &cpoptions = s:save_cpo
  unlet s:save_cpo
endif
