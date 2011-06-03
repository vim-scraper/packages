" **** $CVSHeader: sw/vim-plugin/HTMLxC/HTMLxC.vim,v 1.2 2004/07/19 12:43:16 intrepid Exp $
" **** $Revision: 1.2 $

" $RCSfile: HTMLxC.vim,v $: make a change in the (HTML hex notation) color specified under
" the cursor by inputting decimal r g b instead of hex.

" Author: Soren Andersen <intrepid *AT* perlmonk -DOT- org>
" License: GPL v2 or later. This is Copyrighted (C)2004 software. Not in the
" Public Domain.
" (Vim) Last modified: 19 Jul 2004 08:45:30


if has('perl')

   funct! RGBtoHEXIstyle_HTML(WunderC)

   perl <<EOPERLCODE
     sub HtoD { sprintf "%3d %3d %3d"=> map{hex} $_[0]=~/([[:xdigit:]]{2})/g }
     sub DtoH { sprintf "%02x%02x%02x"=> split(/\s/,shift) }
     $WORD_u_C = do { @rv=VIM::Eval("expand(a:WunderC)");
                      $rv = shift @rv ? pop @rv : undef ; };
     $ErrC++ if ! $WORD_u_C;
     $LINE_u_C = do { @rv=VIM::Eval("getline('.')"); $rv= shift @rv ? pop @rv : undef; };
     $hexprs=$WORD_u_C; $hexprs=~ s%.*\#([[:xdigit:]]{6}).*\z%$1%;
     $idx= do { @rv=VIM::Eval("match('$LINE_u_C','$hexprs',)"); $rv= shift @rv ? pop @rv:undef };
     ($cline,$ccol)= $curwin->Cursor();
     $curwin->Cursor($cline,$idx);
     $hasGUI = do { @rv=VIM::Eval("has('gui_running')"); $rv= shift @rv ? pop @rv : 0 };
     # VIM::Msg( "the GUI is ". ($hasGUI ? "running" : "not running") );
     if  ( $hasGUI ) {
         VIM::DoCommand( "let s:reply = inputdialog('Enter space-separated ".
	      "triplet (r g b):','" . HtoD($hexprs) ."',)" );
     } else {
         VIM::DoCommand( "call inputsave()"     );
         VIM::DoCommand( "let s:reply =       input('Enter space-separated ".
	      "triplet (r g b):','" . HtoD($hexprs) ."')"  );
         VIM::DoCommand( "call inputrestore()"  );
     }
     $replv= do { @rv=VIM::Eval('s:reply'); $rv= shift @rv ? pop @rv : undef };
     $replv= join( q| |, $replv=~ /0*(\d{1,3})/g );
     $hexafied = DtoH($replv);
     if (!$replv) {
         VIM::Msg('Failed to get a valid reply value.')
     }
     else         {
        #VIM::Msg("The reply was \"$replv\" which looks like \"$hexafied\" as hex.");
         VIM::DoCommand("normal R$hexafied")
     }

EOPERLCODE
   endfu

else

   funct! RGBtoHEXIstyle_HTML(WunderC)
      exec 'echo Not able to operate on '. expand('<cWORD>') .' without Perl!\<CR>'
   endfu

endif

nnoremap <silent><leader>*     :call RGBtoHEXIstyle_HTML('<cWORD>')<cr><esc>
" eof
