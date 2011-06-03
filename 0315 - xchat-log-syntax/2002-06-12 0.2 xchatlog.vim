" Vim syntax file
" Language:	xchat log 
" Maintainer:	Bret Towe <btowe@gbis.com>
" Last Change:	2002-06-12
" Version:	0.2
"
" Notes:
" this is far from complete
" it lacks highlighting for topic also 
" highlighting for colored nicks (not sure if can be done anyways)
" and its prob needing a clean up so its easier to read


" highlight:  -->	NickName (~ident@host) has joined #channel
" this sets bold for the nick highlights () to darkgrey and the host to cyan
:syntax region Hosts matchgroup=paren start=/(/ end=/)/ contained
:syntax region Join start="-->"hs=s+4 end="has joined"he=e-11 oneline contains=Hosts


" highlight: <--	NickName has quit (quitmsg)
" also: <-- NickName (ident@host) has left #zsnes (partmsg)
" this only really highlights the () as thats all that is done in xchat
:syntax region Left matchgroup=paren start=/(/ end=/)/ oneline contained
:syntax region Quit matchgroup=normal start="[<-]--"hs=s+4 end="$" oneline contains=Left


" TODO
" highlight: (following 2 lines)
" ---	Topic for #channel is not for your eyes
" ---	Topic for #channel set by NickName at somedatethatnoonecaresabout
":syntax 


" highlight: <NickName>	hi
" set the <> to blue
:syntax region Nicks matchgroup=nick start="<" end=">\t"he=e-1 oneline

" highlight: >NickName<	hi
" set the >< to green
:syntax region Msg matchgroup=privmsg start=">" end="<\t"he=e-1 oneline

" highlight: -NickName- VERSION xchat 1.8.7 Linux 2.4.17 [i686/848MHz]
" set the -- to blue and the nick to magenta (this is for all ctcp that gives these replys)
:syntax region Ctcp matchgroup=nick start="-\(-\)\@!" end="-\t"he=e-1 oneline

" highlight: *	NickName jumps off a cliff
" set actions (* nick) to magneta
:syntax region Action start="\*\t" end="$" oneline


" colors for the syntax
:hi paren ctermfg=DarkGray guifg=#737573
:hi Hosts ctermfg=Cyan guifg=#00cdcd
:hi nick ctermfg=Blue guifg=#0000ff
:hi privmsg ctermfg=Green guifg=#00ce00
:hi Action ctermfg=Magenta guifg=#9c20ee
:hi Ctcp ctermfg=Magenta guifg=#9c20ee
:hi topic ctermfg=Cyan guifg=#31deee
:hi Join cterm=bold gui=bold
