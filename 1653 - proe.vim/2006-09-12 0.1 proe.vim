" Vim syntax file
" Language:	Pro/Engineer
" Maintainer:	Allan Tokuda (ssamaurai@yahoo.com)
" Last Change:	2006 September 12

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif


" I prefixed items with "mk_" because Pro/E files are largely used for 'Mapkeys'.
" However, some of the syntax items here apply to general Pro/E syntax.

" There is no vocabulary built in.  I was unable to download a good list of
" commands.  This documentation for Wildfire 2 is in PDF form, and translating
" to text and HTML both fail miserably for me.
" http://www.cad-resources.com/WF2_configuration_options.pdf#search=%22pro%2Fengineer%20configuration%20options%22

" I'm confused as to why the tildes ~ and the 'mapkey(continued) ' markings
" were necessary to the syntax... thoughts, anyone?


syntax match mk_command /\%(\%(\%(^\s*mapkey(continued) \)\?\~ \?\)\|^\s*\%(mapkey(continued)\|!\)\@!\)\@<=[^ ]\+ \@=/

" I give particular attention to mapkeys commands because their syntax in
" particular is horribly difficult to read-- it was my motivation for doing this.

syntax region mk_def start="mapkey " end=";\?$" contained contains=mk_keys,mk_desc

syntax match mk_keys /\%(mapkey \)\@<=.*\%( @MAPKEY_LABEL\)\@=/

syntax match mk_desc /\%(@MAPKEY_LABEL\)\@<=[^;]*;\@=/

syntax match mk_continued "^mapkey(continued) \~ "

syntax region mk_thingie start="`" end="`"

syntax match mk_special /\\\|;\|\~/

syntax match mk_comment /!.*$/


hi mk_command guifg=#ffff00
hi mk_keys guifg=#ff0000
hi mk_desc guifg=#4444ff
hi mk_continued guifg=#333333
hi mk_thingie guifg=#339933
hi mk_special guifg=#00ffff
hi mk_comment guifg=#ffffff
