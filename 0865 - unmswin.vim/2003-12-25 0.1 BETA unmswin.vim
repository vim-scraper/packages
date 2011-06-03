" Undo the effect of the mswin.vim script
" Author: Tony Mechelynck <antoine.mechelynck@belgacom.net>
" Last Change: 2003 Dec 25
" 
" Undo :behave mswin. Since we don't know the previous values of the options
" modified by :behave and by mswin.vim, we set them back to their defaults.
" 
set selectmode& mousemodel& keymodel& selection& backspace& whichwrap&

" The rest consists of undoing mswin.vim's mappings
vunmap <BS>
vunmap <C-X>
vunmap <S-Del>
vunmap <C-C>
vunmap <C-Insert>
unmap <C-V>
unmap <S-Insert>
cunmap <C-V>
cunmap <S-Insert>
iunmap <C-V>
vunmap <C-V>
iunmap <S-Insert>
vunmap <S-Insert>
" we don't undo the mapping of Ctrl-Q because Ctrl-Q has no other meaning
unmap <C-S>
vunmap <C-S>
iunmap <C-S>
" we don't change guioptions, setting it back to default would cause too much
" trouble
unmap <C-Z>
iunmap <C-Z>
unmap <C-Y>
iunmap <C-Y>
" don't unmap Alt-Space
unmap <C-A>
iunmap <C-A>
cunmap <C-A>
unmap <C-Tab>
iunmap <C-Tab>
cunmap <C-Tab>
unmap <C-F4>
iunmap <C-F4>
cunmap <C-F4>
