" comment.vim, version 1.0
" jerome.plut at normalesup dot org

function! CommentStyle(s)
  if match (a:s, '@') >= 0
  let str1 = substitute (a:s, '@.*$', '', '')
    let str2 = substitute (a:s, '^.*@', '', '')
  else
    let str1 = a:s. ' '
    let str2 = ''
  endif
  let pat1 = substitute (str1, '[][*^.$~]', '\\&', 'g')
  let pat1 = substitute (pat1, '\s*$', '\\s*', '')
  let str1 = substitute (str1, '&', '\\&', 'g')
  if str2 == ''
    " s:l1 contains the computed patterns to comment, s:l2 those to
    " uncomment
    let s:l1 = [ 'sm@^@'.str1.'@e' ]
    let s:l2 = [ 'sm@^\s*'.pat1.'@@e' ]
  else
    let pat2 = substitute (str2, '[][*^.$~]', '\\&', 'g')
    let pat2 = substitute (pat2, '^\s*', '\\s*', '')
    let str2 = substitute (str2, '&', '\\&', 'g')
    " protect any comment that becomes nested
    " with non-ASCII chars, to avoid collisions
    let s:l1 = ['sm@«¤@«¤¤@ge', 'sm@'.pat1.'@«¤«@ge', 'sm@^@'.str1.'@e']
    let s:l1+= ['sm@¤»@¤¤»@ge', 'sm@'.pat2.'@»¤»@ge', 'sm@$@'.str2.'@e']
    let s:l2 = ['sm@^\s*'.pat1.'@@e', 'sm@«¤«@'.str1.'@ge', 'sm@«¤¤@«¤@ge']
    let s:l2+= ['sm@'.pat2.'\s*$@@e', 'sm@»¤»@'.str2.'@ge', 'sm@¤¤»@¤»@ge']
  endif
endfunction

function! Comment() range
  for s in s:l1
    execute ':sil '.a:firstline.','.a:lastline.s
  endfor
endfunction

function! UnComment() range
  let pre = ':sil '.a:firstline.','.a:lastline
  for s in s:l2
    execute ':sil '.a:firstline.','.a:lastline.s
  endfor
endfunction

command! -nargs=1 CommentStyle call CommentStyle (<f-args>)

map <silent> >c :call Comment()<CR>
map <silent> <c :call UnComment()<CR>
map =c :CommentStyle<Space>

au FileType * CommentStyle #
au FileType vim CommentStyle "
au FileType c,css,sql CommentStyle /* @ */
au FileType tex,mf,mytex,lilypond,postscr CommentStyle %
au FileType cpp,xkb CommentStyle \\\\
au FileType groff,nroff CommentStyle .\\"
au FileType config CommentStyle dnl
au FileType lua CommentStyle --
au FileType xdefaults CommentStyle !
au FileType html CommentStyle <!-- @ -->
au FileType python CommentStyle """ @ """
" This makes quotes in emails
au FileType mail CommentStyle >
