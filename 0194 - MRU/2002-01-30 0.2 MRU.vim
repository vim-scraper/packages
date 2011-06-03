" Author: Gergely Kontra <kgergely@mcl.hu>
" Version: 0.2
" Description:
" Description:
"   Most recently used files appear in the file menu
" Installation:
"   Drop it into your plugin directory
"   $MRU variable should contain the full filename, where the MRU files should
"   be written
"   MRU_num can contain the number of files to store (default is 4)
"
" History:
"    0.1: * Initial release (not published)
"    0.2: * You can access the files through your keyboard (1-9), when you are
"           in the file menu
"         * Bugfixes
"         * When you click on an item, the function named OPEN_FUNC will be
"           called, or it will be opened in a window (in the current window,
"           when the file is not modified, or in a new window othervise)
"
" TODO:
"    Are all valid filenames escaped?
"

if !exists('SpWhenModified') "integration with FavMenu
  fu! SpWhenModified(f)
    if &mod
      exe 'sp '.a:f
    else
      exe 'e '.a:f
    endif
  endf
  fu! SpWhenNamedOrModified(f)
    if bufname('')!='' || &mod
      exe 'sp '.a:f
    else
      exe 'e '.a:f
    end
  endf
  fu! OpenFile()
    if exists('g:OPEN_FUNC')
      retu g:OPEN_FUNC
    el
      retu 'SpWhenModified'
    en
  endf
end

fu! <SID>AddThisFile(f)
  if a:f!='//' && !buflisted(a:f)  " if param not good...
    retu
  end
  sp $MRU|set nobl|1
  " First cleanup old MRU's
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  " WARNING: Keep next 2 lines in sync with the :g below
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  g/^.\+$/silent! exe 'aun &File.'.
    \'[&'.line('.').']\ '.
    \escape(fnamemodify(getline('.'),':p:t'),' \.')
  " Figure out fullname
  let fullname=fnamemodify(a:f,':p')
  if a:f!='//' " add this file to the top (if real file)
    if search('^\V'.escape(fullname,'\').'\$','w')
      move 0
    else
      exe 'norm ggO'.fullname."\<Esc>"
    endif
  endif
  let num=1+(exists('g:MRU_num') ? g:MRU_num : 4)
  silent! exe num.',$d _'
  " Build up new files
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  " WARNING: Keep next command in synx with the :g above
  " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  g/^.\+$/exe 'amenu 10.'.(line('.')+511).' &File.'.
    \'[&'.line('.').']\ '.
    \escape(fnamemodify(getline('.'),':p:t'),' \.').
    \'<Tab>'.
    \escape(fnamemodify(getline('.'),':p:h'),' \.').
    \' :call <C-R>=OpenFile()<CR>("'.
    \escape(getline('.'),'\').'")<CR>'
  let pm=&pm|let &pm=''|wq|let &pm=pm
endf

if !exists('$MRU')
  if has('unix')
    let $MRU=$HOME.'/.vimrecent'
  else
    let $MRU=$VIM.'\_vimrecent'
  end
end

amenu 10.511 &File.-SepMRU- <Nop>
silent call <SID>AddThisFile('//')
aug MRU
au BufWritePre * silent call <SID>AddThisFile(@%)
aug END
