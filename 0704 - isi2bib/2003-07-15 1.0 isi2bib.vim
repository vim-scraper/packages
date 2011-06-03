" Script:	isi2bib.vim
" Author:	Ajit J. Thakkar (ajit AT unb DOT ca)
" Last Change:	2003 July 15
" Version:	1.0
" URL:		http://www.unb.ca/chem/ajit/vim.htm
"
" Function:
" Convert a bibliographic database from ISI (Institute of Scientific
" Information, Web of Science) export format to BIBTeX format.
"
" Installation:
" Place somewhere in your runtimepath (typically ~/.vim or $HOME/vimfiles)
"
" Noninteractive Usage:
" From the command line, cd to the directory of the ISI database,
" and issue the command
" 	vim -u NONE -c ":ru isi2bib.vim" fname
" where fname is the name of the ISI database file.
" The BIBTeX database will be saved in base(fname).bib
" where base(fname) is the ISI file name without an extension.
"
" Description:
" BIBTeX keys are constructed as follows: (last name of first author).(last
" two digits of year).(disambiguation mark) where the disambiguation mark is a
" lower case letter that is used only when needed. For example, a key could be
" Smith03 if it is unique but may be Smith03f if it is the 7th key starting
" with Smith03.
"
" Only records of type(PT) article are converted. Other record types are
" ignored. Only the author(AU), title(TI), pages(BP,EP), year(PY), volume(VL)
" and journal(JI) fields are converted.
"
" Recent ISI records (1996 and later) use both lower- and upper-case. The case
" is respected and protected by braces when necessary.
"
" Older (pre-1996) ISI records use only upper case. A crude conversion to
" mixed case is made.
"
" Some manual cleanup of the bib file will be required; for example, to use
" math mode for symbols, etc.
"
" Limitations:
" Vim version 6.0 or later required.
"

fun! s:Clean(Name)
  let GoodName=a:Name
  " Format all-caps names
  if GoodName !~# '\l'
    let GoodName=substitute(GoodName,'\(\u\)\(\u\+\),','\1\L\2,',"g")
  endif
  " Format initials
  let GoodName=substitute(GoodName,'\(\u\)\(\u\)','\1\. \2',"g")
  let GoodName=substitute(GoodName,'\(\u\)$','\1\.',"g")
  return GoodName
endfun

set nocp viminfo= lazyredraw nohidden noswapfile updatecount=0 undolevels=0
set report=9999
if has('autocmd')
  filetype plugin indent off
endif
if exists('syntax_on')
  syntax off
endif
new
wincmd w
1

while search('^PT ',"W") > 0
  if strpart(getline('.'),3) !=? "journal"
    continue
  endif
  " Author(s)
  .+
  let author=s:Clean(strpart(getline('.'),3))
  while getline(line('.')+1) =~ '^  '
    .+
    let coworker=s:Clean(strpart(getline('.'),3))
    let author=author.' and '.coworker
  endwhile
  " Title
  call search('^TI ',"W")
  let title=strpart(getline('.'),3)
  while getline(line('.')+1) =~ '^  '
    .+
    let title=title.strpart(getline('.'),2)
  endwhile
  if title !~# '\l'
    " Format all-caps title
    let title=substitute(title,'\(\u\)\(\u\+\)','\1\L\2',"g")
  else
    " Format mixed-case title
    let title=substitute(title,'\<\u\w*\>','{&}',"g")
    let title=substitute(title,'^{\(\w\+\)}','\1',"g")
  endif
  let title=substitute(title,"center dot center dot center dot",'$\\cdots$',"g")
  " Pages
  call search('^BP ',"W")
  let pages=strpart(getline('.'),3)
  if getline(line('.')+1) =~# '^EP ' && strpart(getline(line('.')+2),3) !~ '^1$'
    let pages=pages.'--'.strpart(getline(line('.')+1),3)
  endif
  " Journal
  call search('^JI ',"W")
  let journal=strpart(getline('.'),3)
  " Year
  call search('^PY ',"W")
  let year=strpart(getline('.'),3)
  " Volume
  call search('^VL ',"W")
  let volume=strpart(getline('.'),3)
  " Write entry in bib file
  wincmd w
  " Create key
  let key=substitute(author,',.*$',"","")
  let key=key.strpart(year,2)
  " Disambiguation mark
  let repeat=0
  if search(key,"w") > 0
    exe 'sil! g/'.key.'/let repeat=repeat+1'
  endif
  if repeat > 0
    let key=key.nr2char(repeat+96)
  endif
  sil! $put=''
  sil! $put='@article{'.key.','
  sil! $put='author={'.author.'},'
  sil! $put='title={'.title.'},'
  sil! $put='journal={'.journal.'},'
  sil! $put='year={'.year.'},'
  sil! $put='volume={'.volume.'},'
  sil! $put='pages={'.pages.'},'
  sil! $put='}'
  wincmd w
endwhile

wincmd w
sil! w! #:r.bib
qa
