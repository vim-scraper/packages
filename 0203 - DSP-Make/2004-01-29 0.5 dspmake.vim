" Title: Set Makeprg for VCC project
" Author: Michael Geddes<michaelrgeddes at optushome.com.au> 
" Version: 0.5
" Type: General Plugin
"
" History:
"   0.5: Go up directories to find a .dsp

if has("menu")
    menu 45.50 &Code.&Set\ DSP\ Make :call <SID>SetVCCMake()<cr>
endif
map <leader>dsp :call <SID>SetVCCMake()<cr> 

fun! s:SetVCCMake()

  let dsp_file = expand('*.dsp')
  if dsp_file != '' | let dsp_file=fnamemodify(dsp_file,':p') | endif

  if isdirectory(@%)
    let where=substitute(fnamemodify(@%,':p'),'[/\\]\+$','','')
  else
    let where=fnamemodify(@%,':p:h')
  endif
  if where != ''
    let dsp_file2=expand(where.'/*.dsp')
    while dsp_file2 == ''
      let par=fnamemodify(where,':h')
      if par==where | break | endif

      let dsp_file2=expand(where.'/*.dsp')
      let where=par
    endwhile
    if dsp_file != dsp_file2
      if dsp_file != '' && dsp_file2 != ''
          let dsp_file=dsp_file."\n"
      endif
      let dsp_file=dsp_file.dsp_file2
    endif
  endif
  if dsp_file == ''
    return
  endif

  if dsp_file =~ "\n"
    let index = confirm("Which Project:", dsp_file, 1)
    if index==0
      return
    endif
    let dsp_file=substitute("\n".dsp_file,'^\('."[^\r\n]*\n".'\)\{'.index."}\\([^\r\n]*\\).\\{-}$" ,'\2','')
    echo '*'.dsp_file.'*'
  endif
"  let choices = 'grep '
  let choices = system(substitute(&grepprg,'[-/][a-zA-Z]\+\s*$','','').' ^^!MESSAGE.*(based.on "'.dsp_file.'"')
  let my='!MESSAGE\s\+"\([^"]*\)"'."[^\n]*\n"
  let choices=substitute(choices,my,"\\1\n",'g')

  let index = confirm("Target:", substitute(choices,"\n*$",'',''), 1)
  if index==0
    return
  endif
  let dsp_target=substitute("\n".choices,'^\('."[^\r\n]*\n".'\)\{'.index."}\\([^\r\n]*\\).\\{-}$" ,'\2','')

  let &makeprg='msdev "'.dsp_file.'" /make "'.dsp_target.'"'
endfun

" vim:ts=2 sw=2 et
