" Title: Set Makeprg for VCC project
" Author: Michael Geddes<michaelrgeddes at optushome.com.au> 
" Version: 0.1
" Type: General Plugin

if has("menu")
	menu 45.50 &Code.&Set\ DSP\ Make :call <SID>SetVCCMake()<cr>
endif
map <leader>dsp :call <SID>SetVCCMake()<cr> 

fun! s:SetVCCMake()

  let dsp_file = expand('*.dsp')

  if isdirectory(@%)
  	let where=fnamemodify(@%,':p:.')
  else
	let where=fnamemodify(@%,':p:.:h').'/'
  endif
  if where != ''
  	let dsp_file2=expand(where.'*.dsp')
	if dsp_file != '' && dsp_file2 != ''
		let dsp_file=dsp_file."\n"
	endif
	let dsp_file=dsp_file.dsp_file2
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
  let choices = system('grep ^^!MESSAGE.*(based.on '.dsp_file.'')
  let my='!MESSAGE\s\+"\([^"]*\)"'."[^\n]*\n"
  let choices=substitute(choices,my,"\\1\n",'g')
	
  let index = confirm("Target:", substitute(choices,"\n*$",'',''), 1)
  if index==0
	return
  endif
  let dsp_target=substitute("\n".choices,'^\('."[^\r\n]*\n".'\)\{'.index."}\\([^\r\n]*\\).\\{-}$" ,'\2','')

  let &makeprg='msdev '.dsp_file.' /make "'.dsp_target.'"'
endfun


