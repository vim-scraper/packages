" totoloco at gmail dot com
" Licensed under sisterware
" http://ushcompu.com.ar
"needs curl in PATH
"install on ~/.vim/plugin/ dir
" based on:
"   paster.vim
"     http://www.vim.org/scripts/script.php?script_id=2602
"   pasto.sh
"     http://pastopasto.com.ar/pastos/view/48

if exists("g:loaded_Pasto") || !executable("curl") || &cp
  finish
endif

let g:loaded_Pasto = '0.5'
let s:pastoBase = 'http://pastopasto.com.ar'

let s:pastoSyntax  = {   'awk':         'awk',
                       \ 'sh':          'bash',
                       \ 'c':           'c',
                       \ 'cpp':         'cpp',
                       \ 'html':        'html4strict',
                       \ 'java':        'java',
                       \ 'javascript':  'javascript',
                       \ 'perl':        'perl',
                       \ 'php':         'php',
                       \ 'python':      'python',
                       \ 'ruby':        'ruby',
                       \ 'vim':         'vim',
                       \
                       \ 'default':     'text' }

function! s:ResolveTextFormat()
  return has_key(s:pastoSyntax, &filetype)
           \ ? s:pastoSyntax[&filetype]
           \ : s:pastoSyntax['default']
endfunction

function! s:PastoPut(user, title, text)
  let params  = 'data[_cli]=1&data[Pasto][nick]='.a:user
  let params .= '&data[Pasto][title]='.a:title
  let params .= '&data[Pasto][type]='.s:ResolveTextFormat()
  let url = s:pastoBase."/pastos/add"
  let command = 'curl -s --data-urlencode "data[Pasto][text]@-" -d "'.params.'" '.url
  echo 'Sending...'
  let output = split(system(command, a:text), '\n')

  echom 'url: '.s:pastoBase.'/pastos/view/'.output[0]
endfunction

function! Pasto() range
  let user = inputdialog('Enter nick / gravatar email: ', $USER)
  let title = inputdialog('Title: ', expand('%'))
  let text = join(getline(a:firstline, a:lastline), "\n")
  echo user
  call s:PastoPut(user, title, text)
endfunction

function! PastoGet()
  let PastoId = inputdialog('Enter PastoId: ')
  let url = s:pastoBase.'/pastos/get/'.PastoId
  let command = 'curl -s '.url
  echo 'Loading...'
  execute '.!'.command
  echo 'Loaded!'
endfunction

com! PastoGet :call PastoGet()
com! -range=% -nargs=0 Pasto :<line1>,<line2>call Pasto()
