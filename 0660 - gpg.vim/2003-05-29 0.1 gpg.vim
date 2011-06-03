"Vim plugin for editing gpg encrypted files
"Maintainer: Guang Yu Liu <guangyuliu@att.com>
"Last Change: 15/05/2003
"you need to have gpg set up already and with the following script
"in your PATH:
"1) dgpg: calls gpg to decrypt, just have this in the script:
"gpg -d 2>/dev/null ### this will decrypt the contents
"
"2) egpg: calls gpg to encrypt:
"gpg -e -r yourname@yourdomain
"### yourname@yourdomain is the yourself as the recipient
"### so that it will encrypt the contents using your public key
"### please check docs from gpg.
"
:augroup gpg
:  autocmd!
:  autocmd BufReadPre,FileReadPre	*.gpg set bin
:  autocmd BufReadPost,FileReadPost	*.gpg '[,']!dgpg
:  autocmd BufReadPost,FileReadPost	*.gpg set nobin
:  autocmd BufReadPost,FileReadPost	*.gpg execute ":doautocmd BufReadPost " . expand("%:r")
:  autocmd BufWritePre,FileWritePre	*.gpg '[,']!egpg
:augroup END
