" projtags.vim
" Brief: Set tags file for per project
" Version: 0.1
" Date: Apr.24, 2007
" Author: Yuxuan 'fishy' Wang <fishywang@gmail.com>
"
" Installation: put this file under your ~/.vim/plugins/
"
" Usage:
"
" Set your projects path into g:ProjTags as a list, for example:
"
" let g:ProjTags = [ "~/work/proj1" ]
" 
" In this case for all file match the pattern "~/work/proj1/*", the tag file
" "~/work/proj1/tags" will be used.
"
" You can also specify a tags file other than the default one by a 2 item list:
"
" let g:ProjTags += [[ "~/work/proj2", "~/work/proj2.tags" ]]
" 
" In this case for all file match the pattern "~/work/proj2/*", the tag file
" "~/work/proj2.tags" will be used.
"
" You can add the above codes into your vimrc file

function! SetProjTags()
	for item in g:ProjTags
		try
			let [filepattern, tagfile] = item
			let filepattern .= "/*"
		catch /.*List.*/ " item is not a list
			let filepattern = item . "/*"
			let tagfile = item . "/tags"
		endtry
		execute 'autocmd BufEnter ' . filepattern . ' :setlocal tags+=' . tagfile 
		unlet item
	endfor
endfunc

call SetProjTags()

