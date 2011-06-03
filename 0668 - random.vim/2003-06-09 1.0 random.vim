"----------------------------------------------------------"
" Vim meta color file                                      "
" Maintainer         : W. H. Jou                           "
" Email              : whjou@singnet.com.sg                "
" Most Recent Update : 2003-06-09                          "
" Version            : 1.0                                 "
"----------------------------------------------------------"
" Define function once only
if !exists("*s:LoadRandomColorScheme")
  function s:LoadRandomColorScheme()
    let s:color_file_list = substitute(globpath(&runtimepath, 'colors/*.vim'), "\n", ',', 'g')
    if strlen(s:color_file_list)
      if s:color_file_list =~ ','
        let s:rnd  = matchstr(localtime(), '..$') + 0
        let s:loop = 0

        while s:loop < s:rnd
          let s:color_file_list = substitute(s:color_file_list, '^\([^,]\+\),\(.*\)$', '\2,\1', '')
          let s:loop            = s:loop + 1
        endwhile
        
        let s:color_file = matchstr(s:color_file_list, '^[^,]\+')
        execute "source" s:color_file
        unlet! s:color_file
        
        unlet! s:loop 
        unlet! s:rnd 
      endif
    endif

    unlet! s:color_file_list 
  endfunction
endif
"----------------------------------------------------------"
call s:LoadRandomColorScheme()
