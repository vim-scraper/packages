"Список файлов, представленных в стандартной библиотеке. Там есть некая
"зависимость, которая указывает следующую принадлежность файла к классу.
"путь.имя класса(разделители в пути заменяем точками, а на конце расширения
"файлаupeйла удаляем .hx.
set completeopt=menu,longest,preview
function! CreateArrayFromPath()
    set ignorecase
    if (exists('$HAXE_HOME'))
        let g:haxepath=$HAXE_HOME
    elseif (exists('$HAXEPATH'))
        let g:haxepath=$HAXEPATH
    endif
    "let g:haxepath=substitute(g:haxepath,'^.\{-}:\?[\/\\]','/','g')
    if $OS=~'WINDOW'
        let g:haxepath=substitute(g:haxepath,'\/','\','g')
    elseif
        let g:haxepath=substitute(g:haxepath,'\\','/','g')
    endif
    set noignorecase
    let s:package=substitute(expand(g:haxepath.'std/**/*.hx'),escape(g:haxepath,'\/').'std[\/\\]','','g')
    let s:addinglib=substitute(expand(g:haxepath.'lib/**/*.hx'),escape(g:haxepath,'\/').'lib[\/\\]\([^\/\\]\+\)[\/\\][^\/\\]\+[\/\\]\1\','\1\\','g')
    let s:addinglib=substitute(s:addinglib,'\w:.\{-}hx\n',"","g")
    let s:package.=s:addinglib
    let s:package_list=split(s:package,'\.hx\>\n\?')
    let s:package_names={}
    for s:package_line in s:package_list
        let s:package_file=s:package_line.'.hx'
        let s:package_line=substitute(s:package_line,'[\\\/]','.','g')
        if s:package_line=~ '^\w\+\.'
            if (has_key(s:package_names,matchstr(s:package_line,'^\w\+\.\@='))) 
               let  s:package_names[matchstr(s:package_line,'^\w\+\.\@=')]=add(s:package_names[matchstr(s:package_line,'^\w\+\.\@=')],matchstr(s:package_line,'\(^\w\+\.\)\@<=.*'))
            else 
                let s:package_names[matchstr(s:package_line,'^\w\+\.\@=')]=[matchstr(s:package_line,'\(^\w\+\.\)\@<=.*')]
            endif
        else 
            if (has_key(s:package_names,'std'))
                let s:package_names['std']=add(s:package_names['std'],s:package_line)
            else 
                let s:package_names['std']=[s:package_line]
            endif
        endif
    endfor
    return s:package_names
endfunction
"Все пакеты и классы, которые содержатся в них
call CreateArrayFromPath()
let g:package_names=s:package_names
let s:package_array_path=[]
let s:package_array_name_essence=[]
for s:package_name in sort(keys(g:package_names))
    if s:package_name=='std'
         let   s:package_path_begin=''
    else 
        let s:package_path_begin=s:package_name.'.'
    endif
    for s:package_name_essence in g:package_names[s:package_name]
        call add(s:package_array_path,s:package_path_begin.s:package_name_essence)
        call add(s:package_array_name_essence,matchstr(s:package_path_begin.s:package_name_essence,'\w\+$'))
    endfor
endfor
let g:package_array_path=s:package_array_path
"echo g:package_array_path
let g:package_array_name_essence=s:package_array_name_essence
"echo g:package_array_name_essence
"Word_before_sep
function! Word_before_sep()
    let line = getline('.')
    let start = col('.') - 1
    let start_from_deliter=start-2
    let temp_array_word=[]
    while line[start_from_deliter] !~'[     \.]' && (start_from_deliter>=0)
        call add(temp_array_word,line[start_from_deliter])
        let start_from_deliter-=1
    endwhile
    let l:word=join(reverse(temp_array_word),'')
    return l:word
endfunction
"Complete class
fun! Complete_class(findstart,base)
    if a:findstart
        " locate the start of the word
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start - 1] =~ '\a'
        let start -= 1
        endwhile
        return start
    else
        let line=getline('.')
        let start=col('.')-1
        if (line[start-1]=='.')
            let l:word_before=Word_before_sep()
            let l:counter=0
            for l:word_before_index in g:package_array_path
                if l:word_before_index=~'^'.l:word_before.'$' || l:word_before_index=~'\.'.l:word_before.'$'
                    let l:word_before_index_pack=l:word_before_index
                    let l:word_before_lib=l:word_before_index
                    let l:word_before_index=substitute(l:word_before_index,'\.','/','g')
                    let l:documentation=substitute(l:word_before_index,'$','.html','g')
                    let l:word_before_index=substitute(l:word_before_index,'$','.hx','g')
                    let l:word_before_index=substitute(l:word_before_index,'^',escape(g:haxepath,' \/').'std/','g')
                    let l:documentation=substitute(l:documentation,'^',escape(g:haxepath,' \/').'std/content/','g')
                    if filereadable(l:documentation)
                        unlet! l:documentation_oneline
                        unlet! l:documentation_lines
                        let l:documentation_lines=readfile(expand(l:documentation),'b')
                        let l:documentation_oneline=join(l:documentation_lines,"\n")

                        "let l:documentation_oneline=substitute(l:documentation_oneline,'<.\{-}>',' ','g')
                    else 
                        let l:documentation_oneline=''
                    endif
                    if filereadable(l:word_before_index)
                        let l:lines=readfile(l:word_before_index)
                    else
                        "let l:word_before_index=substitute(l:word_before_index,'^',escape(g:haxepath,' \/').'/lib/','g')
                        let l:word_before_libarr=split(l:word_before_lib,'\.')
                        let l:word_before_libelems=''
                        let startidx=0
                        for l:word_before_libnum in l:word_before_libarr
                            if startidx!=0
                                let l:word_before_libelems.='/'.l:word_before_libnum
                            else
                                let startidx+=1
                            endif
                        endfor
                        let l:word_before_index=g:haxepath.'lib/'.l:word_before_libarr[0].'/**/'.l:word_before_libarr[0].l:word_before_libelems.'.hx'
                        let l:word_before_index=expand(escape(l:word_before_index,' \/'))
                        let l:lines=readfile(l:word_before_index)
                    endif
                    for l:line_str in l:lines
                        if l:line_str =~'^\s*\(public\|static\|dynamic\|inline\)*\s*\(public\|static\|dynamic\|inline\)*\s*\(function\)\s\+[A-Za-z0-9_]\+'
                            let l:word_on_cursor=substitute(l:line_str,'^.*\(function\)\s\+\([A-Za-z0-9_]\+\).*','\2','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline,'\%^\_.*\(<dd>\_.\{-}</dd>\).*\(<dt>\_.\{-}\<'.l:word_on_cursor.'\_.\{-}<\/dt>\)\_.*$\|.*','\1\n\2','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline_word,'<.\{-}>',' ','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline_word,'&gt;','>','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline_word,'&lt;','<','g')
                            call complete_add({'word': l:word_on_cursor ,'info': l:documentation_oneline_word ,'dup':1,'menu':l:word_before_index_pack.' '.l:line_str })
                        elseif l:line_str =~'^\s*\(public\|static\|dynamic\|inline\)*\s*\(public\|static\|dynamic\|inline\)\s*var\s\+[A-Za-z0-9_]\+'
                            let l:word_on_cursor=substitute(l:line_str,'^\s*\(public\|static\|private\)*\s*\(public\|static\|private\)*\s*var\s\+\([A-Za-z0-9_]\+\).*','\3','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline,'\%^\_.*\(<dd>\_.\{-}</dd>\).*\(<dt>\_.\{-}\<'.l:word_on_cursor.'\_.\{-}<\/dt>\)\_.*$\|.*','\1\n\2','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline_word,'<.\{-}>',' ','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline_word,'&gt;','>','g')
                            let l:documentation_oneline_word=substitute(l:documentation_oneline_word,'&lt;','<','g')
                            call complete_add({'word': l:word_on_cursor,'info':l:documentation_oneline_word, 'dup':1,'menu':l:word_before_index_pack.' '.l:line_str })
                        endif
                    endfor
                endif
                let l:counter+=1
            endfor
            
        else 
            let l:counter=0
            for m in g:package_array_name_essence
                if m =~ a:base.'\w*$'
                    call complete_add({'word':m, 'dup':1,'menu': g:package_array_path[l:counter]})
                endif
                if complete_check()
                    break
                endif
                let l:counter+=1
            endfor
        endif
    endif
endfun  
function! Test()
    wincmd P
    exe "%!ls"
    wincmd p
endfun
imap <buffer> <F7> <C-0>:call Test()
set completefunc=Complete_class
set omnifunc=Complete_class

