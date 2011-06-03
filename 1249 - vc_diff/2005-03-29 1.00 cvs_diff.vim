"for check patch

"export MyPatch()
"export MakePatchFile()
"export ShowDifference()
"export FoldFileName()

set patchexpr=MyPatch()
function! MyPatch()
    execute '!patch -R -o ' . v:fname_out . 
        \ ' ' . v:fname_in . ' < ' . v:fname_diff
endfunction


let s:patch_archive="./patch_archive.txt"

function! MakePatchCmd(file_name, patch_file)
    let l:make_patch_cmd=
        \ ':!cvs diff -p ' . 
        \ a:file_name . 
        \ ' | grep -v "^\?" > ' .
        \ a:patch_file
    if a:file_name==""
        let l:make_patch_cmd=l:make_patch_cmd . 
            \ ';grep "^Index: " ' .
            \ a:patch_file
    endif
    return l:make_patch_cmd
endfunction

function! MakePatch(paf, patch_target, patch_file_name)
    let     l:patch_sp="Index: " . a:patch_target
    silent execute 
        \ '!awk "BEGIN {make_patch_flag=0;skip_line=0}' . 
        \ '/^' . 
        \ escape(l:patch_sp, "/") . 
        \ '\$/{make_patch_flag=1;next}' . 
        \ '/^Index:/{if(make_patch_flag==1){exit}}' . 
        \ '{if((make_patch_flag==1)&&(skip_line>2))' . 
        \ '{print \$0}else {skip_line++}} " ' .
        \ a:paf  . 
        \ ' > ' .
        \ a:patch_file_name
endfunction

function! ShowDifference()
    let     l:patch_target=strpart(getline("."), 7)
    if getftime(l:patch_target) == -1
        echo "Not such file"
        return
    endif
    let     l:patch_tmp="./patch"
    call    MakePatch(expand("%"), l:patch_target, l:patch_tmp)
    if getfsize(l:patch_target) == 0
        echo "Not patch"
    endif
    silent execute ":set columns=165"
    silent execute ":e " . l:patch_target
    silent execute ":vert diffpatch " . l:patch_tmp
endfunction

function! MakePatchFile()
    execute MakePatchCmd("", s:patch_archive)
    if getftime(s:patch_archive) == -1
        echo "Need " . s:patch_archive
        return
    endif
    silent execute ":e " . s:patch_archive
    call FoldPatch()
endfunction

function! FoldPatch()
    let l:last_line=line("$")
    let i = 1
    let l:fold_begin=0
    let l:fold_end=0
    while i <= l:last_line
        if getline(i) =~# "^Index: .*$"
            if l:fold_end != 0
                silent execute l:fold_begin + 1 . "," . l:fold_end . "fold"
                let l:fold_begin = 0
                let l:fold_end = 0
            endif
            let l:fold_begin=i
        elseif l:fold_begin != 0
            let l:fold_end=i
        endif
        let i = i + 1
    endwhile
    if l:fold_end != 0
    silent execute l:fold_begin + 1 . "," . l:fold_end . "fold"
    endif
endfunction

function! FoldFileName()
    let i=line(".")
    if getline(i) !~# "^Index: .*$"
        return
    endif
    let l:last_line=line("$")
    while i <= l:last_line
        if foldclosed(i) != -1
            silent execute line(".") . "," . foldclosedend(i) . "fold"
            break
        endif
        let i = i + 1
    endwhile
endfunction

command! -nargs=0 CDmake call MakePatchFile()
command! -nargs=0 CDshow call ShowDifference()
