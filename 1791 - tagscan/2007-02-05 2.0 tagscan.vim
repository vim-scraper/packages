" File: tagscan.vim
" Author: Nickolay Golubev
" Email: golubev.nikolay@gmail.com
"
"  if you've fail with lines
"  tags: tag1, tag2, tag3
"  --- some info
"  --- some info
"  tags: tag3, tag1, tag5
"  --- some info
"  --- some info
"  You can type command
"  :ScanTags
"  and do fast find by tags lines you want
"  :Select   command do add tag to list ( <CR> key )
"  :Deselect command remove last tag from list ( <BS> key )
"
"  Just start to type tag name you need and you'll automatically move to
"  correct place, then push <CR>
"  Use <Esc> to start type from begin
"
"  Script written for easy find information in tagged files (.sco files for
"  example)
"
" Please email me problems
"
if exists('g:tag_scan_plugin')
    finish
endif

let g:tag_scan_plugin = 1

function! s:ReturnTags( string )
    let string = substitute( a:string, 'tags:\(.*\)', '\1', '')
    return split(string,'\W\+')
endfunction

function! s:GatherAllTags()
    let current_line_number = line('.')
    exec ':1'

    let line_number = 1

    let result = []
    let search_flags = 'cW'

    while line_number > 0
        let line_number = search( 'tags:', search_flags )

        if 0 == line_number
            break
        endif

        let line = getline( line_number )
        let search_flags = 'W'

        let tags = s:ReturnTags( line )

        let additional_tag = "#".line_number

        if line_number > 1
            let header_line = getline ( line_number - 1)
            let header = substitute( header_line, 'header:\s*\(.*\)', '\1', '')
            if header != ""
                let additional_tag = "#:".header
            endif
        endif

        call add(tags, additional_tag)
        call add(result, [line_number, sort(tags)] )
    endwhile

    return result
endfunction

function! s:IsInList( list, value )
    for item in a:list
        if item == a:value
            return 1
        endif
    endfor

    return 0
endfunction

function! s:BuildUniqueTagList( list )
    let result = []
        
    for tag_list in a:list
        for item in tag_list[1]
            if ! s:IsInList( result, item )
                call add(result, item)
            endif
        endfor
    endfor

    return result
endfunction

function! s:ClearBuffer()
    setlocal modifiable
    let last_line = line('$')
    exec ":1,".last_line."d"
    setlocal nomodifiable
endfunction

function! s:JumpToResult( line_number )
    exec "wincmd c"
    exec a:line_number
endfunction

function! s:SelectTag( tag_name )
    let tags_count = len( b:lastTags )

    let i = 0
    while i < tags_count
        let list = b:lastTags[i][1]

        if ! s:IsInList( list, a:tag_name )
            call remove(b:lastTags, i)
            let i-=1
            let tags_count-=1
        endif

        let i += 1
    endwhile

    if len(b:lastTags) == 1
        call s:JumpToResult( b:lastTags[0][0] )
        return 1
    endif

    call add( b:selected_tags, a:tag_name )
    call s:RefreshSelectWindow()

    return 0
endfunction

function! s:IsSelectedTagsLine( line_number )
    return a:line_number == 3
endfunction

function! s:IsInformationArea( line_number )
    if a:line_number < 4 
        return 1
    endif

    return 0
endfunction

function! s:HandleMatches()
    let matches = s:CheckTagMatches()
    let b:matches = matches
    
    if matches
        call s:HighlihgtTagsMatches()
    else
        call s:HighlihgtTagsNormal()
    endif
endfunction

function! s:SelectTagUnderCursor()
    let b:last_typed_chars = ""
    let line_number = line('.')

    if b:matches && s:IsSelectedTagsLine(line_number)
        call s:JumpToResult( b:matches_line_number )
        return
    endif

    if s:IsInformationArea(line_number)
        return
    endif

    let line = getline(line_number)
    if line == "" 
        return
    endif


    if s:SelectTag( line )
        return
    endif

    call s:HandleMatches()

    if len(b:list_to_select) == 1 && ! b:matches
        call s:SelectTag(b:list_to_select[0])
    endif

    if len(b:list_to_select) == 0 && b:matches
        call s:JumpToResult( b:matches_line_number )
    endif

endfunction

function! s:HighlihgtTagsNormal()
    hi link tags_scan_selected          Identifier
    hi link tags_scan_selected_info     Statement
endfunction
function! s:HighlihgtTagsMatches()
    hi link tags_scan_selected          Identifier
    hi link tags_scan_selected_info     Todo
endfunction

function! s:DeselectLastTag()
    if len (b:selected_tags) == 0
        return
    endif

    let tag = b:selected_tags[-1]
    call s:DeselectTag( tag )

    call s:HandleMatches()
endfunction

function! s:DeselectTag( tag_name )
    let tags_count = len( b:allTags )

    let i = 0
    while i < tags_count
        let list = b:allTags[i]

        let doAdd = 0
        if ! s:IsInList( list[1], a:tag_name )
            let doAdd = 1
        endif

        for item in b:selected_tags
            if item != a:tag_name && ! s:IsInList( list[1], item )
                let doAdd = 0
                break
            endif
        endfor

        if doAdd
            call add(b:lastTags, list)
        endif

        let i += 1
    endwhile

    call filter( b:selected_tags, 'v:val !~ "'.a:tag_name.'"')
    call s:RefreshSelectWindow()
endfunction

function! s:CheckTagMatches()
    let selected = sort(b:selected_tags)
    for tag_list in b:allTags
        if selected == tag_list[1][1:-1] || selected == tag_list[1][0:-1]
            let b:matches_line_number = tag_list[0]
            return 1
        endif
    endfor

    return 0
endfunction

function! s:AddTagsToBuffer( tags )
    setlocal modifiable

    call s:MapQuickSearch()
    let unique = s:BuildUniqueTagList( a:tags )

    let unique = sort(unique)
    let line_number = 0

    let added_count = 0
    let b:list_to_select = []
    for item in unique
        if s:IsInList( b:selected_tags, item)
            continue
        endif
        if item =~ '^#'
            continue
        endif

        call append( line_number, item ) 
        call add(b:list_to_select, item)
        let line_number += 1
        let added_count += 1
    endfor

    if added_count == 0
        call s:UnmapQuickSearch()
        for item in unique
            if s:IsInList( b:selected_tags, item)
                continue
            endif
            if item =~ '^#'
                call append( line_number, item ) 
                call add(b:list_to_select, item)
                let line_number += 1
                continue
            endif
        endfor
    else
        "call s:MapQuickSearch()
    endif

    exec line_number+1."d"
    setlocal nomodifiable
endfunction

function! s:AddHeaderInfo()
    setlocal modifiable

    let selected_tags_prompt = ""
    for item in b:selected_tags 
        let selected_tags_prompt .= item.', '
    endfor

    call append(0, "selected tags: ".selected_tags_prompt)
    call append(0, "typed tag: ")
    call append(0, "source: ".b:source_name)

    setlocal nomodifiable
endfunction

function! s:RefreshSelectWindow()
    call s:ClearBuffer()
    call s:AddTagsToBuffer( b:lastTags )
    call s:AddHeaderInfo()
    exec ":4"
endfunction

function! s:TypedTagRefresh()
    set modifiable
    let text = "typed tag: ".b:last_typed_chars
    call setline(2, text)
    set nomodifiable
endfunction

function! s:MoveToClear()
    let b:last_typed_chars = ""
    call s:TypedTagRefresh()
endfunction

function! s:MoveToPlus( char )
    let b:last_typed_chars .= a:char

    exec ":3"

    let search_flags = 'cW'
    let line_number = search( '^'.b:last_typed_chars, search_flags )

    if line_number
        exec ":".line_number
    endif

    call s:TypedTagRefresh()
endfunction

let s:alphabet = "q w e r t y u i o p a s d f g h j k l z x c v b n m _ 1 2 3 4 5 6 7 8 9 0 "
let s:alphabet_list = split(s:alphabet, " ")

function! s:MapQuickSearch()
    for letter in s:alphabet_list
        let map_command = "nnoremap <buffer> ".letter." :MoveToPlus '".letter."' <CR>" 
        exec map_command
    endfor

    nnoremap <buffer> <Esc> :MoveToClear<CR>
endfunction

function! s:UnmapQuickSearch()
    for letter in s:alphabet_list
        let map_command = "nunmap <buffer> ".letter
        exec map_command
    endfor

    nunmap <buffer> <Esc>
endfunction

function! s:CreateBufferWindowAndSetup( name )
    exec "split _FIND_BY_TAGS_"

    setlocal noswapfile
    setlocal buftype=nofile
    setlocal bufhidden=delete

    syn match tags_scan_source /^source:/ nextgroup=tags_scan_source_info
    syn match tags_scan_selected /^selected tags:/ nextgroup=tags_scan_selected_info
    syn match tags_typed_tag /^typed tag:/ nextgroup=tags_typed_tag_info
    syn match tags_scan_source_info /.*/ contained
    syn match tags_scan_selected_info /.*/ contained
    syn match tags_typed_tag_info /.*/ contained

    hi link tags_scan_source            Define
    hi link tags_scan_source_info       Comment

    hi link tags_typed_tag              Statement
    hi link tags_typed_tag_info         String

    call s:HighlihgtTagsNormal()

    command! -buffer Select call <SID>SelectTagUnderCursor()
    command! -buffer Deselect call <SID>DeselectLastTag()
    command! -buffer -nargs=1 MoveToPlus call <SID>MoveToPlus(<args>)
    command! -buffer MoveToClear call <SID>MoveToClear()

    nnoremap <BS> :Deselect<CR>
    nnoremap <buffer> <CR> :Select<CR>

    let b:last_typed_chars = ""
    call s:MapQuickSearch()
endfunction

function! s:ShowSelectWindow( allTags )
    let full_name = expand("%")
    let short_name = expand("%:t")

    call s:CreateBufferWindowAndSetup( short_name )

    let b:source_name = full_name
    let b:selected_tags = []
    let b:allTags = a:allTags
    let b:lastTags = copy (b:allTags)
    let b:matches = 0
    let b:matches_line_number = 1

    call s:RefreshSelectWindow()
endfunction

function! s:ScanTags()
    let tags = s:GatherAllTags()
    call s:ShowSelectWindow( tags )
endfunction

command! ScanTags call s:ScanTags()

