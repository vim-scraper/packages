" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/MapFinder.vim	[[[1
46
" This plugin provides commands to find maps using vim patterns, which are
" matched against the rhs of a map.
" Author: Marcin Szamotulski

" Search for a map:
" a:mode one of i,n,v,c.
function! <SID>MapSearch(bang,rhs_pattern,...)
    let mode = ( a:0 >= 1 ? a:1 : '' )
    let more = &more
    setl nomore
    redir => maps
	exe "silent ".mode."map"
    redir end
    let &l:more = more
    let list = split(maps, "\n")
    let rhs_list  = ( a:bang == "" ? map(copy(list), 'matchstr(v:val, ''.\s\+\S\+\s\+\zs.*'')') :
		\ map(copy(list), 'matchstr(v:val, ''.\s\+\zs\S\+\s\+.*'')') )
    let i = 0
    let i_list = []
    for rhs in rhs_list
	if rhs =~ a:rhs_pattern
	    call add(i_list, i)
	endif
	let i+=1
    endfor

    let found_maps = []
    for i in i_list
	call add(found_maps, list[i])
    endfor
    if len(found_maps) > 0
	echo join(found_maps, "\n")
    else
	echohl WarningMsg
	echo "No such map"
	echohl Normal
    endif
endfunction
command! -bang -nargs=? Map	:call <SID>MapSearch(<q-bang>,<q-args>, '')
command! -bang -nargs=? Nmap	:call <SID>MapSearch(<q-bang>,<q-args>, 'n')
command! -bang -nargs=? Imap	:call <SID>MapSearch(<q-bang>,<q-args>, 'i')
command! -bang -nargs=? Cmap	:call <SID>MapSearch(<q-bang>,<q-args>, 'c')
command! -bang -nargs=? Vmap	:call <SID>MapSearch(<q-bang>,<q-args>, 'v')
command! -bang -nargs=? Smap	:call <SID>MapSearch(<q-bang>,<q-args>, 's')
command! -bang -nargs=? Omap	:call <SID>MapSearch(<q-bang>,<q-args>, 'o')
command! -bang -nargs=? Lmap	:call <SID>MapSearch(<q-bang>,<q-args>, 'l')
doc/MapFinder.txt	[[[1
18
*map_finder.txt* Find maps using vim patterns		15 September 2011
Author: Marcin Szamotulski
Email: mszamot [AT] gmail [dot] com

Map finder is a small plugin which provides commands which filter the output
of |:map|, |:nmap|, |:imap|, |:cmap|, |:smap|, |:vmap|, |:omap|, |:lmap|
commands. It provides commands:
:Map[!]		{pattern}		*:Map*
:Nmap[!]	{pattern}		*:Nmap*
:Imap[!]	{pattern}		*:Imap*
:Smap[!]	{pattern} 		*:Smap*
:Vmap[!]	{pattern}		*:Vmap*
:Omap[!]	{pattern}		*:Omap*
:Lmap[!]	{pattern}		*:Lmap*
where {pattern} is a vim pattern which will filter the _rhs_ of command maps
as shown by the corresponding _map command_. If |<bang>| is added the pattern is
matched against output of [nisvol]map commands with only first column removed
(which indicates the map mode).
