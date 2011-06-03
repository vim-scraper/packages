" File : bufmenu2.vim
" vim: ts=2 sw=2 et fdm=marker foldmarker=function!,endfunction
" Maintainer: Laurent Garnier <lauranger@free.fr>
" Last Change: 2004 Mar 30
" Version : 1.0.2
"
"-----------------------------------------------------------------------------
" Vim utility plugin for balancing highly loaded buffer menu hierarchy.
"
"-----------------------------------------------------------------------------
" Installation:
" Adjust &menuitems to your taste, may be in your .gvimrc or _vimrc
" Drop it in your user plugin directory ( :help runtimepath ),
" next vim will use it. No need for anything else.
"
"-----------------------------------------------------------------------------

" Taken from menu.vim
if !exists("no_buffers_menu")

if !exists("g:bmenu_priority")
  let g:bmenu_priority = 60
endif

if !exists("g:bmenu_max_pathlen")
  let g:bmenu_max_pathlen = 25
endif

function! s:BufMenuTruncName(fname)
	let name = a:fname
	if g:bmenu_max_pathlen < 5
		let name = ""
	else
		let len = strlen(name)
		if len > g:bmenu_max_pathlen
			let amount = (g:bmenu_max_pathlen / 2) - 2
			let left = strpart(name, 0, amount)
			let amount = g:bmenu_max_pathlen - amount - 3
			let right = strpart(name, len - amount, amount)
			let name = left . '...' . right
		endif
	endif
	return name
endfunction

function! s:BufMenuMunge(fname, bnum)
  let name = a:fname
  if name == ''
    if !exists("g:menutrans_no_file")
      let g:menutrans_no_file = "[No file]"
    endif
    let name = g:menutrans_no_file
  else
    let name = fnamemodify(name, ':p:~:.')
  endif
  " detach file name and separate it out:
  let name2 = fnamemodify(name, ':t')
  if a:bnum >= 0
    let name2 = name2 . ' (' . a:bnum . ')'
  endif
  let name = name2 . "\t" . <SID>BufMenuTruncName(fnamemodify(name,':h'))
  "let name = name2 . "\t"
  let name = escape(name, "\\. \t|")
  let name = substitute(name, "\n", "^@", "g")
  return name
endfunction

function! s:BufMenuHash(name)
  " Make name all upper case, so that chars are between 32 and 96
  let nm = substitute(a:name, ".*", '\U\0', "")
  if has("ebcdic")
    " HACK: Replace all non alphabetics with 'Z'
    "       Just to make it work for now.
    let nm = substitute(nm, "[^A-Z]", 'Z', "g")
    let sp = char2nr('A') - 1
  else
    let sp = char2nr(' ')
  endif
  " convert first six chars into a number for sorting:
  return (char2nr(nm[0]) - sp) * 0x800000 + (char2nr(nm[1]) - sp) * 0x20000 + (char2nr(nm[2]) - sp) * 0x1000 + (char2nr(nm[3]) - sp) * 0x80 + (char2nr(nm[4]) - sp) * 0x20 + (char2nr(nm[5]) - sp)
endfunction

function! s:Compare(fChampion, fChallenger, fDirection)
  let ret = 0
  let champData = toupper(s:usableBufNameTail{s:orderedBuf{a:fChampion}})
  let challData = toupper(s:usableBufNameTail{s:orderedBuf{a:fChallenger}})
  if champData < challData
	let ret = -a:fDirection
  elseif champData > challData
	let ret = a:fDirection
  endif
  return ret
endfunction

function! s:Swap(fFirst, fSecond)
  let temp = s:orderedBuf{a:fFirst}
  let s:orderedBuf{a:fFirst} = s:orderedBuf{a:fSecond}
  let s:orderedBuf{a:fSecond} = temp
endfunction

" From Robert Webb (at last I found back my source ;) thanks to google
" sortR+vim, see vim documentation for eval
" I stripped comments
function! s:SortR(fStart, fEnd, fCmpFunc, fSwapFunc, fDirection)
  if a:fStart >= a:fEnd
    return
  endif
  "
  let partition = a:fStart - 1
  let middle = partition
  let pivotBefore = (a:fStart + a:fEnd) / 2
  let pivot = pivotBefore
  let i = a:fStart
  while (i <= a:fEnd)
    exec "let result = " . a:fCmpFunc . "(i, pivot, " . a:fDirection . ")"
    if result <= 0
      let partition = partition + 1
      if result == 0
        let middle = partition
		let pivot = middle
      endif
      if i != partition
		exec "call " . a:fSwapFunc . "(i, partition)"
      endif
    endif
    let i = i + 1
  endwhile

  if middle != partition
	exec "call " . a:fSwapFunc . "(middle, partition)"
  endif
  call s:SortR(a:fStart, partition - 1, a:fCmpFunc, a:fSwapFunc, a:fDirection)
  call s:SortR(partition + 1, a:fEnd, a:fCmpFunc, a:fSwapFunc, a:fDirection)
endfunction



" First pass :
" figure out how many buffers there are
" s:usableBufCount is always defined
" s:usableBufIDs{s:usableBufCount}, s:usableBufNameTail{s:usableBufCount} 
" are defined as soon as a buffer exists
function! s:firstPass()
  let buf = 1
  let s:existingFirstLetters = ""
  let usableBufCountSoFar = 0
  while buf <= bufnr('$')
    if bufexists(buf) && !isdirectory(bufname(buf)) && buflisted(buf)
					    \ && !getbufvar(buf, "&bufsecret")
              \ && ( ! exists("s:disappearing") || s:disappearing != buf )
      let usableBufCountSoFar = usableBufCountSoFar + 1
      let s:orderedBuf{usableBufCountSoFar} = usableBufCountSoFar
      let s:usableBufIDs{usableBufCountSoFar} = buf
      let l:usableBufNameTail = fnamemodify(bufname(buf), ':p:~:t')
      let s:usableBufNameTail{usableBufCountSoFar} = l:usableBufNameTail
    endif
    let buf = buf + 1
  endwhile
  let s:usableBufCount = usableBufCountSoFar
endfunction

function! s:longestMatchFromStart(fName1, fName2, fbIgnoreCase)
  let idx = 0
  let len1 = strlen(a:fName1)
  let len2 = strlen(a:fName2)
  let guard = len1 <= len2 ? len1 : len2
  if a:fbIgnoreCase
    while idx <= guard && tolower(a:fName1[idx]) == tolower(a:fName2[idx])
      let idx = idx +1
    endwhile
   else
    while idx <= guard && a:fName1[idx] == a:fName2[idx]
      let idx = idx +1
    endwhile
  endif
  return idx
endfunction

function! s:groupSize()
  let total = s:usableBufCount
  let max = &menuitems
  let sq = max*max -4*total
  let x0 = 1
  let xn = sq
  let too = 1
  while x0 != xn && too < 15
    let x0 = xn
    let xn = ( x0*x0 + sq ) / (2*x0)
    let too = too +1
  endwhile
  let ret = (max +xn) / 2
  "echo "total=".total.", max=".max."  en ".too." ->".ret
  return ret
endfunction

function! s:cleaning()
	if exists("s:groupNumber")
	  let idx = s:groupNumber -1
	  unlet s:groupNumber
	  while idx >= 0
		if exists("s:groupStart".idx)
		  unlet s:groupStart{idx}
		endif
		if exists("s:groupEnd".idx)
		  unlet s:groupEnd{idx}
		endif
		if exists("s:groupName".idx)
		  unlet s:groupName{idx}
		endif
        let idx = idx -1
	  endwhile
    endif
	return 2
endfunction

function! s:groupDivide()
  let target = s:usableBufCount / s:groupSize()
  if target < 2
    let target = 2
  endif
  let targetToo = target +2
  let minBufIDIdx = 1
  let usableBufIDIdx = minBufIDIdx
  let charIdx = 0
  let maxBufIDIdx{charIdx} = s:usableBufCount
  let groupIdx = 0
  let s:groupStart{groupIdx} = usableBufIDIdx
  let lastChar = -2
  let augmenting2 = 0
  while charIdx >= 0
    let usableName = s:usableBufNameTail{s:orderedBuf{usableBufIDIdx}}
    let currChar = charIdx <= strlen(usableName) ? char2nr(tolower(usableName[charIdx])) : -1
    if usableBufIDIdx < maxBufIDIdx{charIdx} && currChar == lastChar
      let usableBufIDIdx = usableBufIDIdx + 1
    else
      let overEndIdx = currChar == lastChar ? usableBufIDIdx +1 : usableBufIDIdx
      "echo "a stop at ".overEndIdx
      let ziend = augmenting2 != 0 ? "".s:groupOverEnd{groupIdx} : ""
      if ( augmenting2 != 0 && overEndIdx -s:groupOverEnd{groupIdx} >= targetToo ) ||
            \ ( augmenting2 == 0 && overEndIdx -s:groupStart{groupIdx} >= targetToo )
        let charIdx = charIdx +1
        let maxBufIDIdx{charIdx} = overEndIdx -1
        if augmenting2 != 0
          "echo "  Fix previous for too big"
          let groupIdx = groupIdx +1
          let s:groupStart{groupIdx} = s:groupOverEnd{groupIdx -1}
          let augmenting2 = 0
        endif
        "echo "  Too big : have to divide"
        let usableBufIDIdx = s:groupStart{groupIdx}
        let lastChar = -2
      elseif usableBufIDIdx == maxBufIDIdx{charIdx}
        if overEndIdx -s:groupStart{groupIdx} >= targetToo
          "echo "  Was augmenting ? :".augmenting2.", fix previous"
          let groupIdx = groupIdx +1
          let s:groupStart{groupIdx} = s:groupOverEnd{groupIdx-1}
        endif
        if currChar != lastChar &&
              \ overEndIdx -s:groupStart{groupIdx} > target
          "echo "  Last will be alone"
          let s:groupOverEnd{groupIdx} = usableBufIDIdx
          let groupIdx = groupIdx +1
          let s:groupStart{groupIdx} = usableBufIDIdx
        endif
        "echo "  Finish group ".groupIdx
        let s:groupOverEnd{groupIdx} = usableBufIDIdx +1
        let groupIdx = groupIdx +1
        let augmenting2 = 0
        while charIdx > 0 && maxBufIDIdx{charIdx} == maxBufIDIdx{charIdx-1}
          let charIdx = charIdx -1
        endwhile
        let usableBufIDIdx = maxBufIDIdx{charIdx} +1
        let s:groupStart{groupIdx} = usableBufIDIdx
        if charIdx == 0
          let s:groupOverEnd{groupIdx} = overEndIdx
        endif
        let charIdx = charIdx -1
      else
        let groupCurrLen = overEndIdx -s:groupStart{groupIdx} +1
        if groupCurrLen <= target
          if groupCurrLen > 1
            let augmenting2 = 1
            let s:groupOverEnd{groupIdx} = overEndIdx
            "echo "  Prepare augmenting group n°".groupIdx." : (".s:groupStart{groupIdx}.", ".overEndIdx."+)/".target
          else
            "echo "  May start (and NOT end) group °".groupIdx
          endif
        else
          let s:groupOverEnd{groupIdx} = overEndIdx
          "echo "  Finish group and start ".groupIdx
          let augmenting2 = 0
          let groupIdx = groupIdx +1
          let s:groupStart{groupIdx} = overEndIdx
        endif
        let usableBufIDIdx = usableBufIDIdx + 1
      endif
    endif
    let lastChar = currChar
  endwhile
  let s:groupNumber = groupIdx
endfunction

function! s:nameGroups()
   let groupRun = 0
   while groupRun < s:groupNumber
     let usableBufIdx = s:groupStart{groupRun}
     let groupFirst = s:usableBufNameTail{s:orderedBuf{usableBufIdx}}

     let prevName = groupFirst
     let s:bufGroup{usableBufIdx} = groupRun
     let maxlen = strlen(groupFirst)

     let usableBufIdx = usableBufIdx +1
     while usableBufIdx < s:groupOverEnd{groupRun}
       let s:bufGroup{usableBufIdx} = groupRun
       let usableName = s:usableBufNameTail{s:orderedBuf{usableBufIdx}}
       let currlen = s:longestMatchFromStart(prevName, usableName, 1)
       if currlen < maxlen
         let maxlen = currlen
       endif
       let prevName = usableName
       let usableBufIdx = usableBufIdx +1
     endwhile
     let s:groupName{groupRun} = strpart(groupFirst, 0, maxlen)
     let min = maxlen < strlen(groupFirst) ? groupFirst[maxlen] : ""
     let max = s:usableBufNameTail{s:orderedBuf{s:groupOverEnd{groupRun} -1}}[maxlen]
     let s:groupName{groupRun} = s:groupName{groupRun}."[".min."-".max."]"
     let groupRun = groupRun +1
   endwhile
   let s:groupCard = groupRun
endfunction

function! s:delThing()
  let s:disappearing = expand("<abuf>")
  call <SID>BufMenuShow()
  unlet s:disappearing
endfunction

function! s:reverseIndex()
	let rank = 1
	while rank <= s:usableBufCount
	  let s:orderedBufsRank{s:orderedBuf{rank}} = rank
	  let rank = rank +1
	endwhile
endfunction

function! s:BufMenuShow(...)

  if a:0 == 1
    let g:bmenu_priority = a:1
  endif

  " remove old menu, if exists; keep one entry to avoid a torn off menu to
  " disappear.
  silent! unmenu &Buffers
  exe 'menu ' . g:bmenu_priority . ".1 &Buffers.Dummy l"
  silent! unmenu! &Buffers

  " create new menu; set 'cpo' to include the <CR>
  let cpo_save = &cpo
  set cpo&vim
  exe 'am <silent> ' . g:bmenu_priority . ".2 &Buffers.&Refresh\\ menu :call <SID>BufMenuShow()<CR>"
  exe 'am ' . g:bmenu_priority . ".4 &Buffers.&Delete :bd<CR>"
  exe 'am ' . g:bmenu_priority . ".6 &Buffers.&Alternate :b #<CR>"
  exe 'am ' . g:bmenu_priority . ".7 &Buffers.&Next :bnext<CR>"
  exe 'am ' . g:bmenu_priority . ".8 &Buffers.&Previous :bprev<CR>"
  exe 'am ' . g:bmenu_priority . ".9 &Buffers.-SEP- :"
  let &cpo = cpo_save
  unmenu &Buffers.Dummy

  " First pass :
  " figure out how many buffers there are
  " s:usableBufCount is always defined
  " s:usableBufIDs{s:usableBufCount}, s:usableBufNameTail{s:usableBufCount} 
  " are defined as soon as a buffer exists
  call <SID>firstPass()
  if s:usableBufCount > 1 
	call <SID>SortR(1, s:usableBufCount,"s:Compare","s:Swap", 1)
	" s:orderedBuf{1..s:usableBufCount} contains the "indices" (ranks)
	" sur s:usableBufIDs{1..s:usableBufCount}
	" et s:usableBufNameTail{1..s:usableBufCount}
	" sorted by comparison expressed by s:Compare -> buffers names
  call <SID>reverseIndex()
  else
	let s:orderedBufsRank{1} = 1
  endif

  let splitmenu = 0
  if s:usableBufCount > &menuitems
    let splitmenu = 1
    call <SID>groupDivide()
    call <SID>nameGroups()
  endif

  let usableBufIDIdx = 1
  while usableBufIDIdx <= s:usableBufCount
    let s:usableBufID = s:usableBufIDs{s:orderedBuf{usableBufIDIdx}}
    let bufFullName = bufname(s:usableBufID)
    let munge = <SID>BufMenuMunge(bufFullName, s:usableBufID)
    let hash = usableBufIDIdx +11
    if splitmenu == 0
      let commString = 'am ' . g:bmenu_priority . '.' . hash . ' &Buffers.' . munge
    else
      let groupNum = s:bufGroup{usableBufIDIdx}
      if s:groupStart{groupNum} == s:groupOverEnd{groupNum} -1 || s:groupCard == 1
        let commString = 'am ' . g:bmenu_priority . '.' . hash . ' &Buffers.' . munge
      else
        let group = s:groupName{groupNum}
        let group = escape(group, "\\. \t|")
        let commString = 'am ' . g:bmenu_priority . '.' . hash . '.' . hash . ' &Buffers.' . group . '.' . munge
      endif
    endif
    exe commString . ' :b' . s:usableBufID . '<CR>'
    let usableBufIDIdx =  usableBufIDIdx +1
  endwhile
  aug buffer_list
  au!
  au BufNewFile,BufCreate,BufFilePost * call <SID>BufMenuShow()
  au BufDelete * call <SID>delThing()
  aug END

endfunction

" When just starting Vim, load the buffer menu later
if has("vim_starting")
  augroup LoadBufferMenu
    au! VimEnter * if !exists("no_buffers_menu") | call <SID>BufMenuShow() | endif
    au  VimEnter * au! LoadBufferMenu
  augroup END
else
  call <SID>BufMenuShow()
endif

endif " !exists("no_buffers_menu")

