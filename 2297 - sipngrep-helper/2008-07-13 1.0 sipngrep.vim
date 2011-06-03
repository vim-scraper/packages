" Helper functions for for ngrep capture of sip traffic
" Language:	sip packets
" Last Change:	14 July 2008
" Maintainer:	Stanisław Pitucha <viraptor@gmail.com>
" License:	This file is placed in the public domain.

if exists("b:loaded_sipngrep_ftplugin")
  finish
endif
let b:loaded_sipngrep_ftplugin = 1

if !exists("*s:LocalCseq")
	function! s:LocalCseq()
		let icsave = &ignorecase

		let currentPos = [line('.'), col('.')]

		let &ignorecase = 0
		let packetStart = search('^[TU] ', 'bcW')
		let packetEnd = search('^\s*$', 'nW')
	
		let &ignorecase = 1
		let cseq = search('^cseq\s*:\s*\d\+\s\+[a-z]\+', 'W', packetEnd)
		let &ignorecase = icsave

		call cursor(currentPos)

		if ! cseq
			return [-1, 'pos = 0']
		endif

		let cseqLine = getline(cseq)
		let cseqParts = matchlist(cseqLine, ':\s*\(\d\+\)\s\+\([A-Z]\+\)')
		if ! len(cseqParts)
			return [-1, 'didn''t match cseq']
		endif

		return [cseqParts[1]+0, cseqParts[2]]
	endfunction
	
	function! s:MatchingPacket(req, cseq)
		if a:cseq[0] == -1
			echoerr 'Cursor is not over a proper packet, error: '.a:cseq[1]
			return
		endif

		let [startLine, startCol] = [line('.'), col('.')]
		let bfopt = {0: '', 1: 'b'}[a:req]

		let icsave = &ignorecase
		let scsave = &smartcase
		let &smartcase = 0
		while 1
			let &ignorecase = 1
			let pktCseq = search('^cseq\s*:\s*'.a:cseq[0].'\s\+'.a:cseq[1], bfopt.'W')
			if ! pktCseq
				echoerr 'No matching '.{0: 'response', 1: 'request'}[a:req].' found'
				call cursor(startLine, startCol)
				break
			endif

			let &ignorecase = 0
			let pktStart = search('^[TU] ', 'bWn')
			if ! a:req && pktStart <= startLine
				continue
			endif
			let pktType = getline(pktStart+1)

			if match(pktType, '^SIP/2\.0 ') == {0: 0, 1: -1}[a:req]
				call cursor(pktStart, 1)
				break
			endif
			
			call cursor(pktCseq+{0: 1, 1: -1}[a:req], 1)
		endwhile
		let &ignorecase = icsave
		let &smartcase = scsave
	endfunction
endif

noremap <buffer> <unique> <script> <Plug>SkipToMatchingRequest <SID>MatchingRequest
noremap <buffer> <unique> <script> <Plug>SkipToMatchingResponse <SID>MatchingResponse
noremap <buffer> <SID>MatchingRequest :call <SID>MatchingPacket(1, <SID>LocalCseq())<CR>
noremap <buffer> <SID>MatchingResponse :call <SID>MatchingPacket(0, <SID>LocalCseq())<CR>

if !hasmapto('<Plug>SkipToMatchingRequest')
	map <buffer> <unique> <LocalLeader>sq <Plug>SkipToMatchingRequest
endif
if !hasmapto('<Plug>SkipToMatchingResponse')
	map <buffer> <unique> <LocalLeader>sr <Plug>SkipToMatchingResponse
endif
