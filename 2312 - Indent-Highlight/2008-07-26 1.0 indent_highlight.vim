" 
" Author: Andrey Tsiporukha
"
" This plugin provides a function that will highlight (or remove
" highlighting ) of the current scope and parent scope based on difference in
" indentation. It can be very usefull for languages that use indentation to
" define the scope ( python, rubby, etc. )
"
" I wrote this script so that I could quickly to restore visual stack when
" coding in python. So for python I create the following mapping in my vimrc:
"
" map <silent> <Leader>ih :call IHighlight( 1, "\^[[:space:]]*\\(def\\\\|class\\)[[:space:]]\\+" )<CR>
" map <silent> <Leader>is :call IHighlight( 0, "\^[[:space:]]*\\(def\\\\|class\\)[[:space:]]\\+" )<CR>
"
" one can extend this functionality and provide binding for movement keys so
" that highlighting also follows the cursor position
"
" which allows me by entering \ih (or \is) to highlihgt current scope along
" with the parent scope and than by entering \ih (or \is) again remove the
" highlighting
"
" the difference between \ih and \is is that
" the first one (\ih) will turn off all syntax highlighting untill indentation highlighting is active
" the syntax highlighting will be turned back on when you enter \ih again turning the indentation highlighting off
" the second one (\is) will apply indentation highlighting on top of existing syntax highlighting


" IndentInSpaces(ln):
"
" returns line indentation in spaces
" ln - line number to be inspected
function! IndentInSpaces(ln)
        let l   = getline(a:ln)
        let len = strlen(l)
        let i   = 0
        let i   = 0
        let sc   = 0
        while i < len
                if l[i] =~ ' '
                        let sc = sc + 1
                elseif l[i] =~ '\t'
                        let sc = sc + &tabstop
                else
                        break
                endif
                let i = i + 1
        endwhile
        return sc
endfunction

" FindSectionWithDifferentIndent( ln, dir, otherScopeDir, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has different indentation compare to "ln" line
" 
" ln - line number of the current line
"
" dir - dirrection in which we will be looking for different indentation
"       dir >= 0 means we'll be looking down from "ln"
"       dir <  0 means we'll be looking up from "ln"
"
" otherScopeDir - specifies whether we are looking for larger or smaller indentation
"                 otherScopeDir > 0 means we'll be looking for lines with smaller indentation
"                 otherScopeDir < 0 means we'll be looking for lines with larger indentation
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"                      if it's empty ("") it will be ignored
function! FindSectionWithDifferentIndent( ln, dir, otherScopeDir, exclude, stopIfSameIndentRE )

	"echo "FindSectionWithDifferentIndent(" . a:ln .','. a:dir.','.a:otherScopeDir.','.a:exclude.','.a:stopIfSameIndentRE .')'

        let currentLN = a:ln
        let thisIndent = IndentInSpaces(currentLN)
        let indent = thisIndent
        let otherScopeStarts = -1
	let visible = line("w0")

	let stopAtLN = 1
	
	let incr = -1

	if a:dir >= 0
		let stopAtLN = line("$")
		let incr = 1
	endif

        while 1
                let currentLN = currentLN + incr

		if ( ( a:dir >= 0 && currentLN >= stopAtLN ) || ( a:dir < 0 && currentLN <= stopAtLN ) )
			break
		endif

		let line = getline(currentLN)

		if match( line, a:exclude ) >= 0
			"echo 'Ignore 1: ' . getline(currentLN)
			continue
		endif

                let indent = IndentInSpaces(currentLN)

		let dif = thisIndent - indent
		
		"echo currentLN . ' i: ' . indent . ' ci: ' . thisIndent

		" a:otherScopeDir > 0 smaller indent
		" a:otherScopeDir < 0 bigger indent
		if ( ( dif < 0 && a:otherScopeDir < 0 ) || ( dif > 0 && a:otherScopeDir > 0 ) || ( dif == 0 && a:stopIfSameIndentRE != "" && match( line, a:stopIfSameIndentRE ) >= 0 ) )
			let otherScopeStarts = currentLN
			break
		endif
        endwhile

	if otherScopeStarts < 0
		let otherScopeStarts = stopAtLN
	endif

        return otherScopeStarts
endfunction


" FindSectionWithDifferentIndentUP( ln, otherScopeDir, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has different indentation compare to "ln" line up from "ln" line
" 
" ln - line number of the current line
"
" otherScopeDir - specifies whether we are looking for larger or smaller indentation
"                 otherScopeDir > 0 means we'll be looking for lines with smaller indentation
"                 otherScopeDir < 0 means we'll be looking for lines with larger indentation
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"                      if it's empty ("") it will be ignored
function! FindSectionWithDifferentIndentUP( ln, otherScopeDir, exclude, stopIfSameIndentRE )
	return FindSectionWithDifferentIndent( a:ln, -1, a:otherScopeDir, a:exclude, a:stopIfSameIndentRE )
endfunction

" FindSectionWithDifferentIndentDN( ln, otherScopeDir, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has different indentation compare to "ln" line down from "ln" line
" 
" ln - line number of the current line
"
" otherScopeDir - specifies whether we are looking for larger or smaller indentation
"                 otherScopeDir > 0 means we'll be looking for lines with smaller indentation
"                 otherScopeDir < 0 means we'll be looking for lines with larger indentation
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"
function! FindSectionWithDifferentIndentDN( ln, otherScopeDir, exclude, stopIfSameIndentRE )
	return FindSectionWithDifferentIndent( a:ln,  1, a:otherScopeDir, a:exclude, a:stopIfSameIndentRE )
endfunction

" FindSectionWithSmallerIndentUP( ln, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has smaller indentation compare to "ln" line up from "ln" line
" 
" ln - line number of the current line
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"
function! FindSectionWithSmallerIndentUP( ln, exclude, stopIfSameIndentRE )
	return FindSectionWithDifferentIndentUP( a:ln,  1, a:exclude, a:stopIfSameIndentRE )
endfunction

" FindSectionWithSmallerIndentDN( ln, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has smaller indentation compare to "ln" line down from "ln" line
" 
" ln - line number of the current line
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"
function! FindSectionWithSmallerIndentDN( ln, exclude, stopIfSameIndentRE )
	return FindSectionWithDifferentIndentDN( a:ln,  1, a:exclude, a:stopIfSameIndentRE )
endfunction

" FindSectionWithLargerIndentUP( ln, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has larger indentation compare to "ln" line up from "ln" line
" 
" ln - line number of the current line
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"
function! FindSectionWithLargerIndentUP( ln, exclude, stopIfSameIndentRE )
	return FindSectionWithDifferentIndentUP( a:ln,  -1, a:exclude, a:stopIfSameIndentRE )
endfunction

" FindSectionWithLargerIndentDN( ln, exclude, stopIfSameIndentRE ):
"
" returns line nuber of the first found line that has larger indentation compare to "ln" line down from "ln" line
" 
" ln - line number of the current line
"
" exlude - regexp that specifies which lines shoud be excluded from the check
"          if it's empty ("") it will be ignored
"
" stopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                      will cause the search to be stopped and line number of
"                      matching line to be reported regardless of the indentation
"
function! FindSectionWithLargerIndentDN( ln, exclude, stopIfSameIndentRE )
	return FindSectionWithDifferentIndentDN( a:ln,  -1, a:exclude, a:stopIfSameIndentRE )
endfunction

" BuildREForLines( s, e ):
"
" Helper function that builds regexp that will match block of lines starting
" at line "s" and ending at line "e"
"
function! BuildREForLines( s, e )
	let re = '\%>' . ( a:s - 1 ) . 'l.' . '\%<' . ( a:e + 1 ) . 'l'
	return re
endfunction

" IHighlight( clearSyntax, parentScopeStopIfSameIndentRE ):
"
" This function does the actual higlighting of the current scope and the
" parent scope based on indentation
"
" clearSyntax - flag that tells function whether to clear all syntax related
"               highlighting before applying indentation based highlighting
"
" parentScopeStopIfSameIndentRE - specifies regexp that will be evaluated against each line and if true
"                                 will cause the search for the parent scope (scope with smaller indent)
"                                 to be stopped and matching lines (up and down) to be used as begining 
"                                 and end of parent scope respectively
function! IHighlight( clearSyntax, parentScopeStopIfSameIndentRE )
	let l = line(".")

	let lnDNp = 0
        let lnUPp = 0
	let lnDNc = 0
	let lnUPc = 0

	let exclRE =  "\^[[:space:]]*\$"

	"echo a:parentScopeStopIfSameIndentRE
        
        let lnUPpe = FindSectionWithSmallerIndentUP( l, exclRE, a:parentScopeStopIfSameIndentRE )
        let lnDNps = FindSectionWithSmallerIndentDN( l, exclRE, a:parentScopeStopIfSameIndentRE )

	if a:parentScopeStopIfSameIndentRE != "" && match( getline(lnUPpe), a:parentScopeStopIfSameIndentRE ) >= 0
		let lnUPps = lnUPpe
	else
		let lnUPps = FindSectionWithSmallerIndentUP( lnUPpe, exclRE, a:parentScopeStopIfSameIndentRE ) + 1
	endif
	
	if a:parentScopeStopIfSameIndentRE != "" && match( getline(lnDNps), a:parentScopeStopIfSameIndentRE ) >= 0
		let lnDNps = lnDNps - 1
		let lnDNpe = lnDNps
	else
		let lnDNpe = FindSectionWithSmallerIndentDN( lnDNps, exclRE, a:parentScopeStopIfSameIndentRE ) - 1
	end

	if lnDNps > lnDNpe
		let lnDNps = lnDNpe
	endif

	if lnUPpe < lnUPps
		let lnUPps = lnUPpe
	endif





	if g:mySyntax == ""
		let g:mySyntax = &syntax
	endif

	try
		if a:clearSyntax
			syntax clear
		else
			syntax clear cScope
			syntax clear pScope
		endif
	catch /.*/
		"echo "Exception: " v:exception
	endtry

	if g:ihOn == 1
		let g:ihOn = 0
		try
			exe "set syn=". g:mySyntax
		catch /.*/
			"echo "Exception: " v:exception
		endtry
		return
	else
		let g:ihOn = 1
	endif



	let childVC = IndentInSpaces(l)
	let parentVC = IndentInSpaces(lnUPpe)


	let reC = BuildREForLines( (lnUPpe + 1), (lnDNps - 1) )
	let reC = '\%>' . childVC . 'v' . '\(' . reC . '\)'
	
	let reP = BuildREForLines( lnUPps, lnDNpe )
	let reP = '\%>' . parentVC . 'v' . '\(' . reP . '\)'

	"echo reP
	"echo reC


        silent execute 'syntax match pScope /' . reP . '/'
        silent execute 'syntax match cScope /' . reC . '/'

        highlight pScope guibg=yellow
        highlight cScope guibg=orange
endfunction

let g:ihOn = 0
let g:mySyntax = ""
