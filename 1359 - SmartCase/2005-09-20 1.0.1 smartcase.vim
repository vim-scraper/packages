" Script Name: smartcase.vim
" Version:     1.0.1
" Last Change: September 21, 2005
" Author:      Yuheng Xie <elephant@linux.net.cn>
"
" Description: replacing words while keeping original lower/uppercase style
"
"              An example, you may want to replace any FileSize appears in
"              your program into LastModifiedTime. Since it appears everywhere
"              as both uppercases and lowercases, you have to write it several
"              times:
"
"                :%s/FileSize/LastModifiedTime/g      " function names
"                :%s/file_size/last_modified_time/g   " variable names
"                :%s/FILE_SIZE/LAST_MODIFIED_TIME/g   " macros
"                :%s/File size/Last modified time/g   " document/comments
"                ......
"
"              This script copes with the case style for you so that you need
"              write just one command:
"
"                :%s/file\A\?size/\=SmartCase("LastModifiedTime")/ig
"
" Details:     SmartCase(...) recognizes words in three case styles: 1: xxxx
"              (all lowercases), 2: XXXX(all uppercases) and 3: Xxxx(one
"              uppercase following by lowercases).
"
"              It first tries to find out the words and their case styles in
"              the reference string (which in most cases will be submatch(0),
"              that is, the string your are going to replace). Then it finds
"              out all the words in your replacing string. Finally it combines
"              the words from your replacing string and the case styles from
"              the reference string into a result string.
"
"              For example, a reference string "getFileName" will be cut into
"              three words: "get"(style 1), "File"(style 3) and "Name"(style
"              3). If the replacing string is "MAX_SIZE", it will be treated
"              as two words: "max" and "size", their case styles is
"              unimportant. The final result string will be "maxSize".
"
"              A note, in the case some uppercases following by some
"              lowercases, e.g. "HTMLFormat", SmartCase will treat it as
"              "HTML"(2) and "Format"(3) instead of "HTMLF"(2) and "ormat"(1).
"
" Usage:       1. call SmartCase(replacing, reference) in replace expression
"
"              The simplest way: (in most cases, you will need the /i flag)
"
"                :%s/goodday/\=SmartCase("HelloWorld")/ig
"
"              This will replace any GoodDay into HelloWorld, GOODDAY into
"              HELLOWORLD, etc.
"
"              If the reference string is ignored, it will be set to
"              submatch(0). Or if it's a number n, it will be set to
"              submatch(n). Example:
"
"                :%s/good\(day\)/\=SmartCase("HelloWorld", 1)/ig
"
"              It's equal to:
"
"                :%s/good\(day\)/\=SmartCase("HelloWorld", submatch(1))/ig
"
"              2. use SmartCase as command
"
"              First search for a string: (\c for ignoring case)
"
"                /\cgoodday
"
"              Then use command: (note that a range is needed, and it doesn't
"              matter whether you say "hello world" or "HelloWorld" as long as
"              words could be discerned.)
"
"                :%SmartCase hello world
"
"              This will do exactly the same as mentioned in usage 1.

command! -rang -nargs=+ SmartCase :<line1>,<line2>s//\=SmartCase(<q-args>)/g

" replace the words in reference with the words from replacing while keeping
" reference's case style
function! SmartCase(...) " SmartCase(replacing, reference = 0)
	if a:0 == 0
		return
	elseif a:0 == 1
		let replacing = a:1
		let reference = submatch(0)
	else
		let replacing = a:1
		let reference = a:2
		if matchstr(reference, '\d\+') == reference
			let reference = submatch(0 + reference)
		endif
	endif

	let regexp = '\l\+\|\u\l\+\|\u\+\l\@!'
	let result = ""
	let i = 0
	let j = 0
	let separator = ""
	let case = 0
	while j < strlen(replacing)
		if i < strlen(reference)
			let s = match(reference, regexp, i)
			if s >= 0
				let e = matchend(reference, regexp, s)
				let separator = strpart(reference, i, s - i)
				let word = strpart(reference, s, e - s)
				if word ==# tolower(word)
					let case = 1  " all lowercases
				elseif word ==# toupper(word)
					let case = 2  " all uppercases
				else
					let case = 3  " one uppercase following by lowercases
				endif
				let i = e
			endif
		endif

		let s = match(replacing, regexp, j)
		if s >= 0
			let e = matchend(replacing, regexp, s)
			let word = strpart(replacing, s, e - s)
			if case == 1
				let result = result . separator . tolower(word)
			elseif case == 2
				let result = result . separator . toupper(word)
			elseif case == 3
				let result = result . separator . toupper(strpart(word, 0, 1)) . tolower(strpart(word, 1))
			else
				let result = result . separator . word
			endif
			let j = e
		else
			break
		endif
	endwhile

	while i < strlen(reference)
		let e = matchend(reference, regexp, i)
		if e >= 0
			let i = e
		else
			break
		endif
	endwhile
	let result = result . strpart(reference, i)

	return result
endfunction
