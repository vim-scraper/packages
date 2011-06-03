
",rem		: (re)place (m)ultiple
"ReadColumns	> ReadColumns(buffer, sep, keycol, valcol)
"ReadUserlist	> ReadUserlist() 	Get user input for multirep
"ReadDictionary	> ReadDictionary(buffer) 	Get list for multirep
"MultiReplace	> MultiReplace(dict) 	Perform multireplace wth dict

",rem		: (re)place (m)ultiple
"---------------------------------------------------------------------
:map ,rem :call MultiReplace("",1,"$")
:vmap ,rem :call MultiReplace("",line("'<"),line("'>"))

"ReadColumns	> ReadColumns(buffer, sep, keycol, valcol)
" --------------------------------------------------------------------
function! ReadColumns(buffer, sep, keycol, valcol)
        let dict={}
        for line in getbufline(a:buffer, 1, "$") 
		let cols = split(line, a:sep)
		if len(cols)>=2 
			if (exists("l:dict[cols[a:keycol-1]]")) 
				if (type(dict[cols[a:keycol-1]])==type(""))
					let temp=dict[cols[a:keycol-1]]
					unlet dict[cols[a:keycol-1]]
					let dict[cols[a:keycol-1]]=[temp]
				endif
                                call add(dict[cols[a:keycol-1]], cols[a:valcol-1])
			else
				let dict[cols[a:keycol-1]] = cols[a:valcol-1]
			endif
		endif
        endfor
        return dict
endfunction


"ReadUserlist	> ReadUserlist() 	Get user input for multirep
" --------------------------------------------------------------------
function! ReadUserlist()
        let dict={}
	let pair = matchlist(input("Enter key=val pair, null input ends: "), '^\([^\t]*\)=\(.*\)')
	while len(pair)>=2 
		if (exists("l:dict[pair[1]]")) 
			if (type(dict[pair[1]])==type(""))
				let temp=dict[pair[1]]
				unlet dict[pair[1]]
				let dict[pair[1]]=[temp]
			endif
			call add(dict[pair[1]], pair[2])
		else
			let dict[pair[1]] = pair[2]
		endif
		let pair = matchlist(input("Enter key=val pair, null input ends: "), '^\([^\t]*\)=\(.*\)')
	endw
        return dict
endfunction


"ReadDictionary	> ReadDictionary(buffer) 	Get list for multirep
" --------------------------------------------------------------------
function! ReadDictionary(buffer)
        let dict={}
        for line in getbufline(a:buffer, 1, "$") 
                let pair = matchlist(line, '^\([^\t]\+\)\t\(.*\)')
		if len(pair)>=2 
			if (exists("l:dict[pair[1]]")) 
				if (type(dict[pair[1]])==type(""))
					let temp=dict[pair[1]]
					unlet dict[pair[1]]
					let dict[pair[1]]=[temp]
				endif
                                call add(dict[pair[1]], pair[2])
			else
				let dict[pair[1]] = pair[2]
			endif
		endif
        endfor
        return dict
endfunction


"MultiReplace	> MultiReplace(dict) 	Perform multireplace wth dict
" --------------------------------------------------------------------
function! DictSortCompare(i1, i2)
	if (len(a:i1)==len(a:i2)) 
		return a:i1 == a:i2 ? 0 : a:i1 > a:i2 ? 1 : -1
	else
		return len(a:i1) < len(a:i2) ? 1 : -1
	endif
endfunction
function! MultiReplace(dict, from, to) range
        if (type(a:dict)!=type({}))
		if (type(a:dict)==type(0)) 
				let userdict = ReadDictionary(a:dict)
		endif
		if (type(a:dict)==type("")) 
			if (a:dict=="") 
				:ls
				let userinput = input("Enter buffer number to use as dictionary or '=' to enter value pairs: ")
				let bufno = str2nr(userinput)
				if (userinput=="=") 
					let userdict = ReadUserlist()
				elseif (userinput=="#") 
					let userdict = ReadDictionary(userinput)
				elseif (userinput=="%") 
					let userdict = ReadDictionary(userinput)
				elseif (bufno>0) 
					let userdict = ReadDictionary(bufno)
				else
					return
				endif
			else
				let userdict = ReadDictionary(str2nr(a:dict))
			endif
		endif
        else
                let userdict = a:dict
        endif
        let replacementsFrom=[]
        let replacementsTo=[]
        let replacementCount=0
        for key in sort(copy(keys(userdict)),"DictSortCompare")
                let str = key
                let str = substitute(str, '#'      , '##'                             , 'g')
                let str = substitute(str, '_'      , '#_'                             , 'g')
                let str = substitute(str, '!'      , '#!'                             , 'g')
                let str = substitute(str, 'n'      , 'n_'                             , 'g')
                let str = substitute(str, '\(\d\)' , '\1_'                            , 'g')
                let str = substitute(str, '\(\d\)_', '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '\(n\)_' , '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '#\(_\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '#\(!\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '#\(#\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
                call add(replacementsFrom, "")
                let replacementsFrom[replacementCount] = str

                if type(userdict[key])==type([])
			call add(replacementsTo, [])
			for str in userdict[key]
				let str = substitute(str, '#'      , '##'                             , 'g')
				let str = substitute(str, '_'      , '#_'                             , 'g')
				let str = substitute(str, '!'      , '#!'                             , 'g')
				let str = substitute(str, 'n'      , 'n_'                             , 'g')
				let str = substitute(str, '\(\d\)' , '\1_'                            , 'g')
				let str = substitute(str, '\(\d\)_', '\="#n".char2nr(submatch(1))."!"', 'g')
				let str = substitute(str, '\(n\)_' , '\="#n".char2nr(submatch(1))."!"', 'g')
				let str = substitute(str, '#\(_\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
				let str = substitute(str, '#\(!\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
				let str = substitute(str, '#\(#\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
				call add(replacementsTo[replacementCount], str)
			endfor
		else
			call add(replacementsTo, "")
			let str = userdict[key]
			let str = substitute(str, '#'      , '##'                             , 'g')
			let str = substitute(str, '_'      , '#_'                             , 'g')
			let str = substitute(str, '!'      , '#!'                             , 'g')
			let str = substitute(str, 'n'      , 'n_'                             , 'g')
			let str = substitute(str, '\(\d\)' , '\1_'                            , 'g')
			let str = substitute(str, '\(\d\)_', '\="#n".char2nr(submatch(1))."!"', 'g')
			let str = substitute(str, '\(n\)_' , '\="#n".char2nr(submatch(1))."!"', 'g')
			let str = substitute(str, '#\(_\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
			let str = substitute(str, '#\(!\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
			let str = substitute(str, '#\(#\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
			let replacementsTo[replacementCount]=str
		endif

                let replacementCount = replacementCount + 1
        endfor
        let idx = a:from
	let rotationcounters={}
        for dummyline in getbufline("%", a:from, a:to) 
                let line = getbufline("%", idx, idx)[0]
		let str = line
                let str = substitute(str, '#'      , '##'                             , 'g')
                let str = substitute(str, '_'      , '#_'                             , 'g')
                let str = substitute(str, '!'      , '#!'                             , 'g')
                let str = substitute(str, 'n'      , 'n_'                             , 'g')
                let str = substitute(str, '\(\d\)' , '\1_'                            , 'g')
                let str = substitute(str, '\(\d\)_', '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '\(n\)_' , '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '#\(_\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '#\(!\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
                let str = substitute(str, '#\(#\)' , '\="#n".char2nr(submatch(1))."!"', 'g')
		let line = str
                let counter=0
                while counter<replacementCount
                        let line = substitute(line, escape(replacementsFrom[counter], '$*.[]\^~'), "#".counter."!", "g")
                        let counter=counter+1
                endwhile
                let counter=0
                while counter<replacementCount
			if (type(replacementsTo[counter])==type([]))
				if (""!=matchstr(line, "#".counter."!"))
					if (!exists("rotationcounters[counter]"))
						let rotationcounters[counter]= 0
					endif
					let line = substitute(line, "#".counter."!", escape(replacementsTo[counter][rotationcounters[counter]], '$*.[]\^~&'), "g")
					let rotationcounters[counter] = (rotationcounters[counter] + 1) % (len(replacementsTo[counter]))
				endif
			else
				let line = substitute(line, "#".counter."!", escape(replacementsTo[counter], '$*.[]\^~&'), "g")
			endif
			let counter=counter+1
                endwhile
                let line = substitute(line, '#n\(\d\d*\)!', '\=nr2char(submatch(1))', 'g')
                call setline(idx, line)
                let idx = idx + 1
        endfor
endfunction

