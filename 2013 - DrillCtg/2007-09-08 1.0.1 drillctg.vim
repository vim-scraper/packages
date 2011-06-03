"Vim global plugin for drill-down search of a ctags file
"Last change: 20070908
"Version: 1.0.1
"Author: Basil Shkara <basil at oiledmachine dot com>
"License: This file is placed in the public domain
"
"DrillCtg allows drill-down search by character across the pathnames in your current loaded ctags file.
"It is extremely fast once loaded.  
"Upon first launching DrillCtg, if a ctags file is currently set, it will parse it and create a Vim list
"(an array) which stays in memory throughout your session.  This is a sorted array which contains all
"unique pathnames, able to searched quickly using Vim's match() function.
"
"If a ctags file is not currently set upon launching DrillCtg, it will prompt you for the location of one.
"There may be a slight pause whilst Vim creates the array intially however subsequent calls to DrillCtg will 
"not be an issue as the array will already be in memory."
"
"If you set a different ctags file, upon the next call to DrillCtg, it will replace the contents of the old
"array with the new pathnames.
"
"Currently this plugin only supports 1 loaded ctags file at a time.  If you have more than 1 ctags
"file loaded, it is likely that this plugin will not work.
"
"Please direct all feature requests and bug reports to my email above.
"
"Installation:
"-------------
"1. Drop the plugin file (drillctg.vim) into your plugins directory.  Refer to the 'add-plugin', 'add-global-plugin' 
"and 'runtimepath' Vim help pages for more details about installing Vim plugins.
"2. Restart Vim to load DrillCtg.
"3. To toggle search box you can use the ":Drill" command to open/close the search box window.
"4. To show available commands type "?".
"
"Info on ctags file:
"-------------------
"Your generated ctags file may not contain all the files from your project as some files may be unsupported types.
"E.g. If you are generating a ctags file on a PHP project which also contains XML files, the XML files will not be
"appended to the ctags file.  Then when you are searching for a filename across your ctags file using DrillCtg,
"you will not have access to these other files.
"You can solve this issue easily by appending these files manually yourself using something like:
"$ find project_dir -name '*.xml' >> ctags_file
"Vim will not use the extra pathname information but DrillCtg will.


"check for Vim 7
if version < 700
	echo "\nDrillCtg requires Vim 7.0"
	echo "You currently have version: ".version
	echo "DrillCtg will not be active this session\n"
    finish
endif

"if plugin already loaded
if exists("loaded_drillctg")
	finish
endif
let loaded_drillctg = 1

"create ex command for toggling drill window
command! -nargs=0 -bar Drill call s:ToggleDrill()

function s:ToggleDrill()
    "if window is open then close it.
    let winnum = bufwinnr("DrillResults")
    if winnum != -1
        execute "bd!".bufnr("DrillResults")
		"restore vim settings
		let &incsearch = s:save_incsearch
		let &hlsearch = s:save_hlsearch
		unlet s:save_incsearch
		unlet s:save_hlsearch
        return
    endif

	"open window
	call s:LoadList()
	"local buffer keymappings
    map <silent><buffer> s :call <SID>SearchList()<CR>
    map <silent><buffer> c :call <SID>ClearDrillWindow()<CR>
	map <silent><buffer> ? :call <SID>HelpWindow()<CR>
	map <silent><buffer> <CR> <C-W>gf<CR> :execute "bd!".bufnr("DrillResults")<CR>
	
	"modify vim settings
	let s:save_incsearch = &incsearch
	let s:save_hlsearch = &hlsearch
	set incsearch
	set hlsearch
endfunction

function s:LoadList()
	"create list and open split if tags file exists
	if (strlen(&tags) > 0 && &tags != "./tags,tags")
		if (!exists("s:tag_list") || s:loaded_tagname != &tags)
			"load tags file
			execute "e ".&tags
	        set buftype=nofile 
	        set bufhidden=hide 
	        setlocal noswapfile 

			"remove tag_name<TAB>
			silent %s/^\w*\t//g
			"remove <TAB>ex_cmd<TAB>extension_fields
			silent %s/\s.*//g
			"remove any other junk
			silent %s/!_.*//g
			"sort the list alphabetically
			sort
			"remove blank lines
			silent %s/^[\ \t]*\n//g
			"remove duplicates 
			silent %s/^\(.*\)\(\n\1\)\+$/\1/
		
			"insert each line into list for indexing and searching
			let s:tag_list = getline("^", "$")
			"don't need buffer anymore
			bd!
			
			let s:loaded_tagname = &tags
			
			"show drill box
			call s:OpenDrillWindow()
		else
			"show drill box
			call s:OpenDrillWindow()
		endif		
	elseif !exists("s:tag_list")
		let s:choice = inputlist(['You have not yet loaded a ctags file.  Choose an option:', '1. Specify ctags file to load', '2. Cancel'])
		if s:choice == 1
			let s:userspec_tags = input("\nPlease enter the path of your ctags file: ", "", "file")
			execute ":set tags=".s:userspec_tags
			echo "\n".&tags." loaded"
			silent call s:LoadList()
		endif
	endif
endfunction

function s:OpenDrillWindow()
	15split DrillResults
	set buftype=nofile 
	set bufhidden=hide 
	setlocal noswapfile
endfunction

function s:HelpWindow()
	echo "\n<CR>:	Open file under cursor in new tab"
	echo "s:	Enter search query"
	echo "c:	Clear search window"
	echo "?:	Display this help text"
endfunction

function s:ClearDrillWindow()
	1,$d
endfunction

function s:SearchList()
	call s:ClearDrillWindow()
	let s:query = input("Query: ","", "file")
	for n in s:tag_list
		let s:match_results = match(n, s:query)
		if (s:match_results > 0)
			let s:append_results = append("", n)
		endif
	endfor
	"position cursor at top
	silent 1
endfunction
