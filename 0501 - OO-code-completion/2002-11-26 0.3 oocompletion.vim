" written by emmanuel.touzery <emmanuel.touzery@wanadoo.fr>
" released under the VIM license.
" version: 0.3
" This plugin will provide functions to get "code completion" in vim.
" it will list all methods for the identifier under the cursor.
" when you start typing a method, it will tell you which methods of the
" relevant class start by what you typed.
" it works by using gd and ctags to get the class of the relevant object,
" and ctags to get the list of functions for this object.
" NOTE: this includes inheritance! functions of the class and the parents
" classes are listed
" normally also only public member functions are listed (TODO all methods
" are listed in C++ due to a parsing bug) (TODO be smart and show only static functions or everything
" but static functions)
" This plugin requires tags to be generated with certain parameters (it
" needs information about inheritance and access of members).
" You can use the GenerateTags command to generate tags for the current directory.
"
" for now this plugin only supports Java and C++. It should only support OO languages,
" though I guess it could be used for C structs maybe.
"
" any feedback is welcome.. it works for me (tm)
"
" REPORTING BUGS: I am using ctags 5.3.1. Please try upgrading if your version is older
" This plugin is untested under windows.


" TODO it's known that the plugin does not work if you ask for the list of functions
" IN THE DECLARATION. eg
" MyClass a;
" if you ask for the list of functions for a, you will get nothing. The reason is that
" gd doesn't move the cursor (we are already on the declaration!), therefore the plugin
" thinks that gd failed. I think it's acceptable because we would be interested in
" completion only when actually using the object, not when declaring it.
"
" TODO sometimes "gd" doesn't work and sends me on another line, but it's not
" the declaration. maybe try to be fool-proof about it and realise by
" myself that I should use ctags.
"
" TODO C++ access (public, private, protected) parsing doesn't always work. the reason
" is that ctags only looks at the implementation. so in the .cpp or the .h.
" if the implementation is in the .cpp, getting the access would require ctags to look in
" in the .h, which it does not do.. for now I think it's acceptable.. if someone has
" an idea to work around this..
" 
" TODO move the ruby code out of this file except for the printPossibleCompletions method: I have at least another plugin using this code and other people might want to reuse the class too.
" TODO differenciate static functions from non-static ones. in completion that's very useful (Class:: should only be completed by static methods and object:: should only be completed by non-static methods)
" TODO support C structs?
" TODO use the "tags" variable to know where to look for the tags file?
" TODO optimise (should be easy: only parse/instanciate in memory what I need, not all the tags..)
" TODO advanced stuff, like C++ template support, which is SURE not to work :O)
" TODO more languages. for now only C++ and Java work.

" call this command to generate the correct tag file.
" this command will ALWAYS generate the tag file, even if it's
" not necessary (if the tags are up-to-date)
command! -nargs=0 GenerateTags call s:GenerateTags()

" list the possible functions for the identifier under the cursor.
" it will first quickly display its class.
command! -nargs=0 PossibleFunctions call s:PossibleFunctions()

" list the possible completions for the beginning of identifier under
" the cursor. It's pretty simple: let's say you are typing:
" myObject->set
" and suddendly you wonder what you can set for this object. go on the
" set word, and call this command. It will first check the call of "myObject"
" and list all the public methods of this class that start by "set".
command! -nargs=0 PossibleCompletions call s:PossibleCompletions()

" well, since I had to do it, why not export it.. it echoes the class name
" of the identifier under the cursor. implementation: it first tries with "gd"
" then with :tag. it just fools around doing "b" and tries guessing the type.
command! -nargs=0 Type call s:TypeUnderCursor()

function! s:GenerateTags()
	silent! call system("ctags --file-scope=no -R --fields=+ia *")
endfunction

function! s:PossibleFunctions()
" 	echo strftime("%Y %b %d %X")
	call <SID>Type(expand("<cword>"))
	if s:type == ""
		return
	endif
	call <SID>PrintPossibleCompletions(s:type, "")
endfunction

function! s:PossibleCompletions()
	" beginning is what you typed, the beginning of the
	" name of the method. We will list all methods whose
	" name start by "beginning".
	let beginning = expand("<cword>")

	" var is the actual variable for which we'll have to
	" get the class name. we'll return a list of methods
	" of that class.
	let var = ""
	while ( (var == "") || (var == beginning) )
		execute "normal b"
		let var = expand("<cword>")
	endwhile
" 	echo beginning
" 	echo var
	call <SID>Type(var)
	if (s:type == "")
		return
	endif
	call <SID>PrintPossibleCompletions(s:type, beginning)
endfunction

function! s:TypeUnderCursor()
	call <SID>Type(expand("<cword>"))
endfunction

function! s:Type(var)
	let var = a:var

	" jump to the definition of the variable,
	" using gd or ctags.
	call <SID>JumpToDef(var)

	" check previous word
	execute "normal b"
	let type = expand("<cword>")
	" down there I test if the possible type is not the
	" variable itself, because for instance char **argv,
	" a "b" brings on **argv, and expand("<cword>") gives
	" argv...
	while ( (!<SID>IsType(type)) || (type == var) )
		execute "normal b"
		let type = expand("<cword>")
	endwhile

	" get back where the cursor was when i was called.
	call <SID>JumpBack()

	" informative for the user: display the type that we
	" discovered.
	echo type
	let s:type = type
endfunction

function! s:JumpToDef(var)
	let line = line(".")
	let col = col(".")
	execute "normal mw"
	execute "normal gd"
	let s:usingTag = 0
	if ( (col(".") == col) && (line(".") == line) )
" 		echo "gd failed"
		" actually gd did not necessarily failed.
		" it's possible that the use is its own declaration eg MyClass a;
		" and the function was called with the cursor on the declaration
		" itself.
		" but in that case we don't really care, we rarely want to type eg
		" MyClass a.test(), it would be more MyClass a;
		" so in this context we shouldn't care about completion.
		let s:usingTag = 1
		execute "tag"
		if search(a:var)
		endif
	endif
endfunction

function! s:JumpBack()
	if s:usingTag
		exe "pop"
	else
		execute "normal `w"
	endif
endfunction

function! s:IsType(type)
	return ( (a:type != "const") && ( a:type != "*const") && (a:type != "*") )
endfunction

" optimise...
function! s:PrintPossibleCompletions(type, beginning)
let s:type = a:type
let s:beginning = a:beginning
ruby <<EOF

# manages one tag.
class Tag
	attr_reader :scope
	attr_reader :name
	attr_reader :className
	attr_reader :type
	attr_reader :inherits
	attr_reader :access

	def initialize(name, file, type, line, scope, inherits, className, access)
		@name = name
		@file = file
		@type = type
		@line = line
		@scope = scope
		@inherits = inherits
		@className = className
		@access = access
	end

	# for debug.
	def to_s()
		return "tag, name : " + @name + ", file : " + @file + ", type : " + @type + ", line : " + ((@line==nil)?(""):(@line)) + ", scope : " + ((@scope == nil)?(""):(@scope)) + ", inherits : " + ((@inherits == nil)?(""):(@inherits)) + ", className : " + ((@className == nil)?(""):(@className)) + ", access : \"" + ((@access == nil)?(""):(@access)) + "\""
	end

	# here is a ctags line:
	# ENTRY_AUTH_KEYCHANGE	snmp/usm/SnmpUser.java	/^	public static final String ENTRY_AUTH_KEYCHANGE = ".6";$/;"	f	class:SnmpUser	access:default
	def Tag.getTagFromCtag(ctag_line)

		# ;\ separates the "extended" information
		# from the standard one.
		ctag_infos = ctag_line.split(";\"")
		
		ctag_infos_base = ctag_infos[0].split("\t")
		ctag_infos_ext = ctag_infos[1].split("\t")
		index = 2 # at 0 it's "", at 1 it's the tag type (c, m, f, ...)
		while (ctag_infos_ext[index] != nil)
			info = ctag_infos_ext[index].split(":")
			# possible optimisation: call chomp only
			# if it's REALLY the last identifier of the line,
			# not "just in case" like that.
			if (info[0] == "line")
				line = info[1].chomp
			end
			if (info[0] == "inherits")
				inherits = info[1].chomp.split(",")
			end
			if ( (info[0] == "class") || (info[0] == "interface") )
				className = info[1].chomp
			end
			if (info[0] == "access")
				access = info[1].chomp
			end
			index = index + 1
		end
		# since there is no ctag_infos_ext[index], there will
		# be a carriage return here.
# 		ctag_infos_ext[index-1].chomp!
		
		scope = ctag_infos_ext[3]
# 		if (scope != nil)
# 			scope.chomp!
# 		end
		return Tag.new(ctag_infos_base[0].chomp, ctag_infos_base[1].chomp, ctag_infos_ext[1].chomp, line, scope, inherits, className, access)
	end

	# is this tag a method? (language dependant)
	# (do it in the constructor and cache it?)
	def tagMethod?()
		lang = language()
		return (@type == "m") if (lang == "java")
		return (@type == "f") if (lang == "cpp")
	end

	# language for this tag (do it in the constructor and cache it?)
	def language()
		return "java" if (@file =~ /java\Z/ )
		return "cpp" if ( (@file =~ /cpp\Z/) || (@file =~ /cc\Z/) || (@file =~ /h\Z/) || (@file =~ /hpp\Z/) )
	end

end

class TagList
	def initialize()
		# will contain all the tags of the tag file.
		@tags = Array.new()
# 		@classByName = Hash.new()

		# will contain an Array<String> containing
		# names of classes for which there is no tag
		# and that I do not have to look for again
		# in the future. eg if you use QT, you might
		# not have the tag info for all QT classes..
		# this is for performance (see classByName)
		@blacklist = Array.new()
	end

	# parsing. TODO: parse only what I need..
	# currently this is very slow.. I had up to
	# 12s when trying on kword source code...
	def addTags()
		file = File.open("tags")

		# now we parse the ctags output
		file.each_line { |ctags_line|

			# skip comments in the ctags file
			if (ctags_line =~ "^!_")
				next
			end
			tag = Tag.getTagFromCtag(ctags_line)
# 			puts tag.to_s
			@tags.push(tag)
		}
		file.close
	end

	# list public methods of this taglist that
	# have the class name you give and which (the methods)
	# names start with "beginning".
	def listMethods(className, beginning)
		puts "****** now listing methods."
# 		indexa = 0
		classSearched = classByName(className)
		@tags.each { |tag|
#			if (tag.type != "c " && tag.name != tag.className && indexa < 50 && tag.className != "")
# 				puts "considering " + tag.to_s
#				indexa = indexa + 1 
#			end
			if ( (tag.tagMethod?()) \
			&& (tag.name =~ "^#{beginning}") \
			&& ( (tag.className == className) || inheritance?(classSearched, tag.className) ) )
#				puts "considering " + tag.to_s
				# only display public methods:
				# the "" and nil are WORKAROUNDS for bugs in C++ parsing for now.
				if ( (tag.access == "public") || (tag.access == "") || (tag.access == nil) )
					puts tag.name + " [" + tag.className + "]"
				end
			end
		}
	end

	# get a Tag object from this TagList and a class name string.
	def classByName(className)
		# we keep a blacklist of classes. For instance
		# if you are using say QT, you'll have constant references
		# to QObject, that is not in your tag files.
		# to avoid checking all the tags looking for this class
		# each time that I'm asked for it, I just blacklist it once for all.
		# this happened with KoRect in kword, and the time went from
		# 12 seconds to instantaneous...
		return nil if @blacklist.include?(className)
		@tags.each { |tag|
			# the "c" is for "class" and the "i" is for
			# java interfaces.
			if ( ( (tag.type == "c") || (tag.type == "i") )\
					 && (tag.name == className) )
# 				@classByName[className] = tag
				return tag
			end
		}
		# if we are here, we failed to find the tag for className.
		# warn the user
		puts "*** WARNING: no tag information for #{className}. Ignoring it, but the function list might be incomplete."
		# and blacklist the tag, to avoid to walk the tag list again
		# for it later.
		@blacklist.push(className)
		return nil
	end

	# returns true if A inherits B
	def inheritance?(tagA, classB)
		# it's NOT a bug if tagA is nil.
		# that just will mean that classByName() returned
		# nil when calling me recursively (see last line of this
		# method), and it's fair, if for instance you inherit from
		# QObject but don't have the tag file for it...
		# we warn the user in classByName, no reason to crash
		# one second later for it.
		return false if (tagA == nil)
		return false if (tagA.inherits == nil)

		if (tagA.inherits.include?(classB))
# 			puts "##"+ classA + "DOES inherit from " + classB
			return true
		end
		if (tagA.inherits == nil)
			return false
		end
		# that's recursive. eg if A->B->C, A *does*
		# inherit from C..
		tagA.inherits.each { |subClassName|
			if (inheritance?(classByName(subClassName), classB))
				return true
			end
		}
		return false
	end

end

# puts "ruby invoked : " + Time.now.min.to_s + ":"+ Time.now.sec.to_s
taglist = TagList.new()
taglist.addTags()
taglist.listMethods(VIM::evaluate("s:type"), VIM::evaluate("s:beginning"))
EOF
endfunction
