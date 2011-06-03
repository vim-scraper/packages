" written by emmanuel.touzery <emmanuel.touzery {at} wanadoo {dot} fr> and Luc Hermitte
" <hermitte {at} free {dot} fr>
" released under the VIM license.
" version: 0.4
" This plugin will provide functions to get "code completion" in vim.
" it will list all methods for the identifier under the cursor.
" when you start typing a method, it will tell you which methods of the
" relevant class start by what you typed.
" it works by using gd and ctags to get the class of the relevant object,
" and ctags to get the list of functions for this object.
" NOTE: this includes inheritance! functions of the class and the parents
" classes are listed
" normally also only public member functions are listed (TODO all methods
" are listed in C++ due to a ctags limitation) (TODO be smart and show only static functions or everything
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

" TODO {{{1
" TODO it's known that the plugin does not work if you ask for the list of functions
" IN THE DECLARATION. eg
" MyClass a;
" if you ask for the list of functions for a, you will get nothing. The reason is that
" gd doesn't move the cursor (we are already on the declaration!), therefore the plugin
" thinks that gd failed. I think it's acceptable because we would be interested in
" completion only when actually using the object, not when declaring it.
"
" keep timestamps for tags?
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
" TODO optimise (should be easy: only parse/instanciate in memory what I need, not all the tags..)
" TODO advanced stuff, like C++ template support, which is SURE not to work :O)
" TODO more languages. for now only C++ and Java work.


" Commmands {{{1
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

" Functions {{{1

function! s:GenerateTags() " {{{2
	silent! call system("ctags --file-scope=no -R --fields=+ia *")
endfunction

function! s:PossibleFunctions() " {{{2
" 	echo strftime("%Y %b %d %X")
	let type = s:Type(expand("<cword>"))
	if type != ""
		call s:PrintPossibleCompletions(type, "")
	endif
endfunction

function! s:PossibleCompletions() " {{{2
	" beginning is what you typed, the beginning of the
	" name of the method. We will list all methods whose
	" name start by "beginning".
	let beginning = expand("<cword>")

	" var is the actual variable for which we'll have to
	" get the class name. we'll return a list of methods
	" of that class.
	let var = ""
	while ( (var == "") || (var == beginning) )
		normal! b
		let var = expand("<cword>")
	endwhile
" 	echo beginning
" 	echo var
	let type =s:Type(var)
	if (type != "")
		call s:PrintPossibleCompletions(type, beginning)
	endif
endfunction

function! s:TypeUnderCursor() " {{{2
	call s:Type(expand("<cword>"))
endfunction

function! s:Type(var) " {{{2
	let var = a:var

	let line = line(".")
	let col = virtcol(".")

	" jump to the definition of the variable,
	" using gd or ctags.
	call s:JumpToDef(var, line, col)

	" check previous word
	normal! b
	let type = expand("<cword>")
	" down there I test if the possible type is not the
	" variable itself, because for instance char **argv,
	" a "b" brings on **argv, and expand("<cword>") gives
	" argv...
	while ( (!s:IsType(type)) || (type == var) )
		normal! b
		let type = expand("<cword>")
	endwhile

	" get back where the cursor was when i was called.
	call s:JumpBack(line, col)

	" informative for the user: display the type that we
	" discovered.
	echo type
	return type
endfunction

function! s:JumpToDef(var, curLine, curVirtCol) " {{{2
	normal! gd
	let s:usingTag = 0
	if ( (virtcol(".") == a:curVirtCol) && (line(".") == a:curLine) )
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

function! s:JumpBack(line, virtCol) " {{{2
	if s:usingTag
		exe "pop"
	else
		exe a:line.'normal! '.a:virtCol.'|'

	endif
endfunction

function! s:IsType(type) " {{{2
	" thanks to Luc Hermitte for suggesting looking for '&' too.
	return ( (a:type != "const") && ( a:type != "*const") && (a:type != "*") && (a:type != "&") )
endfunction

" optimise...
function! s:PrintPossibleCompletions(type, beginning) " {{{2
let s:type = a:type
let s:beginning = a:beginning
ruby<<EOF
def tagFiles()
	# get the 'tags' VIM variable
	tagsVar = VIM::evaluate("&tags") #"./tags,/"

	# if the user does not have his/her
	# 'tags' variable set :
	if ( (tagsVar == "") || (tagsVar == nil) )
		# we provide a reasonnable default :
		tagsVar = "./tags,/"
	end

	curDir = curVimDir()
	# As the current directory may contain spaces, we need to reintroduce
	# backspaces in the current directory.
	curDir.gsub!(/ /, '\\ ')
	
	# replace eg ./tags by `pwd`.tags
	# this is useful for the uniq! done
	# at the end of this function.
	# i don't want to parse once `pwd`/tags
	# and once ./tags...
	tagsVar.gsub!(/^\./, curDir)

	# we split by " " but NOT by "\ ",
	# which is a valid space in a filename
	result = tagsVar.scan(/(?:\\ |[^,; ])+/)

	# now we remove the "\ ", ruby
	# otherwise later when we replace
	# all the "\" by "/" for the windows
	# paths, all is messed up.
	result.each { |tr|
		tr.gsub!(/\\ / ," ")

		# in here i want only paths with "/"
		# separators. that's the ruby way
		# (hey, i didn't say i think it's a good
		# idea :O/)
		tr.gsub!(/\\/, "/")
	}

	# we support the "/" trick: if 'tags'
	# include "/", then we check all the files
	# in directories under us.
	if result.include?("/")
		result.delete("/")
		# now we must add "pwd/tags" and all subdirs
# 		# we start at the dir under us.
# 		curDir = dirUp(Dir.pwd)
		# current directory
		curDir = curVimDir() 
		while (!File.rootDir?(curDir))
			result.push(curDir + "/tags")
			curDir = File.dirUp(curDir)
		end
		# we didn't add rootdir/tags yet
		result.push("#{curDir}/tags")
	end
	# remove duplicate dirs
	result.uniq!

	return result
end

# return the current directory.
# depending on cpoptions, this might
# be the path of the current file, or
# the current directory itself (and those
# might of course be different)
def curVimDir()
	# is pwd(), if 'cpoptions' contains 'd'
	if (VIM::evaluate('&cpoptions').include?(?d))
		curDir = Dir.pwd
	else
		curDir = VIM::evaluate("expand('%:p:h')")
	end
	return curDir
end

# some more methods for the class
# file...
class File
	# will return path itself if path
	# is the root of the drive.
	# requires "/" as the dir separator,
	# even on windows (do a gsub to fix it
	# if needed)
	def File.dirUp(path)
		# remove final "/" if there is one
		cleanPath = path.chomp(File::SEPARATOR)
		return path if (File.rootDir?(path))
		File.split(cleanPath)[0]
	end

	# is this dir the root dir?
	# requires "/" as the dir separator,
	# even on windows (do a gsub to fix it
	# if needed)
	def File.rootDir?(path)
		# remove final "/" if there is one
		cleanPath = path.chomp(File::SEPARATOR)
		# UNIX root dir:
		return true if path == "/"
		# windows network drives \\machine\drive\dir
		# we're at the root if it's something like
		# \\machine\drive
		return true if cleanPath =~ %r{^//\w+/\w+$}
		# now standard windows root directories
		# (a: c: d: ...)
		return true if cleanPath =~ /^[a-zA-Z]:$/
		return false
	end
end

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

	# for now "==" is not defined for speed (i often do comparisons
	# will nil)

	# I need a hash method because Array.uniq uses it
	# to remove duplicate elements and I want that duplicate
	# elements are properly accounted for...
	def hash
		return @name.hash() + @type.hash()
	end

	# http://165.193.123.250/book/ref_c_object.html#Object.hash
	# "must have the property that a.eql?(b) implies a.hash == b.hash."
	# without this, Array.uniq doesn't work properly.
	def eql?(other)
		return hash == other.hash
	end

	# here is a ctags line:
	# ENTRY_AUTH_KEYCHANGE	snmp/usm/SnmpUser.java	/^	public static final String ENTRY_AUTH_KEYCHANGE = ".6";$/;"	f	class:SnmpUser	access:default
	def Tag.getTagFromCtag(ctag_line, knownTags)

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
		result = Tag.new(ctag_infos_base[0].chomp, ctag_infos_base[1].chomp, ctag_infos_ext[1].chomp, line, scope, inherits, className, access)
		# if the tag is already known..
# 		if (knownTags.include?(result))
# # 			puts "already known tag"
# 			# don't parse it again
# 			return nil
# 		end
		return result
	end

	# is this tag a method? (language dependant)
	# (do it in the constructor and cache it?)
	def tagMethod?()
		lang = language()
		return (@type == "m") if (lang == "java")
		return (@type == "f") if (lang == "cpp")
	end

	# is this tag defining a class? (language dependant)
	def tagClass?()
		return (@type == "c") || (@type == "i")
	end

	# language for this tag (do it in the constructor and cache it?)
	def language()
		return "java" if (@file =~ /java\Z/ )
		return "cpp" if ( (@file =~ /cpp\Z/) || (@file =~ /cc\Z/) || (@file =~ /h\Z/) || (@file =~ /hpp\Z/) )
	end

end

class TagList
	def initialize()

# TODO split normal tags and class tags.

		# will contain all the tags of the tag file.
		@nonClassTags = Array.new()
		@classTags = Array.new()
		@matchingTags = Array.new()
# 		@classByName = Hash.new()

		# will contain an Array<String> containing
		# names of classes for which there is no tag
		# and that I do not have to look for again
		# in the future. eg if you use QT, you might
		# not have the tag info for all QT classes..
		# this is for performance (see classByName)
		@blacklist = Array.new()

		# will contain the name of all the classes that were
		# not found in the researches through the directories
		# may contain duplicate names
		# may contain classes that were not found in one directory
		# and found in another. you'll have to do a "detect" at
		# the end to find the relevant ones.
		@missingClasses = Array.new()
	end

	# parsing. TODO: parse only what I need..
	def parseTags(tagFile)

		# if we want to keep all info
		# in memory, don't remove the already
		# parsed tags. otherwise, remove them,
		# we'll keep the relevant stuff in
		# matchingTags.
		if (!$keepAllInfo)
			@nonClassTags = Array.new
		end

		file = File.open(tagFile)

		# now we parse the ctags output
		file.each_line { |ctags_line|

			# skip comments in the ctags file
			next if (ctags_line =~ "^!_")

			tag = Tag.getTagFromCtag(ctags_line, @matchingTags)
# 			puts tag.to_s
# 			if (tag != nil)
			if (tag.tagClass?())
				@classTags.push(tag)
			else
				@nonClassTags.push(tag)
			end
		}
		file.close
	end
	
	# list public methods of this taglist that
	# have the class name you give and which (the methods)
	# names start with "beginning".
	def listMethods(className, beginning)
		tagFiles().each { |curFile|
			next if (!FileTest.exist?(curFile))
			searchForMethods(curFile, className, beginning)
			#puts "black list size " + @blacklist.size.to_s + " " + @blacklist[0]
			@missingClasses += @blacklist
			@blacklist.clear
		}
	end

	def putsMethods()
		@missingClasses.uniq!
		@missingClasses.each { |className|
			item = @matchingTags.detect { |tag|
				(tag.name == className)
			}
			if (item == nil)
				puts "WARNING: did not find definition of " + className
			end
		}
 		@matchingTags.uniq!
		@matchingTags.each { |tag|
			if ( tag.tagMethod?() && ( (tag.access == "public") \
					|| (tag.access == "") \
					|| (tag.access == nil) ) )
				puts tag.name + " [" + tag.className + "]"
			end
		}
	end

	def searchForMethods(tagFile, classSearchedName, beginning)
		if (VIM::evaluate("&verbose").to_i > 0)
			puts "parsing tags for " + tagFile
		end
		parseTags(tagFile)
		#puts "call classByName #1"
		if @classSearched == nil
			@classSearched = classByName(classSearchedName)
		end
		@nonClassTags.each { |tag|
			# the goal is to catch classes because i'll
			# need the classes info,
			# when i'll go down the dir hierarchy.

			if ( ( (tag.className == classSearchedName) || inheritance?(@classSearched, tag.className) ) && (tag.name =~ "^#{beginning}") )
#				puts "considering " + tag.to_s
				# only display public methods:
				# the "" and nil are here because of insufficient info from ctags for C++
				# TODO i can strip one of "" or nil out i guess.
# 				if ( (tag.access == "public") \
# 						|| (tag.access == "") \
# 						|| (tag.access == nil) )
# 					puts tag.name + " [" + tag.className + "]"
					@matchingTags.push(tag)
# 				end
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
		@classTags.each { |tag|
			if (tag.name == className)
#				@classByName[className] = tag
				#puts "classByName: found " + tag.name
				return tag
			end
		}
		
		# if we are here, we failed to find the tag for className.
		# warn the user
# 		puts "adding the class " + className + " in the blacklist"
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

# this file separator API is badly broken
# or I missed something..

# puts "ruby invoked : " + Time.now.min.to_s + ":"+ Time.now.sec.to_s
$keepAllInfo = false 
taglist = TagList.new()
taglist.listMethods(VIM::evaluate("s:type"), VIM::evaluate("s:beginning"))
taglist.putsMethods()
EOF
endfunction
" ======================================================================
" vim600: set fdm=marker:
