""""""""""""""""""""""""""""""""""""""""""""""""""
" Classbrowser Menu
" 
" Version: $Revision: 1.9.1 $
" Date   : Juni 2001
"
" Author : Matthias Veit <matthias_veit@yahoo.de>
"""""""""""""""""""""""""""""""""""""""""""""""""""


function! s:RubyInit()
ruby << RUBYBLOCK

###########################################
# statics used to setup
# 
# change this for your own needs
SessionMenue="Session&Classes"
ClassMenue="Class&Members"
MAXCLASSES = 40
MAXTYPES = 10

###########################################
# for debugging purposes
def debug(message)
	VIM.evaluate("input(\">>>#{message}<<<\")")
end
###########################################
# Valueholder for all entry relevant stuff
class XEntry
  attr_accessor :name
  attr_accessor :line
  attr_accessor :type
  def initialize(type, name, line)
	@type = type
	@name = name
	@line = line.to_i
  end
  def <=>(object)
	return @name<=>object.name
  end
end

###########################################
# Class
#   -modifytime is saved, update only after change
#   -menu saves menupos 
class XClass
  attr_accessor :elements
  attr_accessor :name
  attr_accessor :classname
  attr_accessor :menu
  attr			:mtime
  @@ctags={
	"class" => "Class",
	"interface" => "Interface",
	"field" => "attribute.",
	"member" => "attribute."
  }
  ###########################################
  # init values
  def initialize(name)
	@name = name
	@elements = Hash.new
	@classname = File.basename(name).sub(/\.\w*/,"")
	@menu = nil
	@mtime=0
	update()
  end
  ###########################################
  # ctags names => my names
  def ctagsname(name)
	if (@@ctags[name].nil?)
	  return name
	else
	  return @@ctags[name]
	end
  end
  ###########################################
  # update, if file has changed
  def update
	modtime = File.stat(@name).mtime
	if(modtime!=@mtime)
	  @elements.clear
	  unique = Hash.new
	  IO.readlines("| ctags -x --c++-types=cdefgmnpstuv  --java-types=cfmi #{@name}").each { |line|
		(name, type, line) = line.split(/\s+/)
		type = ctagsname(type)
		#look for overloaded names
		if (unique.has_key?(type+name))
		  olcnt=2
		  olcnt+=1 while (unique.has_key?(type+name+"(#{olcnt})"))
		  name = "#{name}(#{olcnt})"
		end
		unique[type+name] = true
		elements[type] = Array.new if (!elements.has_key?(type))
		xtype = elements[type] 
		xtype.push(XEntry.new(type, name, line))
	  }
	  @mtime = modtime
	end
  end
  ###########################################
  # give Entry of given line
  def getEntry(line)
	found = XEntry.new(nil,nil,0)
	@elements.values.each{ |typelist|
	  typelist.each { |xentry|
		if (xentry.line<=line and xentry.line>found.line)
		  found = xentry
		end
	  }
	}
	if (found.line!=0)
	  return found 
	else
	  return nil
	end
  end
  ###########################################
  # comparator
  def <=>(object)
	return @classname<=>object.classname
  end
end

###########################################
# Session (static) provides ui
class XSession

  #instance variables
  @xclasses 

  ###########################################
  # Singleton interface
  def XSession.getInstance
	return @@singleton
  end

  def initialize()
	  @xclasses = Array.new
  end

  ###########################################
  # get XClass-object for given file (Hash replacement)
  def getXClass(file)
	@xclasses.each{ |xclass|
	  if (xclass.name == file)
		return xclass
	  end
	}
	return nil
  end

  ###########################################
  # returns a preamble for shortened menues
  def getPreamble(name)
  return case name.upcase
	when (/^[ABCD]/) 
	  "abcd."
	when (/^[EFGH]/) 
	  "efgh."
	when (/^[IJKL]/) 
	  "ijkl."
	when (/^[MNOP]/) 
	  "mnop."
	when (/^[QRST]/) 
	  "qrst."
	when (/^[UVWXYZ]/) 
	  "uvwxyz."
	else 
	  ">>>."
	end
  end

  ###########################################
  # draws the menu with specified content
  def drawmenu(xclass, sessiononly=false)
	classpreamble = ""
	classpreamble = getPreamble(xclass.classname) if (@xclasses.size>MAXCLASSES)
	xclass.elements.keys.sort.each{ |type|
	  typelist = xclass.elements[type]
	  typelist.sort.each{ |xentry|
		preamble = ""
		preamble = getPreamble(xentry.name) if (typelist.size>MAXTYPES)
		VIM::command("amenu 300 #{ClassMenue}.#{type}\\ #{preamble}\\ #{xentry.name} #{xentry.line}G") if (not sessiononly)
		VIM::command("amenu 200.#{xclass.menu} #{SessionMenue}.#{classpreamble}#{xclass.classname}.#{type}\\ #{preamble}\\ #{xentry.name} :silent! e! +#{xentry.line} #{xclass.name}<cr>")
	  }
	}
  end
  
  ###########################################
  # reorders to archieve alphabetical order
  def reOrder
	VIM::command("silent! aunmenu #{SessionMenue}")
	pos=0
	@xclasses.each { |xclass|
	  pos += 10
	  xclass.menu= pos
	  drawmenu(xclass, true)
	}
  end
  
  ###########################################
  # adds a class to the session
  def addClass(file)
	xclass = XClass.new(file)
	@xclasses.push(xclass)
	@xclasses.sort!
	if (@xclasses.size==(MAXCLASSES+1)) #reorder
	  reOrder
	else #look if there are spare places
	  index = @xclasses.index(xclass)
	  if (@xclasses.size == 1)
		  reOrder 
	  elsif (index == 0) #beginning
		xclass.menu = @xclasses[1].menu-1
		if xclass.menu<0
		  reOrder 
		end
	  elsif (@xclasses.last == xclass)
		xclass.menu = @xclasses[index-1].menu+10
	  else
		if (@xclasses[index-1].menu+1 == @xclasses[index+1].menu)
		  reOrder
		else
		  xclass.menu = @xclasses[index-1].menu+1
		end
	  end
	end
	drawmenu(xclass, false)
  end
  
  ###########################################
  # removes a class from the session
  def removeClass(file)
	xclass = getXClass(file)
	@xclasses.delete(xclass)
	if (@xclasses.size==(MAXCLASSES)) #reorder
	  reOrder
	elsif (@xclasses.size==(0)) #delete menu
	  VIM::command("silent! aunmenu #{SessionMenue}")
	else
	  classpreamble = ""
	  classpreamble = getPreamble(xclass.name) if (@xclasses.size>MAXCLASSES)
	  VIM::command("silent! aunmenu #{SessionMenue}.#{classpreamble}#{xclass.classname}")
	end
  end

  ###########################################
  # update menu
  def showClass(file)
	xclass = getXClass(file)
	if (xclass.nil?)
	  addClass(file)
	else
	  xclass.update
	  drawmenu(xclass, false)
	end
  end

  ###########################################
  # set statusline with current entry
  def setStatusLine
	strentry = ""
	xclass = getXClass($curbuf.name)
	if (!xclass.nil?)
	  (row, col) = $curwin.cursor
	  entry = xclass.getEntry(row)
	  strentry = "%{'[#{entry.name}\\\ (#{entry.type})]'}".sub(/\./,"") if (!entry.nil?)
	end
	VIM.command("set statusline=%<%f%h%m%r%=#{strentry}\\\ \\\ %l,%c%V\\\ %P")
  end

  ###########################################
  # enable StatusLine
  def enableStatusLine(bool)
    VIM.command("augroup classbrowser")
	if (bool)
	  VIM.command("autocmd! CursorHold * ruby XSession.getInstance.setStatusLine")
	  VIM.command("set updatetime=1000")
	else
	  VIM.command("autocmd! CursorHold * ")
	  VIM.command("set statusline=")
	  VIM.command("set updatetime=4000") #default=4000
	end
	VIM.command("augroup END")
  end

  ###########################################
  # initial behaviour: show all session classes
  def sessionInit()
	0.upto(VIM::Buffer.count-1) { |x|
	  if (getXClass(VIM::Buffer[x].name).nil?)
		@xclasses.push(XClass.new(VIM::Buffer[x].name))
	  end
	}
	reOrder
  end

  #class variables
  @@singleton = XSession.new
end
RUBYBLOCK
endfunction
	
"show current class
function! s:Update()
	if (exists("g:menu_gui_enabled"))
		ruby VIM::command("silent! aunmenu #{ClassMenue}")
		ruby XSession.getInstance.showClass(VIM::Buffer.current.name)
	endif
endfunction

"eval ruby code only once
function! s:InitGUI()
	let g:menu_gui_enabled = 1
	:call <SID>RubyInit()
endfunction

"new class is loaded
function! s:AddSession()
	if (exists("g:menu_gui_enabled"))
		ruby XSession.getInstance.addClass(VIM::Buffer.current.name)
	endif
endfunction

"remove class from session
function! s:DeleteSession()
	if (exists("g:menu_gui_enabled"))
		ruby XSession.getInstance.removeClass(VIM::Buffer.current.name)
	endif
endfunction

"sledge hammer for start
function UpdateSession()
	if (exists("g:menu_gui_enabled"))
		ruby XSession.getInstance.sessionInit()
	endif
endfunction

"enable status line
function EnableStatusline()
	if (exists("g:menu_gui_enabled"))
		ruby XSession.getInstance.enableStatusLine(true)
	endif
endfunction

"disable status line
function DisableStatusline()
	if (exists("g:menu_gui_enabled"))
		ruby XSession.getInstance.enableStatusLine(false)
	endif
endfunction

amenu 190.40.10 E&xtended.menu.statusline.enable :call EnableStatusline()<cr>
amenu 190.40.20 E&xtended.menu.statusline.disable :call DisableStatusline()<cr>
amenu 190.40.30 E&xtended.menu.update_session :call UpdateSession()<cr>

augroup classbrowser
	autocmd GUIEnter * call <SID>InitGUI()
	autocmd GUIEnter * call UpdateSession()
	autocmd BufEnter * call <SID>Update()
	autocmd BufAdd * call <SID>AddSession()
	autocmd BufWritePost * call <SID>Update()
	autocmd BufDelete * call <SID>DeleteSession()
augroup END

