##
## vimproject.py
##
## Mike Wilson, 2001-2005, mrw@whisperingwind.co.uk
##
## A Python script to create nexted menus in gvim to open files in a
## project for editing. A crude but useful way to quickly open files
## in a project, particularly those several directories deep -- saves
## a lot of typing.
##
## Requires gvim compiled with Python support. Ubunto Linux comes with
## a suitable package in the standard distro: vim-python.
##
## The project is described in an XML file "vimproject.xml". You
## should have received an example of the project file along with this
## script. This script parses that file and creates a "Project" menu
## on gvim's menu bar.
##
## (C) Copyright 2001-2005, Mike Wilson, Wrexham, Wales, UK
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Library General
## Public License along with this library; if not, write to the
## Free Software Foundation, Inc., 59 Temple Place - Suite 330,
## Boston, MA  02111-1307  USA.
##
## History
##
## January 2001	First version
## May 2005	Amended to use xml.sax in place of xmllib which is
##     		deprecated.
##

import os.path
import string
import glob
from xml.sax import make_parser, handler
import vim

##
## A VimNode represents one node in the project menu hierarchy: it's
## either the top-level menu or a sub-menu.
##

class VimNode:
    def __init__ (self, parent, name):
	self.clear ()
	self.parent = parent
	self.name = name

    ##
    ## Clear this node. Removes its attributes and sub-branches.
    ##

    def clear (self):
	self.globList = []
	self.regexList = []
	self.branchList = []

    ##
    ## Add a glob filename matcher to this branch. Files matching this
    ## pattern will be added to the branch's menu when buildMenus is
    ## called.
    ##

    def addGlob (self, attributes):
	self.globList.append (attributes ["pattern"])

    ##
    ## TODO: regex filename matching isn't implemented yet.
    ##

    def addRegex (self, attributes):
	self.regexList.append (attributes ["pattern"])

    ##
    ## Add a sub-branch to this branch.
    ##

    def addBranch (self, node):
	self.branchList.append (node)

    ##
    ## Return this branch's parent (None if this is the top-level
    ## menu).
    ##

    def getParent (self):
	return self.parent

    ##
    ## Make a vim menu name for this branch. The top-level menu
    ## returns "Project", a sub-menu called "Spam" returns
    ## "Project.Spam". This is used in a vim menu command to create
    ## the menu.
    ##

    def getMenuPath (self):
	if self.parent:
	    return self.parent.getMenuPath () + "." + self.name
	else:
	    return self.name

    ##
    ## Build the menu for this branch and all its sub-branches.
    ##

    def buildMenus (self, files):
	self.buildGlobMenus (files)
	self.buildRegexMenus (files)

	for child in self.branchList:
	    child.buildMenus (files)

    ##
    ## Build menu items using a glob pattern.
    ##

    def buildGlobMenus (self, files):
	allPaths = []

	for pattern in self.globList:
	    allPaths = allPaths + glob.glob (pattern)

	allPaths.sort ()

	for path in allPaths:
	    self.addFile (path, files)

    ##
    ## TODO: regex filename matching isn't implemented yet.
    ##

    def buildRegexMenus (self, files):
	pass

    ##
    ## Add a file to a menu. Add a menu item to vim. The command for
    ## the menu will switch the edit session to that file.
    ##

    def addFile (self, path, files):
	if path not in files:
	    try:
		if os.path.isfile (path):
		    vim.command (self.makeEditCommand (path))

		files.append (path)
	    except vim.error:
		pass

    ##
    ## Build the vim command to add a menu which, when selected, will
    ## open the file at "path".
    ##

    def makeEditCommand (self, path):
	fileName = os.path.basename (path)
	command = "amenu " + self.getMenuPath () + "."
	command = command + string.replace (fileName, ".", "\\.")
	command = command + " :python vimEditFile (\"" + path + "\")<CR>"

	return command


##
## A branch which is a sub-menu of the top-level Project menu or of
## another sub-menu.
##

class VimBranch (VimNode):
    def __init__ (self, parent, attributes):
	VimNode.__init__ (self, parent, attributes ["name"])

##
## A Branch which is the top-level Project menu.
##

class VimProject (VimNode):
    def __init__ (self, projectPath = "vimproject.xml"):
	VimNode.__init__ (self, None, "Project")
	self.projectPath = projectPath

	try:
	    vim.command ("aunmenu Project")
	except vim.error:
	    pass

	if os.path.isfile (self.projectPath):
	    self.load ()

    ##
    ## Switch the edit session to the file "filePath". First looks for
    ## an existing buffer containing the file. If there isn't a buffer
    ## for the file, start a new session with :edit. (It works like
    ## that so the cursor position in an existing buffer is retained).
    ##

    def editFile (self, filePath):
	found = None

	for buffer in vim.buffers:
	    if buffer.name and os.path.exists (buffer.name):
		if os.path.samefile (buffer.name, filePath):
		    found = buffer.name

	if found:
	    vim.command ("buffer " + filePath)
	else:
	    vim.command ("edit " + filePath)

    ##
    ## Load the Project file and rebuild the menus.
    ##

    def load (self):
	self.clear ()
	parser = make_parser ()
	parser.setContentHandler (ProjectHandler (self))
	parser.parse ("vimproject.xml")
	self.loadMenus ()

    ##
    ## Load the Project menus from scratch. If any files have been
    ## added to the project, they will now be displayed on the
    ## appropriate menu.
    ##

    def loadMenus (self):
	vim.command ("amenu Project.Reload :python vimReloadProject()<CR>")
	vim.command ("amenu Project.- xyzzy")
	self.buildMenus ([])


##
## Exception class for errors in the vimproject.xml file.
##

class ProjectError (Exception):
    pass

##
## Helper class to parse the vimproject.xml file.
##

class ProjectHandler (handler.ContentHandler):
    def __init__ (self, project):
	self.project = project
	self.currentNode = None

    def startElement (self, name, attributes):
	if name == "project":
	    self.startProject (name, attributes)
	elif name == "branch":
	    self.startBranch (name, attributes)
	elif name == "glob":
	    self.startGlob (name, attributes)
	elif name == "path":
	    self.addPath (name, attributes)
	else:
	    raise ProjectError, "Invalid tag in project"

    def endElement (self, name):
	if name == "project":
	    self.endProject ()
	else:
	    if name == "branch":
		self.endBranch ()

    def startProject (self, name, attributes):
	self.mandatoryAttributes (name, attributes, ["title"])
	self.currentNode = self.project

    def endProject (self):
	self.currentNode = self.currentNode.getParent ()

    def startBranch (self, name, attributes):
	if self.currentNode is not None:
	    newBranch = VimBranch (self.currentNode, attributes)
	    self.currentNode.addBranch (newBranch)
	    self.currentNode = newBranch

    ##
    ## Callback for the </branch> end tag.
    ##

    def endBranch (self):
	self.currentNode = self.currentNode.getParent ()

    ##
    ## Callback for the <glob> start tag.
    ##

    def startGlob (self, name, attributes):
	if self.currentNode is not None:
	    self.currentNode.addGlob (attributes)

    def addPath (self, name, attributes):
	vim.command ("set path+=" + attributes ["add"])

    def mandatoryAttributes (self, name, attributes, mandatory):
	mandatoryCount = 0

	for attribute in attributes.keys ():
	    if attribute in mandatory:
		mandatoryCount = mandatoryCount + 1

	##
	## TODO: Find the length of a list!
	##

	##if mandatoryCount != size (mandatory):
	    ##raise ProjectError, "Missing attribute"


__currentProject = VimProject ()

##
## Open the project: Called from vim, preferably the $HOME/.gvimrc
## file.
##

def vimOpenProject ():
    __currentProject = VimProject ()

##
## Called from vim by the menus built by the project. Switch the edit
## session to "fileName".
##

def vimEditFile (fileName):
    __currentProject.editFile (fileName)

##
## Called from vim when the "Reload" menu option is selected.
##

def vimReloadProject ():
    __currentProject.load ()

