" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/TagsParser.txt
1073
*TagsParser.txt*	Vim TagsParser Plugin		June 11, 2006

Author: A. Aaron Cornelius 

To contact you can email me at <ADotAaronDotCorneliusAtgmailDotcom> if you
deobfuscate my address.

==============================================================================
Copyright (C) 2006 A. Aaron Cornelius

This program is free software; you can redistribute it and/or
modify it under The terms of The GNU General Public License
as published by The Free Software Foundation; either version 2
of The License, or (at your option) any later version.

This program is distributed in The hope that it will be useful,
but WITHOUT ANY WARRANTY; without even The implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See The
GNU General Public License for more details.

You should have received a copy of The GNU General Public License
along with this program; if not, write to The Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
USA.

==============================================================================
0. TagsParser Overview			*tagsparser*

This plugin has two uses, management of dynamic tags files and display of tags
for an open file.  The use and configuration of these two features is
described in detail below.

1.  Tags Parser Installation		|tagsparser-install|
2.  Tags Parser Global Configuration	|tagsparser-config|
3.  Tags Parser Global Commands		|tagsparser-commands|
4.  Dynamic Tags Overview		|tagsparser-dynamictags|
5.  Dynamic Tags Usage			|tagsparser-dynamictags-usage|
6.  Dynamic Tags Configuration		|tagsparser-dynamictags-config|
7.  Dynamic Tags Commands		|tagsparser-dynamictags-commands|
8.  Tag Window Overview			|tagsparser-tagwindow|
9.  Tag Window Usage			|tagsparser-tagwindow-usage|
10. Tag Window Configuration		|tagsparser-tagwindow-config|
11. Tag Window Commands			|tagsparser-tagwindow-commands|
12. Depreciated Options			|tagsparser-depreciated-options|
13. Known Issues			|tagsparser-issues|
14. Changelog				|tagsparser-changelog|
15. Todo list				|tagsparser-todo|

==============================================================================
1. Tags Parser Installation		*tagsparser-install*

To install this plugin you simply need to ungzip and untar the contents of
the downloaded tarball into a directory that is located somewhere in your
|runtimepath|.  This is usually one of $HOME/.vim, $HOME/vimfiles,
$VIM/vimfiles, or $VIMRUNTIME.

If you are using Vim 7, and have downloaded the |vimball| file instead of the 
tarball file, simply source the vimball and it should install the plugin files 
for you.

To enable this documentation to be accessible via :help, run
:helptags <install_dir>/doc where the <install_dir> would be the directory
where you expanded the tarball.

					*tagsparser-install-more*
In addition to the TagsParser.vim file, you will need three other things to
enable this plugin to work properly.  You will need to have Perl installed,
Exuberant Ctags (http://ctags.sourceforge.net/).

Once these are installed you will need to ensure that the plugin can find the
ctags executable.  One way to do this is simply make sure your path is setup 
properly.  Otherwise you can specify these using the |TagsParserTagsProgram| 
global variable.

If you would like to enable Ada support for ctags (and this plugin), there is a
file included with this plugin which will enable Ada tags support in ctags.
The details of this are in the |tagsparser-ada-support| section, near the
|tagsparser-tagswindow-languages| section.

					*tagsparser-Perl-warning*
You need all three of these programs installed to use any feature of this
plugin.  If any one of them is not found the plugin will not load and it will
inform you of this fact.  If you have multiple versions of VIM using the same
.vimrc/runtimepath, you can force VIM to not complain about Perl not being
enabled at startup using the |TagsParserNoPerlWarning| global variable.

==============================================================================
2. Tags Parser Global Configuration	*tagsparser-config*

					*tagsparsertagsprogram*
TagsParserTagsProgram	The location of the Exuberant ctags program.  If this
			is not hardcoded the TagsParser plugin will attempt to
			find the proper find program, it may not succeed, but
			it is more likely to succeed than the find program
			automatic search.  This is because there is no windows
			component called "ctags" like there is for "find".  It
			is set like: >
	let g:TagsParserTagsProgram = "C:/cygwin/usr/local/bin/ctags"
<

TagsParserNoPerlWarning			*tagsparsernoperlwarning*
			This is an option you can set if you have the
			TagsParser plugin installed in your |runtimepath| but
			some versions of VIM that you may launch don't have
			Perl support compiled in.  This option being set to 1
			will prevent the TagsParser from complaining when the
			non-Perl supported VIM starts up.  This is usually only
			useful on a combined WIN32/Cygwin VIM setup. >
	let g:TagsParserNoPerlWarning = 1
<
					*tagsparseroff*
TagsParserOff		This option will let you disable all TagsParser 
			functionality so that upon initialization nothing is 
			done automatically.  When the plugin is off the tag 
			window cannot be opened, tags files will not be saved, 
			and you will not be able to tag a directory using the
			|:TagDir| command.  Non-tag related functions and 
			activities such as the |tagsparser-tagwindow-quickfix|
			commands and the |TagsParserLastPositionJump| will not 
			be affected and are still enabled.  The same affect 
			can be done on the fly using the |:TagsParserOff| 
			command. >
	let g:TagsParserOff = 1
<
TagsParserDisableLang_{filetype}	*tagsparserdisablelang_{filetype}*
			This option will let you prevent a language which is 
			currently supported by the TagsParser plugin from 
			being tagged or recognized by the plugin.  The 
			{filetype} option must be in the form that VIM 
			recognizes the filetype as.  So to disable C++ support 
			you would have to disable it like so: >
	let g:TagsParserDisableLang_cpp = 1
<
			Because "cpp" is the way that VIM sets the filetype 
			for C++ files.  This is usually not a big deal, except 
			for languages which ctags uses non-alphabetic 
			characters in the name of the language.  The only 
			languages that this is an issue for (at the moment) 
			are C++ (cpp) and C# (cs).  These language strings are 
			listed in the |tagsparser-language-table| section.  
			There are no default values for this class of 
			variables.

TagsParserLastPositionJump		*tagsparserlastpositionjump*
			This is a handy feature to enable, what it will do is 
			everytime you open up VIM, if you keep a viminfo file, 
			the cursor will jump to the last line and column the 
			cursor was at when you exited the file.  Additionally, 
			if you are editing multiple buffers, this will cause 
			the cursor to be placed at the same exact place in the 
			buffer as you were before exiting the buffer.  If this 
			is enabled, along with the 
			|TagsParserHighlightCurrentTag| option, it will cause 
			the correct tag to be highlighted immediately upon 
			entering the buffer (assuming that the tag window is 
			open, and a valid tag file exists for the file being 
			edited in the buffer).  To read more about this refer 
			to |last-position-jump| or |line()|.  The default 
			value for this flag is 0. >
	let g:TagsParserLastPositionJump = 0
<
==============================================================================
3. Tags Parser Global Commands		*tagsparser-commands*

					*:tagsparseroff*
:TagsParserOff		This command can be used to turn off all TagsParser
			behavior after VIM is started and the plugin has been
			installed.  When the plugin is off the tag window
			cannot be opened, tags files will not be saved, and
			you will not be able to tag a directory using the
			|:TagDir| command.  Non-tag related functions and 
			activities such as the |tagsparser-tagwindow-quickfix| 
			commands and the |TagsParserLastPositionJump| will not 
			be affected and are still enabled.  This works the 
			same way as the |TagsParserOff| configuration 
			variable.

					*<leader>tof*
<leader>tof		This is the default mapping installed which will call
			the |:TagsParserOff| command.

					*:tagsparseron*
:TagsParserOn		This command can be used to turn on all TagsParser
			behavior after VIM is started and the plugin has been
			installed, but the plugin has also been turned off.
			This can be either by the |:TagsParserOff| command or
			via the |TagsParserOff| configuration variable.  This
			re-enables all TagsParser functionality as described in
			this help file.

					*<leader>ton*
<leader>ton		This is the default mapping installed which will call
			the |:TagsParserOn| command.


==============================================================================
4. Dynamic Tags Overview		*tagsparser-dynamictags*

The dynamic tags portion of the TagsParser plugin was created to enable a
directory to be specified as a project directory, using the 
|TagsParserTagsPath| variable, and every file in that directory will be tagged 
dynamically.  So as soon as you save a file the tags for that file will be 
updated or created depending on whether the tag file existed or not.

This way the normal VIM tag search feature |tags-and-searches| will be able to
be used while always having up-to-date tags.  You can also specify static tag
files utilizing the standard |tags-option|.  This can be used to specify a set
of tags that you would like to reference but will not be changing very often
(if at all).

In this way you can have your project files updated as you change them, but
any supporting code, such as an OS, can still be referenced.  Since the tags
which change the least often can just be in 1 file as opposed to the many that
will be created for the project directory, it will reduce the overhead of
searching those files.

					*tagsparser-dynamictags-languages*
The dynamic tags portion of the TagsParser plugin supports all languages that
Exuberant Ctags supports.  If you have added your own languages in a format
that VIM can understand, the TagsParser should support any additions.

As mentioned in the |tagsparser-install-more| and
|tagsparser-tagswindow-languages| sections, there is more information about
Ada support in the |tagsparser-ada-support| section.

==============================================================================
5. Dynamic Tags Usage			*tagsparser-dynamictags-usage*

After you have properly installed the TagsParser plugin and any supporting
programs you are ready to use the dynamic tag functionality of the TagsParser.
To use the dynamic tags, the only thing you need to do is setup your project
directories using the |TagsParserTagsPath| global variable.  Once this is 
done, every file you save that is under that path (and meets any exclude or 
include criteria, more on that in a second) will automatically create or update 
a tag file.  If that tag file did not exist before now, it will be added to the
regular VIM |tags-option| so tag searching via the normal methods (|tag|, |ts|,
|<CTRL-]>|, |<CTRL-T>|, etc.) are supported.

If you would like to setup some sort of standard "library" type tags file with 
the same options as the TagsParser plugin uses to tag files, this is an 
example (aside from any user or language specific options that the you may 
enable via your .vimrc), of the options that TagsParser enables when it 
processes files. >
	ctags -f <full_path_tag_file> --format=2 <g:TagsParserCtagsOptions> 
	<g:TagsParserCtagsOptions_{filetype}> --excmd=p --fields=+nS \
	--sort=yes --tag-relative=yes <full_path_source_file>
<
					*tagsparser-ctags-spaces*
There is a problem I have found with ctags when it is compiled to use an 
external sort function.  When specifying a tag file, which has spaces in the 
file name or path name, and the sort option is on, Exuberant Ctags will 
produce an error.  This is because the tag file name to be sorted is not 
passed to the external sort program quoted.  If the TagsParser plugin produces 
errors when saving files or running the |TagDir| command this is likely the 
cause.

There are three ways to fix this problem:
1) Don't use spaces in file or directory names (obviously this is not the best 
'solution').
2) Fix the Exuberant Ctags source so that all filenames are passed to the sort 
program quoted.  This is more work, but has the benefit of using less memory 
and being faster than the Ctags internal sort.
3) Use Ctags internal sort.  This is the easiest option.  To use the internal 
Ctags sort algorithm, you must recompile the ctags program when supplying 
--disable-external-sort during the configure step.

					*tagsparser-include-exclude-usage*
You can automatically include or exclude files or directories under the tag
path by using these options: |TagsParserDirExcludePattern|,
|TagsParserFileExcludePattern|, |TagsParserDirIncludePattern|, and
|TagsParserFileIncludePattern|.  If an option is defined as empty ("") it is
considered disabled.  The exclude pattern is searched first so if you have
both include and exclude patterns defined, it will allow you to define an
exception to a standard include rule.  By default the include patterns are
left empty (""), and the exclude patterns are set to exclude some standard
types of temporary directories and files.

Directories:	.* (such as .vim)
		_* (such as _vim)
		tmp, TMP
		temp, TEMP
		backup, BACKUP

Files (case insensitive):
		*.o	(object file)
		*.obj	(object file)
		*.a	(static library file)
		*.so	(dynamic library file)
		*.d	(typical dependency file)
		*.bak	(typical backup file)
		*.swp	(swap file)
		*~	(Vim backup file)
		core	(core file from program crash)
		tags	(tag file)
		*.tags	(file specific tag file)
		*.txt	(text document)
		*.ali	(GNAT link information file)
		*.da	(GCOV data file)
		*.bb	(GCOV basic block file)
		*.bbg	(GCOV basic block file)
		*.gcov	(GCOV results file)

Previous versions of the TagsParser plugin utilized Perl-style regular 
expressions.  This is no longer the case, all include and exclude patterns 
utilize Vim-style regular expressions.

==============================================================================
6. Dynamic Tags Configuration		*tagsparser-dynamictags-config*

					*tagsparsertagspath*
TagsParserTagsPath	This is the option that defines the project
			directories.  These can be defined in any way that
			makes sense for your platform.  Special directory
			characters such as ~ are allowed.  This uses Vim path 
			characters so you can use ** to define subdirectory 
			searching along with other options.  See |path| for 
			more information, be sure to enclose your path in 
			quotes. Here are some examples:
			
			All directories under C:/Working and all files only in 
			the user's test directory: >
	let g:TagsParserTagsPath = "C:/Working/**,~/test"
<
			All directories named 'src' and 'inc' under 
			/vob/project: >
	let g:TagsParserTagsPath = "/vob/project/**/src,/vob/project/**/inc"
<
			Any directory 2 or less directories deep past /work: >
	let g:TagsParserTagsPath = "/work/**2/"
<
			If there are too many files or subdirectories defined 
			in this way, it may slow down tag searching.  To avoid 
			this make extensive use of the special vim characters 
			such as **, **[0-255], *, etc.

					*tagsparserctagsoptions*
TagsParserCtagsOptions	This option can be used to enable or disable global 
			Ctags functionality.  This could be something like 
			disabling the inclusion of file-scoped tags: >
	let g:TagsParserCtagsOptions = "--file-scope=no"
<
			Or disabling ctags from following symbolic links: >
	let g:TagsparserCtagsOptions = "--links=no"
<
			There are some options that are enabled by default to 
			aid the TagsParser plugin and these should not be 
			turned off, some won't matter to the behavior of the 
			TagsParser (such as "--sort=yes", it would make tag 
			searching by VIM slower though).  And some will make 
			a difference (such as "--fields=+nS", disabling either 
			option would break some or all of the TagsParser 
			functionality).  However, even if you include some of 
			these in your g:TagsParserCtagsOptions settings by 
			accident it should not make a difference since the 
			hard coded values are processed after the user 
			options.  The default for this option is empty. >
	let g:TagsParserCtagsOptions = ""
<

TagsParserCtagsOptions_{filetype}	*tagsparserctagsoptions_{filetype}*
			This option can be used to enable or disable filetype 
			specific Ctags functionality.  For example, if you 
			wanted to enable all types for C/C++ files (function 
			prototypes, external variables and local variables are 
			turned off by default), that could be done like this: >
	let g:TagsParserCtagsOptions_c = "--c-kinds=+pxl"
	let g:TagsParserCtagsOptions_cpp = "--c++-kinds=+pxl"
<
			Or, to turn on all Ada type processing: >
	let g:TagParserCtagsOptions_ada = "--ada-kinds=+PTUVRKOEay"
<
			Keep in mind, that disabling some types for languages 
			which have extended support may prevent some types 
			from being listed in the Tag Window.  If a type is not 
			displayed because it's "parent" type is disabled, it 
			will still be in the tag file and still be able to be 
			located using regular VIM tag commands (see |tags| or 
			|tagsparser-dynamictags-usage| for details about 
			regular VIM tag commands).  So if you disable the 
			tagging of C/C++ structures: >
	let g:TagsParserCtagsOptions_c = "--c-kinds=-s"
<
			Then you will not be able to see any member tags ("m") 
			that belong to any structures as long as nested tag 
			display is enabled (which is the default).  Refer to 
			|TagsParserNoNestedTags| for more information on how 
			to disable nested tag display.

			This option can also be useful where ctags won't 
			automatically recognize a file as the proper type but 
			VIM does.  By default, ctags does not recognize *.mk 
			files as Makefiles, but VIM does.  So one quick way to 
			make sure that your *.mk files get tagged properly is 
			to do this: >
	let g:TagsParserCtagsOptions_make = "--language-force=make"
<
			(Of course, you could just create a .ctags file in 
			your home directory and add this line to it: 
			'--langmap=make:.mk'.  But what fun would that be).

			There are no default values for this class of 
			variables.

TagsParserDirExcludePattern		*tagsparserdirexcludepattern*
			The default behavior of this is described in
			|tagsparser-include-exclude-usage|.  The default
			definition of this variable is: >
	let g:TagsParserDirExcludePattern =
		\ '.\+/\..\+\|.\+/_.\+\|\%(\ctmp\)\|' .
		\ '\%(\ctemp\)\|\%(\cbackup\)'
<

TagsParserFileExcludePattern		*tagsparserfileexcludepattern*
			The default behavior of this is described in
			|tagsparser-include-exclude-usage|.  The default
			definition of this variable is: >
	let g:TagsParserFileExcludePattern = 
		\ '^.*\.\%(\co\)$\|^.*\.\%(\cobj\)$\|' .
		\ '^.*\.\%(\ca\)$\|^.*\.\%(\cso\)$\|^.*\.\%(\cd\)$\|' .
		\ '^.*\.\%(\cbak\)$\|^.*\.\%(\cswp\)$\|^.\+\~$\|' .
		\ '^\%(\ccore\)$\|^\%(\ctags\)$\|^.*\.\%(\ctags\)$\|' .
		\ '^.*\.\%(\ctxt\)$\|^.*\.\%(\cali\)$\|^.*\.\%(\cda\)$\|' .
		\ '^.*\.\%(\cbb\)$\|^.*\.\%(\cbbg\)$\|^.*\.\%(\cgcov\)$'
<

TagsParserDirIncludePattern		*tagsparserdirincludepattern*
			The default behavior of this is described in
			|tagsparser-include-exclude-usage|.  The default
			definition of this variable is: >
	let g:TagsParserDirIncludePattern = ""
<
			So by default this option is considered disabled.

TagsParserFileIncludePattern		*tagsparserfileincludepattern*
			The default behavior of this is described in
			|tagsparser-include-exclude-usage|.  The default
			definition of this variable is: >
	let g:TagsParserFileIncludePattern = ""
<
			So by default this option is considered disabled.

==============================================================================
7. Dynamic Tags Commands		*tagsparser-dynamictags-commands*

					*:tagdir*
:TagDir {dir}		This command will take a directory and create a tag
			file for every file file under that directory taking
			into consideration any include and exclude criteria
			(|tagsparser-include-exclude-usage|).  The command
			will do directory name completion for you.  This can
			be used to get an initial set of tags for your project
			directory, or pretty much however you want.  The
			directory you specify with this command does _not_ need
			to be located within the |TagsParserTagsPath| for the 
			command to work.

==============================================================================
8. Tag Window Overview			*tagsparser-tagwindow*

The tag window can be used to view a list of the tags that belong to the file
which is open.  It will sort them hierarchically based on the type that ctags
uses.  Fold markers are used so the file, types or tags with members can all
be folded.  If a tag type is defined as having a "member" the TagsParser can
display things this way.  For example, if struct foo is defined in foo.c: >

	struct foo
	{
	    int bar;
	};
<
The following tags would be created (using the options that I use in the
TagsParser). >

	foo	foo.c	/^struct foo$/;"	s	line:1
	bar	foo.c	/^    int bark;/;"	m	line:3	struct:foo
<
So you can see that it is easy to know that struct memeber bar belongs to
struct foo.  This information is used so that all structures are displayed
under the structure they belong to.

If you are using Exuberant Ctags 5.5.4, it has support for tagging local
variables in functions, and this is supported by TagsParser.  Local variables
are not tagged by default however, so you will have to enable this via the
|TagsParserCtagsOptions_{filetype}| configuration variable.

Currently only Ada and C/H/C++ files have extended support built in to the
TagsParser plugin.  This is mostly because I am familar with the lanugage and
how ctags creates the tags file for the Ada and C/H/C++ languages.  See
|tagsparser-tagwindow-languages| for more information.

					*tagsparser-tagwindow-languages*
The Tag Window portion of the TagsParser currently supports all languages that
the basic Exuberant Ctags 5.5.4 supports, along with Ada.  It is simple to
add baisc support to the TagsParser plugin.  If you want a more complicated
language support added, such as hierarchical and member type support it should
also be mostly straight forward to do, but it isn't quite as simple.  I am
happy to help with any requests for hierarchical language support also because
I feel like this is where the TagsParser plugin is most useful.  The 'H' 
language isn't really a seperate language, but some people add it as 
a seperate type.  So if you have an 'H' language type added with your VIM 
setup then the TagsParser will recognize it as a c/c++ file.  Here is the list 
of currently supported lanuages, and what modes they are supported in.  Some 
types supported by Ctags do not appear to have a direct correlation with a VIM 
type.

					*tagsparser-language-table*
Language	Support			VIM filetype	Ctags type
-------------------------------------------------------------------------
Ada		extended support	ada		ada (add-on)
Assembly	basic supoort		asm		asm
Asp		basic supoort		asp		asp
Awk		basic supoort		awk		awk
Beta		basic supoort				beta
C		extended support	c		c
H		extended support			c++
C++		extended support	cpp		c++
C#		basic support		cs		c#
Cobol		basic support				cobol
Eiffel		basic support		eiffel		eiffel
Erlang		basic support		erlang		erlang
Fortran		basic support		fortran		fortran
Html		basic support		html		html
Java		basic support		java		java
Javascript	basic support		javascript	javascript
Lisp		basic support		lisp		lisp
Lua		basic support		lua		lua
Makefiles	basic support		make		make
Perl		basic support		Perl		Perl
Php		basic support		php		php
Python		basic support		python		python
Rexx		basic support		rexx		rexx
Ruby		basic support		ruby		ruby
Scheme		basic support		scheme		scheme
Sh script	basic support		sh		sh
Slang		basic support		slang		slang
Sml		basic support		sml		sml
Sql		basic support		sql		sql
Tcl		basic support		tcl		tcl
Vera		basic support				vera
Verilog		basic support		verilog		verilog
Vim script	basic support		vim		vim
Yacc		basic support		yacc		yacc

					*tagsparser-ada-support*
Ada is supported because I have added support for it to my ctags install.
This file is included with in the TagsParser tarball in the ctags directory.
To utilize this file, you must have the ctags source somewhere, place the
ada.c file into the directory where that source is located.  Lastly, compile 
and install the ctags program.  The usual way to do this is to run these 
commands in the source directory: >
	
	./configure
	make
	make install
<
The README file that comes with the ctags source contains more information
about the configuration and installation of the ctags program.

==============================================================================
9. Tag Window Usage			*tagsparser-tagwindow-usage*

The tag window is meant to be very intuitive to use.  The details of how it
operates are affected by the way you configure your options
(|tagsparser-tagwindow-config|).

					*tagsparser-tagwindow-opening*
By default the Tag Window is enabled so that whenever the |TagsParserToggle| 
command is used, or the |TagsParserAutoOpenClose| variable is set (and the 
file being edited is within the |TagsParserTagsPath|, etc.), the Tag Window 
will open.  If you wish to disable the Tag Window, you can set the 
|TagsParserNoTagWindow| variable.

a tag window is resized while it is opened, the tag window will attempt to
maintain that size if it is ever closed and re-opened during that VIM session.
When VIM is closed the tag window will revert back to it's default or
preconfigured size (if |TagsParserWindowSize| is set).

To open the tag window use |TagsParserToggle|.  The default behavior is that
the tag window will increase the size of your vim window (if possible).  The
size of the window is based on the |TagsParserWindowSize| variable.  If you do
not want this behavior set the |TagsParserNoResize| flag.

The position of the opened Tag Window defaults to a right vertical split, you 
can move it to the left side of the screen if you want though using the 
|TagsParserWindowLeft| variable.  If you prefer the window to be opened using 
a horizontal split instead of vertical, use the |TagsParserHorizontalSplit| 
option.  With that option set the Tag Window opens on the bottom of the VIM 
window by default.  If you would prefer the Tag Window to open on the bottom 
of the VIM window, set both the |TagsParserHorizontalSplit| and 
|TagsParserWindowTop| variables together.

					*tagsparser-tagwindow-contents*
Tag Nesting has a lot to do with how the Tag Window appears, if the file being 
edited is one of the filetypes that has extended support in TagsParser, it 
will be displayed with nested tags.  See |tagsparser-languages-support| for 
more information on those languages.  If you wish to disable this nesting 
behavior you can set the |TagsParserNoNestedTags| variable.

					*tagsparser-tagwindow-tagselection*
When a tag is selected It will go back to the window the file is loaded in and
go to the line of the selected tag (see |tagsparser-issues-1| for an instance
where this may not happen properly).  If the tag is currently located in a fold
then surrounding code will be unfolded.

					*tagsparser-tagwindow-folding*
About the only interaction that the window requires is that you can fold and
unfold the various hierarchies in the tag window, and you can select a tag.
The methods of folding are the same as the standard VIM folding methods (see
|folding| or |fold-methods| if you need more information).  To select a tag you
can either hit <CR> to select it, double left click on a tag, or if the
|TagsParserSingleClick| option is set when VIM is started, you can just use a
single click to select a tag.  If a tag is selected (either manually, or by the 
|TagsParserHighlightCurrentTag| option) and that tag is located within a fold,
that fold will be opened.  When a new tag is selected, that fold will be
refolded automatically.  If a fold is opened manually before a tag within that
fold is selected, then the fold will not be closed when focus is removed from
that tag.  See |TagsParserFoldLevel| for more details.  By default the fold 
column (|'foldcolumn'|) is displayed in the Tag Window.  To turn the fold 
column off use the |TagsParserFoldColumnDisabled| variable.

Also, the |TagsParserFoldHeading_{filetype}_{letter}| configuration variable 
can be used to force a heading to be folded automatically.

					*tagsparser-tagwindow-syntax*
The items in the tag window do have syntax matching by default.  The things
which are highlighted are the file name, the tags themselves, the tag types
(such as Struct, Class, etc.) and the fold markers (so you can set them to be
easy to ignore).  You can also set the colors used to highlight a selected
tag.  The syntax types which match these are: |TagsParserFileName|,
|TagsParserTag|, |TagsParserTypeName|, |TagsParserFoldMarker|,
|TagsParserHighlight|.  You can override the default highlighting for these
groups in the normal way defined in |:highlight| or |:highlight-link|.

					*tagsparser-tagwindow-quickfix*
As you may or may not know, the quickfix window opens differently based on
whether you have vertical or horizontal splits already open.  I personally
don't like my quickfix window to be vertical so there are some commands and
mappings the TagsParser plugin enables to open up a full sized window below
everything, or a single pane-sized window split below the current opened file.
The commands are |:TagsParserCBot| and |:TagsParserCBotWin| for the full-sized
window below everything, or |:TagsParserCOpen| and |:TagsParserCWindow| for
the window below only the current file.  There are also key mappings installed
for these commands, they are |<leader>tbo|, |<leader>tbw|, |<leader>to| and
|<leader>tw| respectively.

==============================================================================
10. Tag Window Configuration		*tagsparser-tagwindow-config*

					*tagsparsernotagwindow*
TagsParserNoTagWindow	This option will forceably disable the Tag Window from 
			being opened.  So if this option is set, and you 
			attempt to open the tag window it will produce an 
			error.  Setting this option is a good way to limit the 
			functionality of the TagsParser plugin to the Dynamic 
			Tag functionality. >
	let g:TagsParserNoTagWindow = 0
<
					*tagsparsernonestedtags*
TagsParserNoNestedTags	This option will prevent the tags displayed in the Tag 
			Window from being displayed in a nested fashion.  The 
			default way of displaying tags is, if the file type of 
			the file being edited (and tagged) is one of the file 
			types that has extended support (Ada, C/H/C++), it will 
			be displayed in a nested fashion.  So local variables 
			get displayed under the function they are declared in, 
			structure members get displayed under the name of the 
			struct they belong to, and so on.  The full list of 
			languages and what their level of support is defined 
			in |tagsparser-language-table|.  The default for this 
			flag is 0. >
	let g:TagsParserNoNestedTags = 0
<

					*tagsparserwindowleft*
TagsParserWindowLeft	This option will cause the Tag Window to be displayed 
			on the left side of the VIM window.  This setting only 
			matters if the |TasgParserHorizontalSplit| option is 
			off.  By default it is displayed on the right side, so 
			this flag is set to 0. >
	let g:TagsParserWindowLeft = 0
<

TagsParserHorizontalSplit		*tagsparserhorizontalsplit*
			This option will cause the Tag Window to be opened 
			using a horizontal split instead of the default 
			vertical split.  When this is set the Tag Window 
			defaults to opening on the bottom of the VIM window.  
			To force it to open on the top of the VIM window you 
			can set use the |TagsParserWindowTop| variable.  By 
			default this flag is 0. >
	let g:TagsParserHorizontalSplit = 0
<
					*tagsparserwindowtop*
TagsParserWindowTop	This option will cause the Tag Window to be displayed 
			on the top of the VIM window using a horizontal split.  
			This setting only matters if the 
			|TagsParserHorizontalSplit| option is set.  By default 
			this flag is 0. >
	let g:TagsParserWindowTop = 0
<
					*tagsparserwindowsize*
TagsParserWindowSize	The size of the tag window when it is opened.  If the
			|TagsParserNoResize| flag is set then the tag window
			will simply steal this many columns from your existing
			VIM window.  The default is 40. >
	let g:TagsParserWindowSize = 40
<
					*tagsparserwindowname*
TagsParserWindowName	The name of the tag window.  This really has not
			particular use, and there are no special options for
			it to set the window name based on the open file, but
			it is here for you to customize if you feel like.  The
			default is "__tags__". >
	let g:TagsParserWindowName = "__tags__"
<

TagsParserAutoOpenClose			*tagsparserautoopenclose*
			If this flag is set, the tag window will automatically
			open if you start editing a file that has a tag file.
			The tag file in this case must be the one which is
			automatically generated.  The default for this is 0. >
	let g:TagsParserAutoOpenClose = 0
<

TagsParserBufExplWorkAround		*tagsparserbufexplworkaround*
			If you have a buffer explorer plugin loaded, sometimes
			the existance of the tag window with the
			|TagsParserAutoOpenClose| flag being set can interfere
			with some of the automatic operations and/or closing of 
			that buffer explorer.  If you find that things are not
			working quite right you can set this option.  If this
			option is set it will effectively close the tag window
			every time you change to a different window in VIM.
			The default for this option is 0 even if the
			|TagsParserAutoOpenClose| flag is set to 1. >
	let g:TagsParserBufExplWorkAround = 0
<
					*tagsparsernoresize*
TagsParserNoResize	If you don't like the tag window increasing the size
			of your VIM window you can set this flag.  The default
			for this flag is 0. >
	let g:TagsParserNoResize = 0
<
					*tagsparsersingleclick*
TagsParserSingleClick	If this option is set a mapping will be installed
			which will allow you to select a tag with a single
			click instead of <CR> or double click as described in
			|tagsparser-tagwindow-usage|.  This mapping can
			sometimes interfere with other mappings you have set
			so it is not on by default.  The default for this flag
			is 0. >
	let g:TagsParserSingleClick = 0
<

TagsParserHighlightCurrentTag		*tagsparserhighlightcurrenttag*
			If this option is set, while the tag window is open
			the tag definition that the cursor is currently on the
			same line of will be highlighted in the tag window.
			This works fairly well, but it depends on how ctags
			defines which line the tag as being defined on.  See
			|tagsparser-issues-1| and |tagsparser-issues-2| for
			some limitations of this option. The default for this
			flag is 0. >
	let g:TagsParserHighlightCurrentTag = 0
<
					*tagsparsersorttype*
TagsParserSortType	The tags displayed in the tag window are sorted either
			in alphabetical order, or by line number.  The options
			for this are either "line" or "alpha".  The default
			for this flag is "alpha". >
	let g:TagsParserSortType = "alpha"
<
					*tagsparserfoldlevel*
TagsParserFoldLevel	The default folding method for the tag window is to
			fold all types which have members.  So instead of
			showing each struct defined in a file open with all
			members displayed, that struct will be folded.  If you
			prefer to override this default behavior you can set
			the fold level manually with this option, the values
			for this option can be any number that is valid for
			the normal |foldlevel| (although, after a while, large
			numbers stop being meaningful).  If this variable is
			left undefined in your .vimrc then the default fold
			method will be followed.  As described in
			|tagsparser-tagwindow-folding|, when a tag is
			highlighted, if it is currently hidden in a fold it
			will be unfolded.  And that fold will be refolded when
			that tag is no longer highlighted.  By default this 
			variable is undefined. >
	unlet! g:TagsParserFoldLevel
<
TagsParserFoldColumnDisabled		*tagsparserfoldcolumndisabled*
			If this option is set then the fold column will not be 
			displayed in the Tag Window.  The default this flag is 
			0. >
	let g:TagsParserFoldColumnDisabled = 0
<
TagsParserDisplaySignature		*tagsparserdisplaysignature*
			The default display of the tag window is the name of
			the tags themselves.  If you prefer to see what the
			difference between the tags is you can turn on this
			option to have the line where the tag is defined
			displayed.  The default for this flag is 0. >
	let g:TagsParserDisplaySignature = 0
<

				*tagsparserdisabletype_{filetype}_{letter}*
TagsParserDisableType_{filetype}_{letter}
			This option cause a type that has been tagged from 
			never being shown.  So if you like tagging externed 
			variables in C/C++, but don't ever want them to show 
			up in the Tag Window, you can disable that option like 
			so: >
	let g:TagsParserDisableType_cpp_x = 1
	let g:TagsParserDisableType_c_x = 1
<
			As with the other {filetype} configuration variables, 
			you must replace the {filetype} string with the string 
			VIM uses as the filetype for files of that type.  The 
			details of the lanugage strings are listed in the 
			|tagsparser-language-table| section.  The {letter} 
			option is whatever the ctags program uses for that 
			particular type.  To get a full list of what options 
			are available for a particular language use the 
			--list-kinds=c ctags option.  Here is an example: >
	$ ctags --list-kinds=c
	c  classes
	d  macro definitions
	e  enumerators (values inside an enumeration)
	f  function definitions
	g  enumeration names
	l  local variables [off]
	m  class, struct, and union members
	n  namespaces
	p  function prototypes [off]
	s  structure names
	t  typedefs
	u  union names
	v  variable definitions
	x  external variable declarations [off]
<
			There are no defaults for this class of variable.

				*tagsparserfoldheading_{filetype}_{letter}*
TagsParserFoldHeading_{filetype}_{letter}
			This option can be used to automatically fold 
			a heading by default.  See the 
			|tagsparser-tagwindow-folding| section for more 
			details on how the TagsParser plugin uses folding in 
			the Tag Window.  Similar to other {filetype} and 
			{letter} configuration variables, the {filetype} must 
			be the string VIM uses as the filetype, these are 
			listed in the |tagsparser_language-table|.  And the 
			{letter} is the letter that the ctags program uses.  
			An example of how to find this is show in the 
			|TagsParserDisableType_{filetype}_{letter}| section.  
			For example, if you wanted function prototypes in 
			C and C++ files to always be folded: >
	let g:TagsParserFoldHeading_c_p = 1
	let g:TagsParserFoldHeading_cpp_p = 1
<
			There are no default values for this class of 
			variable.

					*tagsparsertag* *tagsparsertag-syntax*
TagsParserTag		Each tag in the tag window match this syntax type.
			The default highlighting for this type matches the
			Normal highlighting group. >
	hi link TagsParserTag Normal
<
					*tagsparserfilename*
					*tagsparserfilename-syntax*
TagsParserFileName	The filename at the top of the tag window match this
			syntax type.  The default highlighting for this type
			matches the Underlined highlighting group. >
	hi link TagsParserFileName Underlined
<
					*tagsparsertypename*
					*tagsparsertypename-syntax*
TagsParserTypeName	The type names in the tag window match this syntax
			type.  The default highlighting for this type matches
			the Special highlighting group. >
	hi link TagsParserTypeName Special
<
					*tagsparserfoldmarker*
					*tagsparserfoldmarker-syntax*
TagsParserFoldMarker	The fold markers in the tag window match this syntax
			type.  The default highlighting for this type matches
			the Ignore highlighting group. >
	hi link TagsParserFoldMarker Ignore
<
					*tagsparserhighlight*
					*tagsparserhighlight-syntax*
TagsParserHighlight	The selected tag in the tag window match this syntax
			type.  The default highlighting for this type matches
			the ToDo highlighting group. >
	hi link TagsParserFoldMarker ToDo
<
==============================================================================
11. Tag Window Commands			*tagsparser-tagwindow-commands*

					*:tagsparsertoggle*
:TagsParserToggle	This command is used to open or close the tags window.
			If the tag window is open it will be closed, if it is
			closed, the window will be opened.  As described in
			|tagsparser-tagwindow-usage| if the
			|TagsParserAutoOpenClose| flag is set and the tag
			window is manually closed (with :TagsParserToggle,
			|:q|, or similar command) the flag will be turned off
			temporarily until the tag window is manually opened
			again via the :TagsParserToggle command.

			This is done to prevent the window from being auto
			opened when it is unwanted.  This allows you to easily
			turn off the auto-open behavior during a vim session.

					*<leader>t<space>*
<leader>t<space>	This is the default mapping installed which will call
			the |:TagsParserToggle| command.

					*:tagsparsercbot*
:TagsParserCBot		This command is used to open up a quickfix window
			(unconditionally) along the same lines as the regular
			|:copen| command.  It will open up a full sized window
			split horizontally below all currently opened windows,
			and then jump to the first error (if there are any).

					*<leader>tbo*
<leader>tbo		This is the default mapping installed which will call
			the |:TagsParserCBot| command.

					*:tagsparsercbotwin*
:TagsParserCBotWin	This command is used to open up a quickfix window along
			the same lines as the regular |:cwindow| command.  It
			will open up a full sized window split horizontally
			below all currently opened windows, and then jump to
			the first error (if there are any).

					*<leader>tbw*
<leader>tbw		This is the default mapping installed which will call
			the |:TagsParserCBotWin| command.

					*:tagsparsercopen*
:TagsParserCOpen	This command is used to open up a quickfix window
			(unconditionally) along the same lines as the regular
			|:copen| command.  It will open up a single pane sized
			window split horizontally below the currently opened
			(and active) file, and then jump to the first error (if
			there are any).

					*<leader>to*
<leader>to		This is the default mapping installed which will call
			the |:TagsParserCOpen| command.

					*:tagsparsercwindow*
:TagsParserCWindow	This command is used to open up a quickfix window along
			the same lines as the regular |:cwindow| command.  It
			will open up a single pane sized window split
			horizontally below the currently opened (and active)
			file, and then jump to the first error (if there are
			any).

					*<leader>tw*
<leader>tw		This is the default mapping installed which will call
			the |:TagsParserCWindow| command.

==============================================================================
12. Depreciated Options			*tagsparser-depreciated-options*

					*tagsparserfindprogram*
TagsParserFindProgram	This variable is depreciated and no longer needed.
			It used to be used for defining where the gnu-style 
			find program was located, but functionality that used 
			find has been moved to native vimscript.

					*mytagspath*
MyTagsPath		This variable is depreciated, the |TagsParserTagsPath|
			variable should be used now instead.  There were 
			problems caused by using a space separated path 
			variable such as attempting to tag a directory that 
			contained names with spaces in it.  So the new path 
			style uses a Vim-style comma separated path.  See 
			|path| for more information on Vim-style paths.

==============================================================================
13. Known Issues			*tagsparser-issues*
					*tagsparser-issues-1*
- If you edit a file and don't save it, tag selection through the tag window
will be inaccurate, and the current tag highlighting will most likely not be
able to locate the tag related to the current cursor position.  This is
because I currently use line numbers from the tag file to verify a match.
This is the method used for the TagsParser plugin to locate a tag because many
files will have tags that are named the same, and even possibly are defined on
a line which looks the same, for example local variables and overloaded
functions.

					*tagsparser-issues-2*
- If there is a tag which in reality takes up a range of lines (such as a
function) the |TagsParserHighlightCurrentTag| option still won't be able to
determine which tag the cursor is in unless the cursor is on the exact line
that ctags uses as the line that the tag is defined on.  That means for a
function defined as (line numbers inserted for clarity): >

	1 int main(int argc, char *argv[])
	2 {
	3     printf("Hello World!");
	4     return 0;
	5 }
<
unless you are on line 1 the TagsParser will not highlight a tag.  This is
because of limitations in the way that tags are defined by ctags.  Only the
line in which the tag is defined is stored.

					*tagsparser-issues-3*
- When a tag is selected manually through the Tag Window, there is no way to
push that onto the tag stack.  So the normal tag navigation method of ^T and 
^] won't work.  However, the cursor positions are marked so you can always jump 
to the previous position using '' (two single quotes).

					*tagsparser-issues-4*
- When you tag a directory of files using the |TagDir| command, the filetypes 
are not parsed first so any user defined overall Ctags options, or language 
specific options will not be used to parse the file the first time.  However, 
next time the file is saved (assuming it is in the |TagsParserTagsPath|), the 
proper options will be used.

					*tagsparser-issues-5*
- By default the Exuberant Ctags program does not deal well with spaces in tag 
file names.  This is because of how parameters are supplied to the external 
sort program.  Ctags uses an external sort by default (if it is available).  
For more information about this particular bug, and how to work around it, 
check out the |tagsparser-ctags-spaces| section.
					
==============================================================================
14. Changelog				*tagsparser-changelog*

0.4 - First bugfix release - 06/11/2006

06/11/2006 - Cleaned up help file.
06/09/2006 - Added some GCOV extensions (*.da, *.bb, *.bbg, *.gcov) to file
             exclude pattern.
06/09/2006 - Added GNAT build artifact extension (*.ali) to file exclude
             pattern.
06/09/2006 - Fixed some spelling errors in messages and comments.
06/09/2006 - Added standard library extensions (*.a, *.so) to file exclude
             pattern.
06/09/2006 - Changed include/exclude regular expressions into Vim regexps 
             instead of Perl regexps.
06/08/2006 - Fixed issues with spaces in paths (mostly of... The root of it
             is when Ctags is using The external sort... at least on Win32).
06/08/2006 - Fixed issue where tag files are created for directory names 
             when using The TagDir command.
06/02/2006 - Added Copyright notice.
06/02/2006 - Fixed tag naming issue where if you have 
	     TagsParserCtagsOptions_{filetype} options defined, it messes up 
	     The name of The tag file.
05/26/2006 - Added nospell to local TagWindow options for Vim 7.

0.3 - Initial Public Release - 05/07/2006

==============================================================================
15. Todo List				*tagsparser-todo*

- Move Perl code to native Vimscript for those who use version 7 Vim  
  (dependence on external find program already removed).
- Make tab pages play nicely with the Tag Window.
- Allow the definition of separate tag paths depending on the current working 
  directory.  Useful for working on large multiple projects.
- Read in a file when running a |TagDir| command so that the correct user 
  options are used to tag the file with.

==============================================================================
 vim:tw=78:ts=8:sts=8:sw=8:noet:ft=help:fo=twa21
plugin/TagsParser.vim
2062
" File:         TagsParser.Vim
" Description:  Dynamic file tagging and mini-window to display tags
" Version:      0.4
" Date:         June 11, 2006
" Author:       A. Aaron Cornelius (ADotAaronDotCorneliusAtgmailDotcom)
"
" Installation:
" ungzip and untar The TagsParser.tar.gz somewhere in your Vim runtimepath
" (typically this should be something like $HOME/.Vim, $HOME/vimfiles or
" $VIM/vimfiles) Once this is done run :helptags <dir>/doc where <dir> is The
" directory that you ungziped and untarred The TagsParser.tar.gz archive in.
"
" Usage:
" For help on The usage of this plugin to :help TagsParser after you have
" finished The installation steps.
"
" Changelog:
" 0.4 - First bugfix release - 06/11/2006
"
" 06/09/2006 - Added some GCOV extensions (*.da, *.bb, *.bbg, *.gcov) to file
"              exclude pattern.
" 06/09/2006 - Added GNAT build artifact extension (*.ali) to file exclude
"              pattern.
" 06/09/2006 - Fixed some spelling errors in messages and comments.
" 06/09/2006 - Added standard library extensions (*.a, *.so) to file exclude
"              pattern.
" 06/09/2006 - Changed include/exclude regular expressions into Vim regexps 
"              instead of Perl regexps.
" 06/08/2006 - Fixed issues with spaces in paths (mostly of... The root of it
"              is when Ctags is using The external sort... at least on Win32).
" 06/08/2006 - Fixed issue where tag files are created for directory names 
"              when using The TagDir command.
" 06/02/2006 - Added Copyright notice. 
" 06/02/2006 - Fixed tag naming issue where if you have 
"              TagsParserCtagsOptions* options defined, it messes up The name
"              of The tag file.
" 05/26/2006 - Added nospell to local TagWindow options for Vim 7.
"
" 0.3 - Initial Public Release - 05/07/2006
"
" Future Changes:
" TODO: Move as much external code (Perl) to internal vimscript.
"       - use Vim dictionary instead of Perl hash
" TODO: Make compatible with Tab pages for Vim 7.
" TODO: allow The definition of separate tag paths depending on The current 
"       working directory
" TODO: read in a file when doing TagDir so that The correct options are used 
"       to tag The file
"
" Bug List:
"
"
" Copyright (C) 2006 A. Aaron Cornelius
"
" This program is free software; you can redistribute it and/or
" modify it under The terms of The GNU General Public License
" as published by The Free Software Foundation; either version 2
" of The License, or (at your option) any later version.
"
" This program is distributed in The hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even The implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See The
" GNU General Public License for more details.
"
" You should have received a copy of The GNU General Public License
" along with this program; if not, write to The Free Software
" Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
" USA.

let s:cpoSave = &cpo
set cpo&vim

" Init Check <<<
if exists('s:TagsParserLoaded')
  finish
endif
let s:TagsParserLoaded = 1
" >>>

" Configuration

" Perl Check <<<
if !has('Perl')
  if !exists('g:TagsParserNoPerlWarning') || g:TagsParserNoPerlWarning == 0
    echomsg "You must have a perl enabled version of VIM to use full functionality of TagsParser plugin."
    echomsg "(to disable this warning set The g:TagsParserNoPerlWarning variable to 1 in your .vimrc)"
  endif
  finish
endif
" >>>
" Global Variables <<<
if !exists("g:TagsParserCtagsOptions")
  let g:TagsParserCtagsOptions = ""
endif

if !exists("g:TagsParserOff")
  let g:TagsParserOff = 0
endif

if !exists("g:TagsParserLastPositionJump")
  let g:TagsParserLastPositionJump = 0
endif

if !exists("g:TagsParserNoNestedTags")
  let g:TagsParserNoNestedTags = 0
endif

if !exists("g:TagsParserNoTagWindow")
  let g:TagsParserNoTagWindow = 0
endif

if !exists("g:TagsParserWindowLeft")
  let g:TagsParserWindowLeft = 0
endif

if !exists("g:TagsParserHorizontalSplit")
  let g:TagsParserHorizontalSplit = 0
endif

if !exists("g:TagsParserWindowTop")
  let g:TagsParserWindowTop = 0
endif

"based on The window position configuration variables, setup the tags window 
"split command
if g:TagsParserWindowLeft != 1 && g:TagsParserHorizontalSplit != 1
  let s:TagsWindowPosition = "botright vertical"
elseif g:TagsParserHorizontalSplit != 1
  let s:TagsWindowPosition = "topleft vertical"
elseif g:TagsParserWindowTop != 1
  let s:TagsWindowPosition = "botright"
else
  let s:TagsWindowPosition = "topleft"
endif

if !exists("g:TagsParserFoldColumnDisabled")
  let g:TagsParserFoldColumnDisabled = 0
endif

if !exists("g:TagsParserWindowSize")
  let g:TagsParserWindowSize = 40
endif

if !exists("g:TagsParserWindowName")
  let g:TagsParserWindowName = "__tags__"
endif

if !exists('g:TagsParserSingleClick')
  let g:TagsParserSingleClick = 0
endif

if !exists("g:TagsParserHighlightCurrentTag")
  let g:TagsParserHighlightCurrentTag = 0
endif

if !exists("g:TagsParserAutoOpenClose")
  let g:TagsParserAutoOpenClose = 0
endif

if !exists("g:TagsParserBufExplWorkAround")
  let g:TagsParserBufExplWorkAround = 0
endif

if !exists("g:TagsParserNoResize")
  let g:TagsParserNoResize = 0
endif

if !exists("g:TagsParserSortType") || g:TagsParserSortType != "line"
  let g:TagsParserSortType = "alpha"
endif

if !exists("g:TagsParserDisplaySignature")
  let g:TagsParserDisplaySignature = 0
endif

"if we are in the C:/WINDOWS/SYSTEM32 dir, change to C.  Odd things seem to
"happen if we are in the system32 directory
if has('win32') && getcwd() ==? 'C:\WINDOWS\SYSTEM32'
  let s:cwdChanged = 1
  cd C:\
else
  let s:cwdChanged = 0
endif

"if the tags program has not been specified by a user level global define,
"find the right tags program.  This checks exuberant-ctags first to handle the
"case where multiple tags programs are installed it is differentiated by an
"explicit name
if !exists("g:TagsParserTagsProgram")
  if executable("exuberant-ctags")
    let g:TagsParserTagsProgram = "exuberant-ctags"
  elseif executable("ctags")
    let g:TagsParserTagsProgram = "ctags"
  elseif executable("ctags.exe")
    let g:TagsParserTagsProgram = "ctags.exe"
  elseif executable("tags")
    let g:TagsParserTagsProgram = "tags"
  else
    echomsg "TagsParser - tags program not found, go to " .
          \"http://ctags.sourceforge.net/ to download it.  OR"
          \"specify the path to a the Exuberant Ctags program " .
          \"using the g:TagsParserTagsProgram variable in your .vimrc"
    finish
  endif
endif

if system(g:TagsParserTagsProgram . " --version") !~? "Exuberant Ctags"
  echomsg "TagsParser - ctags = " . g:TagsParserTagsProgram . " go to"
        \"http://ctags.sourceforge.net/ to download it.  OR"
        \"specify the path to a the Exuberant Ctags program " .
        \"using the g:TagsParserTagsProgram variable in your .vimrc"
  finish
endif

if s:cwdChanged == 1
  cd C:\WINDOWS\SYSTEM32
endif

"These variables are in Vim-style regular expressions, not per-style like they 
"used to be.  See ":help usr_27.txt" and ":help regexp" for more information.
"If the patterns are empty then they are considered disabled

"Init the directory exclude pattern to remove any . or _ prefixed directories
"because they are generally considered 'hidden'.  This will also have the
"benefit of preventing the tagging of any .tags directories
if !exists("g:TagsParserDirExcludePattern")
  let g:TagsParserDirExcludePattern = '.\+/\..\+\|.\+/_.\+\|\%(\ctmp\)\|' .
        \ '\%(\ctemp\)\|\%(\cbackup\)'
endif

if !exists("g:TagsParserDirIncludePattern")
  let g:TagsParserDirIncludePattern = ""
endif

"Init the file exclude pattern to take care of typical object, library
"backup, swap, dependency and tag file names and extensions, build artifacts, 
"gcov extenstions, etc.
if !exists("g:TagsParserFileExcludePattern")
  let g:TagsParserFileExcludePattern = '^.*\.\%(\co\)$\|^.*\.\%(\cobj\)$\|' .
        \ '^.*\.\%(\ca\)$\|^.*\.\%(\cso\)$\|^.*\.\%(\cd\)$\|' .
        \ '^.*\.\%(\cbak\)$\|^.*\.\%(\cswp\)$\|^.\+\~$\|' .
        \ '^\%(\ccore\)$\|^\%(\ctags\)$\|^.*\.\%(\ctags\)$\|' .
        \ '^.*\.\%(\ctxt\)$\|^.*\.\%(\cali\)$\|^.*\.\%(\cda\)$\|' .
        \ '^.*\.\%(\cbb\)$\|^.*\.\%(\cbbg\)$\|^.*\.\%(\cgcov\)$'
endif

if !exists("g:TagsParserFileIncludePattern")
  let g:TagsParserFileIncludePattern = ""
endif

" >>>
" Script Autocommands <<<
" No matter what, always install The LastPositionJump autocommand, if enabled
if g:TagsParserLastPositionJump == 1
  au BufWinEnter * if line("'\"") > 0 && line("'\"") <= line("$") | exec "normal g`\"" | endif
endif
" only install the autocommands if the g:TagsParserOff variable is not set
if g:TagsParserOff == 0 && g:TagsParserNoTagWindow == 0
  augroup TagsParserAutoCommands
    autocmd!
    "setup an autocommand that will expand the path described by
    "g:TagsParserTagsPath into a valid tag path
    autocmd VimEnter * call <SID>TagsParserExpandTagsPath() |
          \ call <SID>TagsParserPerformOp("open", "")

    "setup an autocommand so that when a file is written to it writes a tag
    "file if it a file that is somewhere within the tags path or the
    "g:TagsParserTagsPath path
    autocmd BufWritePost ?* call <SID>TagsParserPerformOp("tag", "")
  augroup end

  augroup TagsParserBufWinEnterWindowNotOpen
    autocmd BufWinEnter ?* call <SID>TagsParserPerformOp("open", "")
  augroup end
elseif g:TagsParserOff == 0 && g:TagsParserNoTagWindow == 1
  augroup TagsParserAutoCommands
    autocmd!
    "setup an autocommand that will expand the path described by
    "g:TagsParserTagsPath into a valid tag path
    autocmd VimEnter * call <SID>TagsParserExpandTagsPath()

    "setup an autocommand so that when a file is written to it writes a tag
    "file if it a file that is somewhere within the tags path or the
    "g:TagsParserTagsPath path
    autocmd BufWritePost ?* call <SID>TagsParserPerformOp("tag", "")
  augroup end
endif
" >>>
" Setup Commands <<<

" TagsParser functionality
command! -nargs=0 TagsParserToggle :call <SID>TagsParserToggle()
nmap <leader>t<space> :TagsParserToggle<CR>

command! -nargs=+ -complete=dir TagDir 
      \ :call <SID>TagsParserSetupDirectoryTags(<q-args>)

" Turning TagsParser functionality completely off (and then back on)
command! -nargs=0 TagsParserOff :call <SID>TagsParserOff()
nmap <leader>tof :TagsParserOff<CR>
command! -nargs=0 TagsParserOn :call <SID>TagsParserOn()
nmap <leader>ton :TagsParserOn<CR>

" do a copen/cwindow so The quickfix window stretches over the whole window
command! -nargs=* TagsParserCBot :botright copen <args>
nmap <leader>tbo :TagsParserCBot<CR>
command! -nargs=* TagsParserCBotWin :botright cwindow <args>
nmap <leader>tbw :TagsParserCBotWin<CR>

" do a 'smart' copen/cwindow so that the quickfix window is only below the
" main window
command! -nargs=* TagsParserCOpen :call <SID>TagsParserCOpen(<f-args>)
nmap <leader>to :TagsParserCOpen<CR>
command! -nargs=* TagsParserCWindow :call <SID>TagsParserCWindow(<f-args>)
nmap <leader>tw :TagsParserCWindow<CR>

" for convenience
nmap <leader>tc :cclose
" >>>
" Initialization <<<

" Check for any depreciated variables and options
if exists("g:MyTagsPath")
  echomsg "The MyTagsPath variable is depreciated, please use TagsParserTagsPath instead.\nThis path should be set in The same way that all VIM paths are, using commas instead of spaces.  Please see ':help path' for more information."
endif

if exists("g:TagsParserFindProgram")
  echomsg "The TagsParserFindProgram variable is no longer necessary, you can remove it from your .vimrc"
endif

"if we are in the C:/WINDOWS/SYSTEM32 dir, change to C.  Odd things seem to
"happen if we are in the system32 directory
if has('win32') && getcwd() ==? 'C:\WINDOWS\SYSTEM32'
  let s:cwdChanged = 1
  cd C:\
else
  let s:cwdChanged = 0
endif

if s:cwdChanged == 1
  cd C:\WINDOWS\SYSTEM32
endif

"init the matched tag fold flag
let s:matchedTagFoldStart = 0
let s:matchedTagFoldEnd = 0
let s:matchedTagWasFolded = 0

"init the update values 
let s:tagsDataUpdated = 1
let s:lastFileDisplayed = ""

let s:newBufBeingCreated = 0

"setup the mappings to handle single click
if g:TagsParserSingleClick == 1
  let s:clickmap = ':if bufname("%") == g:TagsParserWindowName <bar> call <SID>TagsParserSelectTag() <bar> endif <CR>'
  if maparg('<LeftMouse>', 'n') == '' 
    " no mapping for leftmouse
    exec ':nnoremap <silent> <LeftMouse> <LeftMouse>' . s:clickmap
  else
    " we have a mapping
    let  s:m = ':nnoremap <silent> <LeftMouse> <LeftMouse>'
    let  s:m = s:m . substitute(substitute(maparg('<LeftMouse>', 'n'), '|', '<bar>', 'g'), '\c^<LeftMouse>', '', '')
    let  s:m = s:m . s:clickmap
    exec s:m
  endif
endif

" TagsParserPerlInit - Init the default type names <<<
function! <SID>TagsParserPerlInit()
perl << PerlFunc
  use strict;
  use warnings;
  no warnings 'redefine';

  # define the data needed for displaying the tag data, define it in
  # the order desired for parsing, and each entry has the key into the
  # tags hash, the title for those types, and what fold level to put it at
  # the default fold level is 3 which allows for something along the lines of
  # namespace->class->function()

  # if you define a new set of types, make sure to prefix the name with
  # a "- " string so that it will be picked up by the "Type" syntax
  my @adaTypes = ( [ "P", "- Package Specs" ],
                   [ "p", "- Packages" ],
                   [ "T", "- Type Specs" ],
                   [ "t", "- Types" ],
                   [ "U", "- Subtype Specs" ],
                   [ "u", "- Subtypes" ],
                   [ "c", "- Components" ],
                   [ "l", "- Literals" ],
                   [ "V", "- Variable Specs" ],
                   [ "v", "- Variables" ],
                   [ "n", "- Constants" ],
                   [ "x", "- Exceptions" ],
                   [ "f", "- Formal Params" ],
                   [ "R", "- Subprogram Specs" ],
                   [ "r", "- Subprograms" ],
                   [ "K", "- Task Specs" ],
                   [ "k", "- Tasks" ],
                   [ "O", "- Protected Data Specs" ],
                   [ "o", "- Protected Data" ],
                   [ "E", "- Entry Specs" ],
                   [ "e", "- Entries" ],
                   [ "b", "- Labels" ],
                   [ "i", "- Identifiers" ],
                   [ "a", "- Auto Vars" ],
                   [ "y", "- Blocks" ] );

  my @asmTypes = ( [ "d", "- Defines" ],
                   [ "t", "- Types" ],
                   [ "m", "- Macros" ],
                   [ "l", "- Labels" ] );

  my @aspTypes = ( [ "f", "- Functions" ],
                   [ "s", "- Subroutines" ],
                   [ "v", "- Variables" ] );

  my @awkTypes = ( [ "f", "- Functions" ] );

  my @betaTypes = ( [ "f", "- Fragment Defs" ],
                    [ "p", "- All Patterns" ],
                    [ "s", "- Slots" ],
                    [ "v", "- Patterns" ] );

  my @cTypes = ( [ "n", "- Namespaces" ],
                 [ "c", "- Classes" ],
                 [ "d", "- Macros" ],
                 [ "t", "- Typedefs" ],
                 [ "s", "- Structures" ],
                 [ "g", "- Enumerations" ],
                 [ "u", "- Unions" ],
                 [ "x", "- External Vars" ],
                 [ "v", "- Variables" ],
                 [ "p", "- Prototypes" ],
                 [ "f", "- Functions" ],
                 [ "m", "- Struct/Union Members" ],
                 [ "e", "- Enumerators" ],
                 [ "l", "- Local Vars" ] );

  my @csTypes = ( [ "c", "- Classes" ],
                  [ "d", "- Macros" ],
                  [ "e", "- Enumerators" ],
                  [ "E", "- Events" ],
                  [ "f", "- Fields" ],
                  [ "g", "- Enumerations" ],
                  [ "i", "- Interfaces" ],
                  [ "l", "- Local Vars" ],
                  [ "m", "- Methods" ],
                  [ "n", "- Namespaces" ],
                  [ "p", "- Properties" ],
                  [ "s", "- Structs" ],
                  [ "t", "- Typedefs" ] );

  my @cobolTypes = ( [ "d", "- Data Items" ],
                     [ "f", "- File Descriptions" ],
                     [ "g", "- Group Items" ],
                     [ "p", "- Paragraphs" ],
                     [ "P", "- Program IDs" ],
                     [ "s", "- Sections" ] );

  my @eiffelTypes = ( [ "c", "- Classes" ],
                      [ "f", "- Features" ],
                      [ "l", "- Local Entities" ] );

  my @erlangTypes = ( [ "d", "- Macro Defs" ],
                      [ "f", "- Functions" ],
                      [ "m", "- Modules" ],
                      [ "r", "- Record Defs" ] );

  my @fortranTypes = ( [ "b", "- Block Data" ],
                       [ "c", "- Common Blocks" ],
                       [ "e", "- Entry Points" ],
                       [ "f", "- Functions" ],
                       [ "i", "- Interface Contents/Names/Ops" ],
                       [ "k", "- Type/Struct Components" ],
                       [ "l", "- Labels" ],
                       [ "L", "- Local/Common/Namelist Vars" ],
                       [ "m", "- Modules" ],
                       [ "n", "- Namelists" ],
                       [ "p", "- Programs" ],
                       [ "s", "- Subroutines" ],
                       [ "t", "- Derived Types/Structs" ],
                       [ "v", "- Program/Module Vars" ] );

  my @htmlTypes = ( [ "a", "- Named Anchors" ],
                    [ "f", "- Javascript Funcs" ] );

  my @javaTypes = ( [ "c", "- Classes" ],
                    [ "f", "- Fields" ],
                    [ "i", "- Interfaces" ],
                    [ "l", "- Local Vars" ],
                    [ "m", "- Methods" ],
                    [ "p", "- Packages" ] );

  my @javascriptTypes = ( [ "f", "- Functions" ] );

  my @lispTypes = ( [ "f", "- Functions" ] );

  my @luaTypes = ( [ "f", "- Functions" ] );

  my @makeTypes = ( [ "m", "- Macros" ] );

  my @pascalTypes = ( [ "f", "- Functions" ],
                      [ "p", "- Procedures" ] );

  my @perlTypes = ( [ "c", "- Constants" ],
                    [ "l", "- Labels" ],
                    [ "s", "- Subroutines" ] );

  my @phpTypes = ( [ "c", "- Classes" ],
                   [ "d", "- Constants" ],
                   [ "f", "- Functions" ],
                   [ "v", "- Variables" ] );

  my @pythonTypes = ( [ "c", "- Classes" ],
                      [ "m", "- Class Members" ],
                      [ "f", "- Functions" ] );

  my @rexxTypes = ( [ "s", "- Subroutines" ] );

  my @rubyTypes = ( [ "c", "- Classes" ],
                    [ "f", "- Methods" ],
                    [ "F", "- Singleton Methods" ],
                    [ "m", "- Mixins" ] );

  my @schemeTypes = ( [ "f", "- Functions" ],
                      [ "s", "- Sets" ] );

  my @shTypes = ( [ "f", "- Functions" ] );

  my @slangTypes = ( [ "f", "- Functions" ],
                     [ "n", "- Namespaces" ] );

  my @smlTypes = ( [ "e", "- Exception Defs" ],
                   [ "f", "- Function Defs" ],
                   [ "c", "- Functor Defs" ],
                   [ "s", "- Signatures" ],
                   [ "r", "- Structures" ],
                   [ "t", "- Type Defs" ],
                   [ "v", "- Value Bindings" ] );

  my @sqlTypes = ( [ "c", "- Cursors" ],
                   [ "d", "- Prototypes" ],
                   [ "f", "- Functions" ],
                   [ "F", "- Record Fields" ],
                   [ "l", "- Local Vars" ],
                   [ "L", "- Block Label" ],
                   [ "P", "- Packages" ],
                   [ "p", "- Procedures" ],
                   [ "r", "- Records" ],
                   [ "s", "- Subtypes" ],
                   [ "t", "- Tables" ],
                   [ "T", "- Triggers" ],
                   [ "v", "- Variables" ] );

  my @tclTypes = ( [ "c", "- Classes" ],
                   [ "m", "- Methods" ],
                   [ "p", "- Procedures" ] );

  my @veraTypes = ( [ "c", "- Classes" ],
                    [ "d", "- Macro Defs" ],
                    [ "e", "- Enumerators" ],
                    [ "f", "- Functions" ],
                    [ "g", "- Enumerations" ],
                    [ "l", "- Local Vars" ],
                    [ "m", "- Class/Struct/Union Members" ],
                    [ "p", "- Programs" ],
                    [ "P", "- Prototypes" ],
                    [ "t", "- Tasks" ],
                    [ "T", "- Typedefs" ],
                    [ "v", "- Variables" ],
                    [ "x", "- External Vars" ] );

  my @verilogTypes = ( [ "c", "- Constants" ],
                       [ "e", "- Events" ],
                       [ "f", "- Functions" ],
                       [ "m", "- Modules" ],
                       [ "n", "- Net Data Types" ],
                       [ "p", "- Ports" ],
                       [ "r", "- Register Data Types" ],
                       [ "t", "- Tasks" ] );

  my @vimTypes = ( [ "a", "- Autocommand Groups" ],
                   [ "f", "- Functions" ],
                   [ "v", "- Variables" ] );

  my @yaccTypes = ( [ "l", "- Labels" ] );

  our %typeMap : unique = ( ada => \@adaTypes,
                            asm => \@asmTypes,
                            asp => \@aspTypes,
                            awk => \@awkTypes,
                            beta =>  \@betaTypes,
                            c => \@cTypes,
                            cpp => \@cTypes,
                            cs => \@csTypes,
                            cobol => \@cobolTypes, 
                            eiffel => \@eiffelTypes, 
                            erlang => \@erlangTypes, 
                            fortran => \@fortranTypes, 
                            html => \@htmlTypes, 
                            java => \@javaTypes, 
                            javascript => \@javascriptTypes, 
                            lisp => \@lispTypes, 
                            lua => \@luaTypes, 
                            make => \@makeTypes,
                            pascal => \@pascalTypes, 
                            perl => \@perlTypes,
                            php => \@phpTypes, 
                            python => \@pythonTypes,
                            rexx => \@rexxTypes, 
                            ruby => \@rubyTypes,
                            scheme => \@schemeTypes, 
                            sh => \@shTypes, 
                            slang => \@slangTypes, 
                            sml => \@smlTypes, 
                            sql => \@sqlTypes, 
                            tcl => \@tclTypes,
                            vera => \@veraTypes, 
                            verilog => \@verilogTypes, 
                            Vim => \@vimTypes,
                            yacc => \@yaccTypes ) unless(%typeMap);

  # create a subtype hash, much like the typeMap.  This will list what
  # sub-types to display, so for example, C struct types will only have it's
  # "m" member list checked which will list the fields of that struct, while
  # namespaces can have all of the types listed in the @cType array.
  my %adaSubTypes  = ( i => \@adaTypes,
                       t => [ [ "c", "" ],
                              [ "l", "" ],
                              [ "a", "- Discriminants" ] ],
                       u => [ [ "c", "" ],
                              [ "l", "" ],
                              [ "a", "- Discriminants" ] ],
                       P => \@adaTypes,
                       p => \@adaTypes,
                       R => \@adaTypes,
                       r => \@adaTypes,
                       K => \@adaTypes,
                       k => \@adaTypes,
                       O => \@adaTypes,
                       o => \@adaTypes,
                       E => \@adaTypes,
                       e => \@adaTypes,
                       y => \@adaTypes );

  my %cSubTypes  = ( f => [ [ "l", "" ] ],
                     s => [ [ "m", "" ] ],
                     u => [ [ "m", "" ] ],
                     g => [ [ "e", "" ] ],
                     c => \@cTypes,
                     n => \@cTypes );

  our %subTypeMap : unique = ( ada => \%adaSubTypes,
                               c => \%cSubTypes,
                               cpp => \%cSubTypes ) unless(%subTypeMap);

  my $success = 0;
  my $value = 0;

  # Disable any languages which the user wants disabled
  foreach my $key (keys %typeMap) {
    ($success, $value) = VIM::Eval("exists('g:TagsParserDisableLang_$key')");
    delete $typeMap{$key} if ($success == 1 and $value == 1);
  }

  # Lastly, remove any headings that the user wants explicitly disabled
  foreach my $key (keys %typeMap) {
    my $typeRef;

    # now remove any unwanted types, start at the end of the list so that we
    # don't mess things up by deleting entries and changing the length of the
    # array
    for (my $i = @{$typeMap{$key}} - 1; $typeRef = $typeMap{$key}[$i]; $i--) {
      ($success, $value) = VIM::Eval("exists('g:TagsParserDisableType_" .
        $key . "_" . $typeRef->[0] . "')");
      splice(@{$typeMap{$key}}, $i, 1) if ($success == 1 and $value == 1);
    }
  }

  our %typeMapHeadingFold : unique = ( ) unless(%typeMapHeadingFold);

  # build up a list of any headings that the user wants to be automatically
  # folded
  foreach my $key (keys %typeMap) {
    my $typeRef;

    # loop through the headings, and add the actual heading pattern to the
    # heading fold structure
    for (my $i = 0; $typeRef = $typeMap{$key}[$i]; $i++) {
      ($success, $value) = VIM::Eval("exists('g:TagsParserFoldHeading_" .
        $key . "_" . $typeRef->[0] . "')");
      push(@{$typeMapHeadingFold{$key}}, $typeRef->[1]) if
        ($success == 1 and $value == 1);
    }
  }

  # Init the list of supported filetypes
  VIM::DoCommand "let s:supportedFileTypes = '" .
    join('$\|^', keys %typeMap) . "'";
  VIM::DoCommand "let s:supportedFileTypes = '^' . s:supportedFileTypes . '\$'";

PerlFunc
endfunction
" >>>

call <SID>TagsParserPerlInit()
" >>>

" Functions

" TagsParserPerformOp - Checks that The current file is in the tag path <<<
" Based on the input, it will either open the tag window or tag the file.
" For either op, it will make sure that the current file is within the
" g:TagsParserTagsPath path, and then perform some additional checks based on
" the operation it is supposed to perform
function! <SID>TagsParserPerformOp(op, file)
  if a:file == ""
    let l:pathName = expand("%:p:h")
    let l:fileName = expand("%:t")
    let l:curFile = expand("%:p")
  else
    let l:pathName = fnamemodify(a:file, ":p:h")
    let l:fileName = fnamemodify(a:file, ":t")
    let l:curFile = fnamemodify(a:file, ":p")
  endif

  "Make sure that the file we are working on is _not_ a directory
  if isdirectory(l:curFile)
    return
  endif

  "before we check to see if this file is in within TagsParserTagsPath, do the 
  "simple checks to see if this file name and/or path meet the include or
  "exclude criteria
  "The general logic here is, if the pattern is not empty (therefore not
  "disabled), and an exclude pattern matches, or an include pattern fails to 
  "match, return early.
  if (g:TagsParserDirExcludePattern != "" && l:pathName =~ g:TagsParserDirExcludePattern) || (g:TagsParserFileExcludePattern != "" && l:fileName =~ g:TagsParserFileExcludePattern) || (g:TagsParserDirIncludePattern != "" && l:pathName !~ g:TagsParserDirIncludePattern) || (g:TagsParserFileIncludePattern != "" && l:fileName !~ g:TagsParserFileIncludePattern)
    return
  endif

  if exists("g:TagsParserTagsPath")
    let l:tagPathFileMatch = globpath(g:TagsParserTagsPath, l:fileName)
  
    " Put the path, and file into lowercase if this is windows... Since 
    " windows filenames are case-insensitive.
    if has('win32')
      let l:curFile = tolower(l:curFile)
      let l:tagPathFileMatch = tolower(l:tagPathFileMatch)
    endif

    " See if the file is within the current path
    if stridx(l:tagPathFileMatch, l:curFile) != -1
      if a:op == "tag"
        call <SID>TagsParserTagFile(a:file)
      elseif a:op == "open" && g:TagsParserAutoOpenClose == 1 && filereadable(l:pathName . "/.tags/" .  substitute(l:fileName, " ", "_", "g") . ".tags") && &filetype =~ s:supportedFileTypes
        call <SID>TagsParserOpenTagWindow()
      endif
    endif
  endif
endfunction
" >>>
" TagsParserTagFile - Runs tags on a file and names The tag file <<<
" this function will run Ctags for a file and write it to
" ./.tags/<file>.tags it will also create the ./.tags directory if it doesn't
" exist
function! <SID>TagsParserTagFile(file)
  "if the file argument is empty, make it the current file with fully
  "qualified path
  if a:file == ""
    let l:fileName = expand("%:p")

    "gather any user options that may be defined
    if exists("g:TagsParserCtagsOptions_{&filetype}")
      let l:userOptions = g:TagsParserCtagsOptions_{&filetype}
    else
      let l:userOptions = ""
    endif
  else
    let l:fileName = a:file
    let l:userOptions = ""
  endif

  "cleanup the tagfile, regular file and directory names, we have to replace
  "spaces in the actual file name with underscores for the tag file, or else
  "the sort option throws an error for some reason
  let l:baseDir = substitute(fnamemodify(l:fileName, ":h"), '\', '/', 'g')
  let l:tagDir = substitute(fnamemodify(l:fileName, ":h") . "/.tags", '\', '/', 'g')
  let l:tagFileName = substitute(fnamemodify(l:fileName, ":h") . "/.tags/" . fnamemodify(l:fileName, ":t") . ".tags", '\', '/', 'g')
  let l:fileName = substitute(l:fileName, '\', '/', 'g')

  "make the .tags directory if it doesn't exist yet
  if !isdirectory(l:tagDir)
    exe system("mkdir \"" . l:tagDir . "\"")
    let l:noTagFile = "true"
  elseif !filereadable(l:tagFileName)
    let l:noTagFile = "true"
  else 
    let l:noTagFile = "false"
  endif
  
  "if we are in the C:/WINDOWS/SYSTEM32 dir, change to C.  Odd things seem to
  "happen if we are in the system32 directory
  if has('win32') && getcwd() ==? 'C:\WINDOWS\SYSTEM32'
    let s:cwdChanged = 1
    cd C:\
  else
    let s:cwdChanged = 0
  endif

  "now run the tags program
  exec system(g:TagsParserTagsProgram . " -f \"" . l:tagFileName . "\" " . g:TagsParserCtagsOptions . " " . l:userOptions . " --format=2 --excmd=p --fields=+nS --sort=yes --tag-relative=yes \"" . l:fileName . "\"")

  if s:cwdChanged == 1
    cd C:\WINDOWS\SYSTEM32
  endif

  if filereadable(l:tagFileName)
    let l:tagFileExists = "true"
  else
    let l:tagFileExists = "false"
  endif

  "if this file did not have a .tags/*.tags file up until this point and
  "now it does call <SID>TagsParserExpandTagsPath to get the new file included
  if l:noTagFile == "true" && l:tagFileExists == "true"
    call <SID>TagsParserExpandTagsPath()
  endif
endfunction
" >>>
" TagsParserExpandTagsPath - Expands a directory into a list of tags <<< 
" This will expand The g:TagsParserTagsPath directory list into valid tag
" files
function! <SID>TagsParserExpandTagsPath()
  if !exists("s:OldTagsPath")
    let s:OldTagsPath = &tags
  endif

  if exists("g:TagsParserTagsPath")
    let &tags = join(split(globpath(g:TagsParserTagsPath, '/.tags/*.tags'), '\n'), ",") . "," . s:OldTagsPath
  endif
endfunction
" >>>
" TagsParserSetupDirectoryTags - creates tags for all files in this dir <<<
" This takes a directory as a parameter and creates tag files for all files
" under this directory based on The same include/exclude rules that are used
" when a file is written out.  Except that this function does not need to
" follow the TagsParserPath rules.
function! <SID>TagsParserSetupDirectoryTags(dir)
  "if the TagsParserOff flag is set, print out an error and do nothing
  if g:TagsParserOff != 0
    echomsg "TagsParser cannot tag files in this directory because plugin is turned off"
    return
  endif

  "make sure that a:dir does not contain \\ but contains /
  let l:dir = substitute(expand(a:dir), '\', '/', "g")

  if !isdirectory(l:dir)
    echomsg "Directory provided : " . l:dir . " is not a valid directory"
    return
  endif

  "find all files in this directory and all subdirectories
  let l:fileList = globpath(l:dir . '/**,' . l:dir, '*')

  "now parse those into separate files using Perl and then call the
  "TagFile for each file to give it a tag list
perl << PerlFunc
  use strict;
  use warnings;
  no warnings 'redefine';

  my ($success, $files) = VIM::Eval('l:fileList');
  die "Failed to access list of files to tag" if !$success; 

  foreach my $file (split(/\n/, $files)) {
    VIM::DoCommand "call <SID>TagsParserPerformOp('tag', '" . $file . "')";
  }
PerlFunc
endfunction
" >>>
" TagsParserDisplayTags - This will display The tags for the current file <<<
function! <SID>TagsParserDisplayTags()
  "For some reason the ->Append(), ->Set() and ->Delete() functions don't
  "work unless the Perl buffer object is the current buffer... So, change
  "to the tags buffer.
  let l:tagBufNum = bufnr(g:TagsParserWindowName)
  if l:tagBufNum == -1
    return
  endif

  let l:curBufNum = bufnr("%")

  "now change to the tags window if the two buffers are not the same
  if l:curBufNum != l:tagBufNum
    "if we were not originally in the tags window, we need to save the
    "filetype before we move, otherwise the calling function will have saved
    "it for us
    let s:origFileType = &filetype
    let s:origFileName = expand("%:t")
    let s:origFileTagFileName = expand("%:p:h") . "/.tags/" . expand("%:t") . ".tags"
    let s:origWinNum = winnr()
    exec bufwinnr(l:tagBufNum) . "wincmd w"
  endif

  "before we start drawing the tags window, check for the update flag, and
  "make sure that the filetype we are attempting to display is supported
  if s:tagsDataUpdated == 0 && s:lastFileDisplayed == s:origFileName ||
        \ s:origFileType !~ s:supportedFileTypes
    "we must return to the previous window before we can just exit
    if l:curBufNum != l:tagBufNum
      exec s:origWinNum . "wincmd w"
    endif

    return
  endif

  "before we start editing the contents of the tags window we need to make
  "sure that the tags window is modifiable
  setlocal modifiable

perl << PerlFunc
  use strict;
  use warnings;
  no warnings 'redefine';

  our %typeMap : unique unless (%typeMap);
  our %subTypeMap : unique unless (%subTypeMap);

  # verify that we are able to display the correct file type
  my ($success, $kind) = VIM::Eval('s:origFileType');
  die "Failed to access filetype" if !$success;

  # get the name of the tag file for this file
  ($success, my $tagFileName) = VIM::Eval('s:origFileTagFileName');
  die "Failed to access tag file name ($tagFileName)" if !$success;

  # make sure that %tags is created (or referenced)
  our %tags : unique unless (%tags);

  # temp array to store our tag info... At the end of the file we will check
  # to see if this is different than the globalPrintData, if it is we update
  # the screen, if not then we do nothing so as to maintain any folded sections
  # the user has created.
  my @printData = ( );

  my $printLevel = 0;

  # get the name of the tag file for this file
  ($success, my $fileName) = VIM::Eval('s:origFileName');
  die "Failed to access file name ($fileName)" if !$success;

  # get the sort type flag
  ($success, my $sortType) = VIM::Eval('g:TagsParserSortType');
  die "Failed to access sort type ($sortType)" if !$success;

  # check on how we should display the tags
  ($success, my $dispSig) = VIM::Eval('g:TagsParserDisplaySignature');
  die "Failed to access display signature flag" if !$success;

  sub DisplayEntry {
    my $entryRef = shift(@_);
    my $localPrintLevel = shift(@_);

    # set the display string, tag or signature
    my $dispString;
    if ($dispSig == 1) {
      $dispString = $entryRef->{"pattern"};

      # remove all whitespace from the beginning and end of the display string
      $dispString =~ s/^\s*(.*)\s*$/$1/;
    }
    else {
      $dispString = $entryRef->{"tag"};
    }

    # each tag must have a {{{ at the end of it or else it could mess with the
    # folding... Since there are no end folds each tag must have a fold marker
    push @printData, [ ("\t" x $localPrintLevel) . $dispString .
      " {{{" . ($localPrintLevel + 1), $entryRef ];

    # now print any members there might be
    if (defined($entryRef->{"members"}) and
        defined($subTypeMap{$kind}{$entryRef->{"type"}})) {
      $localPrintLevel++;
      # now print any members that this entry may have, only
      # show types which make sense, so for a "s" entry only
      # display "m", this is based on the subTypeMap data
      foreach my $subTypeRef (@{$subTypeMap{$kind}{$entryRef->{"type"}}}) {
        # for each entry in the subTypeMap for this particular
        # entry, check if there are any entries, if there are print them
        if (defined $entryRef->{"members"}{$subTypeRef->[0]}) {
          # display a header (if one exists)
          if ($subTypeRef->[1] ne "") {
            push @printData, [ ("\t" x $localPrintLevel) . $subTypeRef->[1] .
              " {{{" . ($localPrintLevel + 1) ];
            $localPrintLevel++;
          }
       
          # display the data for this sub type, sort them properly based
          # on the global flag
          if ($sortType eq "alpha") {
            foreach my $member (sort { $a->{"tag"} cmp $b->{"tag"} }
              @{$entryRef->{"members"}{$subTypeRef->[0]}}) {
              DisplayEntry($member, $localPrintLevel);
            }
          }
          else {
            foreach my $member (sort { $a->{"line"} <=> $b->{"line"} }
              @{$entryRef->{"members"}{$subTypeRef->[0]}}) {
              DisplayEntry($member, $localPrintLevel);
            }
          }
       
          # reduce the print level if we increased it earlier
          # and print a fold end marker
          if ($subTypeRef->[1] ne "") {
            $localPrintLevel--;
          }
        }
      }
      $localPrintLevel--;
    }
  }

  # at the very top, print out the filename and a blank line
  push @printData, [ "$fileName {{{" . ($printLevel + 1) ];
  push @printData, [ "" ];
  $printLevel++;

  foreach my $ref (@{$typeMap{$kind}}) {
    # verify that there are any entries defined for this particular tag
    # type before we start trying to print them and that they don't have a
    # parent tag.

    my $printTopLevelType = 0;
    foreach my $typeCheckRef (@{$tags{$tagFileName}{$ref->[0]}}) {
      $printTopLevelType = 1 if !defined($typeCheckRef->{"parent"});
    }
     
    if ($printTopLevelType == 1) {
      push @printData, [ ("\t" x $printLevel) . $ref->[1] . " {{{" .
        ($printLevel + 1) ] ;
    
      $printLevel++;
      # now display all the tags for this particular type, and sort them
      # according to the sortType
      if ($sortType eq "alpha") {
        foreach my $tagRef (sort { $a->{"tag"} cmp $b->{"tag"} }
          @{$tags{$tagFileName}{$ref->[0]}}) {
          unless (defined $tagRef->{"parent"}) {
            DisplayEntry($tagRef, $printLevel);
          }
        }
      }
      else {
        foreach my $tagRef (sort { $a->{"line"} <=> $b->{"line"} }
          @{$tags{$tagFileName}{$ref->[0]}}) {
          unless (defined $tagRef->{"parent"}) {
            DisplayEntry($tagRef, $printLevel);
          }
        }
      }
      $printLevel--;

      # between each listing put a line
      push @printData, [ "" ];
    }
  }

  # this hash will be used to keep all of the data referenceable... So that we
  # will be able to print the correct information, reach that info when the tag
  # is to be selected, and find the current tag that the cursor is on in the
  # main window
  our @globalPrintData : unique = ( ) unless(@globalPrintData);

  # check the last file displayed... If it is blank then this is a forced
  # update
  ($success, my $lastFileDisplayed) = VIM::Eval('s:lastFileDisplayed');
  die "Failed to access last file displayed" if !$success;

  # check to see if the data has changed
  my $update = 1;
  if (($lastFileDisplayed ne "") and ($#printData == $#globalPrintData)) {
    $update = 0;
    for ( my $index = 0; $index <= $#globalPrintData; $index++ ) {
      if ($printData[$index][0] ne $globalPrintData[$index][0]) {
        $update = 1;
      }
      # no matter if the display data changed or not, make sure to assign the
      # tag reference to the global data... Otherwise things like line numbers
      # may have changed and the tag window would not have the proper data
      $globalPrintData[$index][1] = $printData[$index][1];
    }
  }

  # if the data did not change, do nothing and quit
  if ($update == 1) {
    # set the globalPrintData array to the new print data contents
    @globalPrintData = @printData;

    # first clean the window
    $main::curbuf->Delete(1, $main::curbuf->Count());

    # set the first line
    $main::curbuf->Set(1, "");

    # append the rest of the data into the window, if this line looks
    # frightening, do a "perldoc perllol" and look at the Slices section
    $main::curbuf->Append(1, map { $printData[$_][0] } 0 .. $#printData);
  }

  # if the fold level is not set, go through the window now and fold any
  # tags that have members
  ($success, my $foldLevel) = VIM::Eval('exists("g:TagsParserFoldLevel")');
  $foldLevel = -1 if($success == 0 || $foldLevel == 0);

  our %typeMapHeadingFold : unique = ( ) unless(%typeMapHeadingFold);

  FOLD_LOOP:
  for (my $index = 0; my $line = $globalPrintData[$index]; $index++) {
    # if this is a tag that has a parent and members, fold it
    if (($foldLevel == -1) and (defined $line->[1]) and
        (defined $line->[1]{"members"}) and (defined $line->[1]{"parent"})) {
      VIM::DoCommand("if foldclosed(" . ($index + 2) . ") == -1 | " .
                     ($index + 2) . "foldclose | endif");
    }
    # we should fold all tags which only have members with empty headings
    elsif (($foldLevel == -1) and (defined $line->[1]{"members"})) {
      foreach my $memberKey (keys %{$line->[1]{"members"}}) {
        foreach my $possibleType (@{$subTypeMap{$kind}{$line->[1]{"type"}}}) {
          # immediately skip to the next loop iteration if we find that a
          # member exists for this tag which contains a non-empty heading
          next FOLD_LOOP if (($memberKey eq $possibleType->[0]) and
                             ($possibleType->[1] ne ""));
        }
      }

      # if we made it this far then this tag should be folded
      VIM::DoCommand("if foldclosed(" . ($index + 2) . ") == -1 | " .
                     ($index + 2) . "foldclose | endif");
    }
    # lastly, if this is a heading which has been marked for folding, fold it
    elsif ((defined $typeMapHeadingFold{$kind}) and
           (not defined $line->[1]) and ($line->[0] =~ /^\s+- .* {{{\d+$/)) {
      foreach my $heading (@{$typeMapHeadingFold{$kind}}) {
        VIM::DoCommand("if foldclosed(" . ($index + 2) . ") == -1 | " .
                       ($index + 2) . "foldclose | endif")
          if ($line->[0] =~ /^\s+$heading {{{\d+$/);
      }
    }
  }
PerlFunc

  "before we go back to the previous window, mark this one as not
  "modifiable, but only if this is currently the tags window
  setlocal nomodifiable

  "mark the update flag as false, and the last file we displayed as what we
  "just worked through
  let s:tagsDataUpdated = 0
  let s:lastFileDisplayed = s:origFileName

  "mark the last tag selected as not folded so accidental folding does not
  "occur
  let s:matchedTagWasFolded = 0

  "go back to the window we were in before moving here, if we were not
  "originally in the tags buffer
  if l:curBufNum != l:tagBufNum
    exec s:origWinNum . "wincmd w"

    if g:TagsParserHighlightCurrentTag == 1
      call <SID>TagsParserHighlightTag(1)
    endif
  endif
endfunction
" >>>
" TagsParserParseCurrentFile - parses The tags file for the current file <<<
" This takes the current file, parses the tag file (if it has not been
" parsed yet, or the tag file has been updated), and saves it into a global
" Perl hash struct for use by the function which prints out the data
function! <SID>TagsParserParseCurrentFile()
  "get the name of the tag file to parse, for the tag file name itself,
  "replace any spaces in the original filename with underscores
  let l:tagFileName = expand("%:p:h") . "/.tags/" . expand("%:t") . ".tags"

  "make sure that the tag file exists before we start this
  if !filereadable(l:tagFileName)
    return
  endif
  
perl << PerlFunc
  use strict;
  use warnings;
  no warnings 'redefine';

  use File::stat;

  # use local to keep %tags available for other functions
  our %tags : unique unless (%tags);
  our %tagMTime : unique unless (%tagMTime);
  our %tagsByLine : unique unless(%tagsByLine);
  
  # get access to the tag file and check it's last modify time
  my ($success, $tagFile) = VIM::Eval('l:tagFileName');
  die "Failed to access tag file variable ($tagFile)" if !$success;

  my $tagInfo = stat($tagFile);
  die "Failed to stat $tagFile" if !$tagInfo;

  # initialize the last modify time if it has not been accessed yet
  $tagMTime{$tagFile} = 0 if !defined($tagMTime{$tagFile});

  # if this file has been tagged before and the tag file has not been
  # updated, just exit
  if ($tagInfo->mtime <= $tagMTime{$tagFile}) {
    VIM::DoCommand "let s:tagsDataUpdated = 0";
    return;
  }
  $tagMTime{$tagFile} = $tagInfo->mtime;
  VIM::DoCommand "let s:tagsDataUpdated = 1";

  # if the tag entries are defined already for this file, delete them now
  delete $tags{$tagFile} if defined($tags{$tagFile});

  # open up the tag file and read the data
  open(TAGFILE, "<", $tagFile) or die "Failed to open tagfile $tagFile";
  while(<TAGFILE>) {
    next if /^!_TAG.*/;
    # process the data
    chomp;

    # split the stuff around the pattern with tabs, and remove the pattern
    # using the special separator ;" character sequence to guard against the
    # possibility of embedded tabs in the pattern
    my ($tag, $file, $rest) = split(/\t/, $_, 3);
    (my $pattern, $rest) = split(/;"\t/, $rest, 2);
    my ($type, $fields) = split(/\t/, $rest, 2);

    # cleanup pattern to remove the / /;" from the beginning and end of the
    # tag search pattern, the hard part is that sometimes the $ may not be at
    # the end of the pattern
    if ($pattern =~ m|/\^(.*)\$/|) {
      $pattern = $1;
    }
    else {
      $pattern =~ s|/\^(.*)/|$1|;
    }

    # there may be some escaped /'s in the pattern, un-escape them
    $pattern =~ s|\\/|/|g;

    # if the " file:" tag is here, remove it, we want it to be in the file
    # since Vim can use the file: field to know if something is file static,
    # but we don't care about it much for this script, and it messes up my
    # hash creation
    $fields =~ s/\tfile://;

    push @{$tags{$tagFile}{$type}}, { "tag", $tag, "type", $type, "pattern",
                                      $pattern, split(/\t|:/, $fields) };
  }
  close(TAGFILE);

  # before worrying about anything else, make up a line number-oriented hash of
  # the tags, this will make finding a match, or what the current tag is easier
  delete $tagsByLine{$tagFile} if defined($tagsByLine{$tagFile});

  while (my ($key, $typeArray) = each %{$tags{$tagFile}}) {
    foreach my $tagEntry (@{$typeArray}) {
      push @{$tagsByLine{$tagFile}{$tagEntry->{"line"}}}, $tagEntry;
    }
  }

  # setup the kind mappings for types that have member-types
  our %adaKinds : unique = ( P => "packspec",
                             p => "package",
                             T => "typespec",
                             t => "type",
                             U => "subspec",
                             u => "subtype",
                             c => "component",
                             l => "literal",
                             V => "varspec",
                             v => "variable",
                             n => "constant",
                             x => "exception",
                             f => "formal",
                             R => "subprogspec",
                             r => "subprogram",
                             K => "taskspec",
                             k => "task",
                             O => "protectspec",
                             o => "protected",
                             E => "entryspec",
                             e => "entry",
                             b => "label",
                             i => "identifier",
                             a => "autovar",
                             y => "annon" ) unless(%adaKinds);
  
  our %cKinds : unique = ( c => "class",
                           g => "enum",
                           n => "namespace",
                           s => "struct",
                           u => "union" ) unless(%cKinds);

  # define the kinds which we can map in a hierarchical fashion
  our %kindMap : unique = ( ada => \%adaKinds,
                            c => \%cKinds,
                            h => \%cKinds,
                            cpp => \%cKinds ) unless(%kindMap);

  ($success, my $kind) = VIM::Eval('&filetype');
  die "Failed to access current file type" if !$success;

  ($success, my $noNestedTags) = VIM::Eval('g:TagsParserNoNestedTags');
  die "Failed to access the nested tag display flag" if !$success;

  # parse the data we just read into hierarchies... If we don't have a
  # kind hash entry for the current file type, just skip the rest of this
  # function
  return if (not defined($kindMap{$kind}) or $noNestedTags == 1);

  # for each key, sort it's entries.  These are the tags for each tag,
  # check for any types which have a scope, and if they do, reference that type
  # to the correct parent type
  #
  # yeah, this loop sucks, but I haven't found a more efficient way to do
  # it yet
  foreach my $key (keys %{$tags{$tagFile}}) {
    foreach my $tagEntry (@{$tags{$tagFile}{$key}}) {
      while (my ($tagType, $tagTypeName) = each %{$kindMap{$kind}}) {
        # search for any member types of the current tagEntry, but only if
        # such a member is defined for the current tag
        if (defined($tagEntry->{$tagTypeName}) and
            defined($tags{$tagFile}{$tagType})) {
          # sort the possible member entries by line number so that when
          # looking for the parent entry we are sure to only get the one who's
          # line is just barely less than the current tag's line
          FIND_PARENT:
          foreach my $tmpEntry (sort { $b->{"line"} <=> $a->{"line"} }
            @{$tags{$tagFile}{$tagType}}) {
            # for the easiest way to do this, only consider tags a match if
            # the line number of the possible parent tag is less than or equal
            # to the line number of the current tagEntry
            if (($tmpEntry->{"tag"} eq $tagEntry->{$tagTypeName}) and
              ($tmpEntry->{"line"} <= $tagEntry->{"line"})) {
              # push a reference to the current tag onto the parent tag's
              # member stack
              push @{$tmpEntry->{"members"}{$key}}, $tagEntry;
              $tagEntry->{"parent"} = $tmpEntry;
              last FIND_PARENT;
            }
          }
        }
      }
    }
  }

  # processing those local vars for C
  if (($kind =~ /c|h|cpp/) and (defined $tags{$tagFile}{"l"}) and
    (defined $tags{$tagFile}{"f"})) {
    # setup a reverse list of local variable references sorted by line
    my @vars = sort { $b->{"line"} <=> $a->{"line"} } @{$tags{$tagFile}{"l"}};

    # sort the functions by reversed line entry... Then we will go through the
    # list of local variables until we find one who's line number exceeds that
    # of the functions.  Then we unshift the array and go to the next function
    FUNC: foreach my $funcRef (sort { $b->{"line"} <=> $a->{"line"} }
      @{$tags{$tagFile}{"f"}}) {
      VAR: while (my $varRef = shift @vars) {
        if ($varRef->{"line"} >= $funcRef->{"line"}) {
          push @{$funcRef->{"members"}{"l"}}, $varRef;
          $varRef->{"parent"} = $funcRef;
          next VAR;
        }
        else {
          unshift(@vars, $varRef);
          next FUNC;
        }
      }
    }
  }
PerlFunc
endfunction
" >>>
" TagsParserOpenTagWindow - Opens up The tag window <<<
function! <SID>TagsParserOpenTagWindow()
  "ignore events while opening the tag window
  let l:oldEvents = &eventignore
  set eventignore=all

  "save the window number and potential tag file name for the current file
  let s:origFileName = expand("%:t")
  let s:origFileTagFileName = expand("%:p:h") . "/.tags/" . expand("%:t") . ".tags"
  let s:origWinNum = winnr()
  "before we move to the new tags window, we must save the type of file
  "that we are currently in
  let s:origFileType = &filetype

  "parse the current file
  call <SID>TagsParserParseCurrentFile()

  "open the tag window
  if !bufloaded(g:TagsParserWindowName)
    if g:TagsParserNoResize == 0
      "track the current window size, so that when we close the tags tab, 
      "if we were not able to resize the current window, that we don't 
      "decrease it any more than we increased it when we opened the tab
      let s:origColumns = &columns
      "open the tag window, + 1 for the split divider
      let &columns = &columns + g:TagsParserWindowSize + 1
      let s:columnsAdded = &columns - s:origColumns
      let s:newColumns = &columns
    endif

    exec s:TagsWindowPosition . " " . g:TagsParserWindowSize  . " split " .
          \ g:TagsParserWindowName

    "settings to keep the buffer from interfering with anything else
    setlocal nonumber
    setlocal nobuflisted
    setlocal noswapfile
    setlocal buftype=nofile
    setlocal bufhidden=delete

    if v:version >= 700
      setlocal nospell
    endif

    "formatting related settings
    setlocal nowrap
    setlocal tabstop=2

    "fold related settings
    if exists('g:TagsParserFoldLevel')
      let l:foldLevelString = 'setlocal foldlevel=' . g:TagsParserFoldLevel
      exec l:foldLevelString
    else
      "if the foldlevel is not defined, default it to something large so that
      "the default folding method takes over
      setlocal foldlevel=100
    endif

    "only turn the fold column on if the disabled flag is not set
    if g:TagsParserFoldColumnDisabled == 0
      setlocal foldcolumn=3
    endif

    setlocal foldenable
    setlocal foldmethod=marker
    setlocal foldtext=TagsParserFoldFunction()
    setlocal fillchars=fold:\ 

    "if the highlight tag option is on, reduce the updatetime... But not too
    "much because it is global and it could impact overall VIM performance
    if g:TagsParserHighlightCurrentTag == 1
      setlocal updatetime=1000
    endif
    
    "command to go to tag in previous window:
    nmap <buffer> <silent> <CR> :call <SID>TagsParserSelectTag()<CR>
    nmap <buffer> <silent> <2-LeftMouse>
          \ :call <SID>TagsParserSelectTag()<CR>

    "the augroup we are going to setup will override this initial
    "autocommand so stop it from running
    autocmd! TagsParserBufWinEnterWindowNotOpen

    "setup and autocommand so that when you enter a new buffer, the new file is
    "parsed and then displayed
    augroup TagsParserBufWinEnterEvents
      autocmd!
      autocmd WinEnter ?* call <SID>TagsParserStoreWindowID(expand("<afile>"))
      autocmd BufWinEnter ?* call <SID>TagsParserHandleBufWinEnter()
      
      "when a file is written, add an event so that the new tag file is parsed
      "and displayed (if there are updates)
      autocmd BufWritePost ?* call <SID>TagsParserParseCurrentFile() |
            \ call <SID>TagsParserDisplayTags()

      "properly handle the WinLeave event
      autocmd BufWinLeave ?* call <SID>TagsParserHandleBufWinLeave()

      "make sure that we don't accidentally close the Vim session when loading
      "up a new buffer
      autocmd BufAdd * let s:newBufBeingCreated = 1
    augroup end

    "add the event to do the auto tag highlighting if the event is set
    if g:TagsParserHighlightCurrentTag == 1
      augroup TagsParserCursorHoldEvent
        autocmd!
        autocmd CursorHold ?* call <SID>TagsParserHighlightTag(0)
      augroup end
    endif

    if !hlexists('TagsParserFileName')
      hi link TagsParserFileName Underlined
    endif

    if !hlexists('TagsParserTypeName')
      hi link TagsParserTypeName Special 
    endif

    if !hlexists('TagsParserTag')
      hi link TagsParserTag Normal
    endif

    if !hlexists('TagsParserFoldMarker')
      hi link TagsParserFoldMarker Ignore
    endif

    if !hlexists('TagsParserHighlight')
      hi link TagsParserHighlight ToDo
    endif

    "setup the syntax for the tags window
    syntax match TagsParserTag '\(- \)\@<!\S\(.*\( {{{\)\@=\|.*\)'
          \ contains=TagsParserFoldMarker
    syntax match TagsParserFileName '^\w\S*'
    syntax match TagsParserTypeName '^\t*- .*' contains=TagsParserFoldMarker
    syntax match TagsParserFoldMarker '{{{.*\|\s*}}}'
  endif

  "display the tags
  call <SID>TagsParserDisplayTags()

  "go back to the previous window, find the winnr for the buffer, and
  "do a :<N>wincmd w
  exec s:origWinNum . "wincmd w"

  "un ignore events 
  let &eventignore=l:oldEvents
  unlet l:oldEvents
  
  "highlight the current tag (if flag is set)
  if g:TagsParserHighlightCurrentTag == 1
    call <SID>TagsParserHighlightTag(1)
  endif
endfunction
" >>>
" TagsParserCloseTagWindow - Closes The tags window <<<
function! <SID>TagsParserCloseTagWindow()
  "ignore events while opening the tag window
  let l:oldEvents = &eventignore
  set eventignore=all

  "if the window exists, find it and close it
  if bufloaded(g:TagsParserWindowName)
    "save current file winnr
    let l:curWinNum = winnr()

    "save the current tags window size
    let l:tagsWindowSize = winwidth(bufwinnr(g:TagsParserWindowName))
 
    "go to and close the tags window
    exec bufwinnr(g:TagsParserWindowName) . "wincmd w"
    close

    if g:TagsParserNoResize == 0
      "resize the Vim window
      if g:TagsParserWindowSize == l:tagsWindowSize && s:newColumns == &columns
        let &columns = &columns - s:columnsAdded
      else
        "if the window sizes have been changed since the window was opened,
        "attempt to save the new sizes to use later
        let g:TagsParserWindowSize = l:tagsWindowSize
        let &columns = &columns - g:TagsParserWindowSize - 1
      endif
    endif
    
    "now go back to the file we were just in assuming it wasn't the
    "tags window in which case this will simply fail silently
    exec l:curWinNum . "wincmd w"

    "zero out the last file displayed variable so that if the tags window is
    "reopened then the tags should be redrawn
    let s:lastFileDisplayed = ""

    "remove all buffer related autocommands
    autocmd! TagsParserBufWinEnterEvents
    autocmd! TagsParserCursorHoldEvent

    augroup TagsParserBufWinEnterWindowNotOpen
      autocmd BufWinEnter ?* call <SID>TagsParserPerformOp("open")
    augroup end
  endif
  
  "un ignore events 
  let &eventignore=l:oldEvents
  unlet l:oldEvents
endfunction
" >>>
" TagsParserToggle - Will toggle The tags window open or closed <<<
function! <SID>TagsParserToggle()
  "if the TagsParserOff flag is set, print out an error and do nothing
  if g:TagsParserOff != 0
    echomsg "TagsParser window cannot be opened because plugin is turned off"
    return
  elseif g:TagsParserNoTagWindow == 1
    echomsg "TagsParser window cannot be opened because the Tag window has been disabled by the g:TagsParserNoTagWindow variable"
    return
  endif

  "check to see if the tags window is loaded, if it is not, open it, if it
  "is, close it
  if bufloaded(g:TagsParserWindowName)
    "if the tags parser is forced closed, turn off the auto open/close flag
    if g:TagsParserAutoOpenClose == 1
      let g:TagsParserAutoOpenClose = 0
      let s:autoOpenCloseTurnedOff = 1
    endif

    call <SID>TagsParserCloseTagWindow()
  else
    if exists("s:autoOpenCloseTurnedOff") && s:autoOpenCloseTurnedOff == 1
      let g:TagsParserAutoOpenClose = 1
      let s:autoOpenCloseTurnedOff = 0
    endif
    call <SID>TagsParserOpenTagWindow()
  endif
endfunction
" >>>
" TagsParserHandleBufWinEnter - handles The BufWinEnter event <<<
function! <SID>TagsParserHandleBufWinEnter()
  "clear out the new buf flag
  let s:newBufBeingCreated = 0

  "if the buffer we just entered is unmodifiable do nothing and return
  if &modifiable == 0
    return
  endif

  "if the auto open/close flag is set, see if there is a tag file for the
  "new buffer, if there is, call open, otherwise, call close
  if g:TagsParserAutoOpenClose == 1
    let l:tagFileName = expand("%:p:h") . "/.tags/" .
          \ substitute(expand("%:t"), " ", "_", "g") . ".tags"

    if !filereadable(l:tagFileName)
      call <SID>TagsParserCloseTagWindow()
    else
      call <SID>TagsParserOpenTagWindow()
    endif
  else
    "else parse the current file and call display tags
    call <SID>TagsParserParseCurrentFile()
    call <SID>TagsParserDisplayTags()
  endif
endfunction
">>>
" TagsParserHandleBufWinLeave - handles The BufWinLeave event <<<
function! <SID>TagsParserHandleBufWinLeave()
  "if there is only the tags buffer left showing after this window exits,
  "exit VIM
  if g:TagsParserAutoOpenClose == 1 && bufnr("%") != bufnr(g:TagsParserWindowName) && bufloaded(g:TagsParserWindowName) && winbufnr(3) == -1 && s:newBufBeingCreated == 0
    call <SID>TagsParserCloseTagWindow()
    qall
  else
    "if we are unloading the tags window, and the auto open/close flag is on,
    "turn it off
    if bufname("%") == g:TagsParserWindowName
      if g:TagsParserAutoOpenClose == 1
        let g:TagsParserAutoOpenClose = 0
        let s:autoOpenCloseTurnedOff = 1
      endif

      "if the buffer explorer work around is enabled, we don't need to manually
      "close the window here
      if g:TagsParserBufExplWorkAround == 0
        call <SID>TagsParserCloseTagWindow()
      endif
    endif

    "close the tag window when cycling buffers if the buffer explorer work
    "around is enabled
    if g:TagsParserBufExplWorkAround == 1
      call <SID>TagsParserCloseTagWindow()
    endif
  endif
endfunction
">>>
" TagsParserSelectTag - activates a tag (if it is a tag) <<<
function! <SID>TagsParserSelectTag()
  "ignore events while selecting a tag
  let l:oldEvents = &eventignore
  set eventignore=all

  "clear out any previous match
  if s:matchedTagWasFolded == 1
    exec s:matchedTagFoldStart . "," . s:matchedTagFoldEnd . "foldclose"
    let s:matchedTagWasFolded = 0
  endif

  match none

perl << PerlFunc
  use strict;
  use warnings;
  no warnings 'redefine';

  my ($success, $lineNum) = VIM::Eval('line(".")');
  die "Failed to access The current line" if !$success;

  our @globalPrintData : unique unless(@globalPrintData);

  # subtract 2 (1 for the append offset, and 1 because it starts at 0) from
  # the line number to get the proper globalPrintData index
  my $indexNum = $lineNum - 2;

  # if this is a tag, there will be a reference to the correct tag entry in
  # the referenced globalPrintData array
  if (defined $globalPrintData[$indexNum][1]) {
    # if this line is folded, unfold it
    ($success, my $folded) = VIM::Eval("foldclosed($lineNum)");
    die "Failed to verify if $lineNum is folded" if !$success;

    if ($folded != -1) {
      ($success, my $foldEnd) = VIM::Eval("foldclosedend($lineNum)");
      die "Failed to retrieve end of fold for line $lineNum" if !$success;

      VIM::DoCommand "let s:matchedTagFoldStart = $folded";
      VIM::DoCommand "let s:matchedTagFoldEnd = $foldEnd";

      VIM::DoCommand "let s:matchedTagWasFolded = 1";
      VIM::DoCommand $folded . "," . $foldEnd . "foldopen";
    }
    else {
      VIM::DoCommand "let s:matchedTagWasFolded = 0";
    }

    # now match this tag
    VIM::DoCommand 'match TagsParserHighlight /\%' . $lineNum .
      'l\S.*\( {{{\)\@=/';

    # go to the proper window, go the correct line, unfold it (if necessary),
    # move to the correct word (the tag) and finally, set a mark
    VIM::DoCommand 'exec s:origWinNum . "wincmd w"';
    VIM::DoCommand $globalPrintData[$indexNum][1]{"line"};

    # get the current line
    ($success, my $curLine) = VIM::Eval("getline('.')");
    die "Failed to access current line " if !$success;

    # now get the index
    my $position = index $curLine, $globalPrintData[$indexNum][1]{"tag"};

    # move to that column if we got a valid value
    VIM::DoCommand("exec 'normal 0" . $position . "l'") if ($position != -1);

    VIM::DoCommand "if foldclosed('.') != -1 | .foldopen | endif";
    VIM::DoCommand "normal m\'";
  }
  else {
    # otherwise we should just toggle this fold open/closed if the line is
    # actually folded
    VIM::DoCommand "if foldclosed('.') != -1 | .foldopen | else | .foldclose | endif";
  }
PerlFunc

  "un ignore events 
  let &eventignore=l:oldEvents
  unlet l:oldEvents
endfunction
" >>>
" TagsParserHighlightTag - highlights The tag that the cursor is on <<<
function! <SID>TagsParserHighlightTag(resetCursor)
  "if this buffer is unmodifiable, do nothing
  if &modifiable == 0
    return
  endif

  "get the current and tags buffer numbers
  let l:curBufNum = bufnr("%")
  let l:tagBufNum = bufnr(g:TagsParserWindowName)

  "return if the tags buffer is not open or this is the tags window we are
  "currently in
  if l:tagBufNum == -1 || l:curBufNum == l:tagBufNum
    return
  endif

  "yank the word under the cursor into register a, and make sure to place the
  "cursor back in the right position
  exec 'normal ma"ayiw`a'
  
  let l:curWinNum = winnr()
  let l:curPattern = getline(".")
  let l:curLine = line(".")
  let l:curWord = getreg("a")

  "ignore events before changing windows
  let l:oldEvents = &eventignore
  set eventignore=all

  "goto the tags window
  exec bufwinnr(l:tagBufNum) . "wincmd w"
  
  "clear out any previous match
  if s:matchedTagWasFolded == 1
    exec s:matchedTagFoldStart . "," . s:matchedTagFoldEnd . "foldclose"
    let s:matchedTagWasFolded = 0
  endif
  let s:matchedTagLine = 0

  match none

perl << PerlFunc
  use strict;
  use warnings;
  no warnings 'redefine';

  # find the current word and line
  my ($success, $curPattern) = VIM::Eval('l:curPattern');
  die "Failed to access current pattern" if !$success;

  ($success, my $curLine) = VIM::Eval('l:curLine');
  die "Failed to access current line" if !$success;

  # the "normal mayiw`a" command above yanked the word under the cursor into
  # register a
  ($success, my $curWord) = VIM::Eval('l:curWord');
  die "Failed to access current word" if !$success;

  # get the name of the tag file for this file
  ($success, my $tagFileName) = VIM::Eval('s:origFileTagFileName ');
  die "Failed to access file name ($tagFileName)" if !$success;

  our @globalPrintData : unique unless (@globalPrintData);
  our %tagsByLine : unique unless(%tagsByLine);

  my $easyRef = undef;
  my $trueRef = undef;

  # now look up this tag
  if (defined $tagsByLine{$tagFileName}{$curLine}) {
    my $count = 1;
    TRUE_REF_SEARCH:
    foreach my $ref (@{$tagsByLine{$tagFileName}{$curLine}}) {
      if (substr($curPattern, 0, length($ref->{"pattern"})) eq
          $ref->{"pattern"}) {
        if ($curWord eq $ref->{"tag"}) {
          $trueRef = $ref;
          last TRUE_REF_SEARCH;
        }
        else {
          $easyRef = $ref;
        }
      } # if (substr($curPattern, 0, length($ref->{"pattern"})) eq ...
    } # TRUE_REF_SEARCH: ...

    # if we didn't find an exact match go with the default match
    $trueRef = $easyRef if (not defined($trueRef));

    # now we have to find the correct line for this tag in the globalPrintData
    my $index = 0;
    while (my $line = $globalPrintData[$index++]) {
      if (defined $line->[1] and $line->[1] == $trueRef) {
        my $tagLine = $index + 1;
      
        # if this line is folded, unfold it
        ($success, my $folded) = VIM::Eval("foldclosed($tagLine)");
        die "Failed to verify if $tagLine is folded" if !$success;

        if ($folded != -1) {
          ($success, my $foldEnd) = VIM::Eval("foldclosedend($tagLine)");
          die "Failed to retreive end of fold for line $tagLine" if !$success;

          VIM::DoCommand "let s:matchedTagFoldStart = $folded";
          VIM::DoCommand "let s:matchedTagFoldEnd = $foldEnd";

          VIM::DoCommand "let s:matchedTagWasFolded = 1";
          VIM::DoCommand $folded . "," . $foldEnd . "foldopen";
        }
        else {
          VIM::DoCommand "let s:matchedTagWasFolded = 0";
        }

        # now match this tag
        VIM::DoCommand 'match TagsParserHighlight /\%' . $tagLine .
          'l\S.*\( {{{\)\@=/';
     
        # now that the tag has been highlighted, go to the tag and make the line
        # visible, and then go back to the tag line so that the cursor is in the
        # correct place
        VIM::DoCommand $tagLine;
        VIM::DoCommand "exec winline()";
        VIM::DoCommand $tagLine;

        last;
      } # if ($line->[1] == $trueRef) {
    } # while (my $line = $globalPrintData[$index++]) {
  } # if (defined $tagsByLine{$tagFileName}{$curLine}) {
PerlFunc
  
  "before we go back to the previous window... Check if we found a match.  If
  "we did not, and the resetCursor parameter is 1 then move the cursor to the
  "top of the file
  if a:resetCursor == 1 && s:matchedTagLine == 0
    exec 1
    exec winline()
    exec 1
  endif

  "go back to the old window
  exec l:curWinNum . "wincmd w"

  "un ignore events 
  let &eventignore=l:oldEvents
  unlet l:oldEvents

  return
endfunction
">>>
" TagsParserFoldFunction - function to make proper tags for folded tags <<<
function! TagsParserFoldFunction()
  let l:line = getline(v:foldstart)
  let l:tabbedLine = substitute(l:line, "\t", "  ", "g")
  let l:finishedLine = substitute(l:tabbedLine, " {{{.*", "", "")
  let l:numLines = v:foldend - v:foldstart
  return l:finishedLine . " : " . l:numLines . " lines"
endfunction
" >>>
" TagsParserOff - function to turn off all TagsParser functionality <<<
function! <SID>TagsParserOff()
  "only do something if The TagsParser is not off already
  if g:TagsParserOff == 0
    "to turn off the TagsParser, call the TagsParserCloseTagWindow() function,
    "which will uninstall all autocommands except for the default
    "TagsParserAutoCommands group (which is always on) and the
    "TagsParserBufWinEnterWindowNotOpen group (which is on when the window is
    "closed)
    call <SID>TagsParserCloseTagWindow()
    
    autocmd! TagsParserAutoCommands
    autocmd! TagsParserBufWinEnterWindowNotOpen

    "finally, set the TagsParserOff flag to 1
    let g:TagsParserOff = 1
  endif
endfunction
" >>>
" TagsParserOn - function to turn all TagsParser functionality back on <<<
function! <SID>TagsParserOn()
  "only do something if The TagsParser is off
  if g:TagsParserOff != 0 && g:TagsParserNoTagWindow == 0
    augroup TagsParserAutoCommands
      autocmd!
      "setup an autocommand that will expand the path described by
      "g:TagsParserTagsPath into a valid tag path
      autocmd VimEnter * call <SID>TagsParserExpandTagsPath() |
            \ call <SID>TagsParserPerformOp("open")

      "setup an autocommand so that when a file is written to it writes a tag
      "file if it a file that is somewhere within the tags path or the
      "g:TagsParserTagsPath path
      autocmd BufWritePost ?* call <SID>TagsParserPerformOp("tag")
    augroup end

    augroup TagsParserBufWinEnterWindowNotOpen
      autocmd BufWinEnter ?* call <SID>TagsParserPerformOp("open")
    augroup end
  elseif g:TagsParserOff != 0 && g:TagsParserNoTagWindow == 1
    augroup TagsParserAutoCommands
      autocmd!
      "setup an autocommand that will expand the path described by 
      "g:TagsParserTagsPath into a valid tag path
      autocmd VimEnter * call <SID>TagsParserExpandTagsPath()

      "setup an autocommand so that when a file is written to it writes a tag
      "file if it a file that is somewhere within the tags path or the
      "g:TagsParserTagsPath path
      autocmd BufWritePost ?* call <SID>TagsParserPerformOp("tag")
    augroup end
  endif
  let g:TagsParserOff = 0
endfunction
" >>>
" TagsParserCOpen - opens The quickfix window nicely <<<
function! <SID>TagsParserCOpen(...)
  let l:windowClosed = 0

  "if the tag window is open, close it
  if bufloaded(g:TagsParserWindowName) && s:TagsWindowPosition =~ "vertical"
    call <SID>TagsParserCloseTagWindow()
    let l:windowClosed = 1
  endif

  "get the current window number
  let l:curBuf = bufnr("%")

  "now open the quickfix window
  if(a:0 == 1)
    exec "copen " . a:1
  else
    exec "copen"
  endif

  "go back to the original window
  exec bufwinnr(l:curBuf) . "wincmd w"

  "go to the first error
  exec "cfirst"

  "reopen the tag window if necessary
  if l:windowClosed == 1
    call <SID>TagsParserOpenTagWindow()
  endif
endfunction
" >>>
" TagsParserCWindow - opens The quickfix window nicely <<<
function! <SID>TagsParserCWindow(...)
  let l:windowClosed = 0

  "if the tag window is open, close it
  if bufloaded(g:TagsParserWindowName) && s:TagsWindowPosition =~ "vertical"
    call <SID>TagsParserCloseTagWindow()
    let l:windowClosed = 1
  endif

  "get the current window number
  let l:curBuf = bufnr("%")

  "now open the quickfix window
  if(a:0 == 1)
    exec "cwindow " . a:1
  else
    exec "cwindow"
  endif
  
  "go back to the original window, if we actually changed windows
  if l:curBuf != bufnr("%")
    exec bufwinnr(l:curBuf) . "wincmd w"

    "go to the first error
    exec "cfirst"
  endif

  "reopen the tag window if necessary
  if l:windowClosed == 1
    call <SID>TagsParserOpenTagWindow()
  endif
endfunction
" >>>
" TagsParserStoreWindowID - stores The new window ID of the current file <<<
function! <SID>TagsParserStoreWindowID(bufName)
  if a:bufName == s:origFileName && winnr() != s:origWinNum
    let s:origWinNum = winnr()
  endif
endfunction
" >>>

let &cpo = s:cpoSave
unlet s:cpoSave

" vim:ft=Vim:fdm=marker:ff=unix:wrap:ts=2:sw=2:sts=2:sr:et:fmr=<<<,>>>:fdl=0
ctags/ada.c
2070
/*
 * File:          Ada.c
 * Description:   Enables extended Ada parsing support in Exuberant Ctags
 * Version:       0.2
 * Author:        A. Aaron Cornelius (ADotAaronDotCorneliusAtgmailDotcom)
 *
 * Installation:
 * You must have the Exuberant Ctags source to install this parser.  Once you
 * have the source, place this file into the directory with the rest of the
 * ctags source.  Then compile and install ctags as normal (usually:
 * './configure', './make', './make install').
 *
 * Changelog:
 * 06/02/2006 - Added missing EOF checks to prevent infinite loops in the case
 *              of an incomplete Ada (or non-Ada) file being parsed.
 * 06/02/2006 - Added Copyright notice.
 *
 * 0.2 - First Revision
 * 05/26/2006 - Fixed an error where tagging the proper scope of something
 *              declared in an anonymous block or anonymous loop was not
 *              working properly.
 * 05/26/2006 - Fixed an error capturing the name of a 'separate' tag.
 * 05/26/2006 - Fixed the cmp() function so that it finds matches correctly.
 * 05/26/2006 - Fixed some spelling errors.
 * 05/26/2006 - Added explicit skipping of use and with clauses.
 *
 * 0.1 - Initial Release
 *
 * Future Changes:
 * TODO: Add inheritance information?
 * TODO: Add signature gathering?
 * TODO: Add capability to create tag for fully qualified object name?
 *
 * Copyright (C) 2006 A. Aaron Cornelius
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA.
 *
 */

#include "general.h"    /* always include first */

#include <string.h>     /* to declare strxxx() functions */
#include <ctype.h>      /* to define isxxx() macros */

#include "parse.h"      /* always include */
#include "read.h"       /* to define file fileReadLine() */
#include "debug.h"      /* for assert */
#include "routines.h"   /* for generic malloc/realloc/free routines */

typedef enum eAdaException
{
  EXCEPTION_NONE,
  EXCEPTION_EOF
} adaException;

static adaException exception;

typedef enum eAdaParseMode
{
  ADA_ROOT,
  ADA_DECLARATIONS,
  ADA_CODE,
  ADA_EXCEPTIONS,
  ADA_GENERIC
} adaParseMode;

typedef enum eAdaKinds
{
  ADA_KIND_SEPARATE = -2,   /* for defining the parent token name of a child
                             * sub-unit */
  ADA_KIND_UNDEFINED = -1,  /* for default/initialization values */
  ADA_KIND_PACKAGE_SPEC,
  ADA_KIND_PACKAGE,
  ADA_KIND_TYPE_SPEC,
  ADA_KIND_TYPE,
  ADA_KIND_SUBTYPE_SPEC,
  ADA_KIND_SUBTYPE,
  ADA_KIND_RECORD_COMPONENT,
  ADA_KIND_ENUM_LITERAL,
  ADA_KIND_VARIABLE_SPEC,
  ADA_KIND_VARIABLE,
  ADA_KIND_FORMAL,
  ADA_KIND_CONSTANT,
  ADA_KIND_EXCEPTION,
  ADA_KIND_SUBPROGRAM_SPEC,
  ADA_KIND_SUBPROGRAM,
  ADA_KIND_TASK_SPEC,
  ADA_KIND_TASK,
  ADA_KIND_PROTECTED_SPEC,
  ADA_KIND_PROTECTED,
  ADA_KIND_ENTRY_SPEC,
  ADA_KIND_ENTRY,
  ADA_KIND_LABEL,
  ADA_KIND_IDENTIFIER,
  ADA_KIND_AUTOMATIC_VARIABLE,
  ADA_KIND_ANONYMOUS,      /* for non-identified loops and blocks */
  ADA_KIND_COUNT            /* must be last */
} adaKind;

static kindOption AdaKinds[] =
{
  { FALSE,  'P', "packspec",    "package specifications" },
  { TRUE,   'p', "package",     "packages" },
  { FALSE,  'T', "typespec",    "type specifications" },
  { TRUE,   't', "type",        "types" },
  { FALSE,  'U', "subspec",     "subtype specifications" },
  { TRUE,   'u', "subtype",     "subtypes" },
  { TRUE,   'c', "component",   "record type components" },
  { TRUE,   'l', "literal",     "enum type literals" },
  { FALSE,  'V', "varspec",     "variable specifications" },
  { TRUE,   'v', "variable",    "variables" },
  { TRUE,   'f', "formal",      "generic formal parameters" },
  { TRUE,   'n', "constant",    "constants" },
  { TRUE,   'x', "exception",   "user defined exceptions" },
  { FALSE,  'R', "subprogspec", "subprogram specifications" },
  { TRUE,   'r', "subprogram",  "subprograms" },
  { FALSE,  'K', "taskspec",    "task specifications" },
  { TRUE,   'k', "task",        "tasks" },
  { FALSE,  'O', "protectspec", "protected data specifications" },
  { TRUE,   'o', "protected",   "protected data" },
  { FALSE,  'E', "entryspec",   "task/protected data entry specifications" },
  { TRUE,   'e', "entry",       "task/protected data entries" },
  { TRUE,   'b', "label",       "labels" },
  { TRUE,   'i', "identifier",  "loop/declare identifiers"},
  { FALSE,  'a', "autovar",     "automatic variables" },
  { FALSE,  'y', "annon",       "loops and blocks with no identifier" }
};

typedef struct sAdaTokenList
{
  int numTokens;
  struct sAdaTokenInfo *head;
  struct sAdaTokenInfo *tail;
} adaTokenList;

typedef struct sAdaTokenInfo
{
  adaKind kind;
  boolean isSpec;
  char *name;
  tagEntryInfo tag;
  struct sAdaTokenInfo *parent;
  struct sAdaTokenInfo *prev;
  struct sAdaTokenInfo *next;
  adaTokenList children;
} adaTokenInfo;

typedef enum eAdaKeywords
{
  ADA_KEYWORD_ACCEPT,
  ADA_KEYWORD_BEGIN,
  ADA_KEYWORD_BODY,
  ADA_KEYWORD_CASE,
  ADA_KEYWORD_CONSTANT,
  ADA_KEYWORD_DECLARE,
  ADA_KEYWORD_DO,
  ADA_KEYWORD_ELSE,
  ADA_KEYWORD_ELSIF,
  ADA_KEYWORD_END,
  ADA_KEYWORD_ENTRY,
  ADA_KEYWORD_EXCEPTION,
  ADA_KEYWORD_FOR,
  ADA_KEYWORD_FUNCTION,
  ADA_KEYWORD_GENERIC,
  ADA_KEYWORD_IF,
  ADA_KEYWORD_IN,
  ADA_KEYWORD_IS,
  ADA_KEYWORD_LOOP,
  ADA_KEYWORD_NEW,
  ADA_KEYWORD_OR,
  ADA_KEYWORD_PACKAGE,
  ADA_KEYWORD_PRAGMA,
  ADA_KEYWORD_PRIVATE,
  ADA_KEYWORD_PROCEDURE,
  ADA_KEYWORD_PROTECTED,
  ADA_KEYWORD_RECORD,
  ADA_KEYWORD_RENAMES,
  ADA_KEYWORD_SELECT,
  ADA_KEYWORD_SEPARATE,
  ADA_KEYWORD_SUBTYPE,
  ADA_KEYWORD_TASK,
  ADA_KEYWORD_THEN,
  ADA_KEYWORD_TYPE,
  ADA_KEYWORD_UNTIL,
  ADA_KEYWORD_USE,
  ADA_KEYWORD_WHEN,
  ADA_KEYWORD_WHILE,
  ADA_KEYWORD_WITH
} adaKeyword;

static const char *AdaKeywords[] =
{
  "accept",
  "begin",
  "body",
  "case",
  "constant",
  "declare",
  "do",
  "else",
  "elsif",
  "end",
  "entry",
  "exception",
  "for",
  "function",
  "generic",
  "if",
  "in",
  "is",
  "loop",
  "new",
  "or",
  "package",
  "pragma",
  "private",
  "procedure",
  "protected",
  "record",
  "renames",
  "select",
  "separate",
  "subtype",
  "task",
  "then",
  "type",
  "until",
  "use",
  "when",
  "while",
  "with"
};

/* variables for managing the input string, position as well as input line
 * number and position */
static const char *line;
static int lineLen;
static int pos;
static unsigned long matchLineNum;
static fpos_t matchFilePos;

/* a utility function */
static void makeSpec(adaKind *kind);

/* prototypes of functions for manipulating the Ada tokens */
static adaTokenInfo *newAdaToken(const char *name, int len,
                                 adaKind kind, boolean isSpec,
                                 adaTokenInfo *parent);
static void freeAdaToken(adaTokenList *list, adaTokenInfo *token);
static void appendAdaToken(adaTokenInfo *parent, adaTokenInfo *token);

/* token list processing function prototypes */
static void initAdaTokenList(adaTokenList *list);
static void freeAdaTokenList(adaTokenList *list);
static void appendAdaTokenList(adaTokenInfo *parent, adaTokenList *children);

/* prototypes of functions for moving through the DEFINED text */
static void readNewLine(void);
static void movePos(int amount);
static boolean cmp(char *buf, int len, char *match);
static boolean adaCmp(char *match);
static boolean adaKeywordCmp(adaKeyword keyword);
static void skipUntilWhiteSpace(void);
static void skipWhiteSpace(void);
static void skipPast(char *past);
static void skipPastKeyword(adaKeyword keyword);
static void skipPastWord(void);

/* prototypes of functions for parsing the high-level Ada constructs */
static adaTokenInfo *adaParseBlock(adaTokenInfo *parent, adaKind kind);
static adaTokenInfo *adaParseSubprogram(adaTokenInfo *parent, adaKind kind);
static adaTokenInfo *adaParseType(adaTokenInfo *parent, adaKind kind);
static adaTokenInfo *adaParseVariables(adaTokenInfo *parent, adaKind kind);
static adaTokenInfo *adaParseLoopVar(adaTokenInfo *parent);
static adaTokenInfo *adaParse(adaParseMode mode, adaTokenInfo *parent);

/* prototypes of the functions used by ctags */
static void storeAdaTags(adaTokenInfo *token);
static void findAdaTags(void);
extern parserDefinition* AdaParser(void);

static void makeSpec(adaKind *kind)
{
  switch(*kind)
  {
    case ADA_KIND_PACKAGE:
      *kind = ADA_KIND_PACKAGE_SPEC;
      break;

    case ADA_KIND_TYPE:
      *kind = ADA_KIND_TYPE_SPEC;
      break;

    case ADA_KIND_SUBTYPE:
      *kind = ADA_KIND_SUBTYPE_SPEC;
      break;

    case ADA_KIND_VARIABLE:
      *kind = ADA_KIND_VARIABLE_SPEC;
      break;

    case ADA_KIND_SUBPROGRAM:
      *kind = ADA_KIND_SUBPROGRAM_SPEC;
      break;

    case ADA_KIND_TASK:
      *kind = ADA_KIND_TASK_SPEC;
      break;

    case ADA_KIND_PROTECTED:
      *kind = ADA_KIND_PROTECTED_SPEC;
      break;

    case ADA_KIND_ENTRY:
      *kind = ADA_KIND_ENTRY_SPEC;
      break;

    default:
      printf("Warning, non-spec type trying to be 'spec'ified\n");
      *kind = ADA_KIND_UNDEFINED;
      break;
  }
}

static adaTokenInfo *newAdaToken(const char *name, int len,
                                 adaKind kind, boolean isSpec,
                                 adaTokenInfo *parent)
{
  char *tmpName = NULL;
  adaTokenInfo *token = xMalloc(1, adaTokenInfo);

  if(name != NULL && len != 0)
  {
    tmpName = xMalloc(len + 1, char);
    strncpy((char *) tmpName, (char *) name, len);
    tmpName[len] = '\0';
  }

  /* init the tag */
  initTagEntry(&token->tag, tmpName);

  token->kind = kind;
  token->isSpec = isSpec;

  /* set the token data */
  token->name = tmpName;
  token->parent = parent;

  /* the default for scope with most Ada stuff is that it is limited to the
   * file (well, package/subprogram/etc. But close enough) */
  token->tag.isFileScope = TRUE;

  /* add the kind info */
  token->tag.kindName = AdaKinds[kind].name;
  token->tag.kind = AdaKinds[kind].letter;

  /* setup the parent and children pointers */
  initAdaTokenList(&token->children);
  appendAdaToken(parent, token);

  return token;
}

static void freeAdaToken(adaTokenList *list, adaTokenInfo *token)
{
  if(token != NULL)
  {
    if(token->name != NULL)
    {
      eFree((void *) token->name);
      token->name = NULL;
    }

    /* before we delete this token, clean up it's children */
    freeAdaTokenList(&token->children);

    /* move the next token in the list to this token's spot */
    if(token->prev != NULL)
    {
      token->prev = token->next;
    }
    else if(list != NULL && token->prev == NULL)
    {
      list->head = token->next;
    }

    /* move the previous token in the list to this token's spot */
    if(token->next != NULL)
    {
      token->next = token->prev;
    }
    else if(list != NULL && token->next == NULL)
    {
      list->tail = token->prev;
    }

    /* decrement the list count */
    if(list != NULL)
    {
      list->numTokens--;
    }

    /* now that this node has had everything hanging off of it rearranged,
     * delete this node */
    eFree(token);
  } /* if(token != NULL) */
}

static void appendAdaToken(adaTokenInfo *parent, adaTokenInfo *token)
{
  /* if the parent or newChild is NULL there is nothing to be done */
  if(parent != NULL && token != NULL)
  {
    /* we just need to add this to the list and set a parent pointer */
    parent->children.numTokens++;
    token->parent = parent;
    token->prev = parent->children.tail;
    token->next = NULL;

    if(parent->children.tail != NULL)
    {
      parent->children.tail->next = token;
    }

    /* the token that was just added always becomes the last token int the
     * list */
    parent->children.tail = token;

    if(parent->children.head == NULL)
    {
      parent->children.head = token;
    }
  }
}

static void initAdaTokenList(adaTokenList *list)
{
  if(list != NULL)
  {
    list->numTokens = 0;
    list->head = NULL;
    list->tail = NULL;
  }
}

static void freeAdaTokenList(adaTokenList *list)
{
  adaTokenInfo *tmp1= NULL;
  adaTokenInfo *tmp2 = NULL;

  if(list != NULL)
  {
    tmp1 = list->head;
    while(tmp1 != NULL)
    {
      tmp2 = tmp1->next;
      freeAdaToken(list, tmp1);
      tmp1 = tmp2;
    }
  }
}

static void appendAdaTokenList(adaTokenInfo *parent, adaTokenList *children)
{
  adaTokenInfo *tmp = NULL;

  if(parent != NULL && children != NULL)
  {
    while(children->head != NULL)
    {
      tmp = children->head->next;
      appendAdaToken(parent, children->head);

      /* we just need to worry about setting the head pointer properly during
       * the list iteration.  The node's pointers will get set properly by the
       * appendAdaToken() function */
      children->head = tmp;
    }

    /* now that we have added all nodes from the children list to the parent
     * node, zero out the children list */
    initAdaTokenList(children);
  }
}

static void readNewLine(void)
{
  /* a simple var to keep track of how many times we hit EOF... If we hit it
   * say, about 1000 times, we will print an error, jump to the end of the
   * program, and store what tags we have */
  static int eofCount = 0;

  while(TRUE)
  {
    line = (const char *) fileReadLine();
    pos = 0;

    if(line == NULL)
    {
      lineLen = 0;
      exception = EXCEPTION_EOF;
      eofCount++;

      if(eofCount >= 1000)
      {
      }
      else
      {
        return;
      }
    }

    lineLen = strlen((char *) line);

    if(lineLen > 0)
    {
      return;
    }
  }
}

static void movePos(int amount)
{
  pos += amount;
  if(exception != EXCEPTION_EOF && pos >= lineLen)
  {
    readNewLine();
  }
}

/* a macro for checking for comments... This isn't the same as the check in
 * cmp() because comments don't have to have whitespace or separation-type
 * characters following the "--" */
#define isAdaComment(buf, pos, len) \
  (((pos) == 0 || (!isalnum((buf)[(pos) - 1]) && (buf)[(pos) - 1] != '_')) && \
   (pos) < (len) && \
   strncasecmp(&(buf)[(pos)], "--", strlen("--")) == 0)

static boolean cmp(char *buf, int len, char *match)
{
  boolean status = FALSE;

  /* if we are trying to match nothing, that is always true */
  if(match == NULL)
  {
    return TRUE;
  }

  /* first check to see if the buffer is empty, if it is, return false */
  if(buf == NULL)
  {
    return status;
  }

  /* A match only happens the number of chars in the matching string match,
   * and whitespace follows... Which means we also must check to see if the
   * end of the line is after the matching string.  Also check for some
   * separation characters such as (, ), :, or ; */
  if((strncasecmp(buf, match, strlen(match)) == 0) &&
     (strlen(match) == len ||
      (strlen(match) < len &&
       (isspace(buf[strlen(match)]) || buf[strlen(match)] == '(' ||
        buf[strlen(match)] == ')' || buf[strlen(match)] == ':' ||
        buf[strlen(match)] == ';'))))
  {
    status = TRUE;
  }

  return status;
}

static boolean adaCmp(char *match)
{
  boolean status = FALSE;

  /* first check to see if line is empty, if it is, throw an exception */
  if(line == NULL)
  {
    exception = EXCEPTION_EOF;
    return status;
  }

  status = cmp((char *) &line[pos], lineLen - pos, match);

  /* if we match, increment the position pointer */
  if(status == TRUE && match != NULL)
  {
    matchLineNum = getSourceLineNumber();
    matchFilePos = getInputFilePosition();

    movePos((strlen(match)));
  }

  return status;
}

/* just a version of adaCmp that is a bit more optimized for keywords */
static boolean adaKeywordCmp(adaKeyword keyword)
{
  boolean status = FALSE;

  /* first check to see if line is empty, if it is, throw an exception */
  if(line == NULL)
  {
    exception = EXCEPTION_EOF;
    return status;
  }

  status = cmp((char *) &line[pos], lineLen - pos,
               (char *) AdaKeywords[keyword]);

  /* if we match, increment the position pointer */
  if(status == TRUE)
  {
    matchLineNum = getSourceLineNumber();
    matchFilePos = getInputFilePosition();

    movePos((strlen(AdaKeywords[keyword])));
  }

  return status;
}

static void skipUntilWhiteSpace(void)
{
  /* first check for a comment line, because this would cause the isspace
   * check to be true immediately */
  while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
  {
    readNewLine();
  }

  while(exception != EXCEPTION_EOF && !isspace(line[pos]))
  {
    /* don't use movePos() because if we read in a new line with this function
     * we need to stop */
    pos++;

    /* the newline counts as whitespace so read in the newline and return
     * immediately */
    if(pos >= lineLen)
    {
      line = (const char *) fileReadLine();
      pos = 0;

      if(line == NULL)
      {
        lineLen = 0;
        exception = EXCEPTION_EOF;
        return;
      }

      lineLen = strlen((char *) line);

      return;
    } /* if(pos >= lineLen) */

    /* now check for comments here */
    while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
    {
      readNewLine();
    }
  } /* while(!isspace(line[pos])) */
}

static void skipWhiteSpace(void)
{
  /* first check for a comment line, because this would cause the isspace
   * check to fail immediately */
  while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
  {
    readNewLine();
  }

  while(exception != EXCEPTION_EOF && isspace(line[pos]))
  {
    movePos(1);

    /* now check for comments here */
    while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
    {
      readNewLine();
    }
  } /* while(isspace(line[pos])) */
}

static void skipPast(char *past)
{
  /* first check for a comment line, because this would cause the isspace
   * check to fail immediately */
  while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
  {
    readNewLine();
  }

  /* now look for the keyword */
  while(exception != EXCEPTION_EOF && !adaCmp(past))
  {
    movePos(1);

    /* now check for comments here */
    while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
    {
      readNewLine();
    }
  }
}

static void skipPastKeyword(adaKeyword keyword)
{
  /* first check for a comment line, because this would cause the isspace
   * check to fail immediately */
  while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
  {
    readNewLine();
  }

  /* now look for the keyword */
  while(exception != EXCEPTION_EOF && !adaKeywordCmp(keyword))
  {
    movePos(1);

    /* now check for comments here */
    while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
    {
      readNewLine();
    }
  }
}

static void skipPastWord(void)
{
  /* first check for a comment line, because this would cause the isspace
   * check to fail immediately */
  while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
  {
    readNewLine();
  }


  /* now increment until we hit a non-word character... Specifically,
   * whitespace, '(', ')', ':', and ';' */
  while(exception != EXCEPTION_EOF && !isspace(line[pos]) &&
        line[pos] != '(' && line[pos] != ')' && line[pos] != ':' &&
        line[pos] != ';')
  {
    movePos(1);

    /* now check for comments here */
    while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
    {
      readNewLine();
    }
  }
}

static adaTokenInfo *adaParseBlock(adaTokenInfo *parent, adaKind kind)
{
  int i;
  adaTokenInfo *token;
  boolean isSpec = TRUE;

  skipWhiteSpace();

  /* if the next word is body, this is not a package spec */
  if(adaKeywordCmp(ADA_KEYWORD_BODY))
  {
    isSpec = FALSE;
  }
  /* if the next word is "type" then this has to be a task or protected spec */
  else if(adaKeywordCmp(ADA_KEYWORD_TYPE) &&
          (kind != ADA_KIND_PROTECTED && kind != ADA_KIND_TASK))
  {
    /* if this failed to validate then we should just fail */
    return NULL;
  }
  skipWhiteSpace();

  /* we are at the start of what should be the tag now... But we have to get
   * it's length.  So loop until we hit whitespace, init the counter to 1
   * since we know that the current position is not whitespace */
  for(i = 1; (pos + i) < lineLen && !isspace(line[pos + i]) &&
      line[pos + i] != '(' && line[pos + i] != ';'; i++);

  /* we have reached the tag of the package, so create the tag */
  token = newAdaToken(&line[pos], i, kind, isSpec, parent);

  movePos(i);
  skipWhiteSpace();

  /* task and protected types are allowed to have discriminants */
  if(line[pos] == '(')
  {
    while(line[pos] != ')')
    {
      movePos(1);
      adaParseVariables(token, ADA_KIND_AUTOMATIC_VARIABLE);
    }
    movePos(1);
  }

  /* we must parse until we hit the "is" string to reach the end of
   * this package declaration, or a "reanames" keyword */
  while(token != NULL)
  {
    skipWhiteSpace();

    if(adaKeywordCmp(ADA_KEYWORD_IS))
    {
      skipWhiteSpace();

      if(adaKeywordCmp(ADA_KEYWORD_SEPARATE))
      {
        /* if the next word is the keyword "separate", don't create the tag
         * since it will be defined elsewhere */
        freeAdaToken(&parent->children, token);
        token = NULL;

        /* move past the ";" ending this declaration */
        skipPast(";");
      }
      else if(adaKeywordCmp(ADA_KEYWORD_NEW))
      {
        /* if this is a "new" something then no need to parse */
        skipPast(";");
      }
      else
      {
        adaParse(ADA_DECLARATIONS, token);
      }

      break;
    } /* if(adaKeywordCmp(ADA_KEYWORD_IS)) */
    else if(adaKeywordCmp(ADA_KEYWORD_RENAMES))
    {
      skipPast(";");
      break;
    }
    else if(adaCmp(";"))
    {
      token->isSpec = TRUE;
      break;
    }
    else
    {
      /* nothing found, move to the next word */
      skipUntilWhiteSpace();
    }
  } /* while(TRUE) - while the end of spec, or beginning of body not found */

  return token;
}

static adaTokenInfo *adaParseSubprogram(adaTokenInfo *parent, adaKind kind)
{
  int i;
  adaTokenInfo *token;
  adaTokenInfo *tmpToken = NULL;

  skipWhiteSpace();

  /* we are at the start of what should be the tag now... But we have to get
   * it's length.  So loop until we hit whitespace or the beginning of the
   * parameter list.  Init the counter to 1 * since we know that the current
   * position is not whitespace */
  for(i = 1; (pos + i) < lineLen && !isspace(line[pos + i]) &&
      line[pos + i] != '(' && line[pos + i] != ';'; i++);

  /* we have reached the tag of the subprogram, so create the tag... Init the
   * isSpec flag to false and we will adjust it when we see if there is an
   * "is", "do" or a ";" following the tag */
  token = newAdaToken(&line[pos], i, kind, FALSE, parent);

  /* move the line position */
  movePos(i);
  skipWhiteSpace();

  /* if we find a '(' grab any parameters */
  if(line[pos] == '(' && token != NULL)
  {
    while(line[pos] != ')')
    {
      movePos(1);
      tmpToken = adaParseVariables(token, ADA_KIND_AUTOMATIC_VARIABLE);
    }
    movePos(1);

    /* check to see if anything was received... If this is an entry this may
     * have a 'discriminant' and not have any parameters in the first
     * parenthesis pair, so check again if this was the case*/
    if(kind == ADA_KIND_ENTRY && tmpToken == NULL)
    {
      /* skip any existing whitespace and see if there is a second parenthesis
       * pair */
      skipWhiteSpace();

      if(line[pos] == '(')
      {
        while(line[pos] != ')')
        {
          movePos(1);
          adaParseVariables(token, ADA_KIND_AUTOMATIC_VARIABLE);
        }
        movePos(1);
      }
    } /* if(kind == ADA_KIND_ENTRY && tmpToken == NULL) */
  } /* if(line[pos] == '(' && token != NULL) */

  /* loop infinitely until we hit a "is", "do" or ";", this will skip over
   * the returns keyword, returned-type for functions as well as any one of a
   * myriad of keyword qualifiers */
  while(exception != EXCEPTION_EOF && token != NULL)
  {
    skipWhiteSpace();

    if(adaKeywordCmp(ADA_KEYWORD_IS))
    {
      skipWhiteSpace();

      if(adaKeywordCmp(ADA_KEYWORD_SEPARATE))
      {
        /* if the next word is the keyword "separate", don't create the tag
         * since it will be defined elsewhere */
        freeAdaToken(&parent->children, token);

        /* move past the ";" ending this declaration */
        skipPast(";");
      }
      else if(adaKeywordCmp(ADA_KEYWORD_NEW))
      {
        /* if this is a "new" something then no need to parse */
        skipPast(";");
      }
      else
      {
        adaParse(ADA_DECLARATIONS, token);
      }

      break;
    } /* if(adaKeywordCmp(ADA_KEYWORD_IS)) */
    else if(adaKeywordCmp(ADA_KEYWORD_RENAMES))
    {
      skipPast(";");
      break;
    }
    else if(adaKeywordCmp(ADA_KEYWORD_DO))
    {
      /* do is the keyword for the beginning of a task entry */
      adaParse(ADA_CODE, token);
      break;
    }
    else if(adaCmp(";"))
    {
      /* this is just a spec then, so set the flag in the token */
      token->isSpec = TRUE;
      break;
    }
    else
    {
      /* nothing found, move to the next word */
      skipPastWord();
    }
  } /* while(TRUE) - while the end of spec, or beginning of body not found */

  return token;
}

static adaTokenInfo *adaParseType(adaTokenInfo *parent, adaKind kind)
{
  int i;
  adaTokenInfo *token = NULL;

  skipWhiteSpace();

  /* get the name of the type */
  for(i = 1; (pos + i) < lineLen && !isspace(line[pos + i]) &&
      line[pos + i] != '(' && line[pos + i] != ';'; i++);

  token = newAdaToken(&line[pos], i, kind, FALSE, parent);

  movePos(i);
  skipWhiteSpace();

  if(line[pos] == '(')
  {
    /* in this case there is a discriminant to this type, gather the
     * variables */
    while(line[pos] != ')')
    {
      movePos(1);
      adaParseVariables(token, ADA_KIND_AUTOMATIC_VARIABLE);
    }
    movePos(1);
    skipWhiteSpace();
  }

  /* check to see what is next, if it is not "is" then just skip to the end of
   * the statement and register this as a 'spec' */
  if(adaKeywordCmp(ADA_KEYWORD_IS))
  {
    skipWhiteSpace();
    /* check to see if this may be a record or an enumeration */
    if(line[pos] == '(')
    {
      movePos(1);
      adaParseVariables(token, ADA_KIND_ENUM_LITERAL);
    }
    else if(adaKeywordCmp(ADA_KEYWORD_RECORD))
    {
      /* until we hit "end record" we need to gather type variables */
      while(TRUE)
      {
        skipWhiteSpace();

        if(adaKeywordCmp(ADA_KEYWORD_END))
        {
          skipWhiteSpace();
          if(adaKeywordCmp(ADA_KEYWORD_RECORD))
          {
            break;
          }
          skipPast(";");
        } /* if(adaKeywordCmp(ADA_KEYWORD_END)) */
        /* handle variant types */
        else if(adaKeywordCmp(ADA_KEYWORD_CASE))
        {
          skipPastKeyword(ADA_KEYWORD_IS);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_WHEN))
        {
          skipPast("=>");
        }
        else
        {
          adaParseVariables(token, ADA_KIND_RECORD_COMPONENT);
          skipPast(";");
        }
      } /* while(TRUE) - end of record not found */
    } /* else if(adaKeywordCmp(ADA_KEYWORD_RECORD)) */
  } /* if(adaKeywordCmp(ADA_KEYWORD_IS)) */
  else
  {
    token->isSpec = TRUE;
  }

  skipPast(";");

  return token;
}

static adaTokenInfo *adaParseVariables(adaTokenInfo *parent, adaKind kind)
{
  /* variables for keeping track of tags */
  int varEndPos = -1;
  int tokenStart = -1;
  adaTokenInfo *token = NULL;

  /* buffer management variables */
  int i = 0;
  int bufPos = 0;
  int bufLen = 0;
  char *buf = NULL;

  /* file and line position variables */
  unsigned long int lineNum;
  int filePosIndex = 0;
  int filePosSize = 32;
  fpos_t *filePos = xMalloc(filePosSize, fpos_t);

  /* skip any preliminary whitespace or comments */
  skipWhiteSpace();
  while(exception != EXCEPTION_EOF && isAdaComment(line, pos, lineLen))
  {
    readNewLine();
  }

  /* before we start reading input save the current line number and file
   * position, so we can reconstruct the correct line & file position for any
   * tags we create */
  lineNum = getSourceLineNumber();
  filePos[filePosIndex] = getInputFilePosition();

  /* setup local buffer... Since we may have to read a few lines to verify
   * that this is a proper variable declaration, and still make a token for
   * each variable, add one to the allocated string to account for a '\0' */
  bufLen = lineLen - pos;
  buf = xMalloc(bufLen + 1, char);
  memcpy((void *) buf, (void *) &line[pos], bufLen);

  /* don't increase bufLen to include the NULL char so that strlen(buf) and
   * bufLen match */
  buf[bufLen] = '\0';

  while(TRUE)
  {
    /* make sure that we don't count anything in a comment as being valid to
     * parse */
    if(isAdaComment(buf, bufPos, bufLen))
    {
      /* move bufPos to the end of this 'line' so a new line of input is
       * read */
      bufPos = bufLen - 1;

      /* if tokenStart is not -2 then we may be trying to track the type
       * of this variable declaration, so set tokenStart to -1 so that the
       * tracking can start over */
      if(tokenStart != -2)
      {
        tokenStart = -1;
      }
    } /* if(isAdaComment(buf, bufPos, bufLen)) */
    /* we have to keep track of any () pairs that may be in the variable
     * declarations.  And then quit if we hit a ';' the real end ')', or also
     * a variable initialization... Once we hit := then we have hit the end of
     * the variable declaration */
    else if(buf[bufPos] == '(')
    {
      i++;
    }
    else if(buf[bufPos] == ')')
    {
      if(i == 0)
      {
        break;
      }
      else
      {
        i--;
      }
    }
    else if(buf[bufPos] == ';' ||
            ((bufPos + 1) < bufLen &&
             (strncasecmp(&buf[bufPos], ":=", strlen(":=")) == 0 ||
              strncasecmp(&buf[bufPos], "=>", strlen("=>")) == 0)))
    {
      break;
    }
    /* if we found the : keep track of where we found it */
    else if(buf[bufPos] == ':' &&
            (bufPos + 1 >= bufLen || buf[bufPos + 1] != '='))
    {
      varEndPos = bufPos;
    }
    /* if we have the position of the ':' find out what the next word is,
     * because if it "constant" or "exception" then we must tag this slightly
     * differently, But only check this for normal variables */
    else if(kind == ADA_KIND_VARIABLE && varEndPos != -1 &&
            !isspace(buf[bufPos]) && tokenStart == -1)
    {
      tokenStart = bufPos;
    }
    else if(kind == ADA_KIND_VARIABLE && varEndPos != -1 && tokenStart >= 0 &&
            ((bufPos + 1) >= bufLen || isspace(buf[bufPos + 1]) ||
             buf[bufPos + 1] == ';'))
    {
      if(cmp(&buf[tokenStart], bufLen - tokenStart,
             (char *) AdaKeywords[ADA_KEYWORD_CONSTANT]) == TRUE)
      {
        kind = ADA_KIND_CONSTANT;
      }
      else if(cmp(&buf[tokenStart], bufLen - tokenStart,
                  (char *) AdaKeywords[ADA_KEYWORD_EXCEPTION]) == TRUE)
      {
        kind = ADA_KIND_EXCEPTION;
      }

      /* set tokenStart to -2 to prevent any more words from being checked */
      tokenStart = -2;
    }

    bufPos++;

    /* if we just incremented beyond the length of the current buffer, we need
     * to read in a new line */
    if(exception != EXCEPTION_EOF && bufPos >= bufLen)
    {
      readNewLine();

      /* store the new file position for the start of this line */
      filePosIndex++;
      while(filePosIndex >= filePosSize)
      {
        filePosSize *= 2;
        filePos = xRealloc(filePos, filePosSize, fpos_t);
      }
      filePos[filePosIndex] = getInputFilePosition();

      /* increment bufLen and bufPos now so that they jump past the NULL
       * character in the buffer */
      bufLen++;
      bufPos++;

      /* allocate space and store this into our buffer */
      bufLen += lineLen;
      buf = xRealloc((char *) buf, bufLen + 1, char);
      memcpy((void *) &buf[bufPos], (void *) line, lineLen);
      buf[bufLen] = '\0';
    } /* if(bufPos >= bufLen) */
  } /* while(TRUE) */

  /* There is a special case if we are gathering enumeration values and we hit
   * a ')', that is allowed so we need to move varEndPos to where the ')' is */
  if(kind == ADA_KIND_ENUM_LITERAL && buf[bufPos] == ')' && varEndPos == -1)
  {
    varEndPos = bufPos;
  }

  /* so we found a : or ;... If it is a : go back through the buffer and
   * create a token for each word skipping over all whitespace and commas
   * until the : is hit*/
  if(varEndPos != -1)
  {
    /* there should be no whitespace at the beginning, so tokenStart is
     * initialized to 0 */
    tokenStart = 0;

    /* before we start set the filePosIndex back to 0 so we can go through the
     * file position table as the read line number increases */
    filePosIndex = 0;

    for(i = 0; i < varEndPos; i++)
    {
      /* skip comments which are '--' unless we are in a word */
      if(isAdaComment(buf, i, varEndPos))
      {
        /* move i past the '\0' that we put at the end of each line stored in
         * buf */
        for( ; i < varEndPos && buf[i] != '\0'; i++);
      } /* if(isAdaComment(buf, i, varEndPos)) */
      else if(tokenStart != -1 && (isspace(buf[i]) || buf[i] == ',' ||
              buf[i] == '\0'))
      {
        /* only store the word if it is not an in/out keyword */
        if(!cmp(&buf[tokenStart], varEndPos, "in") &&
           !cmp(&buf[tokenStart], varEndPos, "out"))
        {
          token = newAdaToken((const char *) &buf[tokenStart], i - tokenStart,
                              kind, FALSE, parent);

          /* now set the proper line and file position counts for this
           * new token */
          token->tag.lineNumber = lineNum + filePosIndex;
          token->tag.filePosition = filePos[filePosIndex];
        }
        tokenStart = -1;
      } /* if(tokenStart != -1 && (isspace(buf[i]) || buf[i] == ',')) */
      else if(tokenStart == -1 && !(isspace(buf[i]) || buf[i] == ',' ||
              buf[i] == '\0'))
      {
        /* only set the tokenStart for non-newline characters */
        tokenStart = i;
      }

      /* after we are finished with this line, move the file position */
      if(buf[i] == '\0')
      {
        filePosIndex++;
      }
    } /* for(i = 0; i < varEndPos; i++) */

    /* if token start was 'started' then we should store the last token */
    if(tokenStart != -1)
    {
      token = newAdaToken((const char *) &buf[tokenStart], i - tokenStart,
                          kind, FALSE, parent);

      /* now set the proper line and file position counts for this
       * new token */
      token->tag.lineNumber = lineNum + filePosIndex;
      token->tag.filePosition = filePos[filePosIndex];
    }
  } /* if(varEndPos != -1) */

  /* now get the pos variable to point to the correct place in line where we
   * left off in our temp buf, and free our temporary buffer.  This is a
   * little different than most buf position moves.  It gets the distance from
   * the current buf position to the end of the buffer, which is also the
   * distance from where pos should be wrt the end of the variable
   * definition */
  movePos((lineLen - (bufLen - bufPos)) - pos);
  eFree((void *) buf);
  eFree((void *) filePos);

  return token;
}

static adaTokenInfo *adaParseLoopVar(adaTokenInfo *parent)
{
  int i;
  adaTokenInfo *token = NULL;

  skipWhiteSpace();
  for(i = 1; (pos + i) < lineLen && !isspace(line[pos + i]); i++);
  token = newAdaToken(&line[pos], i, ADA_KIND_AUTOMATIC_VARIABLE, FALSE,
                      parent);
  movePos(i);

  /* now skip to the end of the loop declaration */
  skipPastKeyword(ADA_KEYWORD_LOOP);

  return token;
}

static adaTokenInfo *adaParse(adaParseMode mode, adaTokenInfo *parent)
{
  int i;
  adaTokenInfo genericParamsRoot;
  adaTokenInfo *token = NULL;

  initAdaTokenList(&genericParamsRoot.children);

  /* if we hit the end of the file, line will be NULL and our skip and match
   * functions will hit this jump buffer with EXCEPTION_EOF */
  while(exception == EXCEPTION_NONE)
  {
    /* find the next place to start */
    skipWhiteSpace();

    /* check some universal things to check for first */
    if(isAdaComment(line, pos, lineLen))
    {
      readNewLine();
      continue;
    }
    else if(adaKeywordCmp(ADA_KEYWORD_PRAGMA) ||
            adaKeywordCmp(ADA_KEYWORD_WITH) ||
            adaKeywordCmp(ADA_KEYWORD_USE))
    {
      /* set the token to NULL so we accidentally don't pick up something
       * from earlier */
      skipPast(";");
      continue;
    }

    /* check for tags based on our current mode */
    switch(mode)
    {
      case ADA_ROOT:
        if(adaKeywordCmp(ADA_KEYWORD_PACKAGE))
        {
          token = adaParseBlock(parent, ADA_KIND_PACKAGE);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PROCEDURE) ||
                adaKeywordCmp(ADA_KEYWORD_FUNCTION))
        {
          token = adaParseSubprogram(parent, ADA_KIND_SUBPROGRAM);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_TASK))
        {
          token = adaParseBlock(parent, ADA_KIND_TASK);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PROTECTED))
        {
          token = adaParseBlock(parent, ADA_KIND_PROTECTED);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_GENERIC))
        {
          /* if we have hit a generic declaration, go to the generic section
           * and collect the formal parameters */
          mode = ADA_GENERIC;
          break;
        } /* else if(adaKeywordCmp(ADA_KEYWORD_GENERIC)) */
        else if(adaKeywordCmp(ADA_KEYWORD_SEPARATE))
        {
          /* skip any possible whitespace */
          skipWhiteSpace();

          /* skip over the "(" until we hit the tag */
          if(line[pos] == '(')
          {
            movePos(1);
            skipWhiteSpace();

            /* get length of tag */
            for(i = 1; (pos + i) < lineLen && line[pos + i] != ')' &&
                !isspace(line[pos + i]); i++);

            /* if this is a separate declaration, all it really does is create
             * a false high level token for everything in this file to belong
             * to... But we don't know what kind it is, so we declare it as
             * ADA_KIND_SEPARATE, which will cause it not to be placed in
             * the tag file, and the item in this file will be printed as
             * separate:<name> instead of package:<name> or whatever the
             * parent kind really is (assuming the ctags option will be on
             * for printing such info to the tag file) */
            token = newAdaToken(&line[pos], i, ADA_KIND_SEPARATE, FALSE,
                                parent);

            /* since this is a false top-level token, set parent to be
             * token */
            parent = token;
            token = NULL;

            /* skip past the ')' */
            skipPast(")");
          } /* if(line[pos] == '(') */
          else
          {
            /* move to the end of this statement */
            skipPast(";");
          }
        } /* else if(adaKeywordCmp(ADA_KEYWORD_SEPARATE)) */
        else
        {
          /* otherwise, nothing was found so just skip until the end of this
           * unknown statement... It's most likely just a use or with
           * clause.  Also set token to NULL so we don't attempt anything
           * incorrect */
          token = NULL;
          skipPast(";");
        }

        /* check to see if we succeeded in creating our token */
        if(token != NULL)
        {
          /* if we made a tag at this level then it shouldn't be file-scope */
          token->tag.isFileScope = FALSE;

          /* if any generic params have been gathered, attach them to
           * token */
          appendAdaTokenList(token, &genericParamsRoot.children);
        } /* if(token != NULL) */

        break;

      case ADA_GENERIC:
        /* if we are processing a generic block, make up some temp children
         * which we will later attach to the root of the real
         * procedure/package/whatever the formal parameters are for */
        if(adaKeywordCmp(ADA_KEYWORD_PACKAGE))
        {
          token = adaParseBlock(parent, ADA_KIND_PACKAGE);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PROCEDURE) ||
                adaKeywordCmp(ADA_KEYWORD_FUNCTION))
        {
          token = adaParseSubprogram(parent, ADA_KIND_SUBPROGRAM);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_TASK))
        {
          token = adaParseBlock(parent, ADA_KIND_TASK);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PROTECTED))
        {
          token = adaParseBlock(parent, ADA_KIND_PROTECTED);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_TYPE))
        {
          skipWhiteSpace();

          /* get length of tag */
          for(i = 1; (pos + i) < lineLen && !isspace(line[pos + i]) &&
              line[pos + i] != '(' && line[pos + i] != ';'; i++);

          appendAdaToken(&genericParamsRoot,
                         newAdaToken(&line[pos], i, ADA_KIND_FORMAL, FALSE,
                                     NULL));

          /* skip to the end of this formal type declaration */
          skipPast(";");
        } /* else if(adaKeywordCmp(ADA_KEYWORD_TYPE)) */
        else if(adaKeywordCmp(ADA_KEYWORD_WITH))
        {
          skipWhiteSpace();
          /* skip over the function/procedure keyword, it doesn't matter for
           * now */
          skipUntilWhiteSpace();
          skipWhiteSpace();

          /* get length of tag */
          for(i = 1; (pos + i) < lineLen && !isspace(line[pos + i]) &&
              line[pos + i] != '(' && line[pos + i] != ';'; i++);

          appendAdaToken(&genericParamsRoot,
                         newAdaToken(&line[pos], i, ADA_KIND_FORMAL, FALSE,
                                     NULL));

          /* increment the position */
          movePos(i);

          /* now gather the parameters to this subprogram */
          if(line[pos] == '(')
          {
            while(line[pos] != ')')
            {
              movePos(1);
              adaParseVariables(genericParamsRoot.children.tail,
                                ADA_KIND_AUTOMATIC_VARIABLE);
            }
            movePos(1);
          }

          /* skip to the end of this formal type declaration */
          skipPast(";");
        } /* else if(adaKeywordCmp(ADA_KEYWORD_WITH)) */
        else
        {
          /* otherwise, nothing was found so just skip until the end of this
           * unknown statement... It's most likely just a use or with
           * clause.  Also set token to NULL so we don't attempt anything
           * incorrect */
          token = NULL;
          skipPast(";");
        }

        /* check to see if we succeeded in creating our token */
        if(token != NULL)
        {
          /* if any generic params have been gathered, attach them to
           * token, and set the mode back to ADA_ROOT */
          appendAdaTokenList(token, &genericParamsRoot.children);
          mode = ADA_ROOT;
        } /* if(token != NULL) */

        break;

      case ADA_DECLARATIONS:
        if(adaKeywordCmp(ADA_KEYWORD_PACKAGE))
        {
          token = adaParseBlock(parent, ADA_KIND_PACKAGE);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PROCEDURE) ||
                adaKeywordCmp(ADA_KEYWORD_FUNCTION))
        {
          token = adaParseSubprogram(parent, ADA_KIND_SUBPROGRAM);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_TASK))
        {
          token = adaParseBlock(parent, ADA_KIND_TASK);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PROTECTED))
        {
          token = adaParseBlock(parent, ADA_KIND_PROTECTED);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_GENERIC))
        {
          /* if we have hit a generic declaration, go to the generic section
           * and collect the formal parameters */
          mode = ADA_GENERIC;
          break;
        }
        else if(adaKeywordCmp(ADA_KEYWORD_TYPE))
        {
          token = adaParseType(parent, ADA_KIND_TYPE);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_SUBTYPE))
        {
          token = adaParseType(parent, ADA_KIND_SUBTYPE);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_BEGIN))
        {
          mode = ADA_CODE;
          break;
        }
        else if(adaKeywordCmp(ADA_KEYWORD_FOR))
        {
          /* if we hit a "for" statement it is defining implementation details
           * for a specific type/variable/subprogram/etc...  So we should just
           * skip it, so skip the tag, then we need to see if there is a
           * 'record' keyword... If there is we must skip past the
           * 'end record;' statement.  First skip past the tag */
          skipPastKeyword(ADA_KEYWORD_USE);
          skipWhiteSpace();

          if(adaKeywordCmp(ADA_KEYWORD_RECORD))
          {
            /* now skip to the next "record" keyword, which should be the end
             * of this use statement */
            skipPastKeyword(ADA_KEYWORD_RECORD);
          }

          /* lastly, skip past the end ";" */
          skipPast(";");
        }
        else if(adaKeywordCmp(ADA_KEYWORD_END))
        {
          /* if we have hit an end then we must see if the next word matches
           * the parent token's name.  If it does we hit the end of whatever
           * sort of block construct we were processing and we must
           * return */
          skipWhiteSpace();
          if(adaCmp(parent->name))
          {
            skipPast(";");

            /* return the token */
            freeAdaTokenList(&genericParamsRoot.children);
            return token;
          } /* if(adaCmp(parent->name)) */
          else
          {
            /* set the token to NULL so we accidentally don't pick up something
             * from earlier */
            token = NULL;
            skipPast(";");
          }
        } /* else if(adaKeywordCmp(ADA_KEYWORD_END)) */
        else if(adaKeywordCmp(ADA_KEYWORD_ENTRY))
        {
          token = adaParseSubprogram(parent, ADA_KIND_ENTRY);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_PRIVATE))
        {
          /* if this is a private declaration then we need to just skip
           * whitespace to get to the next bit of code to parse */
          skipWhiteSpace();
        }
        else
        {
          /* if nothing else matched this is probably a variable, constant
           * or exception declaration */
          token = adaParseVariables(parent, ADA_KIND_VARIABLE);
          skipPast(";");
        }

        /* check to see if we succeeded in creating our token */
        if(token != NULL)
        {
          /* if this is one of the root-type tokens... Do some extra
           * processing */
          if(token->kind == ADA_KIND_PACKAGE ||
             token->kind == ADA_KIND_SUBPROGRAM ||
             token->kind == ADA_KIND_TASK ||
             token->kind == ADA_KIND_PROTECTED)
          {
            /* if any generic params have been gathered, attach them to
             * token */
            appendAdaTokenList(token, &genericParamsRoot.children);
          }
        } /* if(token != NULL) */
        break;

      case ADA_CODE:
        if(adaKeywordCmp(ADA_KEYWORD_DECLARE))
        {
          /* if we are starting a declare block here, and not down at the
           * identifier definition then make an anonymous token to track the
           * data in this block */
          token = newAdaToken(NULL, 0, ADA_KIND_ANONYMOUS, FALSE, parent);

          /* save the correct starting line */
          token->tag.lineNumber = matchLineNum;
          token->tag.filePosition = matchFilePos;

          adaParse(ADA_DECLARATIONS, token);
        } /* if(adaKeywordCmp(ADA_KEYWORD_DECLARE)) */
        else if(adaKeywordCmp(ADA_KEYWORD_BEGIN))
        {
          /* if we are starting a code block here, and not down at the
           * identifier definition then make an anonymous token to track the
           * data in this block, if this was part of a proper LABEL:
           * declare/begin/end block then the parent would already be a label
           * and this begin statement would have been found while in the
           * ADA_DECLARATIONS parsing section  */
          token = newAdaToken(NULL, 0, ADA_KIND_ANONYMOUS, FALSE, parent);

          /* save the correct starting line */
          token->tag.lineNumber = matchLineNum;
          token->tag.filePosition = matchFilePos;

          adaParse(ADA_CODE, token);
        } /* else if(adaKeywordCmp(ADA_KEYWORD_BEGIN)) */
        else if(adaKeywordCmp(ADA_KEYWORD_EXCEPTION))
        {
          mode = ADA_EXCEPTIONS;
          break;
        } /* else if(adaKeywordCmp(ADA_KEYWORD_EXCEPTION)) */
        else if(adaKeywordCmp(ADA_KEYWORD_END))
        {
          /* if we have hit an end then we must see if the next word matches
           * the parent token's name.  If it does we hit the end of whatever
           * sort of block construct we were processing and we must
           * return */
          skipWhiteSpace();
          if(adaCmp(parent->name))
          {
            skipPast(";");

            /* return the token */
            freeAdaTokenList(&genericParamsRoot.children);
            return token;
          } /* if(adaCmp(parent->name)) */
          else if(adaKeywordCmp(ADA_KEYWORD_LOOP))
          {
            /* a loop with an identifier has this syntax:
             * "end loop <ident>;" */
            skipWhiteSpace();

            /* now check for the parent loop's name */
            if(adaCmp(parent->name))
            {
              skipPast(";");

              /* return the token */
              freeAdaTokenList(&genericParamsRoot.children);
              return token;
            } /* if(adaCmp(parent->name)) */
          } /* else if(adaKeywordCmp(ADA_KEYWORD_LOOP)) */
          else
          {
            /* otherwise, nothing was found so just skip until the end of
             * this statement */
            skipPast(";");
          }
        } /* else if(adaKeywordCmp(ADA_KEYWORD_END)) */
        else if(adaKeywordCmp(ADA_KEYWORD_ACCEPT))
        {
          adaParseSubprogram(parent, ADA_KIND_ENTRY);
        } /* else if(adaKeywordCmp(ADA_KEYWORD_ACCEPT)) */
        else if(adaKeywordCmp(ADA_KEYWORD_FOR))
        {
          /* if this is a for loop, then we may need to pick up the
           * automatic loop iterator, But... The loop variable is only
           * available within the loop itself so make an anonymous label
           * parent for this loop var to be parsed in */
          token = newAdaToken((const char *) AdaKeywords[ADA_KEYWORD_LOOP],
                              strlen(AdaKeywords[ADA_KEYWORD_LOOP]),
                              ADA_KIND_ANONYMOUS, FALSE, parent);
          adaParseLoopVar(token);
          adaParse(ADA_CODE, token);
        } /* else if(adaKeywordCmp(ADA_KEYWORD_FOR)) */
        else if(adaKeywordCmp(ADA_KEYWORD_WHILE))
        {
          token = newAdaToken((const char *) AdaKeywords[ADA_KEYWORD_LOOP],
                              strlen(AdaKeywords[ADA_KEYWORD_LOOP]),
                              ADA_KIND_ANONYMOUS, FALSE, parent);

          /* skip past the while loop declaration and parse the loop body */
          skipPastKeyword(ADA_KEYWORD_LOOP);
          skipWhiteSpace();
          adaParse(ADA_CODE, token);
        } /* else if(adaKeywordCmp(ADA_KEYWORD_WHILE)) */
        else if(adaKeywordCmp(ADA_KEYWORD_LOOP))
        {
          token = newAdaToken((const char *) AdaKeywords[ADA_KEYWORD_LOOP],
                              strlen(AdaKeywords[ADA_KEYWORD_LOOP]),
                              ADA_KIND_ANONYMOUS, FALSE, parent);

          /* save the correct starting line */
          token->tag.lineNumber = matchLineNum;
          token->tag.filePosition = matchFilePos;

          /* parse the loop body */
          skipWhiteSpace();
          adaParse(ADA_CODE, token);
        } /* else if(adaKeywordCmp(ADA_KEYWORD_LOOP)) */
        else if(line != NULL &&
                strncasecmp((char *) &line[pos], "<<", strlen("<<")) == 0)
        {
          movePos(strlen("<<"));

          /* if the first chars are <<, find the ending >> and if we do that
           * then store the label tag, start i at strlen of "<<" plus 1
           * because we don't want to move the real pos until we know for
           * sure this is a label */
          for(i = 1; (pos + i) < lineLen &&
              strncasecmp((char *) &line[pos + i], ">>", strlen(">>")) != 0;
              i++);

          /* if we didn't increment to the end of the line, a match was
           * found, if we didn't just fall through */
          if((pos + i) < lineLen)
          {
            token = newAdaToken(&line[pos], i, ADA_KIND_LABEL, FALSE, parent);
            skipPast(">>");
            token = NULL;
          }
        } /* else if(strncasecmp(line[pos], "<<", strlen("<<")) == 0) */
        /* we need to check for a few special case keywords that might cause
         * the simple ; ending statement checks to fail, first the simple
         * one word keywords and then the start <stuff> end statements */
        else if(adaKeywordCmp(ADA_KEYWORD_SELECT) ||
                adaKeywordCmp(ADA_KEYWORD_OR) ||
                adaKeywordCmp(ADA_KEYWORD_ELSE))
        {
          skipWhiteSpace();
        }
        else if(adaKeywordCmp(ADA_KEYWORD_IF) ||
                adaKeywordCmp(ADA_KEYWORD_ELSIF))
        {
          skipPastKeyword(ADA_KEYWORD_THEN);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_CASE))
        {
          skipPastKeyword(ADA_KEYWORD_IS);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_WHEN))
        {
          skipPast("=>");
        }
        else
        {
          /* set token to NULL so we don't accidentally not find an identifier,
           * But then fall through to the != NULL check */
          token = NULL;

          /* there is a possibility that this may be a loop or block
           * identifier, so check for a <random_word>: statement */
          for(i = 1; (pos + i) < lineLen; i++)
          {
            /* if we hit a non-identifier character (anything But letters, _
             * and ':' then this is not an identifier */
            if(!isalnum(line[pos + i]) && line[pos + i] != '_' &&
               line[pos + i] != ':')
            {
              /* if this is not an identifier then we should just bail out of
               * this loop now */
              break;
            }
            else if((line[pos + i] == ':') && (line[pos + i + 1] != '='))
            {
              token = newAdaToken(&line[pos], i, ADA_KIND_IDENTIFIER, FALSE,
                                  parent);
              break;
            }
          } /* for(i = 1; (pos + i) < lineLen; i++) */

          /* if we created a token, we found an identifier.  Now check for a
           * declare or begin statement to see if we need to start parsing
           * the following code like a root-style token would */
          if(token != NULL)
          {
            /* if something was found, reset the position variable and try to
             * find the next item */
            movePos(i + 1);
            skipWhiteSpace();

            if(adaKeywordCmp(ADA_KEYWORD_DECLARE))
            {
              adaParse(ADA_DECLARATIONS, token);
            }
            else if(adaKeywordCmp(ADA_KEYWORD_BEGIN))
            {
              adaParse(ADA_CODE, token);
            }
            else if(adaKeywordCmp(ADA_KEYWORD_FOR))
            {
              /* just grab the automatic loop variable, and then parse the
               * loop (it may have something to tag which will be a 'child'
               * of the loop) */
              adaParseLoopVar(token);
              adaParse(ADA_CODE, token);
            }
            else if(adaKeywordCmp(ADA_KEYWORD_WHILE))
            {
              /* skip to the loop keyword */
              skipPastKeyword(ADA_KEYWORD_LOOP);
              skipWhiteSpace();

              /* parse the loop (it may have something to tag which will be
               * a 'child' of the loop) */
              adaParse(ADA_CODE, token);
            } /* else if(adaKeywordCmp(ADA_KEYWORD_WHILE)) */
            else if(adaKeywordCmp(ADA_KEYWORD_LOOP))
            {
              skipWhiteSpace();

              /* parse the loop (it may have something to tag which will be
               * a 'child' of the loop) */
              adaParse(ADA_CODE, token);
            }
            else
            {
              /* otherwise, nothing was found so this is not a valid identifier,
               * delete it */
              freeAdaToken(&parent->children, token);
            }
          } /* if(token != NULL) */
          else
          {
            /* since nothing was found, simply skip to the end of this
             * statement */
            skipPast(";");
          }
        } /* else... No keyword tag fields found, look for others such as
           * loop and declare identifiers labels or just skip over this
           * line */

        break;

      case ADA_EXCEPTIONS:
        if(adaKeywordCmp(ADA_KEYWORD_PRAGMA))
        {
          skipPast(";");
        }
        else if(adaKeywordCmp(ADA_KEYWORD_WHEN))
        {
          skipWhiteSpace();
          token = adaParseVariables(parent, ADA_KIND_AUTOMATIC_VARIABLE);
        }
        else if(adaKeywordCmp(ADA_KEYWORD_END))
        {
          /* if we have hit an end then we must see if the next word matches
           * the parent token's name.  If it does we hit the end of whatever
           * sort of block construct we were processing and we must
           * return */
          skipWhiteSpace();
          if(adaCmp(parent->name))
          {
            skipPast(";");

            /* return the token */
            freeAdaTokenList(&genericParamsRoot.children);
            return token;
          } /* if(adaCmp(parent->name)) */
          else
          {
            /* otherwise, nothing was found so just skip until the end of
             * this statement */
            skipPast(";");
          }
        } /* else if(adaKeywordCmp(ADA_KEYWORD_END)) */
        else
        {
          /* otherwise, nothing was found so just skip until the end of
           * this statement */
          skipPast(";");
        }

        break;

      default:
        Assert(0);
    } /* switch(mode) */
  } /* while(exception == EXCEPTION_NONE)  */

  freeAdaTokenList(&genericParamsRoot.children);
  return token;
}

static void storeAdaTags(adaTokenInfo *token)
{
  adaTokenInfo *tmp = NULL;

  if(token != NULL)
  {
    /* do a spec transition if necessary */
    if(token->isSpec == TRUE)
    {
      makeSpec(&token->kind);

      if(token->kind != ADA_KIND_UNDEFINED)
      {
        token->tag.kindName = AdaKinds[token->kind].name;
        token->tag.kind = AdaKinds[token->kind].letter;
      }
    }

    /* fill in the scope data */
    if(token->parent != NULL)
    {
      if(token->parent->kind > ADA_KIND_UNDEFINED &&
         token->parent->kind < ADA_KIND_COUNT)
      {
        token->tag.extensionFields.scope[0] =
          AdaKinds[token->parent->kind].name;
        token->tag.extensionFields.scope[1] = token->parent->name;
      }
      else if(token->parent->kind == ADA_KIND_SEPARATE)
      {
        token->tag.extensionFields.scope[0] = AdaKeywords[ADA_KEYWORD_SEPARATE];
        token->tag.extensionFields.scope[1] = token->parent->name;
      }
    } /* else if(token->parent->kind == ADA_KIND_ANONYMOUS) */
  } /* if(token->parent != NULL) */

  /* one check before we try to make a tag... If this is an anonymous
   * declare block then it's name is empty.  Give it one */
  if(token->kind == ADA_KIND_ANONYMOUS && token->name == NULL)
  {
    token->name = (char *) AdaKeywords[ADA_KEYWORD_DECLARE];
    token->tag.name = AdaKeywords[ADA_KEYWORD_DECLARE];
  }

  /* now 'make' tags that have their options set, But only make anonymous
   * tags if they have children tags */
  if(token->kind > ADA_KIND_UNDEFINED && token->kind < ADA_KIND_COUNT &&
     AdaKinds[token->kind].enabled == TRUE &&
     ((token->kind == ADA_KIND_ANONYMOUS && token->children.head != NULL) ||
      token->kind != ADA_KIND_ANONYMOUS))
  {
    makeTagEntry(&token->tag);
  }

  /* now make the child tags */
  tmp = token->children.head;
  while(tmp != NULL)
  {
    storeAdaTags(tmp);
    tmp = tmp->next;
  }

  /* we have to clear out the declare name here or else it may cause issues
   * when we try to process it's children, and when we try to free the token
   * data */
  if(token->kind == ADA_KIND_ANONYMOUS &&
     strncasecmp(token->name, AdaKeywords[ADA_KEYWORD_DECLARE],
                 strlen((char *) AdaKeywords[ADA_KEYWORD_DECLARE])) == 0)
  {
    token->name = NULL;
    token->tag.name = NULL;
  }
}

/* main parse function */
static void findAdaTags(void)
{
  adaTokenInfo root;
  adaTokenInfo *tmp;

  /* init all global data now */
  exception = EXCEPTION_NONE;
  line = NULL;
  pos = 0;
  matchLineNum = 0;
  matchFilePos = 0;

  /* init the root tag */
  root.kind = ADA_KIND_UNDEFINED;
  root.isSpec = FALSE;
  root.name = NULL;
  root.parent = NULL;
  initAdaTokenList(&root.children);

  /* read in the first line */
  readNewLine();

  /* tokenize entire file */
  while(exception != EXCEPTION_EOF && adaParse(ADA_ROOT, &root) != NULL);

  /* store tags */
  tmp = root.children.head;
  while(tmp != NULL)
  {
    storeAdaTags(tmp);
    tmp = tmp->next;
  }

  /* clean up tokens */
  freeAdaTokenList(&root.children);
}

/* parser definition function */
extern parserDefinition* AdaParser(void)
{
  static const char *const extensions[] = { "adb", "ads", "Ada", NULL };
  parserDefinition* def = parserNew("Ada");
  def->kinds = AdaKinds;
  def->kindCount = ADA_KIND_COUNT;
  def->extensions = extensions;
  def->parser = findAdaTags;
  return def;
}

/*
 * vim:ff=unix
 */
