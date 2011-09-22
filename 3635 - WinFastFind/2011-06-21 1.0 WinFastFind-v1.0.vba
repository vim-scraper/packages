" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc\WinFastFind.txt	[[[1
152
*winfastfind.txt*	For Vim version 7.3.  Last change: 2011 Jun 06

			       ---------------------
			       Fast Find for Windows
			       ---------------------

Author:  Venkat Mandela <vNOeSPnkat.mAMandela@gmail.com>
	  (remove NOSPAM from email first)

Copyright: (c) 2011 by Venkat Mandela        *winfastfind-copyright*
	   The VIM LICENSE applies to WinFastFind.vim, and WinFastFind.txt
	   (see |copyright|) except use "WinFastFind" instead of "Vim".
	   No warranty, express or implied.
	   Use At-Your-Own-Risk!

==============================================================================

1. Contents				                   *WinFastFind* *WinFastFind-contents*

	1. Contents...........................................: WinFastFind-contents
	2. Overview...........................................: WinFastFind-overview
	3. Installation ......................................: WinFastFind-install
	4. Usage..............................................: WinFastFind-usage
	5. Configuration......................................: WinFastFind-params

==============================================================================

2. Overview                                     *WinFastFind-overview*

I work on Windows at work. I have frequently found the "find" command on Vim to
lag when the "path" variable is set to recurse through a large directory. This
is my attempt to solve this problem.

I am using an external filename search tool called "Everything" along with a
sed script to find for files by name from VIM. 

==============================================================================

3. Installation							WinFastFind-install

To use this plugin, you need the below external tools. All of them are free.
Except for Perl, all of them have installers in KB.

1. Perl - I use strawberry perl but any other version of Perl should work as
well. There is no dependency on any perl modules.

http://strawberryperl.com/

2. Sed - I use sed from GnuWin32 on my system.

http://gnuwin32.sourceforge.net/packages/sed.htm

3. Everything - This is a windows file name search tool available from 
http://www.voidtools.com/Everything-1.2.1.371.exe

4. Everything command line Interface - This is a command line to obtain search
results from Everything database.  

http://www.voidtools.com/es.exe

After installing the above tools, ensure that Perl,sed and es.exe are present
in the system path. This can be verified by running.

1. Perl -v
2. sed --help
3. es --help

Once the external tools are setup, install the plugin using the Vimball or the
zip file. The plugin currently consists of three files.

plugin\WinFastFind.vim
plugin\handlesubsts.pl
doc\WinFastFind.txt

==============================================================================

4. Usage 											*WinFastFind-usage*

Set the VIM path variable to paths you need to be searched.

e.g.

:set path+=c:/cygwin/usr/include/**

The plugin defines one function

FastFindFile()

You can call the function by typin

:call FastFindFile("main.c")

For convenience, the plugin defines a command Ff. The same command as above can be
executed using

:ff main.c

Only parts of file name can also be given

:ff ma n.c

The plugin also defines a key combination 'ff' in the normal mode, which can be used similar to the key combination 'gf'.

==============================================================================

5. Configuration parameters 						*WinFastFind-params*

g:winfastfind#ignorecase = 1

Set to 1 to ignore case both in filename and in path. Default is to ignore case.

g:winfastfind#matchpath = 1

When this parameter is set, the search string is matched in the path as well as file name.
This is a feature from Everything. Default is to match the path as it allows fuzzy searches.

g:winfastfind#ignorefiles = "swp,o,obj,a,pp,orig"

Add the extensions you want to ignore from the search. These are matched at the end of the filename only.

g:winfastfind#matchwholeword= 0

When this parameter is set, the full string is matched.

g:winfastfind#sortbypath = 1

When set, the search results are matched by path.

==============================================================================

6. How the plugin works 								*WinFastFind-execution*

On startup, the plugin generates a name for the sed script based on the VIM
instance name. Using the VIM instance name ensures that this script is unique
for each open instance of VIM. By default,this script is placed in the plugin
directory. The name of the script is the instance name. e.g. GVIM1.sed.

When you a search, the plugin first checks if any configuration parameters have
changed. If they have, a new sed script is generated. Then it proceeds to
invoke Everything and filter the results using "sed". The results show up in a
list from which you can select by clicking the file or entering the zero based
file index in the list.


==============================================================================

7. TODO list											*WinFastFind-execution*

1. Delete the generated sed script when exiting VIM
2. Append index numbers when displaying files so that it is easy to select.
3. Use Vim tab completion to allow user to do incremental searches.

plugin\WinFastFind.vim	[[[1
106
let g:sedscriptname = ""
let g:pathlistbuilt = 0
let g:sedscriptname = expand("<sfile>:p:h")."\\".v:servername.".sed"
let g:perlfilepath = expand("<sfile>:p:h")."\\handlesubsts.pl"
let g:winfastfind#ignorecase = 1
let g:winfastfind#matchpath = 1
let g:winfastfind#ignorefiles = "swp,o,obj,a,pp,orig"
let g:winfastfind#matchwholeword= 0
let g:winfastfind#sortbypath = 1
let g:winfastfind#escmdline = ""

" Build the command line for search
function s:BuildEsCommandLine()

	let g:winfastfind#escmdline ="es.exe" 

	if(!g:winfastfind#ignorecase)
		let g:winfastfind#escmdline = join( [g:winfastfind#escmdline,"-i"]," ")
	endif

	if(g:winfastfind#matchpath)
		let g:winfastfind#escmdline = join( [g:winfastfind#escmdline,"-p"]," ")
	endif

	if(g:winfastfind#matchwholeword)
		let g:winfastfind#escmdline = join( [g:winfastfind#escmdline,"-w"]," ")
	endif

	if(g:winfastfind#sortbypath)
		let g:winfastfind#escmdline = join( [g:winfastfind#escmdline,"-s"]," ")
	endif
endfunction

call s:BuildEsCommandLine()

function s:BuildParamList()
	let localList = [ &path, g:winfastfind#ignorecase, g:winfastfind#matchpath, g:winfastfind#ignorefiles]
	return localList
endfunction

let g:winfastfind#prevparamlist = s:BuildParamList()
"Decho g:winfastfind#prevparamlist

"Decho g:winfastfind#prevparamlist
"Decho g:sedscriptname
"Decho g:perlfilepath

"TODO Add cleanup of the generated sed script on exit.
"TODO Write a real makefile

function! g:BuildPathList()
"	Decho strftime("%c")
	let tempfilename = &path
"	Decho g:sedscriptname
	let cmdline = join(["perl",shellescape(g:perlfilepath), shellescape(&path),shellescape(g:sedscriptname),shellescape(g:winfastfind#ignorefiles),shellescape(g:winfastfind#ignorecase)]," ")
	let perlout = system(cmdline)
"	"Decho split(perlout,',')
	"call system("perl",g:perlfilepath,&path,g:sedscriptname)
	call s:BuildEsCommandLine()
	let g:pathlistbuilt = 1
endfunction

function! FastFindFile(...)

	let s:currparamlist = s:BuildParamList()

	if(!g:pathlistbuilt) || (g:winfastfind#prevparamlist!= s:currparamlist)
"		Decho "Rebuilding path script"
		call g:BuildPathList()
		let g:winfastfind#prevparamlist = s:currparamlist
	endif
	let filename = join(a:000," ")
	let sedcmd = "| sed -n -f "
	let searchresults =[]
	:try
	let pathcmdline = join([g:winfastfind#escmdline,filename,sedcmd,shellescape(g:sedscriptname)]," ")
"	Decho pathcmdline
	let searchresultsjoined = system(pathcmdline)
	call extend(searchresults,split(searchresultsjoined,nr2char(10)))
	:finally
	:endtry
"	Decho searchresults

	if empty(searchresults)
		echo "No file found"
	elseif len(searchresults)==1
		exec "edit" searchresults[0]
	else
		let fileIndex = inputlist(searchresults)
		exec "edit" searchresults[fileIndex]
	endif
endfunction

" Function mapping

" Type ff on top any file name to open it when in
" normal mode. This is similar to the gf command
map ff :call FastFindFile(expand("<cfile>"))<CR>

" Define a command Ff to execute from command file
" :Ff main.c
command -nargs=+ Ff call FastFindFile(<f-args>)

" Map the command Ff to ff for ease of use
" :ff main.c
execute 'cabbr ' . 'ff' . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . 'Ff' . '" : "' . 'ff' . '"<CR>'
plugin\handlesubsts.pl	[[[1
64
use strict;
use warnings;
use Data::Dump qw(dump);

my $vimpath = "";
$vimpath = shift;
my $tempSedFile= shift;
my $ignorePath = shift;
my $ignorecase= shift;
my $return = 1;
my @outputList = [];
my %driveMaps = ();

my @substStatus = `subst`;

#TODO Check for recursive drive mappings
for my $subst (@substStatus) {
	chomp $subst;
	my @splitLines = split(/\\:\s+=>\s+/,$subst);
	if($#splitLines==1){
		$driveMaps{$splitLines[0]}=$splitLines[1];
	}
}
#$dump(%driveMaps);
my $output= "";

my $fp;
open $fp,'>', $tempSedFile;
my @ignorepatterns=split(/,/,$ignorePath);

my $iext;
foreach $iext (@ignorepatterns){
	printf $fp "\/\\.".$iext."\$\/I {\nd\n }\n"; 
}

my @newpaths= ();
my $ignoreCaseStr="I";
if($ignorecase==0){
	$ignoreCaseStr="";
}

if(length($vimpath)!=0){
	my @paths = split(/,/,$vimpath);
	my $path;
	foreach $path (@paths){
		$path=~s/\*\*$//;
		$path=~s/\//\\/g;
		$path=~s/^\s*//;
		my $driveLetter = substr($path,0,2);
		if (exists $driveMaps{$driveLetter} ) {
			my $basePath=$driveMaps{$driveLetter};
			$path =substr($path,2);
			$path= $basePath.$path;
		}
		push (@newpaths,$path);
		$path=~s/\\/\\\\/g;
		$path=~s/ /\\ /g;
		printf $fp "\/".$path."/$ignoreCaseStr {\np\n }\n" unless ($path=~/^\.$/)|(length($path)==0);
	}
}
$output = join(',',@newpaths);
print $output;
close $fp;

