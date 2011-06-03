#! perl
#*******************************************************************
#FILE           files2menu.pm
#PACKAGE        gesamt
#ABSTRACT       finds files in all subdirs starting in parent dir
#SCCS           %W% , %E%; $Revision$ $Date$ 
#BY             Ramming	(Tue, 3.Apr.2001, 15:33:1)
#DESCR          finds files in all subdirs starting in parent dir
#				creates menu commands for gvim for each found
#
#				Usage: $ThisScript <options>\n\nVersion $VERSION
#				options:
#				                    generate file list on stdout (default)
#				 -o <filelist>      generate filelist in file <filelist>
#				 -v                 generate VIM scriptfile
#				 -v <scriptfile>    generate VIM Scriptfile <scriptfile>
#				 -f <patternlist>   use search pattern <patternlist>
#				                    predefined pattern lists:
#				                    CPP, EXE, MAK, GL, DOC, VIM
#				 -m                 create menu entry <menuEntry> for each found file
#				                    (blanks allowed in name)
#				 -c                 create submenu after <n> entries
#				 -d                 debug mode
#				 -s <startdir>      start in CWD (default) or parent dir ('..') or <startdir>
#									file in a script file.
#LEVEL          ?
#DEPENDS_ON     ls (GNU fileutils) 3.16, vim5.8
#NOTES          ?
#*******************************************************************

$VERSION = "1.0 (7/2001) - Th.Ramming";

$LS_CMD = "ls -A -f";

sub printUsage
{
	# get base name of this script
	$ThisScript = $0;
	$ThisScript =~ s#.*\\##g; # substitute backslashes with slashes
	($ThisScript) = split ('\.',$ThisScript);
	$ThisScript = lc($ThisScript);

	print "\nUsage: $ThisScript <options>\n\nVersion $VERSION\n";
	print "\noptions:\n";
    print "                    generate file list on stdout (default)\n";
    print " -o <filelist>      generate filelist in file <filelist>\n";
    print " -v                 generate VIM scriptfile\n";
    print " -v <scriptfile>    generate VIM Scriptfile <scriptfile>\n";
    print " -f <patternlist>   use search pattern <patternlist>\n";
    print "                    predefined pattern lists:\n";
    print "                    CPP, EXE, MAK, GL, DOC, VIM\n";
    print " -m                 create menu entry <menuEntry> for each found file\n";
    print "                    (blanks allowed in name)\n";
    print " -c                 create submenu after <n> entries\n";
    print " -d                 debug mode\n";
    print " -s <startdir>      start in CWD (default) or parent dir ('..') or <startdir>\n";
}


#---- defaults -----------------------------------------------------
 
@SRC_SEARCHPAT = ("*.c", "*.cpp", "*.c++", "*.cxx", "*.h", "*.hxx", "*.h++", "*.hfpi", "*.800");
@EXE_SEARCHPAT = ("*.exe", "*.com", "*.bat", "*.pm", "*.pl");
@GL_SEARCHPAT = ("*gl.dat");
@MAK_SEARCHPAT = ("*.mak", "makefile", "genmak.bat", "*.bld");
@DOC_SEARCHPAT = ("*.txt", "*.1st", "*.doc", "*.lst");
@VIM_SEARCHPAT = ("*.vim");

#@SearchPat = @SRC_SEARCHPAT; # may select another set of filetypes here
@SearchPat = @SRC_SEARCHPAT; # may select another set of filetypes here
$VIM_SCRIPT = "files.vim";	 # vim script file to be generated
$VIM_MENU	= "Files";		 # menu entry under which files are entered
$START_PARENTDIR = 1;		 # 1: start in parent dir, else in current dir
$MAX_CNT_LEVELENTRIES = 40;	 # max count of entries in a submenu


#-------------------------------------------------------------------
# get options
# options:
#TODO: implement command line interface
# 						generate file list on stdout (default)
# -o <filelist>			generate filelist in file <filelist>
# -v					generate VIM scriptfile (default)
# -v <scriptfile>		generate VIM Scriptfile <scriptfile>
# -find <patternlist>	use search pattern <patternlist>
# -menu 				create menu entry <menuEntry> for each found file
#						(blanks allowed in name)
# -heiht				create submenu after <n> entries
# -debug
# -startdir <startdir>	start in CWD (default) or parent dir ('..') or <startdir>

# examine options and args
require 'Getopts.pl';
#do Getopts('dhmv:f:s:c:');
if (Getopts('dhm:v:f:s:c:o:') == 0) { printUsage(); exit();}

$DBG = $opt_d;

if ($opt_h) {printUsage(); exit();}
if ($opt_c) {$MAX_CNT_LEVELENTRIES = $opt_c;}
if ($opt_m) {$VIM_MENU = $opt_m;}
if ($opt_v) {$VIM_SCRIPT = $opt_v;}
if ($opt_o) {$LISTFILE = $opt_o;}
if ($opt_s) {$StartDir = $opt_s; $START_PARENTDIR = 0;}

if ($opt_f eq "CPP") {@SearchPat = @SRC_SEARCHPAT;}
else { if ($opt_f eq "CPP") {@SearchPat = @SRC_SEARCHPAT;}
else { if ($opt_f eq "EXE") {@SearchPat = @EXE_SEARCHPAT;}
else { if ($opt_f eq "GL") {@SearchPat = @GL_SEARCHPAT;}
else { if ($opt_f eq "MAK") {@SearchPat = @MAK_SEARCHPAT;}
else { if ($opt_f eq "DOC") {@SearchPat = @DOC_SEARCHPAT;}
else { if ($opt_f eq "VIM") {@SearchPat = @VIM_SEARCHPAT;}
else { if ($opt_f) {@SearchPat = split ' ',$opt_f;}
}}}}}}} # if else ends

#-------------------------------------------------------------------

$VIM_MENU =~ s# #\\ #g;		 # hide blanks in menu command


use Cwd;
if ($StartDir eq ".")  { $StartDir = cwd(); }
if ($StartDir eq "..") { $START_PARENTDIR = 1; }

# find out parent dir name (localdir - last path entry)
if ($START_PARENTDIR) {
	$StartDir = cwd();
	$StartDir =~ s#(^.*)/.*$#\1#; # cwd must support '/' between dirs
}


if ($DBG) {
	print "$0";
	if ($opt_c) { print " -c $MAX_CNT_LEVELENTRIES";}
	if ($opt_m) { print " -m $VIM_MENU";}
	if ($opt_v) { print " -v $VIM_SCRIPT";}
	if ($opt_s) { print " -s $StartDir";}
	if ($opt_f) { print " -f @SearchPat";}
	print "\n";
}

unlink ($VIM_SCRIPT);

if ($DBG) { print "Startdir = $StartDir\n";}

sub createVimMenu
{
	local (@RawEntries) = @_;
	local ($Entry, @Words, $Word,$Line);
	local ($MenuLevel,$NewEntryLevel,@MenuEntryCnt,@SubMenuName);

	if ($DBG) { foreach (@RawEntries) { print "create menu for $_\n";}}
	
	# menu cmd for gvim: 
	# 'menu 12.50 Proj.Locals.findfile\.pm	:e findfile.pm<CR>'
	# raw:
	#'p:/Audion/sw/test/base_demo2/Display01/AD-WANDL/Ad.h'
	open (FH_VIM,">>$VIM_SCRIPT") or die "could not open $VIM_SCRIPT";
	
	print FH_VIM ":menu  12.55 Proj.-SEP9-			:<CR>\n";
	print FH_VIM ":menu 12.55 Proj.remove\\ menu\\ '$VIM_MENU'	:unmenu 12.55 Proj.$VIM_MENU<CR>:unmenu 12.55 Proj.remove\\ menu\\ '$VIM_MENU'<CR>:unmenu 12.55 Proj.-SEP9- <CR>\n";
	
	$MenuLevel = 1;
	foreach $Entry (@RawEntries) {
		@Words = split /\//, $Entry;
		$Line  = ":menu 12.55 Proj.$VIM_MENU";

		$NewEntryLevel = 0;
		foreach $Word(@Words) {
			$Word =~ s#\.#\\\.#g;
			$NewEntryLevel++;
			while ($SubMenuName[$NewEntryLevel] ne "") {
				$Line = $Line.".$SubMenuName[$NewEntryLevel]";
				$NewEntryLevel++;
			}
			$Line = $Line.".$Word";
		}

		$MenuEntryCnt[$NewEntryLevel]++;

		if ($MenuEntryCnt[$NewEntryLevel] > $MAX_CNT_LEVELENTRIES ) {
			$SubMenuName[$NewEntryLevel] = $MAX_CNT_LEVELENTRIES."more";
		}

		if ($NewEntryLevel < $MenuLevel) {
			$SubMenuName[$NewEntryLevel] = "";
			$MenuEntryCnt[$NewEntryLevel] = 0;
		}

		$MenuLevel = $NewEntryLevel;
		
		$Line = $Line." :e $Entry<CR>";
		print FH_VIM "$Line\n";
	}

	close (FH_VIM);

}

sub createFileList
{
	local $ActDir = shift;
	local @FileList;
	local $FN_ERR = "lserror.tmp";

	foreach $Pat (@SearchPat) {
		# glob cuts filenames to 8 chars (like dosprompt)
		#@Files =  grep -f, glob("$ActDir/$Pat");
		@Files = `$LS_CMD $ActDir/$Pat 2>$FN_ERR`; # only files, no '.', '..'
		unlink ($FN_ERR);
		map { chop;} @Files;
		if ($#Files != -1) {
			push @FileList, @Files;
		}
	}

	if (($LISTFILE ne "") && open(FH_OUT,">$LISTFILE")) {
		#print FH_OUT "File list:\n---------\n";
		foreach $File (@FileList) {
			#print FH_OUT "File list:\n---------\n";
			print FH_OUT "File: \n";
			#print "File: \n";
		}
		close (FH_OUT);
	} else {
		foreach $File (@FileList) {
			print "$File\n";
		}
	}
	 
	@FileList;
}

sub do_cmd
{
	local $Dir = shift;
	local $Cmd = shift;
	local (@Files);

	# TODO: use $Cmd, createVimMenu is a fix example for $Cmd
	@Files = createFileList($Dir);
	if ($#Files != -1) {
		createVimMenu(@Files);
	}
}

# recursively find deepest directory:
sub findSubDir
{
	local $Start = shift;
	local $Cmd = shift;
	local (@DirList);

	if ($DBG) {print "working in '$Start'\n";}

	# read dir entries
	@DirList = grep -d, glob("$Start/*");
	
	# foreach direntry do the same
	foreach (@DirList) {
		findSubDir($_,$Cmd);
	}

	# if no direntry exists,
	#if ($#DirList == -1) {
		# execute command
		#print "  $Start>$Cmd\n";
		do_cmd($Start,$Cmd);
	#}
}

findSubDir($StartDir,"command");

