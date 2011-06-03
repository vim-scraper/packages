" Author: Dave Eggum (deggum@synopsys.com)
" Version: 0.3
" Last Change: Mon Jan 24 16:18:54 PST 2005
"
" Description:
"    The focus of this script is to provide context-sensitive word completion.
"    At this point, context-sensitive word completion is limited members of an
"    object, static classes (i.e. CUtils::complete_this) and structs.
"
"    I expect this script to (mostly) work with many Object Oriented
"    Languages, but the script has only been tested with Vera and C++ so far.
"    If it doesn't work well for the language you are using, then please let
"    me know!  Also, if you have some sample Java code that I could test this
"    script on then I would appreciate seeing it!
"
"    This script relies heavily on exuberant ctags and the perl extension in
"    vim. If you do not have ctags, then you can pick it up for free at
"    http://ctags.sf.net (you should be using it anyway, see ":help tags"). If
"    you do not know if you have the perl extension to vim installed, you can
"    find out by typing ":version" in vim. If you see "+perl" in the output,
"    then you've got it. If you don't, then see ":help perl-compiling" for (a
"    little) help. You will need to recompile vim with the perl feature
"    enabled. See the documentation provided with the vim distribution for
"    instructions on how to do that.
"

" Usage:
"    Note: The usage of this script is modeled after vim's insert-expand
"    feature (See ":help i_CTRL-X") and is best explained with an example...
"
"    Suppose you have an instance of an object (o), and you've typed:
"
"    o.set_
"
"    While you are still in insert mode, press control-q, and this script
"    will then look up the first member of that object that starts with "set_"
"    and complete it for you, like this:
"
"    o.set_range(integer max, integer min)
"
"    If this isn't the completion that you want, then type control-q again (or
"    control-j) for the next member that starts with "set_":
"
"    o.set_name(string name)
"
"    and again for the next one:
"
"    o.set_value
"
"    If you've gone too far then you can go back with control-k. After each
"    completion you are left in insert mode - so you can just continue typing
"    when you've found the one you want.
"
"    This script looks up the type of the object instance before the "." or
"    "->" by first looking for a local definition, if it doesn't find one then
"    it looks in the tag file. If it finds more than one object type in the
"    tags file, then it will provide completions for the first one it finds.
"    If it chooses the wrong type, then press control-l to move to the next
"    object definition.
"
" Setup:
"    Place this file in your "plugin" directory.
"
"    You must run ctags on your code, enabling as many kinds as possible. Try
"
"    ctags --list-kinds=all
"
"    for a listing of all the options available for each language. You then
"    need to set the "tags" vim option to point to your tags file. See ":help
"    'tags'" for how to do this.
"
"    Notice that after time, your tags file will be out of date with the
"    current version of your code. All you have to do is run ctags again to
"    get a new picture of your environment. You have to do this if you use
"    vim's built-in features for tags anyway...
"
" Todo:
"    - test against java code.
"    - try to implement a basic understanding of class inheritance.
"    - arrays are not understood.
"    - continue context completion after a function, very much needed for C++
"      and Java (var.getObject().<c-q>). This could be slow...
"    - suggest function parameter completion when the fill name is provided:
"      util.do_stuff(<c-q>
"
" History:
"    0.3 - static members are completed: i.e. ClassName::completeThis...
"        - C structs are now understood and treated like classes
"        - local variable definition detection is much better... it uses more
"          of vim's built-in features. (yay!)
"        - more bug fixes. Finding the definition of a local variable is much
"          more solid.
"        - *removed* the variable complete feature... the added complexity
"          wasn't worth it. Will put it back in if there is enough demand for
"          it.
"    0.2 - tested with C++.
"        - object names can now be a local variable as well as a member
"          variable.
"        - variables can now be expanded as well.
"        - added more documenation
"    0.1 - initial release

perl << .
sub dprint() {
   $debugging = 0	unless defined $debugging;
   if ($debugging) {
      print "@_\n";
   }
}

sub get_tags_file() {
   my $tags = VIM::Eval("&tags");
   my @files = split(/,/, $tags);
   my $tagfile;
   while ($tagfile = shift @files) {
      return $tagfile if (-r $tagfile);
   }

   return "";
}

sub is_function_definition() {
   my ($line) = shift;
   return 1 if ($line =~ /^[\w\s]*(task|function)/); # a vera task/function
   return 1 if ($line =~ /^\w+\s+\w+::/); # a c++ function def
   return 1 if ($line =~ /^(\w+)::~?\1/); # a c++ constructor
   return 0;
}

# returns an empty string if no type is found
sub find_local_type() {
   my ($success, $val) = VIM::Eval("FindLocalVariableLine()");
   $val =~ s/[\*&]//g;
   $val =~ s/.*;\s*//g;
   $val =~ s/.*\(//;
   $val =~ s/\w+\s*,//g;

   $val =~ /(\S+)\s*$/;
   return $1;
}

sub find_tag_completions() {
   my ($file, $tag, $findtypes) = @_;
   my (%types);

   &dprint("find_tag_completions: looking in tags file for $tag");
   open(TAGSFILE, $file);
   while (<TAGSFILE>) {
      if (/^($tag)\b.*\/\^.*?\b(\w+)\s*\*?\s*$tag/)
      {
         my $first = $1;
         my $second = $2;
         unless ($second =~ /task/)
         {
            &dprint("find_tag_completions: found tag >$first< >$second<");
            $types{($findtypes == 1)?$second:$first} = 1;
         }
      }
   }
   close TAGSFILE;

   return keys %types;
}

sub get_tag_types() {
   my ($file, $tag, $delim) = @_;

   if ($delim =~ /::/)
   {
      &dprint("get_tag_types: static var, returning $tag");
      return $tag;
   }

   # first look for a local definition of this tag
   my $type = &find_local_type();
   unless (length $type == 0)
   {
      &dprint("get_tag_types: found local type, returning $type");
      return $type;
   }
   &dprint("get_tag_types: looking for tag in tags file...");
   my @types = &find_tag_completions($file, $tag, 1);
   return @types;
}

sub get_members_of_class() {
   my ($file, $class, $pat) = @_;

   &dprint("get_members_of_class: looking for pat:$pat. in class:$class.");
   my $p = "(class|enum|struct)";
   my (%members);
   open(TAGSFILE, $file);
   while (<TAGSFILE>) {
      if (/(\b$pat\w*\s*\(.*\)).*$p:$class/) {
         $members{$1} = 1;
      }
      elsif (/(\b$pat\w*\s*\(.*)\$.*$p:$class/) {
         $m = &find_rest_of_prototype($file, $_);
         $members{"$1$m"} = 1;
      }
      elsif (/^(\b$pat\w*).*$p:$class/) {
         $members{$1} = 1;
      }
   }
   close TAGSFILE;
   my @members = keys %members;
   return @members;
}

sub find_rest_of_prototype() {
   my ($tagfile, $line) = @_;

   $line =~ /\w+\s+(\S+)/;
   my ($file) = $1;
   if ($file !~ /^\//) {
      # this isn't an absolute path
      $file = "$tagfile$file";
      $file =~ s/tags//;
   }
   if (!-r $file) {
      VIM::Msg("Cannot find file $file");
      return "";
   }

   $line =~ /\/\^(.+)\$/;
   my $searchpat = quotemeta $1;
   my $keeplooking = 0;
   my $rest = "";
   open(SOURCEFILE, $file);
   while (<SOURCEFILE>) {
      if (/^$searchpat$/) {
         $keeplooking = 1;
      }
      elsif ($keeplooking) {
         if (/\)/) {
            s/\s+(\w*.*\)).*/$1/;
            chomp;
            $rest .= " $_";
            last;
         }
         else {
            s/\s+(\w+.*,?).*/$1/;
            chomp;
            $rest .= " $_";
         }
      }
   }
   close SOURCEFILE;
   return $rest;
}

sub study_line() {
   my ($linenum, $col) = $curwin->Cursor();
   my $line = $curbuf->Get($linenum);
   my $after = substr($line, $col+1);
   $line = substr($line, 0, $col+1);

   my @tags = split(/([\. \(]|->|::)/, $line);
   my ($pat, $tag);

   my $before = $line;
   my $delim = "";
   if ($line =~ /(\.|->|::)$/)
   {
      # looking for all members of this tag
      $pat = "";
      $delim = pop @tags; # pop delim
      $tag = pop @tags;
   }
   elsif ($line =~ /(\.|->|::)/)
   {
      # looking for only members that start with the pattern after the . or ->
      $pat = quotemeta(pop @tags);
      $before =~ s/(\.|->|::)$pat\s*$/$1/;
      $delim = pop @tags;
      $tag = pop @tags;
   }

   &dprint("study_line: lnum:$lnum,before:$before,after:$after,tag:$tag,pat:$pat,delim:$delim.");
   return ($linenum, $before, $after, $tag, $pat, $delim);
}

sub in_complete_mode() {
   my $start = shift;

   unless (defined $complete_mode) {
      $complete_mode = $start;
      return $complete_mode;
   }
   if ($complete_mode == 0) {
      return $complete_mode;
   }

   my ($l, $c) = $curwin->Cursor();
   if ($l == $lnum && $c == $col) {
      $complete_mode = 1;
   }
   elsif ($col == -1) {
      $complete_mode = 0;
   }
   else {
      $complete_mode = 0;
   }
   return $complete_mode;
}

sub do_next_entry() {
   my $direction = shift;
   if ($direction =~ /F/) {
      $memberindex = 0;
      $complete_mode = 1;
   }
   elsif ($direction =~ /N/) {
      if (&in_complete_mode(0)) {
         $memberindex++;
         $memberindex = 0 if ($memberindex > $#members);
      }
      else {
         &leave_in_insert_mode();
         return;
      }
   }
   elsif ($direction =~ /P/) {
      if (&in_complete_mode(0)) {
         $memberindex--;
         $memberindex = $#members if ($memberindex < 0);
      }
      else {
         &leave_in_insert_mode();
         return;
      }
   }

   &dprint("do_next_entry: before:$before,memberindex:$memberindex,member:$members[$memberindex]");
   my $newline = "$before$members[$memberindex]";
   $col = (length $newline) - 1;
   $newline .= $after;

   $curbuf->Set($lnum, $newline);
   $curwin->Cursor($lnum,$col);
   &leave_in_insert_mode();
}

sub leave_in_insert_mode() {
   my ($linenum, $col) = $curwin->Cursor();
   my $line = $curbuf->Get($linenum);

   if (length $line == $col+1) {
      VIM::DoCommand("startinsert!");
   }
   else {
      VIM::DoCommand("normal l");
      VIM::DoCommand("startinsert");
   }
}

sub use_next_tag() { 
   $typeindex++;
   $typeindex = 0 if ($typeindex > $#types);

   @members = &get_members_of_class($tagsfile, $types[$typeindex], $pat);
   my $found = $#types + 1;
   if ($#members == -1)
   {
      VIM::Msg("No members for type $types[$typeindex]. ($found types)");
      &leave_in_insert_mode();
      return;
   }
   VIM::Msg("members for $types[$typeindex]. ($found types)");

   push @members, $pat;
   &do_next_entry("F");
}

sub context_complete() {
   $debugging = 0;
   ($lnum, $before, $after, $tag, $pat, $delim) = &study_line();

   if (length $delim == 0)
   {
      &leave_in_insert_mode();
      return;
   }

   $col = -1 unless (defined $col);
   if (&in_complete_mode(0)) {
      &dprint("context_complete: already in complete mode...");
      &do_next_entry("N");
      return;
   }

   $tagsfile = &get_tags_file;
   if (length $tagsfile == 0) {
      VIM::Msg("No tags file found!");
      &leave_in_insert_mode();
      return;
   }

   @types = &get_tag_types($tagsfile, $tag, $delim);
   if ($#types == -1) {
      VIM::Msg("No tags found for $tag!");
      &leave_in_insert_mode();
      return;
   }

   $typeindex = -1;
   &use_next_tag();
   $debugging = 0;
}
.

function! FindLocalVariableLine()
   set lazyredraw

   " save the current spot
   let linenum = line(".")
   let col = col(".")

   " find the topline in order to restore the cursor position relative to the
   " screen
   normal H
   let topline = line(".")
   call cursor(linenum, col)

   " Use vim's "godo local declaration" feature (gd) and determine the variable type
   normal Bgd
   let line = getline(".")
   let c = col(".")
   let line = strpart(line, 0, c-1)

   " restore the cursor and screen positions
   call cursor(topline, 0)
   normal zt

   call cursor(linenum, col)
   set nolazyredraw
   return line
endfunction

inoremap <silent> <C-Q> <ESC>:perl -w &context_complete()<cr>
inoremap <silent> <C-J> <ESC>:perl -w &do_next_entry("N")<cr>
inoremap <silent> <C-K> <ESC>:perl -w &do_next_entry("P")<cr>
inoremap <silent> <C-L> <ESC>:perl -w &use_next_tag()<cr>

" vim: fdm=indent:sw=3:ts=3
