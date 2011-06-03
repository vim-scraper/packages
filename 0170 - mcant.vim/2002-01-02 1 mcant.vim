"a simple setting for getting the most out of ant.
"just execute :mak [target name], and ant will find the first 
"buildfile it can, then execute it. the emacs setting
"lets vim automagically parse the error msgs. :cn, :cc, and 
":cl work like a charm. I turned down ant's normal
"chatty output with -quiet. Edit to taste.
"
"Happy jabba coding!
"
"-mcclain

set makeprg=ant\ -quiet\ -emacs\ -find\ build.xml 
