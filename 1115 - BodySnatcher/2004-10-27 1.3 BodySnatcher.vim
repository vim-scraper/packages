" First created: Oct 23 2004 (EDT)
" Last modified: 21:33(TZ:EDT) 27-Oct-2004

" **** $CVSHeader: perl-proj/other/vimplugins/bodysnatcher/BODY-snatcher.vim,v 1.3 2004/10/28 01:39:15 somian Exp $
" **** $Revision: 1.3 $

" Author: Soren Andersen <intrepid *AT* perlmonk -DOT- org>
" Copyright: (C)2004 Soren Andersen (USA)
" License: GPL v2 or later. This is Copyrighted software. It is not in the
" Public Domain.
" Depends: CPAN module "Text::Balanced" installed.

if has('perl')
perl <<EOthePERLCODE
  use vars ('$DeeBugg');

  sub VInform       {
      my $text = shift;
      VIM::DoCommand( join q| |, q|call confirm('|, $text, q|')| );
  }

  sub splitUnQ    {
       my %serenity = ();
       my ($attrrhs,$attrlhs,$tpos,$slim);
       chomp(my $wstr = shift);
       $wstr =~s#  \<\w+\s* ([^>]+)>  #$1#x; # remove the name ("body") and angle braces.
       my $npos = index($wstr,q{="}) || index($wstr,q{='}) || 0x0; #"
       $slim = 24;  # stop runaway loop
       unless ($npos < $[)  {
       do {
	   my $ssti;
	   my $savs = $wstr;
	   pos $wstr = $tpos || $npos - 1;
	   $npos = pos $wstr;
	   if ($npos) {
	       $ssti = do { my $idx = $npos;        --$idx while
	                           substr ($wstr,$idx,1) !~/\s/ and ($idx > 0) ;
		            $idx };
	       $attrrhs = Text::Balanced::extract_delimited( $wstr,q/"'/,q/=/ ); #"
	   }     # NOTE that extract_delimited *changes* pos() on the scalar it operates on.
	   if ($attrrhs)  {
		   $attrrhs = substr ($attrrhs,1,-1);
		   $attrlhs = substr ($savs, $ssti, $npos - $ssti);
		   $attrlhs  ||= 'NULL';
		   $attrlhs =~ s{\s*(\S+)\s*}  {$1};
		   $serenity{ $attrlhs } = $attrrhs;
		   substr ($wstr, $ssti, $npos - $ssti) = "";
		   $npos = pos $wstr;
	   }     # Again!
	         # NOTE that extract_delimited *changes* pos() on the scalar it operates on.
	  }
	       until (! --$slim) or ( $tpos = index $wstr, q{=} ) < $[;
       }
       my @barepair = ($wstr =~ /\S+\=/) ? #if there is any of the string left...
	          do { my @p = split q[\s+],$wstr;
		       map { s@^\s*(\S+)\s*\z@$1@ ; $_}
		          map (my @q = split(q{\s*=\s*},$_),@p)
		  } : ();
    if($DeeBugg)  {
       VInform("We found the pairs:\n" .
	       join "\n",
		 map (sprintf (qq{%13s  =  %-s}, ($_||q[NULL]), $serenity{$_}),
		   keys %serenity));
    }

       return { %serenity, @barepair };
  }


  sub hIppO        {
       my %nonStyle = ( "BACKGROUND" => "background-image"
	               ,"BGCOLOR"    => "background-color"
		       ,"TEXT"       => "color"
		       ,"LINK"       => "a:link"
		       ,"ALINK"      => "a:active"
		       ,"VLINK"      => "a:visited" );
       my $adata = splitUnQ(shift);
       my ($ila,$hsd);

       for (keys %$adata) {
	   my $canonolli = $_;
	      $canonolli =~ y/a-z/A-Z/;
	   do { VInform('WARNING: Unknown HTML-ish attribute name! - '. $canonolli);
	        next; }    unless exists $nonStyle{ $canonolli };

	   if        ( substr ($nonStyle{ $canonolli },0,2) eq q[a:] ) {
	     $hsd .= sprintf q[      %-13s { color : %s; }%s],
	                $nonStyle{ $canonolli },  $adata->{ $_ }, qq{\n};
	   } elsif   ( $canonolli eq 'BACKGROUND' ) {
	        $ila .=
		   sprintf q{%s : url(%s); },
	                $nonStyle{ $canonolli },  $adata->{ $_ };
	   } else {
	        $ila .=
		   sprintf      q{%s : %s; },
	                $nonStyle{ $canonolli },  $adata->{ $_ };
	   }
       }
       if  ( $hsd ) {
       my $a_attr = $hsd;
       (my $ilp =           <<'           EOHDOC') =~ s#^\s+##gm;
           <!-- this style block programmatically generated -->
       <style type="text/css">
           EOHDOC

       ($hsd  =             <<"           EOHDOC") =~ s#^\s+\<#<#gm;
      body  { $ila}
           </style>
           <!--  end programmatically generated style block -->
 
           EOHDOC
           $hsd = $ilp . $a_attr . $hsd;
	   $ila = qq[<body>];
       } else {
	   $ila = qq[<body style="$ila">];
       }

       return ($hsd,$ila);
  }


  sub findTheBody  {
       my $reass;
       my $neck;
       my $preserving = shift;
       my $lco = 0;
       my $dlc =  $curbuf->Count;
       my $lncount_bodystart = 0;
       my $seekTheEnd = sub {
	     my $nln = $lco;
	     my $lbuff = "";
	     while ( my $nld = $curbuf->Get($nln) )
	     {
		 chomp $nld;
		 $lbuff .= index($nld,q{>})? substr ($nld,0,index($nld,q{>}))
		                           : $nld;
		 if (index $nld,q{>})
		    { $lncount_bodystart = $nln; last }
	     }
		                                          $lbuff }; # end of sub

       while ($lco < $dlc)  {
	    my $cul = $curbuf->Get($lco);
	    $neck = $lco if $cul =~ m{ </(?i:head)> }x;
            if   (
	$cul =~ m{ <(?i:body\s) [^>]+ }x    ) {
		   my $psATR  =  $&;
		   my $comPL  =  $' eq q{>}? q{>} : $seekTheEnd->();
		   $lncount_bodystart ||= $lco;
		   $reass = $psATR . $comPL;
		   last
	    }
       } continue { $lco++ }

       VIM::Msg("Perl could not find a complete BODY string in the document buffer",
		 q|ErrorMsg|)                unless $reass;

       $preserving = $reass if $preserving;
       my @head_shoulders = hIppO($reass);
       if($preserving)  {
	   $preserving =~ s#  \<(?i:body\s+)  #<body #x;
	   $preserving =~ s#  \>\s*\z  ##x;
           $curbuf->Set($lncount_bodystart,
	              $preserving .                    (
		     $head_shoulders[1] # Is there any inline style data?
		      ?  substr ($head_shoulders[1], 5, length($head_shoulders[1]) - 6)
		                              . q{>}
					# Or was it all put in HEAD style block?
	              :                         q{}
		                                       )
		       ); # end "Set()"
       } else {
	    $curbuf->Set($lncount_bodystart, $head_shoulders[1])
	         if $head_shoulders[1] && $neck;
       }

       $curbuf->Append($neck - 1,
	    split("\n",$head_shoulders[0]))
	              if $head_shoulders[0] && $neck;
  } # end of subroutine
EOthePERLCODE

   funct! BodySnatch()
   let Preserve = ''
   perl <<EOthePERLCALL
      my $bline;
      unless ( eval { require Text::Balanced } )
         { VIM::Msg( q[Sorry, you must have "Text::Balanced" (a CPAN perl extention module)]
		    .q[ to run this plugin.], q|ErrorMsg|)
	 } else { import Text::Balanced "extract_delimited" }

      VIM::DoCommand(
    q{:let Preserve =} .
    q{inputdialog('Do you wish to preserve the original HTML <body> attributes?', 'no')}
                    );
      my $arg_pres = do { my($sucx,$ansval) = VIM::Eval( q{Preserve} );
                          $sucx && $ansval };
                                  
      if ($arg_pres and $arg_pres ne 'no')  {
	       $bline = &findTheBody(1)
      } else {
	       $bline = &findTheBody(0)  }
EOthePERLCALL
   endfu

else
   funct! BodySnatch()
      exec 'echo Not able to do anything without Perl!\<CR>'
   endfu

endif

nnoremap <silent><leader>))     :call BodySnatch()<cr><esc>
" eof
