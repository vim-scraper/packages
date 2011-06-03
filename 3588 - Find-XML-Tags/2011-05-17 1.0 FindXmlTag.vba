" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/FindXmlTag.vim	[[[1
8
if ( exists( "g:FindXmlTag_loaded" || &compatible || version < 700 ) )
  finish
endif

let g:FindXmlTag_loaded = 1

com! -nargs=+ Findxmltagexact call FindXmlTag#FindXmlTag( 1, <f-args> )
com! -nargs=+ Findxmltagflexible call FindXmlTag#FindXmlTag( 0, <f-args> )
autoload/FindXmlTag.vim	[[[1
74
" Test: cellpadding=0 cellpadding="0" cellpadding='0'
function! FindXmlTag#MakeAttributeSearchString( name, value )
  let result = '\<' . a:name . '\_s*=\_s*\%('

  let result .= '''' . a:value . ''''
  let result .= '\|'
  let result .= '"' . a:value . '"'
  let result .= '\|'
  let result .= a:value

  let result .= '\)'

  return result
endfunction

function! FindXmlTag#MakeTagSearchString( exact, tag, attributes )
  let result = '<' . a:tag

  if ( len( a:attributes ) > 0 )
    let numAttributes = 0

    let result .= '\%('
    let result .= '\_s\+'

    if ( !a:exact )
      let result .= '[^<>]\{-}'
    endif

    let result .= '\%('

    let i = 0

    while ( i < len( a:attributes ) )
      let name  = a:attributes[ i ]
      let value = a:attributes[ i + 1 ]

      if ( i > 0 )
        let result .= '\|'
      endif

      let result .= '\%('
      let result .= FindXmlTag#MakeAttributeSearchString( name, value )
      let result .= '\)'

      let numAttributes += 1

      let i += 2
    endwhile

    let result .= '\)'
    let result .= '\)'

    let result .= '\{' . numAttributes . '}'
  endif

  let result .= a:exact == 1 ? '\_s*' : '[^<>]\{-}'
  let result .= '/\?>'

  return result
endfunction

" Findxmltagexact table cellspacing \d\+ cellpadding \d\+ border \d\+
" Test: <table cellpadding=0 cellspacing="0" border='0'><tr><td></td></tr></table>
"
" This should match the strict one, also.
" Findxmltagflexible table cellspacing \d\+ cellpadding \d\+ border \d\+
" Test: <table class="test" cellpadding=0 cellspacing="0" style="padding: 5px; margin: 3px;" border='0'>
function! FindXmlTag#FindXmlTag( exact, tag, ... )
  let searchString = FindXmlTag#MakeTagSearchString( a:exact, a:tag, a:000 )

  let @/ = searchString

  call histadd( '/', searchString )
endfunction
doc/FindXmlTag.txt	[[[1
128
*FindXmlTag.txt*             For Vim Version 7.3      Last change: 2011 May 16

TABLE OF CONTENTS                                             *FindXmlTag_toc*

  1. Purpose                                              |FindXmlTag_Purpose|
  2. Usage                                                  |FindXmlTag_Usage|
    2.1. Commands                                        |FindXmlTag_Commands|
    2.2. Functions                                      |FindXmlTag_Functions|
  3. Examples                                            |FindXmlTag_Examples|
  4. History                                              |FindXmlTag_History|


==============================================================================
PURPOSE                                                   *FindXmlTag_Purpose*

I was editing an HTML file written by several developers over the course of a
few years. They contained tags like (yes, literally!) >
        <table cellpadding=0 cellspacing="0" border='0'>
        <table cellspacing="0" cellpadding=0 border='0'>
        <table cellspacing=0 cellpadding=0 border=0>
<
They're all basically the same table definition, but the attributes are
switched around and the attribute values are formatted differently each time.
(Thank the multiple developers, each with their own style.) Running HTML Tidy
wasn't an option because I didn't want to create a huge diff in version
control that indicated a lot of changes where I didn't actually change
anything. Thus, this plugin.

The commands and functions defined in this script will match any XML/HTML
element/tag with the specified attributes and values.


==============================================================================
USAGE                                                       *FindXmlTag_Usage*

For the usage examples, assume we have the same HTML table tags as defined in
|FindXmlTag_Purpose|.

  Commands                                               *FindXmlTag_Commands*

  Note that both the commands accept regular expressions instead of hard-code
  string literals for all parameters.

                                                             *Findxmltagexact*
  Command usage: >
        Findxmltagexact name_of_tag [name_of_attribute value_of_attribute]*
<
  This command creates a regular expression that will search for all
  permutations of the specified attributes and will match all elements
  containing PRECISELY those elements (and no more).

  To match all of the tags above, use >
        Findxmltagexact table cellpadding 0 cellspacing 0 border 0
<
  This will NOT match >
        <table cellspacing="0" cellpadding="0" border="0" style="padding: 5px;">
<
  because of the additional style attribute.

                                                          *Findxmltagflexible*
  Called in exactly the same way as |Findxmltagexact|, this results in an
  expression that will match all elements that contain AT LEAST the specified
  elements. Thus, this will match the example above with the style attribute.

  This is useful for the scenario where you want all table with a border of 0,
  no matter what other attributes they have: >
        Findxmltagflexible table border 0
<
  Note that flexible matching will also match elements that are considered
  fixed. For example, the above example will match ALL of the table tags from
  before.

  Functions                                             *FindXmlTag_Functions*

                                        *FindXmlTag#MakeAttributeSearchString*
  Given an attribute name and value (such as "border" and 0), returns a
  regular expression that will match all three versions of the attribute >
        border = 0
        border="6"
        border='3'
<
  Note how spaces (and newlines) around the equal sign are ignored.

                                              *FindXmlTag#MakeTagSearchString*
  Given an XML element (or HTML tag), with a set of name and value attributes,
  returns a regular expression that will match all combinations of the tag.
  The very first argument is whether or not the returned expression should be
  exact or flexible.

  Uses |FindXmlTag#MakeAttributeSearchString| to create expressions for
  attributes.

                                                       *FindXmlTag#FindXmlTag*
  Takes the same arguments as |FindXmlTag#MakeTagSearchString| but will set
  the search register (and history) with the result instead of returning it.
  This is the function called by both |Findxmltagexact| and
  |Findxmltagflexible|.


==============================================================================
EXAMPLES                                                 *FindXmlTag_Examples*

- To match any div that has an id attribute, irrespective of value, but no
  other attributes: >
        Findxmltagexact div id [^"']*
<
  This will match any of >
        <div id=test>
        <div id="someId"/>
        <div id='another id'>
<
  It is tempting to use ".*" or ".\{-}", but that would also match >
        <div id="abcd" class="someClass">
<
- To match any element with a class attribute, use >
        Findxmltagflexible \w\+ class [^"']*
<
- To find all tables with a border, use >
        Findxmltagexact table border \d\+
<

==============================================================================
HISTORY                                                   *FindXmlTag_History*

Version 1.0: Initial version.


 vim:tw=78:ts=8:ft=help:norl:sw=8:et
