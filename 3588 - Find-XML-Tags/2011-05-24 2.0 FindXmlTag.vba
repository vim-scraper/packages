" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/FindXmlTag.txt	[[[1
190
*FindXmlTag.txt*             For Vim Version 7.3      Last change: 2011 May 16

TABLE OF CONTENTS                                             *FindXmlTag_toc*

  1. Purpose                                              |FindXmlTag_Purpose|
  2. Usage                                                  |FindXmlTag_Usage|
    2.1. Commands                                        |FindXmlTag_Commands|
    2.2. Functions                                      |FindXmlTag_Functions|
    2.3. Options                                          |FindXmlTag_Options|
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

  ----------------------------------------------------------------------------
  Commands                                               *FindXmlTag_Commands*

  Note that both the commands accept regular expressions instead of hard-code
  string literals for all parameters. However, because of the way the final
  regular expression is compiled, it is possible that, if you craft a
  particularly clever regular expression, it may not work.

                                                             *FindXmlTagExact*
  Command usage: >
        FindXmlTagExact name_of_tag [name_of_attribute value_of_attribute]*
<
  This command creates a regular expression that will search for all
  permutations of the specified attributes and will match all elements
  containing PRECISELY those elements (and no more).

  To match all of the tags above, use >
        FindXmlTagExact table cellpadding 0 cellspacing 0 border 0
<
  This will NOT match >
        <table cellspacing="0" cellpadding="0" border="0" style="padding: 5px;">
<
  because of the additional style attribute.

                                                          *FindXmlTagFlexible*
  Called in exactly the same way as |FindXmlTagExact|, this results in an
  expression that will match all elements that contain AT LEAST the specified
  elements. Thus, this will match the example above with the style attribute.

  This is useful for the scenario where you want all table with a border of 0,
  no matter what other attributes they have: >
        FindXmlTagFlexible table border 0
<
  Note that flexible matching will also match elements that are considered
  fixed. For example, the above example will match ALL of the table tags from
  before.

  ----------------------------------------------------------------------------
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
  This is the function called by both |FindXmlTagExact| and
  |FindXmlTagFlexible|.

  ----------------------------------------------------------------------------
  Options                                                 *FindXmlTag_Options*

                                                         *g:FindXmlTag_anyValue*
  If you don't care about the value of an attribute, as long as it has one,
  specify this string as the value and the expression generator will craft a
  regular expression that will match all values; for example, >
        FindXmlTagExact td border `
<
  will find all td tags that have a border attribute with any value, including
  none. Specifically, it will match all of these: >
        <td border=1>
        <td border>
        <td border="5">
<
  Defaults to one back-tick (`).

                                                          *g:FindXmlTag_noValue*
  Older HTML pages will sometimes provide standalone attributes (such as
  "nowrap") with no value; use this as the "value" for such an attribute. For
  example, >
        FindXmlTagExact td nowrap ``
<
  will match >
        <td nowrap>
<
  but not >
        <td nowrap="1">
<
  Defaults to two back-ticks (``).


==============================================================================
EXAMPLES                                                 *FindXmlTag_Examples*

- To match any div that has an id attribute, irrespective of value, but no
  other attributes (assuming |g:FindXmlTag_anyValue| is still the default): >
        FindXmlTagExact div id `
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
        FindXmlTagFlexible \w\+ class [^"']*
<
- To find all tables with a border, use >
        FindXmlTagExact table border \d\+
<

==============================================================================
HISTORY                                                   *FindXmlTag_History*

Version 2.0: Bug fixes and enhancements, courtesy of Ingo Karkat, who not only
pointed them out, but also provided a patch to fix them:

- Renamed the commands to camel-case: FindXmlTagExact and FindXmlTagFlexible.
  Hopefully, this will make them easier for most people to distinguish between
  the commands (I'm lazy and don't like hitting the shift key, which is why I
  had them the way I did initially).

- Specifying no value for the last attribute now behaves the same as
  specifying the "any" value (see |g:FindXmlTag_anyValue|), making these two
  commands do the same thing: >
        FindXmlTagFlexible bean id `
        FindXmlTagFlexible bean id
<
- The regular expression had a bug where it would match both "<bean>" and
  "<beans>" even though it had been called with just "bean".

- The regular expression had a bug where flexible tags weren't found across
  multiple lines.

- If 'hlsearch' is set, makes sure the search results get highlighted (useful
  if |:nohlsearch| was used to temporarily turn the highlighting off).

Version 1.5: Added options to easily add attributes with any value or no
value.

Version 1.0: Initial version.


 vim:tw=78:ts=8:ft=help:norl:sw=8:et
plugin/FindXmlTag.vim	[[[1
16
if ( exists( "g:FindXmlTag_loaded" || &compatible || version < 700 ) )
  finish
endif

let g:FindXmlTag_loaded = 1

if ( !exists( "g:FindXmlTag_anyValue" ) )
  let g:FindXmlTag_anyValue = '`'
endif

if ( !exists( "g:FindXmlTag_noValue" ) )
  let g:FindXmlTag_noValue = '``'
endif

com! -nargs=+ FindXmlTagExact    call FindXmlTag#FindXmlTag( 1, <f-args> ) | if ( &hlsearch ) | set hlsearch | endif
com! -nargs=+ FindXmlTagFlexible call FindXmlTag#FindXmlTag( 0, <f-args> ) | if ( &hlsearch ) | set hlsearch | endif
autoload/FindXmlTag.vim	[[[1
92
" Test: cellpadding=0 cellpadding="0" cellpadding='0'
function! FindXmlTag#MakeAttributeSearchString( name, value )
  let result = '\<' . a:name

  " Only attach the value clause if the attribute HAS a value.
  if ( a:value != g:FindXmlTag_noValue )
    " If the default value is selected, replace with the default wildcard expression.
    let value = a:value == g:FindXmlTag_anyValue ? '[^''"]*' : a:value

    if ( a:value == g:FindXmlTag_anyValue )
      let result .= '\%('
    endif

    let result .= '\_s*=\_s*\%('

    let result .= '''' . value . ''''
    let result .= '\|'
    let result .= '"' . value . '"'
    let result .= '\|'
    let result .= value

    let result .= '\)'

    if ( a:value == g:FindXmlTag_anyValue )
      let result .= '\)\?'
    endif
  endif

  return result
endfunction

let s:anyAttribute = '\_[^<>]\{-}'

function! FindXmlTag#MakeTagSearchString( exact, tag, attributes )
  let result = '<' . a:tag

  if ( len( a:attributes ) > 0 )
    let numAttributes = 0

    let result .= '\%('
    let result .= '\_s\+'

    if ( !a:exact )
      let result .= s:anyAttribute
    endif

    let result .= '\%('

    let i = 0

    while ( i < len( a:attributes ) )
      let name  = a:attributes[ i ]
      let value = get( a:attributes, i + 1, g:FindXmlTag_anyValue )

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

  let result .= a:exact == 1 ? '\_s*' : len( a:attributes ) == 0 ? '\%(\_s\+' . s:anyAttribute . '\)\?' : s:anyAttribute
  let result .= '\/\?>'

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
