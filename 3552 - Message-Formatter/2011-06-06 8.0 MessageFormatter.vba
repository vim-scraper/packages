" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/MessageFormatter.txt	[[[1
688
*MessageFormatter.txt*       For Vim Version 7.3      Last change: 2011 May 06

TABLE OF CONTENTS                                       *MessageFormatter_toc*

  1. Purpose                                        |MessageFormatter_Purpose|
  2. Usage                                            |MessageFormatter_Usage|
  3. Dependencies                              |MessageFormatter_Dependencies|
  4. Options                                        |MessageFormatter_Options|
  5. Mappings                                      |MessageFormatter_Mappings|
  6. Commands                                      |MessageFormatter_Commands|
  7. Patterns                                      |MessageFormatter_Patterns|
  8. Pattern Modifiers                     |MessageFormatter_PatternModifiers|
  9. Notes                                            |MessageFormatter_Notes|
  10. Special Values                          |MessageFormatter_SpecialValues|
  11. Tips                                             |MessageFormatter_Tips|
  12. Examples                                     |MessageFormatter_Examples|


==============================================================================
PURPOSE                                             *MessageFormatter_Purpose*

This plugin allows the simplification of repetitive code fragments, allowing
much faster entry; for example, this text >
        getset  List<String>  names
<
gets converted to >
        public List<String> getNames()
        {
                return m_names;
        }

        public void setNames( List<String> val )
        {
                m_names = val;
        }
<
More importantly, it figures out when you're defining a boolean variable and
replaces the "get" with "is"; thus, >
        getset  boolean  old enough
<
becomes >
        public boolean isOldEnough()
        {
                return m_oldEnough;
        }

        public void setOldEnough( boolean val )
        {
                m_oldEnough = val;
        }
<
Observe how "old enough" was automatically camel-cased to "oldEnough", and,
when used with "get" or "is", was also capitalized. The same variable can be
used over and over again, each time with (if desired) different formatting
directives; for example, this template pattern >
        My name is {john::f_fName} and my full name is {{fName} smith::uq_fullName}.
<
gets expanded to >
        My name is John and my full name is "JOHN SMITH".
<
Observe, also, how the "fullName" directive recursively contained and expanded
the fName directive.


==============================================================================
USAGE                                                 *MessageFormatter_Usage*

For basic usage, see |MessageFormatter_Commands| and |MessageFormatter_Mappings|.


==============================================================================
DEPENDENCIES                                   *MessageFormatter_Dependencies*

This plugin depends on my GetVar.vim, which may be downloaded from
http://www.vim.org/scripts/script.php?script_id=353. This allows you to set
all the options on a per window, buffer, tab or global basis.


==============================================================================
OPTIONS                                             *MessageFormatter_Options*

                                           *g:MessageFormatter_blankParameter*

If a parameter value is intentionally blank, use this as the value (multiple
whitespace characters get treated as one delimiter, so simply putting extra
spaces instead won't do what you want).

Defaults to the back tick (`).

                                       *g:MessageFormatter_parameterSeparator*

Regular expression used to separate multiple arguments for parameter
expansion.

Defaults to two or more whitespace characters. You could change it to a single
tab, for example.

If no separators are found on the line, it will break the line on a single
space instead. This is useful if the arguments are simple and contain no
whitespace; for example, using the built-in getset: >
        getset boolean someBooleanVariable
<
At the same time, a more complex variation should use two spaces because it
contains spaces within: >
        getset  HashMap<String, String>  some map variable
<
(You can specify the last variable name as a sequence of words; they will
automatically be converted to camel case. Tweak the template definition and
replace the "c_" bit with "Cl_" to get snake case (as in the
|MessageFormatter_Tips| section).

                                               *g:MessageFormatter_jumpMarker*

Defaults to «». Useful if you have a jumping macro system installed (such as
the very good snippetsEmu.vim). The expression "!jump!" In template
definitions will be replaced with this, as will inline template variables so
you can jump from one to the next.

                                   *g:MessageFormatter_createDefaultTemplates*

Defaults to 1. Will install some sample global templates. Use
Listglobaltemplates to get a list of what's available.

                              *g:MessageFormatter_formatCurrentLineAsFallback*

Defaults to 1. If |<Plug>FormatCurrentTemplate| is called when the cursor
isn't in a template previously placed by |<Plug>PlaceTemplateInText|, the
processor will process the current line as an ad-hoc template instead (same as
|<Plug>FormatOneLine|); if this is set to 0 instead, the system will give an
error message.

                                         *g:MessageFormatter_autoAddJumpToEnd*

If set to 1 (the default), and a template contains !jump! Directives, another
one is added to the end of the directive to make it easier to jump to the end
to continue typing. For example, this template ends up with an extra !jump! At
the very end, >
        Addglobaltemplate do do\n{\n!jump!\n} while ( !jump! );
<
while this one does not: >
        Addglobaltemplate safetern ( {::var} == null ? "" : {var} )
<
The mechanism is smart enough to only tweak those templates that don't already
contain a !jump! directive at the very end, so you don't have to tweak legacy
templates. If you only ever want the functionality to be available
selectively, set |g:MessageFormatter_autoAddJumpToEnd| to 0 and add it
manually to those templates where you want it.

                                      *g:MessageFormatter_highlightDirectives*

If 1 (defaults to 1), directives of one of these formats are highlighted: >
        {::modifiers_directiveName}
        {::directiveName}
        {«»::directiveName}
        {«»::modifiers_directiveName}
<
Where «» is whatever |g:MessageFormatter_jumpMarker| is set to.

                                  *g:MessageFormatter_highlightDirectivesLink*

Defaults to "Error". The syntax item to which the directives are linked for
highlighting.

                          *g:MessageFormatter_highlightDirectiveModifiersLink*

Defaults to "Constant". The syntax item to which the directive modifiers are
linked for highlighting.


                                     *g:MessageFormatter_moveArgumentsToStart*

If 1 (the default), then all input arguments are moved to the beginning of the
expression when using |<Plug>PlaceTemplateInText|. For example, this template >
        Addlocaltemplate + let {::c_var} += {def 1::increment}
<
will expand to >
        {|::n_var}{«»::n_override-increment}let {c_var} += {increment}{eval {p_override-increment} == '' ? {1::p_default-increment} : {p_override-increment}::n_increment}
<
placing both "var" and the override for the default value for "increment" at
the very beginning of the template, making it easier to enter the values. As
before, the default parameters come after the non-default ones and are
characterized by their names being prefixed with the word "override" (the
actual default value can be seen later as it is prefixed with the word
"default").

For the sake of comparison, if the option is 0, this is what the template
expands to: >
        let {|::c_var} += {increment}{eval {«»::p_override-increment} == '' ? {1::p_default-increment} : {p_override-increment}::n_increment}
<
The end result is exactly the same, but the parameters are now in the middle
of the expression as entered when defining the template instead of at the
beignning.

In both cases, | marks the location of the cursor.

Note that this option has no bearing when using |<Plug>PlaceTemplateForLine|
because the expansion there isn't done inline, anyway.


==============================================================================
MAPPINGS                                           *MessageFormatter_Mappings*

                                                 *<Plug>FormatCurrentTemplate*

This is both a normal mode and an insert mode mapping; it expands the last
inline template placed by |<Plug>PlaceTemplateInText|, as long as the cursor is
within the range of lines placed by the mapping.

If invoked from insert mode, the cursor is moved to the very end of the last
line in the template and left in insert mode. From normal mode, the cursor
remains where it was, leaving you in normal mode. (I almost always type the
last value and hit the hotkey straight from insert mode and then continue
typing.)

Default mapping: <c-del> (both insert and normal mode)

                                                   *<Plug>PlaceTemplateInText*

Given a template name as the WORD (including special characters) on the cusor,
replaces it with the expansion; if the expansion isn't found, it's replaced
with itself, wrapped in exclamation marks, so "adsfasdfadsf" becomes
"!adsfasdfadsf!". You can then hit undo do get to the original and spell it
correctly, if that's what happened.

Default mapping: <Leader><Leader> (insert mode only)

                                                  *<Plug>PlaceTemplateForLine*

Given a line containing nothing but a template name and parameter values, will
replace the line with the fully expanded template. The arguments should be
separated by |g:MessageFormatter_parameterSeparator| or, if the arguments are
single words (themselves contain no whitespace) they may be separated by a
single space to ease data entry. If a truly blank value is desired for any of
the parameters, use |g:MessageFormatter_blankParameter| as the value.

This is probably the most commonly used mapping of the entire plugin, after
templates have been defined, though |<Plug>PlaceTemplateInText| definitely has
its place where inline templates are desired, as this mapping commandeers the
entire line.

Default mapping: `` (two back-ticks; insert mode only)

                                                         *<Plug>FormatOneLine*

Expands the current cursor line as if it were a template, even if it was laid
down manually. For example, if you have >
        public String {my variable::c_var} = {q_var};
<
and you hit the hotkey, you get >
        public String myVariable = "my variable";
<
Default mapping: <c-del><c-del> (normal mode only)

                                                     *<Plug>FormatVisualRange*

The big cousin of |<Plug>FormatOneLine|; does the same thing, except for a
visually selected range of lines. Useful for ad-hoc templates comprising
multiple lines where the variable may appear on multiple lines. For example, >
        /**
         * Constant defining for {my variable::c_var}.
         */
        public static String {C_var} = {wq_var};
<
gives >
        /**
         * Constant defining for myVariable.
         */
        public static String MY_VARIABLE = "my variable";
<

Default mapping: <c-del> (visual mode only)

                                                  *<Plug>FormatOpModeTemplate*

Same thing as |<Plug>FormatVisualRange|, except it works in operator-pending
mode. In the above example, if the cursor were on the first line ("/**"), you
could hit the hotkey for <Plug>FormatOpModeTemplate followed by 3j to achieve
the same effect.

Default mapping: <c-s-del> (normal mode only)

                                 *<Plug>MessageFormatter_InsertModeCompletion*

Invokes user-defined insert mode custom completion (|compl-function|) and
completes template names; the popup menu contains the template definitions for
quick reference, also.

Default mapping: // (insert mode only)


Note:   |<Plug>PlaceTemplateInText| looks only at the current word so is useful
        for templates that are meant to be used as part of a whole line (look
        at "safetern" in the examples), although it can be used for multi-line
        templates, also. Conversely, |<Plug>PlaceTemplateForLine| looks at the
        entire line because it expects all the parameter values to be provided
        up front.


==============================================================================
COMMANDS                                           *MessageFormatter_Commands*

                                                           *Addglobaltemplate*

Adds a global template that's available everywhere; for example >
        Addglobaltemplate const /**\n* Constant defining for {::c_var}.\n*/\npublic static String {C_var} = {wq_var};
<
The template is invoked by typing "const" and hitting the hotkey for inline
expansion (|<Plug>PlaceTemplateInText|) or by typing "const" followed by
arguments and hitting the hotkey for automatic expansion
(|<Plug>PlaceTemplateForLine|).

                                                            *Addlocaltemplate*

Same thing as |Addglobaltemplate|, except it uses buffer variables to store the
template so it's only available for this buffer.

Useful for ftplugin settings.
                                                         *Listglobaltemplates*

Simple list of global template name.

                                                          *Listlocaltemplates*

Simple list of local template name.

                                                           *FormatVisualRange*

Expands a range of text lines in the buffer containing default values for the
parameters; best called from mappings rather than directly, though you can
select a range of lines and call it on that (|<Plug>FormatVisualRange| does just
that, in fact).

                                                          *Setcolordirectives*

Takes one argument (1 or 0); if 1, enables the highlighting of directives (see
g:MessageFormatter_highlightDirectives|) in text when placed through
|<Plug>PlaceTemplateInText| or manually for one-time expansion.


==============================================================================
PATTERNS                                           *MessageFormatter_Patterns*

                                                   *MessageFormatter_example1*

The examples (loaded unless |g:MessageFormatter_createDefaultTemplates| is set
to 0 in your vimrc) define some fairly complex templates. Dissecting one: >
        Addglobaltemplate var {::type} {::c_var} = new {eval {p_type} =~# '^List' ? 'Array{type}' : {p_type} =~# '^Map' ? 'Hash{type}' : {p_type}::instanceType}();
<
Template name:       var
Number of arguments: 2 ("type" and "var"); defined by the {::...} format

Template particulars:

{::type}:
        get value for "type" and use as is.
{::c_var}:
        get value for "var" and camel-case it.
{eval {p_type} =~# '^List' ? 'Array{type}' : {p_type} =~# '^Map' ? 'Hash{type}' : {p_type}::instanceType}:
        variable "instanceType" is given an "eval" value,

This gets evaluated as a ternary expression:
        {p_type} =~# '^List' ? 'Array{type}' : {p_type} =~# '^Map' ? 'Hash{type}' : {p_type}

        If the value, wrapped in single quotes (p_type; suitable for use in a
        Vim expression), starts with List, instantiate it as an ArrayList; if
        it starts with Map, instantiate it has a HashMap; otherwise, use as
        is. (List and Map are interfaces in Java and can't be instantiated
        directly).

This allows you to to type >
        var List<String> names
<

And hit the hotkey for |<Plug>PlaceTemplateForLine| (see the mapping for more
details) in insert mode to get >
        List<String> names = new ArrayList<String>();
<
                                                   *MessageFormatter_example2*

Another one: >
        Addglobaltemplate do do\n{\n!jump!\n} while ( !jump! );
<
Type "do" and hit the mapping for |<Plug>PlaceTemplateInText| and it will
replace the word with the expansion in place (can be used with "var", also),
moving the cursor to the first !jump! And replacing the other !jump! with jump
characters.

If the |<Plug>PlaceTemplateInText| mapping is used for expansions such as "var"
above, the cursor goes to the first {::...} variable and all others change to
{!jump!::...} so a jump hotkey (assuming there is a jumper plugin installed)
will move from one to the next. Once the values have been entered, hit the
hotkey for |<Plug>FormatCurrentTemplate| (insert or normal mode) and the
template will be expanded inline. The plugin remembers the last template
placed, so if a second template is placed before the first is expanded, the
first will be forgotten.


==============================================================================
PATTERN MODIFIERS                          *MessageFormatter_PatternModifiers*

Modifiers are supplied to prior to the variable name and are of the format >
        {modifiers_variableName}
        {::modifiers_variableName}
        {default value::modifiers_variableName}
<

Multiple modifiers are permitted and are processed in the order entered, so
"tl" will first title case and then lower-case (resulting in all lower-case)
while "lt" will do it the other way around.

    Modifier  Meaning
    --------  -----------------------------------------------------------
       a      repeats value as is (optional)
       c      camelCase
       C      CONSTANT_CASE
       e      escapes quotes and backslashes
       f      Capitalizes the first letter
       l      converts the entire thing to lower-case
       m      followed by a number, repeats string n times (default 1)
       n      suppresses output; useful for ensuring variable entry order
       o      if the argument is non-empty, prepends a comma and a space
       p      wraps argument in apostrophes, escaping inner apostrophes
       P      Same as "p", except that it doesn't modify empty values
       q      wraps argument in double quotes, escaping it as for "e"
       Q      Same as "q", except that it doesn't modify empty values
       s      if the argument is non-empty, wraps it in spaces
       t      Title Cases The Entire Argument
       u      UPPER CASES THE ARGUMENT
       w      changes "helloThere" or "HELLO_THERE" to "hello there"


                                    *MessageFormatter_patternModifierExamples*

Here are some examples:

                  Argument             Result
                  -------------------  --------------------
                  {john smith::c_var}  johnSmith
                  {john smith::C_var}  JOHN_SMITH
                  {john smith::t_var}  John Smith
                  {!@::m10_var}        !@!@!@!@!@!@!@!@!@!@
                  {++::m5q_var}        "++++++++++"


==============================================================================
NOTES                                                 *MessageFormatter_Notes*

- Patterns replace all \n instances (backslashes followed by 'n') with
  newlines and all !jump! occurrences with the value of
  MessageFormatter_jumpMarker.

                                                         *MessageFormatter_CR*

- Patterns replace all instances of "<CR>" (case-sensitive and without the
  quotes) with newlines AT THE VERY END OF PROCESSING. This allows "eval"
  expressions to result in multiple lines. Note that the indentation may break
  here because this uses 'paste' to make sure lines are placed in as they are
  found.

                                                         *MessageFormatter_SW*

- Similarly, patterns replace all instances of "<SW>" with 'shiftwidth'
  spaces; this can be used to reset indentation in case the use of "<CR>"
  above breaks indentation.

- Template definitions are searched locally and then globally, thus a local
  template with the same name will always be used instead of the global one.


==============================================================================
SPECIAL VALUES                                *MessageFormatter_SpecialValues*

If the value for a particular parameter begins with one of these, it gets
treated differently:

                                                       *MessageFormatter_eval*

- "eval ": Whatever follows is executed as a Vim expression and the return
  value from that is used as the actual value. For example, >
        {eval strpart( {::p_name}, 0, 3 )::u_shortName}
<
  will produce, assuming "name" is "John" (the "u" convets it to upper-case): >
        JOH
<
                                                        *MessageFormatter_ask*

- "ask": Puts up an input prompt, asking for the value of the the parameter
  interactively; if something follows the "ask", it is used as the default
  value for the input. For example, >
        {ask {::lName}, {::fName}::t_fullName}
<
  prompts for the value, of the full name, using "lName, fName" as the
  default. (The "t" converts it to title-case, allowing inputs such as
  "smith" and "john".)

Both forms allow the embedding of further parameters within recursively; you
could even put further "ask" prompts within (for lName and fName above, for
example).

                                                        *MessageFormatter_def*

- "def ": The remainder of the value is treated as a default value; that is,
  if the user passes in nothing for the particular parameter (or, with
  |<Plug>PlaceTemplateForLine|, using |g:MessageFormatter_blankParameter|),
  this value is used instead. For example, >
        Addlocaltemplate for {{def i::c_var}::n_cVar}for ( int {cVar} = {def 0::start}; {cVar} < {::c_limit}; {cVar}++ )\n{\n«»\n}«»
<
  This is a simplistic example, but it default to i and 0, respectively, for
  "var" and "start" if nothing is passed in for those variables.

  The system supports recursive use of default values; so, this will work: >
        Addlocaltemplate test {def {def john::t_fName} {def smith::t_lName}::t_fullName}
<
  Note that this example accepts up to three arguments (fName, lName and
  fullName); setting them to the empty value
  (|g:MessageFormatter_blankParameter|) is the same as using the default.

  Default values aren't required; for example, from before: >
        Addlocaltemplate for {{def i::c_var}::n_cVar}for ( int {cVar} = {def 0::start}; {cVar} < {::c_limit}; {cVar}++ )\n{\n«»\n}«»
<
  This template may be expanded by simply >
        for 20
<
  as only the "limit" variable is required.

  Note that the order in which the variables are defined in the template is
  considered a relative order; in reality, all the supplied arguments are
  applied to the non-default parameters before they are applied to the default
  parameters. In this example, if all the values are to be provided, then it
  has to look like this: >
        for  20  counter  3
<
  This might seem counterintuitive as the required value is, in fact, provided
  last.

                                                   *MessageFormatter_emptyDef*

An empty default value should be specified as >
        {def ::someVar}
<
Note the space between the "def" and the "::"; simply using >
        {def::someVar}
<
sets the value of "someVar" as "def".

                                                        *MessageFormatter_tem*

  - "tem ": The remainder of the value is taken to be the name of another
    template and is replaced with the expansion of that. For example, given
    these three templates: >
        Addlocaltemplate c {::c_arg}
        Addlocaltemplate C {::C_arg}
        Addlocaltemplate cf {::cf_arg}
<
    We can now define this template: >
        Addlocaltemplate a {tem c::u_a} {tem C::a}
<
    Note that the variable modifiers and name aren't used (the "u_a" and "a"
    don't matter, though they are required--you may make them all the same).

    Recursion is supported, so another template that uses "a" may be defined: >
        Addlocaltemplate b {tem cf::a} {tem a::a}
<
    Now, calling "b": >
        b  this is neat
<
    results in >
        ThisIsNeat thisIsNeat THIS_IS_NEAT
<
    Note also that inner templates that use the same variable name ("arg") get
    consolidated so it needs to be specified only once (ending up with only
    one required argument, not three); thus, the inline expansion of "b"
    (using |<Plug>PlaceTemplateInText|) gives >
        {|::n_arg}{cf_arg} {c_arg} {C_arg}«»
<
    (You end up in insert mode, with the cursor location indicated by the |.)

                                                        *MessageFormatter_iab*

  - "iab ": Expands the specified insert-mode abbreviation (see |:iabbrev|);
    for example, if the following abbreviation has been defined: >
        iab js John Smith
<
    then, this snippet: >
        {iab js::name}
<
    gets expanded to: >
        {John Smith::name}
<
    This may be recursive, so the following works (the inner abbreviation
    expands to "js" which is recursively expanded to "John Smith"): >
        iab test js
        {iab {iab test::inner}::name}
<

==============================================================================
TIPS                                                   *MessageFormatter_Tips*

- To get snake_case_variable_name type variables while starting with "snake
  case variable name", use "Cl" as the modifier (convert to
  A_CONSTANT_TYPE_VARIABLE and then lower-case the whole thing). >
        {::Cl_var}
<

- Similarly, to get ClassName type variables from "class name", use "cf" as
  the modifier (camel case to get "className" and then capitalize the first
  letter). >
        {::cf_var}
<

                                    *MessageFormatter_parameterExpansionOrder*

- Templates with default variables apply parameters in this order:

  - First, all non-optional (non-default value-supplied) variables are
    fulfilled in the order they are seen.

  - Any remaining parameters are then applied to the default varaibles, also
    in the order they are seen.

  To illustrate with an example, take this template: >
        Addlocaltemplate test First name: {def john::t_fName}, last name: {::t_lName}, middle initial: {::u_middleInitial} and age: {def 25::age}.
<
  The arguments for this are provided as: >
        test lName middleInitial fName age
<
  The two required arguments come first and the two optional arguments come
  second. This cannot be changed. However, if the order of the two required
  arguments needs to be reversed (maybe the middle initials make sense before
  the last name), redefine the template as such: >
        Addlocaltemplate test {::n_middleInitial}{::n_lName}First name: {def john::t_fName}, last name: {t_lName}, middle initial: {u_middleInitial} and age: {def 25::age}.
<
  The "n" modifier causes the value to be defined, but not displayed. The same
  trick can be used for the default parameters to change their order as
  desired, but again, the required parameters will be fulfilled first
  irrespective of how they are defined in the template.

  With or without default variables, recursive parameters are expanded with
  the most deeply nested one first to allow outer ones to use the value
  specified; for example: >
        {ask {ask::fName} {ask::lName}::fullName} (first name {fName}, last name {lName}
<
  The above asks for "fName" first, then "lName" and, finally, for "fullName",
  using the values for the first two as the default for the last.


==============================================================================
EXAMPLES                                           *MessageFormatter_Examples*

Here are some 'filetype' specific templates that might be useful (for placing
in the appropriate |ftplugin| files).

  - Java for loop (only one required parameter: the end value) >
        Addlocaltemplate for {{def i::c_var}::n_cVar}for ( int {cVar} = {def 0::start}; {cVar} {def <::compare} {::c_limit}; {cVar}++ )\n{\n!jump!\n}!jump!
<
  - JavaScript function definition (requires only the function name, which
    gets camel-cased automatically) >
        Addlocaltemplate fun function {::c_functionName} ({def ::s_args})\n{\n«»\n}
<
  - Vim plugin default mapping specification (requires the mapping expression
    and the default mapping; optionally takes the mode, defaulting that to
    "n") >
        Addlocaltemplate mapping if ( !hasmapto( '<Plug>{::mapExpression}', '{def n::mode}' ) )\n{mode}map <silent> {::defaultMapping} <Plug>{mapExpression}\nendif
<
  - Vim plugin default option specification (takes the variable name and
    value) >
        Addlocaltemplate option if ( !exists( "g:{::var}" ) )\nlet g:{var} = {::value}\nendif
<
  - Vim expression to add a specified value to a variable (takes the variable
    name, camcel casing it automatically and an optional increment) >
        Addlocaltemplate + let {::c_var} += {def 1::increment}
<
  - Vim expression to make variable assignment (slightly) easier (for those
    not used to typing "let" first); camel-cases only the left half because
    the right might be an expression: >
        Addlocaltemplate = let {::c_a} = {::b}
<
  - Vim version of a for loop (takes the name of the variable and the terminal
    value): >
        Addlocaltemplate for let {::c_var} = 0\n\nwhile ( {c_var} < {::terminalValue} )\n«»\n\nlet {c_var} += 1\nendwhile
<
  - XHTML input tag; takes the name of the input component, optionally taking
    a type and value (defaults to "text" and no value, respectively); if no
    value is provided, the value attribute is suppressed altogether: >
        Addlocaltemplate input <input type={def text::q_type} id={::q_id} name={q_id} {def ::n_value}{eval {p_value} == '' ? '' : {value="{e_value}"::p_valueAttribute}::val}/>
<

 vim:tw=78:ts=8:ft=help:norl:sw=8:et
plugin/MessageFormatter.vim	[[[1
670
if ( exists( "g:MessageFormatter_loaded" || &compatible || version < 703 ) )
  finish
endif

let g:MessageFormatter_loaded = 1


if ( !exists( "g:MessageFormatter_blankParameter" ) )
  let g:MessageFormatter_blankParameter = '`'
endif

if ( !exists( "g:MessageFormatter_parameterSeparator" ) )
  let g:MessageFormatter_parameterSeparator = '\s\{2,}'
endif

if ( !exists( "g:MessageFormatter_jumpMarker" ) )
  let g:MessageFormatter_jumpMarker = '«»'
endif

if ( !exists( "g:MessageFormatter_formatCurrentLineAsFallback" ) )
  let g:MessageFormatter_formatCurrentLineAsFallback = 1
endif

if ( !exists( "g:MessageFormatter_autoAddJumpToEnd" ) )
  let g:MessageFormatter_autoAddJumpToEnd = 1
endif

if ( !exists( "g:MessageFormatter_highlightDirectives" ) )
  let g:MessageFormatter_highlightDirectives = 1
endif

if ( !exists( "g:MessageFormatter_highlightDirectivesLink" ) )
  let g:MessageFormatter_highlightDirectivesLink = 'Error'
endif

if ( !exists( "g:MessageFormatter_moveArgumentsToStart" ) )
  let g:MessageFormatter_moveArgumentsToStart = 1
endif

if ( !exists( "g:MessageFormatter_highlightDirectiveModifiersLink" ) )
  let g:MessageFormatter_highlightDirectiveModifiersLink = 'Constant'
endif

function! FormatMessage( message, recursive )
  if ( !exists( "g:MessageFormatter_parameters" ) )
    return a:message
  endif

  return MessageFormatter#FormatMessage( a:message, g:MessageFormatter_parameters, a:recursive )
endfunction

function! AddFormatParameters( ... )
  call GetVar#Allocate( "g:MessageFormatter_parameters", [] )

  " It's a List, so just append the parameters.
  if ( type( g:MessageFormatter_parameters ) == 3 )
    let g:MessageFormatter_parameters += a:000
  else
    let index = 0

    " Dictionary
    for parameter in a:000
      " Locate the next unused numerical key value.
      while ( has_key( g:MessageFormatter_parameters, index ) )
        let index += 1
      endwhile

      let g:MessageFormatter_parameters[ index ] = parameter

      let index += 1
    endfor
  endif
endfunction

function! AddDictionaryFormatParameter( arg )
  let [ dummy, key, value; rest ] = matchlist( a:arg, '^\(\S\+\)\s\+\(.*\)$' )

  call GetVar#Allocate( "g:MessageFormatter_parameters", {} )

  " List; can't add.
  if ( type( g:MessageFormatter_parameters ) == 3 )
    echoerr "The parameters are contained a List, not a Dictionary. To just add the parameter as is, use Addformatparameter instead."

    echo "The parameter list: " . string( g:MessageFormatter_parameters )

    return
  endif

  let g:MessageFormatter_parameters[ key ] = value
endfunction

function! IncrementCurrentPointer()
  let s:MessageFormatter_currentPointer += 1
endfunction

function! ExtractVariableName()
  let s:variableProcessedCorrectly = 0

  let valueFinished     = 0
  let modifiersFinished = 0
  let value             = ''
  let modifiers         = ''
  let variableName      = ''

  while ( s:MessageFormatter_currentPointer < len( s:MessageFormatter_text ) )
    let char = s:MessageFormatter_text[ s:MessageFormatter_currentPointer ]

    call IncrementCurrentPointer()

    if ( char == '\' )
      " Escaped character; put it in as is (still escaped).
      let value .= char . s:MessageFormatter_text[ s:MessageFormatter_currentPointer ]

      call IncrementCurrentPointer()
    elseif ( char == '{' )
      " Process inner variable recursively.
      let value .= '{' . ExtractVariableName() . ( s:variableProcessedCorrectly ? '}' : '' )

      let s:variableProcessedCorrectly = 0
    elseif ( char == ':' )
      " Value/variable name separator (::).
      if ( s:MessageFormatter_text[ s:MessageFormatter_currentPointer ] == ':' )
        let valueFinished = 1

        call IncrementCurrentPointer()
      else
        let value .= char
      endif
    elseif ( char == '}' )
      let s:variableProcessedCorrectly = 1

      break
    elseif ( valueFinished )
      if ( modifiersFinished )
        let variableName .= char
      elseif ( char == '_' )
        let modifiersFinished = 1
      else
        let modifiers .= char
      endif
    else
      let value .= char
    endif
  endwhile

  if ( !valueFinished )
    let [ dummy, modifiers, variableName; remainder ] = matchlist( value, '\([_]*\)_\=\(.*\)' )
    let value                                         = ''
  elseif ( !modifiersFinished )
    let variableName = modifiers
    let modifiers = ''
  endif

  " If they passed in an empty value on purpose, we should honour it.
  if ( value != '' || valueFinished )
    let s:MessageFormatter_parameters[ variableName ] = value
  endif

  return ( modifiers == '' ? '' : modifiers . '_' ) . variableName
endfunction

function! FormatContainedMessage( text, ... )
  let s:MessageFormatter_text           = a:text
  let s:MessageFormatter_parameters     = exists( "a:2" ) && type( a:2 ) == 4 ? a:2 : {}
  let s:MessageFormatter_currentPointer = 0

  let messageToFormat = ExtractVariableName()

  return MessageFormatter#FormatMessage( messageToFormat, s:MessageFormatter_parameters, 1, exists( "a:1" ) && a:1 == 1 )
endfunction

function! PlaceTemplateInText()
  " Break the undo chain so hitting undo gives the user back the word they had typed to launch this mapping.
  execute "normal! i\<c-g>u"

  let saveZ = @z

  normal "zciW*

  let templateName = @z

  let @z = saveZ

  let result = GetTemplateDefinition( templateName )

  let result = ExpandDirectiveValues( result, 'tem' )
  let result = ExpandDirectiveValues( result )
  let result = ConsolidateDuplicateDirectives( result )

  if ( GetVar#GetVar( 'MessageFormatter_moveArgumentsToStart' ) == 1 )
    let args = ''

    while ( result =~ s:inputDirectiveExpression )
      let args .= substitute( result, '.\{-}' . s:inputDirectiveExpression . '.*', '{::n_\2}', '' )
      let result = substitute( result, s:inputDirectiveExpression, '{\1\2}', '' )
    endwhile

    let result = args . result
  endif

  " Make the first one the cursor location.
  " let result = substitute( result, '{::', '{|::', '' )
  " Make the rest placeholders for jumping.
  let result = substitute( result, '{::', '{!jump!::', 'g' )
  " Convert newlines
  let result = substitute( result, '\\n', "\n", 'g' )

  " If the expansion contains jump directives.
  if ( GetVar#GetVar( "MessageFormatter_autoAddJumpToEnd" ) == 1 && result =~# '!jump!' && result !~# '!jump!$' )
    let result .= '!jump!'
  endif

  " Convert jump directives
  let result = substitute( result, '!jump!', GetVar#GetVar( "MessageFormatter_jumpMarker" ), 'g' )

  silent! undojoin

  let savedFo = &fo
  set fo=
  execute 'normal! "_s' . result . "\<esc>"
  let &fo = savedFo

  " Store for automatic expansion later.
  let b:MessageFormatter_snippetStart = line( "'[" )
  let b:MessageFormatter_snippetEnd   = line( "']" )

  call MessageFormatter#EditFirstJumpLocation( b:MessageFormatter_snippetStart, b:MessageFormatter_snippetEnd )
endfunction

function! GetTemplateDefinition( templateName )
  " First look for a local key and then for a global key.
  let result = ''
  let root   = 'b'
  let hasKey = exists( root . ':MessageFormatter_templates' ) && has_key( {root}:MessageFormatter_templates, a:templateName )

  if ( !hasKey )
    let root   = 'g'
    let hasKey = exists( root . ':MessageFormatter_templates' ) && has_key( {root}:MessageFormatter_templates, a:templateName )
  endif

  if ( hasKey )
    let result = {root}:MessageFormatter_templates[ a:templateName ]
  else
    let result = '!' . a:templateName . '!'
  endif

  return result
endfunction

let s:openBrace  = '_OPEN_BRACE_'
let s:closeBrace = '_CLOSE_BRACE_'

" echo ExtractInnermostDirective( "public static final {::type} {::C_var} = {def {var}::n_value}{eval '{type}' == 'String' ? {qp_value} : {p_value}::parsedValue};" )
" echo ExtractInnermostDirective( "public static final {::type} {::C_var} = {sub {p_var}, '[aeiou]', '1', 'g'::n_value}{eval '{type}' == 'String' ? {qp_value} : {p_value}::parsedValue};" )
function! ExtractInnermostDirective( line, ... )
  " let directiveExpression  = '\%(sub\|def\) '
  let directiveExpression = '\%(' . ( exists( "a:1" ) ? a:1 : 'def ' ) . '\) '
  let directiveLength     = 4
  let lineLength          = strlen( a:line )
  let directiveDetails    = {}

  let directiveDetails.directive    = ''
  let directiveDetails.startIndex   = -1
  let directiveDetails.endIndex     = -1
  let directiveDetails.defaultValue = ''
  let directiveDetails.modifiers    = ''
  let directiveDetails.variable     = ''

  let value = ''

  let parsingDefault = 0

  let parseDepth     = 0
  let currentPointer = 0

  while ( currentPointer < lineLength )
    let char = a:line[ currentPointer ]

    let currentPointer += 1

    if ( char == '\' )
      " Escaped character; put it in as is (still escaped).
      if ( parsingDefault == 1 )
        let value .= char . a:line[ currentPointer ]
      endif

      let currentPointer += 1
    elseif ( char == '{' )
      let newDirective = 0

      " Might be the start of a default block; the next four characters have to be matched to see if they are "def ". An actual directive would have to be much longer
      " because it has to be at least: {def a::a} (10 characters).
      if ( currentPointer < lineLength - directiveLength )
        let testCharacters = a:line[ currentPointer : currentPointer + directiveLength - 1 ]

        if ( testCharacters =~# directiveExpression )
          let directiveDetails.startIndex = currentPointer - 1
          let directiveDetails.directive  = testCharacters
          let currentPointer             += directiveLength
          let parsingDefault              = 1
          let value                       = ''
          let newDirective                = 1
        endif
      endif

      " Just another directive; if we're inside another default expression, continue collecting characters.
      if ( newDirective == 0 && parsingDefault == 1 )
        let value      .= char
        let parseDepth += 1
      endif
    elseif ( char == '}' )
      if ( parsingDefault == 1 )
        " End of default
        if ( parseDepth == 0 )
          let parsingDefault            = 0
          let directiveDetails.endIndex = currentPointer - 1

          let [ expression, directiveDetails.defaultValue, directiveDetails.modifiers, directiveDetails.variable; remainder ] = matchlist( value, '\(.*\)::\%(\([^_]*\)_\)\=\(.*\)' )

          break
        else
          let value .= char
        endif

        let parseDepth -= 1
      endif
    elseif ( parsingDefault == 1 )
      let value .= char
    endif
  endwhile

  return directiveDetails
endfunction

" Recursive def should work now; for example:
" echo ExpandDirectiveValues( "public static final {::type} {::C_var} = {def {var}::n_value}{eval '{type}' == 'String' ? {qp_value} : {p_value}::parsedValue};" )
function! ExpandDirectiveValues( line, ... )
  let result = a:line

  let blank     = GetVar#GetVar( "MessageFormatter_blankParameter" )
  let directive = exists( "a:1" ) ? a:1 : 'def'

  let prefixDefaults         = ''
  let s:numOptionalArguments = 0
  let directiveDetails       = ExtractInnermostDirective( result, directive )

  while ( directiveDetails.endIndex != -1 )
    if ( directiveDetails.directive ==# 'def ' )
      if ( directiveDetails.defaultValue == '' )
        let directiveDetails.defaultValue = blank
      endif

      " Capitalize first letter.
      " let variableName = substitute( directiveDetails.variable, '^.', '\u&', '\1' )
      let variableName = directiveDetails.variable

      let prefixDefaults .= "{eval {::p_override-" . variableName . "} == '' ? {" . directiveDetails.defaultValue . "::p_default-" . variableName . "} : {p_override-" . variableName . "}::n_" . directiveDetails.variable . "}"

      let result = result[ 0 : directiveDetails.startIndex ] . ( directiveDetails.modifiers == '' ? '' : directiveDetails.modifiers . '_' ) . directiveDetails.variable . result[ directiveDetails.endIndex : ]
    elseif ( directiveDetails.directive ==# 'tem ' )
      " If the match is at index 0, directiveDetails.startIndex - 1 becomes -1, which means the entire string, which isn't what we want (we want the empty string).
      let tempResult = directiveDetails.startIndex > 0 ? result[ 0 : directiveDetails.startIndex - 1 ] : ''
      let result     = tempResult . GetTemplateDefinition( directiveDetails.defaultValue ) . result[ ( directiveDetails.endIndex + 1 ) : ]
    endif

    let directiveDetails        = ExtractInnermostDirective( result, directive )
    let s:numOptionalArguments += 1
  endwhile

  " Non-default parameters first; this way, when we apply arguments, the non-default ones get filled first, leaving a shortfall to fall to the default values.
  let result = result . prefixDefaults

  return result
endfunction

let s:inputDirectiveExpression = '{::\([^_}]*_\)\=\([^}]*\)}'

function! ConsolidateDuplicateDirectives( line )
  " For each input directive, add the variable name to a dictionary. If it is already on the dictionary, remove the :: bit from it.
  let existingVariables = {}
  let result            = ''
  let startIndex        = 0
  let matchIndex        = match( a:line, s:inputDirectiveExpression, startIndex )

  while( matchIndex >= 0 )
    let result .= a:line[ startIndex : matchIndex ]
    let [ original, modifier, variable; remainder ] = matchlist( a:line, '.\{' . matchIndex . '}' . s:inputDirectiveExpression . '.*' )

    if ( !has_key( existingVariables, variable ) )
      let result .= '::'

      let existingVariables[ variable ] = ''
    endif

    let result .= modifier . variable
    let result .= '}'

    let startIndex = matchend( a:line, s:inputDirectiveExpression, startIndex )
    let matchIndex = match( a:line, s:inputDirectiveExpression, startIndex )
  endwhile

  let result .= a:line[ startIndex : ]

  return result
endfunction

function! PlaceTemplateForLine( lineNumber )
  " Break the undo chain so hitting undo gives the user back the word they had typed to launch this mapping.
  execute "normal! i\<c-g>u"

  let jumpCharacters = GetVar#GetVar( "MessageFormatter_jumpMarker" )
  let line           = getline( a:lineNumber )

  let args    = split( line, GetVar#GetVar( "MessageFormatter_parameterSeparator" ) )
  let numArgs = len( args )

  " If only one argument was found, it's possible they decided to split on single spaces with no spaces in the text.
  if ( numArgs == 1 )
    let args = split( line, '\s\+' )
    let numArgs = len( args )
  endif

  let templateName       = args[ 0 ]
  let templateDefinition = GetTemplateDefinition( templateName )

  let templateDefinition = ExpandDirectiveValues( templateDefinition, 'tem' )
  let templateDefinition = ExpandDirectiveValues( templateDefinition )
  let templateDefinition = ConsolidateDuplicateDirectives( templateDefinition )

  " By keeping the empty result, the first value can always be prepended as is--if the expression starts with a variable, it'll just be the empty string.
  let splitted  = split( templateDefinition, "{::", 1 )
  let numSplits = len( splitted )

  if ( templateDefinition == '!' . templateName . '!' )
    echo printf( "The template \"%s\" wasn't found locally or globally.", templateName )

    startinsert!
  elseif ( numArgs < ( numSplits - s:numOptionalArguments ) )
    if ( s:numOptionalArguments > 0 )
      echo printf( "Not enough arguments for \"%s\"; need at least %d (up to %d), but got only %d.", templateName, numSplits - s:numOptionalArguments - 1, numSplits - 1, numArgs - 1 )
    else
      echo printf( "Not enough arguments for \"%s\"; need exactly %d, but got only %d.", templateName, numSplits - 1, numArgs - 1 )
    endif

    startinsert!
  else
    let result = splitted[ 0 ]

    let argCounter = 1

    while ( argCounter < numSplits )
      let result .= '{'

      if ( argCounter < numArgs )
        let result .= args[ argCounter ] == GetVar#GetVar( "MessageFormatter_blankParameter" ) ? '' : args[ argCounter ]
      endif

      let result .= '::'
      let result .= splitted[ argCounter ]

      let argCounter += 1
    endwhile

    " Convert newlines
    let result = substitute( result, '\\n', "\n", 'g' )

    " If the expansion contains jump directives.
    if ( GetVar#GetVar( "MessageFormatter_autoAddJumpToEnd" ) == 1 && result =~# '!jump!' && result !~# '!jump!$' )
      let result .= '!jump!'
    endif

    " Convert jump directives
    let result = substitute( result, '!jump!', jumpCharacters, 'g' )

    let savedFo = &fo
    set fo=
    execute "normal! cc\<c-r>=result\<esc>"
    let &fo = savedFo

    let snippetStart = line( "'[" )
    let snippetEnd   = line( "']" )

    '[,']Formatvisualrange 0

    call MessageFormatter#EditFirstJumpLocation( snippetStart, snippetEnd )
  endif
endfunction

" An add abbreviation method that replaces empty {::variable} types with {«»::variable} and the very first one with a |; also replaces literal \n (backslash
" followed by an n) with newline characters.
function! AddMessageFormatterTemplate( isGlobal, args )
  let [ original, variable, expansion; remainder ] = matchlist( a:args, '^\(\S\+\)\s\+\(.*\)$' )

  let variableRoot   = a:isGlobal ? "g" : "b"
  let templateHolder = variableRoot . ':MessageFormatter_templates'

  if ( !exists( templateHolder ) )
    let {templateHolder} = {}
  endif

  let {templateHolder}[ variable ] = expansion
endfunction

function! ShowTemplates( scope, templateName )
  try
    let dictionary = {a:scope}:MessageFormatter_templates

    if ( a:templateName == '' )
      echo sort( keys (dictionary) )
    elseif ( has_key( dictionary, a:templateName ) == 1 )
      echo a:templateName . ': ' . dictionary[ a:templateName ]
    else
      let globalOrLocal = a:scope == 'g' ? 'global' : 'local'

      echo printf( "No %s template called '%s' found. List of %s templates: %s", globalOrLocal, a:templateName, globalOrLocal, string( sort( keys( dictionary ) ) ) )
    endif
  catch
    echo printf( "No %s templates defined.", a:scope == 'g' ? 'global' : 'local' )
  endtry
endfunction

function! LocalTemplateList( A, C, P )
  return exists( "b:MessageFormatter_templates" ) ? join( sort( keys( b:MessageFormatter_templates ) ), "\n" ) : ""
endfunction

function! GlobalTemplateList( A, C, P )
  return exists( "g:MessageFormatter_templates" ) ? join( sort( keys( g:MessageFormatter_templates ) ), "\n" ) : ""
endfunction

" This should be an option.
function! s:ColorDirectives()
  " syn match MessageFormatter_DirectiveDefaults '[:_]\zs\%(override\|default\)' containedin=MessageFormatter_Directive
  syntax match MessageFormatter_DirectiveModifiers '::\zs[^_}]*\ze_' containedin=MessageFormatter_Directive
  execute 'syntax match MessageFormatter_Directive ''{\%(' . GetVar#GetVar( 'MessageFormatter_jumpMarker' ) . '\)\=::.\{-}}'' containedin=ALL'
endfunction

function! SetColorDirectives( enable )
  augroup MessageFormatter
    au!
    if ( a:enable )
      au Syntax * call s:ColorDirectives()
      au ColorScheme * call s:ColorDirectives()

      call s:ColorDirectives()
    else
      syntax clear MessageFormatter_DirectiveModifiers
      syntax clear MessageFormatter_Directive
    endif
  augroup END
endfunction

function! MessageFormatter_CompleteTemplates( findstart, base )
  if ( a:findstart )
    " locate the start of the word
    let line  = getline( '.' )
    let start = col( '.' ) - 1

    while ( start > 0 && line[ start - 1 ] =~ '\S' )
      let start -= 1
    endwhile

    return start
  else
    " find months matching with "a:base"
    let templates = exists( "b:MessageFormatter_templates" ) ? b:MessageFormatter_templates : {}
    call extend( templates, g:MessageFormatter_templates )

    let res = []

    " for m in ( sort( keys( b:MessageFormatter_templates ) ) + sort( keys( g:MessageFormatter_templates ) ) )
    for m in ( sort( keys( templates ) ) )
      if ( m =~ '^' . a:base )
        let newItem = {}

        let newItem.word = m
        let newItem.menu = templates[ m ]

        call add( res, newItem )
      endif
    endfor

    return res
  endif
endfun

com! -nargs=+ Addglobaltemplate call AddMessageFormatterTemplate( 1, <q-args> )
com! -nargs=+ Addlocaltemplate call AddMessageFormatterTemplate( 0, <q-args> )

com! -nargs=? -complete=custom,GlobalTemplateList Listglobaltemplates call ShowTemplates( 'g', <q-args> )
com! -nargs=? -complete=custom,LocalTemplateList Listlocaltemplates call ShowTemplates( 'b', <q-args> )

com! -nargs=? -range Formatvisualrange :call MessageFormatter#FormatVisualRange( <line1>, <line2>, <q-args> )

com! -nargs=+ Formatmessage echo FormatMessage( <q-args>, 0 )
com! -nargs=+ Formatmessagerecursive echo FormatMessage( <q-args>, 1 )
com! Resetformatparameters Unlet g:MessageFormatter_parameters
com! -nargs=+ Addformatparameter call AddFormatParameters( <q-args> )
com! -nargs=+ Adddictionaryformatparameter call AddDictionaryFormatParameter( <q-args> )
com! Showparameters echo GetVar#GetSafe( "g:MessageFormatter_parameters", "<No parameters have been defined.>" )
com! -nargs=+ Formatcontainedmessage echo FormatContainedMessage( <q-args> )

com! -nargs=1 Setcolordirectives call SetColorDirectives( <args> )


execute 'hi link MessageFormatter_DirectiveModifiers ' . GetVar#GetVar( 'MessageFormatter_highlightDirectiveModifiersLink' )
execute 'hi link MessageFormatter_Directive ' . GetVar#GetVar( 'MessageFormatter_highlightDirectivesLink' )

if ( GetVar#GetVar( 'MessageFormatter_highlightDirectives' ) == 1 )
  Setcolordirectives 1
endif


" SALMAN: Create another mechanism for a set of fixed values to be used. Let user select first few letters of one of these values instead of the whole thing; in
" case of ambiguity, first match works.

" If the template start and end were stored, expands automatically (even if the template spans multiple lines).
if ( !hasmapto( '<Plug>FormatCurrentTemplate', 'i' ) )
  imap <silent> <c-del> <Plug>FormatCurrentTemplate
endif

if ( !hasmapto( '<Plug>FormatCurrentTemplate', 'n' ) )
  nmap <silent> <c-del> <Plug>FormatCurrentTemplate
endif

if ( !hasmapto( '<Plug>PlaceTemplateInText', 'i' ) )
  imap <silent> <Leader><Leader> <Plug>PlaceTemplateInText
endif

if ( !hasmapto( '<Plug>PlaceTemplateForLine', 'i' ) )
  imap <silent> `` <Plug>PlaceTemplateForLine
endif

if ( !hasmapto( '<Plug>FormatOneLine', 'n' ) )
  nmap <silent> <c-del><c-del> <Plug>FormatOneLine
endif

if ( !hasmapto( '<Plug>FormatVisualRange', 'v' ) )
  vmap <silent> <c-del> <Plug>FormatVisualRange
endif

" Mapping for operator-mode.
if ( !hasmapto( '<Plug>FormatOpModeTemplate', 'n' ) )
  nmap <silent> <c-s-del> <Plug>FormatOpModeTemplate
endif

if ( !hasmapto( '<Plug>MessageFormatter_InsertModeCompletion', 'i' ) )
  imap <silent> // <Plug>MessageFormatter_InsertModeCompletion
endif

imap <Plug>FormatCurrentTemplate <esc>:call MessageFormatter#FormatCurrentTemplate( 1 )<cr>
nmap <Plug>FormatCurrentTemplate :call MessageFormatter#FormatCurrentTemplate( 0 )<cr>
inoremap <Plug>PlaceTemplateInText <esc>:call PlaceTemplateInText()<cr>
inoremap <Plug>PlaceTemplateForLine <esc>:call PlaceTemplateForLine( '.' )<cr>
nmap <Plug>FormatOneLine :Formatvisualrange<cr>
vmap <Plug>FormatVisualRange :Formatvisualrange<cr>
nmap <Plug>FormatOpModeTemplate :set opfunc=MessageFormatter#FormatOpModeTemplate<cr>g@
imap <Plug>MessageFormatter_InsertModeCompletion <c-o>:set completefunc=MessageFormatter_CompleteTemplates<cr><c-x><c-u>


if ( GetVar#GetSafe( "g:MessageFormatter_createDefaultTemplates", 1 ) == 1 )
  Addglobaltemplate eval {::expression} = {eval {expression}::expressionValue}
  Addglobaltemplate evalq {eval {::expression}::expressionValue}
  Addglobaltemplate sep {eval strpart( repeat( {def =::p_separator}, {def &tw::length} ), 0, {length} )::line}
  Addglobaltemplate ca {::c_arg}
  Addglobaltemplate co {::C_arg}
  Addglobaltemplate cf {::cf_arg}
  Addglobaltemplate sc {::Cl_arg}
  Addglobaltemplate gf {eval expand( '%:p:t:r' )::thisFile}
  Addglobaltemplate td {eval strftime("%A, %B %d, %Y")::thisDate}
endif
autoload/MessageFormatter.vim	[[[1
732
" MessageFormatter.vim: an autoload plugin to format strings with parameters
" By: Salman Halim
"
" Version 8.0:
"
" New special value: "iab" to expand insert-mode abbreviations inline; see :help MessageFormatter_iab.
"
" New mapping: <Plug>InsertModeCompletion (// by default) that will complete templates during insert-mode.
"
" Version 7.5:
"
" New special value: "tem" to include other template definitions inline; see :help MessageFormatter_tem.
"
" Version 7.0:
"
" New option: g:MessageFormatter_moveArgumentsToStart; if 1 (the default), input arguments are moved to the front for easier expansion. See :help
" g:MessageFormatter_moveArgumentsToStart for details.
"
" More new options: g:MessageFormatter_highlightDirectives and g:MessageFormatter_highlightDirectivesLink; control the highlighting of directives in text to
" make them stand out.
"
" New command: Setcolordirectives: toggles the directive highlighting feature.
"
" Version 6.5:
"
" New modifier:
"
" o: if the value is non-empty, prepends a ", " (a comma and a space); otherwise, leaves it empty
" P: Just like 'p', but ignores empty strings
" Q: Just like 'q', but ignores empty strings
"
" Version 6.0:
"
" Added an option: g:MessageFormatter_autoAddJumpToEnd; if this is 1 (the default) and if a parameter contains !jump! directives, another !jump! is added to the
" end to allow the user to quickly continue typing beyond the template. See :help g:MessageFormatter_autoAddJumpToEnd.
"
" Version 5.5:
"
" Fixed bug in default processing.
"
" Improved help and added examples to help (:help MessageFormatter_Examples).
"
" Version 5.0:
"
" Added a default value mechanism: if a template variable is defined like this:
"
" {def John::firstName}
"
" then, during expansion, if an empty value is passed in for firstName, "John" will be used instead. This value can be recursive and may contain other
" parameters, as before. (Including other "def" expansions.)
"
" Also, parameters with default values may be left out to have their default value used; see :help MessageFormatter_def for more details.
"
" Version 4.5:
"
" Bug fixes, mostly, though added one more option:
"
" g:MessageFormatter_formatCurrentLineAsFallback (default 1): if attempting to format a template via the <Plug>FormatCurrentTemplate mapping when not actually
" in a template, will fall back to formatting just the current line as an ad-hoc template if this is 1. If 0, will give an error message instead.
"
" Version 4.0:
"
" Changed the interface, adding the ability to expand templates while typing.
"
" Commands:
" Addglobaltemplate: adds a template pattern useful everywhere
" Addlocaltemplate: adds a template pattern for the current buffer only (might be used from an ftplugin)
"
" Listglobaltemplates: simple list of global templates
" Listlocaltemplates: simple list of local (buffer-specific) templates
"
" Formatvisualrange: expands a range of text lines (in the buffer) containing default values. Best called from mappings rather than directly.
"
" Patterns:
"
" The examples (loaded unless g:MessageFormatter_createDefaultTemplates is set to 0 in your vimrc) define some fairly complex templates. Dissecting one:
"
" Addglobaltemplate var {::type} {::c_var} = new {eval {p_type} =~# '^List' ? 'Array{type}' : {p_type} =~# '^Map' ? 'Hash{type}' : {p_type}::instanceType}();
"
" Template name: var
" Number of arguments: 2 ('type' and 'var'); defined by the {::...} format
"
" Template particulars:
"
" {::type}: get value for 'type' and use as is.
" {::c_var}: get value for 'var' and camel-case it.
" {eval {p_type} =~# '^List' ? 'Array{type}' : {p_type} =~# '^Map' ? 'Hash{type}' : {p_type}::instanceType}: variable 'instanceType' is given an "eval" value,
" which is evaluated as a ternary expression:
"
" {p_type} =~# '^List' ? 'Array{type}' : {p_type} =~# '^Map' ? 'Hash{type}' : {p_type}
"
" If the value, wrapped in single quotes (p_type; suitable for use in a Vim expression), starts with List, instantiate it as an ArrayList; if it starts with
" Map, instantiate it has a HashMap; otherwise, use as is. (List and Map are interfaces in Java and can't be instantiated directly).
"
" This allows you to to type
"
" var List<String> names
"
" and hit the hotkey for <Plug>PlaceTemplateForLine (see the mapping for more details) in insert mode to get
"
" List<String> names = new ArrayList<String>();
"
" Another one:
"
" Addglobaltemplate do do\n{\n!jump!\n} while ( !jump! );
"
" Type do and hit the mapping for <Plug>PlaceTemplateInText and it will replace the word with the expansion in place (can be used with 'var', also), moving the
" cursor to the first !jump! and replacing the other !jump! with jump characters.
"
" If the <Plug>PlaceTemplateInText mapping is used for expansions such as 'var'' above, the cursor goes to the first {::...} variable and all others change to
" {!jump!::...} so a jump hotkey (assuming there is a jumper plugin installed) will move from one to the next. Once the values have been entered, hit the hotkey
" for <Plug>FormatCurrentTemplate (insert or normal mode) and the template will be expanded inline. The plugin remembers the last template placed, so if a
" second template is placed before the first is expanded, the first will be forgotten.
"
" Notes:

" - Patterns replace all \n instances (backslashes followed by 'n') with newlines and all !jump! occurrences with the value of MessageFormatter_jumpMarker.
"
" - Template definitions are searched locally and then globally, thus a local template with the same name will be used instead of the global one.
"
" Mappings:
"
" The following mappings have been defined (shown here with default values); if you set up the <Plug> mappings yourself, these won't be assigned:
"
" imap <c-del>          <Plug>FormatCurrentTemplate: expands the last inline template placed by <Plug>PlaceTemplateInText, as long as the cursor is within the
" range of lines placed by the mapping. The cursor is placed at the end of the expansion, in insert mode, so more typing can be accomplished.
"
" nmap <c-del>          <Plug>FormatCurrentTemplate: same as the imap version, but from normal mode, except that the cursor doesn't move and stays in normal
" mode.
"
" imap <Leader><Leader> <Plug>PlaceTemplateInText: given a template name as the word on the cursor, replaces it with the expansion; if the expansion isn't
" found, it's just replaced with itself, wrapped in exclamation marks (so, 'adsfasdfadsf' becomes '!adsfasdfadsf!'). Hit undo to get back to the word.
"
" imap ``               <Plug>PlaceTemplateForLine: given a line containing nothing but a template name and parameter values, will replace the line with the
" fully expanded template. The arguments should be separated by MessageFormatter_parameterSeparator or, if the arguments are single words (themselves contain no
" whitespace) they may be separated by a single space to ease data entry. If a truly blank value is desired for any of the parameters, use
" MessageFormatter_blankParameter as the value.
"
" nmap <c-del><c-del>   <Plug>FormatOneLine
"
" nmap <c-s-del>        <Plug>FormatOpModeTemplate
"
" Note: <Plug>PlaceTemplateInText looks only at the current word so is useful for templates that are meant to be used as part of a whole line (look at
" 'safetern' in the examples), although it can be used for multi-line templates, also. <Plug>PlaceTemplateForLine looks at the entire line because it expects
" all the parameter values to be provided up front.
"
" Options (with default values):
"
" g:MessageFormatter_blankParameter (default '`'--back-tick): if a value is to be blank, use this in its place to indicate same
"
" g:MessageFormatter_parameterSeparator (default '\s\{2,}'--two or more whitespace characters): separator for multiple arguments; maybe change it to a single
" tab or another regular expression such as :: or something
"
" g:MessageFormatter_jumpMarker (default '«»'): replaces !jump! values in expansions with this expression as well as, in inline parameters, for all but the
" first parameter (the cursor is placed on the first one).
"
" As always, they may be set on a per buffer basis (with b: versions), per window or per tab because my GetVar script is required for this to work.
"
" New modifiers types:
"
" m: expects a number immediately after and repeats the text the specified number of times; no number is the same as 1. So, {m3_name} converts 'John' to
" 'JohnJohnJohn' and {*::m30_v} becomes ******************************
" p: Wrap result in apostrophes, escaping single apostrophes with doubles (suitable for Vim literal string use); so, {ab'c::p_var} gives 'ab''c' (including the
" quotes)
" q: Wrap result in quotes, escaping inner quotes and backslashes with backslashes; so, {ab"c::q_var} gives "ab\"c" (including the quotes)
" s: If the result is empty, nothing; otherwise, wrap it in spaces. So, '0' becomes ' 0 ', but '' remains ''
"
"
" Version 3.5:
"
" Added a command version of FormatContainedMessage called Formatcontainedmessage that passes everything on the command-line as-is to the function and echoes
" the result.
"
" Added a new default value type:
"
" If the default value for for a parameter (passed to FormatContainedMessage) is "ask", it defaults to asking the user (via an input). If the value is anything
" followed by "ask", it will use that as the default value for the input.
"
" Examples:
" "
" {ask::first name} will display an input prompt asking, "Enter value for first name".
"
" {ask Smith::last name} will display an input prompt asking "Enter value for last name", but will offer "Smith" as the default value (just press enter to
" accept).
"
" As always, recursion is supported, so
"
" My name is {ask {ask John::first name} {ask Smith::last name}::full name} (family name is {last name}).
"
" Will first prompt for the first name, offering "John" as the default, then for the last name, offering "Smith" as the default and, finally, for the full name,
" offering the entered first and last name together as the default ("John Smith" by default) before displaying the formatted message.
"
" Version 3.0:
"
" Fairly big changes. RELIES UPON MY GetVar.vim script now.
"
" New formatting parameter:
"
" n: non-displayed value. The return value is suppressed--useful for adding a value to the cache to be used later.
"
" New functions:
"
" FormatContainedMessage: Works like MessageFormatter#FormatMessage except that it's always recursive and that its original text string can contain default
" values for the parameters (so the second parameter is optional); for example,
"
" echo FormatContainedMessage( 'My first name is {John::first name} and my full name is {{first name} Smith::full name}; how many letters in {u_full name}?' )
"
" Gives
"
" My first name is John and my full name is John Smith; how many letters in JOHN SMITH?
"
" This can be recursive (look at "full name" and can contain more default values, such as starting things with "eval " to have them evaluated. To force an empty
" string, use {::name} (just {name} assumes that the value will be specified elsewhere).
"
" Some commands and functions to allow the reuse of parameters:
"
" FormatMessage: takes a string and returns a formatted value, using previously specified parameters through:
"
" Formatmessage and Formatmessagerecursive: command versions that call FormatMessage; process whatever is typed.
"
" Addformatparameter: adds whatever you type as is (may be recursive) as a list parameter.
"
" Adddictionaryformatparameter: adds whatever you type as is, with the first word being the name of the parameter and the rest of the arguments the value.
"
" Showparameters: Displays the list of specified parameters.
"
" Resetformatparameters: Removes the list of parameters currently specified.
"
" Version 2.0:
"
" Added new formatting modifier:
"
" e: Escapes out quotation marks (") and backslashes, leaving the value suitable for placing in quotes. For example, {e_fName} where fName is Jo\nhn results in
" Jo\\nhn.
"
" If an expansion parameter starts with "eval ", the rest of the value is evaluated and the return value used as the actual parameter value. If recursion is on,
" that value may contain further parameters.
"
" Example:
"
" echo MessageFormatter#FormatMessage('public static final {type} {C_variable} = {value};', {'type':'eval input("Type for {variable}: ", "String")', 'variable':'eval input("Variable name: ")', 'value':'eval input("Value: ", "\"{C_variable}\"")'}, 1)
"
" Bear in mind that 'type' and 'value' both use the parameter 'variable'. If 'variable' were to refer to either of these, you'd have circular recursion. There
" is no check in place for that; you'd just end up with a stack overflow.
"
" Note, also, that the expression is evaluated only once. After that, its value is stored on the cache--this allows eval parameters to refer to other eval
" parameters (only useful if recursion is on).
"
" Version 1.5:
"
" Added a cache so repeated expansions of the same variable can be looked up rather than computed (potentially much faster, especially when recursion is on).
"
" Original version (1.0):
"
" Given a pattern string and a set of values, replaces the parameters in the pattern with the specified values. The values may be either a Dictionary or a List,
" and the expansion can be done recursively or just once, as in the example below.
"
" Example (note the 1 at the end, indicating recursion as fullName is defined as fName and lName):
"
" echo MessageFormatter#FormatMessage( "My name is {fullName} (first name '{fName}', last name '{lName}').", { 'fName': 'john', 'lName': 'smith', 'fullName': '{t_fName} {t_lName}' }, 1 )
"
" Echoes (note how fullName gets further expanded): My name is John Smith (first name 'john', last name 'smith').
"
" Same example, with a 0 at the end (or nothing, as the recursion parameter is optional):
"
" Echoes (note how fullName is not expanded recursively): My name is {t_fName} {t_lName} (first name 'john', last name 'smith').
"
" Same (recursive) example using a List instead of a Dictionary:
"
" echo MessageFormatter#FormatMessage( "My name is {2} (first name '{0}', last name '{1}').", [ 'john', 'smith', '{t_0} {t_1}' ], 1 )
"
" Note: To use an actual '{' or '}', escape it with a backslash.
"
" Observe how some of the parameters start with t_ before the actual name. There are a number of formatting parameters:
"
" a (optional): as is
" l: lower case
" u: upper case
" f: first letter capitalized only
" t: title case the entire string
" c: camel case: converts 'an interesting phrase' to 'anInterestingPhrase'
" C: constant mode: converts 'anInterestingPhrase' or 'an interesting phrase' to 'AN_INTERESTING_PHRASE'
" w: from 'anInterestingPhrase' or 'AN_INTERESTING_PHRASE' to 'an intersting phrase'
"
" Both the parameters AND the formatting directives are case-sensitive. If multiple formatting parameters are specified, they will be applied in the order
" supplied. For example {c_val} where 'val' is 'some variable' results in 'someVariable'. However, {cf_val} gives 'SomeVariable' because the following 'f'
" capitalizes the first letter of the result from 'c' (camel case).
"
" Installation: Unzip into a directory in your &runtimepath.
"
" Other examples
"
" let g:test="public void set{f_1}( {0} val )\n".
" \ "\\{\n".
" \ "m_{1} = val;\n".
" \ "\\}\n".
" \ "\n".
" \ "public {0} get{f_1}()\n".
" \ "\\{\n".
" \ "return m_{1};\n".
" \ "\\}"
" let g:parameters=[ 'String', 'test' ]

" let g:message = "{0}, {1}, \\{{2},\\} {3} {0} {0} {2}"
" let g:parameters = [ "john", "smith (plus {u_0})", "{1}, {0} {t_3}", '\{{0} {1}\}' ]
"
" let g:test="/**\n" .
"       \ "Constant value for '{f_0}'.\n" .
"       \ "/\n" .
"       \ "public static final String {C_0} = \"{c_0}\";"


" Case in directives is maintained. Thus, {fname} is NOT the same as {FNAME}.
"
" This is especially important during recursion.

" Modifiers may be combined: {wt_test}--where 'test' maps to 'SOME_CONSTANT_TEXT'--will result in 'Some Constant Text'.
"
" The moifiers are executed in the order received.
"
" a (optional): as is
" c: camel case: converts 'an interesting phrase' to 'anInterestingPhrase'
" C: constant mode: converts 'anInterestingPhrase' or 'an interesting phrase' to 'AN_INTERESTING_PHRASE'
" e: Escapes out quotation marks (") and backslashes, leaving the value suitable for placing in quotes
" f: first letter capitalized only
" l: lower case
" m: expects a number immediately after and repeats the text the specified number of times; no number is the same as 1. So, {m3_name} converts 'John' to
" 'JohnJohnJohn' and {*::m30_v} becomes ******************************
" n: Suppresses output; useful for adding the value to cache without actually displaying it
" o: if the value is non-empty, prepends a ", " (a comma and a space); otherwise, leaves it empty
" p: Wrap result in apostrophes, escaping single apostrophes with doubles (suitable for Vim literal string use)
" P: Just like 'p', but ignores empty strings
" q: Wrap result in quotes, escaping inner quotes and backslashes with backslashes
" Q: Just like 'q', but ignores empty strings
" s: If the result is empty, nothing; otherwise, wrap it in spaces. So, '0' becomes ' 0 ', but '' remains ''
" t: title case the entire string
" u: upper case
" w: from 'anInterestingPhrase' or 'AN_INTERESTING_PHRASE' to 'an intersting phrase'
function! MessageFormatter#ModifyValue( value, modifiers )
  let result = a:value
  let i      = 0

  while ( i < len( a:modifiers ) )
    let modifier = a:modifiers[ i ]

    if ( modifier ==# '' || modifier ==# 'a' )
      let result = result
    elseif ( modifier ==# 'l' )
      let result = tolower( result )
    elseif ( modifier ==# 'u' )
      let result = toupper( result )
    elseif ( modifier ==# 'f' )
      let result = substitute( result, '^.', '\U&', '' )
    elseif ( modifier ==# 't' )
      let result = substitute( result, '\(\<.\)\(\S*\)', '\u\1\L\2', 'g' )
    elseif ( modifier ==# 'c' )
      " If there are no spaces or underscores, it's probably already camel case, so leave it alone.
      if ( match( result, '[_ ]' ) >= 0 )
        let result = substitute( tolower( result ), '[_ ]\([a-z]\)', '\u\1', 'g' )
      endif
    elseif ( modifier ==# 'C' )
      let result = toupper( substitute( substitute( result, '\C\([^A-Z]\)\([A-Z]\)', '\1_\2', 'g' ), '[[:space:]_]\+', '_', 'g' ) )
    elseif ( modifier ==# 'w' )
      let result = tolower( substitute( substitute( result, '\C\([^A-Z]\)\([A-Z]\)', '\1 \2', 'g' ), '_', '', 'g' ) )
    elseif ( modifier ==# 'e' )
      let result = escape( result, '"\' )
    elseif ( modifier ==# 'p' )
      let result = "'" . substitute( result, "'", "''", "g" ) . "'"
    elseif ( modifier ==# 'P' )
      let result = result == '' ? '' : "'" . substitute( result, "'", "''", "g" ) . "'"
    elseif ( modifier ==# 'q' )
      let result = '"' . escape( result, '"\' ) . '"'
    elseif ( modifier ==# 'Q' )
      let result = result == '' ? '' : '"' . escape( result, '"\' ) . '"'
    elseif ( modifier ==# 'n' )
      let result = ''
    elseif ( modifier ==# 's' )
      let result = result == '' ? result : ' ' . result . ' '
    elseif ( modifier ==# 'o' )
      let result = result == '' ? result : ', ' . result
    elseif ( modifier ==# 'm' )
      let multiplier = ''

      while ( a:modifiers[ i + 1 ] =~ '\d' )
        let multiplier .= a:modifiers[ i + 1 ]

        let i += 1
      endwhile

      let result = repeat( result, multiplier == '' ? 1 : multiplier )
    else
      " Unrecognized modifier.
      let result = '!' . modifier . '!' . result
    endif

    let i += 1
  endwhile

  return result
endfunction

" Expands an abbreviation (insert mode) and returns the value.
function! MessageFormatter#ExpandAbbreviation( abbreviation )
  let savedZ = @z

  " On a fresh line, expand the abbreviation, yank the result and then undo it.
  execute "silent! normal o\<esc>0C" . a:abbreviation . "\<esc>0\"zy$u"

  let result = @z

  let @z = savedZ

  return result
endfunction

function! MessageFormatter#ProcessOnce( message, parameters, recursive )
  let isDictionary = type( a:parameters ) == 4

  let parameterPattern = '\C{\%(\([^_}]*\)_\)\=\([^}]\+\)}'

  let result     = ''
  let startIndex = 0
  let matchIndex = match( a:message, parameterPattern, startIndex )

  while ( matchIndex >= 0 )
    let [ expansionDirective, modifiers, parameter; dummy2 ] = matchlist( a:message, parameterPattern, startIndex )

    if ( matchIndex > 0 )
      let result .= a:message[ startIndex : matchIndex - 1 ]
    endif

    " If it's a dictionary, it needs to have the key; otherwise (it's a list), it needs to have at least as many items as the parameter. If not, just return the
    " parameter unexpanded.
    let keyExists      = 0
    let parameterValue = ''
    let gotValue       = 0

    " If it's in the cache already, no need to look among the parameters for a value.
    if ( has_key( s:parameterCache, parameter ) )
      let parameterValue = s:parameterCache[ parameter ]

      let gotValue = 1
    else
      if ( isDictionary )
        let keyExists = has_key( a:parameters, parameter )
      else
        let keyExists = len( a:parameters ) > parameter
      endif

      if ( keyExists )
        let parameterValue = a:parameters[ parameter ]

        if ( a:recursive )
          let parameterValue = MessageFormatter#FormatMessageInternal( parameterValue, a:parameters, 1 )
        endif

        if ( parameterValue =~# '^eval ' )
          let parameterValue = string( eval( substitute( parameterValue, '^eval ', '', '' ) ) )

          " Remove up to two apostrophes from the outside; they are usually spurious--if not, will deal with it if needed.
          let parameterValue = substitute( parameterValue, '^''\{,2}\(.\{-}\)''\{,2}$', '\1', 'g' )
        elseif ( parameterValue =~# 'ask\%( \|$\)' )
          let parameterValue = input( "Set '" . parameter . "' to: ", substitute( parameterValue, '^ask \=', '', '' ) )
        elseif ( parameterValue =~# '^iab ' )
          let parameterValue = MessageFormatter#ExpandAbbreviation( substitute( parameterValue, '^iab ', '', '' ) )
        endif

        let s:parameterCache[ parameter ] = parameterValue

        let gotValue = 1
      endif
    endif

    if ( gotValue == 1 )
      let result .= MessageFormatter#ModifyValue( parameterValue, modifiers )
    else
      let result .= expansionDirective
    endif

    let startIndex = matchend( a:message, parameterPattern, startIndex )
    let matchIndex = match( a:message, parameterPattern, startIndex )
  endwhile

  let result .= a:message[ startIndex : ]

  return result
endfunction

function! MessageFormatter#FormatMessageInternal( message, parameters, recursive )
  let expandedMessage = substitute( a:message, '\\{', '_OPEN_BRACE_', 'g' )
  let expandedMessage = substitute( expandedMessage, '\\}', '_CLOSE_BRACE_', 'g' )

  let result = MessageFormatter#ProcessOnce( expandedMessage, a:parameters, a:recursive )

  let result = substitute( result, '_OPEN_BRACE_', '{', 'g' )
  let result = substitute( result, '_CLOSE_BRACE_', '}', 'g' )

  return result
endfunction

let s:parameterCache = {}

function! MessageFormatter#ResetParameterCache()
  let s:parameterCache = {}
endfunction

" If not recursive, start from beginning, get parameter, expand, continue.
"
" If recursive, repeat until no change.
function! MessageFormatter#FormatMessage( message, parameters, ... )
  let recursive = exists( "a:1" ) && a:1 == 1

  if ( !exists( "a:2" ) || a:2 != 1 )
    call MessageFormatter#ResetParameterCache()
  endif

  return MessageFormatter#FormatMessageInternal( a:message, a:parameters, recursive )
endfunction

function! MessageFormatter#FormatOpModeTemplate( type, ... )
  execute "'[,']Formatvisualrange"
endfunction

function! MessageFormatter#EditFirstJumpLocation( startLine, endLine )
  " Where the cursor should go.
  " |
  " |
  " a|b
  " ||
  " \%(^\|[^|]\)\zs|\ze\%([^|]\|$\)
  "
  " Either the beginning of the line or a non-pipe character followed by a pipe followed by a non-pipe charcter or the end of the line. Forces the system to find
  " a single pipe character (avoiding the || boolean construct).
  " let searchPosition = search( '\%(^\|[^|]\)\zs|\ze\%([^|]\|$\)', 'cW', line( "']" ) )

  " Go to the beginning of the snippet
  execute a:startLine
  normal! 0

  let jumpCharacters = GetVar#GetVar( "MessageFormatter_jumpMarker" )

  " If a jump marker is found, put the cursor there; otherwise, move it to the end of the expansion.
  let searchPosition = search( jumpCharacters, 'cW', a:endLine )

  " Fix this if at end of line... if col at end of line == searchPosition + length of strlen...
  if ( searchPosition > 0 )
    let cursorPositionFromLine = col( '$' ) - col( '.' )

    execute 'normal "_' . strlen( jumpCharacters ) . 'x'

    " End of line
    if ( cursorPositionFromLine == strlen( jumpCharacters ) )
      startinsert!
    else
      startinsert
    endif
  else
    normal ']
    startinsert!
  endif
endfunction

function! MessageFormatter#FormatCurrentTemplate( endInInsertMode )
  let error = ''

  if ( !exists( "b:MessageFormatter_snippetStart" ) || !exists( "b:MessageFormatter_snippetEnd" ) )
    let error = "Not in a formally defined template."
  else
    let currentLine = line( '.' )

    if ( currentLine >= b:MessageFormatter_snippetStart && currentLine <= b:MessageFormatter_snippetEnd )
      " Break the undo chain.
      execute "normal! i\<c-g>u"

      execute b:MessageFormatter_snippetStart . ',' . b:MessageFormatter_snippetEnd . 'Formatvisualrange'

      " Start insert mode so the user can continue.
      if ( a:endInInsertMode )
        call MessageFormatter#EditFirstJumpLocation( b:MessageFormatter_snippetStart, b:MessageFormatter_snippetEnd )
      endif

      return
    endif
  endif

  if ( error == '' )
    let error = "Not inside the last used template."
  endif

  if ( GetVar#GetVar( "MessageFormatter_formatCurrentLineAsFallback" ) == 1 )
    echo error . ' Falling back to current line.'

    " Break the undo chain.
    execute "normal! i\<c-g>u"

    Formatvisualrange
  else
    echo error
  endif
endfunction

let s:escapeOpenBrace = '_OPEN_DIRECTIVE_BRACE_'
let s:escapeCloseBrace = '_CLOSE_DIRECTIVE_BRACE_'

" The last parameter, if 0 or not provided, doesn't break the undo chain.
function! MessageFormatter#FormatVisualRange( line1, line2, ... )
  " Break the undo chain unless asked not to.
  if ( !exists( "a:1" ) || a:1 == 1 )
    execute "normal! i\<c-g>u"
  endif

  call MessageFormatter#ResetParameterCache()

  let s:MessageFormatter_parameters = {}
  let currentLine                   = a:line1
  let firstLine                     = 1

  " Matches an expression that can be expanded; doesn't examine contents or anything; is a blanket match to see whether we should process the line.
  let blanketDirectiveExpression = '{.\{-}::\%([^_}]_*\)\=[^}]\{-}}'

  let directiveExpression       = '{\(\%(\\[{}]\|[^{}]\)\{-}\)::\%(\(\%(\\[{}]\|[^{}]\)\{-}\)_\)\=\(\%(\\[{}]\|[^{}]\)\{-}\)}'
  let simpleDirectiveExpression = '{\(\%(\\[{}]\|[^{}]\)\+\)}'

  " Extract the variable values from the lines, leaving the lines containing only variable names.
  while ( currentLine <= a:line2 )
    let originalLine = getline( currentLine )
    let newLine      = originalLine

    if ( newLine =~ blanketDirectiveExpression )
      let newLine    = ''
      let startIndex = 0
      let matchIndex = match( originalLine, simpleDirectiveExpression, startIndex )

      while ( matchIndex >= 0 )
        let [ original, variable; remainder ] = matchlist( originalLine, simpleDirectiveExpression, startIndex )

        if ( matchIndex > 0 )
          let newLine .= originalLine[ startIndex : matchIndex - 1 ]
        endif

        if ( variable !~ '::' )
          let newLine .= s:escapeOpenBrace . variable . s:escapeCloseBrace
        else
          let newLine .= original
        endif

        let startIndex = matchend( originalLine, simpleDirectiveExpression, startIndex )
        let matchIndex = match( originalLine, simpleDirectiveExpression, startIndex )
      endwhile

      let newLine .= originalLine[ startIndex : ]
    endif

    while ( newLine =~ directiveExpression )
      let [ original, value, modifiers, variable; remainder ] = matchlist( newLine, directiveExpression )

      let value = substitute( value, s:escapeOpenBrace, '{', 'g' )
      let value = substitute( value, s:escapeCloseBrace, '}', 'g' )

      let s:MessageFormatter_parameters[ variable ] = value == GetVar#GetVar( "MessageFormatter_jumpMarker" ) || value == GetVar#GetVar( "MessageFormatter_blankParameter" ) ? '' : value

      let replacement = modifiers == '' ? '\4' : '\3_\4'
      let newLine     = substitute( newLine, '^\(.\{-}\)' . directiveExpression . '\(.*\)$', '\1' . s:escapeOpenBrace . replacement . s:escapeCloseBrace . '\5', '' )
    endwhile

    let newLine = substitute( newLine, s:escapeOpenBrace, '{', 'g' )
    let newLine = substitute( newLine, s:escapeCloseBrace, '}', 'g' )

    if ( newLine !=# originalLine )
      if ( firstLine == 1 )
        let firstLine = 0
      else
        silent! undojoin
      endif

      call setline( currentLine, newLine )
    endif

    let currentLine += 1
  endwhile

  " Process the lines.
  let currentLine = a:line1

  while ( currentLine <= a:line2 )
    let thisLine = getline( currentLine )

    " Only process lines with directives (or what appear to be directives) on them.
    if ( thisLine =~ '{.\{-}}' )
      let newLine = MessageFormatter#FormatMessage( thisLine, s:MessageFormatter_parameters, 1, 1 )

      if ( newLine !=# thisLine )
        silent! undojoin

        call setline( currentLine, newLine )
      endif
    endif

    let currentLine += 1
  endwhile

  " Replace <CR> with newlines and <SW> with 'shiftwidth' spaces. This requires the entire set of lines to be stored and then restored en masse, I think.
  let result      = ''
  let changes     = 0
  let currentLine = a:line1

  while ( currentLine <= a:line2 )
    let thisLine = getline( currentLine )
    let newLine  = substitute( thisLine, '\C<CR>', "\n", 'g' )
    let newLine  = substitute( newLine, '\C<SW>', repeat( ' ', &sw ), 'g' )

    if ( newLine != thisLine )
      let changes = 1
    endif

    let result .= newLine

    if ( currentLine < a:line2 )
      let result .= "\n"
    endif

    let currentLine += 1
  endwhile

  let savedFo = &fo
  let savedPaste = &paste
  set fo=
  set paste
  execute "normal! " . a:line1 . "ggV" . a:line2 . "ggc\<c-r>=result\<ESC>"
  let &paste = savedPaste
  let &fo = savedFo
endfunction
