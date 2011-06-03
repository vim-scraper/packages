" MessageFormatter.vim: an autoload plugin to format strings with parameters
" By: Salman Halim
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
" Installation: drop into your autoload directory and use the MessageFormatter#FormatMessage function.
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
" l: lower case
" u: upper case
" f: first letter capitalized only
" t: title case the entire string
" c: camel case: converts 'an interesting phrase' to 'anInterestingPhrase'
" C: constant mode: converts 'anInterestingPhrase' or 'an interesting phrase' to 'AN_INTERESTING_PHRASE'
" w: from 'anInterestingPhrase' or 'AN_INTERESTING_PHRASE' to 'an intersting phrase'
" e: Escapes out quotation marks (") and backslashes, leaving the value suitable for placing in quotes.
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
      let result = substitute( result, '\(\<.\)\(\S\+\)', '\u\1\L\2', 'g' )
    elseif ( modifier ==# 'c' )
      " Breaks if text is already camelized.
      let result = substitute( tolower( result ), '[_ ]\([a-z]\)', '\u\1', 'g' )
    elseif ( modifier ==# 'C' )
      let result = toupper( substitute( substitute( result, '\C\([^A-Z]\)\([A-Z]\)', '\1_\2', 'g' ), '[[:space:]_]\+', '_', 'g' ) )
    elseif ( modifier ==# 'w' )
      let result = tolower( substitute( substitute( result, '\C\([^A-Z]\)\([A-Z]\)', '\1 \2', 'g' ), '_', '', 'g' ) )
    elseif ( modifier ==# 'e' )
      let result = escape( result, '"\' )
    else
      " Unrecognized modifier.
      let result = '!' . modifier . '!' . result
    endif

    let i += 1
  endwhile

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
    let keyExists = 0

    if ( isDictionary )
      let keyExists = has_key( a:parameters, parameter )
    else
      let keyExists = len( a:parameters ) > parameter
    endif

    if ( keyExists )
      if ( has_key( s:parameterCache, parameter ) )
        let parameterValue = s:parameterCache[ parameter ]
      else
        let parameterValue = a:parameters[ parameter ]

        if ( a:recursive )
          let parameterValue = MessageFormatter#FormatMessageInternal( parameterValue, a:parameters, 1 )
        endif

        if ( parameterValue =~ '^eval ' )
          let parameterValue = eval( substitute( parameterValue, '^eval ', '', '' ) )
        endif

        let s:parameterCache[ parameter ] = parameterValue
      endif

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

" If not recursive, start from beginning, get parameter, expand, continue.
"
" If recursive, repeat until no change.
function! MessageFormatter#FormatMessage( message, parameters, ... )
  let recursive = exists( "a:1" ) && a:1 == 1

  let s:parameterCache = {}

  return MessageFormatter#FormatMessageInternal( a:message, a:parameters, recursive )
endfunction
