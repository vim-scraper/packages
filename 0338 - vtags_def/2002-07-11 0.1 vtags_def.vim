" VTags language definitions.
"
" Expansions:
" 
" %F  File name.
" %L  Entire line contents.
" %C  Last context.
" %N  Line number.
"
" Flags:
"
" c   Don't set LastContext for this object.
"
" Example:
"
" We already have an "abc" language defined, and we want to add another
" language, which we will call "dummy".
"
" First, we add the language to the languages list:
" 	let g:vtags_lang = "abc, dummy"
"
" Now, we'll define the objects that this language uses. Let's say that it uses
" functions, classes and foobars.
" 	let g:vtags_dummy_obj = "function, class, foobar"
" 
" If we would want to define classes, we could do something like:
" 	let g:vtags_dummy_class_regexp = 'class\s*\(\w\{-}\)\s*{'
" 	let g:vtags_dummy_class_tag = '\1\t%F\t:%N'
" 	let g:vtags_dummy_class_flags = ''  " optional; see the possible flags
" 	let g:vtags_dummy_class_enable = 1  " optional; 1 by default
"
" Now, if we are in a buffer that has the filetype set to "dummy" and we run
" the tags parser, the generated output should contain all the classes defined
" in the file. For example, if we'd have a class named "blah" at line 24, by
" using the above tag definition, the generated tag for this class will be:
" 	blah <Tab> some_file.dum <Tab> :24

let g:vtags_lang = "php, vim"

" Default tag definitions.

let s:DefaultTag = '\1\t%F\t/^%L/'
let s:DefaultTag_Function = s:DefaultTag . ';"\tf'
let s:DefaultTag_Class = s:DefaultTag    . ';"\tc'
let s:DefaultTag_Variable = s:DefaultTag . ';"\tv\tvalue:\2\tcontext:%C'

" PHP language definition.
let g:vtags_php_obj = "variable, function, class"

let g:vtags_php_variable_regexp = '\c\%[var]\s*\$\(\w\{-}\)\s*=\s*\(.\{-}\)\s*;'
let g:vtags_php_variable_tag = s:DefaultTag_Variable
let g:vtags_php_variable_flags = 'c'
let g:vtags_php_variable_enable = 0
let g:vtags_php_function_regexp = '\c\<function\s\s*\(\w\{-}\)\s*(\([^)]\{-}\))\s*{'
let g:vtags_php_function_tag = s:DefaultTag_Function
let g:vtags_php_class_regexp = '\c\<class\s\s*\(\w\{-}\)\s*{'
let g:vtags_php_class_tag = s:DefaultTag_Class

" Vim language definition.
let g:vtags_vim_obj = "variable, function, syn"

let g:vtags_vim_variable_regexp = 'let\s\s*\([a-zA-Z_][a-zA-Z0-9_:]*\)\s*=\s*\(.\{-}\)\s*\%(|\|$\)'
let g:vtags_vim_variable_tag = s:DefaultTag_Variable
let g:vtags_vim_variable_flags = 'c'
let g:vtags_vim_variable_enable = 0
let g:vtags_vim_function_regexp = '\%(end\|".*\)\@<!\<fun\%[ction]!\{0,1}\s\s*\%([sgb]:\)*\([A-Z]\w\{-}\)(\(\%(\w\|,\|\s\)*\))'
let g:vtags_vim_function_tag = s:DefaultTag_Function
let g:vtags_vim_syn_regexp = 'sy\%[ntax]\s\s*\%(match\|region\|def\s\s*link\)\s\s*\(\w\{-}\)\s'
let g:vtags_vim_syn_tag = '\1\t%F\t/^%L/;"\ts'
let g:vtags_vim_syn_flags = 'c'
let g:vtags_vim_syn_enable = 0
