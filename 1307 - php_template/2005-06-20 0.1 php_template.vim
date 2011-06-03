" Vim filetype plugin file for adding getter/setter methods
" Language: PHP
" Maintainer: Sergio Carvalho <sergio.carvalho@portugalmail.com>
" Last Change: 2005 Jul 20
" Revision: 0.1
" Credit:
"    - Based on java_getset.vib by Pete Kazmier (pete-vim AT kazmier DOT com)
"
" -------------------------------------------------------------------------------
"   Copyright (c) 2005 Sergio Carvalho <sergio.carvalho@portugalmail.com>
" -------------------------------------------------------------------------------
"   php_getset.vim is free software; you can redistribute it and/or modify
"   it under the terms of the GNU Lesser General Public License as published by
"   the Free Software Foundation; either version 2.1 of the License, or
"   (at your option) any later version.
"
"   php_getset.vim is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU Lesser General Public License for more details.
"
"   You should have received a copy of the GNU Lesser General Public License
"   along with php_getset.vim; if not, write to the Free Software
"   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
"   02111-1307 USA
" -------------------------------------------------------------------------------
"   Author: Sergio Carvalho <sergio.carvalho@portugalmail.com>
" -------------------------------------------------------------------------------
"
" This filetype plugin provides a couple of code templates for PHP5
" development. The code is formatted according to PEAR Coding Standards.
"
" The three provided templates are:
"  - Class file template
"  - Method docblock template
"  - Field getter and setter template
"
" Install by placing the file in ~/.vim/ftplugin. To use:
" For the method docblock and getter/setter templates, just write the code as 
" usual. A mapping is triggered when you type ( after the method name, and
" when you type the ; after the field definition.
" For the class file template, type 'class' in normal mode. You'll be prompted
" for relevant information, and the class skeleton will be generated.
if exists("b:did_phpclass_plugin")
  finish
endif
let b:did_phpclass_plugin = 1

" Mappings
nmap class :call InsertClass()<CR>
imap ( (<ESC>:call TryMethodDocblock()<CR>a
imap ; ;<ESC>:call TryGetterSetter()<CR>a

" Make sure we are in vim mode
let s:save_cpo = &cpo
set cpo&vim

let s:indent = ''
let s:function_regexp = '\(public\|private\|protected\)\( static\)\? function \([^(]*\)('
"let s:field_regexp = '\(public\|private\|protected\)\( static\)\? \$\([a-zA-Z_][a-zA-Z0-9_]*\)\(.*=.*\);'

" Insert a method docblock template, iff no docblock exists for the method
" (and we are actually in a method line)
"
" This function inserts a docblock template above the current line. It will
" abort and do nothing if one of these is true:
"  - The current line is not a method declaration
"  - The line above the current one looks like the end of a docblock
if !exists("TryMethodDocblock")
    function TryMethodDocblock()
        if (match(getline(line('.')), s:function_regexp) == -1 || 
          \ (line('.') != 1 &&
          \  match(getline(line('.') - 1), '/\* }}} \*/') == -1 &&
          \  match(getline(line('.') - 1), '*/') != -1
          \ )
          \)
            return
        endif
        
        let function = substitute(getline(line('.')), s:function_regexp, '\3', 'g')

        let text = s:phptemplate_functiondocblock
        let text = substitute(text, '%function%', function, 'g')

        call s:append_text(line('.') - 1, text)
        call append(line('.'), '    /* }}} */')

    endfunction
endif

let s:field_regexp = '\( *\(public\|private\|protected\)\( static\)\? \$\([a-zA-Z_][a-zA-Z0-9_]*\)\(\( = \| =\|=\|= \)\(array\)\?.*\)\?;\)'
if !exists("TryGetterSetter")
    function TryGetterSetter()
        if (match(getline(line('.')), s:field_regexp) == -1 
          \)
            return
        endif

        let visibility = substitute(getline(line('.')), s:field_regexp, '\2', 'g')
        let static = substitute(getline(line('.')), s:field_regexp, '\3', 'g')
        let name = substitute(getline(line('.')), s:field_regexp, '\4', 'g')
        call append(line('.'), name)
        normal j^gUl
        let Name = getline(line('.'))
        normal ddk
        let defaultValue = substitute(getline(line('.')), s:field_regexp, '\5', 'g')
        let isArray = substitute(getline(line('.')), s:field_regexp, '\7', 'g') == 'array'

        if (isArray)
            let text = s:phptemplate_arraygettersetter
        else
            let text = s:phptemplate_gettersetter
        endif

        let text = substitute(text, '%visibility%', visibility, 'g')
        let text = substitute(text, '%static%', static, 'g')
        let text = substitute(text, '%name%', name, 'g')
        let text = substitute(text, '%Name%', Name, 'g')
        let text = substitute(text, '%defaultValue%', defaultValue, 'g')
        if (static == '')
            let text = substitute(text, '%thisOrSelf%', '$this->', 'g')
        else
            let text = substitute(text, '%thisOrSelf%', 'self::', 'g')
        endif

        call s:append_text(line('.') - 1, text)
        call append(line('.'), '    ')
        normal ddkzcj
    endfunction
endif

if !exists("InsertClass")
    function InsertClass() range
        let text = s:phptemplate_class

        let text = s:parameterize_template(text, "Package name? ", '%package%')
        let text = s:parameterize_template(text, "Class name? ", '%classname%')
        if exists('g:php_author')
            let text = substitute(text, '%author%', g:php_author, 'g')
        else
            let text = s:parameterize_template(text, "Author name and email? ", '%author%')
        endif

        call s:append_text(line('.'), text)
        normal dd
        set expandtab tabstop=4 softtabstop=4 shiftwidth=4 foldmethod=marker foldlevel=0
    endfunction
endif

if !exists("*s:parameterize_template")
    function s:parameterize_template(template, prompt, marker)
        return substitute(a:template, a:marker, input(a:prompt), 'g')
    endfunction
endif

if !exists("*s:append_text")
  function s:append_text(pos, text)
    let pos = a:pos
    let string = a:text

    while 1
      let len = stridx(string, "\n")

      if len == -1
        call append(pos, s:indent . string)
        break
      endif

      call append(pos, s:indent . strpart(string, 0, len))

      let pos = pos + 1
      let string = strpart(string, len + 1)

    endwhile
  endfunction
endif

if exists("b:phptemplate_class")
  let s:phptemplate_class = b:phpclass_template
else
  let s:phptemplate_class =
    \ "<?php\n" .
    \ "/* LICENSE AGREEMENT. If folded, press za here to unfold and read license {{{ \n" .
    \ "   vim: set expandtab tabstop=4 softtabstop=4 shiftwidth=4 foldmethod=marker:\n" .
    \ "   -------------------------------------------------------------------------------\n" .
    \ "     Copyright (c) 2005 %author%\n" .
    \ "   -------------------------------------------------------------------------------\n" .
    \ "     This file is part of %package%.\n" .
    \ "\n" .
    \ "     XML_RPC is free software; you can redistribute it and/or modify\n" .
    \ "     it under the terms of the GNU Lesser General Public License as published by\n" .
    \ "     the Free Software Foundation; either version 2.1 of the License, or\n" .
    \ "     (at your option) any later version.\n" .
    \ "\n" .
    \ "     XML_RPC2 is distributed in the hope that it will be useful,\n" .
    \ "     but WITHOUT ANY WARRANTY; without even the implied warranty of\n" .
    \ "     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" .
    \ "     GNU Lesser General Public License for more details.\n" .
    \ "\n" .
    \ "     You should have received a copy of the GNU Lesser General Public License\n" .
    \ "     along with XML_RPC2; if not, write to the Free Software\n" .
    \ "     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA\n" .
    \ "     02111-1307 USA\n" .
    \ "   -------------------------------------------------------------------------------\n" .
    \ "     Author: %author%\n" .
    \ "   -------------------------------------------------------------------------------\n" .
    \ "}}} */      \n" .
    \ "/**\n" .
    \ " * @package %package%\n" .
    \ " */\n" .
    \ "/**\n" .
    \ " */\n" .
    \ "/* dependencies {{{ */\n" .
    \ "/* }}} */\n" .
    \ "/**\n" .
    \ " * %classname% Single line description\n" .
    \ " *\n" .
    \ " * %classname% long description\n" .
    \ " *\n" .
    \ " * @author %author%\n" .
    \ " * @package %package%\n" .
    \ " *\n" .
    \ " * TODO Fill out class phpdoc\n" .
    \ " */\n" .
    \ "class %classname%\n" .
    \ "{\n" .
    \ "    /* fields {{{ */\n" .
    \ "    /* }}} */\n" .
    \ "    /* Constructor {{{ */\n" .
    \ "    /**\n" .
    \ "     * Create a new %classname%\n" .
    \ "     *\n" .
    \ "     * @TODO Fill out constructor docblock\n" .
    \ "     */\n" .
    \ "    protected function __construct()\n" .
    \ "    {\n" .
    \ "    }\n" .
    \ "    /* }}} */\n" .
    \ "    /* Destructor {{{ */\n" .
    \ "    /**\n" .
    \ "     * Free resources held by %classname%\n" .
    \ "     */\n" .
    \ "    protected function __destruct()\n" .
    \ "    {\n" .
    \ "    }\n" .
    \ "    /* }}} */\n" .
    \ "}\n" .
    \ "?>"
endif

if exists("b:phptemplate_functiondocblock")
  let s:phptemplate_functiondocblock = b:phptemplate_functiondocblock
else
  let s:phptemplate_functiondocblock =
    \ "    /* %function% {{{ */\n" .
    \ "    /**\n" .
    \ "     * %function%. TODO Single line description\n" .
    \ "     *\n" .
    \ "     * TODO Multiline description\n" .
    \ "     *\n" .
    \ "     * @param type Description\n" .
    \ "     * @return type Description\n" .
    \ "     */"
endif

if exists("b:phptemplate_gettersetter")
  let s:phptemplate_gettersetter = b:phptemplate_gettersetter
else
  let s:phptemplate_gettersetter =
    \ "    /* %name% field {{{ */\n" .
    \ "    /** TODO: Field short description \n" .
    \ "     */\n" .
    \ "    protected%static% $_%name%%defaultValue%;\n" .
    \ "    /**\n" .
    \ "     * _%name% Getter\n" .
    \ "     *\n" .
    \ "     * @TODO Change return type\n" .
    \ "     * @return mixed _%name% current value\n" .
    \ "     */\n" .
    \ "    %visibility%%static% function get%Name%()\n" .
    \ "    {\n" .
    \ "        return %thisOrSelf%_%name%;\n" .
    \ "    }\n" .
    \ "    /**\n" .
    \ "     * _%name% Setter\n" .
    \ "     *\n" .
    \ "     * @TODO Change param type\n" .
    \ "     * @param mixed _%name% new value\n" .
    \ "     */\n" .
    \ "    %visibility%%static% function set%Name%($value)\n" .
    \ "    {\n" .
    \ "        %thisOrSelf%_%name% = $value;\n" .
    \ "    }\n" .
    \ "    /* }}} */" 
endif

if exists("b:phptemplate_arraygettersetter")
  let s:phptemplate_arraygettersetter = b:phptemplate_arraygettersetter
else
  let s:phptemplate_arraygettersetter = 
    \ "    /* %name% field {{{ */\n" .
    \ "    /** TODO: Field short description \n" .
    \ "     */\n" .
    \ "    protected%static% $_%name%%defaultValue%;\n" .
    \ "    /**\n" .
    \ "     * _%name% Getter\n" .
    \ "     *\n" .
    \ "     * @TODO Change return type\n" .
    \ "     * @return mixed _%name% current value\n" .
    \ "     */\n" .
    \ "    %visibility%%static% function get%Name%()\n" .
    \ "    {\n" .
    \ "        return %thisOrSelf%_%name%;\n" .
    \ "    }\n" .
    \ "    /**\n" .
    \ "     * _%name% Setter\n" .
    \ "     *\n" .
    \ "     * @TODO Change param type\n" .
    \ "     * @param mixed _%name% new value\n" .
    \ "     */\n" .
    \ "    %visibility%%static% function set%Name%($value)\n" .
    \ "    {\n" .
    \ "        %thisOrSelf%_%name% = $value;\n" .
    \ "    }\n" .
    \ "    /**\n" .
    \ "     * Add a new element to the _%name array.\n" .
    \ "     *\n" .
    \ "     * @TODO Pick one of the two signatures below\n" .
    \ "     *\n" .
    \ "     * @param mixed Key for new value insertion\n" .
    \ "     * @param mixed New value\n" .
    \ "     * or\n" .
    \ "     * @param mixed New value\n" .
    \ "     */\n" .
    \ "    %visibility%%static% function add%Name%($valueOrKey, $value = null)\n" .
    \ "    {\n" .
    \ "        if (is_null($value)) {\n" .
    \ "            %thisOrSelf%_%name%[] = $valueOrKey;\n" .
    \ "        } else {\n" .
    \ "            %thisOrSelf%_%name%[$valueOrKey] = $value;\n" .
    \ "        }\n" .
    \ "    }\n" .
    \ "    /* }}} */" 
endif


