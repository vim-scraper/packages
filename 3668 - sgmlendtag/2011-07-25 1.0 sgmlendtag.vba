" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/sgml.vim	[[[1
151
" Author:  Eric Van Dewoestine
"
" License: {{{
"   Copyright (c) 2005 - 2011, Eric Van Dewoestine
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Eric Van Dewoestine nor the names of its
"     contributors may be used to endorse or promote products derived from
"     this software without specific prior written permission of
"     Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

" CompleteEndTag() {{{
" Function to complete an sgml end tag name.
" Ex. imap <buffer> <silent> / <c-r>=sgml#CompleteEndTag()<cr>
function sgml#CompleteEndTag()
  let line = getline('.')
  if line[col('.') - 2] == '<' && line[col('.') - 1] !~ '\w'
    let tag = s:GetStartTag(line('.'))
    if tag != ''
      let result = '/' . tag

      let col = col('.')
      let del = 1
      if line[col - 1] == '>'
        " handle quirks w/ delimitMate and my matchem plugin
        if exists('b:_l_delimitMate_buffer')
          if len(b:_l_delimitMate_buffer) && b:_l_delimitMate_buffer[-1] == '>'
            call remove(b:_l_delimitMate_buffer, -1)
            let del = 0
          endif
        endif
        if exists('b:matchemqueue')
          if len(b:matchemqueue) && b:matchemqueue[-1] == '>'
            call remove(b:matchemqueue, -1)
            let del = 0
          endif
        endif

        if del
          " delete the closing char and force adding a new one so that vim
          " reindents for us.
          let result .= "\<del>"
        else
          " handle match plugins (delimitMate, matchem) w/out breaking repeat
          call setline('.', line[:col - 2] . line[col - 0:])
        endif
      endif

      let result .= '>'
      return result
    endif
  endif
  return '/'
endfunction " }}}

" s:GetStartTag(line) {{{
function s:GetStartTag(line)
  let pairpos = searchpairpos('<\w', '', '</\w', 'bnW')
  if pairpos[0]
    " test if tag found is self closing
    if search('\%' . pairpos[0] . 'l\%' . pairpos[1] . 'c\_[^>]*/>', 'bcnW')
      let pos = getpos('.')
      call cursor(pairpos[0], pairpos[1])
      try
        return s:GetStartTag(a:line)
      finally
        call setpos('.', pos)
      endtry
    endif

    let line = getline(pairpos[0])
    let pos = getpos('.')
    call cursor(pairpos[0], pairpos[1])
    try
      let tags = s:ExtractTags(line)
      " place the cursor at the end of the line
      call cursor(line('.'), col('$'))
      for tag in reverse(tags)
        " find first non self closing tag searching backwards
        call search('<' . tag . '\>\([^>]\{-}[^/]\)\?>', 'b', line('.'))

        " see if the tag has a matching close tag
        let pairpos = searchpairpos('<' . tag . '\>', '', '</' . tag . '\>', 'nW')
        if !pairpos[0] || pairpos[0] > a:line
          return tag
        endif
      endfor
      call cursor(line('.'), 1)
      return s:GetStartTag(a:line)
    finally
      call setpos('.', pos)
    endtry
  endif
  return ''
endfunction " }}}

" s:ExtractTags() {{{
" Extracts a list of open tag names from the current line.
function s:ExtractTags(line)
  let line = a:line
  let tags = []
  while line =~ '<\w\+'
    let tag = substitute(line, '.\{-}<\([a-zA-Z0-9:_]\+\).*', '\1', '')
    if line !~ '<' . tag . '[^>]\{-}/>' && !s:IgnoreTag(tag)
      call add(tags, tag)
    endif
    let line = substitute(line, '.\{-}<' . tag . '\(.*\)', '\1', '')
  endwhile
  return tags
endfunction " }}}

" s:IgnoreTag(tag) {{{
" Determines if a tag should be ignored.
function s:IgnoreTag(tag)
  if exists('b:SgmlCompleteEndTagIgnore')
    for ignore in b:SgmlCompleteEndTagIgnore
      if a:tag == ignore
        return 1
      endif
    endfor
  endif
  return 0
endfunction " }}}

" vim:ft=vim:fdm=marker
doc/sgmlendtag.txt	[[[1
42
*sgmlendtag.txt*

-----------------------------------------------------------------------------
Sgml End Tag Completion                          *sgmlendtag* 

  Overview                           |sgmlendtag-overview|
  Configuration                      |sgmlendtag-configuration|

-----------------------------------------------------------------------------
Overview                                         *sgmlendtag-overview*

This plugin aims to provide a simple means to complete end tags in sgml
documents (xml, html, etc.). Unlike some other end tag completion plugins, the
end tag is not auto added when you finish typing the opening tag, but instead
is inserted only when you start typing the closing tag (when you hit the slash
in '</'), at which point the tag name and closing > are inserted
automatically. The intent of this approach is to prevent an interruption in
your flow of typing by not requiring you to ever have to navigate around text
added to the right of the cursor.


-----------------------------------------------------------------------------
Configuration                                    *sgmlendtag-configuration*

b:SgmlCompleteEndTagIgnore                       *b:SgmlCompleteEndTagIgnore*
  A buffer local variable that you can set in an ftplugin file etc, to define
  a list of tag names which should not be considered for end tag completion.
  By default, for html files only, this variable is set to ['br', 'input']
  since <br> and <input> are valid self contained tags that have not
  corresponding end tag.

Note: The mapping to complete end tags is defined in both an html and xml
ftplugin which requires you to have ftplugin support enabled
(|:filetype-plugin-on|). Should you edit a filetype that contains sgml tags
but does not source either the html nor the xml filetype plugins, you can
manually define the sgml end tag completion mapping by adding the following to
an ftplugin file for that filetype: >

  imap <buffer> <silent> / <c-r>=sgml#CompleteEndTag()<cr>
>

vim:tw=78:ft=help:norl:
ftplugin/html_sgmlendtag.vim	[[[1
44
" Author:  Eric Van Dewoestine
"
" License: {{{
"   Copyright (c) 2005 - 2011, Eric Van Dewoestine
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Eric Van Dewoestine nor the names of its
"     contributors may be used to endorse or promote products derived from
"     this software without specific prior written permission of
"     Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

if !exists('b:SgmlCompleteEndTagIgnore')
  let b:SgmlCompleteEndTagIgnore = ['br', 'input']
endif

imap <buffer> <silent> / <c-r>=sgml#CompleteEndTag()<cr>

" vim:ft=vim:fdm=marker
ftplugin/xml_sgmlendtag.vim	[[[1
40
" Author:  Eric Van Dewoestine
"
" License: {{{
"   Copyright (c) 2005 - 2011, Eric Van Dewoestine
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Eric Van Dewoestine nor the names of its
"     contributors may be used to endorse or promote products derived from
"     this software without specific prior written permission of
"     Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

imap <buffer> <silent> / <c-r>=sgml#CompleteEndTag()<cr>

" vim:ft=vim:fdm=marker
