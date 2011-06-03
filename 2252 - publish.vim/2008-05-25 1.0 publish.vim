" Vim plugin
" Maintainer: Peter Odding <xolox@home.nl>
" Last Change: 2008/05/26
" URL: http://xolox.ath.cx/vim/plugin/publish.vim
" See Also: http://xolox.ath.cx/vim/plugin/openurl.vim

" Documentation {{{1

" Introduction: Vim has been my favorite and only editor for several years now.
" I've customized and written several syntax highlighting modes and everything
" is set up the way I like it. When I stumbled unto the :TOhtml command I
" realized Vim could be used to publish source-code on my website. This plugin
" makes it as easy as :update.

" Usage: When you've defined the global variables 'publish_pattern' and
" 'publish_directory' this plugin will automatically publish your buffers when
" you save them. The value of 'publish_pattern' is used as a pattern to search
" for within the first ten lines of the buffer. If it matches a line, the
" capture of the first subpattern will be appended to the filepath defined by
" 'publish_directory' to get the filepath to which the buffer should be
" published. When the directory 'publish_directory' does not exist you'll be
" promted whether you want to temporarily disable automatic publishing. You can
" do this manually as well by defining 'publish_automatic' to 0. Several
" globals can be used to change the way your buffers are published, they are
" the keys documented for the Publish() function prefixed with 'publish_'.

" Example: Take a look at the fourth line of this script. It starts with 'URL:'
" and contains the address where you probably found this script. When I save
" this script it's published to '/mnt/website/vim/plugin/publish.vim'. The
" directory '/mnt/website/' is a mount point for the root of my website
" http://xolox.ath.cx/. I have the following options in my vimrc:

"   let publish_pattern = 'URL:\s\+http://xolox.ath.cx/\(\S\+\)'
"   let publish_directory = '/mnt/website'

" Scripting: If you don't like the autocommand but want to publish Vim buffers
" from your own scripts you can use the function Publish(path [, options]).
" Several key/value pairs are supported in the 'options' dictionary:

" colors:  Colorscheme to use for publishing the buffer;
" title:   Unencoded content of the <title> element;
" header:  HTML to insert just before the end of <head>;
" linkify: Replace e-mail addresses with obfuscated hyper links.

" Note that this plugin overwrites existing files.

" }}}

if !exists('publish_automatic')
 let publish_automatic = 1
endif

augroup PluginPublish
 autocmd! BufWritePost * call s:AutoPublish()
augroup END

fun! s:AutoPublish() " {{{1
 if g:publish_automatic
  if exists('g:publish_pattern') && exists('g:publish_directory')
   let view = winsaveview()
   try
    call cursor(1, 1)
    let linenr = search(g:publish_pattern, 'c', 10)
    if !linenr | return | endif
    let path = get(matchlist(getline(linenr), g:publish_pattern), 1)
    if empty(path)
     let prompt = "The regex 'publish_pattern' didn't capture a path!"
     throw printf(prompt, g:publish_pattern)
    endif
    if !isdirectory(g:publish_directory)
     let prompt = "The publish directory doesn't exist! (%s)"
     throw printf(prompt, g:publish_directory)
    endif
    let options = { 'title': path }
    for key in ['colors', 'header', 'linkify']
     let global = 'g:publish_' . key
     if exists(global)
      let options[key] = eval(global)
     endif
    endfor
    let path = g:publish_directory . '/' . path
    echomsg 'Publishing to' fnamemodify(path, ':~:.')
    silent call Publish(path, options)
   catch
    let prompt = "An error occurred while publishing your buffer:"
    let prompt .= "\n\n " . v:exception . "\n at " . v:throwpoint
    let prompt .= "\n\nDisable automatic publishing for this session?"
    let g:publish_automatic = confirm(prompt, "&Yes\n&No") == 2
   finally
    call winrestview(view)
   endtry
  endif
 endif
endfun

fun! Publish(path, ...) " {{{1
 let options = a:0 && type(a:1) == type({}) ? a:1 : {}
 let restore = []
 try
  let directory = fnamemodify(a:path, ':h')
  if !isdirectory(directory) | call mkdir(directory, 'p') | endif
  if has_key(options, 'colors') && type(options.colors) == type('')
   if !exists('g:colors_name') || g:colors_name != options.colors
    if exists('g:colors_name')
     call add(restore, 'colorscheme ' . escape(g:colors_name, ' '))
     call add(restore, 'doautocmd ColorScheme')
    endif
    execute 'colorscheme' escape(options.colors, ' ')
    doautocmd ColorScheme
   endif
  endif
  if &modeline
   call add(restore, 'setglobal modeline')
   setglobal nomodeline
  endif
  runtime syntax/2html.vim
  call add(restore, 'bwipeout! ' . bufnr('%'))
  if has_key(options, 'title') && type(options.title) == type('')
   %s!<title>\zs.*\ze</title>!\=s:EscapeHTML(options.title)!e
  endif
  if has_key(options, 'header') && type(options.header) == type('')
   %s!\ze</head>!\=options.header!e
  endif
  if get(options, 'linkify', 1)
   %s/\v\l+(\.\l+)*\@\l+(\.\l+)+/\=s:ObfuscateEmail(submatch(0))/eg
  endif
  execute 'write!' escape(a:path, ' ')
  return 1
 finally
  for cmd in restore
   execute cmd
  endfor
 endtry
endfun

fun! s:ObfuscateEmail(email) " {{{1
 let label = s:ObfuscateHTML(a:email)
 let value = s:ObfuscateHTML('mailto:' . s:ObfuscateURL(a:email))
 return printf('<a href="%s">%s</a>', value, label)
endfun

fun! s:ObfuscateURL(s) " {{{1
 return join(map(split(a:s, '.\zs'), "printf('%%%x', char2nr(v:val))"), '')
endfun

fun! s:ObfuscateHTML(s) " {{{1
 return join(map(split(a:s, '.\zs'), "printf('&#%2i;', char2nr(v:val))"), '')
endfun

fun! s:EscapeHTML(s) " {{{1
 return substitute(a:s, '[<>&]', '\=printf("&#%2i;", char2nr(submatch(0)))', 'g')
endfun " }}}

" vim: sw=1 tw=90
