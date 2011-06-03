" Title: gen_vimoptrc.vim : Generate vim option configuration file from option-window.
" File: gen_vimoptrc.vim
" Author: Omi Taku (mail@nanasi.jp)
" URL: http://nanasi.jp/
" Version: 0.5
" Last Modified: 2007/11/30
"
" Overview
" --------
" Generate "_vimoptrc" file that contains your all vim optin settings.
" New command ":GenVimOptrc", this script added, parse "option-window" and
" generate configuration file to your home directory.
" This script support your easily update configuration file.
"
" Installation
" ------------
" 1. Copy the autoresize.vim script to the $HOME/.vim/plugin or
"    the $HOME/vimfiles/plugin. Refer to the ':help add-plugin',
"    ':help add-global-plugin' and ':help runtimepath' topics for
"    more details about Vim plugins.
" 2. Restart Vim.
"
" Usage
" -----
" 1. Run ":options" command.
" 2. Update your vim options.
" 3. Run ":GenVimOptrc" command in "option-window",
"    this command generate "_gvimoptrc" in your $HOME directory.
" 4. Source "_gvimoptrc" in your gvimrc,
"    for example
"
"    :if has("gui_running")
"      :source $HOME/_gvimoptrc
"    :endif
"
" Configuration
" -------------
" 1. You want use ".gvimoptrc" instead of "_gvimoptrc",
"    set your _gvimrc this code.
"    
"    example.
"        :let g:GEN_VIMOPTRC_USE_DOT = 1
"
" Limitation
" ----------
" 1. Currently only GUI version of vim is supported.
"
" Note
" ----
" 1. Should see also ":help save-settings".
"
"
" loaded check
:if exists('loaded_gen_vimoptrc')
    :finish
:endif
:let loaded_gen_vimoptrc = 1

" use dot file or under bar file
:if !exists("g:GEN_VIMOPTRC_USE_DOT")
  :let g:GEN_VIMOPTRC_USE_DOT = 0
:endif

" command
:command! -nargs=0 GenVimOptrc :call s:GenVimOptrc()

" terminal only options
:let s:TERM_ONLY_OPT = [ "term", "ttytype", "tty", "ttybuiltin", "tbi", "notbi", "ttyfast", "notf", "tf", "weirdinvert", "nowiv", "wiv", "esckeys", "ek", "noek", "scrolljump", "sj", "ttyscroll", "tsl", "guicursor", "gcr", "title", "notitle", "titlelen", "titlestring", "titlestring", "titleold", "icon", "noicon", "iconstring", "restorescreen", "rs", "nors" ]
" gui only options
:let s:GUI_ONLY_OPT = []

" generate _vimoptrc
:function s:GenVimOptrc()
  " get filename
  :let s:filename=expand('%:t')

  " if not option-window then, return
  :if s:filename != "option-window"
    :return
  :endif

  " generate _vimoptrc
  " set redir
	:if has("gui_running")
    :if has("g:GEN_VIMOPTRC_USE_DOT")
      :exe "redir! > $HOME/.gvimoptrc"
    :else
      :exe "redir! > $HOME/_gvimoptrc"
    :endif
  :else
    :if has("g:GEN_VIMOPTRC_USE_DOT")
      :exe "redir! > $HOME/.vimoptrc"
    :else
      :exe "redir! > $HOME/_vimoptrc"
    :endif
  :endif

    " read file, and get option name and value
    :let s:lastnum = line("$")
    :let s:i = 0
    :while s:i < s:lastnum
      :let s:i = s:i + 1
      :let s:linevalue = getline(s:i)
      :let s:optconf = s:GetOptConf(s:linevalue)
			:if s:optconf != ""
      	:silent! echon s:optconf . "\n"
			:endif
    :endwhile

  " end
  :redir END

:endfunction

" return option name and value
:function s:GetOptConf(linevalue)
  :if match(s:linevalue, "^ \tset ") < 0
		:return ""
	:endif

  " get option name
  :if match(a:linevalue, "=") >= 0
    :let s:optname = substitute(s:linevalue, '^ \tset \([^=]*\)=.*', '\1', "")
  :else
    :let s:optname = substitute(s:linevalue, '^ \tset \(no\)\=\([a-z]*\).*', '\2', "")
  :endif

	" gui or terminal
	:if has("gui_running")
		:for item in s:TERM_ONLY_OPT
			:if item == s:optname
				:return ""
			:endif
		:endfor
	:else
		:for item in s:GUI_ONLY_OPT
			:if item == s:optname
				:return ""
			:endif
		:endfor
	:endif

  " get option value
  :if s:optname == "pt" && &pt =~ "\x80"
    :let s:optvalue = <SID>s:PTvalue()
  :else
    :exe ":let s:optvalue = substitute(&" . s:optname . ', "[ \\t\\\\\"|]", "\\\\\\0", "g")'
  :endif

  " return option name and value with :set
  :if match(a:linevalue, "=") >= 0 || (s:optvalue != "0" && s:optvalue != "1")
    :return ":set " . s:optname . "=" . s:optvalue
  :else
    :if s:optvalue
      :return ":set " . s:optname
    :else
      :return ":set no" . s:optname
    :endif
  :endif
:endfunction

" copy from optwin.vim
:function s:PTvalue()
  :redir @a
  :silent set pt
  :redir END
  :return substitute(@a, '[^=]*=\(.*\)', '\1', "")
:endfunction

" vim: set st=2 sts=2 ts=2 ft=vim expandtab nowrap :
