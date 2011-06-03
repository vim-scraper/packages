" Vim syntax file
" Language:	Jifty  Web Frame Application
" Maintainer:	  Lin Yo-An  <cornelius.howl+vim@gmail.com> 
" Last Change:	2009 Jan 25
"
" INSTALL:
" put jifty.vim to ~/.vim/after/syntax/jifty.vim
" append following lines to ~/.vim/syntax/perl.vim
"
" if version < 600 
"     so <sfile>:p:h/jifty.vim
" else
"     runtime! syntax/jifty.vim
" endif

syn keyword     jfmethods  current_user_can before_create after_create take_action

" template declare
syn keyword     jftdtag           span cell h1 h2 h3 h4 h5 h6 pre hyperlink form table row th thead tfoot script p label input ul li strong img 
syn keyword     jftdfunction      show outs outs_raw with get set page content form_submit 
syn keyword     jftdfunction      out url serial setup_session current_user handle_request form new_action new_action_from_request failed_actions succeeded_actions redirect_required
syn keyword     jftdfunction      webservices_redirect redirect caller tangent link render_region render_messages render_success_messages render_error_messages query_string escape escape_uri navigation
syn keyword     jftdfunction      page_navigation include_css add_css include_javascript add_javascript remove_javascript add_external_javascript
syn keyword     jftdfunction      clear_state_variables get_region region replace_current_region current_region qualified_region add_action register_action has_action start submit

syn cluster     jifty             contains=jftdtag,jftdfunction

" let template and div fold
"syn region      jfprivtemplate        start=+^private\s\+template+ end=+^};+ transparent fold
syn region      jftemplate        start=+^\(private\s\+\)\=template+ end=+^};+ transparent fold

syn match       jfscolname  "\v((param|column)\_[ \n]*)@<=\w+" contained
syn keyword     jfscol      param column      nextgroup=jfscolname contained
syn keyword     jfspropertybe     is are      nextgroup=jfsproperty contained
syn keyword     jfsproperty   type refers_to render_as valid hints default since label immutable mandatory
            \   nextgroup=jfspropis contained

syn region jfscolumn start=+^\s*\(param\|column\)\>+  end=+;+ contained contains=jfspropertybe,jfsproperty,jfscol,jfscolname,
            \ perlComment,perlString,perlFunction,perlFloat,perlNumber,perlSpecialString,perlStringUnexpanded fold

syn region jfms start=+schema[ \n]*{+ end=+}[\n ]*;+ contains=jfscolumn,perlComment fold

hi link jfscol        perlStatement
hi link jftdtag       perlStatement
hi link jftdfunction  perlMethod
hi link jfmethods     perlMethod

hi link jfscolname    perlType
hi link jfspropertybe perlIdentifier
hi link jfsproperty   perlIdentifier


