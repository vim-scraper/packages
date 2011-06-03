" $Id: jbosslog.vim,v 1.1 2002/09/17 16:10:00 rhiestan Exp $
" Vim syntax file
" Language:	jbosslog log file
" Maintainer:	Bob Hiestand <bob@hiestandfamily.org>
" Last Change:	$Date: 2002/09/17 16:10:00 $
" Remark: Add some color to log files produced by sysklogd.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword jbosslogError	error Error ERROR exception Exception
syn match	jbosslevel	/level=[^ ]*/	nextgroup=jbossclass skipwhite
syn match	jbossclass	/class=[a-zA-Z._0-9]*/	nextgroup=jbosslogger skipwhite
syn match	jbosslogger	/\S\+/	
syn match	jbosslogDate	/^[0-9][^l]*/	nextgroup=jbosslevel skipwhite

if !exists("did_jbosslog_syntax_inits")
  let did_jbosslog_syntax_inits = 1
  hi link jbosslogDate 	Comment
  hi link jbosslogger	Type
  hi link jbosslogError 	Error
  hi link jbosslevel 	String
  hi link jbossclass 	String
endif

let b:current_syntax="jbosslog"
