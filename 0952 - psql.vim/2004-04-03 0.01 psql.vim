" Vim syntax file
" Language:	SQL, PLPGSQL (PostgreSQL)
" Maintainer:	Devdas Bhagat <devdas@dvb.homelinux.org>
" Derived from the syntax file for plsql maintained by 
" Paul Moore <gustav@morpheus.demon.co.uk>
" Last Change:	2004 Arpil 02

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" The SQL reserved words, defined as keywords.

syn keyword psqlSpecial  false null true

syn keyword psqlKeyword	access add as asc begin by check cluster column
syn keyword psqlKeyword	compress connect current cursor decimal default desc
syn keyword psqlKeyword	else elsif end exception exclusive file for from
syn keyword psqlKeyword	function group having identified if immediate increment
syn keyword psqlKeyword	index initial into is level loop maxextents mode modify
syn keyword psqlKeyword	nocompress nowait of offline on online start
syn keyword psqlKeyword	successful synonym table then to trigger uid
syn keyword psqlKeyword	unique user validate values view whenever
syn keyword psqlKeyword	where with option order pctfree privileges procedure
syn keyword psqlKeyword	public resource return row rowlabel rownum rows
syn keyword psqlKeyword	session share size type using
syn keyword psqlKeyword	declare constraint join
syn keyword psqlKeyword	inner outer cross natural full left right

syn keyword psqlOperator	not and or primary key
syn keyword psqlOperator	in any some all between exists
syn keyword psqlOperator	ilike like escape cast
syn keyword psqlOperator 	union intersect minus
syn keyword psqlOperator 	prior distinct references

syn keyword psqlStatement alter analyze audit comment commit create
syn keyword psqlStatement delete drop execute explain grant insert lock 
syn keyword psqlStatement rename revoke rollback savepoint select set
syn keyword psqlStatement truncate update

syn keyword psqlType	boolean char character date float integer long
syn keyword psqlType	mlslabel number raw rowid varchar varchar2 varray
syn keyword psqlType	int int4 int8 float4 float8 line interval lseq
syn keyword psqlType	smallint bigint serial bigserial inet cidr macaddr
syn keyword psqlType	bit varbit bool box bytea circle numeric decimal 
syn keyword psqlType	path point polygon text time timetz timestamp 
syn keyword psqlType	timestamptz real 

" Strings and characters:
syn region psqlString		start=+"+  skip=+\\\\\|\\"+  end=+"+
syn region psqlString		start=+'+  skip=+\\\\\|\\'+  end=+'+

" Numbers:
syn match psqlNumber		"-\=\<\d*\.\=[0-9_]\>"

" Comments:
syn region psqlComment    start="/\*"  end="\*/"
syn match psqlComment	"--.*"

syn sync ccomment psqlComment

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_psql_syn_inits")
  if version < 508
    let did_psql_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink psqlComment	Comment
  HiLink psqlKeyword	psqlSpecial
  HiLink psqlNumber	Number
  HiLink psqlOperator	psqlStatement
  HiLink psqlSpecial	Special
  HiLink psqlStatement	Statement
  HiLink psqlString	String
  HiLink psqlType	Type

  delcommand HiLink
endif

let b:current_syntax = "psql"

" vim: ts=8
