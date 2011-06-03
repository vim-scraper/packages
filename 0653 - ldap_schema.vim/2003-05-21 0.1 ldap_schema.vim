" Vim syntax file
" Language    ldap schmema definition language	
" Maintainer  Rostislav Matl <matl@bithill.net>
" Last Change	

" remove old syntax
syn clear
" be case sensitive
syn case match

syn match schemaKeyword "^attributetype"
syn match schemaKeyword "^objectclass"
syn match schemaKeyword "NAME"
syn match schemaKeyword "DESC"
syn match schemaKeyword "OBSOLETE"
syn match schemaKeyword "SUP"
syn match schemaKeyword "EQUALITY"
syn match schemaKeyword "ORDERING"
syn match schemaKeyword "SUBSTR"
syn match schemaKeyword "SYNTAX"
syn match schemaKeyword "SINGLE-VALUE"
syn match schemaKeyword "COLLECTIVE"
syn match schemaKeyword "NO-USER-MODIFICATION"
syn match schemaKeyword "USAGE"
syn match schemaKeyword "userApplications" "DirectotyOperation" "distributedOperation" "dSAO[eration"

syntax region String   start="'" end="'"
syntax match  Comment  "#.*$"

hi link schemaKeyword Statement
