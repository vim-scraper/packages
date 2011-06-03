if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  " we define it here so that included files can test for it
  let main_syntax='anttestreport'
endif

sy region JUnitFold start="^\s*at\>" skip="^\s*\at\>" end="^\s*$" fold transparent contains=ALLBUT,JUnitFold
sy match JUnitError "Tests run: [0-9]\+, Failures: [0-9]\+, Errors: \([0-9]\+\)"
sy match JUnitError "no method"
sy match JUnitFail "Tests run: [0-9]\+, Failures: [0-9]\+, Errors: 0"
sy match JUnitSuccess "Tests run: [0-9]\+, Failures: 0, Errors: 0"
sy keyword JUnitKeyword Testcase Testsuite took sec  elapsed interface type
sy keyword JUnitFail FAILED
sy keyword JUnitError ERROR 
sy match JUnitSpecialChar "(\|)\|,\|:"
sy match JUnitNumber "\<[0-9]\+\(.[0-9]\+\)\?\>"
sy keyword JUnitOperator is in at
sy match JUnitMeaningless "\(Testcase:\s*\)\@<=test"
sy keyword JUnitMeaningless named defined
sy match JUnitClass "\(\([a-z]\w*\.\)\+\)\zs\<[A-Z][[:alnum:]$]*\>"

sy match JUnitMeaningless "Unknown Source"
sy match JUnitMeaningless "Native Method"

"JMock
sy keyword JMockKeyword invoked Allowed mockParameters
sy keyword JMockKeyword expected once  
sy keyword JMockFail Invoked 
sy keyword JMockOperator returns eq
sy match JMockFail "no match found"
sy match JMockFail "was not invoked"
sy match JMockSpecialChar "<\|>"
sy match JMockKeyword  "mock object"


hi JUnitFail guibg=NONE guifg=Red
hi JUnitError guibg=Red guifg=DarkBlue
hi JUnitSuccess	guibg=NONE guifg=Green
hi JUnitKeyword   guibg=NONE guifg=Green 
hi JUnitMeaningless guibg=NONE guifg=DarkGreen
hi JUnitClass guifg=#008080  
hi link JUnitNumber Number
hi link JUnitSpecialChar Function
hi link JUnitOperator Operator
hi link JMockKeyword JUnitKeyword
hi link JMockFail JUnitFail
hi link JMockOperator JUnitOperator
hi link JMockSpecialChar JUnitSpecialChar

