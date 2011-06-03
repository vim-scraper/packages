XPTemplate priority=personal

let s:f = g:XPTfuncs()

XPTvar $TRUE          1
XPTvar $FALSE         0
XPTvar $NULL          NULL
XPTvar $UNDEFINED     NULL
XPTvar $VOID_LINE     /* void */;
XPTvar $BRif \n

XPTinclude
      \ _common/common
      \ _condition/lisp.like

XPT - " (+ x y)
(- `:_numbers:^)

XPT + " (+ x y)
(+ `:_numbers:^)

XPT * " (+ x y)
(* `:_numbers:^)

XPT / " (+ x y)
(/ `:_numbers:^)

XPT = " (= x y)
(= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT /= " (/= x y)
(/= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT < " (< x y)
(< `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT <= " (<= x y)
(<= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT > " (> x y)
(> `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT >= " (>= x y)
(>= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT abs " (abs number)
(abs `number^)

XPT and " (and boolean boolean)
(and `boolean-1^` `boolean-2^`:_booleans:^)

XPT append " (append list list)
(append `list^`:_lists:^)

XPT apply " (apply function args...)
(apply `function-object^` `:_args:^)

XPT aref " (aref array subscripts...)
(aref `array^ `subscript^` `subscript...{{^` `subscript^` `subscript...^`}}^)

XPT _args hidden
`...args^` `arg^` `...args^

XPT assoc " (assoc item alist)
(assoc `item^ `alist^` `:_key:^)

XPT atom " (atom object)
(atom `object^)

XPT _booleans hidden
` `...booleans^` `boolean^` `...booleans^

XPT boundp " (boundp symbol)
(boundp `symbol^)

XPT car " (car list)
(car `list^)

XPT cdr " (cdr list)
(cdr `list^)

XPT clear-output " (clear-output output-stream)
(clear-output` `output-stream^)

XPT cond " (cond (test result) ...)
(cond
  (`test^ `result^)` `...^
`  (`test^ `result^)` `...^)

XPT cons " (cons object object)
(cons `object-1^ `object-2^)

XPT consp " (consp object)
(consp `object^)

XPT copy-alist " (copy-alist alist)
(copy-alist `alist^)

XPT copy-list " (copy-list list)
(copy-list `list^)

XPT copy-tree " (copy-tree tree)
(copy-tree `tree^)

XPT defmacro " (defmacro name (args) body)
(defmacro `name^ (`arg^` `...arg^` `arg^` `...arg^)
`  "`documentation`"^
  `body^` `...body^
`  `body^` `...body^)

XPT defun " (defun name (args) body)
(defun `name^ (`arg^` `...arg^` `arg^` `...arg^)
`  "`documentation`"^
  `body^` `...body^
`  `body^` `...body^)

XPT defvar " (defvar name)
(defvar `name^` `value^)

XPT divide alias=/ " (/ x y)

XPT do " (do ..)
(do
  ((`var^ `init^` `stepper^)` `...more^
`   (`var^ `init^` `stepper^)` `...more^)
  (`end-test^` `...end_form^
`   `end-form^` `...end_form^
   `return-value^)` `...body^
`  `body^` `...body^)

XPT dolist " (dolist ...)
(dolist (`var^ `list-form^` `result-form^)
  `body^)

XPT dotimes " (dotimes ...)
(dotimes (`index-var^ `upper-limit-form^` `result-form^)
  `body^)

XPT eighth " (eighth list)
(eighth `list^)

XPT endp " (endp list)
(endp `list^)

XPT eq " (eq x y)
(eq `object-1^ `object-2^)

XPT equal " (equal x y)
(equal `object-1^ `object-2^)

XPT eql " (eql x y)
(eql `object-1^ `object-2^)

XPT eval " (eval form)
(eval `form^)

XPT fboundp " (fboundp symbol)
(fboundp `symbol^)

XPT fifth " (fifth list)
(fifth `list^)

XPT finish-output " (finish-output output-stream)
(finish-output` `output-stream^)

XPT first " (first list)
(first `list^)

XPT force-output " (force-output output-stream)
(force-output` `output-stream^)

XPT format " (format destination control-string args)
(format `destination^ "`control-string^"` `:_args:^)

XPT _forms hidden
`form^` `...form^
`form^` `...form^

XPT fourth " (fourth list)
(fourth `list^)

XPT fresh-line " (fresh-line object output-stream)
(fresh-line `output-stream^)

XPT funcall " (funcall functional-object args...)
(funcall `functional-object^` `:_args:^)

XPT functionp " (functionp object)
(functionp `object^)

XPT get " (get symbol indicator default)
(get `symbol^ `indicator^ `default^)

XPT if " (if .. then .. else)
(if `test^
  `then^
  `else^)

XPT intersection " (intersection set set)
(intersection `set-1^ `set-2^)

XPT _key hidden
`key...{{^ :key `function^`}}^

XPT lambda " (lambda (args) body)
(lambda (`arg^` `...arg^` `arg^` `...arg^)
`  `body^` `...body^
`  `body^` `...body^)

XPT last " (last list)
(last `list^)

XPT length " (length list)
(length `sequence^)

XPT _let_args hidden
`var^ `value^

XPT let " (let (var value) ...)
(let (`(`:_let_args:^)` `...another_var^
`      (`:_let_args:^)` `...another_var^)
  `form^` `...form^
  `form^` `...form^)

XPT letstar " (let* (var value) ...)
(let* (`(`:_let_args:^)` `...another_var^
`       (`:_let_args:^)` `...another_var^)
  `form^` `...form^
  `form^` `...form^)

XPT list " (list object ...)
(list `object^` `...object^` `object^` `...object^)

XPT listp " (listp object...)
(listp `object^)

XPT liststar " (list* object ...)
(list* `object^` `...object^` `object^` `...object^)

XPT _lists hidden
` `...list^` `list^` `...list^

XPT load " (load "filename")
(load "`file^")

XPT macroexpand " (macroexpand form env)
(macroexpand `form^` `env^)

XPT mapc " (mapc function args...)
(mapc `function^` `:_args:^)

XPT mapcan " (mapcan function args...)
(mapcan `function^` `:_args:^)

XPT mapcar " (mapcar function args...)
(mapcar `function^` `:_args:^)

XPT mapcon " (mapcon function args...)
(mapcon `function^` `:_args:^)

XPT mapl " (mapl function args...)
(mapl `function^` `:_args:^)

XPT maplist " (maplist function args...)
(maplist `function^` `:_args:^)

XPT member " (member element list)
(member `element^ `list^)

XPT minus alias=- " (- number number)

XPT multiply alias=* " (* number number)

XPT nconc " (nconc list list)
(nconc `list^ `list^)

XPT nintersection " (nintersection set set)
(nintersection `set-1^ `set-2^)

XPT ninth " (ninth list)
(ninth `list^)

XPT not " (not boolean)
(not `boolean^)

XPT nth " (nth number list)
(nth `number^ `list^)

XPT nthcdr " (nthcdr number list)
(nthcdr `number^ `list^)

XPT nreverse " (nreverse list)
(nreverse `list^)

XPT null " (null object)
(null `object^)

XPT numberp " (numberp object)
(numberp `object^)

XPT _numbers hidden
`number-1^ `number-2^` `...number^` `number^` `...number^

XPT nsublis " (nsublis alist tree)
(nsublis `alist^ `tree^)

XPT nsubst " (nsubst new old tree)
(nsubst `new^ `old^ `tree^)

XPT _objects hidden
` `...object^` `object^` `...object^

XPT _open_element_type hidden
 :element-type `element-type^
XSET element-type=:default

XPT _open_external_format hidden
 :external-format `file-format^
XSET file-format=:default

XPT _open_if_does_not_exist hidden
 :if-does-not-exist `_if_does_not_exist_action^
XSET _if_does_not_exist_action=ChooseStr( ':error', ':create', 'nil' )

XPT _open_if_exists hidden
 :if-exists `_if_exists_action^
XSET _if_exists_action=ChooseStr( ':error', ':new-version', ':rename', ':rename-and-delete', ':overwrite', ':append', ':supersede', 'nil' )

XPT _open_more hidden
` `element-type...{{^`:_open_element_type:^`}}^` `if-exists...{{^`:_open_if_exists:^`}}^` `if-does-not-exist...{{^`:_open_if_does_not_exist:^`}}^` `external-format...{{^`:_open_external_format:^`}}^

XPT open " (open ...)
(open `pathname^ :direction `direction^` `more...{{^`:_open_more:^`}}^)
XSET direction=ChooseStr( ':input', ':output', ':io', ':probe' )

XPT or " (or boolean boolean)
(or `boolean-1^` `boolean-2^`:_booleans:^)

XPT pairlis " (pairlis keys values alist)
(pairlis `keys^ `data^ `alist^)

XPT plus alias=+ " (+ x y)

XPT prin1 " (prin1 object output-stream)
(prin1 `object^` `output-stream^)

XPT princ " (princ object output-stream)
(princ `object^` `output-stream^)

XPT print " (print object output-stream)
(print `object^` `output-stream^)

XPT printbase " *PRINT-BASE*
*PRINT-BASE*

XPT printlevel " *PRINT-LEVEL*
*PRINT-LEVEL*

XPT printlength " *PRINT-LENGTH*
*PRINT-LENGTH*

XPT progn " (progn form ...)
(progn
  `form^`
  `...form^`
  `form^`
  `...form^)

XPT quote " (quote object)
(quote `object^)

XPT _read_options hidden
` `input-stream^` `eof-error-p^` `eof-value^` `recursive-p^

XPT read " (read ...)
(read` `options...{{^`:_read_options:^`}}^)
XSET direction=ChooseStr( ':input', ':output', ':io', ':probe' )

XPT readbase " *READ-BASE*
*READ-BASE*

XPT remprop " (remprop symbol indicator)
(remprop `symbol^ `indicator^)

XPT rest " (rest list)
(rest `list^)

XPT return " (return result)
(return `result^)

XPT reverse " (reverse list)
(reverse `list^)

XPT rplaca " (rplaca destination source)
(rplaca `destination^ `source^)

XPT rplacd " (rplacd destination source)
(rplacd `destination^ `source^)

XPT second " (second list)
(second `list^)

XPT set " (set symbol value)
(set `symbol^ `value^)

XPT setf " (setf place newvalue)
(setf `place^ `newvalue^)

XPT setq " (setq variable form)
(setq `:_var_form:^` `more...{{^ `Include:_var_form^` `more...^`}}^)

XPT seventh " (seventh list)
(seventh `list^)

XPT sixth " (sixth list)
(sixth `list^)

XPT stringp " (stringp object)
(stringp `object^)

XPT sublis " (sublis alist tree)
(sublis `alist^ `tree^)

XPT subst " (subst new old tree)
(subst `new^ `old^ `tree^)

XPT symbolfunction " (symbol-function symbol)
(symbol-function `symbol^)

XPT symbolp " (symbolp object)
(symbolp `object^)

XPT symbolvalue " (symbol-value symbol)
(symbol-value `symbol^)

XPT tenth " (tenth list)
(tenth `list^)

XPT terpri " (terpri object output-stream)
(terpri `output-stream^)

XPT third " (third list)
(third `list^)

XPT typep " (typep object)
(typep `object^)

XPT union " (union set set)
(union `set-1^ `set-2^)

XPT _var_form hidden
`variable^ `form^

XPT when " (when ...)
(when `test^
  `:_forms:^)

XPT zerop " (zerop object)
(zerop `object^)

