" mosalisp.vim - lisp interpreter
" Maintainer:   Yukihiro Nakadaira <yukihiro.nakadaira@gmail.com>
" License:      This file is placed in the public domain.
" Last Change:  2007-08-12
"
" Usage:
"   :source mosalisp.vim
"   :call mosalisp.repl()   " type (exit) or CTRL-C to exit
"
" Example:
"   :call mosalisp.repl()
"   > (define func (lambda () (display "hello, world")))
"   > (func)
"   hello, world
"   > (:call "append" 0 '("line1" "line2"))
"   => 0
"   > (:execute "new file.txt")
"   "file.txt" [New File]
"   > (let loop ((i 0))
"   >> (when (< i 3)
"   >>> (printf "%d" i)
"   >>> (loop (+ i 1))))
"   0
"   1
"   2
"
"   See mosalisp.init() function and trailing script for more
"   information.

let s:sfile = expand("<sfile>:p")

let s:lib = {}
let mosalisp = s:lib

function s:lib.repl()
  let save_more = &more
  set nomore
  let self.inbuf = []
  let self.read_nest = 1
  let self.getchar = self.getchar_input
  let self.scope = [self.top_env]
  let self.stack = [["op_loop", 1, self.NIL]]
  while self.stack[0][0] != "op_exit"
    let op = remove(self.stack, 0)
    try
      call self[op[0]](op)
    catch
      echohl Error
      echo "Exception from" v:throwpoint
      echo v:exception
      echohl None
      break
    endtry
  endwhile
  let &more = save_more
endfunction

function s:lib.load_str(str, ...)
  let self.inbuf = split(a:str, '\zs')
  let self.getchar = self.getchar_str
  let self.scope = [self.top_env]
  let self.stack = [["op_loop", 0, self.NIL]]
  while self.stack[0][0] != "op_exit"
    let op = remove(self.stack, 0)
    try
      call self[op[0]](op)
    catch
      echohl Error
      echo "Exception from" v:throwpoint
      echo v:exception
      echohl None
      break
    endtry
  endwhile
  let res = self.stack[0][1]
  return get(a:000, 0, 0) ? res : self.to_vimobj(res)
endfunction

function s:lib.load(fname, ...)
  return self.load_str(join(readfile(a:fname), "\n"), get(a:000, 0, 0))
endfunction

function s:lib.dump_env()
  for env in self.scope
    for name in sort(keys(env))
      let item = env[name]
      echo printf("%s [%s]", name, item.type)
    endfor
  endfor
endfunction

function s:lib.get_funcname(number)
  for name in keys(self)
    if type(self[name]) == type(function("tr"))
      let s = string(self[name])
      if s =~ printf("'%d'", a:number)
        return name
      endif
    endif
  endfor
endfunction

" {{{ read
function s:lib.read()
  call self.skip_blank()
  let c = self.peekchar()
  if c == "eof"
    return self.Undefined
  elseif c == '('
    let self.read_nest += 1
    let res = self.read_list()
    let self.read_nest -= 1
    return res
  elseif c == '"'
    return self.read_string()
  elseif c =~ '\d' || c =~ '[-+]' && get(self.inbuf, 1, "") =~ '\d'
    return self.read_number()
  elseif c == '#'
    return self.read_const()
  elseif c == "'" || c == '`'
    return self.read_quote()
  elseif c == ','
    return self.read_unquote()
  else
    return self.read_symbol()
  endif
endfunction

function s:lib.read_list()
  let res = []
  call self.getchar()
  call self.skip_blank()
  while self.peekchar() != ')'
    if self.peekchar() == "eof"
      throw "eof"
    elseif self.peekchar() == "."
      call self.getchar()
      call self.skip_blank()
      call add(res, self.read())
      call self.skip_blank()
      call self.getchar()  " skip ')'
      let lis = self.mk_list(res)
      let p = lis
      while 1
        if p.cdr.cdr == self.NIL
          let p.cdr = p.cdr.car
          break
        endif
        let p = p.cdr
      endwhile
      return lis
    endif
    call add(res, self.read())
    call self.skip_blank()
  endwhile
  call self.getchar()
  return self.mk_list(res)
endfunction

function s:lib.read_string()
  let res = self.getchar()
  while self.peekchar() != '"'
    if self.peekchar() == "eof"
      throw "eof"
    elseif self.peekchar() == '\'
      let res .= self.getchar() . self.getchar()
    else
      let res .= self.getchar()
    endif
  endwhile
  let res .= self.getchar()
  return self.mk_string(eval(res))
endfunction

function s:lib.read_number()
  let res = self.getchar()
  while self.peekchar() !~ 'eof\|[() \t\n]'
    let res .= self.getchar()
  endwhile
  return self.mk_number(eval(res))
endfunction

function s:lib.read_const()
  call self.getchar()
  let c = self.getchar()
  if c == 'f'
    return self.False
  elseif c == 't'
    return self.True
  elseif c == 'o'
    let res = ""
    while self.peekchar() =~ '\o'
      let res .= self.getchar()
    endwhile
    return self.mk_number(str2nr(re, 8))
  elseif c == 'd'
    let res = ""
    while self.peekchar() =~ '\d'
      let res .= self.getchar()
    endwhile
    return self.mk_number(str2nr(res, 10))
  elseif c == 'x'
    let res = ""
    while self.peekchar() =~ '\x'
      let res .= self.getchar()
    endwhile
    return self.mk_number(str2nr(res, 16))
  endif
endfunction

function s:lib.read_symbol()
  let res = ""
  while self.peekchar() !~ 'eof\|[() \t\n]'
    let res .= self.getchar()
  endwhile
  return self.mk_symbol(res)
endfunction

function s:lib.read_quote()
  let res = [self.mk_symbol(self.getchar() == "'" ? 'quote' : 'quasiquote')]
  call add(res, self.read())
  return self.mk_list(res)
endfunction

function s:lib.read_unquote()
  call self.getchar()
  if self.peekchar() == '@'
    call self.getchar()
    let res = [self.mk_symbol('unquote-splicing')]
  else
    let res = [self.mk_symbol('unquote')]
  endif
  call add(res, self.read())
  return self.mk_list(res)
endfunction

function s:lib.skip_comment()
  while self.getchar() !~ 'eof\|\n'
    " pass
  endwhile
endfunction

function s:lib.skip_blank()
  while self.peekchar() =~ '\_s'
    call self.getchar()
  endwhile
  if self.peekchar() == ';'
    call self.skip_comment()
    call self.skip_blank()
  endif
endfunction

function s:lib.peekchar()
  if empty(self.inbuf)
    let c = self.getchar()
    call self.putchar(c)
  else
    let c = self.inbuf[0]
  endif
  return c
endfunction

function s:lib.getchar_input()
  if self.inbuf == []
    let prefix = repeat(">", self.read_nest) . " "
    try
      let str = input(prefix)
    catch /Vim:Interrupt/
      let self.inbuf = ["eof"]
      return "eof"
    endtry
    echon printf("\r%s%s", prefix, str)
    let self.inbuf = split(str, '\zs') + ["\n"]
    return self.getchar_input()
  endif
  if self.inbuf[0] == "eof"
    return "eof"
  endif
  return remove(self.inbuf, 0)
endfunction

function s:lib.getchar_str()
  if self.inbuf == []
    return "eof"
  endif
  return remove(self.inbuf, 0)
endfunction

function s:lib.putchar(c)
  if a:c != "eof"
    call insert(self.inbuf, a:c)
  endif
endfunction
" }}}

" {{{ eval

function s:lib.mk_symbol(str)
  if !has_key(self.symbol_table, a:str)
    let self.symbol_table[a:str] = {"type":"symbol", "val":a:str}
  endif
  return self.symbol_table[a:str]
endfunction

function s:lib.mk_number(num)
  return {"type":"number", "val":a:num}
endfunction

function s:lib.mk_string(str)
  return {"type":"string", "val":a:str}
endfunction

function s:lib.mk_hash(hash)
  return {"type":"hash", "val":a:hash}
endfunction

function s:lib.mk_vim_function(func)
  return {"type":"procedure", "val":"f_vim_function", "func":a:func}
endfunction

function s:lib.mk_list(lst)
  let p = self.NIL
  for item in a:lst
    let p = self.cons(item, p)
  endfor
  return self.reverse(p)
endfunction

function s:lib.mk_closure(code)
  return {
        \ "type": "closure",
        \ "val": "f_closure",
        \ "scope": copy(self.scope),
        \ "code": copy(a:code)
        \ }
endfunction

function s:lib.mk_macro(code)
  return {
        \ "type": "macro",
        \ "val": "f_closure",
        \ "scope": copy(self.scope),
        \ "code": copy(a:code)
        \ }
endfunction

function s:lib.mk_continuation()
  return {
        \ "type": "continuation",
        \ "val": "f_continue",
        \ "scope": copy(self.scope),
        \ "stack": map(copy(self.stack), 'copy(v:val)')
        \ }
endfunction

function s:lib.cons(car, cdr)
  return {"type":"pair", "car":a:car, "cdr":a:cdr}
endfunction

function s:lib.reverse(cell)
  let p = self.NIL
  let x = a:cell
  while x.type == "pair"
    let p = self.cons(x.car, p)
    let x = x.cdr
  endwhile
  return p
endfunction

function s:lib.op_read(op)
  call add(self.stack[0], self.read())
endfunction

function s:lib.op_eval(op)
  let code = a:op[1]
  if code.type == "symbol"
    for env in self.scope
      if has_key(env, code.val)
        call add(self.stack[0], env[code.val])
        return
      endif
    endfor
    call self.error(printf("Unbounded Variable: %s", code.val))
  elseif code.type == "pair"
    call insert(self.stack, ["op_call", code, code.cdr])
    call insert(self.stack, ["op_eval", code.car])
  else
    call add(self.stack[0], code)
  endif
endfunction

function s:lib.op_print(op)
  let value = a:op[1]
  if value.type != "undefined"
    echo "=>" self.to_str(a:op[1])
  endif
  call add(self.stack[0], a:op[1])
endfunction

function s:lib.op_loop(op)
  let [do_print, ret] = a:op[1:]
  if self.peekchar() == "eof"
    call insert(self.stack, ["op_exit", ret])
    return
  endif
  call insert(self.stack, ["op_loop", do_print])
  if do_print
    call insert(self.stack, ["op_print"])
  endif
  call insert(self.stack, ["op_eval"])
  call insert(self.stack, ["op_read"])
endfunction

function s:lib.op_error(op)
  let args = a:op[1]
  echohl Error
  echo args.car.val
  echohl None
  call insert(self.stack, ["op_exit", self.NIL])
endfunction

function s:lib.op_call(op)
  let [orig, code, func] = a:op[1:]
  if func.type == "macro"
    call insert(self.stack, ["op_eval"])
    call insert(self.stack, ["op_macro_replace", orig])
    call insert(self.stack, ["op_apply", func, code])
  elseif func.type == "syntax"
    call insert(self.stack, ["op_apply", func, code])
  else
    if code == self.NIL
      call insert(self.stack, ["op_apply", func, self.NIL])
    else
      call insert(self.stack, ["op_apply", func])
      call insert(self.stack, ["op_args", code.cdr, self.NIL])
      call insert(self.stack, ["op_eval", code.car])
    endif
  endif
endfunction

function s:lib.op_args(op)
  let [code, args, arg] = a:op[1:]
  let args = self.cons(arg, args)
  if code == self.NIL
    call add(self.stack[0], self.reverse(args))
  else
    call insert(self.stack, ["op_args", code.cdr, args])
    call insert(self.stack, ["op_eval", code.car])
  endif
endfunction

function s:lib.op_apply(op)
  let [func, args] = a:op[1:]
  call self[func.val](func, args)
endfunction

function s:lib.op_macro_replace(op)
  let [orig, code] = a:op[1:]
  for key in keys(orig)
    unlet orig[key]
  endfor
  call extend(orig, code)
  call add(self.stack[0], code)
endfunction

function s:lib.op_macro_eval(op)
  let lst = a:op[1]
  let [macro, args] = [lst.car, lst.cdr]
  call self[macro.val](macro, args)
endfunction

function s:lib.op_return(op)
  let self.scope = a:op[1]
  call add(self.stack[0], a:op[2])
endfunction

function s:lib.op_define(op)
  call self.define(a:op[1].val, a:op[2])
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.op_set(op)
  let [name, value] = a:op[1:]
  for env in self.scope
    if has_key(env, name.val)
      let env[name.val] = value
      call add(self.stack[0], self.Undefined)
      return
    endif
  endfor
  call self.error(printf("Unbounded Variable: %s", name.val))
endfunction

function s:lib.op_if(op)
  let [t, f, cond] = a:op[1:]
  call insert(self.stack, ["op_eval", (cond != self.False) ? t : f])
endfunction

function s:lib.op_cond(op)
  let [code, expr, cond] = a:op[1:]
  if cond != self.False
    call self.begin(expr)
  else
    if code == self.NIL
      call add(self.stack[0], self.Undefined)
    elseif code.car.car.type == "symbol" && code.car.car.val == "else"
      call insert(self.stack, ["op_cond", self.NIL, code.car.cdr, self.True])
    else
      call insert(self.stack, ["op_cond", code.cdr, code.car.cdr])
      call insert(self.stack, ["op_eval", code.car.car])
    endif
  endif
endfunction

function s:lib.op_or(op)
  let [code, cond] = a:op[1:]
  if cond != self.False
    call add(self.stack[0], cond)
  elseif code == self.NIL
    call add(self.stack[0], cond)
  else
    call insert(self.stack, ["op_or", code.cdr])
    call insert(self.stack, ["op_eval", code.car])
  endif
endfunction

function s:lib.op_and(op)
  let [code, cond] = a:op[1:]
  if !(cond != self.False)    " (not cond)
    call add(self.stack[0], cond)
  elseif code == self.NIL
    call add(self.stack[0], cond)
  else
    call insert(self.stack, ["op_and", code.cdr])
    call insert(self.stack, ["op_eval", code.car])
  endif
endfunction

function s:lib.error(msg)
  let args = self.mk_list([self.mk_string(a:msg)])
  call insert(self.stack, ["op_error", args])
endfunction

function s:lib.define(name, obj)
  let self.scope[0][a:name] = a:obj
endfunction

function s:lib.begin(code)
  let p = self.reverse(a:code)
  while p.type == "pair"
    call insert(self.stack, ["op_eval", p.car])
    let p = p.cdr
  endwhile
endfunction

function s:lib.s_lambda(this, code)
  call add(self.stack[0], self.mk_closure(a:code))
endfunction

function s:lib.s_macro(this, code)
  call add(self.stack[0], self.mk_macro(a:code))
endfunction

function s:lib.s_quote(this, code)
  call add(self.stack[0], a:code.car)
endfunction

function s:lib.s_define(this, code)
  let code = a:code
  if code.car.type == "pair"
    call insert(self.stack, ["op_define", code.car.car,
          \ self.mk_closure(self.cons(code.car.cdr, code.cdr))])
  else
    call insert(self.stack, ["op_define", code.car])
    call insert(self.stack, ["op_eval", code.cdr.car])
  endif
endfunction

function s:lib.s_set(this, code)
  call insert(self.stack, ["op_set", a:code.car])
  call insert(self.stack, ["op_eval", a:code.cdr.car])
endfunction

function s:lib.s_if(this, code)
  call insert(self.stack, ["op_if", a:code.cdr.car,
        \ get(a:code.cdr.cdr, "car", self.Undefined)])
  call insert(self.stack, ["op_eval", a:code.car])
endfunction

function s:lib.s_cond(this, code)
  call insert(self.stack, ["op_cond", a:code, self.NIL, self.False])
endfunction

function s:lib.s_begin(this, code)
  if a:code == self.NIL
    call add(self.stack[0], self.Undefined)
  else
    call self.begin(a:code)
  endif
endfunction

function s:lib.s_or(this, code)
  call insert(self.stack, ["op_or", a:code, self.False])
endfunction

function s:lib.s_and(this, code)
  call insert(self.stack, ["op_and", a:code, self.True])
endfunction

function s:lib.f_error(this, args)
  " (error "message" obj ...)
  call insert(self.stack, ["op_error", a:args])
endfunction

function s:lib.f_exit(this, args)
  " (exit [exitcode])
  if a:args == self.NIL
    call insert(self.stack, ["op_exit", self.NIL])
  else
    call insert(self.stack, ["op_exit", a:args.car])
  endif
endfunction

function s:lib.f_load(this, args)
  " (load filename)
  let save = [self.inbuf, self.getchar, self.stack]
  call add(self.stack[0], self.load(self.to_vimobj(a:args.car), 1))
  let [self.inbuf, self.getchar, self.stack] = save
endfunction

function s:lib.f_eval(this, args)
  " (eval '(proc a b c))
  call insert(self.stack, ["op_eval", a:args.car])
endfunction

function s:lib.f_macro_eval(this, args)
  " (macro-eval '(macro a b c))
  let code = a:args.car
  call insert(self.stack, ["op_macro_eval"])
  call insert(self.stack, ["op_args", code.cdr, self.NIL])
  call insert(self.stack, ["op_eval", code.car])
endfunction

function s:lib.f_closure(this, args)
  let [this, args] = [a:this, a:args]
  if self.stack[0][0] != "op_return"
    call insert(self.stack, ["op_return", self.scope])
  endif
  let self.scope = [{}] + this.scope
  let p = this.code.car
  while p.type == "pair"
    call self.define(p.car.val, args.car)
    let [p, args] = [p.cdr, args.cdr]
  endwhile
  if p != self.NIL
    call self.define(p.val, args)
  endif
  call self.begin(this.code.cdr)
endfunction

function s:lib.f_call_cc(this, args)
  let cont = self.mk_continuation()
  call insert(self.stack, ["op_apply", a:args.car, self.cons(cont, self.NIL)])
endfunction

function s:lib.f_cons(this, args)
  call add(self.stack[0], self.cons(a:args.car, a:args.cdr.car))
endfunction

function s:lib.f_car(this, args)
  call add(self.stack[0], a:args.car.car)
endfunction

function s:lib.f_cdr(this, args)
  call add(self.stack[0], a:args.car.cdr)
endfunction

function s:lib.f_set_car(this, args)
  let a:args.car.car = a:args.cdr.car
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.f_set_cdr(this, args)
  let a:args.car.cdr = a:args.cdr.car
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.f_continue(this, args)
  let self.stack = map(copy(a:this.stack), 'copy(v:val)')
  let self.scope = copy(a:this.scope)
  call add(self.stack[0], (a:args == self.NIL) ? self.NIL : a:args.car)
endfunction

function s:lib.f_not(this, args)
  if a:args.car != self.False
    call add(self.stack[0], self.False)
  else
    call add(self.stack[0], self.True)
  endif
endfunction

function s:lib.f_cmp_T(this, args)
  let cmp = printf("let b = (lhs %s rhs)", a:this.op)
  let lhs = self.to_vimobj(a:args.car)
  let p = a:args.cdr
  while p.type == "pair"
    let rhs = self.to_vimobj(p.car)
    execute cmp
    if !b
      call add(self.stack[0], self.False)
      return
    endif
    let p = p.cdr
    let lhs = rhs
  endwhile
  call add(self.stack[0], self.True)
endfunction

function s:lib.f_cmp_ins_T(this, args)
  let cmp = printf("let b = (lhs %s rhs)", a:this.op)
  let lhs = a:args.car
  let p = a:args.cdr
  while p.type == "pair"
    let rhs = p.car
    execute cmp
    if !b
      call add(self.stack[0], self.False)
      return
    endif
    let p = p.cdr
    let lhs = rhs
  endwhile
  call add(self.stack[0], self.True)
endfunction

function s:lib.f_sum_T(this, args)
  let cmd = printf("let sum = sum %s val", a:this.op)
  if a:this.op == "-" && a:args.cdr == self.NIL
    let [sum, p] = [-self.to_vimobj(a:args.car), a:args.cdr]
  elseif a:this.op == "/" && a:args.cdr == self.NIL
    let [sum, p] = [1, a:args]
  else
    let [sum, p] = [self.to_vimobj(a:args.car), a:args.cdr]
  endif
  while p.type == "pair"
    let val = self.to_vimobj(p.car)
    execute cmd
    let p = p.cdr
  endwhile
  call add(self.stack[0], self.mk_number(sum))
endfunction

function s:lib.f_printf(this, args)
  let args = []
  let p = a:args
  while p.type == "pair"
    if p.car.type == "string"
      call add(args, p.car.val)
    else
      call add(args, self.to_str(p.car))
    endif
    let p = p.cdr
  endwhile
  if len(args) == 1
    echo args[0]
  else
    echo call("printf", args)
  endif
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.f_type(this, args)
  call add(self.stack[0], self.mk_string(a:args.car.type))
endfunction

function s:lib.f_vim_call(this, args)
  " (:call func . args)
  let [func; args] = self.to_vimobj(a:args)
  call add(self.stack[0], self.to_lispobj(call(func, args)))
endfunction

function s:lib.f_vim_execute(this, args)
  " (:execute expr)
  let [expr] = self.to_vimobj(a:args)
  execute expr
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.f_vim_let(this, args)
  " (:let name value)
  let [name, value] = self.to_vimobj(a:args)
  execute printf("let %s = value", name)
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.f_vim_function(this, args)
  " vim function wrapper
  let args = self.to_vimobj(a:args)
  let res = call(a:this.func, args)
  call add(self.stack[0], self.to_lispobj(res))
endfunction

function s:lib.f_hash_table_ref(this, args)
  " (hash-table-ref hash key)
  let [hash, key] = self.to_vimobj(a:args)
  let value = hash[key]
  call add(self.stack[0], self.to_lispobj(value))
endfunction

function s:lib.f_hash_table_set(this, args)
  " (hash-table-set! hash key value)
  let [hash, key, value] = self.to_vimobj(a:args)
  let hash[key] = value
  call add(self.stack[0], self.Undefined)
endfunction

function s:lib.to_str(obj)
  if a:obj.type == "undefined"       | return "#<undefined>"
  elseif a:obj.type == "NIL"         | return "()"
  elseif a:obj.type == "boolean"     | return (a:obj.val ? "#t" : "#f")
  elseif a:obj.type == "number"      | return string(a:obj.val)
  elseif a:obj.type == "string"      | return string(a:obj.val)
  elseif a:obj.type == "symbol"      | return a:obj.val
  elseif a:obj.type == "hash"        | return string(a:obj.val)
  elseif a:obj.type == "pair"        | return string(self.to_vimobj(a:obj))
  elseif a:obj.type == "closure"     | return "#<closure>"
  elseif a:obj.type == "continuation"| return "#<continuation>"
  elseif a:obj.type == "procedure"   | return "#<procedure>"
  elseif a:obj.type == "syntax"      | return "#<syntax>"
  elseif a:obj.type == "macro"       | return "#<macro>"
  endif
endfunction

function s:lib.to_vimobj(obj)
  if a:obj.type == "undefined"       | return a:obj.val
  elseif a:obj.type == "NIL"         | return a:obj.val
  elseif a:obj.type == "boolean"     | return a:obj.val
  elseif a:obj.type == "number"      | return a:obj.val
  elseif a:obj.type == "string"      | return a:obj.val
  elseif a:obj.type == "symbol"      | return a:obj.val
  elseif a:obj.type == "hash"        | return a:obj.val
  elseif a:obj.type == "pair"
    let res = []
    let p = a:obj
    while p.type == "pair"
      call add(res, self.to_vimobj(p.car))
      let p = p.cdr
    endwhile
    " TODO: How to tell whether object is pair or list?
    if p != self.NIL
      call add(res, self.to_vimobj(p))
    endif
    return res
  elseif a:obj.type == "closure"     | return a:obj
  elseif a:obj.type == "continuation"| return a:obj
  elseif a:obj.type == "procedure"   | return a:obj
  elseif a:obj.type == "syntax"      | return a:obj
  elseif a:obj.type == "macro"       | return a:obj
  endif
endfunction

function s:lib.to_lispobj(obj)
  if type(a:obj) == type(0)          | return self.mk_number(a:obj)
  elseif type(a:obj) == type("")     | return self.mk_string(a:obj)
  elseif type(a:obj) == type({})     | return self.mk_hash(a:obj)
  elseif type(a:obj) == type([])
    return self.mk_list(map(copy(a:obj), 'self.to_lispobj(v:val)'))
  elseif type(a:obj) == type(function("tr")) | return self.mk_vim_function(a:obj)
  endif
endfunction

" }}}

function s:lib.init()
  let self.inbuf = []
  let self.read_nest = 1
  let self.symbol_table = {}
  let self.top_env = {}

  " constant
  let self.Undefined = {"type":"undefined", "val":["#<undefined>"]}
  let self.NIL = {"type":"NIL", "val":[]}
  let self.False = {"type":"boolean", "val":0}
  let self.True  = {"type":"boolean", "val":1}
  lockvar self.Undefined
  lockvar self.NIL
  lockvar self.False
  lockvar self.True

  " register
  let self.scope = [self.top_env]
  let self.stack = []

  call self.define("lambda", {"type":"syntax", "val":"s_lambda"})
  call self.define("macro" , {"type":"syntax", "val":"s_macro"})
  call self.define("quote" , {"type":"syntax", "val":"s_quote"})
  call self.define("define", {"type":"syntax", "val":"s_define"})
  call self.define("set!"  , {"type":"syntax", "val":"s_set"})
  call self.define("if"    , {"type":"syntax", "val":"s_if"})
  call self.define("cond"  , {"type":"syntax", "val":"s_cond"})
  call self.define("begin" , {"type":"syntax", "val":"s_begin"})
  call self.define("or"    , {"type":"syntax", "val":"s_or"})
  call self.define("and"   , {"type":"syntax", "val":"s_and"})

  call self.define("error" , {"type":"procedure", "val":"f_error"})
  call self.define("exit"  , {"type":"procedure", "val":"f_exit"})
  call self.define("load"  , {"type":"procedure", "val":"f_load"})
  call self.define("eval"  , {"type":"procedure", "val":"f_eval"})
  call self.define("macro-eval", {"type":"procedure", "val":"f_macro_eval"})
  call self.define("call-with-current-continuation", {"type":"procedure", "val":"f_call_cc"})
  call self.define("cons"  , {"type":"procedure", "val":"f_cons"})
  call self.define("car"   , {"type":"procedure", "val":"f_car"})
  call self.define("cdr"   , {"type":"procedure", "val":"f_cdr"})
  call self.define("set-car!", {"type":"procedure", "val":"f_set_car"})
  call self.define("set-cdr!", {"type":"procedure", "val":"f_set_cdr"})
  call self.define("not"   , {"type":"procedure", "val":"f_not"})
  call self.define("="     , {"type":"procedure", "val":"f_cmp_T", "op":"=="})
  call self.define("=="    , {"type":"procedure", "val":"f_cmp_T", "op":"=="})
  call self.define("!="    , {"type":"procedure", "val":"f_cmp_T", "op":"!="})
  call self.define(">"     , {"type":"procedure", "val":"f_cmp_T", "op":">"})
  call self.define(">="    , {"type":"procedure", "val":"f_cmp_T", "op":">="})
  call self.define("<"     , {"type":"procedure", "val":"f_cmp_T", "op":"<"})
  call self.define("<="    , {"type":"procedure", "val":"f_cmp_T", "op":"<="})
  call self.define("=~"    , {"type":"procedure", "val":"f_cmp_T", "op":"=~"})
  call self.define("!~"    , {"type":"procedure", "val":"f_cmp_T", "op":"!~"})
  call self.define("=#"    , {"type":"procedure", "val":"f_cmp_T", "op":"==#"})
  call self.define("==#"   , {"type":"procedure", "val":"f_cmp_T", "op":"==#"})
  call self.define("!=#"   , {"type":"procedure", "val":"f_cmp_T", "op":"!=#"})
  call self.define(">#"    , {"type":"procedure", "val":"f_cmp_T", "op":">#"})
  call self.define(">=#"   , {"type":"procedure", "val":"f_cmp_T", "op":">=#"})
  call self.define("<#"    , {"type":"procedure", "val":"f_cmp_T", "op":"<#"})
  call self.define("<=#"   , {"type":"procedure", "val":"f_cmp_T", "op":"<=#"})
  call self.define("=~#"   , {"type":"procedure", "val":"f_cmp_T", "op":"=~#"})
  call self.define("!~#"   , {"type":"procedure", "val":"f_cmp_T", "op":"!~#"})
  call self.define("=?"    , {"type":"procedure", "val":"f_cmp_T", "op":"==?"})
  call self.define("==?"   , {"type":"procedure", "val":"f_cmp_T", "op":"==?"})
  call self.define("!=?"   , {"type":"procedure", "val":"f_cmp_T", "op":"!=?"})
  call self.define(">?"    , {"type":"procedure", "val":"f_cmp_T", "op":">?"})
  call self.define(">=?"   , {"type":"procedure", "val":"f_cmp_T", "op":">=?"})
  call self.define("<?"    , {"type":"procedure", "val":"f_cmp_T", "op":"<?"})
  call self.define("<=?"   , {"type":"procedure", "val":"f_cmp_T", "op":"<=?"})
  call self.define("=~?"   , {"type":"procedure", "val":"f_cmp_T", "op":"=~?"})
  call self.define("!~?"   , {"type":"procedure", "val":"f_cmp_T", "op":"!~?"})
  call self.define("is"    , {"type":"procedure", "val":"f_cmp_ins_T", "op":"is"})
  call self.define("isnot" , {"type":"procedure", "val":"f_cmp_ins_T", "op":"isnot"})
  call self.define("+"     , {"type":"procedure", "val":"f_sum_T", "op":"+"})
  call self.define("-"     , {"type":"procedure", "val":"f_sum_T", "op":"-"})
  call self.define("*"     , {"type":"procedure", "val":"f_sum_T", "op":"*"})
  call self.define("/"     , {"type":"procedure", "val":"f_sum_T", "op":"/"})
  call self.define("%"     , {"type":"procedure", "val":"f_sum_T", "op":"%"})
  call self.define("printf", {"type":"procedure", "val":"f_printf"})
  call self.define("display", {"type":"procedure", "val":"f_printf"})

  call self.define("%type" , {"type":"procedure", "val":"f_type"})
  call self.define(":call" , {"type":"procedure", "val":"f_vim_call"})
  call self.define(":execute", {"type":"procedure", "val":"f_vim_execute"})
  call self.define(":let", {"type":"procedure", "val":"f_vim_let"})

  call self.define("hash-table-ref", {"type":"procedure", "val":"f_hash_table_ref"})
  call self.define("hash-table-set!", {"type":"procedure", "val":"f_hash_table_set"})

  " load init script
  echo "loading init script ..."
  let lines = readfile(s:sfile)
  let start = index(lines, "mzscheme <<EOF") + 1
  let end = index(lines, "EOF", start + 1) - 1
  call self.load_str(join(lines[start : end], "\n"))
  echo "done"
endfunction

call s:lib.init()

finish
mzscheme <<EOF
;; init script
;; "mzscheme <<EOF" is only used for highlighting.

(define (procedure? x) (=~ (%type x) "procedure\\|closure"))
(define (syntax? x)    (= (%type x) "syntax"))
(define (macro? x)     (= (%type x) "macro"))
(define (null? x)      (= (%type x) "NIL"))
(define (pair? x)      (= (%type x) "pair"))
(define (symbol? x)    (= (%type x) "symbol"))
(define (boolean? x)   (= (%type x) "boolean"))
(define (number? x)    (= (%type x) "number"))
(define (string? x)    (= (%type x) "string"))
(define (hash? x)      (= (%type x) "hash"))
(define (list? x)      (if (pair? x) (list? (cdr x)) (null? x)))
(define (zero? x)      (= x 0))
(define (positive? x)  (> x 0))
(define (negative? x)  (< x 0))
(define (odd? x)       (= 1 (% x 2)))
(define (even? x)      (= 0 (% x 2)))
(define (abs x)        (if (< x 0) (- x) x))
(define eq? is)
(define (eqv? x y)
  (if (and (or (number? x) (string? x))
           (or (number? y) (string? y)))
      (= x y)
      (eq? x y)))

(define let
  (macro code
    (if (list? (car code))
        ; (let ((x xi) (y yi) (z zi)) body ...)
        `((lambda ,(map car (car code))
            ,@(cdr code))
          ,@(map cadr (car code)))
        ; (let loop ((x xi) (y yi) (z zi)) body ...)
        `((lambda ()
            (define ,(cons (car code) (map car (cadr code)))
              ,@(cddr code))
            ,(cons (car code) (map cadr (cadr code))))))))

(define let*
  (macro code
    (define (make args body)
      (if (null? args)
          `(begin ,@body)
          `((lambda ,(list (caar args))
              ,(make (cdr args) body))
            ,@(cdar args))))
    (if (list? (car code))
        ; (let* ((x xi) (y yi) (z zi)) body ...)
        (make (car code) (cdr code))
        ; (let* loop ((x xi) (y yi) (z zi)) body ...)
        (make (cadr code) `((define ,(cons (car code) (map car (cadr code)))
                              ,@(cddr code))
                            ,(cons (car code) (map cadr (cadr code))))))))

(define letrec
  (macro code
    (if (list? (car code))
        ; (letrec ((x xi) (y yi) (z zi)) body ...)
        `((lambda ()
            ,@(map (lambda (arg) `(define ,(car arg) ,@(cdr arg))) (car code))
            ,@(cdr code)))
        ; (letrec loop ((x xi) (y yi) (z zi)) body ...)
        `((lambda ()
            ,@(map (lambda (arg) `(define ,(car arg) ,@(cdr arg))) (cadr code))
            (define ,(cons (car code) (map car (cadr code)))
              ,@(cddr code))
            ,(cons (car code) (map cadr (cadr code))))))))

(define (apply func . args)
  (let loop ((p '()) (args args))
    (if (null? (cdr args))
        (eval `(,func ,@(append (reverse p) (car args))))
        (loop (cons (car args) p) (cdr args)))))

(define (length lst)
  (let loop ((n 0) (p lst))
    (if (pair? p)
        (loop (+ n 1) (cdr p))
        n)))

(define (append . lst)
  (define (cat p q r)
    (if (null? p)
        (if (null? q)
            (reverse r)
            (cat (car q) (cdr q) r))
        (cat (cdr p) q (cons (car p) r))))
  (cat '() lst '()))

(define (reverse lst)
  (define (loop lst p)
    (if (pair? lst)
        (loop (cdr lst) (cons (car lst) p))
        p))
  (loop lst '()))

(define when
  (macro code
    `(if ,(car code)
       (begin
         ,@(cdr code)))))

(define unless
  (macro code
    `(if (not ,(car code))
       (begin
         ,@(cdr code)))))

;;;;; === copy from init.scm in minischeme ===
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define call/cc call-with-current-continuation)

(define (list . x) x)

(define (map proc list)
    (if (pair? list)
        (cons (proc (car list)) (map proc (cdr list)))
        '()))

(define (for-each proc list)
    (if (pair? list)
        (begin (proc (car list)) (for-each proc (cdr list)))
        #t ))

(define (list-tail x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1))))

(define (list-ref x k)
    (car (list-tail x k)))

(define (last-pair x)
    (if (pair? (cdr x))
        (last-pair (cdr x))
        x))

(define (head stream) (car stream))

(define (tail stream) (force (cdr stream)))

;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;; 
;; --- If you don't use macro or quasiquote, cut below. ---

(define quasiquote
 (macro (l)
   (define (mcons f l r)
     (if (and (pair? r)
              (eq? (car r) 'quote)
              (eq? (car (cdr r)) (cdr f))
              (pair? l)
              (eq? (car l) 'quote)
              (eq? (car (cdr l)) (car f)))
         (list 'quote f)
         (list 'cons l r)))
   (define (mappend f l r)
     (if (or (null? (cdr f))
             (and (pair? r)
                  (eq? (car r) 'quote)
                  (eq? (car (cdr r)) '())))
         l
         (list 'append l r)))
   (define (foo level form)
     (cond ((not (pair? form)) (list 'quote form))
           ((eq? 'quasiquote (car form))
            (mcons form ''quasiquote (foo (+ level 1) (cdr form))))
           (#t (if (zero? level)
                   (cond ((eq? (car form) 'unquote) (car (cdr form)))
                         ((eq? (car form) 'unquote-splicing)
                          (error "Unquote-splicing wasn't in a list:" 
                                 form))
                         ((and (pair? (car form)) 
                               (eq? (car (car form)) 'unquote-splicing))
                          (mappend form (car (cdr (car form))) 
                                   (foo level (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))
                   (cond ((eq? (car form) 'unquote) 
                          (mcons form ''unquote (foo (- level 1) 
                                                     (cdr form))))
                         ((eq? (car form) 'unquote-splicing)
                          (mcons form ''unquote-splicing
                                      (foo (- level 1) (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))))))
   (foo 0 l)))

;;;;; following part is written by a.k

;;;;	atom?
(define (atom? x)
  (not (pair? x)))

;;;;	memq
(define (memq obj lst)
  (cond
    ((null? lst) #f)
    ((eq? obj (car lst)) lst)
    (else (memq obj (cdr lst)))))

;;;;    equal?
(define (equal? x y)
  (if (pair? x)
    (and (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y)))
    (and (not (pair? y))
         (eqv? x y))))

;;;;	(do ((var init inc) ...) (endtest result ...) body ...)
;;
(define do
  (macro (vars endtest . body)
    (let ((do-loop '%do-loop))
      `(letrec ((,do-loop
                  (lambda ,(map (lambda (x)
                                  (if (pair? x) (car x) x))
                             `,vars)
                    (if ,(car endtest)
                      (begin ,@(cdr endtest))
                      (begin
                        ,@body
                        (,do-loop
                          ,@(map (lambda (x)
                                   (cond
                                     ((not (pair? x)) x)
                                     ((< (length x) 3) (car x))
                                     (else (car (cdr (cdr x))))))
                              `,vars)))))))
         (,do-loop
           ,@(map (lambda (x)
                    (if (and (pair? x) (cdr x))
                      (car (cdr x))
                      '()))
               `,vars))))))
;;;;; === end ===

;;;;; === test ===
(define (test1)
  (define (endless n) (printf "%d" n) (endless (+ n 1)))
  (endless 0))

(define (test2)
  (define (x n) (printf "x: %d" n) (y (+ n 1)))
  (define (y n) (printf "y: %d" n) (z (+ n 1)))
  (define (z n) (printf "z: %d" n) (x (+ n 1)))
  (x 0))

(define (test3)
  (define cc #f)
  (define n (call/cc (lambda (k) (set! cc k) 0)))
  (if (< n 10) (begin (printf "%d" n) (cc (+ n 1)))))

(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (str->hex str)
  (define format
    (macro args
      `(:call "printf" ,@args)))
  (define (str-len str) (:call "strlen" str))
  (define (str-ref str n) (:call "strpart" str n 1))
  (define (str-ref-hex str n)
    (format "%02X" (:call "char2nr" (str-ref str n))))
  (let loop ((i 0) (res '()))
    (if (>= i (str-len str))
      (:call "join" (reverse res) "")
      (loop (+ i 1) (cons (str-ref-hex str i) res)))))

EOF

" vim:set foldmethod=marker:
