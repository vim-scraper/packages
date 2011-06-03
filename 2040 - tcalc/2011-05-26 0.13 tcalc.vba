" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/tcalc.vim	[[[1
125
" tcalc.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2010-09-19.
" @Revision:    0.0.555

" call tlog#Log('Load: '. expand('<sfile>')) " vimtlib-sfile

if !exists('g:tcalc_initialize')
    " A string that will be read when first invoking |:TCalc|.
    " Define some abbreviations. Use 'ls' to see them.
    " :nodefault:
    " :read: let g:tcalc_initialize = '' "{{{2
    let g:tcalc_initialize = '
                \ :binom ( n:Numeric k:Numeric ) args n fak k fak n k - fak * / ;
                \ :fak ( Numeric ) args dup 1 > ( dup 1 - fak * ) ( . 1 ) ifelse ;
                \ :fib ( Numeric ) args dup 1 > ( dup 1 - fib swap 2 - fib + ) if ;
                \ :ld ( Numeric ) args log 2 log / ;
                \ :ln ( Numeric ) args log ;
                \ :logx ( number:Numeric base:Numeric ) args number log base log / ;
                \ :rev ( Numeric ) args 1 swap / ;
                \ :Z ( Numeric ) args Integer ;
                \ :Q ( Numeric ) args Rational ;
                \ :C ( Numeric ) args Complex ;
                \ '
    " \ :binom ( Numeric Numeric ) args copy1 fak rot2 dup fak rot2 - fak * / ;
    " \ :logx ( Numeric Numeric ) args swap log swap log / ;
endif

if !exists('g:tcalc_lines')
    " The height of the window. If negative, use fixed height.
    let g:tcalc_lines = 10 "{{{2
endif


if !exists('g:tcalc_dir')
    " The default directory where "source" finds files.
    let g:tcalc_dir = fnamemodify('~/.tcalc', ':p') "{{{2
endif


function! tcalc#Calculator(reset, initial_args) "{{{3
    " if a:full_screen
    "     edit __TCalc__
    " else
        split __TCalc__
    " end
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
    setlocal modifiable
    setlocal foldmethod=manual
    setlocal foldcolumn=0
    setlocal filetype=
    setlocal nowrap
    if g:tcalc_lines < 0
        exec 'resize '. (-g:tcalc_lines)
    endif
    if a:reset
        ruby TCalc::VIM.reset
    end
    ruby TCalc::VIM.repl(VIM::evaluate('a:initial_args'))
    call s:CloseDisplay()
    echo
endf


function! tcalc#Eval(initial_args) "{{{3
    ruby TCalc::VIM.evaluate(VIM::evaluate('a:initial_args'))
    echo
endf


function! s:CloseDisplay() "{{{3
    if winnr('$') == 1
        bdelete!
    else
        wincmd c
    endif
endf


function! s:PrintArray(lines, reversed, align) "{{{3
    norm! ggdG
    let arr = split(a:lines, '\n', 1)
    if !a:reversed
        let arr = reverse(arr)
    end
    let ilen = len(arr)
    let imax = len(ilen)
    let lines = map(range(ilen), 'printf("%0'. imax .'s: %s", ilen - v:val - 1, arr[v:val])')
    call append(0, lines)
    norm! Gdd
    if winnr('$') > 1 && g:tcalc_lines >= 0
        if a:align && g:tcalc_lines > 0
            let rs = min([g:tcalc_lines, ilen])
        else
            let rs = min([&lines, ilen])
        endif
        exec 'resize '. rs
    endif
    " let top = ilen - (g:tcalc_lines >= 0 ? g:tcalc_lines : &lines)
    norm! Gzb
    echo
    redraw
endf


function! s:DisplayStack(stack_lines) "{{{3
    return s:PrintArray(a:stack_lines, 1, 1)
endf


function! tcalc#Complete(ArgLead, CmdLine, CursorPos) "{{{3
    ruby <<EOR
    ids = TCalc::VIM.completion(VIM::evaluate('a:ArgLead'))
    VIM::command("return split(#{ids.join("\n").inspect}, '\n')")
EOR
endf

exec 'rubyfile '. expand('<sfile>:p:h:h') .'/ruby/tcalc.rb'

syntax/tcalc.vim	[[[1
70
" tcalc.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-11-27.
" @Last Change: 2009-02-15.
" @Revision:    0.0.45

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif
if version < 508
    command! -nargs=+ HiLink hi link <args>
else
    command! -nargs=+ HiLink hi def link <args>
endif


syntax keyword TCalcWords
            \ ls yank y define let rm unlet hex HEX oct bin
            \ dec print inspect float format dup d copy c pop p
            \ . del delete rot r swap s stack_empty? stack_size
            \ iqueue_empty? iqueue_size Array group g ungroup u
            \ Sequence seq map mmap plot if ifelse recapture do
            \ clear debug begin end Rational Complex Integer
            \ Matrix at args assert validate source require
            \ history p pp #
            \ % * ** + +@ - -@ / < <= <=> == === =~ > >= DIG E EPSILON I
            \ MANT_DIG MAX MAX_10_EXP MAX_EXP MIN MIN_10_EXP MIN_EXP PI
            \ RADIX ROUNDS Scalar Unify abs abs2 acos acos! acosh acosh!
            \ ancestors angle arg asin asin! asinh asinh! atan atan! atan2
            \ atan2! atanh atanh! between? ceil chr column column_size
            \ column_vectors compare_by compare_by_row_vectors conj
            \ conjugate cos cos! cosh cosh! covector denominator det
            \ determinant div divmod downto dup each2 eql? eqn? equal? erf
            \ erfc exp exp! extend finite? floor frexp frozen? gcd gcd2
            \ gcdlcm hypot im imag image infinite? inner_product integer?
            \ inv inverse inverse_from is_a? kind_of? lcm ldexp log log!
            \ log10 log10! minor modulo nan? next nil? nonzero? numerator
            \ polar power! power2 prec prec_f prec_i prime_division quo r
            \ rank real regular? remainder round row row_size row_vectors
            \ rsqrt sin sin! singular? sinh sinh! sqrt sqrt! square? step
            \ succ t tan tan! tanh tanh! times transpose truncate upto
            \ zero?

syntax match TCalcDefWord /\(:\w\+\ze\(\s\|$\)\|\(^\|\s\)\zs;\)/
syntax match TCalcOperations /[+*/%\<>=!^-]\+/
syntax match TCalcNumeric /[+-]\?\d\S*/
syntax match TCalcArgument /\w\+:\S\+/ contained containedin=TCalcBlock
syntax region TCalcString start=/"/ end=/"/ skip=/\\"/
syntax region TCalcComment start=/\/\*/ end=/\*\//

syntax region TCalcLambda matchgroup=PreProc start=/((/ end=/)/ transparent
syntax region TCalcBlock matchgroup=PreProc start=/(/ end=/)/ transparent
syntax region TCalcArray matchgroup=Delimiter start=/\[/ end=/\]/ transparent

HiLink TCalcDefWord Special
HiLink TCalcWords Statement
HiLink TCalcString Constant
HiLink TCalcOperations Statement
HiLink TCalcNumeric Constant
HiLink TCalcArgument Type
HiLink TCalcComment Comment


delcommand HiLink
let b:current_syntax = 'tcalc'

plugin/tcalc.vim	[[[1
157
" tcalc.vim -- A RPN calculator for vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2010-01-03.
" @Revision:    359
" GetLatestVimScripts: 2040 1 tcalc.vim
"
" TODO: Error checks for malformed input
" TODO: Pretty printing (of arrays)
" TODO: Doesn't work: [ 1 2 3 ] ( [ 'Numeric 'Numeric 'Numeric ] ) assert
" TODO: Integrate with some CAS package/library

if &cp || exists("loaded_tcalc") || !has('ruby')
    finish
endif
let loaded_tcalc = 13

let s:save_cpo = &cpo
set cpo&vim


" :display: TCalc[!]
" With !, reset stack & input queue.
command! -bang -nargs=* -bar TCalc call tcalc#Calculator(!empty('<bang>'), <q-args>)

" command! -nargs=* -bar TCalcEval call tcalc#Eval(<q-args>)


let &cpo = s:save_cpo
unlet s:save_cpo

finish
CHANGES:
0.1
- Initial release

0.2
- Arguments were not properly reverted: 12 4 / now yields 3.
- The input will be split into tokens, i.e. you can input "1 2 + <cr>" 
or "1<cr>2<cr>+<cr>". (Command-line completions doesn't work properly 
though.)
- The syntax has slightly changed: "CmdCount,Arg", eg, "y3,a"

0.3
- The swap count argument is increased by one (for conformance with the 
rot command).
- Shortcuts are now RPN expression (elements at the stack can be 
referred to by # (= top element) or #N).
- Removed g:tcalc_reverse_display
- Positions on the stack can be referred to by #N.
- rot works the other way round
- d, dup command
- clear command
- print, hex, HEX, oct, dec, bin, float, format commands
- Removed dependency on tlib
- Variables; ls, vars, let, =, rm commands
- Command line completion for variables and commands

0.4
- COUNT can be "#", in which case the top number on the stack will be 
used (e.g. "3 dup3" is the same as "3 3 dup#")
- Disabled vars, (, ) commands
- Variables are words
- New words can be defined in a forth-like manner ":NAME ... ;"
- Built-in commands get evaluated before any methods.
- Messages can be sent to objects on the stack by "#N,METHOD", e.g. "1 2 
g2 3 #1,<<" yields "[1,2,3]"
- The copyN, cN command now means: push a copy of element N.
- ( ... ) push unprocessed tokens as array
- recapture command (feed an array of unprocessed tokens to the input 
queue)
- if, ifelse commands
- delN, deleteN commands
- Can push strings ("foo bar")
- "Symbols" à la 'foo (actually a string)

0.5
- Minor fix: command regexp

0.6
- Included support for rational and complex numbers
- Included matrix support 
- Syntax for pushing arrays [ a b c ... ]
- New at method to select an item from array-like objects
- Removed shortcut variables.

0.7
- Comments: /* ... */
- New words:
    - assert: Display an error message if the stack doesn't match the 
    assertion.
    - validate: Like assert but push a boolean (the result of the check) 
    on the stack.
    - do: synonym for recapture.
    - source: load a file (see also g:tcalc_dir)
    - require: load a ruby library
    - p: print an object (doesn't do much, but prettyprint seems 
    to have problems)
    - history (useful when using tcalc as stand-alone calculator)
- tcalc.rb can now be used as stand-alone program.

0.8
- Named arguments: args is a synonym for assert but provides for named 
arguments.
- New words: Sequence/seq, map, mmap, plot (a simple ASCII function 
plotter), stack_size, stack_empty?, iqueue_size, iqueue_empty?
- Syntactic sugar for assignments: VALUE -> VAR
- Defined "Array" as a synonym for "group"
- "define" command as alternative to the forth-like syntax for defining 
words
- Dynamic binding of words/variables (the words "begin ... end" 
establish a new scope)
- The stack, the input queue, and the dictionary are accessible like 
words (__STACK__, __IQUEUE__, __WORDS__)
- TCalc and tcalc#Calculator take initial tokens as argument.
- TCalc! with [!] will reset the stack & input queue.
- Completion of partial commands
- Readline-support for CLI mode (--no-curses).
- Simple key handling for the curses-based frontend
- Non-VIM-versons save the history in ~/.tcalc/history.txt
- #VAR,METHOD has slightly changed.
- TCalc syntax file.
- FIX: Command line completion

0.9
- FIX: Curses frontend: Display error messages properly
- FIX: readline support.
- FIX: sort words on completion
- Distribute as zip

0.10
- rm,* ... Remove all words
- If g:tcalc_lines < 0, use fixed window height.
- VIM: use the tcalc window to display plots, lists etc.
- FIX: Nested words

0.11
- New words: all?, any?, array_*, and, or, !=
- Debugger (sort of)
- Curses frontend: Show possible completions; map 127 to backspace, F1 
to 'ls'.
- FIX: Nested blocks & more

0.12
- Force arrity for methods: METHOD@N -> pass the N top items on the 
stack as arguments to method

0.13
- Moved the definition of some variables from plugin/tcalc.vim to 
autoload/tcalc.vim


" - TCalcEval command that evaluates an expression and copies the result 
" to the unnamed "" register.

ftplugin/tcalc.vim	[[[1
15
" tcalc.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-11-29.
" @Last Change: 2009-02-15.
" @Revision:    0.1.4

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

setlocal commentstring=/*\ %s\ */

ruby/tcalc.rb	[[[1
1713
#!/usr/bin/env ruby
# tcalc.rb
# @Last Change: 2010-09-19.
# Author::      Tom Link (micathom AT gmail com)
# License::     GPL (see http://www.gnu.org/licenses/gpl.txt)
# Created::     2007-10-23.
#
# TODO:
# - reimplement the whole stuff in java or maybe d? ... or whatever
# - call this interpreter "whatever" :)


require 'matrix'
require 'mathn'
require 'optparse'
# require 'pp'
require 'continuation' if RUBY_VERSION >= '1.9.0'

module TCalc
    CONFIG_DIR = ENV['TCALC_HOME'] || File.join(ENV['HOME'], '.tcalc')

    module_function
    def lib_filename(filename)
        File.join(CONFIG_DIR, filename) if File.directory?(CONFIG_DIR)
    end
end


class TCalc::Base
    FLOAT_PERIOD = (1.1).to_s[1..1]
    attr_accessor :eval_and_exit

    def initialize(&block)
        @cmds = [
            'ls',
            'yank', 'y',
            'define', 'let', 'rm', 'unlet',
            'hex', 'HEX', 'oct', 'bin', 'dec', 'print', 'inspect', 'float', 'format',
            'dup', 'd',
            'copy', 'c',
            'pop', 'p', '.',
            'del', 'delete',
            'rot', 'r',
            'swap', 's',
            'stack_empty?', 'stack_size',
            'iqueue_empty?', 'iqueue_size',
            'Array', 'group', 'g',
            'ungroup', 'u',
            'Sequence', 'seq',
            'map', 'mmap', 'any?', 'all?',
            'array_push', 'array_pop', 'array_unshift', 'array_shift', 'array_empty?',
            'plot',
            'if', 'ifelse',
            'recapture', 'do',
            'clear',
            'debug',
            'begin', 'end',
            'Rational', 'Complex', 'Integer', 'Matrix',
            'at',
            'args', 'assert', 'validate',
            'source', 'require',
            'history', 'help',
            'p',
            # 'puts', 'pp',
            '#',
            '!=', 'and', 'or',
        ]
        @ymarks = ['+', '*', 'x', '.', '#', ':', '~', '^', '@', '$', 'o', '"']
        reset_words true
        reset
        @format  = '%p'
        @word_rx = '[[:alpha:]_]+'
        @debug         = $DEBUG
        @debug_breaks  = []
        @debug_status  = 'step'
        @buffer        = ''
        @history       = []
        @history_size  = 30
        @eval_and_exit = false
        @numclasses    = [Float, Complex, Rational, Integer, Matrix, Vector]
        setup
        instance_eval &block if block
    end


    def setup
        iqueue_reset initial_iqueue
    end



    def tokenize(string)
        string.scan(/("(\\"|[^"])*?"|\S+)+/).map {|a,b| a}
    end


    # spaghetti code ahead.
    def repl(initial=[])
        (iqueue).concat(initial) unless initial.empty?
        loop do
            if iqueue_empty?
                if @eval_and_exit
                    dump_stack
                    return
                else
                    display_stack
                    cmdi = read_buffered_input
                    break if quit?(cmdi)
                    @history.unshift(cmdi)
                    @history[@history_size..-1] = nil if @history.size > @history_size
                    iqueue_reset tokenize(cmdi)
                end
            end
            while !iqueue_empty?
                begin
                    cmd = iqueue_shift
                    # puts cmd if @debug
                    if !cmd.kind_of?(String)
                        stack_push cmd

                    elsif cmd == '(' or cmd == '(('
                        body  = []
                        case cmd
                        when '(('
                            depth = 1
                            args_to_be_set = true
                            body << 'begin' << '('
                        else
                            depth = 0
                            args_to_be_set = false
                        end
                        while !iqueue_empty?
                            elt = iqueue_shift
                            case elt
                            when '(('
                                depth += 2
                                body << elt
                            when '('
                                depth += 1
                                body << elt
                            when ')'
                                if depth == 0
                                    if cmd == '(('
                                        body << 'end'
                                    end
                                    break
                                else
                                    if depth == 1 and args_to_be_set
                                        args_to_be_set = false
                                        if body.last == '('
                                            body.pop
                                        else
                                            body << ')' << 'args'
                                        end
                                    else
                                        body << ')'
                                    end
                                    depth -= 1
                                end
                            else
                                body << elt
                            end
                        end
                        if depth == 0
                            stack_push body
                        else
                            echo_error 'Unmatched ('
                        end

                    elsif cmd == '['
                        stack_push '['

                    elsif cmd == ']'
                        start = stack.rindex('[')
                        if start
                            arr = stack_get(start + 1 .. -1)
                            stack_set start .. -1, nil
                            stack_push arr
                        else
                            echo_error 'Unmatched ]'
                        end

                    elsif cmd =~ /^[+-]?\d/
                        stack_push eval(cmd).to_f

                    elsif cmd =~ /^([+-])?\.(\d.*)$/
                        stack_push eval('%s0.%s' % [$1, $2]).to_f

                    elsif cmd =~ /^"(.*)"$/
                        stack_push eval(cmd)

                    elsif cmd =~ /^'(.*)$/
                        stack_push $1

                    elsif cmd =~ /^#(\d+)?$/
                        n = 1 + ($1 || 1).to_i
                        val = (stack).delete_at(-n)
                        stack_push val if val

                    elsif cmd =~ /^(#@word_rx)=$/
                        set_word($1, stack_pop)

                    # elsif cmd =~ /^:(\(.+?\))?(#@word_rx)$/
                    elsif cmd =~ /^:(#@word_rx)$/
                        idx = iqueue.index(';')
                        def_lambda($1, iqueue_get(0 .. idx - 1))
                        iqueue_set 0 .. idx, nil

                    elsif cmd == '->'
                        set_word iqueue_shift, stack_pop

                    elsif cmd == '/*'
                        idx = iqueue.index('*/')
                        iqueue_set 0 .. idx, nil

                    else
                        if @debug and (@debug_status == 'step' or
                                       (@debug_status == 'run' and @debug_breaks.any? {|b| cmd =~ b}))
                            loop do
                                dbgcmd = read_input('DEBUG (%s): ' % cmd)
                                case dbgcmd
                                when 's', 'step'
                                    @debug_status = 'step'

                                when 'r', 'run'
                                    @debug_status = 'run'

                                when 'b', 'break'
                                    bp = read_input('DEBUG Breakpoints (REGEXPs): ')
                                    bp.split(/\s+/).map do |b|
                                        @debug_breaks << Regexp.new(b)
                                    end

                                when 'B', 'unbreak'
                                    @debug_breaks = []

                                when 'ii', 'iqueue'
                                    print_array([iqueue.join(' ')])
                                    press_enter

                                when 'is', 'stack'
                                    print_array(stack)
                                    press_enter

                                when 'ls'
                                    list_words

                                when 'h', 'help'
                                    print_array([
                                                 's,  step   ... Step mode',
                                                 'r,  run    ... Run (until breakpoint is reached)',
                                                 'b,  break  ... Define breakpoints',
                                                 'B,  unbreak... Reset breakpoints',
                                                 'ii, iqueue ... Inspect input queue',
                                                 'is, stack  ... Inspect input queue',
                                                 'ls         ... List words',
                                                 'h,  help   ... Help',
                                                 'n, c, <CR> ... Continue',
                                            ], false, false)
                                    press_enter

                                when 'n', 'c'
                                    break

                                else
                                    break
                                end
                            end
                        end

                        cmdm = /^(#?[^@#,[:digit:]]*)(#|\d+)?(@(\d+))?(,(.+))?$/.match(cmd)
                        cmd_name   = cmdm[1]
                        cmd_count  = cmdm[2]
                        cmd_arrity = cmdm[4] ? cmdm[4].to_i : nil
                        cmd_ext    = cmdm[6]

                        if cmd_name =~ /^#(#@word_rx)$/
                            cmd_name = '#'
                            cmdw = $1
                        else
                            cmdw = nil
                        end

                        case cmd_count
                        when '#'
                            cmd_count = stack_pop.to_i

                        when nil, ''
                            cmd_count = 1

                        else
                            cmd_count = cmd_count.to_i
                        end

                        # p "DBG", cmd_name, cmd_count, cmd_ext
                        cmd_count.times do
                            if words.has_key?(cmd_name)
                                # p "DBG t2"
                                # p "DBG word"
                                if cmd_name =~ /^__.*?__$/
                                    iqueue_unshift(get_word(cmd_name).dup)
                                else
                                    iqueue_unshift(*get_word(cmd_name))
                                end

                            elsif @cmds.include?(cmd_name)
                                # p "DBG t1"
                                case cmd_name

                                when 'help'
                                    if help
                                        return
                                    end

                                when 'history'
                                    print_array(@history, false, false)
                                    n = read_input('Select entry: ')
                                    if n =~ /^\d+$/
                                        # iqueue_unshift(*tokenize(@history[n.to_i]))
                                        @buffer = @history[n.to_i]
                                    end

                                when 'source'
                                    filename = stack_pop
                                    if check_this(String, filename, cmd, cmd_name)
                                        unless File.exist?(filename)
                                            filename = lib_filename(filename)
                                        end
                                        if filename and File.exist?(filename)
                                            contents = File.read(filename)
                                            iqueue_unshift(*tokenize(contents))
                                        else
                                            echo_error 'source: File does not exist'
                                        end
                                    end
                                    break

                                when 'require'
                                    fname = stack_get(-1)
                                    require stack_pop if check_this(String, fname, [fname, cmd], cmd_name)
                                    #
                                # when 'puts'
                                #     puts stack_pop
                                #     press_enter

                                when 'p'
                                    p stack_pop
                                    press_enter

                                # when 'pp'
                                #     pp stack_pop
                                #     press_enter

                                when 'debug'
                                    @debug = cmd_count.to_i != 0
                                    if @debug
                                        @debug_status = cmd_ext || 'step'
                                    end

                                when 'assert'
                                    assertion = stack_pop
                                    unless check_assertion(assertion, [assertion, cmd])
                                        iqueue_reset
                                        break
                                    end

                                when 'args'
                                    assertion = stack_pop
                                    ok, names = descriptive_assertion(assertion, [assertion, cmd])
                                    names.compact.each do |name|
                                        # p "DBG", name
                                        set_word(name, stack_pop)
                                    end
                                    unless ok
                                        iqueue_reset
                                        break
                                    end

                                when 'validate'
                                    assertion = stack_pop
                                    stack_push check_assertion(assertion, [assertion, cmd], true)
                                    break

                                when 'ls'
                                    list_words

                                when 'yank', 'y'
                                    args = format(stack_get(-cmd_count .. -1))
                                    args = args.join("\n")
                                    export(cmd_ext, args)
                                    break

                                when 'define'
                                    name = stack_pop
                                    body = stack_pop
                                    def_lambda(name, body)

                                when 'let'
                                    set_word(cmd_ext, stack_pop)

                                when 'unlet', 'rm'
                                    case cmd_ext
                                    when '*'
                                        reset_words
                                    else
                                        words.delete(cmd_ext)
                                    end

                                when 'begin'
                                    scope_begin

                                when 'end'
                                    scope_end

                                when 'hex'
                                    @format = '%x'

                                when 'HEX'
                                    @format = '%X'

                                when 'oct'
                                    @format = '%o'

                                when 'bin'
                                    @format = '%016b'

                                when 'dec'
                                    @format = '%d'

                                when 'print', 'inspect'
                                    @format = '%p'

                                when 'float'
                                    @format = '%f'

                                when 'format'
                                    @format = cmd_ext

                                when 'copy', 'c'
                                    stack_push stack_get(-cmd_count - 1)
                                    break

                                when 'dup', 'd'
                                    stack_push stack_get(-1) unless stack_empty?

                                when 'del', 'delete'
                                    (stack).delete_at(-cmd_count - 1)
                                    break

                                when 'stack_empty?'
                                    stack_push stack_empty?
                                    break

                                when 'stack_size'
                                    stack_push stack.size
                                    break

                                when 'iqueue_empty?'
                                    stack_push iqueue_empty?
                                    break

                                when 'iqueue_size'
                                    stack_push iqueue.size
                                    break

                                when 'pop', 'p', '.'
                                    stack_pop

                                when 'rot', 'r'
                                    n = cmd_count + 1
                                    (stack).insert(-n, stack_pop)
                                    break

                                when 'swap', 's'
                                    n = cmd_count + 1
                                    val = stack_get(-n .. -1).reverse
                                    stack_set -n .. -1, val
                                    break

                                when 'g', 'group', 'Array'
                                    acc  = []
                                    rows = cmd_count
                                    rows.times {acc << stack_pop}
                                    stack_push acc.reverse
                                    break

                                when 'u', 'ungroup'
                                    # iqueue_unshift(*stack_pop)
                                    (stack).concat(stack_pop)

                                when 'recapture', 'do'
                                    block = stack_pop
                                    cmd_count.times {iqueue_unshift(*block)} if check_this(Array, block, [block, cmd], cmd_name)
                                    break

                                when 'clear'
                                    stack_reset
                                    break

                                when 'if'
                                    test, ifblock = stack_get(-2..-1)
                                    stack_set -2..-1, nil
                                    if test
                                        iqueue_unshift(*ifblock)
                                    end

                                when 'ifelse'
                                    test, ifblock, elseblock = stack_get(-3..-1)
                                    stack_set -3..-1, nil
                                    if test
                                        iqueue_unshift(*ifblock)
                                    else
                                        iqueue_unshift(*elseblock)
                                    end

                                when 'at'
                                    index = stack_pop
                                    item  = stack_pop
                                    case index
                                    when Array
                                        stack_push item[*index]
                                    else
                                        stack_push item[index]
                                    end

                                when 'map'
                                    fun  = stack_pop
                                    seq  = stack_pop
                                    iseq = seq.map do |xval|
                                        [xval, fun, '#1,<<']
                                    end
                                    stack_push []
                                    iqueue_unshift *(iseq.flatten)

                                when 'mmap'
                                    fun  = stack_pop
                                    seq  = stack_pop
                                    iseq = seq.map do |xval|
                                        ['[', xval, xval, fun, ']', '#1,<<']
                                    end
                                    stack_push []
                                    iqueue_unshift *(iseq.flatten)

                                when 'any?'
                                    fun  = stack_pop
                                    seq  = stack_pop
                                    check_this(Array, fun, [seq, fun, cmd], cmd_name)
                                    check_this(Array, seq, [seq, fun, cmd], cmd_name)
                                    iseq = [false]
                                    while !seq.empty?
                                        iseq.unshift(*[seq.pop, fun, '(', true, ')', '('].flatten)
                                        iseq.push(')', 'ifelse')
                                    end
                                    iqueue_unshift *(iseq.flatten)

                                when 'all?'
                                    fun  = stack_pop
                                    seq  = stack_pop
                                    check_this(Array, fun, [seq, fun, cmd], cmd_name)
                                    check_this(Array, seq, [seq, fun, cmd], cmd_name)
                                    iseq = [true]
                                    seq.each do |elt|
                                        iseq.unshift(*[elt, fun, '(', ].flatten)
                                        iseq.push(')', '(', false, ')', 'ifelse')
                                    end
                                    iqueue_unshift *(iseq.flatten)

                                when 'array_push'
                                    val = stack_pop
                                    arr = stack_get(-1)
                                    check_this(Array, arr, [arr, val, cmd], cmd_name)
                                    arr << val

                                when 'array_pop'
                                    arr = stack_get(-1)
                                    check_this(Array, arr, [arr, val, cmd], cmd_name)
                                    stack_push arr.pop

                                when 'array_unshift'
                                    val = stack_pop
                                    arr = stack_get(-1)
                                    check_this(Array, arr, [arr, val, cmd], cmd_name)
                                    arr.unshift(val)

                                when 'array_shift'
                                    arr = stack_get(-1)
                                    check_this(Array, arr, [arr, val, cmd], cmd_name)
                                    stack_push arr.shift

                                when 'array_empty?'
                                    arr = stack_get(-1)
                                    check_this(Array, arr, [arr, val, cmd], cmd_name)
                                    stack_push arr.empty?

                                when 'plot'
                                    xdim = stack_pop
                                    ydim = stack_pop
                                    vals = stack_pop
                                    plot(ydim, xdim, vals, cmd_ext)

                                when 'seq', 'Sequence'
                                    step = stack_pop
                                    top  = stack_pop
                                    idx  = stack_pop
                                    acc  = []
                                    while idx <= top
                                        acc << idx
                                        idx += step
                                    end
                                    stack_push acc

                                when 'Integer'
                                    stack_push stack_pop.to_i

                                when 'Rational'
                                    denominator = stack_pop
                                    numerator   = stack_pop
                                    stack_push Rational(numerator.to_i, denominator.to_i)

                                when 'Complex'
                                    imaginary = stack_pop
                                    real      = stack_pop
                                    if check_this(Numeric, imaginary, [imaginary, real, cmd], cmd_name) and check_this(Numeric, real, [imaginary, real, cmd], cmd_name)
                                        stack_push Complex(real, imaginary)
                                    end

                                when 'Matrix'
                                    mat = stack_get(-1)
                                    if check_this(Array, mat, [mat, cmd], cmd_name)
                                        stack_push Matrix[ *stack_pop ]
                                    end

                                when '!='
                                    stack_push (stack_pop != stack_pop)

                                when 'and'
                                    val1 = stack_pop
                                    val2 = stack_pop
                                    stack_push (val1 && val2)

                                when 'or'
                                    val1 = stack_pop
                                    val2 = stack_pop
                                    stack_push (val1 || val2)

                                when '#'
                                    if cmdw
                                        item = get_word(cmdw)
                                    else
                                        item = (stack).delete_at(-1 - cmd_count)
                                    end
                                    argn = cmd_arrity || item.method(cmd_ext).arity
                                    args = get_args(1, argn)
                                    val  = item.send(cmd_ext, *args)
                                    stack_push val
                                end

                            else
                                # p "DBG t4"
                                if RUBY_VERSION >= '1.9.0'
                                    cmd_method = cmd_name.intern
                                else
                                    cmd_method = cmd_name
                                end
                                catch(:continue) do
                                    @numclasses.each do |c|
                                        if c.instance_methods.include?(cmd_method)
                                            # p "DBG #{c}"
                                            argn = cmd_arrity || c.instance_method(cmd_method).arity
                                            args = get_args(0, argn)
                                            # p "DBG", cmd_method, argn, args
                                            val = [args[0].send(cmd_method, *args[1 .. -1])].flatten
                                            # p "DBG", val
                                            (stack).concat(val)
                                            throw :continue
                                        end
                                    end

                                    [Math, *@numclasses].each do |c|
                                        if c.constants.include?(cmd_method)
                                            # p "DBG #{c} constant"
                                             stack_push c.const_get(cmd_method)
                                             throw :continue
                                        end
                                    end

                                    [Math, *@numclasses].each do |c|
                                        if c.methods.include?(cmd_method)
                                            # p "DBG t3", cmd_method
                                            # p "DBG math"
                                            argn = cmd_arrity || c.method(cmd_method).arity
                                            args = get_args(1, argn)
                                            (stack).concat([c.send(cmd_method, *args)].flatten)
                                            throw :continue
                                        end
                                    end

                                    completion = complete_command(cmd_name, cmd_count, cmd_ext)
                                    if completion
                                        iqueue_unshift(completion)
                                    else
                                        echo_error "Unknown or ambiguous command: #{cmd_name}"
                                    end

                                end
                            end
                        end
                    end

                rescue Exception => e
                    if @debug
                        print_array([
                            '%s: %s' % [e.class, e.to_s], 
                            '__IQUEUE__',
                            iqueue.join(' '),
                            '__STACK__',
                            stack,
                            '__BACKTRACE__',
                            e.backtrace[0..5]].flatten, true, false)
                        press_enter
                        # raise e
                    elsif @eval_and_exit
                        echo_error '%s: %s' % [e.class, e.to_s]
                        exit 5
                    else
                        echo_error e.to_s.inspect
                    end

                end
            end
        end
        cleanup
    end


    def quit?(input)
        input.nil? || input =~ /^(bye|exit|quit|)$/
    end


    def cleanup
        puts 'Bye!'
    end


    def reset
        stack_reset
        iqueue_reset
        self
    end


    def reset_words(initial=false)
        new = {'__WORDS__' => nil}
        unless initial
            old = words
            words.each do |k, v|
                if k =~ /^__.*?__$/
                    new[k] = v
                end
            end
        end
        @words_stack = [new]
    end


    def list_words
        wd = words
        ls = wd.keys.sort.map do |key|
            next if key =~ /^_/
                val = wd[key]
            case val
            when Array
                val = val.join(' ')
            else
                val = val.inspect
            end
            "#{key}: #{val}"
        end
        print_array(ls.compact, true, false)
        press_enter
    end


    def words
        @words_stack[0]
    end


    def get_word(word)
        case word
        when '__WORDS__'
            rv = words.dup
            rv.delete_if {|k,v| k =~ /^__.*?__$/}
            [ rv ]
        else
            (words)[word]
        end
    end


    def set_word(word, value)
        # p "DBG set_word", word, value, words
        case word
        when '__WORDS__'
            w = value.dup
            @words_stack[0].each {|k,v| w[k] = v if k =~ /^__.*?__$/}
            @words_stack[0] = w
        else
            (words)[word] = value
        end
    end


    def def_lambda(word, body)
        word_def = ['begin', *body] << 'end'
        set_word(word, word_def)
    end


    def stack
        get_word '__STACK__'
    end


    def stack_reset(val=[])
        set_word '__STACK__', val
    end


    def stack_get(pos)
        (stack)[pos]
    end


    def stack_set(pos, arg)
        (stack)[pos] = arg
    end


    def stack_push(*args)
        (stack).push(*args)
    end


    def stack_pop
        (stack).pop
    end


    def stack_empty?
        (stack).empty?
    end


    def iqueue
        get_word '__IQUEUE__'
    end


    def iqueue_reset(val=[])
        set_word '__IQUEUE__', val
    end


    def iqueue_get(pos)
        (iqueue)[pos]
    end


    def iqueue_set(pos, arg)
        (iqueue)[pos] = arg
    end


    def iqueue_unshift(*args)
        (iqueue).unshift(*args)
    end


    def iqueue_shift
        (iqueue).shift
    end


    def iqueue_empty?
        # p "DBG", iqueue.class
        (iqueue).empty?
    end


    # This is probably the most expensive way to do this. Also, for 
    # objects like Array, Hash, this is likely to yield unexpected 
    # results.
    # This needs to be changed. For the moment it has to suffice though.
    def scope_begin
        @words_stack.unshift(words.dup)
    end


    def scope_end
        if @words_stack.size > 1
            @words_stack.shift
        else
            echo_error 'Scope error: end without begin'
        end
    end


    def help
        help_file = File.join(File.dirname(__FILE__), '..', 'doc', 'tcalc.txt')
        puts File.read(help_file)
        false
    end


    def plot(ydim, xdim, yvals, register)
        yyvals = yvals.map {|x,y| y}
        ymax   = yyvals.max
        ymin   = yyvals.min
        yrange = ymax - ymin
        xmin   = 0
        xmax   = 0
        xmarks = [nil] * xdim
        yscale = (ydim - 1) / yrange
        xscale = (xdim - 1) / (yvals.size - 1)
        canvas = Array.new(ydim) { ' ' * xdim }

        yvals.each_with_index do |xy, i|
            xpos = [[0, (i * xscale).round].max, xdim - 1].min
            xval, *yvals = xy
            if xval < xmin
                xmin = xval
            elsif xval > xmax
                xmax = xval
            end
            xmarks[xpos] = xval
            ylast = yvals.size - 1
            yvals.reverse.each_with_index do |y, yi|
                ypos = [ydim - 1, [0.0, (y - ymin) * yscale].max].min.round
                if yi == ylast
                    mark = xval.to_s.split(/[,.]/, 2)
                    mark = mark[1][0..0]
                else
                    mark = @ymarks[yi % @ymarks.size]
                end
                canvas[ypos][xpos] = mark
            end
        end

        ydiml = [ymin.round.to_s.size, ymax.round.to_s.size].max + 4
        ydim.to_i.times do |i|
            canvas[i].insert(0, "% #{ydiml}.2f: " % (ymin + i / yscale))
        end

        # canvas.unshift ''
        # xdiml = [('%.1f' % xmin).size, ('%.1f' % xmax).size].max
        xdiml = [xmin.to_i.abs.to_s.size, xmax.to_i.abs.to_s.size].max
        xlast = nil
        (0..xdiml).each do |i|
            row  = ' ' * xdim 
            (xdim.to_i).times do |j|
                xval0 = xmarks[j]
                if xval0
                    xvalt = xval0.truncate.abs
                    xsig  = xval0 >= 0 ? '+' : '-'
                    xval  = "#{xsig}%#{xdiml}d" % xvalt
                else
                    xval  = xlast
                end
                xch = xval[i..i]
                if j == 0 or j == xdim - 1
                    row[j] = xch
                elsif xsig == '+' and xlast != xval
                    row[j] = xch
                elsif xsig == '-' and xlast == xval and j > 2 and xvalt != 0
                    row[j - 1] = ' '
                    row[j] = xch
                end
                xlast = xval
            end
            canvas.unshift(' ' * (ydiml - 3) + '.    ' + row)
        end

        case register
        when nil, ''
            print_array(canvas, false, false)
            press_enter
        else
            export(register, canvas.reverse.join("\n"))
        end
    end


    def display_stack
        puts
        puts '--------------------------------'
        dstack = format(stack)
        idx  = dstack.size
        idxl = (idx - 1).to_s.size
        dstack.map! do |line|
            idx -= 1
            "%#{idxl}d: %s" % [idx, line]
        end
        puts dstack.join("\n")
    end


    def dump_stack
        dstack = format(stack)
        puts dstack.join(' ')
    end


    def read_buffered_input(*args)
        rv = read_input
        @buffer = ''
        return rv
    end


    def read_input(prompt='> ')
        print prompt
        STDIN.gets
    end


    def lib_filename(filename)
        TCalc.lib_filename(filename)
    end


    def initial_iqueue
        init = lib_filename('init.tca')
        if init and File.readable?(init)
            tokenize(File.read(init))
        else
            []
        end
    end


    def export(register, args)
        echo_error 'Export not supported'
    end


    def get_args(from, to)
        args = []
        for i in from..to
            args << stack_pop unless stack_empty?
        end
        args.reverse
    end


    def format(elt, level=1)
        case elt
        when Array
            elt = elt.map {|e| format(e, level + 1)}
            if level > 1
                '[%s]' % elt.join(', ')
            else
                beg = 0
                while elt[beg].nil? and beg < elt.size
                    beg += 1
                end
                elt[beg..-1]
            end
        when nil
            nil
        else
            sprintf(@format, elt)
        end
    end


    def check_assertion(assertion, cmd, quiet=false)
        ok, names = descriptive_assertion(assertion, cmd, quiet)
        return ok
    end


    def descriptive_assertion(assertion, cmd, quiet=false)
        names = []
        case assertion
        when Array
            ok = true
            assertion.reverse.each_with_index do |a, i|
                item = stack_get(-1 - i)
                ok1, name = check_item(a, item, cmd)
                names << name
                if ok and !ok1
                    ok = ok1
                end
            end
        else
            ok, name = check_item(assertion, (stack).last, cmd, quiet)
            names << name if name
        end
        return [ok, names]
    end


    def check_item(expected, observed, cmd, quiet=false)
        name = nil
        case expected
        when String
            if expected =~ /^(\w+):(.*)$/
                name = $1
                expected = $2
            end
            o = eval(expected)
        else
            o = expected
        end
        return [o ? check_this(o, observed, cmd, nil, quiet) : true, name]
    end


    def check_this(expected, observed, cmd, prefix=nil, quiet=false)
        case expected
        when Class
            ok = observed.kind_of?(expected)
        else
            ok = observed == expected
        end
        unless quiet
            unless ok
                echo_error "#{prefix || 'validate'}: Expected #{expected.to_s}, got #{observed.inspect}: #{cmd.inspect}"
            end
        end
        return ok
    end


    def print_array(arr, reversed=true, align=true)
        idxl = (arr.size - 1).to_s.size
        if reversed
            idx = -1
            arr.each do |e|
                idx += 1
                puts "%#{idxl}d: %s" % [idx, e]
            end
        else
            idx = arr.size
            arr.reverse.each do |e|
                idx -= 1
                puts "%#{idxl}d: %s" % [idx, e]
            end
        end
    end

    
    def press_enter
        puts '-- Press ANY KEY --'
        STDIN.getc
    end


    def echo_error(msg)
        puts msg
        sleep 1
    end


    def complete_command(cmd_name, cmd_count, cmd_ext, return_many = false)
        eligible = completion(cmd_name, return_many)
        if eligible.size == 1
            return eligible[0]
        elsif return_many
            return eligible
        else
            return nil
        end
    end


    def completion(alt, return_many = false)
        alx = Regexp.new("^#{Regexp.escape(alt)}.*")
        ids = @numclasses.map {|klass| klass.instance_methods | klass.constants}
        ids += Numeric.constants | Numeric.instance_methods | Math.methods | Math.constants | words.keys | @cmds
        ids.flatten!
        ids.uniq!
        ids = catch(:exit) do
            if return_many
                ids.map! {|a| a.to_s}
            else
                ids.map! do |a|
                    as = a.to_s
                    throw :exit, [as] if as == alt
                    as
                end
            end
            ids.sort! {|a,b| a <=> b}
            ids.delete_if {|e| e !~ alx}
            ids
        end 
        ids
    end
end



class TCalc::VIM < TCalc::Base
    # @tcalc = TCalc::VIM.new
    @tcalc = self.new

    class << self
        def get_tcalc
            # unless @tcalc
            #     # @tcalc = self.new
            #     @tcalc = TCalc::VIM.new
            # end
            @tcalc
        end

        def reset
            get_tcalc.reset.setup
        end

        def repl(initial_args)
            tcalc = get_tcalc
            args  = tcalc.tokenize(initial_args)
            tcalc.repl(args)
        end

        def evaluate(initial_args)
            tcalc = get_tcalc
            tcalc.eval_and_exit = true
            begin
                repl(initial_args)
            ensure
                tcalc.eval_and_exit = false
            end
        end

        def completion(alt)
            @tcalc.completion(alt)
        end
    end


    def setup
        iqueue_reset (initial_iqueue + tokenize(VIM::evaluate("g:tcalc_initialize")))
    end


    def quit?(input)
        input.empty? or input == "\n"
    end


    def cleanup
    end


    def help
        VIM::command(%{help tcalc})
        VIM::command(%{wincmd p})
        true
    end


    def display_stack
        dstack = format(stack).join("\n")
        # VIM::evaluate(%{s:DisplayStack(split(#{dstack.inspect}, '\n'))})
        VIM::evaluate(%{s:DisplayStack(#{dstack.inspect})})
    end


    def dump_stack
        dstack = format(stack)
        VIM::command(%{let @" = #{dstack.join(' ').inspect}})
    end


    def print_array(arr, reversed=true, align=true)
        lines = arr.join("\n")
        rev   = reversed ? 1 : 0
        align = align ? 1 : 0
        VIM::evaluate(%{s:PrintArray(#{lines.inspect}, #{rev}, #{align})})
        # VIM::command("echo | redraw")
        # super
    end


    def read_input(prompt='> ')
        VIM::evaluate("input(#{prompt.inspect}, #{@buffer.inspect}, 'customlist,tcalc#Complete')")
    end


    def lib_filename(filename)
        File.join(VIM::evaluate('g:tcalc_dir'), filename)
    end


    def export(register, args)
        VIM::command("let @#{register || '*'} = #{args.inspect}")
    end


    def press_enter
        VIM::command("echohl MoreMsg")
        VIM::command("echo '-- Press ANY KEY --'")
        VIM::command("echohl NONE")
        VIM::evaluate("getchar()")
        VIM::command("echo")
    end


    def echo_error(msg)
        VIM::command("echohl error")
        VIM::command("echom #{msg.inspect}")
        VIM::command("echohl NONE")
        VIM::command("sleep 1")
        # press_enter
    end

end


class TCalc::CommandLine < TCalc::Base
    @@readline = false

    class << self
        def use_readline(val)
            @@readline = val
        end
    end


    def setup
        trap('INT') do
            cleanup
            exit 1
        end

        history = lib_filename('history.txt')
        if history and File.readable?(history)
            @history = eval(File.read(history))
        end

        if @@readline
            Readline.completion_proc = proc do |string|
                completion(string)
            end
            def read_input(prompt='> ')
                Readline.readline(prompt, true)
            end
        end

        super
    end


    def cleanup
        super
        history = lib_filename('history.txt')
        if history
            File.open(history, 'w') do |io|
                io.puts @history.inspect
            end
        end
    end


    def press_enter
        # puts '--8<-----------------'
    end

end


class TCalc::Curses < TCalc::CommandLine
    class << self
        def use_readline(val)
            puts 'Input via readline is not yet supported for the curses frontend.'
            puts 'Patches are welcome.'
            exit 5
        end
    end


    def setup
        super
        require 'curses'
        @curses = Curses
        @curses.init_screen
        @curses.cbreak
        @curses.noecho
        if (@has_colors = @curses.has_colors?)
            @curses.start_color
            @curses.init_pair(1, @curses::COLOR_YELLOW, @curses::COLOR_RED);
        end
    end


    def cleanup
        @curses.close_screen
        super
    end


    def help
        help_file = File.join(File.dirname(__FILE__), '..', 'doc', 'tcalc.txt')
        help_text = File.readlines(help_file)
        print_array(help_text, false, false, true)
        press_enter
        false
    end

    def display_stack
        @curses.clear
        dstack = format(stack)
        print_array(dstack)
    end


    def print_array(arr, reversed=true, align=true, topdown=false)
        @curses.clear
        y0   = curses_lines - 3
        x0   = 3 + @curses.cols / 3
        arr  = arr.reverse if reversed
        idxs = (arr.size - 1).to_s.size
        idxf = "%0#{idxs}d:"
        xlim = @curses.cols - idxs
        xlin = xlim - x0
        arr.each_with_index do |e, i|
            ii = i % y0
            if i > 0 and ii == 0
                press_enter
            end
            y1 = topdown ? ii : (y0 - ii)
            @curses.setpos(y1, 0)
            @curses.addstr(idxf % i)
            if align
                period = e.rindex(FLOAT_PERIOD) || e.size
                @curses.setpos(y1, x0 - period)
                @curses.addstr(e[0..xlin])
            else
                @curses.setpos(y1, idxs + 2)
                @curses.addstr(e[0..xlim])
            end
        end
        @curses.setpos(y0 + 1, 0)
        @curses.addstr('-' * @curses.cols)
        @curses.refresh
    end


    def read_input(prompt='> ', index=0, string=@buffer)
        # @curses.setpos(@curses::lines - 1, 0)
        # @curses.addstr('> ' + string)
        # @curses.getstr
        histidx = -1
        curcol0 = prompt.size
        curcol  = string.size
        redraw_stack = false
        acc = []
        consume_char = nil
        debug_key = false
        loop do
            @curses.setpos(@curses::lines - 1, 0)
            @curses.addstr(prompt + string + ' ' * (@curses.cols - curcol - curcol0))
            @curses.setpos(@curses::lines - 1, curcol + curcol0)
            char = callcc do |cont|
                consume_char = cont
                @curses.getch
            end
            if redraw_stack
                display_stack
                redraw_stack = false
            end
            case char
            when 27
                acc = [27]
            when @curses::KEY_EOL, 10
                return string
            when @curses::KEY_BACKSPACE, 8, 127
                if curcol > 0
                    string[curcol - 1 .. curcol - 1] = ''
                    curcol -= 1
                end
            when @curses::KEY_CTRL_D, 4
                if curcol < string.size
                    string[curcol .. curcol] = ''
                end
            when @curses::KEY_CTRL_K, 11
                string[curcol..-1] = ''
                curcol = string.size
            when @curses::KEY_CTRL_W
                i = curcol
                while i > 0
                    i -= 1
                    if string[i..i] == ' '
                        break
                    end
                end
                string[i..curcol] = ''
                curcol = i
            when @curses::KEY_UP
                if histidx < (@history.size - 1)
                    histidx += 1
                    string = @history[histidx].dup
                    curcol = string.size
                end
            when @curses::KEY_DOWN
                if histidx > 0
                    histidx -= 1
                    string = @history[histidx].dup
                else
                    histidx = -1
                    string = ''
                end
                curcol = string.size
            when @curses::KEY_RIGHT
                curcol += 1 if curcol < string.size
            when @curses::KEY_LEFT
                curcol -= 1 if curcol > 0
            when @curses::KEY_CTRL_E, @curses::KEY_END
                curcol = string.size
            when @curses::KEY_CTRL_A, @curses::KEY_HOME
                curcol = 0
            when @curses::KEY_CTRL_I, 9
                s = string[0..curcol - 1]
                m = /\S+$/.match(s)
                if m
                    c0 = m[0]
                    cc = complete_command(c0, nil, nil, true)
                    # p "DBG", cc
                    case cc
                    when Array
                        print_array(cc.sort)
                        redraw_stack = true
                    when String
                        string = [s, cc[c0.size .. -1], string[curcol .. - 1]].join
                        curcol += cc.size - c0.size
                    else
                        echo_error('No completion: %s' % c0, 0.5)
                    end
                end
            else
                if acc.empty?
                    string.insert(curcol, '%c' % char)
                    curcol += 1
                elsif char
                    acc << Curses.keyname(char)
                    case acc
                    when [27, '[', 'A']
                        acc = []
                        consume_char.call @curses::KEY_UP
                    when [27, '[', 'B']
                        acc = []
                        consume_char.call @curses::KEY_DOWN
                    when [27, '[', 'C']
                        acc = []
                        consume_char.call @curses::KEY_RIGHT
                    when [27, '[', 'D']
                        acc = []
                        consume_char.call @curses::KEY_LEFT
                    when [27, '[', '7', '~']
                        acc = []
                        consume_char.call @curses::KEY_HOME
                    when [27, '[', '8', '~']
                        acc = []
                        consume_char.call @curses::KEY_END
                    when [27, '[', '1', '1', '~']
                        acc = []
                        string += ' ls'
                        consume_char.call @curses::KEY_EOL
                    when [27, '[', '2', '0', '~']
                        acc = []
                        debug_key = !debug_key
                    when [27, '[', '3', '~']
                        acc = []
                        consume_char.call @curses::KEY_CTRL_D
                    when [27, 'O', 'a'] # ctrl-up
                        acc = []
                    when [27, 'O', 'b'] # ctrl-down
                        acc = []
                    when [27, 'O', 'c'] # ctrl-right
                        acc = []
                        while curcol < string.size
                            if string[curcol..curcol] == ' '
                                break
                            end
                            curcol += 1
                        end
                    when [27, 'O', 'd'] # ctrl-left
                        acc = []
                        while curcol > 0
                            if string[curcol..curcol] == ' '
                                break
                            end
                            curcol -= 1
                        end
                    when [27, '[', '3', '^'] # ctrl-del
                        acc = []
                    when [27, '[', '5', '~'] # page-up
                        acc = []
                    when [27, '[', '6', '~'] # page-down
                        acc = []
                    else
                        if debug_key
                            string += acc.inspect
                            curcol = string.size
                        end
                    end
                end
            end
        end
    end


    def press_enter
        msg = '-- Press ANY KEY --'
        # @curses.setpos(@curses::lines - 1, @curses::cols - msg.size)
        @curses.setpos(@curses::lines - 1, 0)
        @curses.addstr(msg)
        @curses.getch
    end


    def echo_error(msg, secs=1)
        @curses.setpos(@curses::lines - 1, 0)
        if @has_colors
            @curses.attron(@curses.color_pair(1));
            @curses.attron(@curses::A_BOLD);
        end
        @curses.addstr(msg)
        if @has_colors
            @curses.attroff(@curses::A_BOLD);
            @curses.attroff(@curses.color_pair(1));
        end
        @curses.refresh
        sleep secs
    end

    def curses_lines
        @curses.lines
    end

end



if __FILE__ == $0
    $tcalculator = TCalc::Curses
    eval_and_exit  = false

    cfg = TCalc.lib_filename('config.rb')
    if cfg and File.readable?(cfg)
        load cfg
    end

    opts = OptionParser.new do |opts|
        opts.banner =  'Usage: tcalc [OPTIONS] [INITIAL INPUT]'
        opts.separator ''
        opts.separator 'tcalc is a free software with ABSOLUTELY NO WARRANTY under'
        opts.separator 'the terms of the GNU General Public License version 2 or newer.'
        opts.separator ''

        opts.separator 'General Options:'
        opts.on('-e', '--[no-]eval', 'Eval arguments and return (implies --no-curses)') do |bool|
            eval_and_exit = bool
            $tcalculator = TCalc::Base
        end

        opts.on('--[no-]curses', 'Use curses gui') do |bool|
            if bool
                $tcalculator = TCalc::Curses
            else
                $tcalculator = TCalc::CommandLine
            end
        end

        opts.on('--[no-]readline', 'Use readline') do |bool|
            if bool
                require 'readline'
            end
            $tcalculator.use_readline bool
        end

        opts.separator ''
        opts.separator 'Other Options:'

        opts.on('--debug', 'Show debug messages') do |v|
            $DEBUG   = true
            $VERBOSE = true
        end

        opts.on('-v', '--verbose', 'Run verbosely') do |v|
            $VERBOSE = true
        end

        opts.on_tail('-h', '--help', 'Show this message') do
            puts opts
            exit 1
        end
    end

    iqueue = opts.parse!(ARGV)
    tcalc = $tcalculator.new do
        @eval_and_exit = eval_and_exit
    end
    tcalc.repl(iqueue)
end


# Local Variables:
# revisionRx: REVISION\s\+=\s\+\'
# End:
doc/tcalc.txt	[[[1
500
*tcalc.txt*         A ruby-based RPN calculator for vim
                    Author: Tom Link, micathom at gmail com

It turns out that ruby is pretty good at maths and deals well with 
complex or rational numbers etc. Anyway, in certain occasions a postfix 
syntax and a stack are better suited for calculations than ruby's infix 
syntax. So, here comes ...

:TCalc[!]
    A small ruby-based[*] RPN-calculator, a simple ASCII function 
    plotter, and stack-based playground :-).

The tcalc.rb script can also be used as a stand-alone calculator (with 
optional support for curses and readline). Run "tcalc --help" for 
information on the command-line options.

Input:
    - Numbers (anything that starts with "-" or a decimal)
    - Strings (anything that matches /^"(.*?)"$/)
        - Be aware that the strings and numbers get evaluated by ruby. 
          You can thus execute ruby code by input like: -1;p'foo'
    - Methods & constants of the following classes are available:
        Float    :: http://www.ruby-doc.org/core/classes/Float.html
        Complex  :: http://www.ruby-doc.org/core/classes/Complex.html
        Rational :: http://www.ruby-doc.org/core/classes/Rational.html
        Integer  :: http://www.ruby-doc.org/core/classes/Integer.html
        Matrix   :: http://www.ruby-doc.org/core/classes/Matrix.html
        Vector   :: http://www.ruby-doc.org/core/classes/Vector.html
        Math     :: http://www.ruby-doc.org/core/classes/Math.html
    - #N (pull the item at position N to the top)
    - Call words: WORD[COUNT][@ARRITY][,ARGUMENT]
        - If COUNT is "#", the top element will be used.
    - Enter, escape => exit

Tokens are separated by blanks (à la forth). The blanks between the 
words thus are significant.


[*] For use with VIM, built-in ruby support (:echo has('ruby')) is 
required.


-----------------------------------------------------------------------
Install~                                            *tcalc-install*

As VIM-plugin:
Extract the archive's contents to ~/vimfiles (or ~/.vim).

Stand-alone ruby script:
Copy the file ruby/tcalc.rb to your path (you could also rename it to 
tcalc).


-----------------------------------------------------------------------
Startup~                                            *tcalc-startup*

At startup, two files are read:
    1. ~/.tcalc/config.rb ... A ruby file
    2. ~/.tcalc/init.tca  ... The initial input queue


-----------------------------------------------------------------------
Commands~                                           *tcalc-commands*

Every method/word/command may take a count as optional argument to 
repeat the command n times. E.g. "+3" will sum up the top 3 numbers, 
"y3" will copy the top 3 items in the "*" register.

The calculator has command-line completion enabled. But:
    - This only works if you input single tokens at a time, i.e. 
      "0.5<cr>sin<cr>".
    - Be aware that not every method is useful in the context of this 
      plugin.

                                                    *tcalc-words*
Words:
    VARIABLE=
    -> VARIABLE
        Set a variable (e.g. let,WORD) to the value of the top element, 
        the variable can be referenced by it name.
        EXAMPLE: >
            1 a=
            1 -> a
<
    :WORD ... ;
        Define a new word (the ending ";" token must be in the input 
        queue)
        EXAMPLE: >
            :fib dup 1 > ( dup 1 - fib swap 2 - fib + ) if ;
<
    ( ... ) 'WORD define
        The "define" command provides an alternative way to define new 
        words. In opposition to the above form, this form can  be used 
        to define word-local (i.e. nested) words.

        These two forms are mostly identical. The main difference is 
        that with the "define" command the definition and the name are 
        first put on the stack and could be manipulated or passed around 
        like any other data until the "define" words uses them to make 
        up a new word. In the above forth-like form (:WORD), the scanner 
        decides that a new word is to be defined and scans the input 
        quere for the closing ";".

    begin ... end
        "begin" creates a new scope (actually a new local dictionary) 
        with local variables and words. Variables/words are dynamically 
        bound (as in certain older lisp dialects). "end" re-establishes 
        the previous context, i.e. it removes any local words/variables.

    rm, unlet
        Remove a word (e.g. rm,WORD)

    ls
        List words. Words beginning with an underscore will be excluded 
        from the list as they are considered to be "internal" data 
        structures (see also below).

    The main difference between "variable" and "word" definitions is 
    that word bodies are enclosed in "begin ... end". The following 
    forms are equivalent: >

        ( begin 2 ** end ) square=
        ( begin 2 ** end ) -> square
        (( ) 2 ** ) -> square
        ( 2 ** ) 'square define
        :square 2 ** ;

        :square ( a:Numeric ) args a 2 ** ;
        (( a:Numeric ) a 2 ** ) -> square
< 
    The body ( ... ) is an array of tokens.

    Word names enclosed in underscores are treated slightly differently 
    on certain occassions. E.g. the stack and the input queue actually 
    are such special variables named "__STACK__" and "__IQUEUE__". The 
    current dictionary is accessible as "__WORDS__" (the special 
    variables are excluded though in order to avoid circular data 
    structures that make ruby ... well, crash). Use with care (if at 
    all).

    Example: >
        /* push some values onto the stack */
        3 2
        /* push a shallow copy of the stack onto the stack */
        __STACK__
        /* assign the stack to some variable */
        mystack=
        /* clear the stack */
        clear
        /* do something else */
        5 6 + a=
        /* get back the old stack */
        [ mystack ] __STACK__=
        /* simply push the values of the old stack */
        mystack


                                                    *tcalc-stack*
Stack (N is an optional numeric argument, which defaults to 1):
    pN, popN, .
        Pop/remove item(s)

    dN, dupN
        Duplicate the top item

    cN, copyN
        Push a copy of element N

    rN, rotN
        Rotate, push the top item to the back

    sN, swapN
        Reverse slice (actually rather a "mirror").
        
        The use of rot and dup might not be exactly what one would 
        expect from these commands.

    gN, groupN, ArrayN
        Group the top N elements on the stack as array
        EXAMPLE: >
            1 2 3 Array3
            /* equivalent to */
            1 2 3 3 Array#
            /* equivalent to */
            [ 1 2 3 ]
            => [1, 2, 3]
< 
    u, ungroup
        Push the elemnts of an array onto the stack (splice)

    clear
        Clear the stack


                                                    *tcalc-view*
View:
    hex, HEX, oct, bin, dec, float
        Change the way numbers are formatted. Calculations are always made 
        with floats.

    print, inspect
        Use ruby's inspect. (DEFAULT)

    format,%FORMAT
        Define a format.


                                                    *tcalc-control*
Control:
    ( ... )
        Push unprocessed tokens on the stack (as array).

    (( ARGUMENTS ) ... )
        This is equivalent to: >
            ( begin ( ARGUMENTS ) args ... end )
<       You can define block-local variables (see "args" for details).

    recapture, do
        Prepend unprocessed tokens on the stack to the input queue -- 
        which will then be processed in the next loop.
        EXAMPLE: >
            :adder_two ( 2 + ) ;
            3 adder_two do
            => 5
< 
    #N,METHOD
        Send METHOD to element N (any ruby object) on the stack. Usually 
        only methods from math-related classes are recognized. In order 
        to send methods to an array, you currently have to use this 
        form.

        This will push the new value on the stack. Certain methods 
        (depending on ruby conventions) will also modify the object.

        You can also send methods to variables.
        EXAMPLE: >
            [ 1 2 ] a= 3 #a,<< .
            ls
            => a: 1.0 2.0 3.0

            ( 2 1 + ) ( 3 * ) #1,concat do
            => 9
<
    if, ifelse (CONDITION IFBLOCK ELSEBLOCK)
        Feed IFBLOCK or ELSEBLOCK to the input queue depending on 
        CONDITION.

    assert
        Display an error if the stack doesn't match the assertion. The last 
        element on the stack is the assertion (either a string or an 
        array of unprocessed tokens/strings).
        Example: With stack: >
            /* Check if the top element is 2.0 */
            2 '2.0 assert
            => true

            /* Check if the top element match */
            1 'Numeric assert
            => true

            /* Check if the two top elements match */
            1 2 ( Numeric Numeric ) assert
            => true
            1 2 ( String String ) assert
            => Expected String, but got 2.0
< 
    args
        Similar to assert but provides for named arguments.
        Example: Instead of >
            :binom ( Numeric Numeric ) args
                copy1 fak rot2
                dup fak rot2 - fak *
                /
            ;
<       you could write: >
            :binom ( n:Numeric k:Numeric ) args
                n fak
                k fak n k - fak *
                /
            ;
<       The function could have been also defined in ruby of course.

    validate
        Like assert but push the result of the check as boolean on the stack.
        Example: >
            'foo 'Numeric validate ( 1 + ) ( . ) ifelse
            => empty stack
            2 'Numeric validate ( 1 + ) ( . ) ifelse
            => 3.0
< 
    /* ... */
        Comments: Ignore tokens.


                                                    *tcalc-VIM*
VIM Integration:
    y, yank
        Copy the top N items to a register (* by default). This command 
        takes a register as optional argument, e.g., "y,e"

    p, puts
        Print the top item.

    source
        Read input from a file.

    require
        Load a ruby library.


                                                    *tcalc-array*
Arrays:
    [ item1 item2 ... ]
        Push processed items as array.

    ARRAY INDEX at
        Push the item at INDEX. In a multi-dimensional array, INDEX can be an 
        array.

        Example: >
            [ 'a 'b 'c ] 1 at
            => "b"

        < BTW this actually is the same as: >
            ( a b c ) 1 at
    
    FROM TO STEP Sequence
        Generate a sequence of number (an Array)
        Example: >
            1 2 0.5 Sequence
            => [1, 1.5, 2]
< 
    [ ARRAY ] ( BLOCK ) map
        Run BLOCK over the elements in ARRAY and push the results onto 
        the stack as new array.
        Example: >
            [ 1 2 3 ] ( 2 ** ) map
            => [1.0, 4.0, 9.0]
<         
    [ ARRAY ] ( BLOCK ) mmap
        Similar to map but store tuples of (x,y) values. The block can 
        also return more than one item.
        Example: >
            [ 1 2 3 ] ( 2 ** ) mmap
            => [[1, 1.0], [2, 4.0], [3, 9.0]]
<  
    [ ARRAY ] ( BLOCK ) any?
        Check if any the elements in the array are true for BLOCK.
        Example: >
            [ 1 2 3 ] ( 2 > ) any? ( 1 ) ( -1 ) ifelse
            => 1.0
<  
    [ ARRAY ] ( BLOCK ) all?
        Check if all the elements in the array are true for BLOCK.
        Example: >
            [ 1 2 3 ] ( 2 > ) all? ( 1 ) ( -1 ) ifelse
            => -1.0
<  
    MMAP YDIM XDIM plot
        A simplicistic ASCII function plotter.
        VIM: The plot command takes a register as optional argument 
        (e.g. "plot,a").
        NOTE: I admit there is some room for improvements.
        Example: >
            0 2 PI * 0.1 seq (( a: ) a sin a cos 0 ) mmap 10 10 plot
            =>
                  1.00: ****   1356890                       ***
                  0.78:    **80      134                   **   
                  0.56:    57 **        6                **     
                  0.33:   34    *        79            **       
                  0.11: 02+++++++*+++++++++0++++++++++*+++++++++
                 -0.11:           **        23      ***       01
                 -0.33:            ***       456   **        99 
                 -0.56:              **        78**        68   
                 -0.78:                **      **02      345    
                 -1.00:                  *******   356801       
                   .    +      +     +     +     +      +     ++
                   .    0      1     2     3     4      5     66
<  
    ARRAY ELEMENT array_push
    ARRAY array_pop
    ARRAY ELEMENT array_unshift
    ARRAY array_shift
        These commands are only using when working with array on the 
        stack that aren't assigned to a word.

    ARRAY array_empty?


                                                    *tcalc-numbers*
Rational numbers:
    NUMERATOR DENOMINATOR Rational
        Example: >
            1 2 Rational


Complex numbers:
    REAL IMAGINARY Complex
        Example: >
            2 3 Complex


Integers:
    NUMBER Integer
        This isn't really useful as the number will be soon converted back to a 
        float.


                                                    *tcalc-matrix*
Matrix:
    ARRAY Matrix
        Example: >
            [ [ 2 1 ] [ -1 1 ] ] Matrix

            d [ 0 1 ] at
            => 1.0

            d determinant
            => 3.0


Known Problems:
    - One cannot send methods containing digits in their names (eg log10).



-----------------------------------------------------------------------
Examples~

Simple-minded primes generator: >

    :primes ( top:Numeric ) args
        (( known_primes:Array current:Numeric top:Numeric )
            current top <=
                ( 
                    [ known_primes ] (( prime:Numeric ) current prime % 0 != ) all?
                        ( [ known_primes current ] )
                        ( [ known_primes ] )
                        ifelse
                    current 2 +
                    top
                    primes_helper
                )
                (
                    [ known_primes ]
                )
                ifelse
        ) -> primes_helper
        [ 2 ] 3 top primes_helper
    ;


========================================================================
Contents~

        :TCalc ............... |:TCalc|
        g:tcalc_initialize ... |g:tcalc_initialize|
        g:tcalc_lines ........ |g:tcalc_lines|
        g:tcalc_dir .......... |g:tcalc_dir|
        tcalc#Calculator ..... |tcalc#Calculator()|
        tcalc#Eval ........... |tcalc#Eval()|
        tcalc#Complete ....... |tcalc#Complete()|


========================================================================
plugin/tcalc.vim~

                                                    *:TCalc*
TCalc[!]
    With !, reset stack & input queue.


========================================================================
autoload/tcalc.vim~

                                                    *g:tcalc_initialize*
g:tcalc_initialize
    A string that will be read when first invoking |:TCalc|.
    Define some abbreviations. Use 'ls' to see them.

                                                    *g:tcalc_lines*
g:tcalc_lines                  (default: 10)
    The height of the window. If negative, use fixed height.

                                                    *g:tcalc_dir*
g:tcalc_dir                    (default: fnamemodify('~/.tcalc', ':p'))
    The default directory where "source" finds files.

                                                    *tcalc#Calculator()*
tcalc#Calculator(reset, initial_args)

                                                    *tcalc#Eval()*
tcalc#Eval(initial_args)

                                                    *tcalc#Complete()*
tcalc#Complete(ArgLead, CmdLine, CursorPos)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
