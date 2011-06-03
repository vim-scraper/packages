" Author: Mykola Golubyev ( Nickolay Golubev )
" Email: golubev.nikolay@gmail.com
" Site: www.railmoon.com

function! s:error(msg)
    echohl Error | echo '[std_auto_include] '.a:msg | echohl None
endfunction

let s:std_maps_is_set = 0

function! s:setup_std_symbol_include_map()
    if s:std_maps_is_set
        return
    endif

    let s:std_include_symbol_list_map = {
                \ 'algorithm': 'adjacent_find binary_search copy copy_backward count count_if equal equal_range fill fill_n find find_end find_first_of find_if for_each generate generate_n includes inplace_merge iter_swap lexicographical_compare lower_bound make_heap max max_element merge min min_element mismatch next_permutation nth_element partial_sort partial_sort_copy partition pop_heap prev_permutation push_heap random_shuffle remove remove_copy remove_copy_if remove_if replace replace_copy replace_copy_if replace_if reverse reverse_copy rotate rotate_copy search search_n set_difference set_intersection set_symmetric_difference set_union sort sort_heap stable_partition stable_sort swap swap_ranges transform unique unique_copy upper_bound',
                \ 'bitset': 'bitset',
                \ 'complex': 'complex',
                \ 'deque': 'deque',
                \ 'exception': 'exception bad_exception  set_terminate set_unexpected terminate unexpected uncaught_exception',
                \ 'fstream': 'basic_filebuf basic_ifstream basic_ofstream basic_fstream filebuf wfilebuf ifstream wifstream ofstream wofstream fstream wfstream',
                \ 'functional': 'plus minus multiplies divides modulus negate equal_to not_equal_to greater greater_equal less less_equal logical_and logical_or logical_not bind_1st bind_2nd not1 not2 ptr_fun mem_fun mem_fun1 mem_fun_ref mem_fun1_ref',
                \ 'ios': 'basic_ios fpos ios ios_base streamoff streampos streamsize wios wstreampos boolalpha dec fixed hex hexfloat internal left noboolalpha noshowbase noshowpoint noshowpos noskipws nounitbuf nouppercase oct right scientific showbase showpoint showpos skipws unitbuf uppercase',
                \ 'iomanip': 'resetiosflags setiosflags setbase setfill setprecision setw',
                \ 'iostream': 'cin cout cerr clog wcin wcout wcerr wclog',
                \ 'istream': 'basic_istream basic_iostream istream wistream iostream wiostream',
                \ 'iterator': 'iterator reverse_iterator iterator_traits insert_iterator back_insert_iterator front_insert_iterator istream_iterator ostream_iterator istreambuf_iterator ostreambuf_iterator advance distance back_inserter front_inserter inserter',
                \ 'limits': 'numeric_limits',
                \ 'list': 'list',
                \ 'locale': 'codecvt codecvt_base codecvt_byname collate collate_byname ctype ctype_base ctype_byname has_facet locale messages messages_base messages_byname money_base money_get money_put moneypunct moneypunct_byname num_get num_put numpunct numpunct_byname time_base time_get time_get_byname time_put time_put_byname use_facet',
                \ 'map': 'map multimap',
                \ 'memory': 'auto_ptr',
                \ 'new': 'new_handler bad_alloc nothrow_t nothrow set_new_handler',
                \ 'numeric': 'accumulate adjacent_difference inner_product partial_sum',
                \ 'ostream': 'basic_ostream ostream wostream ends flush endl',
                \ 'queue': 'queue priority_queue',
                \ 'set': 'set multiset',
                \ 'sstream': 'basic_stringbuf stringbuf wstringbuf basic_istringstream istringstream wistringstream basic_ostringstream ostringstream wostringstream basic_stringstream stringstream wstringstream',
                \ 'stack': 'stack',
                \ 'stdexcept': 'logic_error domain_error invalid_argument length_error out_of_range runtime_error range_error overflow_error underflow_error',
                \ 'streambuf': 'basic_streambuf streambuf wstreambuf',
                \ 'string': 'char_traits basic_string string wstring getline',
                \ 'typeinfo': 'type_info bad_cast bad_typeid',
                \ 'valarray': 'gslice gslice_array indirect_array mask_array slice slice_array valarray',
                \ 'utility': 'pair make_pair',
                \ 'vector': 'vector',
                \
                \ 'cstdlib': 'abort abs atexit atof atoi atol atoll bsearch calloc div div_t _Exit exit free getenv labs llabs ldiv ldiv_t ldiv ldiv_t malloc mblen mbstowcs mbtowc qsort rand realloc size_t srand strtod strtof strtol strtold strtoll strtoul strtoull system wchar_t wcstombs wctomb',
                \ 'cctype': 'isalnum isalpha isblank iscntrl isdigit isgraph islower isprint ispunct isspace isupper isxdigit tolower toupper',
                \ 'clocale': 'lconv setlocale localeconv',
                \ 'cmath': 'abs acos acosh asin asinh atan atanh atan2 cbrt ceil copysign cos cosh double_t erf erfc exp expm1 exp2 fabs fdim float_t floor fma fmax fmin fmod fpclassify frexp hypot ilogb isfinite isgreater isgreaterequal isinf isless islessequal islessgreater isnan isnormal isunordered ldexp lgamma llrint llround log log10 log1p log2 logb lrint lround modf nan nearbyint nextafter nexttoward pow remainder remquo rint round scalbln scalbn signbit sin sinh sqrt tan tanh tgamma trunc acosf acoshf asinf asinhf atanf atanhf atan2f cbrtf ceilf copysignf cosf coshf erff erfcf expf exp2f expm1f fabsf fdimf floorf fmaf fmaxf fminf fmodf frexpf hypotf ilogbf ldexpf lgammaf llrintf llroundf logf log10f log1pf log2f logbf lrintf lroundf modff nanf nearbyintf nextafterf nexttowardf pow remainderf remquof rintf roundf scalblnf scalbnf sinf sinhf sqrtf tanf tanhf tgammaf truncf acosl acoshl asinl asinhl atanl atanhl atan2l cbrtl ceill copysignl cosl coshl erfl erfcl expl exp2l expm1l fabsl fdiml floorl fmal fmaxl fminl fmodl frexpl hypotl ilogbl ldexpl lgammal llrintl llroundl logl log10l log1pl log2l logbl lrintl lroundl modfl nanl nearbyintl nextafterl nexttowardl pow remainderl remquol rintl roundl scalblnl scalbnl sinl sinhl sqrtl tanl tanhl tgammal truncl assoc_laguerre assoc_legendre beta comp_ellint_1 comp_ellint_2 comp_ellint_3 cyl_bessel_i cyl_bessel_j cyl_bessel_k cyl_neumann ellint_1 ellint_2 ellint_3 expint hermite laguerre legendre riemann_zeta sph_bessel sph_legendre sph_neumann assoc_laguerref assoc_legendref betaf comp_ellint_1f comp_ellint_2f comp_ellint_3f cyl_bessel_if cyl_bessel_jf cyl_bessel_kf cyl_neumannf ellint_1f ellint_2f ellint_3f expintf hermitef laguerref legendref riemann_zetaf sph_besself sph_legendref sph_neumannf assoc_laguerrel assoc_legendrel betal comp_ellint_1l comp_ellint_2l comp_ellint_3l cyl_bessel_il cyl_bessel_jl cyl_bessel_kl cyl_neumannl ellint_1l ellint_2l ellint_3l expintl hermitel laguerrel legendrel riemann_zetal sph_bessell sph_legendrel sph_neumannl',
                \ 'csignal': 'sig_atomic_t signal raise',
                \ 'cstdarg': 'va_list',
                \ 'cstddef': 'ptrdiff_t size_t',
                \ 'cstdio': 'FILE fpos_t clearerr fclose feof ferror fflush fgetc fgetpos fgets fopen fprintf fputc fputs fread freopen fscanf fseek fsetpos ftell fwrite getc getchar gets perror printf putc putchar puts remove rename rewind scanf setbuf setvbuf sprintf sscanf tmpfile tmpnam ungetc vfprintf vprintf vsprintf',
                \ 'cstring': 'memcpy memmove strcpy strncpy strcat strncat memcmp strcmp strcoll strncmp strxfrm strcspn strspn strtok memset strerror strlen memchr',
                \ 'ctime': 'clock_t time_t tm clock difftime mktime time asctime ctime gmtime localtime strftime',
                \ 'cwchar': 'wint_t btowc fgetwc fgetws fputwc fputws fwide fwprintf fwscanf getwc getwchar mbrlen mbrtowc mbsinit mbsrtowcs putwc putwchar swprintf swscanf ungetwc vfwprintf vfwscanf vswprintf vswscanf vwprintf vwscanf wcrtomb wcscat wcscmp wcscoll wcscpy wcscspn wcsftime wcslen wcsncat wcsncmp wcsncpy wcsrtombs wcsspn wcstod wcstof wcstok wcstol wcstoul wcsxfrm wctob wmemcmp wmemcpy wmemmove wmemset wprintf wscanf wcschr',
                \ 'cwctype': 'wint_t wctype_t wctrans_t iswalnum iswalpha iswblank iswcntrl iswdigit iswgraph iswlower iswprint iswprint iswpunct iswspace iswupper iswxdigit iswctype towlower towupper towctrans wctrans wctype'
                \ }




    let s:std_symbol_include_map = {}

    for [key, value] in items(s:std_include_symbol_list_map)
        for symbol in split(value)
            let s:std_symbol_include_map[symbol] = key
        endfor
    endfor

    let s:std_maps_is_set = 1

    "for [key, value] in items(s:std_symbol_include_map)
        "echo key.' '.value
    "endfor
endfunction

function! s:remove_std_headers()
    let register_value = @"

    for header in keys(s:std_include_symbol_list_map)
        exec 'silent g/#include\s\+<'.header.'>/d'
    endfor

    let @" = register_value
endfunction

function! s:find_std_symbols()
    let result = {}
    let search_flags = 'We'

    call s:setup_std_symbol_include_map()

    let position = getpos('.')

    1
    while 1
        let matched_position = search('std::\w\+', search_flags)
        if 0 == matched_position
            break
        endif

        let symbol = expand('<cword>')
        let result[symbol] = 1
    endwhile

    call setpos('.', position)

    return result
endfunction

function! s:add_std_includes()
    let symbols = s:find_std_symbols()
    let includes = {}

    for symbol in keys(symbols)
        if has_key(s:std_symbol_include_map, symbol)
            let includes[s:std_symbol_include_map[symbol]] = 1
        else
            call s:error('No record for "'.symbol.'"')
        endif
    endfor

    call s:remove_std_headers()

    " find line number of last include
    1
    let first_include_last_number = search( '^#include', 'cW' )
    $
    let last_include_last_number = search( '^#include', 'Wb' )

    " prepare list of includes
    let include_lines = []
    for include in keys(includes)
        let include_line = '#include <'.include.'>'
        call add(include_lines, include_line)
    endfor

    call sort(include_lines)
    call insert(include_lines, '', 0)
    call add(include_lines, '')

    call append(last_include_last_number + 1, include_lines)

    " remove extra blank lines
    exec first_include_last_number.','.last_include_last_number.'s/\(\n\)\n\+/\1\1/' 

    $
    let last_include_last_number = search( '^#include', 'Wb' )

    exec last_include_last_number
endfunction

command! StdIncludes call s:add_std_includes()

