"color_toon.vim -- colorscheme toy
" Maintainer:   Walter Hutchins
" Last Change:  July 06 2006
" Version:      1.2
" Setup:        Copy color_toon.vim to ~/.vim/plugin/
"
"               Windows - copy to $VIMRUNTIME/plugin
"                       - adjust myjunk | adjust plugin location if necessary.
"
" Thanks to: colortest
" Thanks to: Tip #634: To view all colours 
" Thanks to: hitest.vim
"Use without any expectations.
"Please liberally use :qa! and :q! whenever you like.
"
"Usage: Colortoon
"       Colortoon -d[ark] -n[ojunk]
"       call Color_toon()
"       
"       Colortoon -d  Does dark colors first (see < 7 Oops:)
"       Colortoon -n  Does not input syntax from any myjunk.vim file,
"                     it uses the current color scheme.
"       
"       Defaults are to start with light colors and try to use myjunk colors.
"
" Usage: (1) First, run Colortoon.
"        (2) Position to desired hi group in myjunk window and type :y b
"        (3) Position to desired color in light or dark window and type :y a
"        (4) Colormake                 - Set foreground color per registers a,b
"            Colormake -b[ackground]   - Set background color per registers a,b
"
"         In steps 2 & 3, ':y a' means :y<SPACEBAR>a<RETURN>
"                                   or :yank<SPACEBAR>a<RETURN>
"                                   or "ayy
"                         ':y b' means :y<SPACEBAR>b<RETURN>
"                                   or :yank<SPACEBAR>b<RETURN>
"                                   or "byy
"                         
"                        If you consistently(!) reverse a and b, it still works.
"
"Somethings: to do are:
"Scroll up and down in the 'dark' and 'light' windows to see what the
"cterm and gui numbers are for the colors.
"Scroll up and down in the 'myjunk.vim' window to see highlighting groups.
"Go into myjunk window and :w! to save current colors in myjunk.vim.
"
"If you happen to get a color scheme, copy 'myjunk.vim' to 'myugly.vim'
"or whatever. Then stick 'let colors_name="myugly"' in myugly.vim.
"If myjunk.vim gets to where you don't like it anymore, then delete it
"and it will be created from your default colorscheme (maybe).
"
"Customize a gui color. Only works for six-digit-hex color numbers.
" (1) Position to the color nearest to your desired color in the light or dark 
" (2) window and type :y a
" (3) type :new
" (4) type "ap
" (5) edit the gui_xxxxxx 
" (6) type :y a
" (7) type :q!
" (8) type :Colormake   or  :Colormake -b
" (9) Go into the myjunk window and type :w! (only if you like it.)
" Note: The custom color won't 'stick' if you later change that hi group.
"
" Note: If you use set guifont= in a vim setting, there could be a resulting
"       font= part in one of the hi groups and you should edit it out
"       to be friendly to xterms.
"
"Oops: vim < 7 can't help it:
"Vim < 7, when you start up Colortoon, there will be an error -- too many 
"highlighting groups while it is trying to show the colors in the 
"light and dark windows.  It stumbles after around 200th one encountered. 
"If you need to see the rest of dark ones, say 'Colortoon -dark'. 
"May need to completely get out of all windows before it will show 
"different ones (this is a funny thing.)
"
"Remember to use :qa! and :q! whenever you like -- to bail out.
command -nargs=* Colortoon call Color_toon(<f-args>)
command -nargs=* Colormake call Color_toon_make(<f-args>)

"Begin function Color_toon
function Color_toon(...)

" save global options and registers
let s:hidden      = &hidden
let s:lazyredraw  = &lazyredraw
let s:more        = &more
let s:report      = &report
let s:shortmess   = &shortmess
let s:wrapscan    = &wrapscan
let s:register_a  = @a
let s:register_b  = @b
let s:register_se = @/

let choice="light"
let remargs=a:0
while remargs > 0
    exec 'let thearg=a:'.remargs
    if thearg ==? "dark" || thearg =~? "^-d"
        let choice="dark"
    elseif thearg ==? "nojunk" || thearg =~? "^-n"
        let nojunk="nojunk"
    endif
    let remargs=remargs - 1
endwhile

if $OS =~? "Windows"
    let myjunk=$VIMRUNTIME . "/colors/myjunk.vim"
else
    let myjunk='~/.vim/colors/myjunk.vim'
endif

let light=s:light()
let dark=s:dark()
let first=light
let last=dark
if exists("choice")
    if choice ==? "dark"
        let first=dark
        let last=light
    endif
endif

if !exists("nojunk") || nojunk != "nojunk"
    colo myjunk
endif

"Current highlighting specs
set hidden lazyredraw nomore report=99999 shortmess=aoOstTW wrapscan
new
":help window-move-cursor
let s:myjunk_window=winnr()
"exe s:myjunk_window . "wincmd w" 
exec 'edit ' myjunk
call s:Myjunk()


"Colors choices to look at
"First colors - 1st ~ 200 display actual color
new
let s:myjunk_window=s:myjunk_window + 1
"exec 'edit ' first
if exists("choice") && choice ==? "dark"
    exec 'edit dark'
else
    exec 'edit light'
endif
1,$d
put =first
g/^$/d
g/^cterm_\d\+/ exec 'hi col_'.expand("<cword>").' ctermfg='.strpart(expand("<cword>"),6).' guifg=#'.strpart(expand("<cword>"),strlen(expand("<cword>"))-6)| exec 'syn keyword col_'.expand("<cword>")." ".expand("<cword>")
1
" we don't want to save the temporary file
set nomodified

"Last colors - ones beyond ~ 200th will display random colors
vnew
let s:myjunk_window=s:myjunk_window + 1
"exec 'edit ' last
if exists("choice") && choice ==? "dark"
    exec 'edit light'
else
    exec 'edit dark'
endif
1,$d
put =last
g/^$/d
g/^cterm_\d\+/ exec 'hi col_'.expand("<cword>").' ctermfg='.strpart(expand("<cword>"),6).' guifg=#'.strpart(expand("<cword>"),strlen(expand("<cword>"))-6)| exec 'syn keyword col_'.expand("<cword>")." ".expand("<cword>")
1
" we don't want to save the temporary file
set nomodified

" the following trick avoids the "Press RETURN ..." prompt
0 append
.

" restore global options and registers
let &hidden      = s:hidden
let &lazyredraw  = s:lazyredraw
let &more        = s:more
let &report      = s:report
let &shortmess   = s:shortmess
let &wrapscan    = s:wrapscan
let @a           = s:register_a
let @b           = s:register_b

" restore last search pattern
call histdel("search", -1)
let @/ = s:register_se

endfunction
"End function Color_toon

"Begin function s:Myjunk()
function s:Myjunk(...)
exe s:myjunk_window . "wincmd w" 

" save global options and registers
let hidden      = &hidden
let lazyredraw  = &lazyredraw
let more          = &more
let report      = &report
let shortmess   = &shortmess
let wrapscan    = &wrapscan
let register_a  = @a
let register_b  = @b
let register_se = @/
set hidden lazyredraw nomore report=99999 shortmess=aoOstTW wrapscan
1,$d
redir @a
hi
redir END
put a
%s/^.*links to.*$//
%s/xxx//
g/^$/d
g/^col_cterm_\d\+/d
g/ cleared/d
g/\(\n\)\s\+/j
" precede syntax command
% substitute /^[^ ]*/syn keyword & &/
" execute syntax commands
syntax clear
% yank b
@b
" remove syntax commands again
% substitute /^syn keyword \(\S\+\) //
%s/^/hi /
"1
" we don't want to save myjunk unless user made a change
set nomodified

" restore global options and registers
let &hidden      = hidden
let &lazyredraw  = lazyredraw
let &more        = more
let &report      = report
let &shortmess   = shortmess
let &wrapscan    = wrapscan
let @a           = register_a
let @b           = register_b
" restore last search pattern
call histdel("search", -1)
let @/ = register_se

" the following trick avoids the "Press RETURN ..." prompt
0 append
.
" show Normal at top
/Normal/

endfunction
"End function s:Myjunk()

"Begin function Color_toon_make
function Color_toon_make(...)
    if a:0 == 0
        let ground="fg"
    elseif a:1 ==? "background" || a:1 =~? "^-b"
        let ground="bg"
    else
        let ground="fg"
    endif
    let group=""
    let color=""
    let reg_a=@a
    let reg_b=@b
    if match(reg_a, "cterm_") != -1
        let color=reg_a
    elseif match(reg_a, "=") != -1
        let group=reg_a
    endif
    if match(reg_b, "cterm_") != -1
        let color=reg_b
    elseif match(reg_b, "=") != -1
        let group=reg_b
    endif
    if group == "" || color == ""
        return
    endif
    let color_group=matchstr(group, '\S*', 3)
    let gui_color=strpart(color, strlen(color)-7)
    let gui_color='#' . substitute(gui_color, '\n', '', '')
    let cterm_start=match(color, '_')
    let cterm_end=match(color, '_', cterm_start+1)
    let cterm_len=cterm_end - cterm_start
    let cterm_color=strpart(color, cterm_start+1, cterm_len-1)

    if ground == "fg"
        let ctermfg=' ctermfg=' . cterm_color . ' '
        let guifg=' guifg=' . gui_color . ' '
        let spec=substitute(group, 'ctermfg=\S*', ctermfg, '')
        let spec=substitute(spec, 'guifg=\S*', guifg, '')
        if match(spec, 'ctermfg=') == -1
            let spec=spec . ctermfg
        endif
        if match(spec, 'guifg=') == -1
            let spec=spec . guifg
        endif
    endif

    if ground == "bg"
        let ctermbg=' ctermbg=' . cterm_color . ' '
        let guibg=' guibg=' . gui_color . ' '
        let spec=substitute(group, 'ctermbg=\S*', ctermbg, '')
        let spec=substitute(spec, 'guibg=\S*', guibg, '')
        if match(spec, 'ctermbg=') == -1
            let spec=spec . ctermbg
        endif
        if match(spec, 'guibg=') == -1
            let spec=spec . guibg
        endif
    endif

    exec spec
    call s:Myjunk()
    if !exists("s:myjunk_window")
        echo "Did you forget to run Colortoon? Myjunk is in window(" . not_exists . ")"
    endif
    return spec
endfunction
"End function Color_toon_make

function s:light()
let light=""
let light=light . "cterm_1_gui_ce0000" . "\n"
let light=light . "cterm_2_gui_00cb00" . "\n"
let light=light . "cterm_3_gui_cecb00" . "\n"
let light=light . "cterm_5_gui_ce00ce" . "\n"
let light=light . "cterm_6_gui_00cbce" . "\n"
let light=light . "cterm_7_gui_e7e3e7" . "\n"
let light=light . "cterm_9_gui_ff0000" . "\n"
let light=light . "cterm_10_gui_00ff00" . "\n"
let light=light . "cterm_11_gui_ffff00" . "\n"
let light=light . "cterm_12_gui_5a5dff" . "\n"
let light=light . "cterm_13_gui_ff00ff" . "\n"
let light=light . "cterm_14_gui_00ffff" . "\n"
let light=light . "cterm_15_gui_ffffff" . "\n"
let light=light . "cterm_32_gui_0087d7" . "\n"
let light=light . "cterm_33_gui_0087ff" . "\n"
let light=light . "cterm_34_gui_00af00" . "\n"
let light=light . "cterm_35_gui_00af5f" . "\n"
let light=light . "cterm_36_gui_00af87" . "\n"
let light=light . "cterm_37_gui_00afaf" . "\n"
let light=light . "cterm_38_gui_00afd7" . "\n"
let light=light . "cterm_39_gui_00afff" . "\n"
let light=light . "cterm_40_gui_00d700" . "\n"
let light=light . "cterm_41_gui_00d75f" . "\n"
let light=light . "cterm_42_gui_00d787" . "\n"
let light=light . "cterm_43_gui_00d7af" . "\n"
let light=light . "cterm_44_gui_00d7d7" . "\n"
let light=light . "cterm_45_gui_00d7ff" . "\n"
let light=light . "cterm_46_gui_00ff00" . "\n"
let light=light . "cterm_47_gui_00ff5f" . "\n"
let light=light . "cterm_48_gui_00ff87" . "\n"
let light=light . "cterm_49_gui_00ffaf" . "\n"
let light=light . "cterm_50_gui_00ffd7" . "\n"
let light=light . "cterm_51_gui_00ffff" . "\n"
let light=light . "cterm_63_gui_5f5fff" . "\n"
let light=light . "cterm_67_gui_5f87af" . "\n"
let light=light . "cterm_68_gui_5f87d7" . "\n"
let light=light . "cterm_69_gui_5f87ff" . "\n"
let light=light . "cterm_70_gui_5faf00" . "\n"
let light=light . "cterm_71_gui_5faf5f" . "\n"
let light=light . "cterm_72_gui_5faf87" . "\n"
let light=light . "cterm_73_gui_5fafaf" . "\n"
let light=light . "cterm_74_gui_5fafd7" . "\n"
let light=light . "cterm_75_gui_5fafff" . "\n"
let light=light . "cterm_76_gui_5fd700" . "\n"
let light=light . "cterm_77_gui_5fd75f" . "\n"
let light=light . "cterm_78_gui_5fd787" . "\n"
let light=light . "cterm_79_gui_5fd7af" . "\n"
let light=light . "cterm_80_gui_5fd7d7" . "\n"
let light=light . "cterm_81_gui_5fd7ff" . "\n"
let light=light . "cterm_82_gui_5fff00" . "\n"
let light=light . "cterm_83_gui_5fff5f" . "\n"
let light=light . "cterm_84_gui_5fff87" . "\n"
let light=light . "cterm_85_gui_5fffaf" . "\n"
let light=light . "cterm_86_gui_5fffd7" . "\n"
let light=light . "cterm_87_gui_5fffff" . "\n"
let light=light . "cterm_98_gui_875fd7" . "\n"
let light=light . "cterm_99_gui_875fff" . "\n"
let light=light . "cterm_103_gui_8787af" . "\n"
let light=light . "cterm_104_gui_8787d7" . "\n"
let light=light . "cterm_105_gui_8787ff" . "\n"
let light=light . "cterm_106_gui_87af00" . "\n"
let light=light . "cterm_107_gui_87af5f" . "\n"
let light=light . "cterm_108_gui_87af87" . "\n"
let light=light . "cterm_109_gui_87afaf" . "\n"
let light=light . "cterm_110_gui_87afd7" . "\n"
let light=light . "cterm_111_gui_87afff" . "\n"
let light=light . "cterm_112_gui_87d700" . "\n"
let light=light . "cterm_113_gui_87d75f" . "\n"
let light=light . "cterm_114_gui_87d787" . "\n"
let light=light . "cterm_115_gui_87d7af" . "\n"
let light=light . "cterm_116_gui_87d7d7" . "\n"
let light=light . "cterm_117_gui_87d7ff" . "\n"
let light=light . "cterm_118_gui_87ff00" . "\n"
let light=light . "cterm_119_gui_87ff5f" . "\n"
let light=light . "cterm_120_gui_87ff87" . "\n"
let light=light . "cterm_121_gui_87ffaf" . "\n"
let light=light . "cterm_122_gui_87ffd7" . "\n"
let light=light . "cterm_123_gui_87ffff" . "\n"
let light=light . "cterm_134_gui_af5fd7" . "\n"
let light=light . "cterm_135_gui_af5fff" . "\n"
let light=light . "cterm_136_gui_af8700" . "\n"
let light=light . "cterm_137_gui_af875f" . "\n"
let light=light . "cterm_138_gui_af8787" . "\n"
let light=light . "cterm_139_gui_af87af" . "\n"
let light=light . "cterm_140_gui_af87d7" . "\n"
let light=light . "cterm_141_gui_af87ff" . "\n"
let light=light . "cterm_142_gui_afaf00" . "\n"
let light=light . "cterm_143_gui_afaf5f" . "\n"
let light=light . "cterm_144_gui_afaf87" . "\n"
let light=light . "cterm_145_gui_afafaf" . "\n"
let light=light . "cterm_146_gui_afafd7" . "\n"
let light=light . "cterm_147_gui_afafff" . "\n"
let light=light . "cterm_148_gui_afd700" . "\n"
let light=light . "cterm_149_gui_afd75f" . "\n"
let light=light . "cterm_150_gui_afd787" . "\n"
let light=light . "cterm_151_gui_afd7af" . "\n"
let light=light . "cterm_152_gui_afd7d7" . "\n"
let light=light . "cterm_153_gui_afd7ff" . "\n"
let light=light . "cterm_154_gui_afff00" . "\n"
let light=light . "cterm_155_gui_afff5f" . "\n"
let light=light . "cterm_156_gui_afff87" . "\n"
let light=light . "cterm_157_gui_afffaf" . "\n"
let light=light . "cterm_158_gui_afffd7" . "\n"
let light=light . "cterm_159_gui_afffff" . "\n"
let light=light . "cterm_160_gui_d70000" . "\n"
let light=light . "cterm_161_gui_d7005f" . "\n"
let light=light . "cterm_162_gui_d70087" . "\n"
let light=light . "cterm_163_gui_d700af" . "\n"
let light=light . "cterm_164_gui_d700d7" . "\n"
let light=light . "cterm_165_gui_d700ff" . "\n"
let light=light . "cterm_166_gui_d75f00" . "\n"
let light=light . "cterm_167_gui_d75f5f" . "\n"
let light=light . "cterm_168_gui_d75f87" . "\n"
let light=light . "cterm_169_gui_d75faf" . "\n"
let light=light . "cterm_170_gui_d75fd7" . "\n"
let light=light . "cterm_171_gui_d75fff" . "\n"
let light=light . "cterm_172_gui_d78700" . "\n"
let light=light . "cterm_173_gui_d7875f" . "\n"
let light=light . "cterm_174_gui_d78787" . "\n"
let light=light . "cterm_175_gui_d787af" . "\n"
let light=light . "cterm_176_gui_d787d7" . "\n"
let light=light . "cterm_177_gui_d787ff" . "\n"
let light=light . "cterm_178_gui_d7af00" . "\n"
let light=light . "cterm_179_gui_d7af5f" . "\n"
let light=light . "cterm_180_gui_d7af87" . "\n"
let light=light . "cterm_181_gui_d7afaf" . "\n"
let light=light . "cterm_182_gui_d7afd7" . "\n"
let light=light . "cterm_183_gui_d7afff" . "\n"
let light=light . "cterm_184_gui_d7d700" . "\n"
let light=light . "cterm_185_gui_d7d75f" . "\n"
let light=light . "cterm_186_gui_d7d787" . "\n"
let light=light . "cterm_187_gui_d7d7af" . "\n"
let light=light . "cterm_188_gui_d7d7d7" . "\n"
let light=light . "cterm_189_gui_d7d7ff" . "\n"
let light=light . "cterm_190_gui_d7ff00" . "\n"
let light=light . "cterm_191_gui_d7ff5f" . "\n"
let light=light . "cterm_192_gui_d7ff87" . "\n"
let light=light . "cterm_193_gui_d7ffaf" . "\n"
let light=light . "cterm_194_gui_d7ffd7" . "\n"
let light=light . "cterm_195_gui_d7ffff" . "\n"
let light=light . "cterm_196_gui_ff0000" . "\n"
let light=light . "cterm_197_gui_ff005f" . "\n"
let light=light . "cterm_198_gui_ff0087" . "\n"
let light=light . "cterm_199_gui_ff00af" . "\n"
let light=light . "cterm_200_gui_ff00d7" . "\n"
let light=light . "cterm_201_gui_ff00ff" . "\n"
let light=light . "cterm_202_gui_ff5f00" . "\n"
let light=light . "cterm_203_gui_ff5f5f" . "\n"
let light=light . "cterm_204_gui_ff5f87" . "\n"
let light=light . "cterm_205_gui_ff5faf" . "\n"
let light=light . "cterm_206_gui_ff5fd7" . "\n"
let light=light . "cterm_207_gui_ff5fff" . "\n"
let light=light . "cterm_208_gui_ff8700" . "\n"
let light=light . "cterm_209_gui_ff875f" . "\n"
let light=light . "cterm_210_gui_ff8787" . "\n"
let light=light . "cterm_211_gui_ff87af" . "\n"
let light=light . "cterm_212_gui_ff87d7" . "\n"
let light=light . "cterm_213_gui_ff87ff" . "\n"
let light=light . "cterm_214_gui_ffaf00" . "\n"
let light=light . "cterm_215_gui_ffaf5f" . "\n"
let light=light . "cterm_216_gui_ffaf87" . "\n"
let light=light . "cterm_217_gui_ffafaf" . "\n"
let light=light . "cterm_218_gui_ffafd7" . "\n"
let light=light . "cterm_219_gui_ffafff" . "\n"
let light=light . "cterm_220_gui_ffd700" . "\n"
let light=light . "cterm_221_gui_ffd75f" . "\n"
let light=light . "cterm_222_gui_ffd787" . "\n"
let light=light . "cterm_223_gui_ffd7af" . "\n"
let light=light . "cterm_224_gui_ffd7d7" . "\n"
let light=light . "cterm_225_gui_ffd7ff" . "\n"
let light=light . "cterm_226_gui_ffff00" . "\n"
let light=light . "cterm_227_gui_ffff5f" . "\n"
let light=light . "cterm_228_gui_ffff87" . "\n"
let light=light . "cterm_229_gui_ffffaf" . "\n"
let light=light . "cterm_230_gui_ffffd7" . "\n"
let light=light . "cterm_231_gui_ffffff" . "\n"
let light=light . "cterm_245_gui_8a8a8a" . "\n"
let light=light . "cterm_246_gui_949494" . "\n"
let light=light . "cterm_247_gui_9e9e9e" . "\n"
let light=light . "cterm_248_gui_a8a8a8" . "\n"
let light=light . "cterm_249_gui_b2b2b2" . "\n"
let light=light . "cterm_250_gui_bcbcbc" . "\n"
let light=light . "cterm_251_gui_c6c6c6" . "\n"
let light=light . "cterm_252_gui_d0d0d0" . "\n"
let light=light . "cterm_253_gui_dadada" . "\n"
let light=light . "cterm_254_gui_e4e4e4" . "\n"
let light=light . "cterm_255_gui_eeeeee" . "\n"
return light
endfunction

function s:dark()
let dark=""
let dark=dark . "cterm_0_gui_000000" . "\n"
let dark=dark . "cterm_4_gui_0000ef" . "\n"
let dark=dark . "cterm_8_gui_7b7d7b" . "\n"
let dark=dark . "cterm_16_gui_000000" . "\n"
let dark=dark . "cterm_17_gui_00005f" . "\n"
let dark=dark . "cterm_18_gui_000087" . "\n"
let dark=dark . "cterm_19_gui_0000af" . "\n"
let dark=dark . "cterm_20_gui_0000d7" . "\n"
let dark=dark . "cterm_21_gui_0000ff" . "\n"
let dark=dark . "cterm_22_gui_005f00" . "\n"
let dark=dark . "cterm_23_gui_005f5f" . "\n"
let dark=dark . "cterm_24_gui_005f87" . "\n"
let dark=dark . "cterm_25_gui_005faf" . "\n"
let dark=dark . "cterm_26_gui_005fd7" . "\n"
let dark=dark . "cterm_27_gui_005fff" . "\n"
let dark=dark . "cterm_28_gui_008700" . "\n"
let dark=dark . "cterm_29_gui_00875f" . "\n"
let dark=dark . "cterm_30_gui_008787" . "\n"
let dark=dark . "cterm_31_gui_0087af" . "\n"
let dark=dark . "cterm_52_gui_5f0000" . "\n"
let dark=dark . "cterm_53_gui_5f005f" . "\n"
let dark=dark . "cterm_54_gui_5f0087" . "\n"
let dark=dark . "cterm_55_gui_5f00af" . "\n"
let dark=dark . "cterm_56_gui_5f00d7" . "\n"
let dark=dark . "cterm_57_gui_5f00ff" . "\n"
let dark=dark . "cterm_58_gui_5f5f00" . "\n"
let dark=dark . "cterm_59_gui_5f5f5f" . "\n"
let dark=dark . "cterm_60_gui_5f5f87" . "\n"
let dark=dark . "cterm_61_gui_5f5faf" . "\n"
let dark=dark . "cterm_62_gui_5f5fd7" . "\n"
let dark=dark . "cterm_64_gui_5f8700" . "\n"
let dark=dark . "cterm_65_gui_5f875f" . "\n"
let dark=dark . "cterm_66_gui_5f8787" . "\n"
let dark=dark . "cterm_88_gui_870000" . "\n"
let dark=dark . "cterm_89_gui_87005f" . "\n"
let dark=dark . "cterm_90_gui_870087" . "\n"
let dark=dark . "cterm_91_gui_8700af" . "\n"
let dark=dark . "cterm_92_gui_8700d7" . "\n"
let dark=dark . "cterm_93_gui_8700ff" . "\n"
let dark=dark . "cterm_94_gui_875f00" . "\n"
let dark=dark . "cterm_95_gui_875f5f" . "\n"
let dark=dark . "cterm_96_gui_875f87" . "\n"
let dark=dark . "cterm_97_gui_875faf" . "\n"
let dark=dark . "cterm_100_gui_878700" . "\n"
let dark=dark . "cterm_101_gui_87875f" . "\n"
let dark=dark . "cterm_102_gui_878787" . "\n"
let dark=dark . "cterm_124_gui_af0000" . "\n"
let dark=dark . "cterm_125_gui_af005f" . "\n"
let dark=dark . "cterm_126_gui_af0087" . "\n"
let dark=dark . "cterm_127_gui_af00af" . "\n"
let dark=dark . "cterm_128_gui_af00d7" . "\n"
let dark=dark . "cterm_129_gui_af00ff" . "\n"
let dark=dark . "cterm_130_gui_af5f00" . "\n"
let dark=dark . "cterm_131_gui_af5f5f" . "\n"
let dark=dark . "cterm_132_gui_af5f87" . "\n"
let dark=dark . "cterm_133_gui_af5faf" . "\n"
let dark=dark . "cterm_232_gui_080808" . "\n"
let dark=dark . "cterm_233_gui_121212" . "\n"
let dark=dark . "cterm_234_gui_1c1c1c" . "\n"
let dark=dark . "cterm_235_gui_262626" . "\n"
let dark=dark . "cterm_236_gui_303030" . "\n"
let dark=dark . "cterm_237_gui_3a3a3a" . "\n"
let dark=dark . "cterm_238_gui_444444" . "\n"
let dark=dark . "cterm_239_gui_4e4e4e" . "\n"
let dark=dark . "cterm_240_gui_585858" . "\n"
let dark=dark . "cterm_241_gui_626262" . "\n"
let dark=dark . "cterm_242_gui_6c6c6c" . "\n"
let dark=dark . "cterm_243_gui_767676" . "\n"
let dark=dark . "cterm_244_gui_808080" . "\n"
return dark
endfunction
