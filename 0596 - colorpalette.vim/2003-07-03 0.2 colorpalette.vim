"ORIGINAL FILE: C:\Vim\colorpalette.vim
"CONTENT: vim script for color palette etc. -- VERSION 0.2 
"AUTHOR: George Nicolaie Nãstasie 
"HOSTNAME: PC CAGONLALACHE
"Created at: Wed 19 Mar 2003 -- 19:13:49



"USAGE:

"To see the Palette of colors:
        "normal mode: ;p
        "command: Palette
"To select a color:
        "normal mode: <RightMouse> or <Up> <Down> arrows 
"To color a word or a line:
        "normal mode for background: <Space>
        "normal mode for foreground: <M-Space>
        "visual mode for background: <RightMouse>
        "visual mode for foreground: <M-RightMouse>
"Color HEX and RGB information:
        "normal mode: i
        "popup menu: info color

"Note: To see in Status-Line the color that has been selected --don´t forget to change the default number of "User".






:fun! Colorinfo()"{{{

:let s:jreg = @j

:         if s:jreg=="#fffafa"
:                             let @k = " -- Snow                    RGB: 255-250-250    HEX: fffafa"
:     elseif s:jreg=="#eee9e9"
:                             let @k = " -- Snow 2                  RGB: 238-233-233    HEX: eee9e9"
:     elseif s:jreg=="#cdc9c9"
:                             let @k = " -- Snow 3                  RGB: 205-201-201    HEX: cdc9c9"
:     elseif s:jreg=="#8b8989"
:                             let @k = " -- Snow 4                  RGB: 139-137-137    HEX: 8b8989"
:     elseif s:jreg=="#f8f8ff"
:                             let @k = " -- Ghost White             RGB: 248-248-255    HEX: f8f8ff"
:     elseif s:jreg=="#f5f5f5"
:                             let @k = " -- White Smoke             RGB: 245-245-245    HEX: f5f5f5"
:     elseif s:jreg=="#dcdcdc"
:                             let @k = " -- Gainsboro               RGB: 220-220-220    HEX: dcdcdc"
:     elseif s:jreg=="#fffaf0"
:                             let @k = " -- Floral White            RGB: 255-250-240    HEX: fffaf0"
:     elseif s:jreg=="#fdf5e6"
:                             let @k = " -- Old Lace                RGB: 253-245-230    HEX: fdf5e6"
:     elseif s:jreg=="#faf0e6"
:                             let @k = " -- Linen                   RGB: 240-240-230    HEX: faf0e6"
:     elseif s:jreg=="#faebd7"
:                             let @k = " -- Antique White           RGB: 250-235-215    HEX: faebd7"
:     elseif s:jreg=="#eedfcc"
:                             let @k = " -- Antique White 2         RGB: 238-223-204    HEX: eedfcc"
:     elseif s:jreg=="#cdc0b0"
:                             let @k = " -- Antique White 3         RGB: 205-192-176    HEX: cdc0b0"
:     elseif s:jreg=="#8b8378"
:                             let @k = " -- Antique White 4         RGB: 139-131-120    HEX: 8b8378"
:     elseif s:jreg=="#ffefd5"
:                             let @k = " -- Papaya Whip             RGB: 255-239-213    HEX: ffefd5"
:     elseif s:jreg=="#ffebcd"
:                             let @k = " -- Blanched Almond         RGB: 255-235-205    HEX: ffebcd"
:     elseif s:jreg=="#ffe4c4"
:                             let @k = " -- Bisque                  RGB: 255-228-196    HEX: ffe4c4"
:     elseif s:jreg=="#eed5b7"
:                             let @k = " -- Bisque 2                RGB: 238-213-183    HEX: eed5b7"
:     elseif s:jreg=="#cdb79e"
:                             let @k = " -- Bisque 3                RGB: 205-183-158    HEX: cdb79e"
:     elseif s:jreg=="#8b7d6b"
:                             let @k = " -- Bisque 4                RGB: 139-125-107    HEX: 8b7d6b"
:     elseif s:jreg=="#ffdab9"
:                             let @k = " -- Peach Puff              RGB: 255-218-185    HEX: ffdab9"
:     elseif s:jreg=="#eecbad"
:                             let @k = " -- Peach Puff 2            RGB: 238-203-173    HEX: eecbad"
:     elseif s:jreg=="#cdaf95"
:                             let @k = " -- Peach Puff 3            RGB: 205-175-149    HEX: cdaf95"
:     elseif s:jreg=="#8b7765"
:                             let @k = " -- Peach Puff 4            RGB: 139-119-101    HEX: 8b7765"
:     elseif s:jreg=="#ffdead"
:                             let @k = " -- Navajo White            RGB: 255-222-173    HEX: ffdead"
:     elseif s:jreg=="#ffe4b5"
:                             let @k = " -- Moccasin                RGB: 255-228-181    HEX: ffe4b5"
:     elseif s:jreg=="#fff8dc"
:                             let @k = " -- Cornsilk                RGB: 255-248-220    HEX: fff8dc"
:     elseif s:jreg=="#eee8dc"
:                             let @k = " -- Cornsilk 2              RGB: 238-232-205    HEX: eee8dc"
:     elseif s:jreg=="#cdc8b1"
:                             let @k = " -- Cornsilk 3              RGB: 205-200-177    HEX: cdc8b1"
:     elseif s:jreg=="#8b8878"
:                             let @k = " -- Cornsilk 4              RGB: 139-136-120    HEX: 8b8878"
:     elseif s:jreg=="#fffff0"
:                             let @k = " -- Ivory                   RGB: 255-255-240    HEX: fffff0"
:     elseif s:jreg=="#eeeee0"
:                             let @k = " -- Ivory 2                 RGB: 238-238-224    HEX: eeeee0"
:     elseif s:jreg=="#cdcdc1"
:                             let @k = " -- Ivory 3                 RGB: 205-205-193    HEX: cdcdc1"
:     elseif s:jreg=="#8b8b83"
:                             let @k = " -- Ivory 4                 RGB: 139-139-131    HEX: 8b8b83"
:     elseif s:jreg=="#fffacd"
:                             let @k = " -- Lemon Chiffon           RGB: 255-250-205    HEX: fffacd"
:     elseif s:jreg=="#fff5ee"
:                             let @k = " -- Seashell                RGB: 255-245-238    HEX: fff5ee"
:     elseif s:jreg=="#eee5de"
:                             let @k = " -- Seashell 2              RGB: 238-229-222    HEX: eee5de"
:     elseif s:jreg=="#cdc5bf"
:                             let @k = " -- Seashell 3              RGB: 205-197-191    HEX: cdc5bf"
:     elseif s:jreg=="#8b8682"
:                             let @k = " -- Seashell 4              RGB: 139-134-130    HEX: 8b8682"
:     elseif s:jreg=="#f0fff0"
:                             let @k = " -- Honeydew                RGB: 240-255-240    HEX: f0fff0"
:     elseif s:jreg=="#e0eee0"
:                             let @k = " -- Honeydew 2              RGB: 244-238-224    HEX: e0eee0"
:     elseif s:jreg=="#c1cdc1"
:                             let @k = " -- Honeydew 3              RGB: 193-205-193    HEX: c1cdc1"
:     elseif s:jreg=="#838b83"
:                             let @k = " -- Honeydew 4              RGB: 131-139-131    HEX: 838b83"
:     elseif s:jreg=="#f5fffa"
:                             let @k = " -- Mint Cream              RGB: 245-255-250    HEX: f5fffa"
:     elseif s:jreg=="#f0ffff"
:                             let @k = " -- Azure                   RGB: 240-255-255    HEX: f0ffff"
:     elseif s:jreg=="#f0f8ff"
:                             let @k = " -- Alice Blue              RGB: 240-248-255    HEX: f0f8ff"
:     elseif s:jreg=="#e6e6fa"
:                             let @k = " -- Lavender                RGB: 230-230-250    HEX: e6e6fa"
:     elseif s:jreg=="#fff0f5"
:                             let @k = " -- Lavender Blush          RGB: 255-240-245    HEX: fff0f5"
:     elseif s:jreg=="#ffe4e1"
:                             let @k = " -- Misty Rose              RGB: 255-228-225    HEX: ffe4e1"
:     elseif s:jreg=="#ffffff"
:                             let @k = " -- White                   RGB: 255-255-255    HEX: ffffff"
:     elseif s:jreg=="#000000"
:                             let @k = " -- Black                   RGB: 0-0-0          HEX: 000000"
:     elseif s:jreg=="#2f4f4f"
:                             let @k = " -- Dark Slate Gray         RGB: 49-79-79       HEX: 2f4f4f"
:     elseif s:jreg=="#696969"
:                             let @k = " -- Dim Gray                RGB: 105-105-105    HEX: 696969"
:     elseif s:jreg=="#708090"
:                             let @k = " -- Slate Gray              RGB: 112-138-144    HEX: 708090"
:     elseif s:jreg=="#778899"
:                             let @k = " -- Light Slate Gray        RGB: 119-136-153    HEX: 778899"
:     elseif s:jreg=="#bebebe"
:                             let @k = " -- Gray                    RGB: 190-190-190    HEX: bebebe"
:     elseif s:jreg=="#d3d3d3"
:                             let @k = " -- Light Gray              RGB: 211-211-211    HEX: d3d3d3"
:     elseif s:jreg=="#191970"
:                             let @k = " -- Midnight Blue           RGB: 25-25-112      HEX: 191970"
:     elseif s:jreg=="#000080"
:                             let @k = " -- Navy                    RGB: 0-0-128        HEX: 000080"
:     elseif s:jreg=="#6495ed"
:                             let @k = " -- Cornflower Blue         RGB: 100-149-237    HEX: 6495ed"
:     elseif s:jreg=="#483d8b"
:                             let @k = " -- Dark Slate Blue         RGB: 72-61-139      HEX: 483d8b"
:     elseif s:jreg=="#6a5acd"
:                             let @k = " -- Slate Blue              RGB: 106-90-205     HEX: 6a5acd"
:     elseif s:jreg=="#7b68ee"
:                             let @k = " -- Medium Slate Blue       RGB: 123-104-238    HEX: 7b68ee"
:     elseif s:jreg=="#8470ff"
:                             let @k = " -- Light Slate Blue        RGB: 132-112-255    HEX: 8470ff"
:     elseif s:jreg=="#0000cd"
:                             let @k = " -- Medium Blue             RGB: 0-0-205        HEX: 0000cd"
:     elseif s:jreg=="#4169e1"
:                             let @k = " -- Royal Blue              RGB: 65-105-225     HEX: 4169e1"
:     elseif s:jreg=="#0000ff"
:                             let @k = " -- Blue                    RGB: 0-0-255        HEX: 0000ff"
:     elseif s:jreg=="#1e90ff"
:                             let @k = " -- Dodger Blue             RGB: 30-144-255     HEX: 1e90ff"
:     elseif s:jreg=="#00bfff"
:                             let @k = " -- Deep Sky Blue           RGB: 0-191-255      HEX: 00bfff"
:     elseif s:jreg=="#87ceeb"
:                             let @k = " -- Sky Blue                RGB: 135-206-250    HEX: 87ceeb"
:     elseif s:jreg=="#87cefa"
:                             let @k = " -- Light Sky Blue          RGB: 135-206-250    HEX: 87cefa"
:     elseif s:jreg=="#4682b4"
:                             let @k = " -- Steel Blue              RGB: 70-130-180     HEX: 4682b4"
:     elseif s:jreg=="#b0c4de"
:                             let @k = " -- Light Steel Blue        RGB: 176-196-222    HEX: b0c4de"
:     elseif s:jreg=="#add8e6"
:                             let @k = " -- Light Blue              RGB: 173-216-230    HEX: add8e6"
:     elseif s:jreg=="#b0e0e6"
:                             let @k = " -- Powder Blue             RGB: 176-224-230    HEX: b0e0e6"
:     elseif s:jreg=="#afeeee"
:                             let @k = " -- Pale Turquoise          RGB: 175-238-238    HEX: afeeee"
:     elseif s:jreg=="#00ced1"
:                             let @k = " -- Dark Turquoise          RGB: 0-206-209      HEX: 00ced1"
:     elseif s:jreg=="#48d1cc"
:                             let @k = " -- Medium Turquoise        RGB: 72-209-204     HEX: 48d1cc"
:     elseif s:jreg=="#40e0d0"
:                             let @k = " -- Turquoise               RGB: 64-224-208     HEX: 40e0d0"
:     elseif s:jreg=="#00ffff"
:                             let @k = " -- Cyan                    RGB: 0-255-255      HEX: 00ffff"
:     elseif s:jreg=="#e0ffff"
:                             let @k = " -- Light Cyan              RGB: 224-255-255    HEX: e0ffff"
:     elseif s:jreg=="#5f9ea0"
:                             let @k = " -- Cadet Blue              RGB: 95-158-160     HEX: 5f9ea0"
:     elseif s:jreg=="#66cdaa"
:                             let @k = " -- Medium Aquamarine       RGB: 102-205-170    HEX: 66cdaa"
:     elseif s:jreg=="#7fffd4"
:                             let @k = " -- Aquamarine              RGB: 127-255-212    HEX: 7fffd4"
:     elseif s:jreg=="#006400"
:                             let @k = " -- Dark Green              RGB: 0-100-0        HEX: 006400"
:     elseif s:jreg=="#556b2f"
:                             let @k = " -- Dark Olive Green        RGB: 85-107-47      HEX: 556b2f"
:     elseif s:jreg=="#8fbc8f"
:                             let @k = " -- Dark Sea Green          RGB: 143-188-143    HEX: 8fbc8f"
:     elseif s:jreg=="#2e8b57"
:                             let @k = " -- Sea Green               RGB: 46-139-87      HEX: 2e8b57"
:     elseif s:jreg=="#3cb371"
:                             let @k = " -- Medium Sea Green        RGB: 60-179-113     HEX: 3cb371"
:     elseif s:jreg=="#20b2aa"
:                             let @k = " -- Light Sea Green         RGB: 32-178-170     HEX: 20b2aa"
:     elseif s:jreg=="#98fb98"
:                             let @k = " -- Pale Green              RGB: 152-251-152    HEX: 98fb98"
:     elseif s:jreg=="#00ff7f"
:                             let @k = " -- Spring Green            RGB: 0-255-127      HEX: 00ff7f"
:     elseif s:jreg=="#7cfc00"
:                             let @k = " -- Lawn Green              RGB: 124-252-0      HEX: 7cfc00"
:     elseif s:jreg=="#7fff00"
:                             let @k = " -- Chartreuse              RGB: 127-255-0      HEX: 7fff00"
:     elseif s:jreg=="#00fa9a"
:                             let @k = " -- Medium Spring Green     RGB: 0-250-154      HEX: 00fa9a"
:     elseif s:jreg=="#adff2f"
:                             let @k = " -- Green Yellow            RGB: 173-255-47     HEX: adff2f"
:     elseif s:jreg=="#32cd32"
:                             let @k = " -- Lime Green              RGB: 50-205-50      HEX: 32cd32"
:     elseif s:jreg=="#9acd32"
:                             let @k = " -- Yellow Green            RGB: 154-205-50     HEX: 9acd32"
:     elseif s:jreg=="#228b22"
:                             let @k = " -- Forest Green            RGB: 34-139-34      HEX: 228b22"
:     elseif s:jreg=="#6b8e23"
:                             let @k = " -- Olive Drab              RGB: 107-142-35     HEX: 6b8e23"
:     elseif s:jreg=="#bdb76b"
:                             let @k = " -- Dark Khaki              RGB: 189-183-107    HEX: bdb76b"
:     elseif s:jreg=="#f0e68c"
:                             let @k = " -- Khaki                   RGB: 240-230-140    HEX: f0e68c"
:     elseif s:jreg=="#eee8aa"
:                             let @k = " -- Pale Goldenrod          RGB: 238-232-170    HEX: eee8aa"
:     elseif s:jreg=="#fafad2"
:                             let @k = " -- Light Goldenrod Yellow  RGB: 250-250-210    HEX: fafad2"
:     elseif s:jreg=="#ffffe0"
:                             let @k = " -- Light Yellow            RGB: 255-255-224    HEX: ffffe0"
:     elseif s:jreg=="#ffff00"
:                             let @k = " -- Yellow                  RGB: 255-255-0      HEX: ffff00"
:     elseif s:jreg=="#ffd700"
:                             let @k = " -- Gold                    RGB: 255-215-0      HEX: ffd700"
:     elseif s:jreg=="#eedd82"
:                             let @k = " -- Light Goldenrod         RGB: 238-221-130    HEX: eedd82"
:     elseif s:jreg=="#daa520"
:                             let @k = " -- Goldenrod               RGB: 218-165-32     HEX: daa520"
:     elseif s:jreg=="#b8860b"
:                             let @k = " -- Dark Goldenrod          RGB: 184-134-11     HEX: b8860b"
:     elseif s:jreg=="#bc8f8f"
:                             let @k = " -- Rosy Brown              RGB: 188-143-143    HEX: bc8f8f"
:     elseif s:jreg=="#cd5c5c"
:                             let @k = " -- Indian Red              RGB: 205-92-92      HEX: cd5c5c"
:     elseif s:jreg=="#8b4513"
:                             let @k = " -- Saddle Brown            RGB: 139-69-19      HEX: 8b4513"
:     elseif s:jreg=="#a0522d"
:                             let @k = " -- Sienna                  RGB: 160-82-45      HEX: a0522d"
:     elseif s:jreg=="#cd853f"
:                             let @k = " -- Peru                    RGB: 205-133-63     HEX: cd853f"
:     elseif s:jreg=="#deb887"
:                             let @k = " -- Burlywood               RGB: 222-184-135    HEX: deb887"
:     elseif s:jreg=="#f5f5dc"
:                             let @k = " -- Beige                   RGB: 245-245-220    HEX: f5f5dc"
:     elseif s:jreg=="#f5deb3"
:                             let @k = " -- Wheat                   RGB: 245-222-179    HEX: f5deb3"
:     elseif s:jreg=="#f4a460"
:                             let @k = " -- Sandy Brown             RGB: 244-164-96     HEX: f4a460"
:     elseif s:jreg=="#d2b48c"
:                             let @k = " -- Tan                     RGB: 210-180-140    HEX: d2b48c"
:     elseif s:jreg=="#d2691e"
:                             let @k = " -- Chocolate               RGB: 210-105-30     HEX: d2691e"
:     elseif s:jreg=="#b22222"
:                             let @k = " -- Firebrick               RGB: 178-34-34      HEX: b22222"
:     elseif s:jreg=="#a52a2a"
:                             let @k = " -- Brown                   RGB: 165-42-42      HEX: a52a2a"
:     elseif s:jreg=="#e9967a"
:                             let @k = " -- Dark Salmon             RGB: 233-150-122    HEX: e9967a"
:     elseif s:jreg=="#fa8072"
:                             let @k = " -- Salmon                  RGB: 250-128-114    HEX: fa8072"
:     elseif s:jreg=="#ffa07a"
:                             let @k = " -- Light Salmon            RGB: 255-160-122    HEX: ffa07a"
:     elseif s:jreg=="#ffa500"
:                             let @k = " -- Orange                  RGB: 255-165-0      HEX: ffa500"
:     elseif s:jreg=="#ff8c00"
:                             let @k = " -- Dark Orange             RGB: 255-140-0      HEX: ff8c00"
:     elseif s:jreg=="#ff7f50"
:                             let @k = " -- Coral                   RGB: 255-127-80     HEX: ff7f50"
:     elseif s:jreg=="#f08080"
:                             let @k = " -- Light Coral             RGB: 240-128-128    HEX: f08080"
:     elseif s:jreg=="#ff6347"
:                             let @k = " -- Tomato                  RGB: 255-99-71      HEX: ff6347"
:     elseif s:jreg=="#ff4500"
:                             let @k = " -- Orange Red              RGB: 255-69-0       HEX: ff4500"
:     elseif s:jreg=="#ff0000"
:                             let @k = " -- Red                     RGB: 255-0-0        HEX: ff0000"
:     elseif s:jreg=="#ff69b4"
:                             let @k = " -- Hot Pink                RGB: 255-105-180    HEX: ff69b4"
:     elseif s:jreg=="#ff1493"
:                             let @k = " -- Deep Pink               RGB: 255-20-147     HEX: ff1493"
:     elseif s:jreg=="#ffc0cb"
:                             let @k = " -- Pink                    RGB: 255-192-203    HEX: ffc0cb"
:     elseif s:jreg=="#ffb6c1"
:                             let @k = " -- Light Pink              RGB: 255-182-193    HEX: ffb6c1"
:     elseif s:jreg=="#db7093"
:                             let @k = " -- Pale Violet Red         RGB: 219-112-147    HEX: db7093"
:     elseif s:jreg=="#b03060"
:                             let @k = " -- Maroon                  RGB: 176-48-96      HEX: b03060"
:     elseif s:jreg=="#c71585"
:                             let @k = " -- Medium Violet Red       RGB: 199-21-133     HEX: c71585"
:     elseif s:jreg=="#d02090"
:                             let @k = " -- Violet Red              RGB: 208-32-144     HEX: d02090"
:     elseif s:jreg=="#ee82ee"
:                             let @k = " -- Violet                  RGB: 238-130-238    HEX: ee82ee"
:     elseif s:jreg=="#dda0dd"
:                             let @k = " -- Plum                    RGB: 221-160-221    HEX: dda0dd"
:     elseif s:jreg=="#da70d6"
:                             let @k = " -- Orchid                  RGB: 218-112-214    HEX: da70d6"
:     elseif s:jreg=="#ba55d3"
:                             let @k = " -- Medium Orchid           RGB: 186-85-211     HEX: ba55d3"
:     elseif s:jreg=="#9932cc"
:                             let @k = " -- Dark Orchid             RGB: 153-50-204     HEX: 9932cc"
:     elseif s:jreg=="#9400d3"
:                             let @k = " -- Dark Violet             RGB: 148-0-211      HEX: 9400d3"
:     elseif s:jreg=="#8a2be2"
:                             let @k = " -- Blue Violet             RGB: 138-43-226     HEX: 8a2be2"
:     elseif s:jreg=="#a020f0"
:                             let @k = " -- Purple                  RGB: 160-32-240     HEX: a020f0"
:     elseif s:jreg=="#9370db"
:                             let @k = " -- Medium Purple           RGB: 147-112-219    HEX: 9370db"
:     elseif s:jreg=="#d8bfd8"
:                             let @k = " -- Thistle                 RGB: 216-191-216    HEX: d8bfd8"
:       else
:       endif


:endfun"}}}


fun! ColorPalette()"{{{


:let l1="#fffafa""{{{
:let l2="#eee9e9"
:let l3="#cdc9c9"
:let l4="#8b8989"
:let l5="#f8f8ff"
:let l6="#f5f5f5"
:let l7="#dcdcdc"
:let l8="#fffaf0"
:let l9="#fdf5e6"
:let l10="#faf0e6"
:let l11="#faebd7"
:let l12="#eedfcc"
:let l13="#cdc0b0"
:let l14="#8b8378"
:let l15="#ffefd5"
:let l16="#ffebcd"
:let l17="#ffe4c4"
:let l18="#eed5b7"
:let l19="#cdb79e"
:let l20="#8b7d6b"
:let l21="#ffdab9"
:let l22="#eecbad"
:let l23="#cdaf95"
:let l24="#8b7765"
:let l25="#ffdead"
:let l26="#ffe4b5"
:let l27="#fff8dc"
:let l28="#eee8dc"
:let l29="#cdc8b1"
:let l30="#8b8878"
:let l31="#fffff0"
:let l32="#eeeee0"
:let l33="#cdcdc1"
:let l34="#8b8b83"
:let l35="#fffacd"
:let l36="#fff5ee"
:let l37="#eee5de"
:let l38="#cdc5bf"
:let l39="#8b8682"
:let l40="#f0fff0"
:let l41="#e0eee0"
:let l42="#c1cdc1"
:let l43="#838b83"
:let l44="#f5fffa"
:let l45="#f0ffff"
:let l46="#f0f8ff"
:let l47="#e6e6fa"
:let l48="#fff0f5"
:let l49="#ffe4e1"
:let l50="#ffffff"
:let l51="#000000"
:let l52="#2f4f4f"
:let l53="#696969"
:let l54="#708090"
:let l55="#778899"
:let l56="#bebebe"
:let l57="#d3d3d3"
:let l58="#191970"
:let l59="#000080"
:let l60="#6495ed"
:let l61="#483d8b"
:let l62="#6a5acd"
:let l63="#7b68ee"
:let l64="#8470ff"
:let l65="#0000cd"
:let l66="#4169e1"
:let l67="#0000ff"
:let l68="#1e90ff"
:let l69="#00bfff"
:let l70="#87ceeb"
:let l71="#87cefa"
:let l72="#4682b4"
:let l73="#b0c4de"
:let l74="#add8e6"
:let l75="#b0e0e6"
:let l76="#afeeee"
:let l77="#00ced1"
:let l78="#48d1cc"
:let l79="#40e0d0"
:let l80="#00ffff"
:let l81="#e0ffff"
:let l82="#5f9ea0"
:let l83="#66cdaa"
:let l84="#7fffd4"
:let l85="#006400"
:let l86="#556b2f"
:let l87="#8fbc8f"
:let l88="#2e8b57"
:let l89="#3cb371"
:let l90="#20b2aa"
:let l91="#98fb98"
:let l92="#00ff7f"
:let l93="#7cfc00"
:let l94="#7fff00"
:let l95="#00fa9a"
:let l96="#adff2f"
:let l97="#32cd32"
:let l98="#9acd32"
:let l99="#228b22"
:let l100="#6b8e23"
:let l101="#bdb76b"
:let l102="#f0e68c"
:let l103="#eee8aa"
:let l104="#fafad2"
:let l105="#ffffe0"
:let l106="#ffff00"
:let l107="#ffd700"
:let l108="#eedd82"
:let l109="#daa520"
:let l110="#b8860b"
:let l111="#bc8f8f"
:let l112="#cd5c5c"
:let l113="#8b4513"
:let l114="#a0522d"
:let l115="#cd853f"
:let l116="#deb887"
:let l117="#f5f5dc"
:let l118="#f5deb3"
:let l119="#f4a460"
:let l120="#d2b48c"
:let l121="#d2691e"
:let l122="#b22222"
:let l123="#a52a2a"
:let l124="#e9967a"
:let l125="#fa8072"
:let l126="#ffa07a"
:let l127="#ffa500"
:let l128="#ff8c00"
:let l129="#ff7f50"
:let l130="#f08080"
:let l131="#ff6347"
:let l132="#ff4500"
:let l133="#ff0000"
:let l134="#ff69b4"
:let l135="#ff1493"
:let l136="#ffc0cb"
:let l137="#ffb6c1"
:let l138="#db7093"
:let l139="#b03060"
:let l140="#c71585"
:let l141="#d02090"
:let l142="#ee82ee"
:let l143="#dda0dd"
:let l144="#da70d6"
:let l145="#ba55d3"
:let l146="#9932cc"
:let l147="#9400d3"
:let l148="#8a2be2"
:let l149="#a020f0"
:let l150="#9370db"
:let l151="#d8bfd8""}}}


	:let @@="".l51."\n".l50."\n".l1."\n".l2."\n".l3."\n".l4."\n".l5."\n".l6."\n".l7."\n".l8."\n".l9."\n".l10."\n".l11."\n".l12."\n".l13."\n".l14."\n".l15."\n".l16."\n".l17."\n".l18."\n".l19."\n".l20."\n".l21."\n".l22."\n".l23."\n".l24."\n".l25."\n".l26."\n".l27."\n".l28."\n".l29."\n".l30."\n".l31."\n".l32."\n".l33."\n".l34."\n".l35."\n".l36."\n".l37."\n".l38."\n".l39."\n".l40."\n".l41."\n".l42."\n".l43."\n".l44."\n".l45."\n".l46."\n".l47."\n".l48."\n".l49."\n".l52."\n".l53."\n".l54."\n".l55."\n".l56."\n".l57."\n".l58."\n".l59."\n".l60."\n".l61."\n".l62."\n".l63."\n".l64."\n".l65."\n".l66."\n".l67."\n".l68."\n".l69."\n".l70."\n".l71."\n".l72."\n".l73."\n".l74."\n".l75."\n".l76."\n".l77."\n".l78."\n".l79."\n".l80."\n".l81."\n".l82."\n".l83."\n".l84."\n".l85."\n".l86."\n".l87."\n".l88."\n".l89."\n".l90."\n".l91."\n".l92."\n".l93."\n".l94."\n".l95."\n".l96."\n".l97."\n".l98."\n".l99."\n".l100."\n".l101."\n".l102."\n".l103."\n".l104."\n".l105."\n".l106."\n".l107."\n".l108."\n".l109."\n".l110."\n".l111."\n".l112."\n".l113."\n".l114."\n".l115."\n".l116."\n".l117."\n".l118."\n".l119."\n".l120."\n".l121."\n".l122."\n".l123."\n".l124."\n".l125."\n".l126."\n".l127."\n".l128."\n".l129."\n".l130."\n".l131."\n".l132."\n".l133."\n".l134."\n".l135."\n".l136."\n".l137."\n".l138."\n".l139."\n".l140."\n".l141."\n".l142."\n".l143."\n".l144."\n".l145."\n".l146."\n".l147."\n".l148."\n".l149."\n".l150."\n".l151."\n""{{{"}}}

"{{{
	:vnew Palette
	:setlocal modifiable
	:setlocal foldcolumn=0
        :setlocal listchars=
        :setlocal nowrap
	:setlocal winwidth=2
        :setlocal winminwidth=2
        :vertical resize -100
        :setlocal noswapfile
        :setlocal buftype=nofile
        :setlocal bufhidden=delete
	:execute ':normal P"'
        :80 "to preselect cyan color...
        ":call Colorinfo()
"}}}
        

syn match #fffafa   "#fffafa""{{{
syn match #eee9e9   "#eee9e9"
syn match #cdc9c9   "#cdc9c9"
syn match #8b8989   "#8b8989"
syn match #f8f8ff   "#f8f8ff"
syn match #f5f5f5   "#f5f5f5"
syn match #dcdcdc   "#dcdcdc"
syn match #fffaf0   "#fffaf0"
syn match #fdf5e6   "#fdf5e6"
syn match #faf0e6   "#faf0e6"
syn match #faebd7   "#faebd7"
syn match #eedfcc   "#eedfcc"
syn match #cdc0b0   "#cdc0b0"
syn match #8b8378   "#8b8378"
syn match #ffefd5   "#ffefd5"
syn match #ffebcd   "#ffebcd"
syn match #ffe4c4   "#ffe4c4"
syn match #eed5b7   "#eed5b7"
syn match #cdb79e   "#cdb79e"
syn match #8b7d6b   "#8b7d6b"
syn match #ffdab9   "#ffdab9"
syn match #eecbad   "#eecbad"
syn match #cdaf95   "#cdaf95"
syn match #8b7765   "#8b7765"
syn match #ffdead   "#ffdead"
syn match #ffe4b5   "#ffe4b5"
syn match #fff8dc   "#fff8dc"
syn match #eee8dc   "#eee8dc"
syn match #cdc8b1   "#cdc8b1"
syn match #8b8878   "#8b8878"
syn match #fffff0   "#fffff0"
syn match #eeeee0   "#eeeee0"
syn match #cdcdc1   "#cdcdc1"
syn match #8b8b83   "#8b8b83"
syn match #fffacd   "#fffacd"
syn match #fff5ee   "#fff5ee"
syn match #eee5de   "#eee5de"
syn match #cdc5bf   "#cdc5bf"
syn match #8b8682   "#8b8682"
syn match #f0fff0   "#f0fff0"
syn match #e0eee0   "#e0eee0"
syn match #c1cdc1   "#c1cdc1"
syn match #838b83   "#838b83"
syn match #f5fffa   "#f5fffa"
syn match #f0ffff   "#f0ffff"
syn match #f0f8ff   "#f0f8ff"
syn match #e6e6fa   "#e6e6fa"
syn match #fff0f5   "#fff0f5"
syn match #ffe4e1   "#ffe4e1"
syn match #ffffff   "#ffffff"
syn match #000000   "#000000"
syn match #2f4f4f   "#2f4f4f"
syn match #696969   "#696969"
syn match #708090   "#708090"
syn match #778899   "#778899"
syn match #bebebe   "#bebebe"
syn match #d3d3d3   "#d3d3d3"
syn match #191970   "#191970"
syn match #000080   "#000080"
syn match #6495ed   "#6495ed"
syn match #483d8b   "#483d8b"
syn match #6a5acd   "#6a5acd"
syn match #7b68ee   "#7b68ee"
syn match #8470ff   "#8470ff"
syn match #0000cd   "#0000cd"
syn match #4169e1   "#4169e1"
syn match #0000ff   "#0000ff"
syn match #1e90ff   "#1e90ff"
syn match #00bfff   "#00bfff"
syn match #87ceeb   "#87ceeb"
syn match #87cefa   "#87cefa"
syn match #4682b4   "#4682b4"
syn match #b0c4de   "#b0c4de"
syn match #add8e6   "#add8e6"
syn match #b0e0e6   "#b0e0e6"
syn match #afeeee   "#afeeee"
syn match #00ced1   "#00ced1"
syn match #48d1cc   "#48d1cc"
syn match #40e0d0   "#40e0d0"
syn match #00ffff   "#00ffff"
syn match #e0ffff   "#e0ffff"
syn match #5f9ea0   "#5f9ea0"
syn match #66cdaa   "#66cdaa"
syn match #7fffd4   "#7fffd4"
syn match #006400   "#006400"
syn match #556b2f   "#556b2f"
syn match #8fbc8f   "#8fbc8f"
syn match #2e8b57   "#2e8b57"
syn match #3cb371   "#3cb371"
syn match #20b2aa   "#20b2aa"
syn match #98fb98   "#98fb98"
syn match #00ff7f   "#00ff7f"
syn match #7cfc00   "#7cfc00"
syn match #7fff00   "#7fff00"
syn match #00fa9a   "#00fa9a"
syn match #adff2f   "#adff2f"
syn match #32cd32   "#32cd32"
syn match #9acd32   "#9acd32"
syn match #228b22   "#228b22"
syn match #6b8e23   "#6b8e23"
syn match #bdb76b   "#bdb76b"
syn match #f0e68c   "#f0e68c"
syn match #eee8aa   "#eee8aa"
syn match #fafad2   "#fafad2"
syn match #ffffe0   "#ffffe0"
syn match #ffff00   "#ffff00"
syn match #ffd700   "#ffd700"
syn match #eedd82   "#eedd82"
syn match #daa520   "#daa520"
syn match #b8860b   "#b8860b"
syn match #bc8f8f   "#bc8f8f"
syn match #cd5c5c   "#cd5c5c"
syn match #8b4513   "#8b4513"
syn match #a0522d   "#a0522d"
syn match #cd853f   "#cd853f"
syn match #deb887   "#deb887"
syn match #f5f5dc   "#f5f5dc"
syn match #f5deb3   "#f5deb3"
syn match #f4a460   "#f4a460"
syn match #d2b48c   "#d2b48c"
syn match #d2691e   "#d2691e"
syn match #b22222   "#b22222"
syn match #a52a2a   "#a52a2a"
syn match #e9967a   "#e9967a"
syn match #fa8072   "#fa8072"
syn match #ffa07a   "#ffa07a"
syn match #ffa500   "#ffa500"
syn match #ff8c00   "#ff8c00"
syn match #ff7f50   "#ff7f50"
syn match #f08080   "#f08080"
syn match #ff6347   "#ff6347"
syn match #ff4500   "#ff4500"
syn match #ff0000   "#ff0000"
syn match #ff69b4   "#ff69b4"
syn match #ff1493   "#ff1493"
syn match #ffc0cb   "#ffc0cb"
syn match #ffb6c1   "#ffb6c1"
syn match #db7093   "#db7093"
syn match #b03060   "#b03060"
syn match #c71585   "#c71585"
syn match #d02090   "#d02090"
syn match #ee82ee   "#ee82ee"
syn match #dda0dd   "#dda0dd"
syn match #da70d6   "#da70d6"
syn match #ba55d3   "#ba55d3"
syn match #9932cc   "#9932cc"
syn match #9400d3   "#9400d3"
syn match #8a2be2   "#8a2be2"
syn match #a020f0   "#a020f0"
syn match #9370db   "#9370db"
syn match #d8bfd8   "#d8bfd8""}}}


hi #fffafa     guifg=#fffafa guibg=#fffafa"{{{
hi #eee9e9     guifg=#eee9e9 guibg=#eee9e9
hi #cdc9c9     guifg=#cdc9c9 guibg=#cdc9c9
hi #8b8989     guifg=#8b8989 guibg=#8b8989
hi #f8f8ff     guifg=#f8f8ff guibg=#f8f8ff
hi #f5f5f5     guifg=#f5f5f5 guibg=#f5f5f5
hi #dcdcdc     guifg=#dcdcdc guibg=#dcdcdc 
hi #fffaf0     guifg=#fffaf0 guibg=#fffaf0
hi #fdf5e6     guifg=#fdf5e6 guibg=#fdf5e6
hi #faf0e6     guifg=#faf0e6 guibg=#faf0e6
hi #faebd7     guifg=#faebd7 guibg=#faebd7
hi #eedfcc     guifg=#eedfcc guibg=#eedfcc
hi #cdc0b0     guifg=#cdc0b0 guibg=#cdc0b0
hi #8b8378     guifg=#8b8378 guibg=#8b8378
hi #ffefd5     guifg=#ffefd5 guibg=#ffefd5
hi #ffebcd     guifg=#ffebcd guibg=#ffebcd
hi #ffe4c4     guifg=#ffe4c4 guibg=#ffe4c4
hi #eed5b7     guifg=#eed5b7 guibg=#eed5b7
hi #cdb79e     guifg=#cdb79e guibg=#cdb79e
hi #8b7d6b     guifg=#8b7d6b guibg=#8b7d6b
hi #ffdab9     guifg=#ffdab9 guibg=#ffdab9
hi #eecbad     guifg=#eecbad guibg=#eecbad
hi #cdaf95     guifg=#cdaf95 guibg=#cdaf95
hi #8b7765     guifg=#8b7765 guibg=#8b7765
hi #ffdead     guifg=#ffdead guibg=#ffdead
hi #ffe4b5     guifg=#ffe4b5 guibg=#ffe4b5
hi #fff8dc     guifg=#fff8dc guibg=#fff8dc
hi #eee8dc     guifg=#eee8dc guibg=#eee8dc
hi #cdc8b1     guifg=#cdc8b1 guibg=#cdc8b1
hi #8b8878     guifg=#8b8878 guibg=#8b8878
hi #fffff0     guifg=#fffff0 guibg=#fffff0
hi #eeeee0     guifg=#eeeee0 guibg=#eeeee0
hi #cdcdc1     guifg=#cdcdc1 guibg=#cdcdc1
hi #8b8b83     guifg=#8b8b83 guibg=#8b8b83
hi #fffacd     guifg=#fffacd guibg=#fffacd
hi #fff5ee     guifg=#fff5ee guibg=#fff5ee
hi #eee5de     guifg=#eee5de guibg=#eee5de
hi #cdc5bf     guifg=#cdc5bf guibg=#cdc5bf
hi #8b8682     guifg=#8b8682 guibg=#8b8682
hi #f0fff0     guifg=#f0fff0 guibg=#f0fff0
hi #e0eee0     guifg=#e0eee0 guibg=#e0eee0
hi #c1cdc1     guifg=#c1cdc1 guibg=#c1cdc1
hi #838b83     guifg=#838b83 guibg=#838b83
hi #f5fffa     guifg=#f5fffa guibg=#f5fffa
hi #f0ffff     guifg=#f0ffff guibg=#f0ffff
hi #f0f8ff     guifg=#f0f8ff guibg=#f0f8ff
hi #e6e6fa     guifg=#e6e6fa guibg=#e6e6fa
hi #fff0f5     guifg=#fff0f5 guibg=#fff0f5
hi #ffe4e1     guifg=#ffe4e1 guibg=#ffe4e1
hi #ffffff     guifg=#ffffff guibg=#ffffff
hi #000000     guifg=#000000 guibg=#000000
hi #2f4f4f     guifg=#2f4f4f guibg=#2f4f4f
hi #696969     guifg=#696969 guibg=#696969
hi #708090     guifg=#708090 guibg=#708090
hi #778899     guifg=#778899 guibg=#778899
hi #bebebe     guifg=#bebebe guibg=#bebebe
hi #d3d3d3     guifg=#d3d3d3 guibg=#d3d3d3
hi #191970     guifg=#191970 guibg=#191970
hi #000080     guifg=#000080 guibg=#000080
hi #6495ed     guifg=#6495ed guibg=#6495ed
hi #483d8b     guifg=#483d8b guibg=#483d8b
hi #6a5acd     guifg=#6a5acd guibg=#6a5acd
hi #7b68ee     guifg=#7b68ee guibg=#7b68ee
hi #8470ff     guifg=#8470ff guibg=#8470ff
hi #0000cd     guifg=#0000cd guibg=#0000cd
hi #4169e1     guifg=#4169e1 guibg=#4169e1
hi #0000ff     guifg=#0000ff guibg=#0000ff
hi #1e90ff     guifg=#1e90ff guibg=#1e90ff
hi #00bfff     guifg=#00bfff guibg=#00bfff
hi #87ceeb     guifg=#87ceeb guibg=#87ceeb
hi #87cefa     guifg=#87cefa guibg=#87cefa
hi #4682b4     guifg=#4682b4 guibg=#4682b4
hi #b0c4de     guifg=#b0c4de guibg=#b0c4de
hi #add8e6     guifg=#add8e6 guibg=#add8e6
hi #b0e0e6     guifg=#b0e0e6 guibg=#b0e0e6
hi #afeeee     guifg=#afeeee guibg=#afeeee
hi #00ced1     guifg=#00ced1 guibg=#00ced1
hi #48d1cc     guifg=#48d1cc guibg=#48d1cc
hi #40e0d0     guifg=#40e0d0 guibg=#40e0d0
hi #00ffff     guifg=#00ffff guibg=#00ffff
hi #e0ffff     guifg=#e0ffff guibg=#e0ffff
hi #5f9ea0     guifg=#5f9ea0 guibg=#5f9ea0
hi #66cdaa     guifg=#66cdaa guibg=#66cdaa
hi #7fffd4     guifg=#7fffd4 guibg=#7fffd4
hi #006400     guifg=#006400 guibg=#006400
hi #556b2f     guifg=#556b2f guibg=#556b2f
hi #8fbc8f     guifg=#8fbc8f guibg=#8fbc8f
hi #2e8b57     guifg=#2e8b57 guibg=#2e8b57
hi #3cb371     guifg=#3cb371 guibg=#3cb371
hi #20b2aa     guifg=#20b2aa guibg=#20b2aa
hi #98fb98     guifg=#98fb98 guibg=#98fb98
hi #00ff7f     guifg=#00ff7f guibg=#00ff7f
hi #7cfc00     guifg=#7cfc00 guibg=#7cfc00
hi #7fff00     guifg=#7fff00 guibg=#7fff00
hi #00fa9a     guifg=#00fa9a guibg=#00fa9a
hi #adff2f     guifg=#adff2f guibg=#adff2f
hi #32cd32     guifg=#32cd32 guibg=#32cd32
hi #9acd32     guifg=#9acd32 guibg=#9acd32
hi #228b22     guifg=#228b22 guibg=#228b22
hi #6b8e23     guifg=#6b8e23 guibg=#6b8e23
hi #bdb76b     guifg=#bdb76b guibg=#bdb76b
hi #f0e68c     guifg=#f0e68c guibg=#f0e68c
hi #eee8aa     guifg=#eee8aa guibg=#eee8aa
hi #fafad2     guifg=#fafad2 guibg=#fafad2
hi #ffffe0     guifg=#ffffe0 guibg=#ffffe0
hi #ffff00     guifg=#ffff00 guibg=#ffff00
hi #ffd700     guifg=#ffd700 guibg=#ffd700
hi #eedd82     guifg=#eedd82 guibg=#eedd82
hi #daa520     guifg=#daa520 guibg=#daa520
hi #b8860b     guifg=#b8860b guibg=#b8860b
hi #bc8f8f     guifg=#bc8f8f guibg=#bc8f8f
hi #cd5c5c     guifg=#cd5c5c guibg=#cd5c5c
hi #8b4513     guifg=#8b4513 guibg=#8b4513
hi #a0522d     guifg=#a0522d guibg=#a0522d
hi #cd853f     guifg=#cd853f guibg=#cd853f
hi #deb887     guifg=#deb887 guibg=#deb887
hi #f5f5dc     guifg=#f5f5dc guibg=#f5f5dc
hi #f5deb3     guifg=#f5deb3 guibg=#f5deb3
hi #f4a460     guifg=#f4a460 guibg=#f4a460
hi #d2b48c     guifg=#d2b48c guibg=#d2b48c
hi #d2691e     guifg=#d2691e guibg=#d2691e
hi #b22222     guifg=#b22222 guibg=#b22222
hi #a52a2a     guifg=#a52a2a guibg=#a52a2a
hi #e9967a     guifg=#e9967a guibg=#e9967a
hi #fa8072     guifg=#fa8072 guibg=#fa8072
hi #ffa07a     guifg=#ffa07a guibg=#ffa07a
hi #ffa500     guifg=#ffa500 guibg=#ffa500
hi #ff8c00     guifg=#ff8c00 guibg=#ff8c00
hi #ff7f50     guifg=#ff7f50 guibg=#ff7f50
hi #f08080     guifg=#f08080 guibg=#f08080
hi #ff6347     guifg=#ff6347 guibg=#ff6347
hi #ff4500     guifg=#ff4500 guibg=#ff4500
hi #ff0000     guifg=#ff0000 guibg=#ff0000
hi #ff69b4     guifg=#ff69b4 guibg=#ff69b4
hi #ff1493     guifg=#ff1493 guibg=#ff1493
hi #ffc0cb     guifg=#ffc0cb guibg=#ffc0cb
hi #ffb6c1     guifg=#ffb6c1 guibg=#ffb6c1
hi #db7093     guifg=#db7093 guibg=#db7093
hi #b03060     guifg=#b03060 guibg=#b03060
hi #c71585     guifg=#c71585 guibg=#c71585
hi #d02090     guifg=#d02090 guibg=#d02090
hi #ee82ee     guifg=#ee82ee guibg=#ee82ee
hi #dda0dd     guifg=#dda0dd guibg=#dda0dd
hi #da70d6     guifg=#da70d6 guibg=#da70d6
hi #ba55d3     guifg=#ba55d3 guibg=#ba55d3
hi #9932cc     guifg=#9932cc guibg=#9932cc
hi #9400d3     guifg=#9400d3 guibg=#9400d3
hi #8a2be2     guifg=#8a2be2 guibg=#8a2be2
hi #a020f0     guifg=#a020f0 guibg=#a020f0
hi #9370db     guifg=#9370db guibg=#9370db
hi #d8bfd8     guifg=#d8bfd8 guibg=#d8bfd8"}}}


"maparile{{{
        :noremap <buffer><silent> <Up> <Up>EBvE"jy:call Colorinfo()<bar>echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon " - <c-r>j"<bar>:hi User5 guibg=<c-r>j<CR><Down><Up>
        :noremap <buffer><silent> <Down> <Down>EBvE"jy:call Colorinfo()<bar>echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon " - <c-r>j"<bar>:hi User5 guibg=<c-r>j<CR><Up><Down>
        ":noremap <buffer><silent> <LeftMouse> EBvE"jy:call Colorinfo()<bar>echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon " - <c-r>k"<cr><LeftMouse>
        :noremap <buffer><silent> <LeftMouse> EBvE"jy:call Colorinfo()<cr><LeftMouse>
       
        ":map <buffer> c EBvE"jy:echohl <C-R>j<cr>:echo "xx"<CR>
	":nmap <buffer> <RightMouse> EBvE"jy:echohl <C-R>j<cr>:echo "xx"<CR>
        :nmap <buffer><silent> <RightMouse> <LeftMouse>:call Colorinfo()<CR>
        :nmap <buffer><silent> i :echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<cr>
        
        :nmenu 1.200 PopUp.-SEP3-			<Nop>
        :nmenu <silent> 1.201 PopUp.info\ color :echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<cr>

        ":au! BufEnter Palette :map c EBvE"jy:echohl <C-R>j<cr>:echo "xx"<CR>
	":au! BufEnter Palette :nmap <RightMouse> EBvE"jy:echohl <C-R>j<cr>:echo "xx"<CR>
        ":au! BufEnter Palette :noremap <Up> <Up>EBvE"jy:echohl <C-R>j<cr>:echo "xx"<CR><Down><Up>
        ":au! BufEnter Palette :noremap <Down> <Down>EBvE"jy:echohl <C-R>j<cr>:echo "xx"<CR><Up><Down>
        :au! BufEnter Palette :vertical resize -100 
        
        ":au! BufEnter Palette :unmap c
	":au! BufLeave Palette :unmap <LeftMouse>
        ":au! BufLeave Palette :unmap <Up>
        ":au! BufLeave Palette :unmap <Down>
        :au! BufLeave Palette :vertical resize -100 

        :nmap <silent> <Space> mawb"rye:syn match <C-R>jbg "<C-R>r"<Bar>:hi <C-R>jbg guibg=<c-r>j<CR><bar>:hi User5 guibg=<c-r>j<cr><bar>:echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<CR>`a
        :nmap <silent> <M-Space> mawb"rye:syn match <C-R>jfg "<C-R>r"<Bar>:hi <C-R>jfg guifg=<c-r>j<CR><bar>:hi User5 guibg=<c-r>j<cr><bar>:echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<CR>`a
        
        :vmap <silent> <Space> "ryma:syn match bg<C-R>j "<C-R>r"<Bar>:hi bg<C-R>j guibg=<c-r>j<CR><bar>:hi User5 guibg=<c-r>j<cr><bar>:echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<CR>`a
        :vmap <silent> <M-Space> "ryma:syn match fg<C-R>j "<C-R>r"<Bar>:hi fg<C-R>j guifg=<c-r>j<CR><bar>:hi User5 guibg=<c-r>j<cr><bar>:echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<CR>`a

        :vmap <silent> <RightMouse> "ryma:syn match bg<C-R>j "<C-R>r"<Bar>:hi bg<C-R>j guibg=<c-r>j<CR><bar>:hi User5 guibg=<c-r>j<cr><bar>:echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<CR>`a
        :vmap <silent> <M-RightMouse> "ryma:syn match fg<C-R>j "<C-R>r"<Bar>:hi fg<C-R>j guifg=<c-r>j<CR><bar>:hi User5 guibg=<c-r>j<cr><bar>:echohl <C-R>j<bar>echon "  "<bar>echohl None<bar>echon "<c-r>k"<CR>`a

"}}}
	

endfunction"}}}

:command! Palette :call ColorPalette()<bar>syntax reset
:nmap ;p :call ColorPalette()<bar>syntax reset<cr>
