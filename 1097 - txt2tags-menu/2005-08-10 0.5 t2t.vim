"------------------------------------------------------------------------------------------
"
"      Filename:    t2t.vim
"   Description:    vim script to support txt2tags
"        Author:    Otávio Corrêa Cordeiro
"         Email:    otavio (at)  geek42 (dot) org
"          Date:    16 - March, 2005
"       Version:    0.5
"
"------------------------------------------------------------------------------------------
"    Copyright:  Copyright (C) 2004 Otávio Corrêa Cordeiro
"
"                This program is free software; you can redistribute it and/or modify
"                it under the terms of the GNU General Public License as published by
"                the Free Software Foundation; either version 2 of the License, or
"                (at your option) any later version.
"
"                This program is distributed in the hope that it will be useful,
"                but WITHOUT ANY WARRANTY; without even the implied warranty of
"                MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"                GNU General Public License for more details.
"
"                You should have received a copy of the GNU General Public License
"                along with this program; if not, write to the Free Software
"                Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"------------------------------------------------------------------------------------------

amenu &txt2tags.Header                                 <Esc><Esc>aTitle<CR>Author<CR>%%date(%m/%d/%Y)<CR>

amenu &txt2tags.Settings.Target.html                   <Esc><Esc>a%!target: html<CR>
imenu &txt2tags.Settings.Target.html                   <Esc><Esc>a%!target: html<CR>
amenu &txt2tags.Settings.Target.xhtml                  <Esc><Esc>a%!target: xhtml<CR>
imenu &txt2tags.Settings.Target.xhtml                  <Esc><Esc>a%!target: xhtml<CR>
amenu &txt2tags.Settings.Target.tex                    <Esc><Esc>a%!target: tex<CR>
imenu &txt2tags.Settings.Target.tex                    <Esc><Esc>a%!target: tex<CR>
amenu &txt2tags.Settings.Target.man                    <Esc><Esc>a%!target: man<CR>
imenu &txt2tags.Settings.Target.man                    <Esc><Esc>a%!target: man<CR>
amenu &txt2tags.Settings.Target.mgp                    <Esc><Esc>a%!target: mgp<CR>
imenu &txt2tags.Settings.Target.mgp                    <Esc><Esc>a%!target: mgp<CR>
amenu &txt2tags.Settings.Target.sgml                   <Esc><Esc>a%!target: sgml<CR>
imenu &txt2tags.Settings.Target.sgml                   <Esc><Esc>a%!target: sgml<CR>

amenu &txt2tags.Settings.Options.add                   <Esc><Esc>a%!options(target): <CR>
imenu &txt2tags.Settings.Options.add                   <Esc><Esc>a%!options(target): <CR>
amenu &txt2tags.Settings.Options.html                  <Esc><Esc>a%!options(html): --toc --style foo.css --toc-level N<CR>
imenu &txt2tags.Settings.Options.html                  <Esc><Esc>a%!options(html): --toc --style foo.css --toc-level N<CR>
amenu &txt2tags.Settings.Options.sgml                  <Esc><Esc>a%!options(sgml): --toc --toc-level N<CR>
imenu &txt2tags.Settings.Options.sgml                  <Esc><Esc>a%!options(sgml): --toc --toc-level N<CR>
amenu &txt2tags.Settings.Options.txt                   <Esc><Esc>a%!options(txt): --toc --toc-level N<CR>
imenu &txt2tags.Settings.Options.txt                   <Esc><Esc>a%!options(txt): --toc --toc-level N<CR>
amenu &txt2tags.Settings.Options.tex                   <Esc><Esc>a%!options(tex): --toc --enum-title<CR>
imenu &txt2tags.Settings.Options.tex                   <Esc><Esc>a%!options(tex): --toc --enum-title<CR>

"amenu &txt2tags.Settings.Include

amenu &txt2tags.Settings.Style.add                     <Esc><Esc>a%!style: <CR>
imenu &txt2tags.Settings.Style.add                     <Esc><Esc>a%!style: <CR>
amenu &txt2tags.Settings.Style.tex                     <Esc><Esc>a%!style(tex): <CR>
imenu &txt2tags.Settings.Style.tex                     <Esc><Esc>a%!style(tex): <CR>
amenu &txt2tags.Settings.Style.html                    <Esc><Esc>a%!style(html): <CR>
imenu &txt2tags.Settings.Style.html                    <Esc><Esc>a%!style(html): <CR>

amenu &txt2tags.Settings.Encoding\ (iso-8859-1)        <Esc><Esc>a%!encoding: iso-8859-1<CR>
imenu &txt2tags.Settings.Encoding\ (iso-8859-1)        <Esc><Esc>a%!encoding: iso-8859-1<CR>
amenu &txt2tags.Settings.Encoding\ (utf-8)             <Esc><Esc>a%!encoding: utf-8<CR>
imenu &txt2tags.Settings.Encoding\ (utf-8)             <Esc><Esc>a%!encoding: utf-8<CR>

"amenu &txt2tags.Settings.IncludeConf
"amenu &txt2tags.Settings.GuiColors

amenu &txt2tags.Settings.PreProc                       <Esc><Esc>a%!preproc: <CR>
imenu &txt2tags.Settings.PreProc                       <Esc><Esc>a%!preproc: <CR>
amenu &txt2tags.Settings.PostProc                      <Esc><Esc>a%!postproc: <CR>
imenu &txt2tags.Settings.PostProc                      <Esc><Esc>a%!postproc: <CR>

amenu &txt2tags.-SEP1-                                 :

amenu &txt2tags.Section                                <Esc><Esc>a=  =<Esc><Left>i
imenu &txt2tags.Section                                <Esc><Esc>a=  =<Esc><Left>i
vmenu &txt2tags.Section                                di=  =<Esc><Left>Pla

amenu &txt2tags.SubSection                             <Esc><Esc>a==  ==<Esc><Left><Left>i
imenu &txt2tags.SubSection                             <Esc><Esc>a==  ==<Esc><Left><Left>i
vmenu &txt2tags.SubSection                             di==  ==<Esc><Left><Left>Pla

amenu &txt2tags.SubSubSection                          <Esc><Esc>a===  ===<Esc><Left><Left>i
imenu &txt2tags.SubSubSection                          <Esc><Esc>a===  ===<Esc><Left><Left>i
vmenu &txt2tags.SubSubSection                          di===  ===<Esc><Left><Left>Pla


amenu &txt2tags.-SEP2-                                 :

amenu &txt2tags.Beautifiers.Bold                       <Esc><Esc>a****<Esc><Left>i
vmenu &txt2tags.Beautifiers.Bold                       di****<Esc><Left>Pla
amenu &txt2tags.Beautifiers.Italic                     <Esc><Esc>a////<Esc><Left>i
vmenu &txt2tags.Beautifiers.Italic                     di////<Esc><Left>Pla
amenu &txt2tags.Beautifiers.Underline                  <Esc><Esc>a____<Esc><Left>i
vmenu &txt2tags.Beautifiers.Underline                  di____<Esc><Left>Pla
amenu &txt2tags.Beautifiers.Verbatim                   <Esc><Esc>a````<Esc><Left>i
vmenu &txt2tags.Beautifiers.Verbatim                   di````<Esc><Left>Pla

amenu &txt2tags.-SEP3-                                 :

amenu &txt2tags.Text\ Blocks.Quote                     <Esc><Esc>a<Tab>
imenu &txt2tags.Text\ Blocks.Quote                     <Esc><Esc>a<Tab>
amenu &txt2tags.Text\ Blocks.Verbatim                  <Esc><Esc>a```<Cr><Cr>```<Up>
imenu &txt2tags.Text\ Blocks.Verbatim                  <Esc><Esc>a```<Cr><Cr>```<Up>
vmenu &txt2tags.Text\ Blocks.Verbatim                  Di```<Cr>```<Cr><Esc>2kp
amenu &txt2tags.Text\ Blocks.List                      <Esc><Esc>i- 
imenu &txt2tags.Text\ Blocks.List                      <Esc><Esc>i- 
vmenu &txt2tags.Text\ Blocks.List                      :s/^/- /<Cr>
amenu &txt2tags.Text\ Blocks.Numbered\ List            <Esc><Esc>i+ 
imenu &txt2tags.Text\ Blocks.Numbered\ List            <Esc><Esc>i+ 
vmenu &txt2tags.Text\ Blocks.Numbered\ List            :s/^/\+ /<Cr>
amenu &txt2tags.Text\ Blocks.Definition\ List          <Esc><Esc>i: 
imenu &txt2tags.Text\ Blocks.Definition\ List          <Esc><Esc>i: 
vmenu &txt2tags.Text\ Blocks.Definition\ List          :s/^/: /<Cr>

amenu &txt2tags.-SEP4-                                 :

amenu &txt2tags.Separators.Thin                         <Esc><Esc>a--------------------------------------------------------<Cr>
imenu &txt2tags.Separators.Thin                         <Esc><Esc>a--------------------------------------------------------<Cr>
amenu &txt2tags.Separators.Large                        <Esc><Esc>a========================================================<Cr>
imenu &txt2tags.Separators.Large                        <Esc><Esc>a========================================================<Cr>

amenu &txt2tags.-SEP5-                                 :

amenu &txt2tags.Comments                               <Esc><Esc>a% 
imenu &txt2tags.Comments                               <Esc><Esc>a% 

amenu &txt2tags.-SEP666-                               :

amenu &txt2tags.About                                  :echo "\ngVIM-menu for txt2tags (http://txt2tags.sourceforge.net/)\nby Otávio Corrêa Cordeiro <otavio(at)geek42(dot)org>\n"<Cr>
