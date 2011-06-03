"------------------------------------------------------------------------------
"  Description: Vim Ada filetype detection file
"     Language: Ada (2005)
"          $Id: ada.vim 214 2006-05-25 09:24:57Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik 
"      $Author: krischik $
"        $Date: 2006-05-25 11:24:57 +0200 (Do, 25 Mai 2006) $
"      Version: 2.0
"    $Revision: 214 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftdetect/ada.vim $
"      History:	24.05.2006 MK Unified Headers
"	 Usage: copy to colors directory
"------------------------------------------------------------------------------

if exists("s:loaded_ftdetect_ada")
    finish
else
    let s:loaded_ftdetect_ada=1
    if has("vms")
        autocmd BufNewFile,BufRead *.gpr,*.ada_m,*.adc setfiletype ada
    else
        autocmd BufNewFile,BufRead *.gpr setfiletype ada
    endif
    finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2005,2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
" vim: filetype=vim encoding=utf8 fileformat=unix
