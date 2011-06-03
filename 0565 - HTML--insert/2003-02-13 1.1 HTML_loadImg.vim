" Script create :IMG command, whitch insert HTML IMG tag
" with fill atributes width and height.
"
" It's neeid identify utility from ImageMagic!
"
" (c) Petr Mach <gsl@seznam.cz>
" http://iglu.cz/wraith/
"
" Call it without parameter for file browser for selecting image.
"
"

function HTML_loadImg(...)
  if(a:0>0)
    let path=a:1
  else  
    if(has("browse"))
      let path=browse('', 'Vyber obrazek pro <img>', '.', '')
    else
      echo "Your VIM not supported cmd :browse, insert image path manualy."
      return
    endif  
  endif  
    let data=system('identify ' . path)
    let old_x=@x
    let @x=substitute(data, '\([^ ]*\) [^ ]* \([0-9]*\)x\([0-9]*\).*', '<img src="\1" width="\2" height="\3" alt="">', '')
    if(data==@x)
      echo "VIM: Error, I can not detect geometry of image!\n" . @x
      return
    endif  
    normal "xph
    let@x=old_x
    startinsert
endfunction

command -nargs=? -complete=file IMG call HTML_loadImg(<f-args>)
