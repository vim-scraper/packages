function! OpenFilesFromClipboard(command)
  let clipboard = @*
  if clipboard =~ '^\s*$'
    let clipboard = @+
  endif

  let lines = split(clipboard, "\n")
  for line in lines
      let ifile     = line
      let file_line = 0

      if ifile =~ ':'

        let file_parts = split(ifile, ':')

        " Unix implementation
        if has('unix')
          let ifile = file_parts[0]

          if file_parts[1] =~ '^\d\+$'
            let file_line = file_parts[1]
          endif
        " Window implementation
        else
          let last_part  = len(file_parts)-2

          if last_part >= 1
            let ifile = join(file_parts[0:last_part], ':')

            if file_parts[last_part+1] =~ '^\s*\d\+\s*$'
              let file_line = substitute(file_parts[last_part+1], '\s\+', '', 'g')
            else
              let ifile = line
              let file_line = 0
              call confirm(ifile)
            endif

          endif
        endif
      endif


      if ifile =~ '^".\+"$'
        let ifile = strpart(ifile, 1, strlen(ifile)-1)
      endif

      execute a:command . ' ' printf('+%d', file_line) ifile
  endfor

endfunction
