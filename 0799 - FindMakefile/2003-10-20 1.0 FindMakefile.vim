function! FindMakefile ()
  exec "cd " . expand ("%:p:h")
  while ! filereadable (getcwd () . "/Makefile") && getcwd () != "/"
    cd ..
  endwhile
  cd .
endfunction
