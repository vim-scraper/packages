
" Templates 
"
" Templates directory
let g:TEMPLATE_HOME = "/your/path/to/directory/with/templates"
"
" TT c	--> print template TEMPLATE_HOME/c on top of file (template top)
"
command -narg=1 TT :execute "0read ".TEMPLATE_HOME.expand("/<arg>")
"
" TH f	--> print template TEMPLATE_HOME/c on current line (template here)
"
command -narg=1 TH :execute ".read ".TEMPLATE_HOME.expand("/<arg>")
"
" LST   --> lists the templates directory
"
command -narg=0 LST :execute "!ls ".TEMPLATE_HOME

