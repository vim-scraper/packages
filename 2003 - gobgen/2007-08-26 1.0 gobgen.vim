" Vim GObject generator plugin
" Last Change: 2007 Aug 26
" Maintainer: Andrey Dubravin <daa84@inbox.ru>
" License: This file is placed in the public domain.

if exists("loaded_gobgen")
    finish
endif
let loaded_gobgen = 1


function! GOBGenerateC()
    if input("Use filename to generate object name (y/n)?", "y") != "y"
	let prefix = input("Enter object prefix name (flybird-directory):")
    else
	let prefix = expand("%:t:r")
    endif

    if prefix == ""
	echohl ErrorMsg
	echo "Can't create class without prefix"
	echohl None
	return
    endif

    let prefix = substitute(prefix, "-", "_", "g")

    let typeName = substitute(prefix, "_\\(.\\)\\|^\\(.\\)", "\\U\\1\\U\\2", "g")
    let typeNamePrivate = typeName . "Private"

    let defineName = toupper(prefix)

    exec "normal oG_DEFINE_TYPE (" . typeName . "," prefix . ", <ENTER_TYPE_HERE>);"
    normal o
    exec "normal o#define" defineName . "_GET_PRIVATE(o)\\"
    exec "normal o(G_TYPE_INSTANCE_GET_PRIVATE ((o)," defineName . "_TYPE," typeNamePrivate . "))"

    normal o
    exec "normal ostruct _" . typeNamePrivate "{"
    normal o};

    normal 2o

    " dispose
    normal ostatic void
    exec "normal o" . prefix . "_dispose (" . typeName "*self)"
    normal o{
    normal o}
    normal o

    " finalize
    normal ostatic void
    exec "normal o" . prefix . "_finalize (" . typeName "*self)"
    normal o{
    normal o}
    normal o

    " init
    normal ostatic void
    exec "normal o" . prefix . "_init (" . typeName "*self)"
    normal o{
    exec "normal o" . typeNamePrivate "*priv;"
    normal o
    exec "normal opriv =" defineName . "_GET_PRIVATE (self);"
    normal o}
    normal o

    " class init
    normal ostatic void
    exec "normal o" . prefix . "_class_init (" . typeName . "Class *self_class)"
    normal o{

    normal oGObjectClass *object_class = G_OBJECT_CLASS (self_class);
    normal o
    exec "normal og_type_class_add_private (self_class, sizeof (" . typeNamePrivate . "));"

    exec "normal oobject_class->dispose =" prefix . "_dispose;"
    exec "normal oobject_class->finalize =" prefix . "_finalize;"

    normal o}
    normal o
endfunction


function! GOBGenerateH()
    if input("Use filename to generate object name (y/n)?", "y") != "y"
	let prefix = input("Enter object prefix name (flybird-directory):")
    else
	let prefix = expand("%:t:r")
    endif

    if prefix == ""
	echohl ErrorMsg
	echo "Can't create class without prefix"
	echohl None
	return
    endif

    let parentName = input("Enter parent class name (Default GObject):")
    if parentName == ""
	let parentName = "GObject"
    endif

    let prefix = substitute(prefix, "-", "_", "g")

    let typeName = substitute(prefix, "_\\(.\\)\\|^\\(.\\)", "\\U\\1\\U\\2", "g")
    let typeNamePrivate = typeName . "Private"

    let defineName = toupper(prefix)

    normal IG_BEGIN_DECLS
    normal o
    exec "normal otypedef struct _" . typeNamePrivate typeNamePrivate . ";"
    normal o

    " Variable memeber structure
    normal otypedef struct {
    exec "normal o" . parentName "parent;"
    normal o
    exec "normal o" . typeNamePrivate "*priv;"
    exec "normal o}" typeName . ";"
    normal o

    normal otypedef struct {
    exec "normal o" . parentName . "Class parent;"
    exec "normal o}" typeName . "Class;"
    normal o

    " setup needed defines
    exec "normal o#define" defineName . "_TYPE (" . prefix . "_get_type ())"
    exec "normal o#define" defineName . "(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), " . defineName . "_TYPE," typeName . "))"
    exec "normal o#define" defineName . "_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), " . defineName . "_TYPE," typeName . "Class))"
    exec "normal o#define IS_" . defineName . "(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), " . defineName . "_TYPE))"
    exec "normal o#define IS_" . defineName . "_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), " . defineName . "_TYPE))"
    exec "normal o#define" defineName . "_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), " . defineName . "_TYPE," typeName . "Class))"

    normal 3o

    normal oG_END_DECLS
endfunction

command! GOBGenerateC call GOBGenerateC()
command! GOBGenerateH call GOBGenerateH()
