" pyab.vim by Ryan (gt3) Kulla
" Description: Abbreviates standard python modules 
" Version: 0.1
" Date: Dec 2003
" 
" Usage: :source pyab.vim
" 
" When editing a file in insert-mode, type the abbreviation followed by a space so 
" that "os.pa " becomes "os.path". If you dont want to use an abbreviation then type the first 
" letter of the abbreviation, then a space, then the rest, then backsapce over the space.
" Example: "os.p a", then backsapce over the space between p and a.
"
" pyab has rules rule for abbreviations. 
" You'll always type things as:
" 
"   module.<first 2 letters of attribute><space> 
"   
" if other attributes have the same first 2 letters then you'd type:
" 
"   module.<first 2 letters of attribute><number><space>
"   
" For example:  
"   os.ge   = os.getcwd(
"   os.ge2  = os.getcwdu(
"   os.ge3  = os.getenv(
"   os.ge4  = os.getpid(
"   os.is   = os.isatty(
"   os._pi  = os._pickle_stat_result
"   os._pi2 = os._pickle_statvfs_result 
"
" The exception to this rule is non-object abbrevations in the "Misc" section below and methods with _'s
"
" This script is only covers the os module right now. Your feedback is very important. if you find it useful 
" please email me (ambiod@sbcglobal.net) and i'll continue to add module support, such as all the standard 
" python modules, Tkinter, WxPython, Pygame, PIL, etc. Suggestions are also welcome!


fun! GetString()
    let s:c = getchar()
    if s:c != 0
        let s:s = nr2char(s:c)
    endif
    return s:s
endfun


fun! DelChar(pat)
    let s:s = GetString()
    return (s:s =~ a:pat) ? '' : s:s
endfun


"make a command called Abx that doesn't append a space after an abbreviation is expanded:
command! -nargs=+ Abx execute "iabbr" <q-args> . "<C-R>=DelChar('\\s')<CR>"


set isk+=. "allow the period in foo.bar to be interpreted


"Misc.
ab #! #!/usr/bin/env python<CR>
Abx __b __builtins__
Abx __de __debug__
Abx __do __doc__
Abx __i __import__
Abx __n __name__


"os
Abx os.F_O os.F_OK
Abx os.O_A os.O_APPEND
Abx os.O_B OS.O_BINARY
Abx os.O_C os.O_CREAT
Abx os.O_E os.O_EXCL
Abx os.O_N os.O_NOINHERIT
Abx os.O_RA os.O_RANDOM
Abx os.O_RD os.O_RDONLY
Abx os.O_RD2 os.O_RDWR
Abx os.O_SE os.O_SEQUENTIAL
Abx os.O_SH os.O_SHORT_LIVED
Abx os.O_TE os.O_TEMPORARY
Abx os.O_TE2 os.O_TEXT
Abx os.O_TR os.O_TRUNC
Abx os.O_W os.O_WRONLY
Abx os.P_D os.P_DETACH
Abx os.P_NO os.P_NOWAIT
Abx os.P_NO2 os.P_NOWAITO
Abx os.P_O os.P_OVERLAY
Abx os.P_W os.P_WAIT
Abx os.R_O os.R_OK
Abx os.TM os.TMP_MAX
Abx os.Us os.UserDict
Abx os.W_O os.W_OK
Abx os.X_O os.X_OK
Abx os.__al os.__all__
Abx os.__bu os.__builtins__
Abx os.__do os.__doc__
Abx os.__fi os.__file__
Abx os.__na os.__name__
Abx os._En os._Environ
Abx os._co os._copy_reg
Abx os._ex os._execvpe
Abx os._ex2 os._exists
Abx os._ex3 os._exit
Abx os._ge os._get_exports_list
Abx os._ma os._make_stat_result
Abx os._ma2 os._make_statvfs_result
Abx os._pi os._pickle_stat_result
Abx os._pi2 os._pickle_statvfs_result
Abx os.ab os.abort(
Abx os.ac os.access(
Abx os.al os.altsep
Abx os.ch os.chdir(
Abx os.ch2 os.chmod(
Abx os.cl os.close(
Abx os.cu os.curdir
Abx os.de os.defpath
Abx os.du os.dup(
Abx os.du2 os.dup2(
Abx os.en os.environ
Abx os.er os.error
Abx os.ex os.execl(
Abx os.ex2 os.execle(
Abx os.ex3 os.execlp(
Abx os.ex4 os.execlpe(
Abx os.ex5 os.execv(
Abx os.ex6 os.execve(
Abx os.ex7 os.execvp(
Abx os.ex8 os.execvpe(
Abx os.ex9 os.extsep
Abx os.fd os.fdopen(
Abx os.fs os.fstat(
Abx os.fs2 os.fsync(
Abx os.ge os.getcwd(
Abx os.ge2 os.getcwdu(
Abx os.ge3 os.getenv(
Abx os.ge4 os.getpid(
Abx os.is os.isatty(
Abx os.li os.linesep
Abx os.li2 os.listdir(
Abx os.ls os.lseek(
Abx os.ls2 os.lstat(
Abx os.ma os.makedirs(
Abx os.mk os.mkdir(
Abx os.na os.name
Abx os.op os.open(
Abx os.pa os.pardir
Abx os.pa2 os.path
Abx os.pa3 os.pathsep
Abx os.pi os.pipe(
Abx os.po os.popen(
Abx os.po2 os.popen2(
Abx os.po3 os.popen3(
Abx os.po4 os.popen4(
Abx os.pu os.putenv(
Abx os.re os.read(
Abx os.re2 os.remove(
Abx os.re3 os.removedirs(
Abx os.re4 os.rename(
Abx os.re5 os.renames(
Abx os.rm os.rmdir(
Abx os.se os.sep
Abx os.sp os.spawnl(
Abx os.sp2 os.spawnle(
Abx os.sp3 os.spawnv(
Abx os.sp4 os.spawnve(
Abx os.st os.startfile(
Abx os.st2 os.stat(
Abx os.st3 os.stat_float_times(
Abx os.st4 os.stat_result
Abx os.st5 os.statvfs_result
Abx os.st6 os.strerror(
Abx os.sy os.sys
Abx os.sy2 os.system(
Abx os.te os.tempnam(
Abx os.ti os.times(
Abx os.tm os.tmpfile(
Abx os.tm2 os.tmpnam(
Abx os.um os.umask(
Abx os.un os.unlink(
Abx os.un2 os.unsetenv(
Abx os.ut os.utime(
Abx os.wa os.waitpid(
Abx os.wa2 os.walk(
Abx os.wr os.write(


"os.path
Abx os.path.__a os.path.__all__
Abx os.path.__b os.path.__builtins__
Abx os.path.__d os.path.__doc__
Abx os.path.__f os.path.__file__
Abx os.path.__n os.path.__name__
Abx os.path.ab os.path.abspath(
Abx os.path.al os.path.altsep
Abx os.path.ba os.path.basename(
Abx os.path.co os.path.commonprefix(
Abx os.path.cu os.path.curdir
Abx os.path.de os.path.defpath
Abx os.path.di os.path.dirname(
Abx os.path.ex os.path.exists(
Abx os.path.ex2 os.path.expanduser(
Abx os.path.ex3 os.path.expandvars(
Abx os.path.ex4 os.path.extsep
Abx os.path.ge os.path.getatime(
Abx os.path.ge2 os.path.getctime(
Abx os.path.ge3 os.path.getmtime(
Abx os.path.ge4 os.path.getsize(
Abx os.path.is os.path.isabs(
Abx os.path.is2 os.path.isdir(
Abx os.path.is3 os.path.isfile(
Abx os.path.is4 os.path.islink(
Abx os.path.is5 os.path.ismount(
Abx os.path.is6 os.path.join(
Abx os.path.no os.path.normcase(
Abx os.path.no2 os.path.normpath(
Abx os.path.pa os.path.pardir
Abx os.path.pa2 os.path.pathsep
Abx os.path.re os.path.realpath(
Abx os.path.se os.path.sep
Abx os.path.sp os.path.split(
Abx os.path.sp2 os.path.splitdrive(
Abx os.path.sp3 os.path.splitext(
Abx os.path.sp4 os.path.splitunc(
Abx os.path.st os.path.stat(
Abx os.path.su os.path.supports_unicode_filenames
Abx os.path.wa os.path.walk(

"sys
Abx sys.ex sys.exit(
Abx sys.ar sys.argv[

"commands

"time

"pygame

"wxpython

"etc
