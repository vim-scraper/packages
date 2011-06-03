" ============================================================================
" ScalaCommenter.vim
"
" $Id: scalacommenter.vim 307 2010-04-09 01:07:43Z  $
"
" Manage ScalaDoc comments for classes, traits, objects, methods, vals and vars:
"  Generate comment templates and
"  Format existing comment '@' tag lines
"
" The comment generation code here is strongly based upon 
" Kalle Bjorklid's JCommenter
"   http://www.vim.org/scripts/script.php?script_id=20
" The basic operational approach, utility functions, some recognition patterns
" and many of the configuration parameters are all from JCommenter.
"
" The parameter algorithm has been re-words, a template parameter 
" algorithm has been added Scala; as well as recognizing class 
" constructors, values and variables.
"
" That said, I fully acknowledge JCommenter as the inspiration 
" and basis for this Scala comment generation tool.
"
" The formatting part of this script, however, is all home grown.
"
" Caveats: 
" The approach to recognizing template parameters only works for
"   simple cases. Something like [Null >: A <: AnyRef] will not be
"   correctly read.
" When method starts with "get", "is", or "has" and the method takes no
"   parameter, it should be possible to automagically produce the comment 
"   documentation and return information.
" When method starts with "set" and the method takes only one
"   parameter, it should be possible to automagically produce the comment 
"   documentation and parameter information.
" When formatting tags, no attempt is made to re-order 2 or more tags
"   of the same type (i.e., @throws listed alphabetically, or @see tags)
" The formatter has no knowledge of embedded HTML tags.
"
" ============================================================================

" ============================================================================
" Configuration Options
"   These help control the behavior of ScalaComment.vim
"   Remember, if you change these and then upgrade to a later version, 
"   your changes will be lost
" ============================================================================

let s:DISABLED = 0
let s:ENABLED = 1

" Move cursor to the place where inserting comments supposedly should start
let b:scommenter_move_cursor = s:ENABLED

" Defines whether to move the cursor to the line which has "/**", or the line
"   after that (effective only if b:scommenter_move_cursor is enabled)
let b:scommenter_description_starts_from_first_line = s:DISABLED

" Start insert mode after calling the commenter. Effective only if 
"   b:scommenter_move_cursor is enabled.
let b:scommenter_autostart_insert_mode = s:DISABLED

" The number of empty rows (containing only the star) to be added for the 
"   description of the method
let b:scommenter_method_description_space = 1

" The number of empty rows (containing only the star) to be added for the 
"   description of the class
let b:scommenter_class_description_space = 2

" The number of empty rows (containing only the star) to be added for the 
"   description of the object
let b:scommenter_object_description_space = 2

" The number of empty rows (containing only the star) to be added for the 
"   description of the trait
let b:scommenter_trait_description_space = 2

" The number of empty rows (containing only the star) to be added for the 
"   description of the inner class
let b:scommenter_inner_class_description_space = 1

" The number of empty rows (containing only the star) to be added for the 
"   description of the inner object
let b:scommenter_inner_object_description_space = 1

" The number of empty rows (containing only the star) to be added for the 
"   description of the inner trait
let b:scommenter_inner_trait_description_space = 1

" The number of empty rows (containing only the star) to be added for the´
"   description of the field. Can be also -1, which means that "/**  */" is 
"   added above the field declaration 
let b:scommenter_field_description_space = 1

" If this option is enabled, and a method has no exceptions, parameters,
"   template parameters or return value, the space for the description of 
"   that method is allways one row. This is handy if you want to keep an 
"   empty line between the description and the tags, as is defined in 
"   Sun's java code conventions
let b:scommenter_smart_description_spacing = s:ENABLED

" For top-level classes with parameters and template parameters and traits with
"   template parameters, if enabled then an empty line separates the 
"   @since tag and any @param and/or @tparam tags.
let b:scommenter_smart_since_spacing = s:ENABLED

" The default content for the author-tag of class-comments. Leave empty to add
"   just the empty tag, or outcomment to prevent author tag generation
let b:scommenter_class_author = 'Ada Lovelace'

" Include '@version version, date' in class/trait/object comments
"   Used to indicate the current version of the particular class/trait/object.
let b:scommenter_class_version = '1.0, ' . strftime("%d/%m/%y")

" Include '@since since_release' in class/trait/object comments
"   Used to indicate that the class/trait/object has been part of the
"   application since a given release.
let b:scommenter_since_release = '1.0'

" The default content for the version-tag of class-comments. Leave empty to add
"   just the empty tag, or comment-out to prevent version tag generation
let b:scommenter_class_svn_id = '$Id: scalacommenter.vim 307 2010-04-09 01:07:43Z  $'

" The default author added to the file comments. Leave empty to add just the
"   field where the author can be added, or comment-out to remove it.
let b:scommenter_file_author = 'Ada Lovelace'

" The default copyright holder added to the file comments. Leave empty to
"   add just the field where the copyright info can be added, or comment-out
"   to remove it.
let b:scommenter_file_copyright_line = ''
let b:scommenter_company_name = 'ScalaCorp, Inc.'
let g:scommenter_file_copyright_list = [
\    'Copyright 2010 ' . b:scommenter_company_name . 'All rights reserved',
\    'PPOPRIETARY/CONFIDENTIAL, Use is subject to licence terms.'
\]

" Set to true if you don't like the automatically added "created"-time
let b:scommenter_file_noautotime = s:DISABLED

" Define whether scommenter tries to parse and update the existing Doc-comments
"   on the item it was executed on. If this feature is disabled, a completely 
"   new comment-template is written
let b:scommenter_update_comments = s:ENABLED


" Whether to prepend an empty line before the generated comment, if the
"   line just above the comment would otherwise be non-empty.
let b:scommenter_add_empty_line = 1

" Define wheter scommenter should remove old tags (eg. if the return value was
"   changed from int to void). Will not work for exceptions, since it should 
"   not remove RuntimeExceptions, and recognizing whether an exception is RTE 
"   is very hard.
" This feature is not throughly tested, and might delete something it was not
"   supposed to, so use with care. Only applicable if 
"   b:scommenter_update_comments is enabled.
let b:scommenter_remove_tags_on_update = 1

" Uncomment and modify if you're not happy with the default file
"   comment-template:
"function! SCommenter_OwnFileComments()
"  call append(0, '/*')
"  call append(1, ' * File name   : ' . bufname("%"))
"  call append(2, ' * authors     : ')
"  call append(3, ' * created     : ' . strftime("%c"))
"  call append(4, ' *')
"  call append(5, ' */')
"endfunction

" Set to 1 to use the StdFileComments function to write file comments
let b:scommenter_std_file_comments = s:DISABLED

" Set to 1 to use the ScalaAPIFileComments function to write file comments
let b:scommenter_scala_api_file_comments = s:DISABLED 

" Set to 1 to use the SunFileComments function to write file comments
let b:scommenter_sun_file_comments = s:DISABLED

" ============================================================================
" End of Configuration Options
" ============================================================================


" ============================================================================
" File:          scalacommenter.vim
" Summary:       Functions for documenting Scala-code
" Author:        Richard Emberson <richard.n.embersonATgmailDOTcom>
" Last Modified: 04/04/2010
" Version:       1.0
" Modifications:
"  1.0 : initial public release.
"
" Tested on vim 7.2 on Linux

" ============================================================================
" Description: 
"
" Functions for automatically generating ScalaDoc compatible comments.
" The ScalaCommentWriter() can produce a number of kinds of comments 
" depending on the current line/range.
" 
" Supported tags in the order they should appear in a comment are:
"
"   * @author      (top-level classes, traits and objects only, required)
"                  listed in chronological order
"   * @version     (top-level classes, traits and objects only, required)
"   * @param       (methods, classes and inner classes only)
"                  listed in argument-declaration order
"   * @tparam      (methods, all classes and all traits only)
"                  listed in template argument-declaration order
"   * @return      (methods only)
"   * @throws      (methods and all classes)
"                  listed in aplhabetical order
"   * @see         
"   * @since       
"   * @serial      
"   * @deprecated  
" 
" For more information (which may or may not apply to Scala )see: 
"       http://java.sun.com/j2se/javadoc/writingdoccomments/
" 
" Comment templates can be generated for the following: 
" 
" 1. File comments: user specifies the template, generated when the
"   cursor is on the first line of the file.
"
"   There are a couple of built-in File Comment templates and each
"   user is encouraged to tailor for their or company needs.
"   Remember to enable only one such file comment template configuration
"   parameter and disable the rest. If all are disabled, then the
"   default File Comment template is used (see below).
"   
"   The StdFileComments() comment is enabled the configuration parameter:
"      let b:scommenter_std_file_comments = s:ENABLED
"   
"   /*
"    * file name   : bufname("%")
"    * authors     : b:scommenter_file_author
"    * created     : strftime("%c")
"    * copyright   : b:scommenter_file_copyright_line
"    *
"    * $Id: scalacommenter.vim 307 2010-04-09 01:07:43Z  $
"    *
"    * modifications:
"    *
"    */
"
"   Here some of the lines are controlled by other configuration parameters.
"   bufname("%") produces the name of the current file
"   strftime("%c") produces the current date/time
"
"
"   The ScalaAPIFileComments() comment is enabled the configuration parameter:
"      let b:scommenter_scala_api_file_comments = s:ENABLED
"   This is the File Comment used in the Scala library.   
"   
"   /*
"   /*                     __                                               *\
"   **     ________ ___   / /  ___     Scala API                            **
"   **    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
"   **  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
"   ** /____/\___/_/ |_/____/_/ | |                                         **
"   **                          |/                                          **
"   *                                                                      */
"
"   $Id: scalacommenter.vim 307 2010-04-09 01:07:43Z  $
"
"   For this template everything is hardcoded. If one wants to change, for
"   instance, the copyright dates, this VimScript code must be modified.
"
"
"   The SunFileComments() comment is enabled the configuration parameter:
"      let b:scommenter_sun_file_comments = s:ENABLED
"   This mirrors the File Comments found in Sun's Java libraries.
"   
"   /*
"    *  bufname("%")
"    *
"    * Copyright 2010 Sun, Inc. All rights reserved
"    * PPOPRIETARY/CONFIDENTIAL, Use is subject to licence terms.
"    *
"    *  $Id: scalacommenter.vim 307 2010-04-09 01:07:43Z  $
"    *
"    */
"
"   This File Comment is provided because is shows how to create a
"   multi-line copyright using a VimScript List.
"
"
"   The DefaultFileComments() comment is used if no other File Comment
"   template has been selected via configuration.  
"
"   /*
"    * bufname("%")
"    * created: strftime("%d/%m/%y")
"    * 
"    * COPYRIGHT
"    * 
"    * $Id: scalacommenter.vim 307 2010-04-09 01:07:43Z  $
"    *
"    */
"   
"   The COPYRIGHT list of strings in the comment can be replaced with your own
"   or your company's either in this VimScript file or using some script
"   of your own.
"   
"   The File Comment is NOT the place to document whats in the file. That
"   is done in the class/trait/object comments. Rather, the File Comment
"   is for file status and copyright information.
"   
" 
" 2. Class comments: generated when on top of a class declaration
"   Tags if applicable: @author, @version, @since, @param and @tparam
" 
"   As an example, the class defined below will yield the following
"   Class Comment when ScalaCommentWriter is executed.
" 
"   /** 
"    * 
"    * 
"    * @author Ada Lovelace
"    * @version 1.0, 06/04/10
"    * @since 1.0
"    * 
"    * @param  int (Int) 
"    * @param  name (String) 
"    * @tparam A 
"    * @tparam B 
"    * @throws IndexOutOfBoundsException
"    */
"   @throws(classOf[IndexOutOfBoundsException])
"   class SomeClass[A,B](int: Int, name: String) {}
"    
"   The value of the @author comment is set by the configuration
"   parameter b:scommenter_class_author.
"   The value of the @version comment is set by the configuration
"   parameter b:scommenter_class_version.
"   The value of the @since comment is set by the configuration
"   parameter b:scommenter_since_release.
"
"   Note that the @param tags have not only the parameter name but also
"   its type. This is done because, generally, a parameter comment
"   includes its type so its included to aid the commenter.
"   This feature may be remove or made configurable depending on feedback.
" 
"   The Exceptions will only be found if they are in an "@throws()"
"   annotation.
" 
" 3. Trait comments: generated when on top of a trait declaration
"   Tags if applicable: @author, @version and @since
"
"   As an example, the trait defined below will yield the following
"   Class Comment when ScalaCommentWriter is executed.
" 
"   /** 
"    * 
"    * 
"    * @author Ada Lovelace
"    * @version 1.0, 06/04/10
"    * @since 1.0
"    */
"   trait SomeTrait {}
" 
"   As with the Class Comment, the values of the @author, @version and
"   @since are controlled by the configurable parameters
"   b:scommenter_class_author. b:scommenter_class_version and
"   b:scommenter_since_release respectfully.
"
" 
" 4. Object comments: generated when on top of a object declaration
"   Tags if applicable: @author, @version and @since
"
"   As an example, the object defined below will yield the following
"   Class Comment when ScalaCommentWriter is executed.
" 
"   /** 
"    * 
"    * 
"    * @author Ada Lovelace
"    * @version 1.0, 06/04/10
"    * @since 1.0
"    */
"   object SomeObject {}
" 
"   As with the Class Comment, the values of the @author, @version and
"   @since are controlled by the configurable parameters
"   b:scommenter_class_author. b:scommenter_class_version and
"   b:scommenter_since_release respectfully.
"
" 
" 5. Inner Class comments: generated when on top of an inner class declaration
"   Tags if applicable: @param and @tparam
"   The Inner Class Comment is similar to the top-level Class Comment except
"   it does not include the @author, @version and @since tags.
" 
"   As an example, the inner class defined below will yield the following
"   Inner Class Comment when ScalaCommentWriter is executed.
" 
"   /** 
"    * 
"    * 
"    * @param  int (Int) 
"    * @param  name (String) 
"    * @tparam A 
"    * @tparam B 
"    * @throws ClassCastException
"    * @throws IllegalAccessException
"    */
"   @throws(classOf[ClassCastException])
"   @throws(classOf[IllegalAccessException])
"   final class InnerSomeClass[A,B](int: Int, name: String) {}
" 
" 
" 6. Inner Trait comments: generated when on top of an inner trait declaration
"
"   An example follows:
"
"   /** 
"    * 
"    */
"   trail InnerSomeTrait {}
" 
" 7. Inner Object comments: generated when on top of an inner object declaration
"
"   An example follows:
"
"   /** 
"    * 
"    */
"   object InnerSomeObject {}
" 
" 
" 8. Method comments: generated when on top of a metod declaration.
"
"   Method Comments include the tags: @param, @tparam and @return 
"   (when return is not Unit).
"
"   /** 
"    * map
"    * 
"    * @param  f (A => B) 
"    * @param  i (Int) 
"    * @param  s (String) 
"    * @tparam B 
"    * @tparam C 
"    * @return (Option[B])
"    * @throws NullPointerException
"    */
"   @throws(classOf[NullPointerException])
"   def map[B,[C]](f: A => B,i: Int,  s: String ): Option[B] =
"
"   Note that the parameter and return types are included.
"   Also, the method name is also included in the comment template.
"   Again, feedback will determine if these stay in future release
"   of the script.
"
" 9. Fields, val and var, comments: generated when on top of a var or val
"   declaration.
"
"   Two different Comments can be generated depending upon the value
"   of b:scommenter_field_description_space. If it is -1, then
"   a single line comment is created:
"
"   /** */
"   val foo =
"
"   On the other hand if it is positive, then a multi-line comment is 
"   created:
"
"   /** 
"    *
"    */ 
"   var bar =
"
" Comment formatting re-orders the order or the '@' tags as well as
" generates spacing so that tag values align and tag text align.
" This include handling multi-line tag text.
" If a comment contains the following comment tag lines:
"
"   * @author    Tom  Jones
"   * @version   1.3 03/10/50
"   * @param  name This is the name
"   * @param  value the value to associate with the time
"   *  This is a second line. Here is a second second line.
"   *    This is a third line. Here is a third third line.
"   * @throws java.io.EOFException For no reason
"   * @author Jane Doe
"   * @tparam A the value to associate with the time
"   * @throws java.io.FooBarException For no reason
"   *  This is a second line. Here is a second second line.
"   *    This is a third line. Here is a second third line.
"   * @see some reference
"   * @since 1.23
"
" First select the lines (I use visual selection) and then invoke 
" the ScalaCommentFormatter() function. This results in:
"
"  * @author  Tom Jones
"  * @author  Jane Doe
"  * @version 1.3 03/10/50
"  * @param   name                    This is the name
"  * @param   value                   the value to associate with the time
"  *                                  This is a second line. Here is a second
"  *                                  second line. This is a third line. Here
"  *                                  is a third third line.
"  * @tparam  A                       the value to associate with the time
"  * @throws  java.io.EOFException    For no reason
"  * @throws  java.io.FooBarException For no reason This is a second line.
"  *                                  Here is a second second line. This is a
"  *                                  third line. Here is a second third line.
"  * @see     some reference
"  * @since   1.23
"
" Note that all of the tag values are aligned and all of the tag text 
" is also aligned. In addition, tag of the same type have
" been grouped together and re-order to abide by Sun's JavaDoc guidelines
" (Scala does not yet have its own commenting guidelines).
"
" Installation:
" 
" 0. Optionally, copy the configuration section above into a second
"    file. If this second file is loaded into vim after this script file,
"    then any configuration changes you've made in the second file are
"    the one this script file uses (or you can not make a copy and
"    just edit this file).
"
" 1. Edit the configuration section. It is commented, so I won't explain the
"    options here.
"
" 2. Put something like
"
"      autocmd FileType scala source $VIM/macros/scalacommenter.vim
"      autocmd FileType scala source $VIM/macros/scalacommenter_config.vim
"
"    to your vimrc. Note that loading the second, configuration file
"    after the actual script guarantees that your options are used 
"    instead of the script defaults in this file.
"
" Important: 
"   If you are editing this file in one Vim session and testing in another
"   session, using:
"
"     :source $HOME/.vim/scalacommenter.vim
"     
"   (or wherever you have place this file) to re-source this file, 
"   then your changes will not take effect unless you comment out the lines:
"
"     if exists("b:did_scalacom")
"       finish
"     endif
"
"   which appear later in this file. This if-statement will only allow
"   this file to be sourced once.
"   As an alternative, you can always after making changes to this file,
"   exit Vim in your test session and re-start it - when Vim is re-started
"   it will completely read this file.
"
"   Also, if you redefine some of the configuration parameters in your
"   .vimrc file, then re-sourcing this file will over-ride those definitions.
"
" Usage:
"
" If you didn't change the mapping specified in the configuration file, 
" you can can trigger the comment-generation by pressing Alt-c (or "Meta-c"). 
" Note that these do not generally work a Unix system. For Unix you
" have to create your own mapping. I use something like the following mapping
" (without the copyright blahs):
"
"   autocmd FileType scala source $VIM/macros/scalacommenter.vim
"   autocmd FileType scala map cm :call ScalaCommentWriter()<CR>
"   autocmd FileType scala map cf :call ScalaCommentFormatter()<CR>
"   autocmd FileType scala let b:scommenter_class_author = 'Richard Emberson'
"   autocmd FileType scala let b:scommenter_file_author = 'Richard Emberson'
"   autocmd FileType scala let g:scommenter_file_copyright_list = [
"   \    'COPYRIGHT and more text'
"   \    'blah, blah'
"   \    'and blah'
"   \]
"
" As described above, the cursor must be on the first line or on the same line
" as the method/class/trait/object/field declaration in order to 
" achieve something useful. If the declaration extends to several lines, 
" the range must be specified.  Range should include everything from the 
" declaration that comes before the '{' or ';'. Everything after either 
" of those characters is ignored, so line-wise selection is a handy 
" way to do this. For multi-line selection uses I use the VIM visual selection
" mode.
"
" Comments:
"
"   Send any comments or bugreports to:
"       Richard Emberson <richard.n.embersonATgmailDOTcom>
"
"   I will repeat what Kalle Bjorklid said, "Happy coding!  ;-)"
" ============================================================================

" ============================================================================
" THE SCRIPT
" ============================================================================

" Load only once:
" if exists("b:did_scalacom")
"  finish
" endif
let b:did_scalacom = 1

" Varible that tells what is put before the written string when using
" the AppendStr-function.
let s:indent = ''

" The string that includes the text of the line on which the commenter
" was called, or the whole range. This is what is parsed.
let s:combinedString = ''

let s:rangeStart = 1 " line on which the range started
let s:rangeEnd = 1   " line on which the range ended

let s:defaultMethodDescriptionSpace = 1
let s:defaultInnerClassDescriptionSpace = 1
let s:defaultClassDescriptionSpace = 1
let s:defaultInnerObjectDescriptionSpace = 1
let s:defaultObjectDescriptionSpace = 1
let s:defaultInnerTraitDescriptionSpace = 1
let s:defaultTraitDescriptionSpace = 1
let s:defaultFieldDescriptionSpace = 1

let s:docCommentStart = -1
let s:docCommentEnd   = -1

" These can be used to add to the tag (@param, @tparam and @return) some
" default text.
let s:defaultParamText  = (exists('b:scommenter_default_param'))  ? b:scommenter_default_param : ''
let s:defaultTParamText  = (exists('b:scommenter_default_tparam'))  ? b:scommenter_default_tparam : ''
let s:defaultReturnText = (exists('b:scommenter_default_return')) ? b:scommenter_default_return : ''
let s:defaultExceptionText  = (exists('b:scommenter_default_exception'))  ? b:scommenter_default_exception : ''

let g:getterName = 'get'

let g:booleanNames = [
\  'is',
\  'has'
\]

" ============================================================================
" Patterns
" ============================================================================

let s:scalaname = '[a-zA-Z_][a-zA-Z0-9_]*'

let s:scalaMethodPattern     = '\(^\|\s\+\)def\s.*'

let s:commentTagPattern     = '^\s*\*\=\s*@[a-zA-Z]\+\(\s\|$\)'

let s:scalaInnerClassPattern  = '^\s\+\(\(protected\|private\|abstract\|final\)\s\+\)*\s*class\s\+' . s:scalaname

let s:scalaInnerObjectPattern = '^\s\+\(\(protected\|private\|abstract\|final\)\s\+\)*\s*object\s\+' . s:scalaname . '\(\[[^\]]\]\)\?\({\|\s\|$\)'

let s:scalaInnerTraitPattern  = '^\s\+\(\(protected\|private\|abstract\|final\)\s\+\)*\s*trait\s\+' . s:scalaname

let s:scalaClassPattern  = '\(^\|\s\+\)class\s\+'. s:scalaname

let s:scalaObjectPattern = '\(^\|\s\)object\s\+' . s:scalaname 

let s:scalaTraitPattern  = '\(^\|\s\)trait\s\+' . s:scalaname 

let s:scalaVariablePattern   = '\(^\|\s\+\)var '
let s:scalaValuePattern   = '\(^\|\s\+\)val '

" ============================================================================
" Public functions
" ============================================================================

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" ScalaCommentWriter
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function! ScalaCommentWriter() range
  let s:oldICValue = &ignorecase
  let &ignorecase = 0

  let s:rangeStart = a:firstline
  let s:rangeEnd = a:lastline
  let s:combinedString = s:GetCombinedString(s:rangeStart, s:rangeEnd)

  if s:IsFileComments()
    call s:WriteFileComments()

  elseif s:IsMethod()
    call s:HandleMethod()
    if s:method_comment_update_only == 0
      call s:AddEmpty()
    endif

  elseif s:IsInnerClass()
    call s:HandleInnerClass()
    if s:method_comment_update_only == 0
      call s:AddEmpty()
    endif

  elseif s:IsInnerObject()
    call s:HandleInnerObject()
    call s:AddEmpty()

  elseif s:IsInnerTrait()
    call s:HandleInnerTrait()
    if s:method_comment_update_only == 0
      call s:AddEmpty()
    endif

  elseif s:IsClass()
    call s:HandleClass()
    if s:method_comment_update_only == 0
      call s:AddEmpty()
    endif

  elseif s:IsObject()
    call s:HandleObject()
    call s:AddEmpty()

  elseif s:IsTrait()
    call s:HandleTrait()
    if s:method_comment_update_only == 0
      call s:AddEmpty()
    endif

  elseif s:IsVariable()
    call s:HandleVariable()

  elseif s:IsValue()
    call s:HandleValue()
  else
    call s:Message('Nothing to do')
  endif

  let &ignorecase = s:oldICValue
endfunction


" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" ScalaCommentFormatter
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function! ScalaCommentFormatter() range
  let s:oldICValue = &ignorecase
  let &ignorecase = 0

  let s:rangeStart = a:firstline
  let s:rangeEnd = a:lastline

  call Reset()

  " check range
  let inComment = s:InCommentTag(s:rangeStart)
  if !inComment
    call s:Message('Start not in Comment')
    return
  endif
  let inComment = s:InComment(s:rangeEnd)
  if !inComment
    call s:Message('End not in Comment')
    return
  endif

  " search range
  let firstTagLine = -1
  let lastTagLine = -1
  let pos = s:rangeStart

  let tag = ''
  let value = ''
  let text = ''
  while pos <= s:rangeEnd
    let line = getline(pos)

    let l:m = matchlist(line, '^\s*\*\s*@\(\S\+\)\s\+\(\S\+\)\(.*\)')
    if l:m != []
      let tag = l:m[1]
      let value = l:m[2]
      let text = s:Trim(l:m[3])
" echo 'tag=' . tag
" echo 'value=' . value
" echo 'text=' . text

      let s:indent = s:GetIndentation(line)

      if firstTagLine == -1
        let firstTagLine = pos
      endif
      let lastTagLine = pos

      call s:LoadTag(tag, value, text)
    else
      let text = substitute(line, '^\s*\*\s*\(.*\)\s*$', '\1', '')
" echo 'text=' . text
      if text != ''
        let lastTagLine = pos
        call s:ExtendTag(tag, text)
      endif
    endif
    let pos = pos + 1
  endwhile

  call s:DetermineTagValueOffset() 
  call s:DetermineTagTextOffset() 

  " For testing
  " call s:DumpTags() 
  
  " For Production
  call s:ReplaceTags(firstTagLine, lastTagLine) 

  let &ignorecase = s:oldICValue
endfunction

" @author value
let g:authorValueList = []
" @version value  (1.2 mm/dd/yy)
let s:versionValue = ''
" @param value text
let g:paramValueTextList = []
" @tparam value text
let g:tparamValueTextList = []
" @return       text
let s:returnText = ''
" @throws value text
let g:throwsValueTextList = []
" @see value
let g:seeValueList = []
" @since value
let s:sinceValue = ''
" @serial value
let s:serialValue = ''
" @deprecated       text
let s:deprecatedText = ''

let s:pageWidth = 80
" offset relative to comment '*'
let s:tagValueOffset = 0
" offset relative to comment '*'
let s:tagTextOffset = 0

function! Reset() 
  let s:indent = ''

  let g:authorValueList = []
  let s:versionValue = ''
  let g:paramValueTextList = []
  let g:tparamValueTextList = []
  let s:returnText = ''
  let g:throwsValueTextList = []
  let g:seeValueList = []
  let s:sinceValue = ''
  let s:serialValue = ''
  let s:deprecatedText = ''

  let s:tagValueOffset = 0
  let s:tagTextOffset = 0
endfunction

function! s:DetermineTagValueOffset() 
  " from longest shortest
  if s:deprecatedText != ''
    let s:tagValueOffset = 10
  elseif s:versionValue != ''
    let s:tagValueOffset = 7
  elseif g:authorValueList != []
    let s:tagValueOffset = 6
  elseif g:tparamValueTextList != []
    let s:tagValueOffset = 6
  elseif g:throwsValueTextList != []
    let s:tagValueOffset = 6
  elseif s:returnText != ''
    let s:tagValueOffset = 6
  elseif s:serialValue != ''
    let s:tagValueOffset = 6
  elseif g:paramValueTextList != []
    let s:tagValueOffset = 5
  elseif s:sinceValue != ''
    let s:tagValueOffset = 5
  elseif g:seeValueList != []
    let s:tagValueOffset = 3
  else
    let s:tagValueOffset = 0
  endif
endfunction

function! s:DetermineTagTextOffset() 
  let l:max = 0
  for authorValue in g:authorValueList
    let t = strlen(authorValue)
    if l:max < t
      let l:max = t
    endif 
  endfor

  if s:versionValue != ''
    let t = strlen(s:versionValue)
    if l:max < t
      let l:max = t
    endif 
  endif

  for param in g:paramValueTextList
    let l:m = matchlist(param, '\([^:]\+\):\(.*\)')
    let t = strlen(l:m[1])
    if l:max < t
      let l:max = t
    endif 
  endfor

  for tparam in g:tparamValueTextList
    let l:m = matchlist(tparam, '\([^:]\+\):\(.*\)')
    let t = strlen(l:m[1])
    if l:max < t
      let l:max = t
    endif 
  endfor

  for ex in g:throwsValueTextList
    let l:m = matchlist(ex, '\([^:]\+\):\(.*\)')
    let t = strlen(l:m[1])
    if l:max < t
      let l:max = t
    endif 
  endfor

  for seeValue in g:seeValueList
    let t = strlen(seeValue)
    if l:max < t
      let l:max = t
    endif 
  endfor

  if s:sinceValue != ''
    let t = strlen(s:sinceValue)
    if l:max < t
      let l:max = t
    endif 
  endif

  if s:serialValue != ''
    if s:serialValue == 'include'
      let t = 7
      if l:max < t
        let l:max = t
      endif 
    elseif s:serialValue == 'exclude'
      let t = 7
      if l:max < t
        let l:max = t
      endif 
    " else its text, not a value
    endif
  endif

  let s:tagTextOffset = l:max + s:tagValueOffset + 1
endfunction

"---------------------------------------------------
" Start Testing Formatting code
"---------------------------------------------------
function! s:DumpTags() 

  for author in g:authorValueList
    let diff = s:tagValueOffset - 6
    echo '@author ' . s:MakeEmptyString(diff) . author
  endfor

  if s:versionValue != ''
    let diff = s:tagValueOffset - 7
    echo '@version ' .  s:MakeEmptyString(diff) . s:versionValue
  endif

  for param in g:paramValueTextList
    let l:m = matchlist(param, '\([^:]\+\):\(.*\)')
    let tagValue = l:m[1]
    let tagText = l:m[2]
    let diff = s:tagValueOffset - 5
    let sp = s:MakeEmptyString(diff)
    let t = '@param ' . sp . tagValue

    call s:DumpTag(t, tagText)
  endfor

  for tparam in g:tparamValueTextList
    let l:m = matchlist(tparam, '\([^:]\+\):\(.*\)')
    let tagValue = l:m[1]
    let tagText = l:m[2]
    let diff = s:tagValueOffset - 6
    let sp = s:MakeEmptyString(diff)
    let t = '@tparam ' . sp . tagValue

    call s:DumpTag(t, tagText)
  endfor

  if s:returnText != ''
    let diff = s:tagTextOffset - 6
    let sp = s:MakeEmptyString(diff)
    let t = '@return ' . sp

    call s:DumpTag(t, s:returnText)
  endif

  for ex in g:throwsValueTextList
    let l:m = matchlist(ex, '\([^:]\+\):\(.*\)')
    let tagValue = l:m[1]
    let tagText = l:m[2]

    let diff = s:tagValueOffset - 6
    let sp = s:MakeEmptyString(diff)
    let t = '@throws ' . sp . tagValue

    call s:DumpTag(t, tagText)
  endfor

  for seeValue in g:seeValueList
    let diff = s:tagValueOffset - 3
    echo '@see ' . s:MakeEmptyString(diff) . seeValue
  endfor

  if s:sinceValue != ''
    let diff = s:tagValueOffset - 5
    echo '@since ' .  s:MakeEmptyString(diff) . s:sinceValue
  endif

  if s:serialValue != ''
    let diff = s:tagValueOffset - 6
    echo '@serial ' .  s:MakeEmptyString(diff) . s:serialValue
  endif

  if s:deprecatedText != ''
    let diff = s:tagTextOffset - 10
    let t = '@deprecated ' .  s:MakeEmptyString(diff) 

    call s:DumpTag(t, s:deprecatedText)
  endif
endfunction

function! s:DumpTag(firstLine, tagText) 
  let firstLine = a:firstLine
  let tagText = a:tagText

  let indent_len = strlen(s:indent)
  if indent_len + s:tagTextOffset + strlen(tagText) < s:pageWidth
    let diff = s:tagTextOffset - strlen(firstLine)
    let sp = s:MakeEmptyString(diff + 2)
    echo firstLine . sp . tagText
  else 

    let offset = s:pageWidth - indent_len - s:tagTextOffset - 6
    let index = offset
    let c = strpart(tagText, index, 1)
    while c != ' ' && index > 0
      let index = index - 1
      let c = strpart(tagText, index, 1)
    endwhile
    let part = strpart(tagText, 0, index)
    let tagText = s:Trim(strpart(tagText, index))

    let diff = s:tagTextOffset - strlen(firstLine)
    let sp = s:MakeEmptyString(diff + 2)
    echo firstLine . sp . part

    while strlen(tagText) > offset
      let index = offset
      let c = strpart(tagText, index, 1)
      while c != ' ' && index > 0
        let index = index - 1
        let c = strpart(tagText, index, 1)
      endwhile
      let part = strpart(tagText, 0, index)
      let tagText = s:Trim(strpart(tagText, index))

      let diff = s:tagTextOffset
      let sp = s:MakeEmptyString(diff + 2)
      echo sp . part
    endwhile

    if strlen(tagText) > 0
      let diff = s:tagTextOffset
      let sp = s:MakeEmptyString(diff + 2)
      echo sp . tagText
    endif
  endif
endfunction

"---------------------------------------------------
" End Testing Formatting code
"---------------------------------------------------

function! s:ReplaceTags(firstLine, lastLine) 
  let firstLine = a:firstLine
  let lastLine = a:lastLine

  " remove lines
  call s:DeleteLines(firstLine, lastLine)

  let s:appendPos = firstLine - 1

  for author in g:authorValueList
    let diff = s:tagValueOffset - 6
    call s:AppendCommentLine('@author ' . s:MakeEmptyString(diff) . author)
  endfor

  if s:versionValue != ''
    let diff = s:tagValueOffset - 7
    call s:AppendCommentLine('@version ' .  s:MakeEmptyString(diff) . s:versionValue)
  endif

  for param in g:paramValueTextList
    let l:m = matchlist(param, '\([^:]\+\):\(.*\)')
    let tagValue = l:m[1]
    let tagText = l:m[2]

    let diff = s:tagValueOffset - 5
    let sp = s:MakeEmptyString(diff)
    let t = '@param ' . sp . tagValue

    call s:AddValueAndText(t, tagText) 
  endfor

  for tparam in g:tparamValueTextList
    let l:m = matchlist(tparam, '\([^:]\+\):\(.*\)')
    let tagValue = l:m[1]
    let tagText = l:m[2]

    let diff = s:tagValueOffset - 6
    let sp = s:MakeEmptyString(diff)
    let t = '@tparam ' . sp . tagValue

    call s:AddValueAndText(t, tagText) 
  endfor

  if s:returnText != ''
    let diff = s:tagTextOffset - 6
    let sp = s:MakeEmptyString(diff)

    call s:AddValueAndText('@return ' . sp, s:returnText)
  endif

  for ex in g:throwsValueTextList
    let l:m = matchlist(ex, '\([^:]\+\):\(.*\)')
    let tagValue = l:m[1]
    let tagText = l:m[2]

    let diff = s:tagValueOffset - 6
    let sp = s:MakeEmptyString(diff)
    let t = '@throws ' . sp . tagValue

    call s:AddValueAndText(t, tagText) 
  endfor

  for seeValue in g:seeValueList
    let diff = s:tagValueOffset - 3
    call s:AppendCommentLine('@see ' . s:MakeEmptyString(diff) . seeValue)
  endfor

  if s:sinceValue != ''
    let diff = s:tagValueOffset - 5
    call s:AppendCommentLine('@since ' .  s:MakeEmptyString(diff) . s:sinceValue)
  endif

  if s:serialValue != ''
    let diff = s:tagValueOffset - 6
    call s:AppendCommentLine('@serial ' .  s:MakeEmptyString(diff) . s:serialValue)
  endif

  if s:deprecatedText != ''
    let diff = s:tagTextOffset - 10

    call s:AddValueAndText('@deprecated ' . s:MakeEmptyString(diff), s:deprecatedText)

  endif
endfunction

function! s:AddValueAndText(firstLine, tagText) 
  let firstLine = a:firstLine
  let tagText = a:tagText

  let indent_len = strlen(s:indent)
  if indent_len + s:tagTextOffset + strlen(tagText) < s:pageWidth
    let diff = s:tagTextOffset - strlen(firstLine)
    let sp = s:MakeEmptyString(diff + 2)
    call s:AppendCommentLine(firstLine . sp . tagText)
  else 

    let offset = s:pageWidth - indent_len - s:tagTextOffset - 6
    let index = offset
    let c = strpart(tagText, index, 1)
    while c != ' ' && index > 0
      let index = index - 1
      let c = strpart(tagText, index, 1)
    endwhile
    let part = strpart(tagText, 0, index)
    let tagText = s:Trim(strpart(tagText, index))

    let diff = s:tagTextOffset - strlen(firstLine)
    let sp = s:MakeEmptyString(diff + 2)
    call s:AppendCommentLine(firstLine . sp . part)

    while strlen(tagText) > offset
      let index = offset
      let c = strpart(tagText, index, 1)
      while c != ' ' && index > 0
        let index = index - 1
        let c = strpart(tagText, index, 1)
      endwhile
      let part = strpart(tagText, 0, index)
      let tagText = s:Trim(strpart(tagText, index))

      let diff = s:tagTextOffset
      let sp = s:MakeEmptyString(diff + 2)
      call s:AppendCommentLine(sp . part)
    endwhile

    if strlen(tagText) > 0
      let diff = s:tagTextOffset
      let sp = s:MakeEmptyString(diff + 2)
      call s:AppendCommentLine(sp . tagText)
    endif
  endif
endfunction

function! s:LoadTag(tagName, tagValue, tagText) 
  let tagName = a:tagName
  let tagValue = a:tagValue
  let tagText = a:tagText

  if tagName == 'author'
    call add(g:authorValueList, tagValue . ' ' . tagText)
  elseif tagName == 'version'
    let s:versionValue = tagValue . ' ' . tagText
  elseif tagName == 'param'
    call add(g:paramValueTextList, tagValue . ':' . tagText)
  elseif tagName == 'tparam'
    call add(g:tparamValueTextList, tagValue . ':' . tagText)
  elseif tagName == 'return'
    let s:returnText = tagValue . ' ' . tagText
  elseif tagName == 'throws'
    call add(g:throwsValueTextList, tagValue . ':' . tagText)
  elseif tagName == 'see'
    call add(g:seeValueList, tagValue . ' ' . tagText)
  elseif tagName == 'since'
    let s:sinceValue = tagValue . ' ' . tagText
  elseif tagName == 'serial'
    let s:serialValue = s:Trim(tagValue . ' ' . tagText)
  elseif tagName == 'deprecated'
    let s:deprecatedText = tagValue . ' ' . tagText
  else
  endif
endfunction

function! s:ExtendTag(tagName, tagText) 
  let tagName = a:tagName
  let tagText = a:tagText

  if tagName == 'author'
    call s:ExtendListTag(g:authorValueList, tagText) 
  elseif tagName == 'version'
    let s:versionValue = s:versionValue . ' ' . tagText
  elseif tagName == 'param'
    call s:ExtendListTag(g:paramValueTextList, tagText) 
  elseif tagName == 'tparam'
    call s:ExtendListTag(g:tparamValueTextList, tagText) 
  elseif tagName == 'return'
    let s:returnText = s:returnText . ' ' . tagText
  elseif tagName == 'throws'
    call s:ExtendListTag(g:throwsValueTextList, tagText) 
  elseif tagName == 'see'
    call s:ExtendListTag(g:seeValueList, tagText) 
  elseif tagName == 'since'
    let s:sinceValue = s:sinceValue . ' ' . tagText
  elseif tagName == 'serial'
    let s:serialValue = s:serialValue . ' ' . tagText
  elseif tagName == 'deprecated'
    let s:deprecatedText = s:deprecatedText . ' ' . tagText
  else
  endif
endfunction

function! s:ExtendListTag(list, tagText) 
  let list = a:list
  let tagText = a:tagText
  let pos = len(list) - 1
  let list[pos] = s:Trim(list[pos]) . ' ' . tagText

endfunction

function! s:MakeEmptyString(len) 
  let len = a:len

  if len <= 0
    return ''
  elseif len == 1
    return ' '
  elseif len == 2
    return '  '
  elseif len == 3
    return '   '
  elseif len == 4
    return '    '
  elseif len == 5
    return '     '
  else
    return '      ' . s:MakeEmptyString(len - 6)
  endif
endfunction


function! s:InComment(pos) 
  let line = getline(a:pos)
  if line =~ '^\s*\*\s.*' 
    return 1
  else
    return 0
endfunction

function! s:InCommentTag(pos) 
  let line = getline(a:pos)
  if line =~ '^\s*\*\s*@.*' 
    return 1
  else
    return 0
endfunction




" ============================================================================
" File Comment
" ============================================================================

" Should file comments be written?
function! s:IsFileComments() 
  return s:rangeStart <= 1 && s:rangeStart == s:rangeEnd
endfunction

" write a file comment
function! s:WriteFileComments()
  let s:appendPos = 0
  if exists("*SCommenter_OwnFileComments")
    call SCommenter_OwnFileComments()
    return
  endif

  if exists("b:scommenter_std_file_comments") && b:scommenter_std_file_comments
    call s:StdFileComments()
    return
  endif

  if exists("b:scommenter_scala_api_file_comments") && b:scommenter_scala_api_file_comments
    call s:ScalaAPIFileComments()
    return
  endif

  if exists("b:scommenter_sun_file_comments") && b:scommenter_sun_file_comments
    call s:SunFileComments()
    return
  endif

  call s:DefaultFileComments()
endfunction

" standard file comment
function! s:StdFileComments()
  if exists("b:scommenter_file_noautotime") && b:scommenter_file_noautotime
    let created = ''
  else
    let created = strftime("%c")
  endif

  let s:appendPos = s:rangeStart - 1
  let s:indent    = ''
  call s:AppendStr('/*')
  call s:AppendStr(' * file name  : ' . bufname("%"))
  if exists("b:scommenter_file_author")
    call s:AppendStr(' * authors    : ' . b:scommenter_file_author)
  endif

  call s:AppendStr(' * created    : ' . created)

  if exists("b:scommenter_file_copyright_line")
    call s:AppendStr(' * copyright  : ' . b:scommenter_file_copyright_line)
  endif

  if exists("b:scommenter_class_svn_id")
    call s:AppendStr(' * ')
    call s:AppendStr(' * ' .  b:scommenter_class_svn_id)
  endif

  call s:AppendStr(' *')
  call s:AppendStr(' * modifications:')
  call s:AppendStr(' *')
  call s:AppendStr(' */')
endfunction  

" the scala libraray file comment
function! s:ScalaAPIFileComments()
  call s:AppendStr('/*                     __                                               *\')
  call s:AppendStr('**     ________ ___   / /  ___     Scala API                            **')
  call s:AppendStr('**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **')
  call s:AppendStr('**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **')
  call s:AppendStr('** /____/\___/_/ |_/____/_/ | |                                         **')
  call s:AppendStr('**                          |/                                          **')
  call s:AppendStr('\*                                                                      */')

  call s:AppendStr('')
  if exists("b:scommenter_class_svn_id")
    call s:AppendStr('// ' .  b:scommenter_class_svn_id)
  endif
endfunction  

" the sun libraray file comment
function! s:SunFileComments()

  if exists("b:scommenter_file_noautotime") && b:scommenter_file_noautotime
    let created = ''
  else
    let created = strftime("%y/%m/%d")
  endif

  let s:appendPos = s:rangeStart - 1
  let s:indent    = ''
  call s:AppendStr('/*')
  call s:AppendStr(' * ' . bufname("%") . '  ' . created)

  call s:AppendStars(1)

  let list_len = len(g:scommenter_file_copyright_list)
  if list_len > 0
    for line in g:scommenter_file_copyright_list
      call s:AppendStr(' * ' . line)
    endfor
  endif

  if exists("b:scommenter_class_svn_id")
    call s:AppendStr(' * ')
    call s:AppendStr(' * ' .  b:scommenter_class_svn_id)
  endif

  call s:AppendStr(' * ')
  call s:AppendStr(' */')
endfunction  

" the default file comment
function! s:DefaultFileComments()

  if exists("b:scommenter_file_noautotime") && b:scommenter_file_noautotime
    let created = ''
  else
    let created = strftime("%d/%m/%y")
  endif

  let s:appendPos = s:rangeStart - 1
  let s:indent    = ''
  call s:AppendStr('/*')
  call s:AppendStr(' * ' . bufname("%"))
  call s:AppendStr(' * created: ' . created)
  call s:AppendStr(' * ')

  let list_len = len(g:scommenter_file_copyright_list)
  if list_len > 0
    for line in g:scommenter_file_copyright_list
      call s:AppendStr(' * ' . line)
    endfor
  endif

  if exists("b:scommenter_class_svn_id")
    call s:AppendStr(' * ')
    call s:AppendStr(' * ' .  b:scommenter_class_svn_id)
  endif
  call s:AppendStr(' * ')

  call s:AppendStr(' */')
endfunction  


" ============================================================================
" Method Comment
" ============================================================================

let g:paramList = []
let g:tparamList = []
let g:exceptionList = []

" is it a method
function! s:IsMethod()
  let str = s:combinedString
  return str =~ s:scalaMethodPattern
endfunction


" write out method comment
function! s:HandleMethod()
  let s:method_returnValue = ''
  let g:paramList = []
  let g:tparamList = []

  let str = s:combinedString
  let s:method_indent = substitute(str, '^\(\s*\)\S.*', '\1', '')

  let len = strlen(s:method_indent)
  if len !=  0
    let str = strpart(str, len)
  endif

  let s:method_pre_def = substitute(str, '^\(\S.*\s*\)def\s*.*', '\1', '')

  let len = strlen(s:method_pre_def)
  if len == strlen(str)
    let s:method_pre_def = ''
  else
    let str = strpart(str, len)
  endif

  let str = strpart(str, 3)
  let str = substitute(str, '\s*\(.*\)', '\1', '')

  let s:method_name = substitute(str, '\s*\([^([: ]\+\).*', '\1', '')
  if s:method_name == 'this'
    let s:method_returnValue = 'instance'
  endif

  let len = strlen(s:method_name)
  let str = strpart(str, len)

  let str = substitute(str, '\s*\(.*\)', '\1', '')

  let len = strlen(str)
  if len != 0

    if strpart(str, 0, 1) == "["
      let endBracketType = s:scanBracketType(str, 1, 0)
      let str = strpart(str, endBracketType + 1)
      let str = substitute(str, '\s*\(.*\)', '\1', '')
    endif

    if strpart(str, 0, 1) == "("
      let endParanType = s:scanParanType(str, 1, 0)
      let str = strpart(str, endParanType + 1)
      let str = substitute(str, '\s*\(.*\)', '\1', '')
    endif

    if strpart(str, 0, 1) == ":"
      let str = s:Trim(strpart(str, 1))
      let equalsPos = stridx(str, '=')
      if equalsPos != -1
        let s:method_returnValue = s:Trim(strpart(str,0,equalsPos))
      else
        let s:method_returnValue = s:Trim(strpart(str,0))
      endif
      if s:method_returnValue == 'Unit'
        let s:method_returnValue = ''
      endif 
    elseif strpart(str, 0, 1) == "="
      " implied return value, i.e.,  def getFoo = foo
      let mpos = match(s:method_name, g:getterName)
      if mpos == 0
        let rest = strpart(s:method_name, 3)
        if rest == ''
            let s:method_returnValue = 'Unknown'
        else
            let s:method_returnValue = rest
        endif
      else
        " boolean return value, i.e.,  def isFoo = foo or def hasFoo = foo
        for bname in g:booleanNames
          let mpos = match(s:method_name, bname)
          if mpos == 0
            let s:method_returnValue = 'Boolean ' . strpart(s:method_name, len(bname))
            break
          endif
        endfor
      endif
    endif

  endif

  " loads exceptions
  call s:GetAnnotationCount()

  let existingDocCommentType = s:HasDocComments()
  let s:method_comment_update_only = 0

  if existingDocCommentType && exists("b:scommenter_update_comments") && b:scommenter_update_comments
    let s:method_comment_update_only = 1
    if existingDocCommentType == 1 
      call s:ExpandSinglelineCommentsEx(s:singleLineCommentPos, 1)
    endif
    let s:firstUpdatedTagLine = -1
    call s:UpdateAllTags(1)

    if exists("b:scommenter_move_cursor") && b:scommenter_move_cursor && s:firstUpdatedTagLine != -1
      exe "normal " . s:firstUpdatedTagLine . "G$"
      if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
        startinsert!
      endif
    endif
  else
    call s:WriteMethodComments()
  endif
endfunction

function! s:UpdateAllTags(isMethod)
  let s:indent = s:GetIndentation(s:combinedString)
  call s:UpdateParameters()
  call s:UpdateTParameters()
  if a:isMethod == 1
    call s:UpdateReturnValue()
  endif
  call s:UpdateExceptions()
endfunction

" --------------------------------
" Parameters
" --------------------------------
function! s:UpdateParameters()
  "Try to find out where the tags that might be added should be written.
  let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'param') - 1
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'tparam') - 1
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'return') - 1
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'since')
  endif

  if tagAppendPos < 0 
    let tagAppendPos = s:docCommentEnd - 1
  endif

  let params = g:paramList
  if len(params) > 0
    for param in params
      let paramName = s:Trim(substitute(param, '\([^:]*\).*', '\1', ''))
      let paramType = s:Trim(substitute(param, '[^:]*:\(.*\)', '\1', ''))

      let tagPos = s:FindTag(s:docCommentStart, s:docCommentEnd, 'param', paramName)
      if tagPos > -1
        let tagAppendPos = tagPos
        continue
      else
       let s:appendPos = tagAppendPos
       call s:AppendStr(' * @param  ' . paramName . ' (' . paramType . ') ' . s:defaultParamText)
       call s:MarkUpdateMade(tagAppendPos + 1)
       let s:docCommentEnd = s:docCommentEnd + 1
       let tagAppendPos = tagAppendPos + 1
      endif
    endfor
  endif

  if exists("b:scommenter_remove_tags_on_update") && b:scommenter_remove_tags_on_update
    call s:RemoveNonExistingParameters()
  endif
endfunction

function! s:RemoveNonExistingParameters()
  let paramlist = g:paramList
  let pos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'param')
  let Start = pos

  while pos > 0
    let line = getline(pos)
    let tagParam = substitute(line, '^\s*\(\*\s*\)\=@[a-zA-Z]*\s\+\(\S*\).*', '\2', '')

    let paramExists = 0

    let params = g:paramList
    if len(params) > 0
      for param in params
        let paramName = s:Trim(substitute(param, '\([^:]*\).*', '\1', ''))
  
        if tagParam == paramName
          let paramExists = 1
	  break
        endif
      endfor
    endif

    if paramExists == 0
      call s:RemoveTag(Start, s:docCommentEnd, 'param', tagParam)
    else
      let Start = Start + 1
    endif

    let g:paramList = paramlist
    let pos = s:FindFirstTag(Start, s:docCommentEnd, 'param')
  endwhile
endfunction

" --------------------------------
" T-Parameters
" --------------------------------
function! s:UpdateTParameters()
  "Try to find out where the tags that might be added should be written.
  let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'tparam') - 1
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'return') - 1
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'since')
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'throws')
  endif

  if tagAppendPos < 0 
    let tagAppendPos = s:docCommentEnd - 1
  endif

  let tparams = g:tparamList
  if len(tparams) > 0
    for tparam in tparams

      let tagPos = s:FindTag(s:docCommentStart, s:docCommentEnd, 'tparam', tparam)
      if tagPos > -1
        let tagAppendPos = tagPos
        continue
      else
       let s:appendPos = tagAppendPos
       call s:AppendStr(' * @tparam ' . tparam. ' ' . s:defaultTParamText)
       call s:MarkUpdateMade(tagAppendPos + 1)
       let s:docCommentEnd = s:docCommentEnd + 1
       let tagAppendPos = tagAppendPos + 1
      endif
    endfor
  endif

  if exists("b:scommenter_remove_tags_on_update") && b:scommenter_remove_tags_on_update
    call s:RemoveNonExistingTParameters()
  endif
endfunction

function! s:RemoveNonExistingTParameters()
  let tparamlist = g:tparamList
  let pos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'tparam')
  let Start = pos

  while pos > 0
    let line = getline(pos)
    let tagTParam = substitute(line, '^\s*\(\*\s*\)\=@[a-zA-Z]*\s\+\(\S*\).*', '\2', '')

    let tparamExists = 0

    let tparams = g:tparamList
    if len(tparams) > 0
      for tparam in tparams

        if tagTParam == tparam
          let tparamExists = 1
	  break
        endif
      endfor
    endif

    if tparamExists == 0
      call s:RemoveTag(Start, s:docCommentEnd, 'tparam', tagTParam)
    else
      let Start = Start + 1
    endif

    let g:tparamList = tparamlist
    let pos = s:FindFirstTag(Start, s:docCommentEnd, 'tparam')
  endwhile
endfunction


" --------------------------------
" Return Value
" --------------------------------
function! s:UpdateReturnValue()
  if s:method_returnValue == ''
    if exists("b:scommenter_remove_tags_on_update") && b:scommenter_remove_tags_on_update
      call s:RemoveTag(s:docCommentStart, s:docCommentEnd, 'return', '')
    endif
    return
  endif

  let newReturnValue = s:method_returnValue

  let returnTagPos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'return')
  if returnTagPos > -1 
    let line = getline(returnTagPos)

    let oldReturnValue = substitute(line,'.*(\([^)]*\))', '\1', '')

    if newReturnValue == oldReturnValue
      return
    endif

    call s:RemoveTag(s:docCommentStart, s:docCommentEnd, 'return', '')

  endif

  let tagAppendPos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'throws') - 1
  if tagAppendPos < 0
    let tagAppendPos = s:docCommentEnd - 1
  endif

  let s:appendPos = tagAppendPos
  call s:AppendStr(' * @return (' . newReturnValue . ')')
  call s:MarkUpdateMade(tagAppendPos + 1)
  let s:docCommentEnd = s:docCommentEnd + 1
endfunction

function! s:MarkUpdateMade(linenum)
  if s:firstUpdatedTagLine == -1 || a:linenum < s:firstUpdatedTagLine
    let s:firstUpdatedTagLine = a:linenum
  endif
endfunction


" --------------------------------
" Exception
" --------------------------------
function! s:UpdateExceptions()
  "Try to find out where the tags that might be added should be written.
  let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'throws') - 1
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'return')
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'tparam')
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'param')
  endif
  if tagAppendPos < 0
    let tagAppendPos = s:FindLastTag(s:docCommentStart, s:docCommentEnd, 'since')
  endif

  if tagAppendPos < 0 
    let tagAppendPos = s:docCommentEnd - 1
  endif

  let exceptions = g:exceptionList
  if len(exceptions) > 0
    for ex in exceptions
      let tagPos = s:FindTag(s:docCommentStart, s:docCommentEnd, 'throws', ex)
      if tagPos > -1
        let tagAppendPos = tagPos
        continue
      else
       let s:appendPos = tagAppendPos
       call s:AppendStr(' * @throws ' . ex. ' ' . s:defaultTParamText)
       call s:MarkUpdateMade(tagAppendPos + 1)
       let s:docCommentEnd = s:docCommentEnd + 1
       let tagAppendPos = tagAppendPos + 1
      endif
    endfor
  endif

  if exists("b:scommenter_remove_tags_on_update") && b:scommenter_remove_tags_on_update
    call s:RemoveNonExistingExceptions()
  endif
endfunction

function! s:RemoveNonExistingExceptions()
  let exceptionList = g:exceptionList
  let pos = s:FindFirstTag(s:docCommentStart, s:docCommentEnd, 'throws')
  let Start = pos

  while pos > 0
    let line = getline(pos)
    let tagEx = substitute(line, '^\s*\(\*\s*\)\=@[a-zA-Z]*\s\+\(\S*\).*', '\2', '')

    let exExists = 0

    let exceptions = g:exceptionList
    if len(exceptions) > 0
      for ex in exceptions

        if tagEx == ex
          let exExists = 1
	  break
        endif
      endfor
    endif

    if exExists == 0
      call s:RemoveTag(Start, s:docCommentEnd, 'throws', tagEx)
    else
      let Start = Start + 1
    endif

    let g:exceptionList = exceptionList
    let pos = s:FindFirstTag(Start, s:docCommentEnd, 'throws')
  endwhile
endfunction





" --------------------------------
" Tag Functions
" --------------------------------

function! s:FindTag(rangeStart, rangeEnd, tagName, tagParam)
  let i = a:rangeStart
  while i <= a:rangeEnd
    if a:tagParam != ''
      if getline(i) =~ '^\s*\(\*\s*\)\=@' . a:tagName . '\s\+' . a:tagParam . '\(\s\|$\)'
        return i
      endif
    else
      if getline(i) =~ '^\s*\(\*\s*\)\=@' . a:tagName . '\(\s\|$\)'
        return i
      endif
    endif 
    let i = i + 1
  endwhile
  return -1
endfunction

function! s:FindFirstTag(rangeStart, rangeEnd, tagName)
  let i = a:rangeStart
  while i <= a:rangeEnd
    if getline(i) =~ '^\s*\(\*\s*\)\=@' . a:tagName . '\(\s\|$\)'
      return i
    endif
    let i = i + 1
  endwhile
  return -1
endfunction

function! s:FindLastTag(rangeStart, rangeEnd, tagName)
  let i = a:rangeEnd
  while i >= a:rangeStart
    if getline(i) =~ '^\s*\(\*\s*\)\=@' . a:tagName . '\(\s\|$\)'
      return i
    endif
    let i = i - 1
  endwhile
  return -1
endfunction

function! s:FindAnyTag(rangeStart, rangeEnd)
  let i = a:rangeStart
  while i <= a:rangeEnd
    if getline(i) =~ '^\s*\(\*\s*\)\=@'
      return i
    endif
    let i = i + 1
  endwhile
  return -1
endfunction

function! s:RemoveTag(rangeStart, rangeEnd, tagName, tagParam)
  let tagStartPos = s:FindTag(a:rangeStart, a:rangeEnd, a:tagName, a:tagParam)
  if tagStartPos == -1
    return 0
  endif
  let tagEndPos = s:FindAnyTag(tagStartPos + 1, a:rangeEnd)
  if tagEndPos == -1
    let tagEndPos = s:docCommentEnd - 1
  endif
  let linesToDelete = tagEndPos - tagStartPos
  if linesToDelete == 0
    let linesToDelete = 1
  endif
  exe "normal " . tagStartPos . "G" . linesToDelete . "dd"
  let s:docCommentEnd = s:docCommentEnd - linesToDelete
endfunction




function! s:scanBracketType(str, atTop, startPos)
  if a:atTop == 1
    let g:tparamList = []
  endif
  let str = a:str
  let startPos = a:startPos
  let currentPos = startPos
  let len = strlen(str)

  let c = strpart(str, currentPos, 1)
  if c == '[' && currentPos < len
    let currentPos = currentPos + 1
    let c = strpart(str, currentPos, 1)
    let startTParam = currentPos

    while c != ']' && currentPos < len 
        if c == '['
          let currentPos = s:scanBracketType(str, 0, currentPos)
        elseif c == ',' 
          let tparamStr = s:Trim(strpart(str, startTParam, currentPos - startTParam))
          let c = strpart(tparamStr, 0, 1)
          if c != '['
            " TODO: not correct
            " regular expression for a template parameter 
            let tparam = substitute(tparamStr, '^[+\|-]\?\([^\s\|<\|>]\+\).*', '\1', '')
            call add(g:tparamList, s:Trim(tparam))
          endif
          let startTParam = currentPos + 1
        endif
        let currentPos = currentPos + 1
        let c = strpart(str, currentPos, 1)
    endwhile

    let tparamStr = s:Trim(strpart(str, startTParam, currentPos - startTParam))
    let c = strpart(tparamStr, 0, 1)
    if c != '['
      let tparam = substitute(tparamStr, '^[+\|-]\?\([^\s\|<\|>]\+\).*', '\1', '')
      if tparam != ''
        call add(g:tparamList, s:Trim(tparam))
      endif
    endif

  elseif c == ']'
    let currentPos = currentPos + 1
  endif
  return currentPos
endfunction

function! s:scanParanType(str, atTop, startPos)
  if a:atTop == 1
    let g:paramList = []
  endif

  let str = a:str
  let atTop = a:atTop
  let startPos = a:startPos
  let currentPos = startPos
  let len = strlen(str)

  let c = strpart(str, currentPos, 1)
  if c == '(' && currentPos < len
    let currentPos = currentPos + 1
    let c = strpart(str, currentPos, 1)
    let startParam = currentPos

    while c != ')' && currentPos < len 
        if c == '('
          let currentPos = s:scanParanType(str, 0, currentPos)
        elseif c == ',' && atTop == 1
          let param = s:Trim(strpart(str, startParam, currentPos - startParam))
          call add(g:paramList, param)
          let startParam = currentPos + 1
        endif
        let currentPos = currentPos + 1
        let c = strpart(str, currentPos, 1)
    endwhile

    if atTop == 1
      let param = s:Trim(strpart(str, startParam, currentPos - startParam))
      if param != ''
        call add(g:paramList, param)
      endif
    endif

  elseif c == ')'
    let currentPos = currentPos + 1
  endif

  return currentPos
endfunction


function! s:WriteMethodComments()
  let annotationCount = s:GetAnnotationCount()
  let s:appendPos = s:rangeStart - 1 - annotationCount
  let s:indent = s:method_indent

  if exists("b:scommenter_method_description_space")
    let descriptionSpace = b:scommenter_method_description_space
  else
    let descriptionSpace = s:defaultMethodDescriptionSpace
  endif

  let s:docCommentStart = s:appendPos
  call s:AppendStr('/** ')

  call s:AppendStr(' * ' . s:method_name)

  let params = g:paramList
  let params_len = len(params)

  let tparams = g:tparamList
  let tparams_len = len(tparams)

  let exceptions = g:exceptionList
  let exceptions_len = len(exceptions)

  if params_len == 0 && tparams_len == 0 && exceptions_len == 0 && s:method_returnValue == '' && exists("b:scommenter_smart_description_spacing") && b:scommenter_smart_description_spacing
    call s:AppendStars(1)
  else 
    call s:AppendStars(descriptionSpace)
  endif

  if params_len > 0
    for param in params
      let paramName = s:Trim(substitute(param, '\([^:]*\).*', '\1', ''))
      let paramType = s:Trim(substitute(param, '[^:]*:\(.*\)', '\1', ''))
      call s:AppendStr(' * @param  ' . paramName . ' (' . paramType . ') ' . s:defaultParamText)
    endfor
  endif

  if tparams_len > 0
    for tparam in tparams
      call s:AppendStr(' * @tparam ' . tparam . ' ' . s:defaultTParamText)
    endfor
  endif

  if s:method_returnValue != ''
    call s:AppendStr(' * @return (' . s:method_returnValue . ')')
  endif

  if exceptions_len > 0
    for ex in exceptions
      call s:AppendStr(' * @throws ' . ex . ' ' . s:defaultExceptionText)
    endfor
  endif

  let s:docCommentEnd = s:appendPos
  call s:AppendStr(' */')
  call s:MoveCursor()
endfunction

" ============================================================================
" Inner Class Comment Functions
" ============================================================================
function! s:IsInnerClass()
  return s:combinedString =~ s:scalaInnerClassPattern
endfunction

function! s:HandleInnerClass()
  let s:indent = s:GetIndentation(s:combinedString)
  let g:paramList = []
  let g:tparamList = []
  let str = s:combinedString

  if exists("b:scommenter_inner_class_description_space")
    let descriptionSpace = b:scommenter_inner_class_description_space
  else
    let descriptionSpace = s:defaultInnerClassDescriptionSpace
  endif

  let str = s:Trim(substitute(str, s:scalaname .'\(.*\)', '\1', ''))

  let startBracketType = stridx(str, '[')
  if startBracketType != -1
    let endBracketType = s:scanBracketType(str, 1, startBracketType)
    let str = strpart(str, endBracketType + 1)
  endif

  let paranStart = stridx(str, '(')
  if paranStart != -1
    let paranEnd = s:scanParanType(str, 1, paranStart)
    let str = s:Trim(strpart(str, paranEnd))
  endif

  let annotationCount = s:GetAnnotationCount()

" XXXXXXXXXXXXX
  let existingDocCommentType = s:HasDocComments()
  let s:method_comment_update_only = 0

  if existingDocCommentType && exists("b:scommenter_update_comments") && b:scommenter_update_comments
    let s:method_comment_update_only = 1
    if existingDocCommentType == 1 
      call s:ExpandSinglelineCommentsEx(s:singleLineCommentPos, 1)
    endif
    let s:firstUpdatedTagLine = -1
    call s:UpdateAllTags(0)

    if exists("b:scommenter_move_cursor") && b:scommenter_move_cursor && s:firstUpdatedTagLine != -1
      exe "normal " . s:firstUpdatedTagLine . "G$"
      if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
        startinsert!
      endif
    endif
  else
    let s:appendPos = s:rangeStart - 1 - annotationCount
    let s:docCommentStart = s:appendPos
    call s:AppendStr('/** ')

    let params = g:paramList
    let params_len = len(params)

    let tparams = g:tparamList
    let tparams_len = len(tparams)

    let exceptions = g:exceptionList
    let exceptions_len = len(exceptions)

    if params_len == 0 && tparams_len == 0 && exists("b:scommenter_smart_description_spacing") && b:scommenter_smart_description_spacing
      call s:AppendStars(1)
    else
      call s:AppendStars(descriptionSpace)
    endif

    if params_len > 0
      for param in params
        let paramName = s:Trim(substitute(param, '\([^:]*\).*', '\1', ''))
        let paramType = s:Trim(substitute(param, '[^:]*:\(.*\)', '\1', ''))
        call s:AppendStr(' * @param  ' . paramName . ' (' . paramType . ') ' . s:defaultParamText)
      endfor
    endif

    if tparams_len > 0
      for tparam in tparams
        call s:AppendStr(' * @tparam ' . tparam . ' ' . s:defaultTParamText)
      endfor
    endif

    if exceptions_len > 0
      for ex in exceptions
        call s:AppendStr(' * @throws ' . ex . ' ' . s:defaultExceptionText)
      endfor
    endif

    let s:docCommentEnd = s:appendPos
    call s:AppendStr(' */')
    call s:MoveCursor()
  endif
endfunction



function! s:IsInnerObject()
  return s:combinedString =~ s:scalaInnerObjectPattern
endfunction

function! s:HandleInnerObject()
  let s:indent = s:GetIndentation(s:combinedString)

  if exists("b:scommenter_inner_object_description_space")
    let descriptionSpace = b:scommenter_inner_object_description_space
  else
    let descriptionSpace = s:defaultInnerObjectDescriptionSpace
  endif

  let s:appendPos = s:rangeStart - 1
  let s:docCommentStart = s:appendPos
  call s:AppendStr('/** ')

  call s:AppendStars(descriptionSpace)

  let s:docCommentEnd = s:appendPos
  call s:AppendStr(' */')
  call s:MoveCursor()
endfunction




function! s:IsInnerTrait()
  return s:combinedString =~ s:scalaInnerTraitPattern
endfunction

function! s:HandleInnerTrait()
  let s:indent = s:GetIndentation(s:combinedString)
  let g:paramList = []
  let g:tparamList = []
  let str = s:combinedString

  if exists("b:scommenter_inner_trait_description_space")
    let descriptionSpace = b:scommenter_inner_trait_description_space
  else
    let descriptionSpace = s:defaultInnerTraitDescriptionSpace
  endif

  let str = s:Trim(substitute(str, s:scalaname .'\(.*\)', '\1', ''))

  let startBracketType = stridx(str, '[')
  if startBracketType != -1
    let endBracketType = s:scanBracketType(str, 1, startBracketType)
    let str = strpart(str, endBracketType + 1)
  endif

" XXXXXXXXXXXXX
  let existingDocCommentType = s:HasDocComments()
  let s:method_comment_update_only = 0

  if existingDocCommentType && exists("b:scommenter_update_comments") && b:scommenter_update_comments
    let s:method_comment_update_only = 1
    if existingDocCommentType == 1 
      call s:ExpandSinglelineCommentsEx(s:singleLineCommentPos, 1)
    endif
    let s:firstUpdatedTagLine = -1
    call s:UpdateAllTags(0)

    if exists("b:scommenter_move_cursor") && b:scommenter_move_cursor && s:firstUpdatedTagLine != -1
      exe "normal " . s:firstUpdatedTagLine . "G$"
      if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
        startinsert!
      endif
    endif
  else

    let s:appendPos = s:rangeStart - 1
    let s:docCommentStart = s:appendPos
    call s:AppendStr('/** ')

    let tparams = g:tparamList
    let tparams_len = len(tparams)

    if tparams_len == 0 && exists("b:scommenter_smart_description_spacing") && b:scommenter_smart_description_spacing
      call s:AppendStars(1)
    else
      call s:AppendStars(descriptionSpace)
    endif

    if tparams_len > 0
      for tparam in tparams
        call s:AppendStr(' * @tparam ' . tparam . ' ' . s:defaultTParamText)
      endfor
    endif

    let s:docCommentEnd = s:appendPos
    call s:AppendStr(' */')
    call s:MoveCursor()
  endif
endfunction


" ============================================================================
" Class Comment Functions
" ============================================================================

function! s:IsClass()
  return s:combinedString =~ s:scalaClassPattern
endfunction


function! s:HandleClass()
  let s:indent = s:GetIndentation(s:combinedString)
  let g:paramList = []
  let g:tparamList = []
  let str = s:combinedString

  if exists("b:scommenter_class_description_space")
    let descriptionSpace = b:scommenter_class_description_space
  else
    let descriptionSpace = s:defaultClassDescriptionSpace
  endif

  let str = s:Trim(substitute(str, s:scalaname .'\(.*\)', '\1', ''))

  let startBracketType = stridx(str, '[')
  if startBracketType != -1
    let endBracketType = s:scanBracketType(str, 1, startBracketType)
    let str = strpart(str, endBracketType + 1)
  endif

  let paranStart = stridx(str, '(')
  if paranStart != -1
    let paranEnd = s:scanParanType(str, 1, paranStart)
    let str = s:Trim(strpart(str, paranEnd))
  endif

  let annotationCount = s:GetAnnotationCount()

" XXXXXXXXXXXXX
  let existingDocCommentType = s:HasDocComments()
  let s:method_comment_update_only = 0

  if existingDocCommentType && exists("b:scommenter_update_comments") && b:scommenter_update_comments
    let s:method_comment_update_only = 1
    if existingDocCommentType == 1 
      call s:ExpandSinglelineCommentsEx(s:singleLineCommentPos, 1)
    endif
    let s:firstUpdatedTagLine = -1
    call s:UpdateAllTags(0)

    if exists("b:scommenter_move_cursor") && b:scommenter_move_cursor && s:firstUpdatedTagLine != -1
      exe "normal " . s:firstUpdatedTagLine . "G$"
      if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
        startinsert!
      endif
    endif
  else

    let s:appendPos = s:rangeStart - 1 - annotationCount
    let s:docCommentStart = s:appendPos
    call s:AppendStr('/** ')

    let params = g:paramList
    let params_len = len(params)

    let tparams = g:tparamList
    let tparams_len = len(tparams)

    let exceptions = g:exceptionList
    let exceptions_len = len(exceptions)

    if params_len == 0 && tparams_len == 0 && exists("b:scommenter_smart_description_spacing") && b:scommenter_smart_description_spacing
      call s:AppendStars(1)
    else
      call s:AppendStars(descriptionSpace)
    endif

    if exists('b:scommenter_class_author')
      call s:AppendStr(' * @author ' . b:scommenter_class_author)
    endif
    if exists('b:scommenter_class_version')
      call s:AppendStr(' * @version ' . b:scommenter_class_version)
    endif
    if exists('b:scommenter_since_release')
      call s:AppendStr(' * @since ' . b:scommenter_since_release)
    endif

    if params_len > 0 || tparams_len > 0
      if exists("b:scommenter_smart_since_spacing") && b:scommenter_smart_since_spacing
        call s:AppendStars(1)
      endif
    endif

    if params_len > 0
      for param in params
        let paramName = s:Trim(substitute(param, '\([^:]*\).*', '\1', ''))
        let paramType = s:Trim(substitute(param, '[^:]*:\(.*\)', '\1', ''))
        call s:AppendStr(' * @param  ' . paramName . ' (' . paramType . ') ' . s:defaultParamText)
      endfor
    endif

    if tparams_len > 0
      for tparam in tparams
        call s:AppendStr(' * @tparam ' . tparam . ' ' . s:defaultTParamText)
      endfor
    endif

    if exceptions_len > 0
      for ex in exceptions
        call s:AppendStr(' * @throws ' . ex . ' ' . s:defaultExceptionText)
      endfor
    endif

    let s:docCommentEnd = s:appendPos
    call s:AppendStr(' */')
    call s:MoveCursor()
  endif
endfunction




function! s:IsObject()
  return s:combinedString =~ s:scalaObjectPattern
endfunction

function! s:HandleObject()
  let s:indent = s:GetIndentation(s:combinedString)

  if exists("b:scommenter_object_description_space")
    let descriptionSpace = b:scommenter_object_description_space
  else
    let descriptionSpace = s:defaultObjectDescriptionSpace
  endif

  let s:appendPos = s:rangeStart - 1
  let s:docCommentStart = s:appendPos
  call s:AppendStr('/** ')

  call s:AppendStars(descriptionSpace)

  if exists('b:scommenter_class_author')
    call s:AppendStr(' * @author ' . b:scommenter_class_author)
  endif
  if exists('b:scommenter_class_version')
    call s:AppendStr(' * @version ' . b:scommenter_class_version)
  endif
  if exists('b:scommenter_since_release')
    call s:AppendStr(' * @since ' . b:scommenter_since_release)
  endif

  let s:docCommentEnd = s:appendPos
  call s:AppendStr(' */')
  call s:MoveCursor()
endfunction




function! s:IsTrait()
  return s:combinedString =~ s:scalaTraitPattern
endfunction

function! s:HandleTrait()
  let s:indent = s:GetIndentation(s:combinedString)
  let g:paramList = []
  let g:tparamList = []
  let str = s:combinedString

  if exists("b:scommenter_trait_description_space")
    let descriptionSpace = b:scommenter_trait_description_space
  else
    let descriptionSpace = s:defaultTraitDescriptionSpace
  endif

  let str = s:Trim(substitute(str, s:scalaname .'\(.*\)', '\1', ''))

  let startBracketType = stridx(str, '[')
  if startBracketType != -1
    let endBracketType = s:scanBracketType(str, 1, startBracketType)
    let str = strpart(str, endBracketType + 1)
  endif

" XXXXXXXXXXXXX
  let existingDocCommentType = s:HasDocComments()
  let s:method_comment_update_only = 0

  if existingDocCommentType && exists("b:scommenter_update_comments") && b:scommenter_update_comments
    let s:method_comment_update_only = 1
    if existingDocCommentType == 1 
      call s:ExpandSinglelineCommentsEx(s:singleLineCommentPos, 1)
    endif
    let s:firstUpdatedTagLine = -1
    call s:UpdateAllTags(0)

    if exists("b:scommenter_move_cursor") && b:scommenter_move_cursor && s:firstUpdatedTagLine != -1
      exe "normal " . s:firstUpdatedTagLine . "G$"
      if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
        startinsert!
      endif
    endif
  else

    let s:appendPos = s:rangeStart - 1
    let s:docCommentStart = s:appendPos
    call s:AppendStr('/** ')

    let tparams = g:tparamList
    let tparams_len = len(tparams)

    if tparams_len == 0 && exists("b:scommenter_smart_description_spacing") && b:scommenter_smart_description_spacing
      call s:AppendStars(1)
    else 
      call s:AppendStars(descriptionSpace)
    endif

    if exists('b:scommenter_class_author')
      call s:AppendStr(' * @author ' . b:scommenter_class_author)
    endif
    if exists('b:scommenter_class_version')
      call s:AppendStr(' * @version ' . b:scommenter_class_version)
    endif
    if exists('b:scommenter_since_release')
      call s:AppendStr(' * @since ' . b:scommenter_since_release)
    endif

    if tparams_len > 0
      if exists("b:scommenter_smart_since_spacing") && b:scommenter_smart_since_spacing
        call s:AppendStars(1)
      endif
    endif

    if tparams_len > 0
      for tparam in tparams
        call s:AppendStr(' * @tparam ' . tparam . ' ' . s:defaultTParamText)
      endfor
    endif

    let s:docCommentEnd = s:appendPos
    call s:AppendStr(' */')
    call s:MoveCursor()
  endif
endfunction


" ============================================================================
" Variable Comment Functions
" ============================================================================
function! s:IsVariable()
  return s:combinedString =~ s:scalaVariablePattern
endfunction

function! s:HandleVariable()
  let s:appendPos = s:rangeStart - 1
  let s:docCommentStart = s:appendPos

  let s:indent = s:GetIndentation(s:combinedString)
  if exists("b:scommenter_field_description_space")
    let descriptionSpace = b:scommenter_field_description_space
  else
    let descriptionSpace = s:defaultFieldDescriptionSpace
  endif

  if descriptionSpace == -1
    let s:docCommentEnd = s:appendPos
    call s:AppendStr('/**  */')
    if exists("b:scommenter_move_cursor")
      normal k$hh
      if exists("b:scommenter_autostart_insert_mode")
        startinsert
      endif
    endif
  else
    call s:AppendStr('/** ')
    call s:AppendStars(descriptionSpace)
    let s:docCommentEnd = s:appendPos
    call s:AppendStr(' */')
    call s:MoveCursor()
  endif
endfunction

" ===========================================================================
" Value Comment Functions
" ============================================================================
function! s:IsValue()
  return s:combinedString =~ s:scalaValuePattern
endfunction

function! s:HandleValue()
  let s:appendPos = s:rangeStart - 1
  let s:docCommentStart = s:appendPos

  let s:indent = s:GetIndentation(s:combinedString)
  if exists("b:scommenter_field_description_space")
    let descriptionSpace = b:scommenter_field_description_space
  else
    let descriptionSpace = s:defaultFieldDescriptionSpace
  endif

  if descriptionSpace == -1
    let s:docCommentEnd = s:appendPos
    call s:AppendStr('/**  */')
    if exists("b:scommenter_move_cursor")
      normal k$hh
      if exists("b:scommenter_autostart_insert_mode")
        startinsert
      endif
    endif
  else
    call s:AppendStr('/** ')
    call s:AppendStars(descriptionSpace)
    let s:docCommentEnd = s:appendPos
    call s:AppendStr(' */')
    call s:MoveCursor()
  endif
endfunction

" ============================================================================
" Existing Doc Comments
" ============================================================================

function! s:HasMultilineDocComments()
  let annotationCount = s:GetAnnotationCount()
  let linenum = s:rangeStart - 1 - annotationCount
  let str = getline(linenum)
  while str =~ '^\s*$' && linenum > 1
    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile
  if str !~ '\*/\s*$' || str =~ '/\*\*.*\*/'
    return 0
  endif
  let s:docCommentEnd = linenum
  let linenum = linenum - 1
  let str = getline(linenum)
  while str !~ '\(/\*\|\*/\)' && linenum >= 1
    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile
  if str =~ '^\s*/\*\*'
    let s:docCommentStart = linenum
    return 1
  else
    let s:docCommentStart = -1
    let s:docCommentEnd   = -1
    return 0
  endif
endfunction

function! s:HasSingleLineDocComments()
  let annotationCount = s:GetAnnotationCount()
  let linenum = s:rangeStart - 1 - annotationCount
  let str = getline(linenum)
  while str =~ '^\s*$' && linenum > 1
    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile
  if str =~ '^\s*/\*\*.*\*/\s*$'
    let s:singleLineCommentPos = linenum
    let s:docCommentStart = linenum
    let s:docCommentEnd   = linenum
    return 1
  endif
  return 0
endfunction

function! s:GetAnnotationCount()
  let g:exceptionList = []

  let linenum = s:rangeStart - 1
  let str = getline(linenum)
  while str =~ '^\s*@[a-zA-Z]\+' && linenum > 1
    if str =~ '^\s*@throws('
      let ex = substitute(str, '^\s*@throws(classOf\[\([^\]]*\)\])' , '\1', '')
      if ex != ''
        call add(g:exceptionList, ex)
      endif
    endif

    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile
  return s:rangeStart - 1 - linenum
endfunction

function! s:HasDocComments()
  if s:HasSingleLineDocComments()
    return 1 
  elseif s:HasMultilineDocComments()
    return 2
  else
    return 0
  endif
endfunction

" ============================================================================
" From single line to multi line
" ============================================================================

function! s:ExpandSinglelineCommentsEx(line, space)
  let str = getline(a:line)
  let singleLinePattern = '^\s*/\*\*\s*\(.*\)\*/\s*$'
  if str !~ singleLinePattern
    return
  endif
  let s:indent = s:GetIndentation(str)
  let str = substitute(str, singleLinePattern, '\1', '')
  exe "normal " . a:line . "Gdd"
  let s:appendPos = a:line - 1
  call s:AppendStr('/**')
  call s:AppendStr(' * ' . str)
  let i = 0
  while a:space > i
    call s:AppendStr(' * ')
    let i = i + 1
  endwhile
  call s:AppendStr(' */')
  let s:docCommentStart = a:line
  let s:docCommentEnd   = a:line + 2 + a:space
endfunction

function! s:ExpandSinglelineComments(line)
  call s:ExpandSinglelineCommentsEx(a:line, 0)
endfunction

" ============================================================================
" Utility functions
" ============================================================================

function! s:AddEmpty()
  if exists("b:scommenter_add_empty_line") && b:scommenter_add_empty_line
    let pos = s:docCommentStart
    if getline(pos) !~ '^\s*$'
      let s:appendPos = pos
      call s:AppendStr("")
    endif
  endif
endfunction

function! s:GetIndentation(string)
  return substitute(a:string, '^\(\s*\).*', '\1', '')
endfunction

function! s:Message(string)
  echo '[ScalaCommenter] ' . a:string
endfunction

" returns one string combined from the strings on the given range.
function! s:GetCombinedString(rangeStart, rangeEnd)
  let line = a:rangeStart
  let combinedString = getline(line)

  while line < a:rangeEnd
    let line = line + 1
    let combinedString = combinedString . ' ' . getline(line)
  endwhile

  return substitute(combinedString, '^\([^;{]*[;{]\=\).*', '\1', '')
endfunction


function! s:MoveCursorToEOL(line)
  exe "normal " . a:line . "G$"
endfunction

function! s:MoveCursor() 
  if !exists("b:scommenter_move_cursor")
    return
  endif
  if !b:scommenter_move_cursor
    return
  endif

  if exists("b:scommenter_description_starts_from_first_line") && b:scommenter_description_starts_from_first_line
    call s:MoveCursorToEOL(s:docCommentStart)
  else
    call s:MoveCursorToEOL(s:docCommentStart + 1)
  endif

  if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
    startinsert
  endif

endfunction

function! s:AppendStars(amount)
  let i = a:amount
  while i > 0
    call s:AppendStr(' * ')
    let i = i - 1
  endwhile
endfunction

function! s:AppendString(amount, str)
  let i = a:amount
  let str = a:str
  while i > 0
    call s:AppendStr(str)
    let i = i - 1
  endwhile
endfunction

let s:appendPos = 1

" A function for appending strings to the buffer.
" First set the 's:appendPos', then call this function repeatedly to append
" strings after that position.
function! s:AppendStr(string)
  call append(s:appendPos, s:indent . a:string)
  let s:appendPos = s:appendPos + 1
endfunction

function! s:AppendCommentLine(string)
  call s:AppendStr('* ' . a:string)
endfunction

function! s:DeleteLines(startPos, endPos)
  execute a:startPos . ',' . a:endPos . 'd'
endfunction

function! s:Trim(string)
  return substitute(a:string, '^\s*\(.\{-}\)\s*$', '\1', '')
endfunction

function! s:ExistsAndTrue(value)
  return exists(value) && value
endfunction

function! s:ExistsAndNotEmpty(value)
  return exists(value) && value != ''
endfunction


