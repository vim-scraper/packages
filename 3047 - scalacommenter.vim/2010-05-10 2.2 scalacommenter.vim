" ============================================================================
" ScalaCommenter.vim
"
" $Id: scalacommenter.vim 318 2010-05-10 22:47:17Z  $
"
" Manage ScalaDoc comments for classes, traits, objects, methods, 
"  vals and vars:
"    Generate comment templates and
"    Format existing comment '@' tag lines
"
" The comment generation code here inspired by Kalle Bjorklid's JCommenter
"   http://www.vim.org/scripts/script.php?script_id=20
" Some of the basic utility functions, some recognition patterns
" and many of the configuration parameters can be found in JCommenter.
" While JCommenter is written in a imperative style, ScalaCommenter
" has components that are Object Prototype based building on 
" the Self.vim script (which can be found at 
" http://www.vim.org/scripts/script.php?script_id=3072).
"
" The parameter algorithm has been re-worked, a template parameter 
" algorithm has been added for Scala; as well as recognizing class 
" constructors, values and variables.
" The formatting part of this script is also new.
"
" ============================================================================
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
" When formatting @see tags, no attempt is made to re-order 2 or more of them.
" The formatter has no knowledge of embedded HTML tags.
" There is currently no support for reading an entity's annotations 
"   and generating comment tags except for the "@throws" and "@deprecated"   
"   annotations which do work.  To support such a feature, users would 
"   have to be able to write their own regular expression to recognize 
"   and extract one or more elements.
"
" ============================================================================

" ============================================================================
" Configuration Options:
"   These help control the behavior of ScalaComment.vim
"   Remember, if you change these and then upgrade to a later version, 
"   your changes will be lost.
" ============================================================================

" Define these just to make the code a little more readable
"   Remember, these are script local and can not be used in your .vimrc
"   file (but 0 and 1 can be used)
let s:IS_FALSE = 0
let s:IS_TRUE = 1

" Move cursor to the place where inserting comments supposedly should start
let b:scommenter_move_cursor = s:IS_TRUE

" Defines whether to move the cursor to the line which has "/**", or the line
"   after that (effective only if b:scommenter_move_cursor is enabled)
let b:scommenter_description_starts_from_first_line = s:IS_FALSE

" Start insert mode after calling the commenter. Effective only if 
"   b:scommenter_move_cursor is enabled.
let b:scommenter_autostart_insert_mode = s:IS_FALSE

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
let b:scommenter_smart_description_spacing = s:IS_TRUE

" For top-level classes with parameters and template parameters and traits with
"   template parameters, if enabled then an empty line separates the 
"   @since tag and any @param and/or @tparam tags.
"
" Note: currently not supported
" let b:scommenter_smart_since_spacing = s:IS_TRUE

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
let b:scommenter_class_svn_id = '$Id: scalacommenter.vim 318 2010-05-10 22:47:17Z  $'

" The default author added to the file comments. Leave empty to add just the
"   field where the author can be added, or comment-out to remove it.
let b:scommenter_file_author = 'Ada Lovelace'

" The default copyright holder added to the file comments. Leave empty to
"   add just the field where the copyright info can be added, or comment-out
"   to remove it.
let b:scommenter_file_copyright_line = ''
let b:scommenter_company_name = 'ScalaCorp, Inc.'
let b:scommenter_file_copyright_list = [
\    'Copyright 2010 ' . b:scommenter_company_name . 'All rights reserved',
\    'PPOPRIETARY/CONFIDENTIAL, Use is subject to licence terms.'
\]

" Set to true if you don't like the automatically added "created"-time
let b:scommenter_file_noautotime = s:IS_FALSE

" Define whether scommenter tries to parse and update the existing Doc-comments
"   on the item it was executed on. If this feature is disabled, a completely 
"   new comment-template is written
let b:scommenter_update_comments = s:IS_TRUE


" Whether to prepend an empty line before the generated comment, if the
"   line just above the comment would otherwise be non-empty.
let b:scommenter_add_empty_line = 1

" Uncomment and modify if you're not happy with the default file
"   comment-template:
"
"function! SCommenter_OwnFileComments()
"  call append(0, '/*')
"  call append(1, ' * File name   : ' . bufname("%"))
"  call append(2, ' * authors     : ')
"  call append(3, ' * created     : ' . strftime("%c"))
"  call append(4, ' *')
"  call append(5, ' */')
"endfunction

" Set to s:IS_TRUE to use the StdFileComments function to write file comments
let b:scommenter_std_file_comments = s:IS_FALSE

" Set to s:IS_TRUE to use the ScalaAPIFileComments function to write file comments
let b:scommenter_scala_api_file_comments = s:IS_FALSE 

" Set to s:IS_TRUE to use the SunFileComments function to write file comments
let b:scommenter_sun_file_comments = s:IS_FALSE

" If true, then only top-level template parameters have @tparam tags 
"   generated. If false, then all template parameters have tags generated
let b:scommenter_top_level_tparams_only = s:IS_TRUE

" Line width of a page. This is used when wrapping text in comments
let b:scommenter_page_width = 80

" If positive, this is the value used to offset extra tag text lines,
"   If non-positive, then the extra text lines will line up with the
"   tag's first line's text.
" NOTE: This is a very useful formatting parameter, I suggest defining
"   it to be 20. That way extra text lines will not have to be all
"   the way on the right like the first text line.
let b:scommenter_extra_line_text_offset = -1

" Used by a user to define third-party tags
" format of a user tag definition:
"     tagName, hasValue, hasText, canHaveMoreThanOne
"  where
"     tagName             - name of the third-party tag (@name)
"     hasValue            - a value string (possibly with spaces but
"                           single line)
"     hasText             - text description (possibly multi-line)
"     canHaveMoreThanOne  - can there be more than on such tag
"
"   Remember that if a tag type has a value, then the combination of
"   tagName and tagValue is unique per comment.
"
" Examples:
"   Tag found in Scala code:
"    @note text
"       let b:scommenter_user_tags = [
"       \["note", 0, 1, 0]
"       \]
"     has name, no value, has text, singleton
"
"   Tags found in earlier Scala code:
"    @pre text
"    @post text
"    @requires value text
"    @provides text
"       let b:scommenter_user_tags = [
"       \["pre", 0, 1, 0],
"       \["post", 0, 1, 0],
"       \["requires", 1, 1, 0],
"       \["provides", 0, 1, 0]
"       \]
"
"   Class concurrency annotations
"    @immutable
"    @threadsafe
"    @notthreadsave
"       let b:scommenter_user_tags = [
"       \["immutable", 0, 1, 0],
"       \["threadsafe", 0, 1, 0],
"       \["notthreadsafe", 0, 1, 0]
"       \]
"
"   Method and Field concurrency annotations
"    @guardedby value
"       let b:scommenter_user_tags = [
"       \["guardedby", 1, 1, 0]
"       \]
"     has name, has value, has text, singleton
"
let b:scommenter_user_tags = []

" If true, then warning messages are printed, otherwise nothing
"   If some configuration parameter is not doing what you think it 
"   should or nothing is happening, then set this to s:IS_TRUE (in your
"   .vimrc file using 1)
let b:scommenter_warning_message_enable = s:IS_FALSE

" Should an empty comment line be printed between an user/unknown
"   tags the standard tags. If there are no user/unknown tags, then
"   no line is produced.
let b:scommenter_line_between_user_unknown_and_std_tags = s:IS_TRUE

" Should the user/unknown tags be formatted before any of the 
"   standard tags. If there are no user/unknown tags, then this
"   parameter does nothing.
let b:scommenter_user_unknown_before_std_tags = s:IS_TRUE
      
" When re-generating comments sometimes the values associated with of @param
"   or @tparam will change but the text associate with the tag still applies.
"   For instance, if a argument name changes from 'id' to 'identifier',
"   then the original 'param' tag will be deleted and a new one generated.
"   This tag might have had useful descriptive text.
"   If this parameter is true, then rather than being deleted, the original
"   tag is move to the bottom of the comment below a warining line.
"   This allows the user to move the tags text to the new tag if that is
"   what is needed
let b:scommenter_warn_deleted_tags = s:IS_TRUE

" ============================================================================
" End of Configuration Options
" ============================================================================


" ============================================================================
" History:
"
" File:          scalacommenter.vim
" Summary:       Functions for documenting Scala-code
" Author:        Richard Emberson <richard.n.embersonATgmailDOTcom>
" Last Modified: 05/10/2010
" Version:       2.2
" Modifications:
"  2.2 : Method parameter recognition failed def getAtomicVars(atomicMethods:
"          List[XMethodInfo], methods: HashMap[global.Symbol, XMethodInfo], 
"          vars: HashMap[global.Symbol, XVarInfo]) : List[XVarInfo] = {  }
"  2.1 : Method recognition failed for the List methods: '::', 
"          ':::' and 'reverse_:::'.
"  2.0 : Refactored comment generation code using Self.vim, the Vim 
"          dictionary-base object prototype system. Unified the code that
"          generated output for both the writing and re-formatting of
"          comments.
"        Throw tags are now sorted in alphabetical order
"        Unified the comment writing code so that comment formatting,
"          first comment generation, and subsequent generation all
"          use the came code.
"        Text associated with an existing comment tag is no longer lost.
"        Add b:scommenter_top_level_tparams_only which controls if all
"          template parameters have @tparam tags generated or only those
"          at the top-level have tags generated.
"        Fixed scanning parameters, now scans past qualifiers like 'val',
"          'var' or 'private var', etc.
"        Supports curried notations func(a: A)(b: B). 
"        Added b:scommenter_extra_line_text_offset allowing the user to control
"          the offset of any additional text associated with a tag.
"        There is now a b:scommenter_user_tags configuration variable allowing
"          the user to register in their .vimrc file third-party tags.
"        Added b:scommenter_warning_message_enable which controls the printing
"          of warning messages (if any)
"        Added b:scommenter_line_between_user_unknown_and_std_tags which
"          controls if a single comment line is printed between the
"          user/unknown tags and the standard tags.
"        Added b:scommenter_user_unknown_before_std_tags which controls the
"          order of formatting of the user/unknown tags and the standard tags.
"        Added b:scommenter_warn_deleted_tags which allows the user to
"          save the text from tags deleted during re-formatting.
"        Supports capturing parameter template @specialized information
"          in comments.
"        The @deprecated(text) annotation now becomes a ScalaDoc @deprecated
"           tag (just as the @throws annotation does).
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
"   * @serialField
"   * @serialData
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
"      let b:scommenter_std_file_comments = s:IS_TRUE
"   
"   /*
"    * file name   : bufname("%")
"    * authors     : b:scommenter_file_author
"    * created     : strftime("%c")
"    * copyright   : b:scommenter_file_copyright_line
"    *
"    * $Id: scalacommenter.vim 318 2010-05-10 22:47:17Z  $
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
"      let b:scommenter_scala_api_file_comments = s:IS_TRUE
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
"   $Id: scalacommenter.vim 318 2010-05-10 22:47:17Z  $
"
"   For this template everything is hardcoded. If one wants to change, for
"   instance, the copyright dates, this VimScript code must be modified.
"
"
"   The SunFileComments() comment is enabled the configuration parameter:
"      let b:scommenter_sun_file_comments = s:IS_TRUE
"   This mirrors the File Comments found in Sun's Java libraries.
"   
"   /*
"    *  bufname("%")
"    *
"    * Copyright 2010 Sun, Inc. All rights reserved
"    * PPOPRIETARY/CONFIDENTIAL, Use is subject to licence terms.
"    *
"    *  $Id: scalacommenter.vim 318 2010-05-10 22:47:17Z  $
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
"    * b:scommenter_file_copyright_list
"    * 
"    * $Id: scalacommenter.vim 318 2010-05-10 22:47:17Z  $
"    *
"    */
"   
"   The b:scommenter_file_copyright_list is a list of lines that will 
"   appear in the comment. The list can be re-defined in this file
"   or in your .vimrc file to produce your own or your company's
"   copyright statement.
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
"    file (e.g., scalacommenter_config.vim. If this second file is 
"    loaded into vim after this script file, then any configuration changes 
"    you've made in the second file are the one this script file uses 
"    (or you can not make a copy and just edit this file).
"
" 1. Edit the configuration section. It is commented, so I won't explain the
"    options here.
"
" 2. Put something like this in your .vimrc file:
"
"      autocmd FileType scala source $VIM/macros/scalacommenter.vim
"      " and optionally
"      autocmd FileType scala source $VIM/macros/scalacommenter_config.vim
"
"    Note that loading the second, configuration file
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
"   autocmd FileType scala let b:scommenter_file_copyright_list = [
"   \    'COPYRIGHT and more text'
"   \    'blah, blah'
"   \    'and blah'
"   \]
"   autocmd FileType scala let b:scommenter_extra_line_text_offset = 20
"   autocmd FileType scala let b:scommenter_user_tags = [
"   \["pre", 0, 1, 0],
"   \["post", 0, 1, 0],
"   \["requires", 1, 1, 0],
"   \["provides", 0, 1, 0]
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
"   Send any comments and/or bug reports/fixes to:
"       Richard Emberson <richard.n.embersonATgmailDOTcom>
"
"   I will repeat what Kalle Bjorklid said, "Happy coding!  ;-)"
" ============================================================================

" ============================================================================
" THE SCRIPT
" ============================================================================

" If set to true, then when re-sourcing this file during a vim session
"   static/global objects may be initialized again before use.
let s:IN_DEVELOPMENT_MODE = s:IS_FALSE

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

let s:defaultMethodDescriptionSpace       = 1
let s:defaultInnerClassDescriptionSpace   = 1
let s:defaultClassDescriptionSpace        = 1
let s:defaultInnerObjectDescriptionSpace  = 1
let s:defaultObjectDescriptionSpace       = 1
let s:defaultInnerTraitDescriptionSpace   = 1
let s:defaultTraitDescriptionSpace        = 1
let s:defaultFieldDescriptionSpace        = 1

let s:deletedTagWaringStr = 'WARNING: the following tags should be delete'

" These can be used to add to the tag (@param, @tparam and @return) some
" default text.
let s:defaultParamText  = (exists('b:scommenter_default_param'))  ? b:scommenter_default_param : ''
let s:defaultTParamText  = (exists('b:scommenter_default_tparam'))  ? b:scommenter_default_tparam : ''
let s:defaultReturnText = (exists('b:scommenter_default_return')) ? b:scommenter_default_return : ''
let s:defaultExceptionText  = (exists('b:scommenter_default_exception'))  ? b:scommenter_default_exception : ''

let s:getterName = 'get'

let g:booleanNames = [
\  'is',
\  'has'
\]

" Where any user tags are copied to during initialization
"   Must protect its value if we re-source this file from a running
"   vim session.
if ! exists("s:userTags") 
  let s:userTags = []
endif

" ============================================================================
" Patterns
" ============================================================================

let s:scalaname = '[a-zA-Z_][a-zA-Z0-9_]*'

let s:scalaMethodPattern     = '\(^\|\s\+\)def\s.*'

" TODO not used
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

  " TODO redo
  call s:ResetAll()

  let s:combinedString = s:GetCombinedString(s:rangeStart, s:rangeEnd)
  let l:ctype = s:GetCommentType()
  if l:ctype == s:FILE_TYPE
    call s:WriteFileComments()

  elseif l:ctype == s:UNKNOWN_TYPE
    call s:Message('Nothing to do')

  else
    " get entity indentaton

    let l:info = s:newInfo(l:ctype)
    let l:entity = s:GetEntity(l:ctype)
    let l:info.entity = l:entity


" TODO The AppendStr methods still use this
let s:indent = l:info.getIndent()

    " count annotations and record throws
    call s:ProcessAnnotation(l:info)

    " per comment type, extract tags from entity
    call l:entity.extractTags(l:info)

    let l:docCommentType = s:HasDocComments(l:info)

    if l:docCommentType == s:DOC_COMMENT_TYPE_NONE
      " per comment type, write tags


      let s:appendPos = l:info.getDocCommentStart() - 1
      let firstLineText = l:info.getFirstLineText() 
      call s:WriteCommentStart(l:info, firstLineText)

      let tagsSet = l:info.getTagsSet()
      let maxTagNameLen = s:FindMaxTagNameLen(tagsSet) 
      let maxValueLen  = s:FindMaxValueLen(tagsSet)
      let tagTextOffset = maxTagNameLen + maxValueLen + 1
      call s:WriteTags(tagsSet, maxTagNameLen, tagTextOffset) 

      call s:WriteCommentEnd(l:info)

      call s:AddEmptyLineBeforeComment(l:info)

    elseif exists("b:scommenter_update_comments") && b:scommenter_update_comments
      if l:docCommentType == s:DOC_COMMENT_TYPE_SINGLE_LINE
        call s:ExpandSinglelineCommentsEx(l:info, 1)
       endif

      let s:firstUpdatedTagLine = -1

      call s:UpdateAllTags(l:info)

      if exists("b:scommenter_move_cursor") && b:scommenter_move_cursor && s:firstUpdatedTagLine != -1
        exe "normal " . s:firstUpdatedTagLine . "G$"
        if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
          startinsert!
        endif
      endif
    endif

    call l:entity.delete()
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

  call s:ResetAll()

  " check range
  let inComment = s:InComment(s:rangeStart)
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
  let pos = s:rangeStart

  let s:indent = s:GetIndentationAtPosition(pos)
  " Here, we have gotten the indent within a comment (at '  *') not
  " at the first line of a comment (at ' /**), so we must decrease
  " the size of the indent by 1.
  if len(s:indent) > 0
    let s:indent = s:MakeEmptyString(len(s:indent) - 1)
  endif

  let l:ctype = s:FILE_TYPE
  let l:info = s:newInfo(l:ctype)
  call l:info.setDocCommentStart(s:rangeStart)
  call l:info.setDocCommentEnd(s:rangeEnd)

  call s:LoadExistingTags(l:info)


  " delete existing comment
  call s:DeleteExistingComment(l:info)


  let s:appendPos = s:rangeStart - 1

  " write existing comment normal lines
  call s:WriteExistingCommentNormalLines(l:info)


  " write tags
  let l:tagsSet = l:info.comment.tagsSet
  let maxTagNameLen = s:FindMaxTagNameLen(l:tagsSet) 
  let maxValueLen  = s:FindMaxValueLen(l:tagsSet)
  let tagTextOffset = maxTagNameLen + maxValueLen + 1
  call s:WriteTags(l:tagsSet, maxTagNameLen, tagTextOffset) 

  let &ignorecase = s:oldICValue
endfunction

" ============================================================================
" Objects
" ============================================================================

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" SELF.VIM ObjectPrototype
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

"
" The following is part of my self.vim script
"

" ++++++++++++++++++++++++++++++++++++++++++++
" Vim type enumerations.
" ++++++++++++++++++++++++++++++++++++++++++++
let g:NUMBER_TYPE     = type(0)
let g:STRING_TYPE     = type("")
let g:FUNCREF_TYPE    = type(function("tr"))
let g:LIST_TYPE       = type([])
let g:DICTIONARY_TYPE = type({})
let g:FLOAT_TYPE      = type(0.0)

function! g:printDict(item) 
  for key in keys(a:item)
   echo key . ': ' . string(a:item[key])
  endfor
endfunction

" ++++++++++++++++++++++++++++++++++++++++++++
" SELF.VIM ObjectPrototype
" ++++++++++++++++++++++++++++++++++++++++++++
function! g:loadObjectPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("g:ObjectPrototype")
      unlet g:ObjectPrototype
    endif
  endif
  if !exists("g:ObjectPrototype")
    "-----------------------------------------------
    " private variables
    "-----------------------------------------------
    let g:ObjectPrototype = { '_type': 'ObjectPrototype' , '_prototype': '' }

    "-----------------------------------------------
    " public methods
    "-----------------------------------------------
    function g:ObjectPrototype.getType() dict
      return g:ObjectPrototype._getType(self)
    endfunction

    function g:ObjectPrototype.getPrototype() dict
      return g:ObjectPrototype._getPrototype(self)
    endfunction

    function g:ObjectPrototype.instanceOf(prototype) dict
      let type = a:prototype._type
      let parent = self._prototype
      while type(parent) == g:DICTIONARY_TYPE
        if parent._type == type
          return 1
        endif
        if type(parent._prototype) == g:DICTIONARY_TYPE
          let parent = parent._prototype
        else
          break
        endif
      endwhile
      return 0
    endfunction

    function g:ObjectPrototype.equals(obj) dict
      return self == a:obj
    endfunction

    function g:ObjectPrototype.create() dict
      return g:ObjectPrototype._create(self)
    endfunction

    function g:ObjectPrototype.delete() dict
      return g:ObjectPrototype._delete(self)
    endfunction


    "-----------------------------------------------
    " private methods
    "-----------------------------------------------
    
    function g:ObjectPrototype._getType(prototype) dict
      return a:prototype._type
    endfunction

    function g:ObjectPrototype._getPrototype(prototype) dict
      return a:prototype._prototype
    endfunction
    
    " --------------------------------------------
    " Creates a copy of a Prototype or an object 
    "   which is itself a copy of a Prototype.
    "   Private values and methods, those that
    "   start with a single '_' are not copied.
    "   Methods and values with no leading '_' or
    "   with more than one leading '_' are copied.
    " --------------------------------------------
    function g:ObjectPrototype._create(prototype) dict
      let l:i = stridx(a:prototype._type, "Prototype")   
      if l:i == -1
        let l:t = a:prototype._type
        let pt = a:prototype._prototype
      else
        let l:t = strpart(a:prototype._type, 0, l:i)
        let pt = a:prototype
      endif
      let l:o = { '_type': l:t, '_prototype': pt }
      " let l:t = strpart(a:prototype._type, 0, l:i)
      " let l:o = { '_type': l:t, '_prototype': a:prototype }

      for key in keys(a:prototype)
        " If its a function, then arrange that a function is defined
        " that calls the parent function. This allows one later to
        " either redefine a derived object's function only 
        " effecting its behavior (and all of its children) or
        " redefine the base object's funciton effecting all
        " children. If we simply copied the function reference
        " we would lose this flexibility - it would not be 'vim.self'. 
        " Curiously, there is just enough reflective power around to
        " allow this to happen.
        " If you get a function calling recursion error, it maybe 
        " because there is some edge case the following code fails
        " to address. Find the fix and tell me about it :-).
        if type(a:prototype[key]) == g:FUNCREF_TYPE
          if key[0] == '_' && key[1] != '_'
            " Private methods are not copied
          else
            " Public methods are copied so that they call the parent's copy
            let l:type = a:prototype._type
            let l:scope = 's:'
            if exists('s:' . l:type)
              let l:scope = 's:'
            elseif exists('g:' . l:type)
              let l:scope = 'g:'
            elseif exists('w:' . l:type)
              let l:scope = 'w:'
            elseif exists('t:' . l:type)
              let l:scope = 't:'
            elseif exists('b:' . l:type)
              let l:scope = 'b:'
            else
              let l:scope = ''
            endif

            let l:fd = "function! l:o." . key . "(...) dict\n"

            if l:scope != ''
              let l:fd = l:fd . "return call(" . l:scope . type . "." . key . ", a:000, self)\n"
            else
              let l:fd = l:fd . "return call(self._prototype." . key . ", a:000, self)\n"
            endif

            let l:fd = l:fd . "endfunction"
            execute l:fd
          endif
        else
          " Data
          if key[0] != '_'
            " Public data
            let l:o[key] = a:prototype[key]
          elseif len(key) > 1 && key[0] == '_' && key[1] == '_'
            " Protected data
            let l:o[key] = deepcopy(a:prototype[key])
          endif
        endif
      endfor
      return l:o
    endfunction

    function g:ObjectPrototype._delete(prototype) dict
      let l:i = stridx(a:prototype._type, "Prototype")   
      if l:i != -1
        throw "Can not delete a prototype: " . a:prototype.getType()
      endif
      for key in keys(a:prototype)
        unlet a:prototype[key]
      endfor
    endfunction

  endif
  return g:ObjectPrototype
endfunction



" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Info
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function! s:loadInfoPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:InfoPrototype")
      unlet s:InfoPrototype
    endif
  endif
  if !exists("s:InfoPrototype")
    let s:InfoPrototype = g:loadObjectPrototype().create()
    let s:InfoPrototype._type = 'InfoPrototype'
    let s:InfoPrototype.__ctype = s:UNKNOWN_TYPE
    let s:InfoPrototype.__descriptionSpace = 0
    let s:InfoPrototype.__indent = ''
    let s:InfoPrototype.__tagsSet = {} " TagsPrototype
    let s:InfoPrototype.__annotationCount = 0
    let s:InfoPrototype.__singleLineCommentPos = -1
    let s:InfoPrototype.__firstLineText = ''
    let s:InfoPrototype.__docCommentEnd = -1
    let s:InfoPrototype.__docCommentStart = -1

    function s:InfoPrototype.getCtype() dict
      return self.__ctype
    endfunction

    function s:InfoPrototype.getDescriptionSpace() dict
      return self.__descriptionSpace
    endfunction
    function s:InfoPrototype.setDescriptionSpace(descriptionSpace) dict
      let self.__descriptionSpace = a:descriptionSpace
    endfunction

    function s:InfoPrototype.getIndent() dict
      return self.__indent
    endfunction
    function s:InfoPrototype.setIndent(indent) dict
      let self.__indent = a:indent
    endfunction

    function s:InfoPrototype.getTagsSet() dict
      return self.__tagsSet
    endfunction

    function s:InfoPrototype.getAnnotationCount() dict
      return self.__annotationCount
    endfunction
    function s:InfoPrototype.setAnnotationCount(cnt) dict
      let self.__annotationCount = a:cnt
    endfunction

    function s:InfoPrototype.getSingleLineCommentPos() dict
      return self.__singleLineCommentPos
    endfunction
    function s:InfoPrototype.setSingleLineCommentPos(pos) dict
      let self.__singleLineCommentPos = a:pos
    endfunction

    function s:InfoPrototype.getFirstLineText() dict
      return self.__firstLineText
    endfunction
    function s:InfoPrototype.setFirstLineText(line) dict
      let self.__firstLineText = a:line
    endfunction

    function s:InfoPrototype.getDocCommentEnd() dict
      return self.__docCommentEnd
    endfunction
    function s:InfoPrototype.setDocCommentEnd(pos) dict
      let self.__docCommentEnd = a:pos
    endfunction

    function s:InfoPrototype.getDocCommentStart() dict
      return self.__docCommentStart
    endfunction
    function s:InfoPrototype.setDocCommentStart(pos) dict
      let self.__docCommentStart = a:pos
    endfunction

  endif
  return s:InfoPrototype
endfunction
function! s:newInfo(ctype)
  let l:o = s:loadInfoPrototype().create()
  let l:o.__ctype = a:ctype
  let l:o.__descriptionSpace = g:descriptionSpaceList[a:ctype]
  let l:o.__indent = s:GetIndentation(s:combinedString)
  let l:o.__tagsSet = s:newTagsSet()

  return l:o
endfunction

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" TagsSet
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

" --------------------------------------------------------
" loadTagsSetPrototype
"   Set of tags container
" --------------------------------------------------------
function! s:loadTagsSetPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:TagsSetPrototype")
      unlet s:TagsSetPrototype
    endif
  endif
  if !exists("s:TagsSetPrototype")
    let s:TagsSetPrototype = g:loadObjectPrototype().create()
    let s:TagsSetPrototype._type = 'TagsSetPrototype'
    let s:TagsSetPrototype.__tags = { }

    function! s:TagsSetPrototype.getTags() dict
      return self.__tags
    endfunction

    function! s:TagsSetPrototype.toList(key) dict
      if has_key(self.__tags, a:key)
        return self.__tags[a:key].toList()
      else
        return []
      endif
    endfunction

    function! s:TagsSetPrototype.hasKey(key) dict
      return has_key(self.__tags, a:key)
    endfunction

    function! s:TagsSetPrototype.removeTag(key) dict
      if has_key(self.__tags, a:key)
        call remove(self.__tags, a:key)
      endif
    endfunction

    function! s:TagsSetPrototype.replaceTag(tag) dict
      let name = a:tag.getName()
      if has_key(self.__tags, name)
        call remove(self.__tags, name)
      endif
      let self.__tags[name] = a:tag
    endfunction

    function! s:TagsSetPrototype.getTag(key) dict
      if has_key(self.__tags, a:key)
        return self.__tags[a:key]
      else
        throw 'TagsSetPrototype.getTag: no value for key: ' . a:key
      endif
    endfunction

    function! s:TagsSetPrototype.empty() dict
      return empty(self.__tags)
    endfunction

    function! s:TagsSetPrototype.maxTagLen() dict
      let max = 0
      for tag in keys(self.__tags)
        let v = len(tag)
        if v > max
          let max = v
        endif
      endfor
      return max
    endfunction

    function! s:TagsSetPrototype.appendList(tags) dict
      for tag in a:tags
        call self.append(tag)
      endfor
    endfunction

    function! s:TagsSetPrototype.append(tag) dict
      let name = a:tag.getName()
      if has_key(self.__tags, name)
        let oldtag = self.__tags[name]
        let newtag = oldtag.merge(a:tag)
        let self.__tags[name] = newtag
      else 
        let self.__tags[name] = a:tag
      endif
    endfunction

    " TODO remove
    function! s:TagsSetPrototype.write() dict
      for tag in values(self.__tags)
        call tag.write()
      endfor
    endfunction
  endif
  return s:TagsSetPrototype
endfunction
function! s:newTagsSet()
  return s:loadTagsSetPrototype().create()
endfunction

function! s:TestTagListToString(tagList)
  let str = ''
  let len = len(a:tagList)
  if len > 0
    for tag in a:tagList
      let tagName = tag.getName()
      let str = str . ' ' . tagName . '('
      if tag.hasValue()
        let str = str . ' ' . tag.getValue()
      endif
      if tag.hasText()
        let str = str . ' ' . tag.getText()
      endif
      let str = str .  ')'
    endfor
  endif
  return str
endfunction



" ********************************************
" START Tag Prototypes
" ********************************************

" --------------------------------------------------------
" loadBaseTagPrototype
"   Create abstract base prototype for all tags
" --------------------------------------------------------
function! s:loadBaseTagPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:BaseTagrototype")
      unlet s:BaseTagPrototype
    endif
  endif
  if !exists("s:BaseTagPrototype")
    let s:BaseTagPrototype = g:loadObjectPrototype().create()
    let s:BaseTagPrototype._type = 'BaseTagPrototype'
    let s:BaseTagPrototype.__tagName = ''
    let s:BaseTagPrototype.__hasValue = s:IS_FALSE
    let s:BaseTagPrototype.__hasText = s:IS_FALSE

    function! s:BaseTagPrototype.initialize(tagName, hasValue, hasText) dict
      let self.__tagName = a:tagName
      let self.__hasValue = a:hasValue
      let self.__hasText = a:hasText
    endfunction

    function! s:BaseTagPrototype.isLeafTag() dict
      throw "Must define in child: isLeafTag"
    endfunction

    function! s:BaseTagPrototype.toList() dict
      throw "Must define in child: toList"
    endfunction

    function! s:BaseTagPrototype.canBePoly() dict
      throw "Must define in child: canBePoly"
    endfunction

    function! s:BaseTagPrototype.merge(tag) dict
      let nameSelf = self.getName()
      let nameTag = a:tag.getName()
      if nameSelf != nameTag
        let msg = "Merging different tag types: @" . nameSelf . " and @" . nameTag
        throw msg
      endif
      if self.isLeafTag()
        if ! self.canBePoly()
          let msg = 'Can not have two tags with name "@' . nameSelf .'"'
          throw msg
        endif
        if a:tag.isLeafTag()
          let hasValue = self.hasValue()
          let hasText = self.hasText()
          let poly = s:newPolyTag(nameSelf, hasValue, hasText)
          call poly.add(self)
          call poly.add(a:tag)
          return poly

        else
          call a:tag.add(self)
          return a:tag

        endif

      elseif a:tag.isLeafTag()
        call self.add(a:tag)
        return self

      else
        " both are polyTags
        let tags = a:tag.getTags()
        for otag in tags
          call self.add(otag)
        endfor
        return self

      endif
    endfunction

    function! s:BaseTagPrototype.getName() dict
      return self.__tagName
    endfunction

    " Value: Generally, a single word but sometimes (like @see)
    " it is text representing a single entity with spaces.
    " At any rate, it is expect not to line wrap.
    function! s:BaseTagPrototype.hasValue() dict
      return self.__hasValue
    endfunction

    function! s:BaseTagPrototype.getValue() dict
      throw "Must define in child: getValue"
    endfunction

    " If a tag has both a value and text, then extendText should be called
    " rather than extendValue.
    function! s:BaseTagPrototype.extendValue(value) dict
      throw "Must define in child: extendValue"
    endfunction

    " Text: Free text, white space, multi-line
    function! s:BaseTagPrototype.hasText() dict
      return self.__hasText
    endfunction

    function! s:BaseTagPrototype.getText() dict
      throw "Must define in child: getText"
    endfunction

    function! s:BaseTagPrototype.setText(text) dict
      throw "Must define in child: setText"
    endfunction

    function! s:BaseTagPrototype.extendText(text) dict
      throw "Must define in child: extendText"
    endfunction

    function! s:BaseTagPrototype.empty() dict
      throw "Must define in child: empty"
    endfunction

    function! s:BaseTagPrototype.write() dict
      throw "Must define in child: write"
    endfunction

    function! s:BaseTagPrototype.toString() dict
      if self.isLeafTag()
        let str = self.getName() . '[('
        if self.hasValue()
          let str = str . ':' . self.getValue()
        endif
        if self.hasText()
          let str = str . ':' . self.getText()
        endif
        let str = str . ')]'
        return str

      else
        let list = self.toList()
        let str = self.getName() . '['
        for tag in list
          let str = str . '('
          if tag.hasValue()
            let str = str . ':' . tag.getValue()
          endif
          if tag.hasText()
            let str = str . ':' . tag.getText()
          endif
          let str = str . ')'
        endfor
        let str = str . ']'
        return str

      endif
    endfunction

  endif
  return s:BaseTagPrototype
endfunction

" --------------------------------------------------------
" loadLeafTagPrototype
"   Create leaf tag prototype parameterized by if it can 
"     have a value, if it can have text and if there can
"     be more than one of them.
" --------------------------------------------------------
function! s:loadLeafTagPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:LeafTagPrototype")
      unlet s:LeafTagPrototype
    endif
  endif
  if !exists("s:LeafTagPrototype")
    let s:LeafTagPrototype = s:loadBaseTagPrototype().create()
    let s:LeafTagPrototype._type = 'LeafTagPrototype'
    let s:LeafTagPrototype.__canBePoly = s:IS_FALSE
    let s:LeafTagPrototype.__tagValue = ''
    let s:LeafTagPrototype.__tagText = ''

    function! s:LeafTagPrototype.initialize(tagName, hasValue, hasText, canBePoly, tagValue, tagText) dict

      let args = [a:tagName, a:hasValue, a:hasText]
      call call(s:BaseTagPrototype.initialize, args, self)

      let self.__canBePoly = a:canBePoly
      let self.__tagValue = a:tagValue
      let self.__tagText = a:tagText
    endfunction

    function! s:LeafTagPrototype.isLeafTag() dict
      return s:IS_TRUE
    endfunction

    function! s:LeafTagPrototype.toList() dict
      return [self]  
    endfunction

    function! s:LeafTagPrototype.canBePoly() dict
      return self.__canBePoly
    endfunction

    function! s:LeafTagPrototype.getValue() dict
      return self.__tagValue
    endfunction

    function! s:LeafTagPrototype.extendValue(value) dict
      if self.hasValue()
        if self.__tagValue == ''
          self.__tagValue = a:value
        else
          self.__tagValue = self.__tagValue . ' ' . a:value
        endif
      else
        throw "Tag " . self.getName() . " does not support value"
      endif
    endfunction

    function! s:LeafTagPrototype.getText() dict
      return self.__tagText
    endfunction

    function! s:LeafTagPrototype.setText(text) dict
      let self.__tagText = a:text
    endfunction

    function! s:LeafTagPrototype.extendText(text) dict
      if self.hasText()
        if self.__tagText == ''
          let self.__tagText = a:text
        else
          let self.__tagText = self.__tagText . ' ' . a:text
        endif
      else
        throw "Tag " . self.getName() . " does not support text"
      endif
    endfunction

    function! s:LeafTagPrototype.empty() dict
      return s:IS_FALSE
    endfunction

    function! s:LeafTagPrototype.write() dict
      let str = ' * ' . self.getName()
      if self.hasValue()
        let str = str . ' ' . self.getValue()
      endif
      if self.hasText()
        let str = str . ' ' . self.getText()
      endif
      call s:AppendStr(str)
    endfunction
  endif
  return s:LeafTagPrototype
endfunction
function! s:newLeafTag(tagName, hasValue, hasText,  canBePoly, tagValue, tagText)
  let l:o = s:loadLeafTagPrototype().create()
  call l:o.initialize(a:tagName, a:hasValue, a:hasText,  a:canBePoly, a:tagValue, a:tagText)
  return l:o
endfunction


" --------------------------------------------------------
" loadPolyTagPrototype
"   Create poly tag prototype which has a list of one or
"     more leaf tag objects. Insertion order is preserved.
" --------------------------------------------------------
function! s:loadPolyTagPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:PolyTagPrototype")
      unlet s:PolyTagPrototype
    endif
  endif
  if !exists("s:PolyTagPrototype")
    let s:PolyTagPrototype = s:loadBaseTagPrototype().create()
    let s:PolyTagPrototype._type = 'PolyTagPrototype'
    let s:PolyTagPrototype.__list = []

    function! s:PolyTagPrototype.initialize(tagName, hasValue, hasText) dict
      let args = [a:tagName, a:hasValue, a:hasText]
      call call(s:BaseTagPrototype.initialize, args, self)
    endfunction

    function! s:PolyTagPrototype.canBePoly() dict
      return s:IS_FALSE
    endfunction

    function! s:PolyTagPrototype.getTags() dict
      return self.__list
    endfunction

    function! s:PolyTagPrototype.isLeafTag() dict
      return s:IS_FALSE
    endfunction

    function! s:PolyTagPrototype.toList() dict
      return self.__list
    endfunction

    function! s:PolyTagPrototype.empty() dict
      return empty(self.__list)
    endfunction

    function! s:PolyTagPrototype.add(tag) dict
      call add(self.__list, a:tag)
    endfunction

    function! s:PolyTagPrototype.write() dict
      for tag in self.__list
        call tag.write()
      endfor
    endfunction
  endif
  return s:PolyTagPrototype
endfunction
function! s:newPolyTag(tagName, hasValue, hasText)
  let l:o = s:loadPolyTagPrototype().create()
  call l:o.initialize(a:tagName, a:hasValue, a:hasText)
  return l:o
endfunction



" --------------------------------------------------------
" newAuthorTag
"   Constructor for an author leaf object.
"     @author value
"     tagName:    author
"     hasValue:   true
"     hasText:    false
"     canBePoly:  true
"     tagValue:   arg
"     tagText:    ''
" --------------------------------------------------------
function! s:newAuthorTag(tagValue)
  return s:newLeafTag('author', s:IS_TRUE, s:IS_FALSE, s:IS_TRUE, a:tagValue, '')
endfunction

" --------------------------------------------------------
" newVersionTag
"   Constructor for a version leaf object.
"     @version value
"     tagName:    version
"     hasValue:   true
"     hasText:    false
"     canBePoly:  false
"     tagValue:   arg
"     tagText:    ''
" --------------------------------------------------------
function! s:newVersionTag(tagValue)
  return s:newLeafTag('version', s:IS_TRUE, s:IS_FALSE, s:IS_FALSE, a:tagValue, '')
endfunction

" --------------------------------------------------------
" newParamTag
"   Constructor for a param leaf object.
"     @param value text
"     tagName:    param
"     hasValue:   true
"     hasText:    true
"     canBePoly:  true
"     tagValue:   arg
"     tagText:    arg
" --------------------------------------------------------
function! s:newParamTag(tagValue, tagText)
  return s:newLeafTag('param', s:IS_TRUE, s:IS_TRUE, s:IS_TRUE, a:tagValue, a:tagText)
endfunction

" --------------------------------------------------------
" newTParamTag
"   Constructor for a tparam leaf object.
"     @tparam value text
"     tagName:    tparam
"     hasValue:   true
"     hasText:    true
"     canBePoly:  true
"     tagValue:   arg
"     tagText:    arg
" --------------------------------------------------------
function! s:newTParamTag(tagValue, tagText)
  return s:newLeafTag('tparam', s:IS_TRUE, s:IS_TRUE, s:IS_TRUE, a:tagValue, a:tagText)
endfunction

" --------------------------------------------------------
" newReturnTag
"   Constructor for a return leaf object.
"     @return text
"     tagName:    return
"     hasValue:   false
"     hasText:    true
"     canBePoly:  false
"     tagValue:   ''
"     tagText:    arg
" --------------------------------------------------------
function! s:newReturnTag(tagText)
  return s:newLeafTag('return', s:IS_FALSE, s:IS_TRUE, s:IS_FALSE, '', a:tagText)
endfunction

" --------------------------------------------------------
" newThrowsTag
"   Constructor for a throws leaf object.
"     @throws value text
"     tagName:    throws
"     hasValue:   true
"     hasText:    true
"     canBePoly:  true
"     tagValue:   arg
"     tagText:    arg
" --------------------------------------------------------
function! s:newThrowsTag(tagValue, tagText)
  return s:newLeafTag('throws', s:IS_TRUE, s:IS_TRUE, s:IS_TRUE, a:tagValue, a:tagText)
endfunction


" --------------------------------------------------------
" newSeeTag
"   Constructor for a see leaf object.
"     @see value 
"     tagName:    see
"     hasValue:   true
"     hasText:    true
"     canBePoly:  true
"     tagValue:   arg
"     tagText:    arg
" --------------------------------------------------------
function! s:newSeeTag(tagValue, tagText)
  return s:newLeafTag('see', s:IS_TRUE, s:IS_TRUE, s:IS_TRUE, a:tagValue, a:tagText)
endfunction

" --------------------------------------------------------
" newSinceTag
"   Constructor for a since leaf object.
"     @since value 
"     tagName:    since
"     hasValue:   true
"     hasText:    false
"     canBePoly:  false
"     tagValue:   arg
"     tagText:    ''
" --------------------------------------------------------
function! s:newSinceTag(tagValue)
  return s:newLeafTag('since', s:IS_TRUE, s:IS_FALSE, s:IS_FALSE, a:tagValue, '')
endfunction

" --------------------------------------------------------
" newSerialTag
"   Constructor for a serail leaf object.
"     @serial value  (field-description|include|exclude)? 
"     tagName:    serial
"     hasValue:   false
"     hasText:    true
"     canBePoly:  false
"     tagValue:   ''
"     tagText:    arg
" --------------------------------------------------------
function! s:newSerialTag(tagText)
  return s:newLeafTag('serial', s:IS_FALSE, s:IS_TRUE, s:IS_FALSE, '', a:tagText)
endfunction

" --------------------------------------------------------
" newSerialFieldTag
"   Constructor for a serailField leaf object.
"     @serialField value text ( field-name field-type field-description?)
"     tagName:    serialField
"     hasValue:   true
"     hasText:    true
"     canBePoly:  false
"     tagValue:   arg
"     tagText:    arg
" --------------------------------------------------------
function! s:newSerialFieldTag(tagValue, tagText)
  return s:newLeafTag('serialField', s:IS_TRUE, s:IS_TRUE, s:IS_FALSE, a:tagValue, a:tagText)
endfunction

" --------------------------------------------------------
" newSerialDataTag
"   Constructor for a serailData leaf object.
"     In Java this is ONLY used when documenting a writeObject method
"     @serialData text (data-description?)
"     tagName:    serialData
"     hasValue:   false
"     hasText:    true
"     canBePoly:  false
"     tagValue:   ''
"     tagText:    arg
" --------------------------------------------------------
function! s:newSerialDataTag(tagValue, tagText)
  return s:newLeafTag('serialData', s:IS_FALSE, s:IS_TRUE, s:IS_FALSE, '', a:tagText)
endfunction

" --------------------------------------------------------
" newDeprecatedTag
"   Constructor for a deprecated leaf object.
"     @deprecated text 
"     tagName:    deprecated
"     hasValue:   false
"     hasText:    true
"     canBePoly:  false
"     tagValue:   ''
"     tagText:    arg
" --------------------------------------------------------
function! s:newDeprecatedTag(tagText)
  return s:newLeafTag('deprecated', s:IS_FALSE, s:IS_TRUE, s:IS_FALSE, '', a:tagText)
endfunction

" --------------------------------------------------------
" newUnknownTag
"   Constructor for an unknown leaf object.
"     @tagName text - unknown tag
"     tagName:    arg
"     hasValue:   false
"     hasText:    true
"     canBePoly:  true
"     tagValue:   ''
"     tagText:    arg
" --------------------------------------------------------
function! s:newUnknownTag(tagName, tagValue, tagText)
  let text = a:tagText
  if a:tagValue != ''
    if text == ''
      let text = a:tagValue
    else
      let text = a:tagValue . ' ' . text
    endif
  endif
  return s:newLeafTag(a:tagName, s:IS_FALSE, s:IS_TRUE, s:IS_TRUE, '', text)
endfunction


" user tags: tag value? text? formatting info
" Scala user tags
"  @note text
"  @getter
"  @pre text
"  @post text
"  @requires value text
"  @provides text
"  @contributor value


" --------------------------------------------------------
" newUserTag
"   Constructor for an user leaf object.
"     @tag value? text?
"     @tagName value? text? - user tag
"     tagName:    arg
"     hasValue:   arg
"     hasText:    arg
"     canBePoly:  arg
"     tagValue:   arg
"     tagText:    arg
" --------------------------------------------------------
function! s:newUserTag(tagName, hasValue, hasText,  canBePoly, tagValue, tagText)
  let value = a:tagValue
  let text = a:tagText

  if a:hasValue
    if ! a:hasText && text != ''
      let value = value . ' ' . text
      let text = ''
    endif
  elseif a:hasText
    if text == ''
      let text = value
    else
      let text = value . ' ' . text
    endif
    let value = ''
  endif

  return s:newLeafTag(a:tagName, a:hasValue, a:hasText,  a:canBePoly, value, text)
endfunction

" --------------------------------------------------------
" CreateTag
"   Create a tag given its name and optional value ant text
" --------------------------------------------------------
function! s:CreateTag(tagName, tagValue, tagText) 
  let l:tagName = a:tagName
  let l:tagValue = a:tagValue
  let l:tagText = a:tagText

  if l:tagName == 'author'
    if l:tagText == ''
      return s:newAuthorTag(l:tagValue)
    else
      return s:newAuthorTag(l:tagValue . ' ' . l:tagText)
    endif

  elseif l:tagName == 'version'
    if l:tagText == ''
      return s:newVersionTag(l:tagValue)
    else
      return s:newVersionTag(l:tagValue . ' ' . l:tagText)
    endif

  elseif l:tagName == 'param'
    return s:newParamTag(l:tagValue, l:tagText)

  elseif l:tagName == 'tparam'
    return s:newTParamTag(l:tagValue, l:tagText)

  elseif l:tagName == 'return'
    if l:tagText == ''
      return s:newReturnTag(l:tagValue)
    else
      return s:newReturnTag(l:tagValue . ' ' . l:tagText)
    endif

  elseif l:tagName == 'throws'
    return s:newThrowsTag(l:tagValue, l:tagText)

  elseif l:tagName == 'see'
    return s:newSeeTag(l:tagValue, l:tagText)

  elseif l:tagName == 'since'
    if l:tagText == ''
      return s:newSinceTag(l:tagValue)
    else
      return s:newSinceTag(l:tagValue . ' ' . l:tagText)
    endif

  elseif l:tagName == 'serial'
    if l:tagText == ''
      return s:newSerialTag(s:Trim(l:tagValue))
    else
      return s:newSerialTag(s:Trim(l:tagValue . ' ' . l:tagText))
    endif

  elseif l:tagName == 'serialField'
    return s:newSerialFieldTag(l:tagValue, l:tagText)

  elseif l:tagName == 'serialData'
    return s:newSerialDataTag(l:tagValue, l:tagText)

  elseif l:tagName == 'deprecated'
    if l:tagText == ''
      return s:newDeprecatedTag(l:tagValue)
    else
      return s:newDeprecatedTag(l:tagValue . ' ' . l:tagText)
    endif

  else
    return s:CreateUserOrUnknownTag(l:tagName, l:tagValue, l:tagText)
  endif
endfunction

" --------------------------------------------------------
" CreateUserOrUnknownTag
"   Create a user or unknown tag. If the tagName matches
"     a user defined tag (see userTags List), a user
"     tag is created otherwise an unknown tag is created.
" --------------------------------------------------------
function! s:CreateUserOrUnknownTag(tagName, tagValue, tagText) 
  " list of user third-party tags
  "    tagName
  "    hasValue
  "    hasText 
  "    canHaveMoreThanOne
  for definition in s:userTags
    let tagName = definition[0]
    if tagName == a:tagName
      let hasValue = definition[1]
      let hasText = definition[2]
      let canBePoly = definition[3]
      return s:newUserTag(tagName, hasValue, hasText, canBePoly, a:tagValue, a:tagText)
    endif
  endfor

  call s:WarningMessage(" unknown or user tag: " . a:tagName)

  return s:newUnknownTag(a:tagName, a:tagValue, a:tagText)

endfunction

" ********************************************
" END Tag Prototypes
" ********************************************



" ********************************************
" START Entity Prototypes
" ********************************************

" Entity Interface Prototype
function! s:loadAbstractEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:AbstractEntityPrototype")
      unlet s:AbstractEntityPrototype
    endif
  endif
  if !exists("s:AbstractEntityPrototype")
    let s:AbstractEntityPrototype = g:loadObjectPrototype().create()
    let s:AbstractEntityPrototype._type = 'AbstractEntityPrototype'
    let s:AbstractEntityPrototype.__standardTags = []
    let s:AbstractEntityPrototype.__name = ''

    function s:AbstractEntityPrototype.getName() dict
      return self.__name
    endfunction
    function s:AbstractEntityPrototype.setName(name) dict
      let self.__name = a:name
    endfunction

    function s:AbstractEntityPrototype.getClassType() dict
      throw "Must define in child: getClassType"
    endfunction

    function s:AbstractEntityPrototype.resolveTags(info) dict
      throw "Must define in child: resolveTags"
    endfunction

    function s:AbstractEntityPrototype.getStandardTags() dict
      return self.__standardTags
    endfunction

    function s:AbstractEntityPrototype.extractTags(info) dict
      throw "Must define in child: extractTags"
    endfunction

    "''''''''''''''''''
    " Extract TParams
    "''''''''''''''''''
    function! s:AbstractEntityPrototype.__processTParams(info, str, startPos) dict
      let l:argDict = self.__makeTParamDict(a:str, a:startPos)

      call self.__scanTParam(l:argDict)

      let tagsSet = a:info.getTagsSet()
      for tparam in l:argDict.tparams
        call tagsSet.append(tparam)
      endfor
      return l:argDict.endPos
    endfunction

    function! s:AbstractEntityPrototype.__makeTParamDict(str, startPos) dict
      let argDict = {}
      let argDict.str = a:str
      let argDict.atTop = s:IS_TRUE
      let argDict.topOnly = b:scommenter_top_level_tparams_only 
      let argDict.tparams = []
      let argDict.startPos = a:startPos
      let argDict.endPos = -1
      return argDict
    endfunction

    function! s:AbstractEntityPrototype.__scanTParam(argDict) dict
      let argDict = a:argDict

      let str = argDict.str
      let atTop = argDict.atTop
      let currentPos = argDict.startPos
      let len = strlen(str)
      if argDict.topOnly == s:IS_TRUE
        let addParam = atTop
      else
        let addParam = s:IS_TRUE
      endif

      let c = strpart(str, currentPos, 1)
      if c == '[' && currentPos < len
        let currentPos = currentPos + 1
        let c = strpart(str, currentPos, 1)
        let startTParam = currentPos
        let textInfo = ''

        while c != ']' && currentPos < len 
            if c == '['
              let argDict.atTop = s:IS_FALSE

              " TODO: is this needed
              let argDict.startPos = currentPos

              call self.__scanTParam(argDict)

              let argDict.atTop = atTop
              let currentPos = argDict.endPos

            elseif c == '@'
              let tmpstr = strpart(str, currentPos+1)
              let idx = stridx(tmpstr, "specialized", 0)
              if idx == 0
                let textInfo = '@specialized'
                let currentPos = currentPos + 12
                let c = strpart(str, currentPos, 1)
                if c == '('
                  let l:argDictInner = self.__makeParamDict(str, currentPos)
                  call self.__scanParam(l:argDictInner)
                  let paranEnd = l:argDictInner.endPos + 1
                  let tt = strpart(str, currentPos, paranEnd - currentPos)
                  let textInfo = textInfo . tt
                  let currentPos = paranEnd + 1
                  let startTParam = currentPos
                else
                  let startTParam = currentPos + 1
                endif
              endif

            elseif c == ',' && addParam == s:IS_TRUE
              let tparamStr = s:Trim(strpart(str, startTParam, currentPos - startTParam))
              let c = strpart(tparamStr, 0, 1)
              if c != '['
                " TODO: not correct
                " regular expression for all template parameter 
                let tparamName = substitute(tparamStr, '^[+\|-]\?\([^\s\|<\|>]\+\).*', '\1', '')
                let tparamName = s:Trim(tparamName)
                let tparam = s:newTParamTag(tparamName, textInfo)
                let textInfo = ''
                call add(argDict.tparams, tparam)
              endif
              let startTParam = currentPos + 1
            endif
            let currentPos = currentPos + 1
            let c = strpart(str, currentPos, 1)
        endwhile

        let tparamStr = s:Trim(strpart(str, startTParam, currentPos - startTParam))
        let c = strpart(tparamStr, 0, 1)
        if c != '[' && addParam == s:IS_TRUE
          let tparamName = substitute(tparamStr, '^[+\|-]\?\([^<\|>]\+\).*', '\1', '')
          let tparamName = s:Trim(tparamName)
          if tparamName != ''
             let tparam = s:newTParamTag(tparamName, textInfo)
             call add(argDict.tparams, tparam)
          endif
        endif

      elseif c == ']'
        let currentPos = currentPos + 1
      endif

      let argDict.endPos = currentPos
    endfunction


    "''''''''''''''''''
    " Extract Params
    "''''''''''''''''''
    function! s:AbstractEntityPrototype.__processParams(info, str, startPos) dict
      let l:argDict = self.__makeParamDict(a:str, a:startPos)
      call self.__scanParam(l:argDict)

      let tagsSet = a:info.getTagsSet()
      for param in l:argDict.params
        call tagsSet.append(param)
      endfor
      return l:argDict.endPos
    endfunction

    function! s:AbstractEntityPrototype.__makeParamDict(str, startPos) dict
      let argDict = {}
      let argDict.str = a:str
      let argDict.atTop = s:IS_TRUE
      let argDict.params = []
      let argDict.startPos = a:startPos
      let argDict.endPos = -1
      return argDict
    endfunction

    function! s:AbstractEntityPrototype.__scanParam(argDict) dict
      let argDict = a:argDict
      let str = argDict.str
      let atTop = argDict.atTop
      let currentPos = argDict.startPos
      let len = strlen(str)
      let bracketDepth = 0

      let c = strpart(str, currentPos, 1)
      if c == '(' && currentPos < len
        let currentPos = currentPos + 1
        let c = strpart(str, currentPos, 1)
        let startParam = currentPos

        while c != ')' && currentPos < len 
            if c == '('
              let argDict.atTop = s:IS_FALSE
              let argDict.startPos = currentPos
    
              call self.__scanParam(argDict)

              let argDict.atTop = atTop
              let currentPos = argDict.endPos

            elseif c == ',' && atTop == s:IS_TRUE && bracketDepth == 0
              let pstr = s:Trim(strpart(str, startParam, currentPos - startParam))
              let paramInfo = substitute(pstr, '\([^:]*\).*', '\1', '')
              let paramName = substitute(paramInfo, '\(\S\+\s\+\)*\s*\(\S\+\)\s*', '\2', '')
              let paramType = substitute(pstr, '[^:]*:\(.*\)', '\1', '')
              let param = s:newParamTag(paramName, s:Trim(paramType))
              call add(argDict.params, param)

              let startParam = currentPos + 1
            elseif c == '[' 
              let bracketDepth = bracketDepth + 1
            elseif c == ']' 
              let bracketDepth = bracketDepth - 1
            endif
            let currentPos = currentPos + 1
            let c = strpart(str, currentPos, 1)
        endwhile

        if atTop == s:IS_TRUE
          let pstr = s:Trim(strpart(str, startParam, currentPos - startParam))
          if pstr != ''
            let paramInfo = substitute(pstr, '\([^:]*\).*', '\1', '')
            let paramName = substitute(paramInfo, '\(\S\+\s\+\)*\s*\(\S\+\)\s*', '\2', '')
            let paramType = substitute(pstr, '[^:]*:\(.*\)', '\1', '')
            let param = s:newParamTag(paramName, s:Trim(paramType))
            call add(argDict.params, param)
          endif
        endif

      elseif c == ')'
        let currentPos = currentPos + 1
      endif

      let argDict.endPos = currentPos
    endfunction

    " all tags in the two lists are the same type (same tag name)
    function! s:AbstractEntityPrototype.__resolveNameTextDictList(newTagList, oldTagList, deletedTagsWrapper)
      let l:newTagList = a:newTagList
      let l:oldTagList = a:oldTagList

      if empty(l:oldTagList)
        " no old params, so use all the new ones
        return l:newTagList

      elseif empty(l:newTagList)
        " no new params, so clear old ones
        return l:newTagList

      " elseif len(l:newTagList) == len(l:oldTagList)
      else
        " maybe all names are the same, nothing to do
        " else some of the new param names are not in the old param list
        
        " Note: There is at least one entry in each list
        " Note: The lists contain the same TYPE of tags 
        "   (have the same tag name)
        " So, get the tag's characteristics
        let tag = l:newTagList[0]
        let hasValue = tag.hasValue()
        let hasText = tag.hasText()
        let canBePoly = tag.canBePoly()

        " If the tags have NO values, then there is NO way to
        " say if the tags are the same or not
        if hasValue
          let len = len(l:oldTagList)

          for newTag in l:newTagList
            let newTagValue = newTag.getValue()

            for oldTag in oldTagList
              let oldTagValue = oldTag.getValue()
              if oldTagValue == newTagValue
                call newTag.setText(oldTag.getText())
                break
              endif
            endfor
          endfor

          " ok, some names are different
          let deletedTags = []
          for oldTag in l:oldTagList
            let oldTagValue = oldTag.getValue()
            let found = s:IS_FALSE
            for newTag in l:newTagList
              let newTagValue = newTag.getValue()
              if oldTagValue == newTagValue
                let found = s:IS_TRUE
                break
              endif
            endfor
            if found == s:IS_FALSE
              call add(deletedTags, oldTag)
            endif
          endfor
          if ! empty(deletedTags)
            call add(a:deletedTagsWrapper, deletedTags)
          endif

          return copy(l:newTagList)

        elseif ! canBePoly
          let oldTag = l:oldTagList[0]
          let oldText = oldTag.getText()
          let newTag = l:newTagList[0]
          let newText = newTag.getText()
          if newText != oldText
            " if the old text is simpley a type name, e.g., Int, String, etc.
            " then use the new text value
            if oldText == 'Int'
              return copy(l:newTagList)
            elseif oldText == 'String'
              return copy(l:newTagList)
            elseif oldText == 'Double'
              return copy(l:newTagList)
            elseif oldText == 'Long'
              return copy(l:newTagList)
            elseif oldText == 'Float'
              return copy(l:newTagList)
            elseif oldText == 'Byte'
              return copy(l:newTagList)
            elseif oldText == 'Short'
              return copy(l:newTagList)
            endif

            " Can not enumerate all other possible types so we punt
            " keep the old text if it exists
            " do not call add(a:deletedTagsWrapper, [oldTag])
            return copy(l:oldTagList)
          endif
          return copy(l:newTagList)

        else
          " Does not have a value so there is no way to tell
          " if which tags to compare
          return copy(l:newTagList)
        endif
      endif

    endfunction

    function! s:AbstractEntityPrototype.__resolveSinceTag(info)
      let l:info = a:info
      let l:commentTagsSet = l:info.comment.tagsSet

      let key = 'since'
      if exists('b:scommenter_since_release')
        let sinceValue = b:scommenter_since_release
        if ! l:commentTagsSet.hasKey(key)
          let sinceTag = s:newSinceTag(sinceValue)
          call l:commentTagsSet.append(sinceTag)
        endif
      endif
    endfunction

    function! s:AbstractEntityPrototype.__resolveAuthorTag(info)
      let l:info = a:info
      let l:commentTagsSet = l:info.comment.tagsSet

      let key = 'author'
      if exists('b:scommenter_class_author')
        let authorValue = b:scommenter_class_author
        let found = s:IS_FALSE
        for authorTag in l:commentTagsSet.toList(key)
          if authorTag.getValue() == authorValue
            let found = s:IS_TRUE
            break
          endif
        endfor
        if found == s:IS_FALSE
          let authorTag = s:newAuthorTag(authorValue)
          call l:commentTagsSet.append(authorTag)
        endif
      endif
    endfunction

    function! s:AbstractEntityPrototype.__resolveVersionTag(info)
      let l:info = a:info
      let l:commentTagsSet = l:info.comment.tagsSet

      let key = 'version'
      if exists('b:scommenter_class_version')
        let versionValue = b:scommenter_class_version
        if ! l:commentTagsSet.hasKey(key)
          let versionTag = s:newVersionTag(versionValue)
          call l:commentTagsSet.append(versionTag)
        endif
      endif
    endfunction

    function! s:AbstractEntityPrototype.__resolveTag(info, key)
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      let key = a:key
      if entityTagsSet.hasKey(key)
        let deletedTagsWrapper = []
        let newTagList = entityTagsSet.toList(key)
        let oldTagList = commentTagsSet.toList(key)
        let tagList = self.__resolveNameTextDictList(newTagList, oldTagList, deletedTagsWrapper)
        call l:commentTagsSet.removeTag(key)
        call l:commentTagsSet.appendList(tagList)

        if ! empty(deletedTagsWrapper) 
          let deletedTagList = deletedTagsWrapper[0]
          call l:info.comment.tagsSet.deletedTagsSet.appendList(deletedTagList)
        endif
      else
        if l:commentTagsSet.hasKey(key)
          let warningTags = l:commentTagsSet.getTag(key)
          call l:info.comment.tagsSet.deletedTagsSet.append(warningTags)
        endif

        call l:commentTagsSet.removeTag(key)
      endif
    endfunction
  endif
  return s:AbstractEntityPrototype
endfunction

function! s:loadInnerClassEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:InnerClassEntityPrototype")
      unlet s:InnerClassEntityPrototype
    endif
  endif
  if !exists("s:InnerClassEntityPrototype")
    let s:InnerClassEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:InnerClassEntityPrototype._type = 'InnerClassEntityPrototype'

    function! s:InnerClassEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'param', 'tparam', 'throws', 'serial', 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:InnerClassEntityPrototype.getClassType() dict
      return s:INNER_CLASS_TYPE
    endfunction

    function! s:InnerClassEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      call self.__resolveSinceTag(l:info)

      let key = 'param'
      call self.__resolveTag(l:info, key)

      let key = 'tparam'
      call self.__resolveTag(l:info, key)

      let key = 'throws'
      call self.__resolveTag(l:info, key)

      let key = 'deprecated'
      call self.__resolveTag(l:info, key)

    endfunction

    function! s:InnerClassEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString

      let l:m = matchlist(str, '.*class\s*\(' . s:scalaname . '\)\s*\(.*\)')
      call l:info.entity.setName(l:m[1])

      let str = l:m[2]

      let nextChar = strpart(str, 0, 1)
      if nextChar == '['
        let endBracketType = self.__processTParams(l:info, str, 0)
        let str = strpart(str, endBracketType + 1)
        let str = substitute(str, '\s*\(.*\)', '\1', '')
      endif

      let nextChar = strpart(str, 0, 1)
      if nextChar == '('
        call self.__processParams(l:info, str, 0)
      endif

      " TODO REMOVE
      " let paramStart = stridx(str, '(')
      " if paramStart != -1
       "  call self.__processParams(l:info, str, 0)
      " endif

      if exists('b:scommenter_since_release')
        let sinceTag = s:newSinceTag(b:scommenter_since_release)
        call l:info.getTagsSet().append(sinceTag)
      endif
    endfunction

  endif
  return s:InnerClassEntityPrototype
endfunction
function! s:newInnerClassEntity()
  let l:o = s:loadInnerClassEntityPrototype().create()
  return l:o
endfunction



function! s:loadInnerTraitEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:InnerTraitEntityPrototype")
      unlet s:InnerTraitEntityPrototype
    endif
  endif
  if !exists("s:InnerTraitEntityPrototype")
    let s:InnerTraitEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:InnerTraitEntityPrototype._type = 'InnerTraitEntityPrototype'

    function! s:InnerTraitEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'tparam', 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:InnerTraitEntityPrototype.getClassType() dict
      return s:INNER_TRAIT_TYPE
    endfunction

    function! s:InnerTraitEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      call self.__resolveSinceTag(l:info)

      let key = 'tparam'
      call self.__resolveTag(l:info, key)

      let key = 'deprecated'
      call self.__resolveTag(l:info, key)

    endfunction

    function! s:InnerTraitEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString

      let l:m = matchlist(str, '.*trait\s*\(' . s:scalaname . '\)\s*\(.*\)')
      "let l:info.entity = l:m[1]
      call l:info.entity.setName(l:m[1])

      let str = l:m[2]

      let nextChar = strpart(str, 0, 1)
      if nextChar == '['
        let endBracketType = self.__processTParams(l:info, str, 0)
        let str = strpart(str, endBracketType + 1)
        let str = substitute(str, '\s*\(.*\)', '\1', '')
      endif

      if exists('b:scommenter_since_release')
        let sinceTag = s:newSinceTag(b:scommenter_since_release)
        call l:info.getTagsSet().append(sinceTag)
      endif
    endfunction

  endif
  return s:InnerTraitEntityPrototype
endfunction
function! s:newInnerTraitEntity()
  let l:o = s:loadInnerTraitEntityPrototype().create()
  return l:o
endfunction


function! s:loadInnerObjectEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:InnerObjectEntityPrototype")
      unlet s:InnerObjectEntityPrototype
    endif
  endif
  if !exists("s:InnerObjectEntityPrototype")
    let s:InnerObjectEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:InnerObjectEntityPrototype._type = 'InnerObjectEntityPrototype'

    function! s:InnerObjectEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:InnerObjectEntityPrototype.getClassType() dict
      return s:INNER_CLASS_TYPE
    endfunction

    function! s:InnerObjectEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      call self.__resolveSinceTag(l:info)

      let key = 'deprecated'
      call self.__resolveTag(l:info, key)

    endfunction

    function! s:InnerObjectEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString

      let l:m = matchlist(str, '.*object\s*\(' . s:scalaname . '\)\s*\(.*\)')
      "let l:info.entity = l:m[1]
      call l:info.entity.setName(l:m[1])

      if exists('b:scommenter_since_release')
        let sinceTag = s:newSinceTag(b:scommenter_since_release)
        call l:info.getTagsSet().append(sinceTag)
      endif
    endfunction

  endif
  return s:InnerObjectEntityPrototype
endfunction
function! s:newInnerObjectEntity()
  let l:o = s:loadInnerObjectEntityPrototype().create()
  return l:o
endfunction


function! s:loadClassEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:ClassEntityPrototype")
      unlet s:ClassEntityPrototype
    endif
  endif
  if !exists("s:ClassEntityPrototype")
    let s:ClassEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:ClassEntityPrototype._type = 'ClassEntityPrototype'

    function! s:ClassEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'author', 'version', 'param', 'tparam', 'throws', 'since', 'serial', 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:ClassEntityPrototype.getClassType() dict
      return s:CLASS_TYPE
    endfunction

    function! s:ClassEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      call self.__resolveAuthorTag(l:info)
      call self.__resolveVersionTag(l:info)
      call self.__resolveSinceTag(l:info)

      let key = 'param'
      call self.__resolveTag(l:info, key)

      let key = 'tparam'
      call self.__resolveTag(l:info, key)

      let key = 'throws'
      call self.__resolveTag(l:info, key)

      let key = 'deprecated'
      call self.__resolveTag(l:info, key)

    endfunction

    function! s:ClassEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString

      let l:m = matchlist(str, '.*class\s*\(' . s:scalaname . '\)\s*\(.*\)')
      call l:info.entity.setName(l:m[1])
      let str = l:m[2]

      if exists('b:scommenter_class_author')
        let authorTag = s:newAuthorTag(b:scommenter_class_author)
        call l:info.getTagsSet().append(authorTag)
      endif
      if exists('b:scommenter_class_version')
        let versionTag = s:newVersionTag(b:scommenter_class_version)
        call l:info.getTagsSet().append(versionTag)
      endif
      if exists('b:scommenter_since_release')
        let sinceTag = s:newSinceTag(b:scommenter_since_release)
        call l:info.getTagsSet().append(sinceTag)
      endif

      let nextChar = strpart(str, 0, 1)
      if nextChar == '['
        let endBracketType = self.__processTParams(l:info, str, 0)
        let str = strpart(str, endBracketType + 1)
        let str = substitute(str, '\s*\(.*\)', '\1', '')
      endif

      let nextChar = strpart(str, 0, 1)
      if nextChar == '('
        call self.__processParams(l:info, str, 0)
      endif
    endfunction

  endif
  return s:ClassEntityPrototype
endfunction
function! s:newClassEntity()
  let l:o = s:loadClassEntityPrototype().create()
  return l:o
endfunction


function! s:loadTraitEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:TraitEntityPrototype")
      unlet s:TraitEntityPrototype
    endif
  endif
  if !exists("s:TraitEntityPrototype")
    let s:TraitEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:TraitEntityPrototype._type = 'TraitEntityPrototype'

    function! s:TraitEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'author', 'version', 'tparam', 'since', 'see', 'deprecated' ]
      return l:o
    endfunction


    function! s:TraitEntityPrototype.getClassType() dict
      return s:TRAIT_TYPE
    endfunction

    function! s:TraitEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      call self.__resolveAuthorTag(l:info)
      call self.__resolveVersionTag(l:info)
      call self.__resolveSinceTag(l:info)

      let key = 'tparam'
      call self.__resolveTag(l:info, key)

      let key = 'deprecated'
      call self.__resolveTag(l:info, key)

    endfunction

    function! s:TraitEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString

      let l:m = matchlist(str, '.*trait\s*\(' . s:scalaname . '\)\s*\(.*\)')
      "let l:info.entity = l:m[1]
      call l:info.entity.setName(l:m[1])

      let str = l:m[2]

      if exists('b:scommenter_class_author')
        let authorTag = s:newAuthorTag(b:scommenter_class_author)
        call l:info.getTagsSet().append(authorTag)
      endif
      if exists('b:scommenter_class_version')
        let versionTag = s:newVersionTag(b:scommenter_class_version)
        call l:info.getTagsSet().append(versionTag)
      endif
      if exists('b:scommenter_since_release')
        let sinceTag = s:newSinceTag(b:scommenter_since_release)
        call l:info.getTagsSet().append(sinceTag)
      endif

      let nextChar = strpart(str, 0, 1)
      if nextChar == '['
        let endBracketType = self.__processTParams(l:info, str, 0)
        let str = strpart(str, endBracketType + 1)
        let str = substitute(str, '\s*\(.*\)', '\1', '')
      endif 

    endfunction

  endif
  return s:TraitEntityPrototype
endfunction
function! s:newTraitEntity()
  let l:o = s:loadTraitEntityPrototype().create()
  return l:o
endfunction


function! s:loadObjectEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:ObjectEntityPrototype")
      unlet s:ObjectEntityPrototype
    endif
  endif
  if !exists("s:ObjectEntityPrototype")
    let s:ObjectEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:ObjectEntityPrototype._type = 'ObjectEntityPrototype'

    function! s:ObjectEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'author', 'version', 'since', 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:ObjectEntityPrototype.getClassType() dict
      return s:OBJECT_TYPE
    endfunction

    function! s:ObjectEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let l:entityTagsSet = l:info.getTagsSet()
      let l:commentTagsSet = l:info.comment.tagsSet

      call self.__resolveAuthorTag(l:info)
      call self.__resolveVersionTag(l:info)
      call self.__resolveSinceTag(l:info)

      let key = 'deprecated'
      call self.__resolveTag(l:info, key)

    endfunction

    function! s:ObjectEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString

      let l:m = matchlist(str, '.*object\s*\(' . s:scalaname . '\)\s*\(.*\)')
      "let l:info.entity = l:m[1]
      call l:info.entity.setName(l:m[1])

      let str = l:m[2]

      if exists('b:scommenter_class_author')
        let authorTag = s:newAuthorTag(b:scommenter_class_author)
        call l:info.getTagsSet().append(authorTag)
      endif
      if exists('b:scommenter_class_version')
        let versionTag = s:newVersionTag(b:scommenter_class_version)
        call l:info.getTagsSet().append(versionTag)
      endif
      if exists('b:scommenter_since_release')
        let sinceTag = s:newSinceTag(b:scommenter_since_release)
        call l:info.getTagsSet().append(sinceTag)
      endif
    endfunction

  endif
  return s:ObjectEntityPrototype
endfunction
function! s:newObjectEntity()
  let l:o = s:loadObjectEntityPrototype().create()
  return l:o
endfunction

function! s:loadMethodEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:MethodEntityPrototype")
      unlet s:MethodEntityPrototype
    endif
  endif
  if !exists("s:MethodEntityPrototype")
    let s:MethodEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:MethodEntityPrototype._type = 'MethodEntityPrototype'

    function! s:MethodEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'param', 'tparam', 'return', 'throws', 'since', 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:MethodEntityPrototype.getClassType() dict
      return s:METHOD_TYPE
    endfunction

    function! s:MethodEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      let allowedTags = ['param', 'tparam', 'return', 'throws', 'deprecated']

      for key in allowedTags
        call self.__resolveTag(l:info, key)
      endfor
    endfunction

    function! s:MethodEntityPrototype.extractTags(info) dict
      let l:info = a:info
      let str = s:combinedString


      let indent = substitute(str, '^\(\s*\)\S.*', '\1', '')
      let len = strlen(indent)
      if len !=  0
        let str = strpart(str, len)
      endif

      let pre_def = substitute(str, '^\(\S.*\s*\)def\+.*', '\1', '')

      let len = strlen(pre_def)
      if len != strlen(str)
        let str = strpart(str, len)
      endif

      let str = strpart(str, 3)
      let str = substitute(str, '\s\+\(.*\)', '\1', '')
      let strlen = strlen(str)
      let index = 0
      let endStr = 0

      let prevC = ''
      while index < strlen
        let c = strpart(str, index, 1)
        if c == ' '
          break
        elseif c == '['
          break
        elseif c == '('
          break
        elseif c == ':'
          if index != 0
            if prevC != ':' && prevC != '_'
              break
            endif
          endif
        endif

        let prevC = c
        let endStr = index
        let index = index + 1
      endwhile
      let method_name = strpart(str, 0, endStr+1)

      call l:info.entity.setName(method_name)

      let len = strlen(method_name)
      let str = strpart(str, len)

      let str = substitute(str, '\s*\(.*\)', '\1', '')

      let len = strlen(str)
      if len != 0
        let nextChar = strpart(str, 0, 1)
        if nextChar == '['
          let endBracketType = self.__processTParams(l:info, str, 0)
          let str = strpart(str, endBracketType + 1)
          let str = substitute(str, '\s*\(.*\)', '\1', '')
        endif

        let nextChar = strpart(str, 0, 1)
        while nextChar == '('
          let endParanType = self.__processParams(l:info, str, 0)
          let str = strpart(str, endParanType + 1)
          let str = substitute(str, '\s*\(.*\)', '\1', '')
          let nextChar = strpart(str, 0, 1)
        endwhile

        if strpart(str, 0, 1) == ":"
          let str = s:Trim(strpart(str, 1))
          let equalsPos = stridx(str, '=')
          let returnName = ''
          if equalsPos != -1
            let returnName = s:Trim(strpart(str,0,equalsPos))
          else
            let returnName = s:Trim(strpart(str,0))
          endif
          if returnName != 'Unit'
            let returnTag = s:newReturnTag(returnName)
            let tags = l:info.getTagsSet()
            call tags.append(returnTag)
          endif 

        elseif strpart(str, 0, 1) == "="
          " implied return value, i.e.,  def getFoo = foo
          let mpos = match(method_name, s:getterName)
          if mpos == 0
            let rest = strpart(method_name, 3)
            if rest == ''
                let returnTag = s:newReturnTag('Unknown')
                let tags = l:info.getTagsSet()
                call tags.append(returnTag)
            else
                let returnTag = s:newReturnTag(rest)
                let tags = l:info.getTagsSet()
                call tags.append(returnTag)
            endif
          else
            " boolean return value, i.e.,  def isFoo = foo or def hasFoo = foo
            let found = s:IS_FALSE
            for bname in g:booleanNames
              let mpos = match(method_name, bname)
              if mpos == 0
                let returnTag = s:newReturnTag('Boolean ' . strpart(method_name, len(bname)))
                let tags = l:info.getTagsSet()
                call tags.append(returnTag)
                let found = s:IS_TRUE
                break
              endif
            endfor

            if found == s:IS_FALSE
              let returnTag = s:newReturnTag('Unknown')
              let tags = l:info.getTagsSet()
              call tags.append(returnTag)
            endif

          endif
        endif
      endif
    endfunction

  endif
  return s:MethodEntityPrototype
endfunction
function! s:newMethodEntity()
  let l:o = s:loadMethodEntityPrototype().create()
  return l:o
endfunction


function! s:loadFieldEntityPrototype()
  if s:IN_DEVELOPMENT_MODE
    if exists("s:FieldEntityPrototype")
      unlet s:FieldEntityPrototype
    endif
  endif
  if !exists("s:FieldEntityPrototype")
    let s:FieldEntityPrototype = s:loadAbstractEntityPrototype().create()
    let s:FieldEntityPrototype._type = 'FieldEntityPrototype'

    function! s:FieldEntityPrototype.create() dict
      let l:o = g:ObjectPrototype._create(self)
      let l:o.__standardTags = [ 'serial', 'serialField', 'since', 'see', 'deprecated' ]
      return l:o
    endfunction

    function! s:FieldEntityPrototype.getClassType() dict
      return s:FIELD_TYPE
    endfunction

    function! s:FieldEntityPrototype.resolveTags(info) dict
      let l:info = a:info
      " nothing to see, move on
    endfunction

    function! s:FieldEntityPrototype.extractTags(info) dict
    endfunction

  endif
  return s:FieldEntityPrototype
endfunction
function! s:newFieldEntity()
  let l:o = s:loadFieldEntityPrototype().create()
  return l:o
endfunction

" ********************************************
" END Entity Prototype
" ********************************************



" --------------------------------------------------------
" GetEntity
"   Given a comment type, create an entity object.
" --------------------------------------------------------
function! s:GetEntity(ctype)
  let l:ctype = a:ctype

  if l:ctype == s:FILE_TYPE  
    return {}
  elseif l:ctype == s:METHOD_TYPE  
    return s:newMethodEntity()
  elseif l:ctype == s:INNER_CLASS_TYPE  
    return s:newInnerClassEntity()
  elseif l:ctype == s:INNER_OBJECT_TYPE  
    return s:newInnerObjectEntity()
  elseif l:ctype == s:INNER_TRAIT_TYPE  
    return s:newInnerTraitEntity()
  elseif l:ctype == s:CLASS_TYPE  
    return s:newClassEntity()
  elseif l:ctype == s:OBJECT_TYPE  
    return s:newObjectEntity()
  elseif l:ctype == s:TRAIT_TYPE  
    return s:newTraitEntity()
  elseif l:ctype == s:VAR_TYPE  
    return s:newFieldEntity()
  elseif l:ctype == s:VAL_TYPE  
    return s:newFieldEntity()
  else
    throw "Bad comment type: " . l:ctype 
  endif
endfunction


" ============================================================================
" Initialization: load and build persistent structures
" ============================================================================

" --------------------------------------------------------
" ResetAll
"   Resets whatever needs resetting and checks if 
"     initialization has occurred.
" --------------------------------------------------------
function! s:ResetAll()
  if s:isInitialized == s:IS_FALSE
    call s:Initialize()
  endif
endfunction

" ++++++++++++++++++++++++++++++++++++++++++++
" Has the script been initialized
" ++++++++++++++++++++++++++++++++++++++++++++
let s:isInitialized = s:IS_FALSE

" --------------------------------------------------------
" Initialize
"   Initialization is done once.
" --------------------------------------------------------
function! s:Initialize()
  if s:isInitialized == s:IS_FALSE
    "-------------------------------------------
    " Initialized Description Space
    "   These MUST be in the order of the 
    "   values of the entity TYPE enums
    "-------------------------------------------
    call s:LoadDescriptionSpace()
    call s:CheckConfiguationParameterValues()

    let s:isInitialized = s:IS_TRUE
  endif

endfunction

" ++++++++++++++++++++++++++++++++++++++++++++
" Description Space array
" ++++++++++++++++++++++++++++++++++++++++++++
let g:descriptionSpaceList = []

" --------------------------------------------------------
" LoadDescriptionSpace
"   Load the descriptionSpaceList array
" --------------------------------------------------------
function! s:LoadDescriptionSpace()
  let g:descriptionSpaceList = []

  " FILE_TYPE - has no description space
  call add(g:descriptionSpaceList, 0)

  " INNER_CLASS_TYPE
  if exists("b:scommenter_inner_class_description_space")
    call add(g:descriptionSpaceList, b:scommenter_inner_class_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultInnerClassDescriptionSpace)
  endif

  " INNER_TRAIT_TYPE
  if exists("b:scommenter_inner_trait_description_space")
    call add(g:descriptionSpaceList, b:scommenter_inner_trait_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultInnerTraitDescriptionSpace)
  endif

  " INNER_OBJECT_TYPE
  if exists("b:scommenter_inner_object_description_space")
    call add(g:descriptionSpaceList, b:scommenter_inner_object_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultInnerObjectDescriptionSpace)
  endif

  " CLASS_TYPE
  if exists("b:scommenter_class_description_space")
    call add(g:descriptionSpaceList, b:scommenter_class_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultClassDescriptionSpace)
  endif

  " TRAIT_TYPE
  if exists("b:scommenter_trait_description_space")
    call add(g:descriptionSpaceList, b:scommenter_trait_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultTraitDescriptionSpace)
  endif

  " OBJECT_TYPE
  if exists("b:scommenter_object_description_space")
    call add(g:descriptionSpaceList, b:scommenter_object_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultObjectDescriptionSpace)
  endif

  " METHOD_TYPE
  if exists("b:scommenter_method_description_space")
    call add(g:descriptionSpaceList, b:scommenter_method_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultMethodDescriptionSpace)
  endif

  " VAL_TYPE
  " VAR_TYPE
  if exists("b:scommenter_field_description_space")
    call add(g:descriptionSpaceList, b:scommenter_field_description_space)
    call add(g:descriptionSpaceList, b:scommenter_field_description_space)
  else
    call add(g:descriptionSpaceList, s:defaultFieldDescriptionSpace)
    call add(g:descriptionSpaceList, s:defaultFieldDescriptionSpace)
  endif
endfunction

" --------------------------------------------------------
" CheckConfiguationParameterValues
"   Check if configuration parameters have valid values
" --------------------------------------------------------
function! s:CheckConfiguationParameterValues()
  if exists("b:scommenter_extra_line_text_offset")
    if b:scommenter_extra_line_text_offset > b:scommenter_page_width
      let str = "Configuration parameter: b:scommenter_extra_line_text_offset"
      let str = str . " with value: " b:scommenter_extra_line_text_offset
      let str = str . " is bigger than the page width: "
      let str = str . b:scommenter_page_width
      let str = str . " as defined by b:scommenter_page_width"
      throw str
    else 
      let suggestedMax = b:scommenter_page_width - 50
      if b:scommenter_extra_line_text_offset > suggestedMax
        let str = "Configuration parameter: b:scommenter_extra_line_text_offset"
        let str = str . " is rather large"
        call s:WarningMessage(str)
      endif
    endif
  endif

  if exists("b:scommenter_user_tags")  
    " list of user third-party tags
    "    tagName
    "    hasValue
    "    hasText 
    "    canHaveMoreThanOne
    for definition in b:scommenter_user_tags
      let len = len(definition)
      if len != 4
        let str = "Bad user defined tag: " . string(definition)
        if (len < 4)
          let str = str . ", too few elements"
        else 
          let str = str . ", too many elements"
        endif
        call s:WarningMessage(str)
      endif
      call add(s:userTags, definition)
    endfor
  endif

endfunction





" ============================================================================
" Code functions
" ============================================================================

" ********************************************
" START Write File Comment
" ********************************************

" --------------------------------------------------------
" WriteFileComments
"   Write the top of file comment
" --------------------------------------------------------
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

" --------------------------------------------------------
" StdFileComments
"   Standard file comment
" --------------------------------------------------------
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

" --------------------------------------------------------
" ScalaAPIFileComments
"   The Scala library file comment style
" --------------------------------------------------------
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

" --------------------------------------------------------
" SunFileComments
"   The Sun Java library file comment style
" --------------------------------------------------------
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

  let list_len = len(b:scommenter_file_copyright_list)
  if list_len > 0
    for line in b:scommenter_file_copyright_list
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

" --------------------------------------------------------
" DefaultFileComments
"   Default file comment.
" --------------------------------------------------------
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

  let list_len = len(b:scommenter_file_copyright_list)
  if list_len > 0
    for line in b:scommenter_file_copyright_list
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

" ********************************************
" END Write File Comment
" ********************************************






" ++++++++++++++++++++++++++++++++++++++++++++
" Type of comment enumeration
" ++++++++++++++++++++++++++++++++++++++++++++
let s:UNKNOWN_TYPE       = -1
let s:FILE_TYPE         = 0
let s:INNER_CLASS_TYPE  = 1
let s:INNER_TRAIT_TYPE  = 2
let s:INNER_OBJECT_TYPE = 3
let s:CLASS_TYPE        = 4
let s:TRAIT_TYPE        = 5
let s:OBJECT_TYPE       = 6
let s:METHOD_TYPE       = 7
let s:VAL_TYPE          = 8
let s:VAR_TYPE          = 9

" --------------------------------------------------------
" GetCommentType
"   Determine what entity (comment) type the cursor is 
"     sitting on.
" --------------------------------------------------------
function! s:GetCommentType()
  if s:rangeStart <= 1 && s:rangeStart == s:rangeEnd
    return s:FILE_TYPE  
  elseif s:combinedString =~ s:scalaMethodPattern
    return s:METHOD_TYPE  
  elseif s:combinedString =~ s:scalaInnerClassPattern
    return s:INNER_CLASS_TYPE  
  elseif s:combinedString =~ s:scalaInnerObjectPattern
    return s:INNER_OBJECT_TYPE  
  elseif s:combinedString =~ s:scalaInnerTraitPattern
    return s:INNER_TRAIT_TYPE  
  elseif s:combinedString =~ s:scalaClassPattern
    return s:CLASS_TYPE  
  elseif s:combinedString =~ s:scalaObjectPattern
    return s:OBJECT_TYPE  
  elseif s:combinedString =~ s:scalaTraitPattern
    return s:TRAIT_TYPE  
  elseif s:combinedString =~ s:scalaVariablePattern
    return s:VAR_TYPE
  elseif s:combinedString =~ s:scalaValuePattern
    return s:VAL_TYPE
  else
    return s:UNKNOWN_TYPE  
  endif
endfunction



" ----------------------------------------------------
" Re-Writing complete comment
" ----------------------------------------------------

" structure of dictionary argument
"   dict                        dictionary argument
"   dict.ctype                  comment type
"   dict.descriptionSpace       lines created for description
"   dict.indent                 indent of entity
"   dict.docCommentStart        start of comment
"   dict.docCommentEnd          end of comment
"   dict.tags                   dictionary for entity tags
"   dict.tagsSet                Entity Tags Set
"   dict.comment                dictionary of existing comment
"   dict.comment.firstLineText  list of lines
"   dict.comment.lineList       list of lines
"   dict.comment.stateList      list of line kinds
"   dict.comment.tagsSet        Comment Tags Set

" --------------------------------------------------------
" UpdateAllTags
"   Read existing comments, resolve existing comment tags
"     with the entity's current tags, delete the existing
"     comment and then write out the new comment (lines
"     and tags)
" --------------------------------------------------------
function! s:UpdateAllTags(info)
  let l:info = a:info

  " read existing comment and tags
  call s:LoadExistingTags(l:info)

  " resolve existing comment tags and template tags
  call l:info.entity.resolveTags(l:info)

  " delete existing comment
  call s:DeleteExistingComment(l:info)

  " write /** (optional first line text)
  call s:WriteCommentStart(l:info, l:info.comment.firstLineText)

  " write existing comment normal lines
  call s:WriteExistingCommentNormalLines(l:info)

  call s:WriteSpacingNormalLinesAndTags(l:info)

  " write tags
  let l:tagsSet = l:info.comment.tagsSet
  let maxTagNameLen = s:FindMaxTagNameLen(l:tagsSet) 
  let maxValueLen  = s:FindMaxValueLen(l:tagsSet)
  let tagTextOffset = maxTagNameLen + maxValueLen + 1
  call s:WriteTags(l:tagsSet, maxTagNameLen, tagTextOffset) 

  " write */ and optionally move cursor
  call s:WriteCommentEnd(l:info)
endfunction

" ++++++++++++++++++++++++++++++++++++++++++++
" What kind of comment line was read at the
"   given line position.
" ++++++++++++++++++++++++++++++++++++++++++++
let s:COMMENT_NORMAL_STATE    = 0
let s:COMMENT_TAG_STATE       = 1
let s:COMMENT_TAG_TEXT_STATE  = 2
let s:COMMENT_TAG_EMPTY_STATE = 3 " empty line(s) between tags

" --------------------------------------------------------
" LoadExistingTags
"   Read in a comment's existing comment lines and tags 
"     from docCommentStart to docCommentEnd.
" --------------------------------------------------------
function! s:LoadExistingTags(info)
  let l:info = a:info
  let offset = l:info.getDocCommentStart()
  let endPos = l:info.getDocCommentEnd()- l:info.getDocCommentStart()
  let index = 0

  let l:info.comment = {}
  let l:info.comment.tagsSet = s:newTagsSet()
  let l:info.comment.firstLineText = ''
  let l:info.comment.lineList = []
  let l:info.comment.stateList = []
  let l:info.comment.tagsSet.deletedTagsSet = s:newTagsSet()

  " do first line of comment
  let line = getline(index + offset)
  let l:m = matchlist(line, '^\s*\/\*\*\s*\(.*\)$')
  if l:m != []
    let l:info.comment.firstLineText = l:m[1]
    let index = index + 1
  endif

  let state = s:COMMENT_NORMAL_STATE

  let tag = {}
  let tagName = ''
  let tagValue = ''
  let tagText = ''
  while index <= endPos
    let line = getline(index + offset)
    let l:m = matchlist(line, '^\s*\*\/')
    if l:m != []
      break
    endif 

    let l:m = matchlist(line, '^\s*\*\s*\(.*\)$')
    call add(l:info.comment.lineList, l:m[1])

    let l:m = matchlist(line, '^\s*\*\s*$')
    " empty line
    if l:m != []
      if state == s:COMMENT_NORMAL_STATE
        call add(l:info.comment.stateList, s:COMMENT_NORMAL_STATE)
      elseif state == s:COMMENT_TAG_STATE
        call add(l:info.comment.stateList, s:COMMENT_TAG_EMPTY_STATE)
        let state = s:COMMENT_TAG_EMPTY_STATE
      elseif state == s:COMMENT_TAG_EMPTY_STATE
        call add(l:info.comment.stateList, s:COMMENT_TAG_EMPTY_STATE)
      else
        call add(l:info.comment.stateList, s:COMMENT_TAG_EMPTY_STATE)
        let state = s:COMMENT_TAG_EMPTY_STATE
      endif

    else
      let l:m = matchlist(line, '^\s*\*\s*@\(\S\+\)\s\+\(\S\+\)\(.*\)')
      if l:m != []
        let tagName = l:m[1]
        let tagValue = l:m[2]
        let tagText = s:Trim(l:m[3])

        let tag = s:CreateTag(tagName, tagValue, tagText)
        call l:info.comment.tagsSet.append(tag)

        let state = s:COMMENT_TAG_STATE
        call add(l:info.comment.stateList, s:COMMENT_TAG_STATE)

      elseif state == s:COMMENT_TAG_STATE
        let tagText = substitute(line, '^\s*\*\s*\(.*\)\s*$', '\1', '')
        call tag.extendText(tagText)

        call add(l:info.comment.stateList, s:COMMENT_TAG_TEXT_STATE)

      else
        call add(l:info.comment.stateList, s:COMMENT_NORMAL_STATE)
      endif
    endif

    let index = index + 1
  endwhile
endfunction


" --------------------------------------------------------
" DeleteExistingComment
"   Delete the comment lines from docComentStart to
"     docCommentEnd.
" --------------------------------------------------------
function! s:DeleteExistingComment(info) 
  let l:info = a:info
  call s:DeleteLines(l:info.getDocCommentStart(), l:info.getDocCommentEnd())
endfunction

" --------------------------------------------------------
" WriteExistingCommentNormalLines
"   Write the existing, non-tag line comments.
" --------------------------------------------------------
function! s:WriteExistingCommentNormalLines(info) 
  let l:info = a:info

  let index = 0
  let len = len(info.comment.lineList)
  while index < len 
    if info.comment.stateList[index] == s:COMMENT_NORMAL_STATE
      let line = info.comment.lineList[index]
      call s:AppendStr(' * ' . line)
    endif
    let index = index + 1
  endwhile
endfunction

" --------------------------------------------------------
" WriteSpacingNormalLinesAndTags
"   Optionally write one or more lines between existing
"     comment "normal lines" and the first tag line.
"     If there are no normal lines for if there are no
"     tag lines, then no spacing is written.
" --------------------------------------------------------
function! s:WriteSpacingNormalLinesAndTags(info) 
  let l:info = a:info

  if ! exists("b:scommenter_smart_description_spacing")
    return
  endif
  if b:scommenter_smart_description_spacing == s:IS_FALSE
    return
  endif

  if ! exists("l:info.comment")
    return
  endif

  " are there non-empty normal lines
  let foundNormalLine = s:IS_FALSE

  let len = len(info.comment.lineList) 
  let index = len - 1
  while index >= 0
    if info.comment.stateList[index] == s:COMMENT_NORMAL_STATE
      let line = info.comment.lineList[index]
      if line =~ '^\s*$'
        " the last normal comment written is empty, so
        " do not need to add one.
        return
      else
        let foundNormalLine = s:IS_TRUE
        break
      endif
    endif
    let index = index - 1
  endwhile

  if foundNormalLine == s:IS_FALSE
    " no normal lines
    return 
  endif

  " are there tags
  let l:tagsSet = l:info.comment.tagsSet
  if l:tagsSet.empty()
    " no tag lines
    return
  endif

  " ok, there are tags and the last normal line is not emptys, so 
  " output a empty line
  call s:AppendCommentLine('')
  
endfunction

" --------------------------------------------------------
" WriteCommentStart
"   Write the start of the comment " /**" and optionally
"     output the first line of comment text on the same
"     line. This is NOT according to the JavaDoc standard
"     but many, many ScalaDoc comments in the Scala
"     base library do this.
" --------------------------------------------------------
function! s:WriteCommentStart(info, firstLineText)
  let l:info = a:info
  let s:appendPos = l:info.getDocCommentStart() - 1

  if a:firstLineText != ''
    call s:AppendStr('/** ' . a:firstLineText)
  else
    call s:AppendStr('/**')
  endif
endfunction


" --------------------------------------------------------
" WriteCommentEnd
"   Write the end of the comment " */" and optionally
"     move the cursor to the top of the comment
" --------------------------------------------------------
function! s:WriteCommentEnd(info)
  call s:AppendStr(' */')
  call s:MoveCursor(a:info)
endfunction











" --------------------------------------------------------
" ProcessAnnotation
"   Scan all annotations above the entity. Record any
"   @throws annotations.
" --------------------------------------------------------
function! s:ProcessAnnotation(info)
  let l:info = a:info
  let throwsWrapper = []
  let deprecatedText = ''

  let linenum = s:rangeStart - 1
  let str = getline(linenum)
  while str =~ '^\s*@[a-zA-Z]\+' && linenum > 1
    if str =~ '^\s*@throws('
      let ex = substitute(str, '^\s*@throws(classOf\[\([^\]]*\)\])' , '\1', '')
      if ex != ''
        call add(throwsWrapper, ex) 
      endif
    endif
    if str =~ '^\s*@deprecated('
      let dep = substitute(str, '^\s*@deprecated("\(.*\)")' , '\1', '')
      if dep != ''
        let deprecatedText = dep
      endif
    endif

    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile

  if ! empty(throwsWrapper)
    call sort(throwsWrapper)

    let tagsSet = l:info.getTagsSet()
    for ex in throwsWrapper
      let tag = s:newThrowsTag(ex, '')
      call tagsSet.append(tag)
    endfor
  endif
  if deprecatedText != ''
    let tagsSet = l:info.getTagsSet()
    let tag = s:newDeprecatedTag(deprecatedText)
    call tagsSet.append(tag)
  endif

  call l:info.setAnnotationCount(s:rangeStart - 1 - linenum)
endfunction


" ++++++++++++++++++++++++++++++++++++++++
" Does a comment exist and what kind is it
" ++++++++++++++++++++++++++++++++++++++++

let s:DOC_COMMENT_TYPE_NONE = 0
let s:DOC_COMMENT_TYPE_SINGLE_LINE = 1
let s:DOC_COMMENT_TYPE_MULTI_LINE = 2

" --------------------------------------------------------
" HasDocComments
"   Does the current entity have a ScalaDoc comment.
" --------------------------------------------------------
function! s:HasDocComments(info)
  let l:info = a:info
  if s:HasSingleLineDocComments(l:info) == s:IS_TRUE
    return s:DOC_COMMENT_TYPE_SINGLE_LINE 
  elseif s:HasMultilineDocComments(l:info) == s:IS_TRUE
    return s:DOC_COMMENT_TYPE_MULTI_LINE 
  else
    call l:info.setDocCommentStart(s:rangeStart - l:info.getAnnotationCount())
    return s:DOC_COMMENT_TYPE_NONE 
  endif
endfunction

" --------------------------------------------------------
" HasSingleLineDocComments
"   Does the current entity have a single-line ScalaDoc
"   comment.
" --------------------------------------------------------
function! s:HasSingleLineDocComments(info)
  let l:info = a:info

  let linenum = s:rangeStart - 1 - l:info.getAnnotationCount()
  let str = getline(linenum)
  while str =~ '^\s*$' && linenum > 1
    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile

  if str =~ '^\s*/\*\*.*\*/\s*$'
    call l:info.setSingleLineCommentPos(linenum)
    call l:info.setDocCommentStart(linenum)
    call l:info.setDocCommentEnd(linenum)
    return s:IS_TRUE
  endif
  return s:IS_FALSE
endfunction

" --------------------------------------------------------
" HasMultilineDocComments
"   Does the current entity have a multi-line ScalaDoc
"   comment.
" --------------------------------------------------------
function! s:HasMultilineDocComments(info)
  let l:info = a:info

  " start above any annotations
  let linenum = s:rangeStart - 1 - l:info.getAnnotationCount()
  let str = getline(linenum)
  while str =~ '^\s*$' && linenum > 1
    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile

  " not at a comment, return false
  if str !~ '\*/\s*$' || str =~ '/\*\*.*\*/'
    return s:IS_FALSE
  endif

  " move up the comment
  let l:docCommentEnd = linenum
  let linenum = linenum - 1
  let str = getline(linenum)
  while str !~ '\(/\*\|\*/\)' && linenum >= 1
    let linenum = linenum - 1
    let str = getline(linenum)
  endwhile

  " do we now see the start of a ScalaDoc comment "/**"
  if str =~ '^\s*/\*\*'
    call l:info.setDocCommentEnd(l:docCommentEnd)
    call l:info.setDocCommentStart(linenum)
    return s:IS_TRUE
  else
    return s:IS_FALSE
  endif
endfunction

" --------------------------------------------------------
" ExpandSinglelineCommentsEx
"   Expand a single-line comment into a multi-line comment
" --------------------------------------------------------
function! s:ExpandSinglelineCommentsEx(info, space)
  let line = a:info.getSingleLineCommentPos()
  let str = getline(line)
  let singleLinePattern = '^\s*/\*\*\s*\(.*\)\*/\s*$'
  if str !~ singleLinePattern
    return
  endif
  let s:indent = s:GetIndentation(str)
  let str = substitute(str, singleLinePattern, '\1', '')
  exe "normal " . line . "Gdd"
  let s:appendPos = line - 1
  call s:AppendStr('/**')
  call s:AppendStr(' * ' . str)
  let i = 0
  while a:space > i
    call s:AppendStr(' * ')
    let i = i + 1
  endwhile
  call s:AppendStr(' */')
  call a:info.setDocCommentStart(line)
  call a:info.setDocCommentEnd(line + 2 + a:space)
endfunction

" --------------------------------------------------------
" AddEmptyLineBeforeComment
"   Adds an empty line before comment if there is no
"     empty line and if b:scommenter_add_empty_line is true.
" --------------------------------------------------------
function! s:AddEmptyLineBeforeComment(info)
  let l:info = a:info
  let l:ctype = l:info.__ctype
  let pos = l:info.getDocCommentStart() - 1
  if pos > 0
    let str = getline(pos)
    let emptyLinePattern = '^\s*$'
    if str !~ emptyLinePattern
      if l:ctype == s:METHOD_TYPE
        call s:AddEmpty(pos)
      elseif l:ctype == s:INNER_CLASS_TYPE
        call s:AddEmpty(pos)
      elseif l:ctype == s:INNER_OBJECT_TYPE
        call s:AddEmpty(pos)
      elseif l:ctype == s:INNER_TRAIT_TYPE
        call s:AddEmpty(pos)
      elseif l:ctype == s:CLASS_TYPE
        call s:AddEmpty(pos)
      elseif l:ctype == s:OBJECT_TYPE
        call s:AddEmpty(pos)
      elseif l:ctype == s:TRAIT_TYPE
        call s:AddEmpty(pos)
      endif
    endif
  endif
endfunction



" =======================
" Formating helpers
" =======================

" --------------------------------------------------------
" FindMaxTagNameLen
"   Given a set of tags, find the maximum name length 
" --------------------------------------------------------
function! s:FindMaxTagNameLen(tagsSet) 
  let tags = a:tagsSet.getTags()

  let maxTagNameLen = 0
  for tag in values(tags)
    if ! tag.empty()
      let name = tag.getName()
      let nameLen = len(name)
      if nameLen > maxTagNameLen
        let maxTagNameLen = nameLen
      endif
    endif
   endfor
   return maxTagNameLen
endfunction

" --------------------------------------------------------
" FindMaxValueLen
"   Given a set of tags, find the maximum value length 
" --------------------------------------------------------
function! s:FindMaxValueLen(tagsSet)
  let tags = a:tagsSet.getTags()

  let maxValueLen = 0
  for tag in values(tags)
    if tag.isLeafTag()
      if ! tag.empty() && tag.hasValue()
        let value = tag.getValue()
        let valueLen = len(value)
        if valueLen > maxValueLen
          let maxValueLen = valueLen
        endif
      endif
    else 
      for t in tag.getTags()
        if ! t.empty() && t.hasValue()
          let value = t.getValue()
          let valueLen = len(value)
          if valueLen > maxValueLen
            let maxValueLen = valueLen
          endif
        endif
      endfor
    endif
   endfor
   return maxValueLen
endfunction



" ********************************************
" START Write Tags
" ********************************************

let s:stdTagOrder = [
\  'author',
\  'version',
\  'param',
\  'tparam',
\  'return',
\  'throws',
\  'see',
\  'since',
\  'serial',
\  'serialField',
\  'serialData',
\  'deprecated'
\ ]

" --------------------------------------------------------
" WriteTags
"   Write all tags.
" --------------------------------------------------------
function! s:WriteTags(tagsSet, maxTagNameLen, tagTextOffset) 
  let tagsSet = a:tagsSet
  let maxTagNameLen = a:maxTagNameLen
  let tagTextOffset = a:tagTextOffset

  if exists("b:scommenter_extra_line_text_offset") && b:scommenter_extra_line_text_offset > 0
    let extraLineTextOffset = b:scommenter_extra_line_text_offset
  else
    " use same offset as first line
    let extraLineTextOffset = tagTextOffset
  endif

  let tags = tagsSet.getTags()

  " any tags not part of the standard tags
  let userOfUnknownTagNames = []
  for tagName in keys(tags)
    let found = s:IS_FALSE
    for stdTagName in s:stdTagOrder
      if tagName == stdTagName
        let found = s:IS_TRUE
        break
      endif
    endfor
    if found == s:IS_FALSE
      call add(userOfUnknownTagNames, tagName)
    endif
  endfor

  " should there be a line betweeen the user/unknown tags and 
  " the standard tags
  if exists("b:scommenter_line_between_user_unknown_and_std_tags") && b:scommenter_line_between_user_unknown_and_std_tags
    let hasLine = ! empty(userOfUnknownTagNames)
  else
    let hasLine = s:IS_FALSE
  endif

  " should user/unknown tags appear before standard tags
  if exists("b:scommenter_user_unknown_before_std_tags") && b:scommenter_user_unknown_before_std_tags
    let userOrUnknowBeforeStd = s:IS_TRUE
  else
    let userOrUnknowBeforeStd = s:IS_FALSE
  endif

  if userOrUnknowBeforeStd
    call  s:WriteUserOrUnknownTag(tags, userOfUnknownTagNames, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
    if hasLine
      call s:AppendCommentLine('')
    endif
    call  s:WriteStdTag(tags, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
  else
    call  s:WriteStdTag(tags, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
    if hasLine
      call s:AppendCommentLine('')
    endif
    call  s:WriteUserOrUnknownTag(tags, userOfUnknownTagNames, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
  endif

  if exists("b:scommenter_warn_deleted_tags") && b:scommenter_warn_deleted_tags 
    " Only when re-generating a comment will the tagsSet have the
    "   deletedTagsSet key.
    if has_key(tagsSet, "deletedTagsSet")
      let deletedTagsSet  = tagsSet.deletedTagsSet
      if ! deletedTagsSet.empty()
        call s:AppendCommentLine('')
        call s:AppendCommentLine(s:deletedTagWaringStr)

        let maxTagNameLen = s:FindMaxTagNameLen(deletedTagsSet) 
        let maxValueLen  = s:FindMaxValueLen(deletedTagsSet)
        let tagTextOffset = maxTagNameLen + maxValueLen + 1

        let deletedTags  = deletedTagsSet.getTags()
        for key in keys(deletedTags)
          let tag = deletedTags[key]
          call s:WriteTag(tag, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
        endfor
      endif
    endif
  endif
endfunction

function! s:WriteStdTag(tags, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
  for tagName in s:stdTagOrder
    if has_key(a:tags, tagName)
      let tag = a:tags[tagName]
      call s:WriteTag(tag, a:maxTagNameLen, a:tagTextOffset, a:extraLineTextOffset) 
    endif
  endfor
endfunction

function! s:WriteUserOrUnknownTag(tags, userOfUnknownTagNames, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
  for tagName in a:userOfUnknownTagNames
    if has_key(a:tags, tagName)
      let tag = a:tags[tagName]
      call s:WriteTag(tag, a:maxTagNameLen, a:tagTextOffset, a:extraLineTextOffset) 
    endif
  endfor
endfunction

" --------------------------------------------------------
" WriteTag  
"   Write tag at given output position (s:appendPos) using 
"     current indent (s:indent)
" --------------------------------------------------------
function! s:WriteTag(tag, maxTagNameLen, tagTextOffset, extraLineTextOffset) 
  let tag = a:tag
  let maxTagNameLen = a:maxTagNameLen
  let tagTextOffset = a:tagTextOffset
  let extraLineTextOffset = a:extraLineTextOffset

  let tagName = tag.getName()
  if tag.isLeafTag()
    let diff = maxTagNameLen - len(tagName) + 1
    let hasValue = tag.hasValue()
    let hasText = tag.hasText()

    if hasValue
      let tagValue = tag.getValue()
      if hasText
        let tagText = tag.getText()
        let sp = s:MakeEmptyString(diff)
        if tagText == ''
          call s:AppendStr(' * @' . tagName . sp . tagValue)
        else
          let t = '@' . tagName . sp . tagValue
          call s:AddValueAndText(t, tagText, tagTextOffset, extraLineTextOffset) 
        endif

      else
        call s:AppendStr(' * @' . tagName . s:MakeEmptyString(diff) . tagValue)
      endif

    elseif hasText
      let tagText = tag.getText()
      if tagText == ''
        call s:AppendStr(' * @' . tagName )
      else
        let diff = maxTagNameLen - len(tagName) + 1
        let sp = s:MakeEmptyString(diff)
        call s:AddValueAndText('@' . tagName . sp, tagText, tagTextOffset, extraLineTextOffset)

      endif

    else
      call s:AppendStr(' * @' . tagName)
    endif

  else 
    let tags = tag.getTags()
    for t in tags
      call s:WriteTag(t, maxTagNameLen, tagTextOffset, extraLineTextOffset)
    endfor

  endif
endfunction

" --------------------------------------------------------
" AddValueAndText
"  A tag line is made up of '* @tagName tagValue? tagText?'. 
"    The tagText might be longer than the page-width. This
"    method will output the tag line-wrapping the tagText
"    across multiple lines if neeed.
" --------------------------------------------------------
function! s:AddValueAndText(firstLine, tagText, tagTextOffset, extraLineTextOffset) 
  let firstLine = a:firstLine
  let tagText = a:tagText
  let tagTextOffset = a:tagTextOffset
  let extraLineTextOffset = a:extraLineTextOffset

  let indent_len = strlen(s:indent)
  if indent_len + tagTextOffset + strlen(tagText) < b:scommenter_page_width
    let diff = tagTextOffset - strlen(firstLine)
    let sp = s:MakeEmptyString(diff + 2)
    call s:AppendCommentLine(firstLine . sp . tagText)
  else 

    let offset = b:scommenter_page_width - indent_len - tagTextOffset - 6
    let index = offset
    let c = strpart(tagText, index, 1)
    while c != ' ' && index > 0
      let index = index - 1
      let c = strpart(tagText, index, 1)
    endwhile
    let part = strpart(tagText, 0, index)
    let tagText = s:Trim(strpart(tagText, index))

    let diff = tagTextOffset - strlen(firstLine)
    let sp = s:MakeEmptyString(diff + 2)
    call s:AppendCommentLine(firstLine . sp . part)

    " keep outputting tagText, breaking it into sections small enough
    " to fit on a line
    let offset = b:scommenter_page_width - indent_len - extraLineTextOffset - 6
    while strlen(tagText) > offset
      let index = offset
      let c = strpart(tagText, index, 1)
      while c != ' ' && index > 0
        let index = index - 1
        let c = strpart(tagText, index, 1)
      endwhile
      let part = strpart(tagText, 0, index)
      let tagText = s:Trim(strpart(tagText, index))

      let diff = extraLineTextOffset
      let sp = s:MakeEmptyString(diff + 2)
      call s:AppendCommentLine(sp . part)
    endwhile

    " last text line
    if strlen(tagText) > 0
      let diff = extraLineTextOffset
      let sp = s:MakeEmptyString(diff + 2)
      call s:AppendCommentLine(sp . tagText)
    endif
  endif
endfunction

" ********************************************
" END Write Tags
" ********************************************



" ============================================================================
" Utility functions
" ============================================================================

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




function! s:GetIndentation(string)
  return substitute(a:string, '^\(\s*\).*', '\1', '')
endfunction

function! s:GetIndentationAtPosition(pos)
  let line = getline(a:pos)
  return s:GetIndentation(line)
endfunction


" --------------------------------------------------------
" WarningMessage
"   Printed only if b:scommenter_warning_message_enable 
"     is true
" --------------------------------------------------------
function! s:WarningMessage(string)
  if exists("b:scommenter_warning_message_enable") && b:scommenter_warning_message_enable
    echo '[ScalaCommenter.WARING]: ' . a:string
  endif
endfunction

function! s:Message(string)
  echo '[ScalaCommenter] ' . a:string
endfunction




" --------------------------------------------------------
" GetCombinedString
"   Returns one string combined from the strings on the 
"     given range.
" --------------------------------------------------------
function! s:GetCombinedString(rangeStart, rangeEnd)
  let line = a:rangeStart
  let combinedString = getline(line)

  while line < a:rangeEnd
    let line = line + 1
    let combinedString = combinedString . ' ' . getline(line)
  endwhile

  return substitute(combinedString, '^\([^;{]*[;{]\=\).*', '\1', '')
endfunction





" --------------------------------------------------------
" AddEmpty
"   Adds an empty line a postion pos if 
"     b:scommenter_add_empty_line is true.
" --------------------------------------------------------
function! s:AddEmpty(pos)
  if exists("b:scommenter_add_empty_line") && b:scommenter_add_empty_line
    if getline(a:pos) !~ '^\s*$'
      let oldAppendPos = s:appendPos
      let s:appendPos = a:pos
      call s:AppendStr("")
      let s:appendPos = oldAppendPos
    endif
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


function! s:AppendCommentLine(string)
  call s:AppendStr(' * ' . a:string)
endfunction

let s:appendPos = 1

" A function for appending strings to the buffer.
" First set the 's:appendPos', then call this function repeatedly to append
" strings after that position.
function! s:AppendStr(string)
  call append(s:appendPos, s:indent . a:string)
  let s:appendPos = s:appendPos + 1
endfunction




function! s:DeleteLines(startPos, endPos)
  execute a:startPos . ',' . a:endPos . 'd'
endfunction

function! s:Trim(string)
  return substitute(a:string, '^\s*\(.\{-}\)\s*$', '\1', '')
endfunction

function! s:MoveCursor(info) 
  if !exists("b:scommenter_move_cursor")
    return
  endif
  if !b:scommenter_move_cursor
    return
  endif

  if exists("b:scommenter_description_starts_from_first_line") && b:scommenter_description_starts_from_first_line
    call s:MoveCursorToEOL(a:info.getDocCommentStart())
  else
    call s:MoveCursorToEOL(a:info.getDocCommentStart() + 1)
  endif

  if exists("b:scommenter_autostart_insert_mode") && b:scommenter_autostart_insert_mode
    startinsert
  endif
endfunction

function! s:MoveCursorToEOL(line)
  exe "normal " . a:line . "G$"
endfunction

