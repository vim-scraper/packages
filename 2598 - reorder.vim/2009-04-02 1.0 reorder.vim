"reorder.vim
" A Vim command for reordering English lists in the form
" "foo, bar, and baz" or "foo, bar, or baz"
"
"Copyright (c) 2009 Ken Bloom
"
"Permission is hereby granted, free of charge, to any person
"obtaining a copy of this software and associated documentation
"files (the "Software"), to deal in the Software without
"restriction, including without limitation the rights to use,
"copy, modify, merge, publish, distribute, sublicense, and/or sell
"copies of the Software, and to permit persons to whom the
"Software is furnished to do so, subject to the following
"conditions:
"
"The above copyright notice and this permission notice shall be
"included in all copies or substantial portions of the Software.
"
"THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
"EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
"OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
"NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
"HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
"WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
"FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
"OTHER DEALINGS IN THE SOFTWARE.


function! s:ReorderList(line1,line2,order)
ruby <<EOF
   require 'rubygems'
   require 'facets'
   class String
      #escapes a string so it can be concatenated into
      #a call to VIM.evaluate
      def vim_escape
	 result=dup
	 result.gsub!("\\",'\\\\\\\\')
	 result.gsub!('"','\"')
	 result
      end
   end
   class Array
     # Pull the indexes in from +self+ into a new array
     # in the order they are given in +order+
     def mypermute order
       order.collect{|x| self[x]}
     end
   end

   # get the vim visual mode selection
   selection=VIM.evaluate("@*")
   old_x=VIM.evaluate("@x")
   # get the argument to the current function
   order=VIM.evaluate("a:order")
   # parse the order parameter
   # convert from 1-based indexing (more convenient for humans)
   # to zero-based indexing (the language of Array#mypermute
   order=order.split(",").map{|x| x.to_i-1}

   #scan for list items, and figure out which conjunction was
   #in the original list (because that's the one that's going to go in the new list)
   conjunctions,items,=selection.scan(/\s*(and |or )?([^,]*)(,|$)/).transpose
   items.reject!{|x| x.nil? or x==""}
   conj=conjunctions.detect{|x| x}

   #put them in order, and join them back together
   result=items.mypermute(order).conjoin(", ",", #{conj}")

   #Now, the secret sauce to put the new list back in the document:
   #escape the string that we've built up, and put it in register x
   VIM::evaluate("setreg('x',\"#{result.vim_escape}\")")
   #more magic: start in normal mode,
   #hit v to switch to visual mode
   #hit gv to get the selection selected again
   #paste register x to overwrite the selection
   VIM::command(":normal vgv\"xP")
   VIM::evaluate("setreg('x',\"#{old_x.vim_escape}\")")
EOF
endfunction

command! -range -nargs=+ Reorder call s:ReorderList(<line1>,<line2>,<f-args>)
