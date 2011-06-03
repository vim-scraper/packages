" Tom Bast
" tb1911@earthlink.net
" $Date: 2002/10/02 15:23:58 $
" $Revision: 1.3 $
" This will create Java Get/set methods for a bean. The process is to first 
" create your properties (for example: int id; String userName;) and 
" let this make the methods. It requires some manual
" work, but not much.  You need to put the datatype in buffer 'a' and 
" the property in buffer 'b'  For example:
" say you have this:
" public String foo, bar, baz;
" Yank word "String" into buffer 'a' and if you want to make get/set for foo,
" yank word "foo" into buffer 'b'.  Then go to where you want the get/set
" methods and type <ctl>gs and the methods with Javadoc are created. 
" To do "bar" next, just
" replace buffer 'b' with "bar"
:map  o   /**Get "bpa.@return "bpa as a "apo<BS>/   public "apa get"bpb   ~A(){return("bpa);}/**Set "bpa.@param The "bpa as a "apo<BS>/public void set"bpb   ~A("apa "bpa){this."bpa = "bpa;return;}

