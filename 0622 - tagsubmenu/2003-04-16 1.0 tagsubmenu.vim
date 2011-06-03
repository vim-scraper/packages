" This script adds a submenu under the Tools menu for vim tags commands
" Use it by copying it into your plugin directory, or source it
" from your .vimrc file.
amenu 40.325 Tools.Tag.Jump.To\ this\ tag<Tab>gCtrl-] g<C-]>
amenu Tools.Tag.Jump.Direct\ to\ first\ match<Tab>Ctrl-]  <C-]>
amenu Tools.Tag.Jump.Back<Tab>Ctrl-T <C-T>
amenu Tools.Tag.Jump.Prompt\ for\ tag<Tab>:tag :tag 
amenu Tools.Tag.Tagselect.on\ word\ at\ cursor<Tab>g] g]
amenu Tools.Tag.Tagselect.Next\ match<Tab>:tn :tn<CR>
amenu Tools.Tag.Tagselect.Previous\ match<Tab>:tp\ or\ :tN :tp <CR>
amenu Tools.Tag.Tagselect.Rewind\ to\ first\ match<Tab>:tr[ewind] :tr<CR>
amenu Tools.Tag.Tagselect.Last\ match<Tab>:tl[ast] :tl<CR>
amenu Tools.Tag.Tagselect.Prompt\ for\ tagselect<Tab>:ts :ts 
amenu Tools.Tag.Tagselect.Prompt\ for\ tagselect\ then\ split<Tab>:sts :sts 
amenu Tools.Tag.Show.tag\ stack<Tab>:tags :tags<CR>
nmenu Tools.Tag.Show.tag\ file\ list<Tab>:set\ tags :se tags<CR>
