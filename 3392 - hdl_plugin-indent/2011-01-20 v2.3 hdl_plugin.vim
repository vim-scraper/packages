"------------------------------------------------------------------------------
" Created by         : Vorx Ltd.com
" Filename           : hdl_plugin.vim
" Author             : ChenYong
" Created On         : 2010-11-02 13:17
" Last Modified      : 2010-12-08 14:07
" Description        : vhdl/verilog plugin
" Version            : v2.3
"
" history            :  v1.0    创建插件，实现编译，加入注释，文件头等功能 
"                       v1.1    加入函数HDL_Component_Build() 可以实现垂直分割窗口
"                               生成component信息
"                       v1.2    加入函数Tb_Build() 可以为vhdl模块生成testbench文档
"                       v1.3    1 生成进程的命令改为：ProBuild
"                               2 加入函数HDL_Tb_Build(type) 函数
"                                   代替函数Tb_Build() 
"                                   修改了testbench文档的生成方式
"                                   功能：可以生成vhdl模块的vhdl testbench或者 verilog testbench
"                               3 修改了HDL_Component_Build()函数
"                                   修改了component的生成方式
"                               4 代码风格做了一些修改
"                               5 修改了光标位置
"                       v1.4    修改了HDL_Tb_Build(type)函数 使生成的component按原信号顺序排列
"                       v1.5    加入菜单
"                       v1.6    优化程序
"                       v1.7    HDL_Component_Build可以用变量定义选择instant窗口的方式
"                               不定义 g:HDL_RightB_Commponent  则水平分割打开
"                               g:HDL_RightB_Commponent = 1 原文件右侧垂直打开
"                               g:HDL_RightB_Commponent = 0 原文件左侧垂直打开
"                       v1.8    修改了一些错误
"                       v1.9    1 修改了HDL_Add_File_Information()和HDL_Add_Content()函数
"                               2 加入变量g:HDL_Author g:HDL_Company g:HDL_Verilog_Timescale
"                                 可以在vimrc中添加设置,例如:
"                                   let g:HDL_Company = "Vorx"
"                                   let g:HDL_Author = "ChenYong"
"                                   let g:HDL_Verilog_Timescale = " 1ns / 1ns"
"                               3 加入generic部分 使可识别generic
"                               4 加入g:HDL_Clock_Period 时钟周期可设置，默认为64
"                                   let g:HDL_Clock_Period = 64
"                               5 暂时不支持一行多个port
"                               6 菜单中加入compile file 默认快捷键为<F7>
"                               7 菜单中加入vlib work 默认快捷键为<F6>
"                                   需要安装modelsim。windows下需设置环境变量PATH=$ModelSim\win32
"                       v2.0    现在可以支持同一行多个port了
"                       v2.1    支持inout端口
"                               支持verilog模块，可为verilog模块生成testbench和instant
"                       v2.2    加入格式整理的功能。
"
"                               支持component，entity，signal，instant模块。
"                               尽量使“:,=>”符号对齐。例如：
"                                   component bufg
"                                        port( i   : in	std_logic;  
"                                                o : out	std_logic 
"                                            );
"                                    end component;
"                               整理后为：
"                                   component bufg
"                                       port(
"                                               i				: in	std_logic;  
"                                               o				: out	std_logic 
"                                            );
"                                   end component;
"
"                               整理前会先进行编译。
"
"                               对于较大的文件可能时间会有3-4秒延迟(indent操作比较费时间)。
"
"                               变量名字超过16个字符将不留空格。如：
"                                   cak_ram_char_ch14: out	std_logic_vector(1 downto 0)
"                       v2.3    加入了一个添加signal的函数，感谢作者sunil shukla。
"                               我仅修改了一下正则匹配方式，和标志begin查找方式。
"                               设定快捷键<leader>,,添加。光标要停留在未定义的signal上。
"                               会询问signal长度。很好用。
"                               Fixed Some Bugs.
"                               Redifined The function name.
"
"
"
"
"
"
"    hdl_plugin is a plugin that enables you to fast instant and generate a testbench file.
"    it can help you to compile by modelsim convenient.
"

"    main function:
"    Add a menu for vim
"
"    Create a Library: 
"    Compile File:
"    Add File Header: 
"    Add Content: 
"    Always/Process: 
"    Entity/Module:
"    Vhdl Component: Creat a window to display the Component information,and add these to clipboard
"    Verilog Instant: Fast instant for verilog.Also add to clipboard
"    Vhdl Testbench: Generate a vhdl testbench file 
"    Verilog Testbench: Generate a verilog testbench file
"    Format Vhdl File: it can help you to finishing the code.
"                      make ":,=>" in the same position.
"                      Support the "component","signal","instant"and "entity" part.
"    Use <leader>, to fast define signal.
"
"    view details:http://www.cnblogs.com/ifys/archive/2010/11/20/1882673.html#
"    e-mail: achillowy@163.com
"    Welcome to post your suggestions to me.
"                      
"------------------------------------------------------------------------------
if exists('b:hdl_plugin') || &cp || version < 700
    finish
endif
let b:hdl_plugin = 1

if !exists("g:HDL_Menu")
    nmenu HDL.Create\ a\ Library<Tab>F6             <Esc>:!vlib work<CR><CR>
    nmenu HDL.Compile\ File<Tab>F7                  <Esc>:ModSimComp<CR><CR>
    nmenu HDL.Add\ File\ Header<Tab>:AddInfo        :AddInfo<CR>
    nmenu HDL.Add\ Content<Tab>:Acontent            :Acontent<CR>
    nmenu HDL.Process/Always<Tab>:ProBuild          :ProBuild<CR>
    nmenu HDL.Entity/Module<Tab>:VhdlEntity         :VhdlEntity<CR>
    nmenu HDL.Vhdl\ Component<Tab>:CompoB           :CompoB<CR> 
    nmenu HDL.Verilog\ Instant<Tab>:InstantV        :InstantV<CR>
    nmenu HDL.Vhdl\ Testbench<Tab>:TbVhdl           :TbVhdl<CR>
    nmenu HDL.Verilog\ Testbench<Tab>:TbVerilog     :TbVerilog<CR>
    nmenu HDL.Format\ Vhdl\ File<Tab>:FormatVHDL    :FormatVHDL<CR> 
endif

command     AddInfo     :call HDL_Add_File_Information()
command     Acontent    :call HDL_Add_Content()
command     ProBuild    :call HDL_Always_Process_Build("posedge", "posedge")
command     VhdlEntity  :call HDL_Entity_Module_Build()
command     ModSimComp  :call HDL_Modelsim_Compile()|:cw
command     CompoB      :call HDL_Component_Build("vhdl")
command     InstantV    :call HDL_Component_Build("verilog")
command     TbVhdl      :call HDL_Tb_Build("vhdl")
command     TbVerilog   :call HDL_Tb_Build("verilog")
command     FormatVHDL  :call HDL_Vhdl_Format()
command     SetSignal   :call HDL_Signal_Dec_Vhdl()

nmap <silent> <unique> <F7> :ModSimComp<CR><CR>
nmap <silent> <unique> <F6> <Esc>:!vlib work<CR><CR>
nmap <unique> <leader>, <Esc>:SetSignal<CR>

if !exists("g:HDL_Width_of_Component")
    let g:HDL_Width_of_Component = "60"
endif

if !exists("g:HDL_Height_of_Component")
    let g:HDL_Height_of_Component = "25"
endif

if !exists("g:HDL_Author")
    let g:HDL_Author = "ChenYong"
endif

if !exists("g:HDL_Company")
    let g:HDL_Company = ""
endif

if !exists("g:HDL_Verilog_Timescale")
    let g:HDL_Verilog_Timescale = "1ns / 1ps"
endif

if !exists("g:HDL_Clock_Period")
    let g:HDL_Clock_Period = 64
endif

"------------------------------------------------------------------------------
"Function    : HDL_Modelsim_Compile() 
"Description : Compile with ModelSim  
"------------------------------------------------------------------------------
function HDL_Modelsim_Compile()
    if HDL_Check_Filetype() == 1
        set makeprg=vcom\ -work\ work\ %
        execute "make"
    elseif HDL_Check_Filetype() == 2 
        set makeprg=vlog\ -work\ work\ %
        execute "make"
    else
        echohl ErrorMsg
        echo "This filetype can't be compiled by modelsim vcom/vlog!"
        echohl None 
    endif
endfunction

"set error format 
set errorformat=\*\*\ %tRROR:\ %f(%l):\ %m,\*\*\ %tRROR:\ %m,\*\*\ %tARNING:\ %m,\*\*\ %tOTE:\ %m,%tRROR:\ %f(%l):\ %m,%tARNING\[%*[0-9]\]:\ %f(%l):\ %m,%tRROR:\ %m,%tARNING\[%*[0-9]\]:\ %m

"------------------------------------------------------------------------
"Function    : HDL_Add_File_Information() 
"Decription  : Add File Header 
"------------------------------------------------------------------------
function HDL_Add_File_Information()
    if HDL_Check_Filetype() == 1
        let comment = "--"
        let others = "library ieee;\nuse ieee.std_logic_1164.all;\nuse ieee.std_logic_arith.all;\n"
                    \."use ieee.std_logic_unsigned.all;\n\n"
    elseif HDL_Check_Filetype() == 2 
        let comment = "//"
        let others = "`timescale ".g:HDL_Verilog_Timescale."\n\n"
    elseif HDL_Check_Filetype() == 3 
        let comment = "\""
        let others = ""
"if you want add other file type ,then add in here 
"    elseif file_type_temp == ""
"        let comment = ""
"        let others = ""
    else 
        let comment = ""
        let others = ""
    endif
    let header = comment."-------------------------------------------------------------------------------\n"
                \.comment." Created by\t\t: ".g:HDL_Company."\n".comment." Filename\t\t\t: ".expand("%")."\n"
                \.comment." Author\t\t\t: ".g:HDL_Author."\n".comment." Created On\t\t: "
                \.strftime("%Y-%m-%d %H:%M")."\n".comment." Last Modified\t: \n".comment
                \." Version\t\t\t: v1.0\n".comment." Description\t\t: \n".comment."\t\t\t\t\t\t\n".comment
                \."\t\t\t\t\t\t\n".comment
                \."-------------------------------------------------------------------------------\n\n"
                \.others
    exe "ks"
    exe "normal gg"
    silent put! =header
    exe "'s"
endfunction

"------------------------------------------------------------------------------
"Function  : HDL_Add_Content() 
"Description: 在光标当前位置插入注释
"------------------------------------------------------------------------------
function HDL_Add_Content()
    let file_type_temp = expand("%:e")
"    let file_type_temp = getftype(expand("%:p"))
    if HDL_Check_Filetype() == 1
        let comment = "--"
    elseif HDL_Check_Filetype() == 2 
        let comment = "//"
    elseif HDL_Check_Filetype() == 3 
        let comment = "\""
"if you want add other file type ,then add in here 
"    elseif HDL_Check_Filetype() == 
"        let comment = ""
    else 
        let comment = ""
    endif
    let content = comment."-------------------------------------------------------------------------------\n"
                \.comment." Function\t\t:\t\n".comment." Description\t:\t\n"
                \.comment."-------------------------------------------------------------------------------\n"
    silent put =content
    call search("Function",'b')
    exe "normal $"
endfunction

"---------------------------------------------------------------
"        Verilog中插入always
"        VHDL中插入process
"        Add an always or process statement
"        you must add comment after signal declare 
"        such as:
"        verilog:
"        input  clk; //clock
"        input  rst; //reset 
"        or:
"        reg    clk; //clock
"        reg    rst; //reset
"        vhdl:
"        port(
"        clk    :   std_logic;      --clock
"        rst    :   std_logic       --reset 
"        )
"        or: 
"        signal     clk     :   std_logic;  --clock
"        signal     rst     :   std_logic;  --reset
"---------------------------------------------------------------
function HDL_Always_Process_Build(clk_edge, rst_edge)
    let file_type_temp = expand("%:e")
    if file_type_temp == "verilog"
       for line in getline(1, line("$"))
           if line =~ '^\s*//'
               continue
           elseif line =~ '^\s*\<input\>.*//\s*\<clock\>\s*$'
              let line = substitute(line, '^\s*\<input\>\s*', "", "")
              let clk  = substitute(line, '\s*;.*$', "", "")
           elseif line =~ '^\s*\<input\>.*//\s*\<reset\>\s*$'
              let line = substitute(line, '^\s*\<input\>\s*', "", "")
              let rst  = substitute(line, '\s*;.*$', "", "")
           elseif line =~ '^\s*\<reg\>.*//\s*\<clock\>\s*$'
              let line = substitute(line, '^\s*\<reg\>\s*', "", "")
              let clk  = substitute(line, '\s*;.*$', "", "")
           elseif line =~ '^\s*\<reg\>.*//\s*\<reset\>\s*$'
              let line = substitute(line, '^\s*\<reg\>\s*', "", "")
              let rst  = substitute(line, '\s*;.*$', "", "")
           endif
       endfor

       if !exists('clk')
           let clk = "clk"
       endif

       if !exists('rst')
           let rst = "rst"
       endif

       let curr_line = line(".")
       if a:clk_edge == "posedge" && a:rst_edge == "posedge"
          call append(curr_line,   "always @(posedge ".clk." or posedge ".rst.") begin ")
          call append(curr_line+1, "  if (".rst.") begin")
          call append(curr_line+2, "  end")
          call append(curr_line+3, "  else begin")
          call append(curr_line+4, "  end")
          call append(curr_line+5, "end")
       elseif a:clk_edge == "negedge" && a:rst_edge == "posedge"
          call append(curr_line,   "always @(negedge ".clk." or posedge ".rst.") begin ")
          call append(curr_line+1, "  if (".rst.") begin")
          call append(curr_line+2, "  end")
          call append(curr_line+3, "  else begin")
          call append(curr_line+4, "  end")
          call append(curr_line+5, "end")
       elseif a:clk_edge == "posedge" && a:rst_edge == "negedge"
          call append(curr_line,   "always @(posedge ".clk." or negedge ".rst.") begin ")
          call append(curr_line+1, "  if (!".rst.") begin")
          call append(curr_line+2, "  end")
          call append(curr_line+3, "  else begin")
          call append(curr_line+4, "  end")
          call append(curr_line+5, "end")
       elseif a:clk_edge == "negedge" && a:rst_edge == "negedge"
          call append(curr_line,   "always @(negedge ".clk." or negedge ".rst.") begin ")
          call append(curr_line+1, "  if (!".rst.") begin")
          call append(curr_line+2, "  end")
          call append(curr_line+3, "  else begin")
          call append(curr_line+4, "  end")
          call append(curr_line+5, "end")
       elseif a:clk_edge == "posedge" && a:rst_edge == ""
          call append(curr_line,   "always @(posedge ".clk.") begin ")
          call append(curr_line+1, "end")
       elseif a:clk_edge == "negedge" && a:rst_edge == ""
          call append(curr_line,   "always @(negedge ".clk.") begin ")
          call append(curr_line+1, "end")
       else
          call append(curr_line,   "always @(*) begin")
          call append(curr_line+1, "end")
       endif
   elseif file_type_temp == "vhd"
       for line in getline(1, line("$"))
           if line =~ '^\s*--.*$'
              continue 
           else
               if line =~ '^.*\<in\>.*\<std_logic\>.*\<clock\>.*$'
                   let line = substitute(line,'\s*:.*$',"","")
                   let clk  = substitute(line,'^\s*',"","")
               elseif line =~ '^.*\<in\>.*\<std_logic\>.*\<reset\>.*$'
                   let line = substitute(line,'\s*:.*$',"","")
                   let rst  = substitute(line,'^\s*',"","")
               elseif line =~ '^.*\<signal\>.*\<std_logic\>.*\<clock\>.*$'
                   let line = substitute(line,'\s*:.*$',"","")
                   let clk  = substitute(line,'^.*\<signal\>\s*',"","")
               elseif line =~ '^.*\<signal\>.*\<std_logic\>.*\<reset\>.*$'
                   let line = substitute(line,'\s*:.*$',"","")
                   let rst  = substitute(line,'^.*\<signal\>\s*',"","")
               endif
           endif
       endfor

       if !exists('clk')
           echohl ErrorMsg
           echo     "Clock Set is Wrong...."
           echohl None
           return
       endif

       if !exists('rst')
           echohl ErrorMsg
           echo     "Reset Set is Wrong...."
           echohl None
           return
       endif

       let curr_line = line('.')
       call append(curr_line,"process(".clk.",".rst.") ") 
       call append(curr_line+1,"begin ")
       call append(curr_line+2,"    if ".rst."='1' then ")
       call append(curr_line+3,"    elsif rising_edge(".clk.") then")
       call append(curr_line+4,"    end if; ")
       call append(curr_line+5,"end process; ")


   else
       echohl ErrorMsg
       echo "Wrong filetype!"
       echohl None 
   endif 
endfunction

"------------------------------------------------------------------------------
"Function    : HDL_Entity_Module_Build() 
"Description : 在当前位置插入entity
"------------------------------------------------------------------------------
function HDL_Entity_Module_Build()
    let file_type_temp = expand("%:e")
    let ent_name = inputdialog("entity name:")
    if ent_name != ""
        if file_type_temp == "vhd"
            let all_part = "entity ".ent_name." is\n\tport (\n\n\t);\nend ".ent_name.";\n\narchitecture arc of "
                        \.ent_name." is\n\n\nbegin\n\nend arc;"
        elseif file_type_temp == "v"
            let all_part = "module ".ent_name."\n(\n\n);\n\nendmodule"
        else 
            echohl ErrorMsg
            echo "Wrong filetype!"
            echohl None 
        endif
        silent put! =all_part
        call search('\<port\>\s*(','b')
    endif
endfunction 

"------------------------------------------------------------------------
"Function    : HDL_Entity_Information() 
"Decription  : get position and port map of the entity 
"------------------------------------------------------------------------
function HDL_Entity_Information()
    " 保存初始位置，entity读取完成跳转回来
    exe "ks"
    if HDL_Check_Filetype() == 1
        " Get the entity position
        let first_line = search('\(--.*\)\@<!\<entity\>\_.\{-}\<is\>','w')
        if first_line == 0
            echo "Can't Find Start Entity."
            return 0
        endif
        "let last_line = searchpair('\<entity\>\_s*\(\<[a-zA-Z0-9_]*\>\)\_s*\<is\>','','\<end\>\s*\1;','w')
        let last_line = search('\<entity\>\_s*\(\<[a-zA-Z0-9_]*\>\)\_s*\<is\>\_.\{-}\zs\<end\>\_s*\1;','w')
        if last_line == 0
            echo "Can't Find End Entity."
            return 0
        endif
        " entity name 
        let line = getline(first_line)
        let s:ent_name = substitute(line,'^\s*\<entity\>\_s*\(\<[a-zA-Z0-9_]*\>\)\_s*\<is\>.*$',"\\1","")
        " 端口的首行和末行
        "let port_start_line = search('\<entity\>\_.\{-}\<is\>\_s*\zs\(--.*\)\@<!\<port\>','w')
        call cursor(first_line,1)
        let port_start_line = search('^\s*\(--.*\)\@<!\<port\>','W',last_line)
"        let port_start_line = search('\<entity\>\_.\{-}\<is\>\_s*\%(--.*\_s*\)\{-}\(--.*\)\@<!\<port\>\_s*\zs(','W')
        if port_start_line == 0
            echo "Can't Find The Start of Port."
            return
        endif
        normal %
        let port_last_line = line('.')
        " 检查generic的首行和末行
        call cursor(first_line,1)
        let s:generic_start_line = search('\(--.*\)\@<!\<generic\>','W',last_line)
        if s:generic_start_line != 0
            call search('(','W')
            normal %
            let generic_last_line = line('.')
            let s:generic_count = 0
            call HDL_Generic_Port(s:generic_start_line,generic_last_line)
        endif
        " 设置3个List来存放端口的信息
        let s:port_cout = 0
        let s:port = []
        let s:type = []
        let s:direction = []
        let i = port_start_line
        while i <= port_last_line
            let line = getline(i)
            " 将行尾的;和最后一行的);去掉
            if i == port_last_line
                let line = substitute(line,'\s*)\s*;.*$',"","")
            else 
                let line = substitute(line,'\s*;.*$',"","")
            endif
            " 注释行跳过
            if line =~ '^\s*--.*$'
                let i = i + 1
                continue
            endif
            " port和signal在一行时删去port(
            if line =~ '^\s*\<port\>\s*(.*'
                let line = substitute(line,'^\s*\<port\>\s*(\s*',"","")
            endif
            " 行首的(删掉
            if line =~ '^\s*(.*$'
                let line = substitute(line,'^\s*(\s*',"","")
            endif
            " 行尾有注释 先删去
            if line =~ '^.*--.*$'
                let line = substitute(line,'--.*$',"","")
            endif
            " 删掉行首的空格
            let line = substitute(line,'^\s*',"","")
            " 删掉行尾空格
            let line = substitute(line,'\s*$',"","")
            " 将信号按顺序存在list列表中
            if line =~ '^.*:\s*\<\%[in]\%[out]\>.*$'
                let port_t = substitute(line,'\s*:.*$',"","")
                let type_t =  substitute(line,'^.*:.*\(\<std_logic\%[_vector]\>\%(([^)]*)\)\?\)\s*',"\\1","")
                let direction_t = substitute(line,'^.*:\s*\(\<[inout]*\>\).*$',"\\1","")
                " 如果多个port在同一行
                if port_t =~ ','
                    let port_t = substitute(port_t,'\s\+',"","g")
                    let comma_pos = [-1]
                    let j = 1
                    while 1
                        let last_comma = stridx(port_t,",",comma_pos[j-1]+1)
                        call add(comma_pos,last_comma)
                        if comma_pos[j] == -1
                            break
                        endif
                        let j = j + 1
                    endwhile  
                    let k = 0
                    while k < j 
                        if k == j - 1
                            call add(s:port,strpart(port_t,comma_pos[k]+1))
                        else
                            call add(s:port,strpart(port_t,comma_pos[k]+1,comma_pos[k+1]-comma_pos[k]-1))
                        endif
                        call add(s:direction,direction_t)
                        call add(s:type,type_t)
                        let s:port_cout = s:port_cout + 1
                        let k = k + 1
                    endwhile
                else
                    " 将端口信息存于List中
                    call add(s:port,port_t)
                    call add(s:direction,direction_t)
                    call add(s:type,type_t)
                    let s:port_cout = s:port_cout + 1
                endif
            else 
                let i = i + 1
                continue
            endif
            let i = i + 1
        endwhile

    elseif HDL_Check_Filetype() == 2
        " 找到文件module
        let module_line = search('\(\/\/.*\)\@<!\<module\>','w')
        if module_line == 0
            echo "Can't Find The Module."
            return 0
        endif
        " 得到module的名字
        let line = getline(module_line)
        let s:ent_name = substitute(line,'\%(\/\/.*\)\@<!\<module\>\s*\(\<[a-zA-Z0-9_]*\>\).*$',"\\1","")
        " 寻找下一个出现的括号来找到端口列表的首行和尾行
        if search("(",'W') 
            let first_line = line('.')
            exe "normal %"
            let last_line = line('.')
        elseif
            return 0
        endif
        " 端口input，output等信息存于list--port_information中
        let port_information = []
        for line in getline(last_line,line('$'))
            if line =~ '^\s*//'
                continue
            elseif line =~ '\%(\/\/.*\)\@<!\<\%[in]\%[out]\%[put]\>'
                let line = substitute(line,' ^\s*\%(\/\/.*\)\@<!\(\<\%[in]\%[out]\%[put]\>.*\)\s*;\s*$',"\\1","")
                call add(port_information,line)
            endif
        endfor
        " 所有端口存于ports中
        let ports = ''
        for line in getline(first_line,last_line)
            let line = substitute(line,'^.*(\s*',"","")
            let line = substitute(line,'\s*)\_s*;.*$',"","")
            let ports = ports.line
        endfor

        " 去掉空格
        let ports = substitute(ports,'\s\+',"","g")
        " 得到ports中每个逗号的位置，并加入list--comma_pos
        let comma_pos = [-1]
        let j = 1
        while 1
            let last_comma = stridx(ports,",",comma_pos[j-1]+1)
            call add(comma_pos,last_comma)
            if comma_pos[j] == -1
                break
            endif
            let j = j + 1
        endwhile  
        " 将各个端口信息转成vhdl的方式存于list中
        let k = 0
        let s:port = []
        let s:direction = []
        let s:type = []
        let s:port_cout = 0
        " 端口名字port加入s:port中
        while k < j 
            if k == j - 1
                let port = strpart(ports,comma_pos[k]+1) 
            else
                let port = strpart(ports,comma_pos[k]+1,comma_pos[k+1]-comma_pos[k]-1)
            endif
            call add(s:port,port)
            " 在port_information中寻找port，如果找到，就将相应信息加入list
            let num = match(port_information,port)
            if num == -1
                echo "port ".port."is not define"
                return 0
            elseif port_information[num] =~ '\<input\>'
                call add(s:direction,"in")
            elseif port_information[num] =~ '\<output\>'
                call add(s:direction,"out")
            elseif port_information[num] =~ '\<inout\>'
                call add(s:direction,"inout")
            endif
            " 有长度信息的[x:y] 则转化成std_logic_vector(x downto y)存入s:type，如果没有则为std_logic
            let len_start = stridx(port_information[num],"[")
            if len_start != -1 
                let len_end = stridx(port_information[num],"]")
                let len = strpart(port_information[num],len_start,len_end-len_start+1)
                let type = HDL_Change2Vhdl(len)
                call add(s:type,type)
            else 
                call add(s:type,"std_logic")
            endif

            let s:port_cout = s:port_cout + 1
            let k = k + 1
        endwhile
        " 暂时不支持generic，设置generic_start_line = 0
        let s:generic_start_line = 0
    else 
        return 0
    endif
    " 跳转回刚刚标记的地方
    "echo s:port
    "echo s:direction
    "echo s:type
    exe "'s"
    return 1
endfunction

"------------------------------------------------------------------------
"Function    : Get generic information from the file 
"Decription  :  
"------------------------------------------------------------------------
function HDL_Generic_Port(start_line,last_line)
    " 设置3个List来存放端口的信息
    let s:generic_count = 0
    let s:generic_port = []
    let s:generic_type = []
    let s:generic_value = []
    let i = a:start_line
    while i <= a:last_line
        let line = getline(i)
        " 空格先删掉
        let line = substitute(line,'\s*',"","g")
        " 注释行跳过
        if line =~ '^--.*$'
            let i = i + 1
            continue
        endif
        " 将最后的;和最后一行的);去掉
        if i == a:last_line
            let line = substitute(line,');.*$',"","")
        else 
            let line = substitute(line,';.*$',"","")
        endif
        " generic和port在一行时删去generic(
        if line =~ '^\<generic\>(.*'
            let line = substitute(line,'\<generic\>(',"","")
        endif
        " (和port在一行时删去(
        if line =~ '^(.*$'
            let line = substitute(line,'(',"","")
        endif
        " 行尾有注释 应先删去
        if line =~ '^.*--.*$'
            let line = substitute(line,'--.*$',"","")
        endif
        
        let pos_1 = stridx(line,":")
        if pos_1 != -1
            let pos_2 = stridx(line,":=")
            let generic_port_t = strpart(line,0,pos_1)
            if pos_2 == -1 "没有初值的情况
                let generic_type_t = strpart(line,pos_1+1)
                let generic_value_t = ""
            else "有初值
                let generic_type_t = strpart(line,pos_1+1,pos_2-pos_1-1)
                let generic_value_t = strpart(line,pos_2+2)
            endif
            let s:generic_count = s:generic_count + 1
            call add(s:generic_port,generic_port_t)
            call add(s:generic_value,generic_value_t)
            call add(s:generic_type,generic_type_t)
        endif
        let i = i + 1
    endwhile
endfunction

"------------------------------------------------------------------------
"Function    : HDL_Check_Filetype()
"Decription  : Check file type 
"               if vhdl return 1
"               if verilog return 2
"               if vim return 3
"               others return 0
"------------------------------------------------------------------------
function HDL_Check_Filetype()
    if expand("%:e") == "vhd"
        return 1
    elseif expand("%:e") == "v" 
        return 2
    elseif expand("%:e") == "vim" 
        return 3
    else 
        return 0
    endif
endfunction

"-----------------------------------------------------------------------
"Function    : HDL_Change2vlog(port_tp) 
"Decription  : port_tp is std_logic_vector(x downto y)
"               return a string as [x:y] 
"------------------------------------------------------------------------
function HDL_Change2vlog(port_tp)
    if a:port_tp =~ '\<std_logic_vector\>'
        let mid = substitute(a:port_tp,'\<std_logic_vector\>\s*(',"","")
        if a:port_tp =~ '\<downto\>'
            let high_tp = substitute(mid,'\s*\<downto\>.*',"","")
            let low_tp = substitute(mid,'.*\<downto\>\s*',"","")
            let low_tp = substitute(low_tp,'\s*).*',"","")
        elseif a:port_tp =~ '\<to\>'
            let high_tp = substitute(mid,'\s*\<to\>.*',"","")
            let low_tp = substitute(mid,'.*\<to\>\s*',"","")
            let low_tp = substitute(low_tp,'\s*).*',"","")
        else 
            return "Wrong"
        endif
        let vlog_tp = "[".high_tp.":".low_tp."]"
    else 
        return "Wrong"
    endif
    return vlog_tp
endfunction

"-------------------------------------------------------------------------------
" Function		: HDL_Change2Vhdl(port_tp)	
" Description	: port_tp is [x:y]	
"                   return a string as std_logic_vector(x downto y)
"-------------------------------------------------------------------------------
function HDL_Change2Vhdl(port_tp)
    let port_tp = substitute(a:port_tp,'\s*',"","g")
    let colon = stridx(port_tp,":")
    let high_tp = strpart(port_tp,1,colon-1)
    let low_tp = strpart(port_tp,colon+1,strlen(port_tp)-colon-2)
"    echo "high_tp= ".high_tp
"    echo "low_tp= ".low_tp
    if high_tp > low_tp
        let vhdl_tp = "std_logic_vector(".high_tp." downto ".low_tp.")"
    else 
        let vhdl_tp = "std_logic_vector(".high_tp." to ".low_tp.")"
    endif
    return vhdl_tp
endfunction


"------------------------------------------------------------------------
"Function    : HDL_Component_Part(lang)
"Decription  : build component part
"------------------------------------------------------------------------
function HDL_Component_Part(lang)
    if a:lang == "vhdl"
        let component_part = "\tcomponent ".s:ent_name." is\n"
        if s:generic_start_line != 0
            let component_part = component_part."\t\tgeneric(\n"
            let i = 0
            while i < s:generic_count 
                if strwidth(s:generic_port[i])<4
                    let component_part = component_part."\t\t\t\t".s:generic_port[i]
                                \."\t\t\t\t: ".s:generic_type[i]."\t"
                elseif strwidth(s:generic_port[i])<8 && strwidth(s:generic_port[i])>=4
                    let component_part = component_part."\t\t\t\t".s:generic_port[i]
                                \."\t\t\t: ".s:generic_type[i]."\t"
                elseif strwidth(s:generic_port[i])<12 && strwidth(s:generic_port[i])>=8
                    let component_part = component_part."\t\t\t\t".s:generic_port[i]
                                \."\t\t: ".s:generic_type[i]."\t"
                elseif strwidth(s:generic_port[i])<16 && strwidth(s:generic_port[i])>=12
                    let component_part = component_part."\t\t\t\t".s:generic_port[i]
                                \."\t: ".s:generic_type[i]."\t"
                elseif strwidth(s:generic_port[i])>=16
                    let component_part = component_part."\t\t\t\t".s:generic_port[i]
                                \.": ".s:generic_type[i]."\t"
                endif
                if s:generic_value[i] != ""
                    let component_part = component_part.":= ".s:generic_value[i]
                endif
                if i != s:generic_count - 1
                    let component_part = component_part.";\n"
                else
                    let component_part = component_part."\n\t);\n"
                endif
                let i = i + 1
            endwhile
        endif
        let component_part = component_part."\t\tport(\n"
        let i = 0
        while i < s:port_cout
            if strwidth(s:port[i])<4 
                let component_part = component_part."\t\t\t\t".s:port[i]."\t\t\t\t: ".s:direction[i]."\t".s:type[i]
            elseif strwidth(s:port[i])<8 && strwidth(s:port[i])>=4
                let component_part = component_part."\t\t\t\t".s:port[i]."\t\t\t: ".s:direction[i]."\t".s:type[i]
            elseif strwidth(s:port[i])<12 && strwidth(s:port[i])>=8
                let component_part = component_part."\t\t\t\t".s:port[i]."\t\t: ".s:direction[i]."\t".s:type[i]
            elseif strwidth(s:port[i])>=12 && strwidth(s:port[i])<16
                let component_part = component_part."\t\t\t\t".s:port[i]."\t: ".s:direction[i]."\t".s:type[i]
            elseif strwidth(s:port[i])>=16 
                let component_part = component_part."\t\t\t\t".s:port[i].": ".s:direction[i]."\t".s:type[i]
            endif
            if i != s:port_cout - 1
                let component_part = component_part.";\n"
            else
                let component_part = component_part."\n\t);\n\tend component;\n\n"
            endif
            let i = i +1
        endwhile
        return component_part
    elseif a:lang == "verilog"
        return ''
    else 
        return ''
    endif
endfunction

"------------------------------------------------------------------------
"Function    : HDL_Instant_Part(lang)
"Decription  : build instant_part 
"------------------------------------------------------------------------
function HDL_Instant_Part(lang)
    if a:lang == "vhdl"
        let instant_part = "\t".s:ent_name."_inst : ".s:ent_name."\n"
        if s:generic_start_line != 0
            let instant_part = instant_part."\tgeneric map(\n"
            let i = 0
            while i < s:generic_count 
                if strwidth(s:generic_port[i])<4
                    let instant_part = instant_part."\t\t\t\t".s:generic_port[i]."\t\t\t\t=> "
                elseif strwidth(s:generic_port[i])<8 && strwidth(s:generic_port[i])>=4
                    let instant_part = instant_part."\t\t\t\t".s:generic_port[i]."\t\t\t=> "
                elseif strwidth(s:generic_port[i])<12 && strwidth(s:generic_port[i])>=8
                    let instant_part = instant_part."\t\t\t\t".s:generic_port[i]."\t\t=> "
                elseif strwidth(s:generic_port[i])<16 && strwidth(s:generic_port[i])>=12
                    let instant_part = instant_part."\t\t\t\t".s:generic_port[i]."\t=> "
                elseif strwidth(s:generic_port[i])>=16
                    let instant_part = instant_part."\t\t\t\t".s:generic_port[i]."=> "
                endif
                if s:generic_value[i] != ""
                    let instant_part = instant_part.s:generic_value[i]
                else
                    let instant_part = instant_part.s:generic_port[i]
                endif
                if i != s:generic_count - 1
                    let instant_part = instant_part.",\n"
                else
                    let instant_part = instant_part."\n\t)\n"
                endif
                let i = i + 1
            endwhile
        endif
        let instant_part = instant_part."\tport map(\n"
        let i = 0
        while i < s:port_cout 
            if strwidth(s:port[i])<4
                let instant_part = instant_part."\t\t\t\t".s:port[i]."\t\t\t\t=>\t".s:port[i]
            elseif strwidth(s:port[i])<8 && strwidth(s:port[i])>=4
                let instant_part = instant_part."\t\t\t\t".s:port[i]."\t\t\t=>\t".s:port[i]
            elseif strwidth(s:port[i])>=8 && strwidth(s:port[i])<12
                let instant_part = instant_part."\t\t\t\t".s:port[i]."\t\t=>\t".s:port[i]
            elseif strwidth(s:port[i])>=12 && strwidth(s:port[i])<16
                let instant_part = instant_part."\t\t\t\t".s:port[i]."\t=>\t".s:port[i]
            else 
                let instant_part = instant_part."\t\t\t\t".s:port[i]."=>\t".s:port[i]
            endif
            if i != s:port_cout -1 
                let instant_part = instant_part.",\n"
            else 
                let instant_part = instant_part."\n\t);\n\n"
            endif
            let i = i + 1
        endwhile
    elseif a:lang == "verilog"
        let instant_part = s:ent_name."\t"
        if s:generic_start_line != 0
            let i = 0
            let instant_part = instant_part."#(\n"
            let parameter = ""
            while i < s:generic_count
                if s:generic_value[i] != ""
                    let parameter = parameter."parameter\t".s:generic_port[i]." = ".s:generic_value[i].";\n"
                else 
                    let parameter = parameter."parameter\t".s:generic_port[i]." = //Add value;\n"
                endif
                if strwidth(s:generic_port[i])<3
                    let instant_part = instant_part."\t.".s:generic_port[i]."\t\t\t\t(".s:generic_port[i].")"
                elseif strwidth(s:generic_port[i])<7 && strwidth(s:generic_port[i])>=3
                    let instant_part = instant_part."\t.".s:generic_port[i]."\t\t\t(".s:generic_port[i].")"
                elseif strwidth(s:generic_port[i])<11 && strwidth(s:generic_port[i])>=7
                    let instant_part = instant_part."\t.".s:generic_port[i]."\t\t(".s:generic_port[i].")"
                elseif strwidth(s:generic_port[i])<15 && strwidth(s:generic_port[i])>=11
                    let instant_part = instant_part."\t.".s:generic_port[i]."\t(".s:generic_port[i].")"
                else
                    let instant_part = instant_part."\t.".s:generic_port[i]."(".s:generic_port[i].")"
                endif
                if i != s:generic_count - 1
                    let instant_part = instant_part.",\n"
                else 
                    let instant_part = instant_part."\n)\n"
                endif
                let i = i + 1
            endwhile
            let instant_part = parameter."\n".instant_part
        endif
        let instant_part = instant_part.s:ent_name."(\n"
        let i = 0
        while i < s:port_cout
            if strwidth(s:port[i])<3
                let instant_part = instant_part."\t.".s:port[i]."\t\t\t\t(".s:port[i]
            elseif strwidth(s:port[i])<7 && strwidth(s:port[i])>=3
                let instant_part = instant_part."\t.".s:port[i]."\t\t\t(".s:port[i]
            elseif strwidth(s:port[i])>=7 && strwidth(s:port[i])<11
                let instant_part = instant_part."\t.".s:port[i]."\t\t(".s:port[i]
            elseif strwidth(s:port[i])>=11 && strwidth(s:port[i]) <15
                let instant_part = instant_part."\t.".s:port[i]."\t(".s:port[i]
            else
                let instant_part = instant_part."\t.".s:port[i]."(".s:port[i]
            endif
            if i != s:port_cout - 1
                let instant_part = instant_part."),\n"
            else 
                let instant_part = instant_part.")\n);\n\n"
            endif
            let i = i + 1
        endwhile
    elseif
        return ''
    endif
    return instant_part
endfunction

"------------------------------------------------------------------------
"Function    : HDL_Inport_Part(lang) 
"Decription  : inport part 
"------------------------------------------------------------------------
function HDL_Inport_Part(lang)
    if a:lang == "vhdl"
        let inport_part = "\t-- Inputs\n"
        let i = 0 
        while i < s:port_cout 
            if s:direction[i] == "in"
                if strwidth(s:port[i])<4
                    let inport_part = inport_part."\tsignal\t".s:port[i]."\t\t\t\t: ".s:type[i]
                elseif strwidth(s:port[i])<8 && strwidth(s:port[i])>=4
                    let inport_part = inport_part."\tsignal\t".s:port[i]."\t\t\t: ".s:type[i]
                elseif strwidth(s:port[i])>=8 && strwidth(s:port[i])<12
                    let inport_part = inport_part."\tsignal\t".s:port[i]."\t\t: ".s:type[i]
                elseif strwidth(s:port[i])>=12 && strwidth(s:port[i])<16 
                    let inport_part = inport_part."\tsignal\t".s:port[i]."\t: ".s:type[i]
                elseif strwidth(s:port[i])>=16
                    let inport_part = inport_part."\tsignal\t".s:port[i].": ".s:type[i]
                endif
                if s:type[i] =~ '\<std_logic_vector\>'
                    let inport_part = inport_part.":=(others=>'0');\n"
                else
                    let inport_part = inport_part.":='0';\n"
                endif
            endif
            let i = i + 1
        endwhile   
        if inport_part == "\t-- Inputs\n"
            let inport_part = ''
        else 
            let inport_part = inport_part."\n"
        endif
    elseif a:lang == "verilog"
        let inport_part = "// Inputs\n"
        let i = 0
        while i < s:port_cout 
            if s:direction[i] == "in"
                if s:type[i] =~ '\<std_logic_vector\>'
                    let inport_part = inport_part."reg\t\t".HDL_Change2vlog(s:type[i])."\t".s:port[i].";\n"
                else 
                    let inport_part = inport_part."reg\t\t\t\t".s:port[i].";\n"
                endif
            endif
            let i = i + 1
        endwhile
        if inport_part == "// Inputs\n"
            let inport_part = ''
        else 
            let inport_part = inport_part."\n"
        endif
    else 
        return ''
    endif
    return inport_part
endfunction

"------------------------------------------------------------------------
"Function    : HDL_Outport_Part(lang) 
"Decription  : outport part 
"------------------------------------------------------------------------
function HDL_Outport_Part(lang)
    if a:lang == "vhdl"
        let outport_part = "\t-- Outputs\n"
        let i = 0 
        while i < s:port_cout 
            if s:direction[i] == "out"
                if strwidth(s:port[i])<4
                    let outport_part = outport_part."\tsignal\t".s:port[i]."\t\t\t\t: ".s:type[i]
                elseif strwidth(s:port[i])<8 && strwidth(s:port[i])>=4
                    let outport_part = outport_part."\tsignal\t".s:port[i]."\t\t\t: ".s:type[i]
                elseif strwidth(s:port[i])>=8 && strwidth(s:port[i])<12
                    let outport_part = outport_part."\tsignal\t".s:port[i]."\t\t: ".s:type[i]
                elseif strwidth(s:port[i])>=12  && strwidth(s:port[i])<16
                    let outport_part = outport_part."\tsignal\t".s:port[i]."\t: ".s:type[i]
                elseif strwidth(s:port[i])>=16
                    let outport_part = outport_part."\tsignal\t".s:port[i].": ".s:type[i]
                endif
                let outport_part = outport_part.";\n"
            endif
            let i = i + 1
        endwhile   
        if outport_part == "\t-- Outputs\n"
            let outport_part = ''
        else 
            let outport_part = outport_part."\n"
        endif
    elseif a:lang == "verilog"
        let outport_part = "// Outputs\n"
        let i = 0
        while i < s:port_cout 
            if s:direction[i] == "out"
                if s:type[i] =~ '\<std_logic_vector\>'
                    let outport_part = outport_part."wire\t".HDL_Change2vlog(s:type[i])."\t".s:port[i].";\n"
                else 
                    let outport_part = outport_part."wire\t\t\t".s:port[i].";\n"
                endif
            endif
            let i = i + 1
        endwhile
        if outport_part == "// Outputs\n"
            let outport_part = ''
        else 
            let outport_part = outport_part."\n"
        endif
    else 
        return ''
    endif
    return outport_part
endfunction
"
"------------------------------------------------------------------------
"Function    : HDL_Inoutport_Part(lang) 
"Decription  : inoutport part 
"------------------------------------------------------------------------
function HDL_Inoutport_Part(lang)
    if a:lang == "vhdl"
        let inoutport_part = "\t-- Inout\n"
        let i = 0 
        while i < s:port_cout 
            if s:direction[i] == "inout"
                if strwidth(s:port[i])<4
                    let inoutport_part = inoutport_part."\tsignal\t".s:port[i]."\t\t\t\t: ".s:type[i]
                elseif strwidth(s:port[i])<8 && strwidth(s:port[i])>=4
                    let inoutport_part = inoutport_part."\tsignal\t".s:port[i]."\t\t\t: ".s:type[i]
                elseif strwidth(s:port[i])>=8 && strwidth(s:port[i])<12
                    let inoutport_part = inoutport_part."\tsignal\t".s:port[i]."\t\t: ".s:type[i]
                elseif strwidth(s:port[i])>=12  && strwidth(s:port[i])<16
                    let inoutport_part = inoutport_part."\tsignal\t".s:port[i]."\t: ".s:type[i]
                elseif strwidth(s:port[i])>=16
                    let inoutport_part = inoutport_part."\tsignal\t".s:port[i].": ".s:type[i]
                endif
                let inoutport_part = inoutport_part.";\n"
            endif
            let i = i + 1
        endwhile   
        if inoutport_part == "\t-- Inout\n"
            let inoutport_part = ''
        else 
            let inoutport_part = inoutport_part."\n"
        endif
    elseif a:lang == "verilog"
        let inoutport_part = "// Inout\n"
        let i = 0
        while i < s:port_cout 
            if s:direction[i] == "inout"
                if s:type[i] =~ '\<std_logic_vector\>'
                    let inoutport_part = inoutport_part."wire\t".HDL_Change2vlog(s:type[i])."\t".s:port[i].";\n"
                else 
                    let inoutport_part = inoutport_part."wire\t\t\t".s:port[i].";\n"
                endif
            endif
            let i = i + 1
        endwhile
        if inoutport_part == "// Inout\n"
            let inoutport_part = ''
        else 
            let inoutport_part = inoutport_part."\n"
        endif
    else 
        return ''
    endif
    return inoutport_part
endfunction

"------------------------------------------------------------------------------
"Function  : HDL_Component_Build() 
"Arguments : Open a new window and put component information on it ;
"            The information also put in the register *.
"------------------------------------------------------------------------------
function HDL_Component_Build(type)
    if a:type == ''
        echo "You haven't set \"type\""
        return
    endif
"    get information of the entity
    if !HDL_Entity_Information() 
        echo "Can't Get the information"
        return
    endif
"    build the component information
    if a:type == "vhdl"
        let component_part = HDL_Component_Part("vhdl")
    elseif a:type == "verilog"
        let component_part = ''
    endif
    let inport_part = HDL_Inport_Part(a:type)
    let outport_part = HDL_Outport_Part(a:type)
    let inoutport_part = HDL_Inoutport_Part(a:type)
    let instant_part = HDL_Instant_Part(a:type)
    let all_part = component_part.inport_part.outport_part.inoutport_part.instant_part
    " let @+ = all_part
    let @* = all_part
    "build component window
    let sp_op = ''
    if exists('g:HDL_RightB_Commponent')
        if g:HDL_RightB_Commponent
            let sp_op = "rightbelow vertical "
        else 
            let sp_op = "vertical "
        endif
    endif
    exe sp_op."split __Instant_File__"
    if sp_op == ''
        exe "resize ".g:HDL_Height_of_Component
    else
        exe "vertical resize ".g:HDL_Width_of_Component
    endif
    silent put! =all_part
    exe "normal gg"
    setlocal noswapfile
    setlocal buftype=nofile
    setlocal bufhidden=delete
    if a:type == "vhdl"
        setlocal filetype=vhdl
    elseif a:type == "verilog"
        setlocal filetype=verilog
    endif
endfunction


"-----------------------------------------------------------------------
"Function    : HDL_Tb_Build() 
"Decription  :  
"------------------------------------------------------------------------
function HDL_Tb_Build(type)
    if a:type == ''
        echo "Do not set \"type\""
        return
    endif
"  Check the file type
    if !HDL_Check_Filetype()
        echohl ErrorMsg
        echo    "This file type is not supported!"
        echohl None
        return
    endif
"    get information of the entity
    if !HDL_Entity_Information() 
        echo "Can't Get the information"
        return
    endif
    if !exists('clk')
        let clk = "clk"
    endif
    if !exists('rst')
        let rst = "rst"
    endif
"    file name and entity name 
    let tb_ent_name = "tb_".s:ent_name
    if a:type == "vhdl"
        let tb_file_name = "tb_".s:ent_name.".vhd"
        let entity_part = "entity ".tb_ent_name." is\nend ".tb_ent_name.";\n\n"
        let architecture_part = "architecture behavior of ".tb_ent_name.
                    \" is\n\n\t-- Component Declaration for the Unit Under Test (UUT)\n"
        let constant_part = "\t-- Clock period definitions\n\tconstant clk_period : time := ".g:HDL_Clock_Period
                    \." ns;\n\nbegin\n\n"
        let clock_part = "\t-- Clock process definitions\n\tprocess\n\tbegin\n\t\t".clk
                    \." <= '0';\n\t\twait for clk_period/2;\n\t\t".clk." <= '1';\n"
                    \."\t\twait for clk_period/2;\n\tend process;\n\n"
        let simulus_part = "\t-- Stimulus process\n\tprocess\n\tbegin\n\t\t-- hold reset state for 100 ns\n"
                    \."\t\twait for 100 ns;\n\t\trst <= '0';\n\n\t\twait for 10000 ns;\n\n"
                    \."\t\t-- Add stimulus here\n\n\t\twait;\n\tend process;\n\nend behavior;\n"
    elseif a:type == "verilog"
        let tb_file_name = "tb_".s:ent_name.".v"
        let entity_part = ''
        let architecture_part = "module ".tb_ent_name.";\n\n"
        let constant_part = ''
        let half_clk = g:HDL_Clock_Period/2
        let clock_part = "// Clock generate \nalways # ".half_clk."\t".clk." <= ~".clk.";\n\n"
        let simulus_part = "initial begin\n\t// Initialize Inputs\n"
        let i = 0
        while i < s:port_cout
            if s:direction[i] == "in"
                let simulus_part = simulus_part."\t\t".s:port[i]." = 0;\n"
            endif
            let i = i + 1
        endwhile
        let simulus_part = simulus_part."\n\t// Wait 100 ns for global reset to finish\n"
                    \."\t#100;\n\trst = 0;\n\n\t// Add stimulus here\n\n\t#10000;\n"
                    \."\t$stop;\n\nend\n\nendmodule\n"
    endif
     "    component part
    let component_part = HDL_Component_Part(a:type)
    let inport_part = HDL_Inport_Part(a:type)
    let outport_part = HDL_Outport_Part(a:type)
    let inoutport_part = HDL_Inoutport_Part(a:type)
    let instant_part = HDL_Instant_Part(a:type)
    let all_part = entity_part.architecture_part.component_part.inport_part.outport_part
                \.inoutport_part.constant_part.instant_part.clock_part.simulus_part
"    检测文件是否已经存在 
    if filewritable(tb_file_name) 
        let choice = confirm("The testbench file has been exist.\nSelect \"Open\" to open existed file.".
                    \"\nSelect \"Change\" to replace it.\nSelect \"Cancel\" to Cancel this operation.",
                    \"&Open\nCh&ange\n&Cancel")
        if choice == 0
            echo "\"Create a Testbench file\" be Canceled!"
            return
        elseif choice == 1
            exe "bel sp ".tb_file_name
            return
        elseif choice == 2
            if delete(tb_file_name) 
                echohl ErrorMsg
                echo    "The testbench file already exists.But now can't Delete it!"
                echohl None
                return
            else 
                echo "The testbench file already exists.Delete it and recreat a new one!"
            endif
        else 
            echo "\"Create a Testbench file\" be Canceled!"
            return
        endif
    endif
    exe "bel sp ".tb_file_name
    silent put! =all_part
    exe "AddInfo"
    if search('\<rst\>.*=') != 0
        exe "normal f0r1"
    endif
    exe "up"
    call search("Add stimulus here")
endfunction

"------------------------------------------------------------------------------
"Function    : HDL_Last_Modified() 
"Description : Add modifiled time to the file's annotation  
"------------------------------------------------------------------------------
function HDL_Last_Modified()
    let l = line("$")
    execute "1," . l . "g/Last Modified\t:/s/Last Modified\t:.*/Last Modified\t: " .
        \ strftime("%Y-%m-%d %H:%M")
endfunction
autocmd BufWritePre,FileWritePre *.vhd   ks|call HDL_Last_Modified()|'s
autocmd BufWritePre,FileWritePre *.v   ks|call HDL_Last_Modified()|'s

"------------------------------------------------------------------------------
"Function    : HDL_Auto_Close_Component() 
"Description : Auto Close the Component file when close the vhd file 
"------------------------------------------------------------------------------
function HDL_Auto_Close_Component()
    if bufloaded("__Instant_File__") 
        if bufloaded(g:TagList_title)
            exe "bdelete! __Instant_File__" 
        else
            exe "bdelete! __Instant_File__"
            exe "q!"
        endif
    endif 
endfunction
autocmd BufUnload   *.vhd,*.v call HDL_Auto_Close_Component() 

"-------------------------------------------------------------------------------
"   FORMAT FUNCTION START  
"-------------------------------------------------------------------------------
"-------------------------------------------------------------------------------
" Function		: HDL_Component_Format(start_line,end_line)	
" Description	: format the component part	
"-------------------------------------------------------------------------------
function HDL_Component_Format(start_line,end_line)
    let end_line = a:end_line
    let curs = a:start_line
    let flag = 0
    while curs <= end_line
        if getline(curs) =~ '\<generic\>'
            let flag = 1
        endif
        if getline(curs) =~ ':\s*\<in\>' || getline(curs) =~ ':\s*\<out\>' || getline(curs) =~ ':\s*\<inout\>'
            if getline(curs) =~ '^\s*--'
                let curs = curs + 1
                continue
            endif
            call cursor(curs,1)
            if getline(curs) =~ '^[^ ].*$'
                exe "s/^/\t/"
                normal 0
            endif
            normal wyiw
            if @0 == "port"
                exe "s/\\<port\\>\\s*(/port(\\r\t/"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            elseif @0 == "("
                exe "s/^\\s*(/\t(\r\t"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            endif
            let port_name = @0
            normal wwyiw
            let port_direction = @0
            normal 0dw
            if strwidth(port_name)<4
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t\t\t\t: ".port_direction."\t/"
            elseif strwidth(port_name)<8 && strwidth(port_name)>=4
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t\t\t: ".port_direction."\t/"
            elseif strwidth(port_name)<12 && strwidth(port_name)>=8
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t\t: ".port_direction."\t/"
            elseif strwidth(port_name)<16 && strwidth(port_name)>=12
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t: ".port_direction."\t/"
            elseif strwidth(port_name)>=16
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name.": ".port_direction."\t/"
            endif
        elseif getline(curs) =~ ':' && flag == 1
             if getline(curs) =~ '^\s*--'
                let curs = curs + 1
                continue
            endif
            call cursor(curs,1)
            if getline(curs) =~ '^[^ ].*$'
                exe "s/^/\t/"
                normal 0
            endif
            normal wyiw
            if @0 == "generic"
                exe "s/\\<generic\\>\\s*(/generic(\\r\t/"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            elseif @0 == "("
"                exe "s/^\\s*(/\t(\r\t"
                normal kJ
                let end_line = end_line - 1
                let curs = curs - 1
                continue
            endif
            let port_name = @0
            normal 0dw
            if strwidth(port_name)<4
                exe "s/\\<".port_name."\\>\\s*:\\s*/".port_name."\t\t\t\t:\t/"
            elseif strwidth(port_name)<8 && strwidth(port_name)>=4
                exe "s/\\<".port_name."\\>\\s*:\\s*/".port_name."\t\t\t:\t/"
            elseif strwidth(port_name)<12 && strwidth(port_name)>=8
                exe "s/\\<".port_name."\\>\\s*:\\s*/".port_name."\t\t:\t/"
            elseif strwidth(port_name)<16 && strwidth(port_name)>=12
                exe "s/\\<".port_name."\\>\\s*:\\s*/".port_name."\t:\t/"
            elseif strwidth(port_name)>=16
                exe "s/\\<".port_name."\\>\\s*:\\s*/".port_name.":\t/"
            endif
        endif
        let curs = curs + 1
    endwhile
    return end_line
endfunction

"-------------------------------------------------------------------------------
" Function		: HDL_Entity_Format(start_line,end_line)	
" Description	: format entity part	
"-------------------------------------------------------------------------------
function HDL_Entity_Format(start_line,end_line)
    let end_line = a:end_line
    let curs = a:start_line
    while curs <= end_line
        if getline(curs) =~ ':\s*\<in\>' || getline(curs) =~ ':\s*\<out\>' || getline(curs) =~ ':\s*\<inout\>'
            if getline(curs) =~ '^\s*--'
                let curs = curs + 1
                continue
            endif
            call cursor(curs,1)
            if getline(curs) =~ '^[^ ].*$'
                exe "s/^/\t/"
                normal 0
            endif
            normal wyiw
            if @0 == "port"
                exe "s/\\<port\\>\s*(/port(\\r\t/"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            elseif @0 == "("
                exe "s/^\\s*(/\t(\r\t"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            endif
            let port_name = @0
            normal wwyiw
            let port_direction = @0
            normal 0dw
            if strwidth(port_name)<4
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t\t\t\t: ".port_direction."\t/"
            elseif strwidth(port_name)<8 && strwidth(port_name)>=4
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t\t\t: ".port_direction."\t/"
            elseif strwidth(port_name)<12 && strwidth(port_name)>=8
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t\t: ".port_direction."\t/"
            elseif strwidth(port_name)<16 && strwidth(port_name)>=12
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name."\t: ".port_direction."\t/"
            elseif strwidth(port_name)>=16
                exe "s/\\<".port_name."\\>\\s*:\\s*\\<".port_direction."\\>\\s\\+/"
                            \.port_name.": ".port_direction."\t/"
            endif
        endif
        let curs = curs + 1
    endwhile
    return end_line
endfunction

"-------------------------------------------------------------------------------
" Function		: HDL_Instant_Format(start_line,end_line)	
" Description	: format the instant part 	
"-------------------------------------------------------------------------------
function HDL_Instant_Format(start_line,end_line)
    let end_line = a:end_line
    let curs = a:start_line
    while curs <= end_line
        if getline(curs) =~ '=>'
            if getline(curs) =~ '^\s*--'
                let curs = curs + 1
                continue
            endif
"            if getline(curs) =~ '^\s*$'
"                let curs = curs + 1
"                continue
"            endif
            call cursor(curs,1)
            if getline(curs) =~ '^[^ ].*$'
                exe "s/^/\t/"
                normal 0
            endif
            normal wyiw
            if @0 == "port"
                exe "s/\\<port\\>\\s\\+\\<map\\>\\s*(/port map(\\r\t/"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            elseif @0 == "generic"
                exe "s/\\<generic\\>\\s\\+\\<map\\>\\s*(/generic map(\\r\t/"
                let end_line = end_line + 1
                let curs = curs + 1
                continue
            elseif @0 == "map"
                normal kJ
"                exe "s/\\<port\\>\\s\\+\\<map\\>\s*(/port map(\\r\t/"
"                exe "normal 0|s/\\<port\\>\s\+\\<map\\>\s*(/port map(\\r\t/"
                let end_line = end_line - 1
                let curs = curs - 1
                continue
"                let curs = curs + 1
"                continue
            elseif @0 == "("
                normal kJ
"                exe "normal J|s/\\<port\\>\\s\\+\\<map\\>\s*(/port map(\\r\t/"
"                exe "normal 0|s/\\<port\\>\s\+\\<map\\>\s*(/port map(\\r\t/"
                let end_line = end_line - 1
                let curs = curs - 1
                continue
"                let curs = curs + 1
"                continue
            endif
            let port = @0
"            normal wwyiw
"            let connect_port = @0
            normal 0dw
            if strwidth(port)<4
"                exe "s/^".port."\\s*=>\\s*".connect_port."/".port."\t\t\t\t=> ".connect_port."/"
                exe "s/^".port."\\s*=>\\s*/".port."\t\t\t\t=> /"
            elseif strwidth(port)>=4 && strwidth(port)<8
"                exe "s/^".port."\\s*=>\\s*".connect_port."/".port."\t\t\t=> ".connect_port."/"
                exe "s/^".port."\\s*=>\\s*/".port."\t\t\t=> /"
            elseif strwidth(port)>=8 && strwidth(port)<12
"                exe "s/^".port."\\s*=>\\s*".connect_port."/".port."\t\t=> ".connect_port."/"
                exe "s/^".port."\\s*=>\\s*/".port."\t\t=> /"
            elseif strwidth(port)>=12 && strwidth(port)<16
"                exe "s/^".port."\\s*=>\\s*".connect_port."/".port."\t=> ".connect_port."/"
                exe "s/^".port."\\s*=>\\s*/".port."\t=> /"
            elseif strwidth(port)>=16 
                exe "s/^".port."\\s*=>\\s*/".port."=> /"
            endif
        endif
        let curs = curs + 1
    endwhile
    return end_line
endfunction
        
"-------------------------------------------------------------------------------
" Function		: HDL_Signal_Format()
" Description	: format signal 	
"-------------------------------------------------------------------------------
function HDL_Signal_Format()
    normal 0
    exe "s/^/\t/"
    normal wyiw
    let signal_name = @0
    if strwidth(signal_name)<4
        exe "s/^\\s*\\<signal\\>\\s*".signal_name."\\s*:\\s*/signal\t".signal_name."\t\t\t\t: /"
    elseif strwidth(signal_name)>=4 && strwidth(signal_name)<8
        exe "s/^\\s*\\<signal\\>\\s*".signal_name."\\s*:\\s*/signal\t".signal_name."\t\t\t: /"
    elseif strwidth(signal_name)>=8 && strwidth(signal_name)<12
        exe "s/^\\s*\\<signal\\>\\s*".signal_name."\\s*:\\s*/signal\t".signal_name."\t\t: /"
    elseif strwidth(signal_name)>=12 && strwidth(signal_name)<16
        exe "s/^\\s*\\<signal\\>\\s*".signal_name."\\s*:\\s*/signal\t".signal_name."\t: /"
    elseif strwidth(signal_name)>=16 
        exe "s/^\\s*\\<signal\\>\\s*".signal_name."\\s*:\\s*/signal\t".signal_name.": /"
    endif
endfunction

"-------------------------------------------------------------------------------
" Function		: HDL_Assign_Format()	
" Description	: format assign 	
"-------------------------------------------------------------------------------
function HDL_Assign_Format()
    if getline('.') =~ '^\s*--'
        return
    else
        exe "s/\\([a-zA-Z0-9_]*\\)\\s*<=\\s*\\(\\S*\\)/\\1\t<= \\2/"
    endif
endfunction

"-------------------------------------------------------------------------------
" Function		: HDL_Vhdl_Format()	
" Description	: Flie Format 	
"-------------------------------------------------------------------------------
function HDL_Vhdl_Format()
    "Check if be compile 
    exe "ModSimComp"
    let qfix = getqflist()
    for q_valid in getqflist()
        if q_valid.valid != 0
            return
        endif
    endfor
    "mark current position
    exe "ks"
    set ignorecase
    if search('\<port\>\_s\+(','w') > 0
        exe "%s/\\<port\\>\\_s\\+(/port(/g"
    endif
    if search('\<map\>\_s\+(','w') > 0
        exe "%s/\\<map\\>\\s\\+(/map(/g"
    endif
    if search('\<generic\>\_s\+(','w') > 0
        exe "%s/\\<generic\\>\\s\\+(/generic(/g"
    endif
    "entity part 
    let start_line = search('\(--.*\)\@<!\<entity\>\s\+[a-zA-Z0-9_]*\s\+\<is\>','w')
    let end_line = search('\(--.*\)\@<!\<entity\>\s\+\([a-zA-Z0-9_]*\)\_.\{-}\zs\(--.*\)\@<!\<end\>\s\+\1','W')
    let end_line = HDL_Entity_Format(start_line,end_line)
    "component part 
    let start_line = search('\(--.*\)\@<!\zs\<component\>','W')
    let com_name = []
    let ins_num = 0
    if start_line != 0
        normal wyiw
        call add(com_name,@0)
    endif
    while start_line
        let end_line = search('\<end\>\s\+\<component\>','W')
        let end_line = HDL_Component_Format(start_line,end_line)
        call cursor(end_line,1)
        let start_line = search('\s*\<component\>\s*;\_.\{-}\zs\<component\>','W')
        if start_line != 0
            normal wyiw
            call add(com_name,@0)
            let ins_num = ins_num + 1
        endif
    endwhile
    "instant part 
    let i = 0
    call search('\<begin\>','w')
    let flag = 'w'
    while i <= ins_num
        if ins_num == 0
            break
        endif
        let start_line = search(':\s*\<'.com_name[i].'\>\_s\{-}\<generic\>\_s\{-}\<map\>\_s\{-}\zs(',flag)
        if start_line != 0
            let flag = 'W'
            normal %
            let end_line = line('.')
            let end_line = HDL_Instant_Format(start_line,end_line)
            let start_line = search('\<port\>\_s\{-}\<map\>\_s\{-}\zs(','W')
            normal %
            let end_line = line('.')
        else
            let start_line = search(':\s*\<'.com_name[i].'\>\_s\{-}\<port\>\_s\{-}\<map\>\_s\{-}\zs(',flag)
            if start_line != 0
                let flag = 'W'
            else 
                let flag = 'w'
                let i = i + 1
                continue
            endif
            normal %
            let end_line = line('.')
        endif
        let end_line = HDL_Instant_Format(start_line,end_line)
    endwhile
    "signal part 
    normal gg
    while search('^\s*\<signal\>\s*[a-zA-Z0-9_]*\s*:\s*.*;.*$','W')
        call HDL_Signal_Format()
    endwhile
    "assign part
    normal gg
    while search('^\s*[a-zA-Z0-9_]*\s*<=\s*\S*','W')
        call HDL_Assign_Format()
    endwhile
    "all
    echo "Formating...please wait..."
    normal gg=G=G
    echo "Format....Done"
    exe "'s"
endfunction
"-------------------------------------------------------------------------------
" Function		: HDL_Signal_Dec_Vhdl()	
" Description	: I modified from a plugin by sunil shukla
"                 author : sunil shukla
"                 email: sunilkshukla@gmail.com
"-------------------------------------------------------------------------------
function HDL_Signal_Dec_Vhdl()
    normal yiw
    normal ma 
    let curr_pos = line(".")  
    " let curr_word= getreg('"0') 
    let curr_word= @0
    " checks for the begin keyword			
    normal gg
    set ignorecase 
    let flags = "W"
    let process_line = search('\%^\_.\{-}\zs\(--.*\)\@<!\<process\>','w')
    if process_line != 0
        let flags = 'bW'
    endif
    let check_for_validity = search('^\s*\<begin\>\s*$', flags)	
    normal mb
    if check_for_validity == 0
        normal `a
        echo "This is not a valid VHDL file"
        return
    endif
    if curr_pos < check_for_validity 
        normal `a
        echo "Not a signal, probably a port"
        return  
    endif
    " checks whether the signal has been already defined 
    normal gg
    let flags = "W"
    while search('\(--.*\)\@<!'.expand(@0), flags) > 0
        if line(".") < check_for_validity
            normal `a
            echo "signal already defined"
            return
        endif
        let flags = "W"
    endwhile
    let bit_width = input("Enter the length of signal: ")
    let bit_width_minus1 = bit_width - 1
    "let sig_type = input("convention (v - std_logic_vector, s - signed, u - unsigned) : ")
    let sig_type = 'v' 
    normal 'b
    if bit_width == 1
        normal O
        exe "s/^/\tsignal\t".expand(@0)."\t: std_logic;/"
        normal `a
    elseif bit_width > 1
        normal O
        if sig_type == 'v'
            exe "s/^/\tsignal\t".expand(@0)."\t: std_logic_vector(".bit_width_minus1." downto 0);/"
        elseif sig_type == 's'
            exe "s/^/\tsignal ".expand(@0).": signed(".bit_width_minus1." downto 0);/"
        elseif sig_type == 'u'
            exe "s/^/\tsignal ".expand(@0).": unsigned(".bit_width_minus1." downto 0);/"
        else 
            echo "wrong selection"
        endif
        normal `a
    else
        normal `a							
        echo "signal not defined"	
        return
    endif
    return "Done"
endfunction				


"function VHDL_Vhdl_Format()
"    let start_line = line('v')
"    call HDL_Component_Format(start_line,start_line)
"endfunction
