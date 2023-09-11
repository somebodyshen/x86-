         ;代码清单c19-1
         ;文件名：c19_core.asm
         ;文件说明：保护模式微型核心程序 
         ;创建日期：2022-06-19

         ;以下常量定义部分。内核的大部分内容都应当固定 
         core_code_seg_sel     equ  0x38    ;内核代码段选择子
         core_data_seg_sel     equ  0x30    ;内核数据段选择子 
         sys_routine_seg_sel   equ  0x28    ;系统公共例程代码段的选择子 
         video_ram_seg_sel     equ  0x20    ;视频显示缓冲区的段选择子
         core_stack_seg_sel    equ  0x18    ;内核堆栈段选择子
         mem_0_4_gb_seg_sel    equ  0x08    ;整个0-4GB内存的段的选择子
         idt_linear_address    equ  0x1f000 ;中断描述符表的线性地址
         core_lin_alloc_at     equ  0x80100000
                                            ;内核中可用于分配的起始线性地址
         core_lin_tcb_addr     equ  0x8001f800
                                            ;内核任务TCB的高端线性地址

;-------------------------------------------------------------------------------
         ;以下是系统核心的头部，用于加载核心程序 
         core_length      dd core_end       ;核心程序总长度#00

         sys_routine_seg  dd section.sys_routine.start
                                            ;系统公用例程段位置#04

         core_data_seg    dd section.core_data.start
                                            ;核心数据段位置#08

         core_code_seg    dd section.core_code.start
                                            ;核心代码段位置#0c


         core_entry       dd start          ;核心代码段入口点#10
                          dw core_code_seg_sel

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vstart=0                ;系统公共例程代码段 
;-------------------------------------------------------------------------------
         ;字符串显示例程
put_string:                                 ;显示0终止的字符串并移动光标 
                                            ;输入：DS:EBX=串地址
         push ecx

         cli

  .getc:
         mov cl,[ebx]
         or cl,cl
         jz .exit
         call put_char
         inc ebx
         jmp .getc

  .exit:
         sti

         pop ecx
         retf                               ;段间返回

;-------------------------------------------------------------------------------
put_char:                                   ;在当前光标处显示一个字符,并推进
                                            ;光标。仅用于段内调用
                                            ;输入：CL=字符ASCII码
         pushad

         ;以下取当前光标位置
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         inc dx                             ;0x3d5
         in al,dx                           ;高字
         mov ah,al

         dec dx                             ;0x3d4
         mov al,0x0f
         out dx,al
         inc dx                             ;0x3d5
         in al,dx                           ;低字
         mov bx,ax                          ;BX=代表光标位置的16位数

         cmp cl,0x0d                        ;回车符？
         jnz .put_0a
         mov ax,bx
         mov bl,80
         div bl
         mul bl
         mov bx,ax
         jmp .set_cursor

  .put_0a:
         cmp cl,0x0a                        ;换行符？
         jnz .put_other
         add bx,80
         jmp .roll_screen

  .put_other:                               ;正常显示字符
         push es
         mov eax,video_ram_seg_sel          ;0xb8000段的选择子
         mov es,eax
         shl bx,1
         mov [es:bx],cl
         pop es

         ;以下将光标位置推进一个字符
         shr bx,1
         inc bx

  .roll_screen:
         cmp bx,2000                        ;光标超出屏幕？滚屏
         jl .set_cursor

         push bx                            ;为了修改原书程序的逻辑问题，新增
         push ds
         push es
         mov eax,video_ram_seg_sel
         mov ds,eax
         mov es,eax
         cld
         mov esi,0xa0                       ;小心！32位模式下movsb/w/d
         mov edi,0x00                       ;使用的是esi/edi/ecx
         mov ecx,1920
         rep movsw
         mov bx,3840                        ;清除屏幕最底一行
         mov ecx,80                         ;32位程序应该使用ECX
  .cls:
         mov word[es:bx],0x0720
         add bx,2
         loop .cls

         pop es
         pop ds

         ;mov bx,1920                       ;为了修改原书程序的逻辑问题，删除
         pop bx                             ;为了修改原书程序的逻辑问题，新增
         sub bx,80                          ;为了修改原书程序的逻辑问题，新增

  .set_cursor:
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         inc dx                             ;0x3d5
         mov al,bh
         out dx,al
         dec dx                             ;0x3d4
         mov al,0x0f
         out dx,al
         inc dx                             ;0x3d5
         mov al,bl
         out dx,al

         popad
         ret

;-------------------------------------------------------------------------------
read_hard_disk_0:                           ;从硬盘读取一个逻辑扇区
                                            ;EAX=逻辑扇区号
                                            ;DS:EBX=目标缓冲区地址
                                            ;返回：EBX=EBX+512
         push eax 
         push ecx
         push edx
      
         push eax

         cli

         mov dx,0x1f2
         mov al,1
         out dx,al                          ;读取的扇区数

         inc dx                             ;0x1f3
         pop eax
         out dx,al                          ;LBA地址7~0

         inc dx                             ;0x1f4
         mov cl,8
         shr eax,cl
         out dx,al                          ;LBA地址15~8

         inc dx                             ;0x1f5
         shr eax,cl
         out dx,al                          ;LBA地址23~16

         inc dx                             ;0x1f6
         shr eax,cl
         or al,0xe0                         ;第一硬盘  LBA地址27~24
         out dx,al

         inc dx                             ;0x1f7
         mov al,0x20                        ;读命令
         out dx,al

  .waits:
         in al,dx
         and al,0x88
         cmp al,0x08
         jnz .waits                         ;不忙，且硬盘已准备好数据传输 

         mov ecx,256                        ;总共要读取的字数
         mov dx,0x1f0
  .readw:
         in ax,dx
         mov [ebx],ax
         add ebx,2
         loop .readw

         sti

         pop edx
         pop ecx
         pop eax
      
         retf                               ;段间返回 

;-------------------------------------------------------------------------------
;汇编语言程序是极难一次成功，而且调试非常困难。这个例程可以提供帮助 
put_hex_dword:                              ;在当前光标处以十六进制形式显示
                                            ;一个双字并推进光标 
                                            ;输入：EDX=要转换并显示的数字
                                            ;输出：无
         pushad
         push ds
      
         mov ax,core_data_seg_sel           ;切换到核心数据段 
         mov ds,ax
      
         mov ebx,bin_hex                    ;指向核心数据段内的转换表
         mov ecx,8
  .xlt:    
         rol edx,4
         mov eax,edx
         and eax,0x0000000f
         xlat
      
         push ecx
         mov cl,al                           
         call put_char
         pop ecx
       
         loop .xlt
      
         pop ds
         popad
         retf

;-------------------------------------------------------------------------------
allocate_a_4k_page:                         ;分配一个4KB的页
                                            ;输入：无
                                            ;输出：EAX=页的物理地址
         push ebx
         push ecx
         push edx
         push ds

         mov eax,core_data_seg_sel
         mov ds,eax

         xor eax,eax
  .b1:
         bts [page_bit_map],eax
         jnc .b2
         inc eax
         cmp eax,page_map_len*8
         jl .b1

         mov ebx,message_3
         call sys_routine_seg_sel:put_string
         hlt                                ;没有可以分配的页，停机

  .b2:
         shl eax,12                         ;乘以4096（0x1000）

         pop ds
         pop edx
         pop ecx
         pop ebx

         ret

;-------------------------------------------------------------------------------
alloc_inst_a_page:                          ;分配一个页，并安装在当前活动的
                                            ;层级分页结构中
                                            ;输入：EBX=页的线性地址
         push eax
         push ebx
         push ecx
         push esi
         push ds

         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax

         ;检查该线性地址所对应的页表是否存在
         mov esi,ebx
         and esi,0xffc00000                 ;清除页表索引和页内偏移部分
         shr esi,20                         ;将页目录索引乘以4作为页内偏移
         or esi,0xfffff000                  ;页目录自身的线性地址+表内偏移

         test dword [esi],0x00000001        ;P位是否为“1”。检查该线性地址是
         jnz .b1                            ;否已经有对应的页表

         ;创建并安装该线性地址所对应的页表
         call allocate_a_4k_page            ;分配一个页做为页表
         or eax,0x00000007
         mov [esi],eax                      ;在页目录中登记该页表

         ;清空当前页表
         mov eax,ebx
         and eax,0xffc00000
         shr eax,10
         or eax,0xffc00000
         mov ecx,1024
  .cls0:
         mov dword [es:eax],0x00000000
         add eax,4
         loop .cls0

  .b1:
         ;检查该线性地址对应的页表项（页）是否存在
         mov esi,ebx
         and esi,0xfffff000                 ;清除页内偏移部分
         shr esi,10                         ;将页目录索引变成页表索引，页表索引乘以4作为页内偏移
         or esi,0xffc00000                  ;得到该线性地址对应的页表项

         test dword [esi],0x00000001        ;P位是否为“1”。检查该线性地址是
         jnz .b2                            ;否已经有对应的页

         ;创建并安装该线性地址所对应的页
         call allocate_a_4k_page            ;分配一个页，这才是要安装的页
         or eax,0x00000007
         mov [esi],eax

  .b2:
         pop ds
         pop esi
         pop ecx
         pop ebx
         pop eax

         retf

;-------------------------------------------------------------------------------
create_copy_cur_pdir:                       ;创建新页目录，并复制当前页目录内容
                                            ;输入：无
                                            ;输出：EAX=新页目录的物理地址
         push ds
         push es
         push esi
         push edi
         push ebx
         push ecx

         mov ebx,mem_0_4_gb_seg_sel
         mov ds,ebx
         mov es,ebx

         call allocate_a_4k_page
         mov ebx,eax
         or ebx,0x00000007
         mov [0xfffffff8],ebx

         invlpg [0xfffffff8]

         mov esi,0xfffff000                 ;ESI->当前页目录的线性地址
         mov edi,0xffffe000                 ;EDI->新页目录的线性地址
         mov ecx,1024                       ;ECX=要复制的目录项数
         cld
         repe movsd

         pop ecx
         pop ebx
         pop edi
         pop esi
         pop es
         pop ds

         retf

;-------------------------------------------------------------------------------
task_alloc_memory:                          ;在指定任务的虚拟内存空间中分配内存
                                            ;输入：EBX=任务控制块TCB的线性地址
                                            ;      ECX=希望分配的字节数
                                            ;输出：ECX=已分配的起始线性地址
         push eax

         push ds

         push ebx                           ;to A

         ;获得本次内存分配的起始线性地址
         mov ax,mem_0_4_gb_seg_sel
         mov ds,ax

         mov ebx,[ebx+0x46]                 ;获得本次分配的起始线性地址
         mov eax,ebx
         add ecx,ebx                        ;本次分配，最后一个字节之后的线性地址

         push ecx                           ;to B

         ;为请求的内存分配页
         and ebx,0xfffff000
         and ecx,0xfffff000
  .next:
         call sys_routine_seg_sel:alloc_inst_a_page
                                            ;安装当前线性地址所在的页
         add ebx,0x1000                     ;+4096
         cmp ebx,ecx
         jle .next

         ;将用于下一次分配的线性地址强制按4字节对齐
         pop ecx                            ;B

         test ecx,0x00000003                ;线性地址是4字节对齐的吗？
         jz .algn                           ;是，直接返回
         add ecx,4                          ;否，强制按4字节对齐
         and ecx,0xfffffffc

  .algn:
         pop ebx                            ;A

         mov [ebx+0x46],ecx                 ;将下次分配可用的线性地址回存到TCB中
         mov ecx,eax

         pop ds

         pop eax

         retf

;-------------------------------------------------------------------------------
allocate_memory:                            ;在当前任务的地址空间中分配内存
                                            ;输入：ECX=希望分配的字节数
                                            ;输出：ECX=起始线性地址 
         push eax
         push ebx

         push ds

         ;得到TCB链表首节点的线性地址
         mov eax,core_data_seg_sel
         mov ds,eax

         mov eax,[tcb_chain]                ;EAX=首节点的线性地址

         mov ebx,mem_0_4_gb_seg_sel
         mov ds,ebx

         ;搜索状态为忙（当前任务）的节点
  .s0:
         cmp word [eax+0x04],0xffff
         jz .s1                             ;找到忙的节点，EAX=节点的线性地址
         mov eax,[eax]
         jmp .s0

         ;开始分配内存
  .s1:
         mov ebx,eax
         call sys_routine_seg_sel:task_alloc_memory

         pop ds

         pop ebx
         pop eax

         retf

;-------------------------------------------------------------------------------
set_up_gdt_descriptor:                      ;在GDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符 
                                            ;输出：CX=描述符的选择子
         push eax
         push ebx
         push edx

         push ds
         push es

         mov ebx,core_data_seg_sel          ;切换到核心数据段
         mov ds,ebx

         sgdt [pgdt]                        ;以便开始处理GDT

         mov ebx,mem_0_4_gb_seg_sel
         mov es,ebx

         movzx ebx,word [pgdt]              ;GDT界限
         inc bx                             ;GDT总字节数，也是下一个描述符偏移
         add ebx,[pgdt+2]                   ;下一个描述符的线性地址

         mov [es:ebx],eax
         mov [es:ebx+4],edx

         add word [pgdt],8                  ;增加一个描述符的大小

         lgdt [pgdt]                        ;对GDT的更改生效

         mov ax,[pgdt]                      ;得到GDT界限值
         xor dx,dx
         mov bx,8
         div bx                             ;除以8，去掉余数
         mov cx,ax
         shl cx,3                           ;将索引号移到正确位置

         pop es
         pop ds

         pop edx
         pop ebx
         pop eax

         retf
;-------------------------------------------------------------------------------
make_seg_descriptor:                        ;构造存储器和系统的段描述符
                                            ;输入：EAX=线性基地址
                                            ;      EBX=段界限
                                            ;      ECX=属性。各属性位都在原始
                                            ;          位置，无关的位清零 
                                            ;返回：EDX:EAX=描述符
         mov edx,eax
         shl eax,16
         or ax,bx                           ;描述符前32位(EAX)构造完毕

         and edx,0xffff0000                 ;清除基地址中无关的位
         rol edx,8
         bswap edx                          ;装配基址的31~24和23~16  (80486+)

         xor bx,bx
         or edx,ebx                         ;装配段界限的高4位

         or edx,ecx                         ;装配属性

         retf

;-------------------------------------------------------------------------------
make_gate_descriptor:                       ;构造门的描述符（调用门等）
                                            ;输入：EAX=门代码在段内偏移地址
                                            ;       BX=门代码所在段的选择子 
                                            ;       CX=段类型及属性等（各属
                                            ;          性位都在原始位置）
                                            ;返回：EDX:EAX=完整的描述符
         push ebx
         push ecx
      
         mov edx,eax
         and edx,0xffff0000                 ;得到偏移地址高16位 
         or dx,cx                           ;组装属性部分到EDX
       
         and eax,0x0000ffff                 ;得到偏移地址低16位 
         shl ebx,16                          
         or eax,ebx                         ;组装段选择子部分
      
         pop ecx
         pop ebx
      
         retf                                   
                             
;-------------------------------------------------------------------------------
initiate_task_switch:                       ;主动发起任务切换
                                            ;输入：无
                                            ;输出：无。
         pushad
         push ds
         push es

         mov eax,core_data_seg_sel
         mov es,eax

         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax

         mov eax,[es:tcb_chain]
         cmp eax,0
         jz .return

         ;搜索状态为忙（当前任务）的节点
  .b0:
         cmp word [eax+0x04],0xffff
         cmove esi,eax                     ;找到忙的节点，ESI=节点的线性地址
         jz .b1
         mov eax,[eax]
         jmp .b0

         ;从当前节点继续搜索就绪任务的节点
  .b1:
         mov ebx,[eax]
         or ebx,ebx
         jz .b2                            ;到链表尾部也未发现就绪节点，从头找
         cmp word [ebx+0x04],0x0000
         cmove edi,ebx                     ;已找到就绪节点，EDI=节点的线性地址
         jz .b3
         mov eax,ebx
         jmp .b1

  .b2:
         mov ebx,[es:tcb_chain]            ;EBX=链表首节点线性地址
  .b20:
         cmp word [ebx+0x04],0x0000
         cmove edi,ebx                     ;已找到就绪节点，EDI=节点的线性地址
         jz .b3
         mov ebx,[ebx]
         or ebx,ebx
         jz .return                        ;链表中已经不存在就绪任务，返回
         jmp .b20

         ;就绪任务的节点已经找到，准备切换到该任务
  .b3:
         not word [esi+0x04]               ;将忙状态的节点改为就绪状态的节点
         not word [edi+0x04]               ;将就绪状态的节点改为忙状态的节点
         jmp far [edi+0x14]                ;任务切换

  .return:
         pop es
         pop ds
         popad

         retf

;-------------------------------------------------------------------------------
terminate_current_task:                     ;终止当前任务
                                            ;注意，执行此例程时，当前任务仍在
                                            ;运行中。此例程其实也是当前任务的
                                            ;一部分 
         mov eax,core_data_seg_sel
         mov es,eax

         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax

         mov eax,[es:tcb_chain]
                                            ;EAX=首节点的线性地址
         ;搜索状态为忙（当前任务）的节点
  .s0:
         cmp word [eax+0x04],0xffff
         jz .s1                             ;找到忙的节点，EAX=节点的线性地址
         mov eax,[eax]
         jmp .s0

         ;将状态为忙的节点改成终止状态
  .s1:
         mov word [eax+0x04],0x3333

         ;搜索就绪状态的任务
         mov ebx,[es:tcb_chain]            ;EBX=链表首节点线性地址
  .s2:
         cmp word [ebx+0x04],0x0000
         jz .s3                            ;已找到就绪节点，EBX=节点的线性地址
         mov ebx,[ebx]
         jmp .s2

         ;就绪任务的节点已经找到，准备切换到该任务
  .s3:
         not word [ebx+0x04]               ;将就绪状态的节点改为忙状态的节点
         jmp far [ebx+0x14]                ;任务切换

;-------------------------------------------------------------------------------
general_interrupt_handler:                  ;通用的中断处理过程
         push eax

         mov al,0x20                        ;中断结束命令EOI
         out 0xa0,al                        ;向从片发送
         out 0x20,al                        ;向主片发送

         pop eax

         iretd

;-------------------------------------------------------------------------------
general_exception_handler:                  ;通用的异常处理过程
         mov ebx,excep_msg
         call sys_routine_seg_sel:put_string

         cli

         hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:                  ;实时时钟中断处理过程

         pushad

         mov al,0x20                        ;中断结束命令EOI
         out 0xa0,al                        ;向8259A从片发送
         out 0x20,al                        ;向8259A主片发送

         mov al,0x0c                        ;寄存器C的索引。且开放NMI
         out 0x70,al
         in al,0x71                         ;读一下RTC的寄存器C，否则只发生一次中断
                                            ;此处不考虑闹钟和周期性中断的情况
         ;请求任务调度
         call sys_routine_seg_sel:initiate_task_switch

         popad

         iretd

;-------------------------------------------------------------------------------
do_task_clean:                             ;清理已经终止的任务并回收资源

         ;搜索TCB链表，找到状态为终止的节点
         ;将节点从链表中拆除
         ;回收任务占用的各种资源（可以从它的TCB中找到）

         retf

sys_routine_end:

;===============================================================================
SECTION core_data vstart=0                  ;系统核心的数据段 
;------------------------------------------------------------------------------- 
         pgdt             dw  0             ;用于设置和修改GDT 
                          dd  0

         pidt             dw  0
                          dd  0

         page_bit_map     db  0xff,0xff,0xff,0xff,0xff,0xff,0x55,0x55
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
         page_map_len     equ $-page_bit_map

         ;符号地址检索表
         salt:
         salt_1           db  '@PrintString'
                     times 256-($-salt_1) db 0
                          dd  put_string
                          dw  sys_routine_seg_sel

         salt_2           db  '@ReadDiskData'
                     times 256-($-salt_2) db 0
                          dd  read_hard_disk_0
                          dw  sys_routine_seg_sel

         salt_3           db  '@PrintDwordAsHexString'
                     times 256-($-salt_3) db 0
                          dd  put_hex_dword
                          dw  sys_routine_seg_sel

         salt_4           db  '@TerminateProgram'
                     times 256-($-salt_4) db 0
                          dd  terminate_current_task
                          dw  sys_routine_seg_sel

         salt_5           db  '@InitTaskSwitch'
                     times 256-($-salt_5) db 0
                          dd  initiate_task_switch
                          dw  sys_routine_seg_sel

         salt_6           db  '@malloc'
                     times 256-($-salt_6) db 0
                          dd  allocate_memory
                          dw  sys_routine_seg_sel

         salt_item_len   equ $-salt_6
         salt_items      equ ($-salt)/salt_item_len

         message_0        db  '  System core is runing in protect mode,'
                          db  'IDT is mounted.',0x0d,0x0a,0

         cpu_brnd0        db  0x0d,0x0a,'  ',0
         cpu_brand  times 52  db 0
         cpu_brnd1        db  0x0d,0x0a,0x0d,0x0a,0

         message_1        db  '  Paging is enabled.System core is mapped to'
                          db  ' linear address 0x80000000.',0x0d,0x0a,0

         message_2        db  '  System wide CALL-GATE mounted and test OK.'
                          db  0x0d,0x0a,0

         message_3        db  '********No more pages********',0

         excep_msg        db  '********Exception encounted********',0

         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 

         core_buf   times 2048 db 0         ;内核用的缓冲区

         ;任务控制块链
         tcb_chain        dd  0

         core_msg1        db  '  Core task created.',0x0d,0x0a,0
                 
         core_msg2        db  '[CORE TASK]: I am working!',0x0d,0x0a,0

core_data_end:
               
;===============================================================================
SECTION core_code vstart=0
;-------------------------------------------------------------------------------
fill_descriptor_in_ldt:                     ;在LDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符
                                            ;          EBX=TCB基地址
                                            ;输出：CX=描述符的选择子
         push eax
         push edx
         push edi
         push ds

         mov ecx,mem_0_4_gb_seg_sel
         mov ds,ecx

         mov edi,[ebx+0x0c]                 ;获得LDT基地址
         
         xor ecx,ecx
         mov cx,[ebx+0x0a]                  ;获得LDT界限
         inc cx                             ;LDT的总字节数，即新描述符偏移地址
         
         mov [edi+ecx+0x00],eax
         mov [edi+ecx+0x04],edx             ;安装描述符

         add cx,8                           
         dec cx                             ;得到新的LDT界限值 

         mov [ebx+0x0a],cx                  ;更新LDT界限值到TCB

         mov ax,cx
         xor dx,dx
         mov cx,8
         div cx
         
         mov cx,ax
         shl cx,3                           ;左移3位，并且
         or cx,0000_0000_0000_0100B         ;使TI位=1，指向LDT，最后使RPL=00 

         pop ds
         pop edi
         pop edx
         pop eax
     
         ret
         
;------------------------------------------------------------------------------- 
load_relocate_program:                      ;加载并重定位用户程序
                                            ;输入: PUSH 逻辑扇区号
                                            ;      PUSH 任务控制块基地址
                                            ;输出：无 
         pushad
      
         push ds
         push es
      
         mov ebp,esp                        ;为访问通过堆栈传递的参数做准备
      
         mov ecx,mem_0_4_gb_seg_sel
         mov es,ecx

         ;清空当前页目录的前半部分（对应低2GB的局部地址空间）
         mov ebx,0xfffff000
         xor esi,esi
  .clsp:
         mov dword [es:ebx+esi*4],0x00000000
         inc esi
         cmp esi,512
         jl .clsp

         mov ebx,cr3                        ;刷新TLB
         mov cr3,ebx

         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址

         ;以下申请创建LDT所需要的内存
         mov ebx,esi
         mov ecx,160                        ;允许安装20个LDT描述符
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x0c],ecx              ;登记LDT基地址到TCB中
         mov word [es:esi+0x0a],0xffff      ;登记LDT初始的界限到TCB中 

         ;以下开始加载用户程序 
         mov eax,core_data_seg_sel
         mov ds,eax                         ;切换DS到内核数据段
       
         mov eax,[ebp+12*4]                 ;从堆栈中取出用户程序起始扇区号 
         mov ebx,core_buf                   ;读取程序头部数据     
         call sys_routine_seg_sel:read_hard_disk_0

         ;以下判断整个程序有多大
         mov eax,[core_buf]                 ;程序尺寸
         mov ebx,eax
         and ebx,0xfffffe00                 ;使之512字节对齐（能被512整除的数低 
         add ebx,512                        ;9位都为0 
         test eax,0x000001ff                ;程序的大小正好是512的倍数吗? 
         cmovnz eax,ebx                     ;不是。使用凑整的结果
      
         mov ecx,eax                        ;实际需要申请的内存数量
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x06],ecx              ;登记程序加载基地址到TCB中
      
         mov ebx,ecx                        ;ebx -> 申请到的内存首地址
         xor edx,edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;总扇区数 
      
         mov eax,mem_0_4_gb_seg_sel         ;切换DS到0-4GB的段
         mov ds,eax

         mov eax,[ebp+12*4]                 ;起始扇区号 
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                           ;循环读，直到读完整个用户程序

         mov edi,[es:esi+0x06]              ;获得程序加载基地址

         ;建立程序头部段描述符
         mov eax,edi                        ;程序头部起始线性地址
         mov ebx,[edi+0x04]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040f200                 ;字节粒度的数据段描述符，特权级3 
         call sys_routine_seg_sel:make_seg_descriptor
      
         ;安装头部段描述符到LDT中 
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt

         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [es:esi+0x44],cx               ;登记程序头部段选择子到TCB 
         mov [edi+0x04],cx                  ;和头部内 
      
         ;建立程序代码段描述符
         mov eax,edi
         add eax,[edi+0x0c]                 ;代码起始线性地址
         mov ebx,[edi+0x10]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040f800                 ;字节粒度的代码段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [edi+0x0c],cx                  ;登记代码段选择子到头部

         ;建立程序数据段描述符
         mov eax,edi
         add eax,[edi+0x14]                 ;数据段起始线性地址
         mov ebx,[edi+0x18]                 ;段长度
         dec ebx                            ;段界限 
         mov ecx,0x0040f200                 ;字节粒度的数据段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [edi+0x14],cx                  ;登记数据段选择子到头部

         ;建立程序堆栈段描述符
         mov eax,edi
         add eax,[edi+0x1c]                 ;数据段起始线性地址
         mov ebx,[edi+0x20]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040f200                 ;字节粒度的堆栈段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [edi+0x1c],cx                  ;登记堆栈段选择子到头部

         ;重定位SALT 
         mov eax,mem_0_4_gb_seg_sel         ;这里和前一章不同，头部段描述符
         mov es,eax                         ;已安装，但还没有生效，故只能通
                                            ;过4GB段访问用户程序头部          
         mov eax,core_data_seg_sel
         mov ds,eax
      
         cld

         mov ecx,[es:edi+0x24]              ;U-SALT条目数(通过访问4GB段取得) 
         add edi,0x28                       ;U-SALT在4GB段内的偏移 
  .b2: 
         push ecx
         push edi
      
         mov ecx,salt_items
         mov esi,salt
  .b3:
         push edi
         push esi
         push ecx

         mov ecx,64                         ;检索表中，每条目的比较次数 
         repe cmpsd                         ;每次比较4字节 
         jnz .b4
         mov eax,[esi]                      ;若匹配，则esi恰好指向其后的地址
         mov [es:edi-256],eax               ;将字符串改写成偏移地址 
         mov ax,[esi+4]
         or ax,0000000000000011B            ;以用户程序自己的特权级使用调用门
                                            ;故RPL=3 
         mov [es:edi-252],ax                ;回填调用门选择子 
  .b4:
      
         pop ecx
         pop esi
         add esi,salt_item_len
         pop edi                            ;从头比较 
         loop .b3
      
         pop edi
         add edi,256
         pop ecx
         loop .b2

         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址

         ;创建0特权级栈
         mov ecx,0                          ;以4KB为单位的栈段界限值
         mov [es:esi+0x1a],ecx              ;登记0特权级栈界限到TCB
         inc ecx
         shl ecx,12                         ;乘以4096，得到段大小
         push ecx
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x1e],ecx              ;登记0特权级栈基地址到TCB
         mov eax,ecx
         mov ebx,[es:esi+0x1a]              ;段长度（界限）
         mov ecx,0x00c09200                 ;4KB粒度，读写，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         ;or cx,0000_0000_0000_0000          ;设置选择子的特权级为0
         mov [es:esi+0x22],cx               ;登记0特权级堆栈选择子到TCB
         pop dword [es:esi+0x24]            ;登记0特权级堆栈初始ESP到TCB

         ;创建1特权级堆栈
         mov ecx,0
         mov [es:esi+0x28],ecx              ;登记1特权级堆栈尺寸到TCB
         inc ecx
         shl ecx,12                         ;乘以4096，得到段大小
         push ecx
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x2c],ecx              ;登记1特权级堆栈基地址到TCB
         mov eax,ecx
         mov ebx,[es:esi+0x28]              ;段长度（界限）
         mov ecx,0x00c0b200                 ;4KB粒度，读写，特权级1
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0001          ;设置选择子的特权级为1
         mov [es:esi+0x30],cx               ;登记1特权级堆栈选择子到TCB
         pop dword [es:esi+0x32]            ;登记1特权级堆栈初始ESP到TCB

         ;创建2特权级堆栈
         mov ecx,0
         mov [es:esi+0x36],ecx              ;登记2特权级堆栈尺寸到TCB
         inc ecx
         shl ecx,12                         ;乘以4096，得到段大小
         push ecx
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x3a],ecx              ;登记2特权级堆栈基地址到TCB
         mov eax,ecx
         mov ebx,[es:esi+0x36]              ;段长度（界限）
         mov ecx,0x00c0d200                 ;4KB粒度，读写，特权级2
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0010          ;设置选择子的特权级为2
         mov [es:esi+0x3e],cx               ;登记2特权级堆栈选择子到TCB
         pop dword [es:esi+0x40]            ;登记2特权级堆栈初始ESP到TCB

         ;在GDT中登记LDT描述符
         mov eax,[es:esi+0x0c]              ;LDT的起始线性地址
         movzx ebx,word [es:esi+0x0a]       ;LDT段界限
         mov ecx,0x00008200                 ;LDT描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x10],cx               ;登记LDT选择子到TCB中
       
         ;创建用户程序的TSS
         mov ecx,104                        ;tss的基本尺寸
         mov [es:esi+0x12],cx              
         dec word [es:esi+0x12]             ;登记TSS界限值到TCB 
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x14],ecx              ;登记TSS基地址到TCB
      
         ;登记基本的TSS表格内容
         mov word [es:ecx+0],0              ;反向链=0

         mov edx,[es:esi+0x24]              ;登记0特权级堆栈初始ESP
         mov [es:ecx+4],edx                 ;到TSS中

         mov dx,[es:esi+0x22]               ;登记0特权级堆栈段选择子
         mov [es:ecx+8],dx                  ;到TSS中

         mov edx,[es:esi+0x32]              ;登记1特权级堆栈初始ESP
         mov [es:ecx+12],edx                ;到TSS中

         mov dx,[es:esi+0x30]               ;登记1特权级堆栈段选择子
         mov [es:ecx+16],dx                 ;到TSS中

         mov edx,[es:esi+0x40]              ;登记2特权级堆栈初始ESP
         mov [es:ecx+20],edx                ;到TSS中

         mov dx,[es:esi+0x3e]               ;登记2特权级堆栈段选择子
         mov [es:ecx+24],dx                 ;到TSS中

         mov dx,[es:esi+0x10]               ;登记任务的LDT选择子
         mov [es:ecx+96],dx                 ;到TSS中

         mov dx,[es:esi+0x12]               ;登记任务的I/O位图偏移
         mov [es:ecx+102],dx                ;到TSS中

         mov word [es:ecx+100],0            ;T=0

         ;访问用户程序头部，获取数据填充TSS 
         mov ebx,[ebp+11*4]                 ;从堆栈中取得TCB的基地址
         mov edi,[es:ebx+0x06]              ;用户程序加载的基地址 

         mov edx,[es:edi+0x08]              ;登记程序入口点（EIP）
         mov [es:ecx+32],edx                ;到TSS

         mov dx,[es:edi+0x0c]               ;登记程序代码段（CS）选择子
         mov [es:ecx+76],dx                 ;到TSS中

         mov dx,[es:edi+0x1c]               ;登记程序堆栈段（SS）选择子
         mov [es:ecx+80],dx                 ;到TSS中

         mov edx,[es:edi+0x20]              ;堆栈的高端线性地址
         mov [es:ecx+56],edx                ;填写TSS的ESP域

         mov dx,[es:edi+0x04]               ;登记程序数据段（DS）选择子
         mov word [es:ecx+84],dx            ;到TSS中。注意，它指向程序头部段
      
         mov word [es:ecx+72],0             ;TSS中的ES=0

         mov word [es:ecx+88],0             ;TSS中的FS=0

         mov word [es:ecx+92],0             ;TSS中的GS=0

         pushfd
         pop dword [es:ecx+36]              ;EFLAGS

         ;在GDT中登记TSS描述符
         mov eax,[es:esi+0x14]              ;TSS的起始线性地址
         movzx ebx,word [es:esi+0x12]       ;段长度（界限）
         mov ecx,0x00008900                 ;TSS描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x18],cx               ;登记TSS选择子到TCB

         ;创建用户任务的页目录
         ;注意！页的分配和使用是由页位图决定的，可以不占用线性地址空间
         call sys_routine_seg_sel:create_copy_cur_pdir
         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov dword [es:ebx+28],eax          ;填写TSS的CR3(PDBR)域

         pop es                             ;恢复到调用此过程前的es段 
         pop ds                             ;恢复到调用此过程前的ds段
      
         popad
      
         ret 8                              ;丢弃调用本过程前压入的参数 
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ;在TCB链上追加任务控制块
                                            ;输入：ECX=TCB线性基地址
         push eax
         push edx
         push ds
         push es

         cli

         mov eax,core_data_seg_sel          ;令DS指向内核数据段 
         mov ds,eax
         mov eax,mem_0_4_gb_seg_sel         ;令ES指向0..4GB段
         mov es,eax
         
         mov dword [es: ecx+0x00],0         ;当前TCB指针域清零，以指示这是最
                                            ;后一个TCB
                                             
         mov eax,[tcb_chain]                ;TCB表头指针
         or eax,eax                         ;链表为空？
         jz .notcb 
         
  .searc:
         mov edx,eax
         mov eax,[es: edx+0x00]
         or eax,eax               
         jnz .searc

         mov [es: edx+0x00],ecx
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain],ecx                ;若为空表，直接令表头指针指向TCB
         
  .retpc:
         sti

         pop es
         pop ds
         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:
         mov ecx,core_data_seg_sel          ;令DS指向核心数据段 
         mov ds,ecx

         mov ecx,mem_0_4_gb_seg_sel         ;令ES指向4GB数据段 
         mov es,ecx

         ;创建中断描述符表IDT
         ;注意！在此期间，不得开放中断，也不得调用put_string例程！

         ;前20个向量是处理器异常使用的
         mov eax,general_exception_handler  ;门代码在段内偏移地址
         mov bx,sys_routine_seg_sel         ;门代码所在段的选择子
         mov cx,0x8e00                      ;32位中断门，0特权级
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
         xor esi,esi
  .idt0:
         mov [es:ebx+esi*8],eax
         mov [es:ebx+esi*8+4],edx
         inc esi
         cmp esi,19                         ;安装前20个异常中断处理过程
         jle .idt0

         ;其余为保留或硬件使用的中断向量
         mov eax,general_interrupt_handler  ;门代码在段内偏移地址
         mov bx,sys_routine_seg_sel         ;门代码所在段的选择子
         mov cx,0x8e00                      ;32位中断门，0特权级
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
  .idt1:
         mov [es:ebx+esi*8],eax
         mov [es:ebx+esi*8+4],edx
         inc esi
         cmp esi,255                        ;安装普通的中断处理过程
         jle .idt1

         ;设置实时时钟中断处理过程
         mov eax,rtm_0x70_interrupt_handle  ;门代码在段内偏移地址
         mov bx,sys_routine_seg_sel         ;门代码所在段的选择子
         mov cx,0x8e00                      ;32位中断门，0特权级
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
         mov [es:ebx+0x70*8],eax
         mov [es:ebx+0x70*8+4],edx

         ;准备开放中断
         mov word [pidt],256*8-1            ;IDT的界限
         mov dword [pidt+2],idt_linear_address
         lidt [pidt]                        ;加载中断描述符表寄存器IDTR

         ;设置8259A中断控制器
         mov al,0x11
         out 0x20,al                        ;ICW1：边沿触发/级联方式
         mov al,0x20
         out 0x21,al                        ;ICW2:起始中断向量
         mov al,0x04
         out 0x21,al                        ;ICW3:从片级联到IR2
         mov al,0x01
         out 0x21,al                        ;ICW4:非总线缓冲，全嵌套，正常EOI

         mov al,0x11
         out 0xa0,al                        ;ICW1：边沿触发/级联方式
         mov al,0x70
         out 0xa1,al                        ;ICW2:起始中断向量
         mov al,0x04
         out 0xa1,al                        ;ICW3:从片级联到IR2
         mov al,0x01
         out 0xa1,al                        ;ICW4:非总线缓冲，全嵌套，正常EOI

         ;设置和时钟中断相关的硬件
         mov al,0x0b                        ;RTC寄存器B
         or al,0x80                         ;阻断NMI
         out 0x70,al
         mov al,0x12                        ;设置寄存器B，禁止周期性中断，开放更
         out 0x71,al                        ;新结束后中断，BCD码，24小时制

         in al,0xa1                         ;读8259从片的IMR寄存器
         and al,0xfe                        ;清除bit 0(此位连接RTC)
         out 0xa1,al                        ;写回此寄存器

         mov al,0x0c
         out 0x70,al
         in al,0x71                         ;读RTC寄存器C，复位未决的中断状态

         sti                                ;开放硬件中断

         mov ebx,message_0
         call sys_routine_seg_sel:put_string

         ;显示处理器品牌信息
         mov eax,0x80000002
         cpuid
         mov [cpu_brand + 0x00],eax
         mov [cpu_brand + 0x04],ebx
         mov [cpu_brand + 0x08],ecx
         mov [cpu_brand + 0x0c],edx

         mov eax,0x80000003
         cpuid
         mov [cpu_brand + 0x10],eax
         mov [cpu_brand + 0x14],ebx
         mov [cpu_brand + 0x18],ecx
         mov [cpu_brand + 0x1c],edx

         mov eax,0x80000004
         cpuid
         mov [cpu_brand + 0x20],eax
         mov [cpu_brand + 0x24],ebx
         mov [cpu_brand + 0x28],ecx
         mov [cpu_brand + 0x2c],edx

         mov ebx,cpu_brnd0                  ;显示处理器品牌信息
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brand
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brnd1
         call sys_routine_seg_sel:put_string

         ;准备打开分页机制

         ;创建系统内核的页目录表PDT
         ;页目录表清零
         mov ecx,1024                       ;1024个目录项
         mov ebx,0x00020000                 ;页目录的物理地址
         xor esi,esi
  .b1:
         mov dword [es:ebx+esi],0x00000000  ;页目录表项清零
         add esi,4
         loop .b1

         ;在页目录内创建指向页目录自己的目录项
         mov dword [es:ebx+4092],0x00020003

         ;在页目录内创建与线性地址0x00000000对应的目录项
         mov dword [es:ebx+0],0x00021003    ;写入目录项（页表的物理地址和属性）

         ;创建与上面那个目录项相对应的页表，初始化页表项
         mov ebx,0x00021000                 ;页表的物理地址
         xor eax,eax                        ;起始页的物理地址
         xor esi,esi
  .b2:
         mov edx,eax
         or edx,0x00000003
         mov [es:ebx+esi*4],edx             ;登记页的物理地址
         add eax,0x1000                     ;下一个相邻页的物理地址
         inc esi
         cmp esi,256                        ;仅低端1MB内存对应的页才是有效的
         jl .b2

  .b3:                                      ;其余的页表项置为无效
         mov dword [es:ebx+esi*4],0x00000000
         inc esi
         cmp esi,1024
         jl .b3

         ;令CR3寄存器指向页目录，并正式开启页功能
         mov eax,0x00020000                 ;PCD=PWT=0
         mov cr3,eax

         cli

         mov eax,cr0
         or eax,0x80000000
         mov cr0,eax                        ;开启分页机制

         ;在页目录内创建与线性地址0x80000000对应的目录项
         ;mov ebx,0xfffff000                 ;页目录自己的线性地址
         ;mov esi,0x80000000                 ;映射的起始地址
         ;shr esi,22                         ;线性地址的高10位是目录索引
         ;shl esi,2
         ;mov dword [es:ebx+esi],0x00021003  ;写入目录项（页表的物理地址和属性）
                                            ;目标单元的线性地址为0xFFFFF200
         mov dword [es:0xfffff800], 0x00021003

         ;将GDT中的段描述符映射到线性地址0x80000000
         sgdt [pgdt]

         mov ebx,[pgdt+2]

         or dword [es:ebx+0x10+4],0x80000000;处理保护模式下初始代码段描述符
         or dword [es:ebx+0x18+4],0x80000000;处理内核的栈段描述符
         or dword [es:ebx+0x20+4],0x80000000;处理显示缓冲区描述符
         or dword [es:ebx+0x28+4],0x80000000;处理公共例程段描述符
         or dword [es:ebx+0x30+4],0x80000000;处理内核数据段描述符
         or dword [es:ebx+0x38+4],0x80000000;处理内核代码段描述符

         add dword [pgdt+2],0x80000000      ;GDTR也用的是线性地址

         lgdt [pgdt]

         ;修改IDTR，将中断描述符表映射到线性地址高端
         sidt [pidt]
         add dword [pidt+2],0x80000000      ;IDTR也用的是线性地址
         lidt [pidt]

         jmp core_code_seg_sel:flush        ;刷新段寄存器CS，启用高端线性地址

   flush:
         mov eax,core_stack_seg_sel
         mov ss,eax

         mov eax,core_data_seg_sel
         mov ds,eax

         sti

         mov ebx,message_1
         call sys_routine_seg_sel:put_string


         ;以下开始安装为整个系统服务的调用门。特权级之间的控制转移必须使用门
         mov edi,salt                       ;C-SALT表的起始位置 
         mov ecx,salt_items                 ;C-SALT表的条目数量 
  .g0:
         push ecx   
         mov eax,[edi+256]                  ;该条目入口点的32位偏移地址 
         mov bx,[edi+260]                   ;该条目入口点的段选择子 
         mov cx,1_11_0_1100_000_00000B      ;特权级3的调用门(3以上的特权级才
                                            ;允许访问)，0个参数(因为用寄存器
                                            ;传递参数，而没有用栈) 
         call sys_routine_seg_sel:make_gate_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+260],cx                   ;将返回的门描述符选择子回填
         add edi,salt_item_len              ;指向下一个C-SALT条目 
         pop ecx
         loop .g0

         ;对门进行测试 
         mov ebx,message_2
         call far [salt_1+256]              ;通过门显示信息(偏移量将被忽略) 

         ;开始创建和确立内核任务
         mov ecx,core_lin_tcb_addr          ;移至高端之后的内核任务TCB线性地址
         mov word [es:ecx+0x04],0xffff      ;任务的状态为“忙”
         mov dword [es:ecx+0x46],core_lin_alloc_at
                                            ;登记内核中可用于分配的起始线性地址
         call append_to_tcb_link            ;将内核任务的TCB添加到TCB链中

         mov esi,ecx

         ;为内核任务的TSS分配内存空间。所有TSS必须创建在内核空间
         mov ecx,104                        ;为该任务的TSS分配内存
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x14],ecx              ;在内核TCB中保存TSS基地址

         ;在程序管理器的TSS中设置必要的项目
         mov word [es:ecx+0],0              ;反向链=0
         mov eax,cr3
         mov dword [es:ecx+28],eax          ;登记CR3(PDBR)
         mov word [es:ecx+96],0             ;没有LDT。处理器允许没有LDT的任务。
         mov word [es:ecx+100],0            ;T=0
         mov word [es:ecx+102],103          ;没有I/O位图。0特权级事实上不需要。
                                            ;不需要0、1、2特权级堆栈。0特级不
                                            ;会向低特权级转移控制。
         
         ;创建TSS描述符，并安装到GDT中 
         mov eax,ecx                        ;TSS的起始线性地址
         mov ebx,103                        ;段长度（界限）
         mov ecx,0x00008900                 ;TSS描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov word [es:esi+0x18],cx          ;登记TSS选择子到TCB

         ;任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
         ;下面的指令为当前正在执行的0特权级任务“程序管理器”后补手续（TSS）。
         ltr cx

         ;现在可认为“程序管理器”任务正执行中
         mov ebx,core_msg1
         call sys_routine_seg_sel:put_string

         ;以下开始创建用户任务
         mov ecx,0x4a
         call sys_routine_seg_sel:allocate_memory
         mov word [es:ecx+0x04],0           ;任务状态：就绪
         mov dword [es:ecx+0x46],0          ;任务内可用于分配的初始线性地址

         push dword 50                      ;用户程序位于逻辑50扇区
         push ecx                           ;压入任务控制块起始线性地址 
         call load_relocate_program
         call append_to_tcb_link            ;将此TCB添加到TCB链中

         ;可以创建更多的任务，例如：
         mov ecx,0x4a
         call sys_routine_seg_sel:allocate_memory
         mov word [es:ecx+0x04],0           ;任务状态：空闲
         mov dword [es:ecx+0x46],0          ;任务内可用于分配的初始线性地址

         push dword 100                     ;用户程序位于逻辑100扇区
         push ecx                           ;压入任务控制块起始线性地址

         call load_relocate_program
         call append_to_tcb_link            ;将此TCB添加到TCB链中

  .do_switch:
         mov ebx,core_msg2
         call sys_routine_seg_sel:put_string

         ;清理已经终止的任务，并回收它们占用的资源
         call sys_routine_seg_sel:do_task_clean

         hlt

         jmp .do_switch

core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
