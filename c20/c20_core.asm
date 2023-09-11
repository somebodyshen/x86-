         ;代码清单20-2，内核程序
         ;文件名：c20_core.asm
         ;李忠，2022-6-7 22:30

         ;以下常量定义部分。内核的大部分内容都应当固定 
         flat_core_code_seg_sel  equ  0x0008      ;平坦模型下的内核（0特权级）4GB代码段选择子
         flat_core_data_seg_sel  equ  0x0010      ;平坦模型下的内核（0特权级）4GB数据段选择子
         flat_user_code_seg_sel  equ  0x001b      ;平坦模型下的用户（3特权级）4GB代码段选择子
         flat_user_data_seg_sel  equ  0x0023      ;平坦模型下的用户（3特权级）4GB数据段选择子

         idt_linear_address    equ  0x8001f000   ;中断描述符表的线性地址
         core_lin_alloc_at     equ  0x80100000   ;内核中可用于分配的起始线性地址
         core_lin_tcb_addr     equ  0x8001f800   ;内核任务TCB的高端线性地址

;-------------------------------------------------------------------------------
         ;以下是系统核心的头部，用于加载核心程序 
SECTION header vstart=0x80040000

         core_length      dd core_end       ;核心程序总长度#00

         core_entry       dd start          ;核心代码段入口点#04

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vfollows=header          ;系统公共例程代码段
;-------------------------------------------------------------------------------
         ;字符串显示例程（适用于平坦内存模型）
put_string:                                 ;显示0终止的字符串并移动光标
                                            ;输入：EBX=字符串的线性地址

         push ebx
         push ecx

         pushfd
         cli                                ;硬件操作期间，关中断

  .getc:
         mov cl,[ebx]
         or cl,cl                           ;检测串结束标志（0）
         jz .exit                           ;显示完毕，返回
         call put_char
         inc ebx
         jmp .getc

  .exit:

         popfd                              ;硬件操作完毕，恢复原先中断状态

         pop ecx
         pop ebx

         ret

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
         and ebx,0x0000ffff                 ;准备使用32位寻址方式访问显存

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
         shl bx,1
         mov [0x800b8000+ebx],cl

         ;以下将光标位置推进一个字符
         shr bx,1
         inc bx

  .roll_screen:
         cmp bx,2000                        ;光标超出屏幕？滚屏
         jl .set_cursor

         push bx                            ;为了修改原书程序的逻辑问题，新增

         cld
         mov esi,0x800b80a0                 ;小心！32位模式下movsb/w/d
         mov edi,0x800b8000                 ;使用的是esi/edi/ecx
         mov ecx,1920
         rep movsd
         mov bx,3840                        ;清除屏幕最底一行
         mov ecx,80                         ;32位程序应该使用ECX
  .cls:
         mov word[0x800b8000+ebx],0x0720
         add bx,2
         loop .cls

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
read_hard_disk_0:                           ;从硬盘读取一个逻辑扇区（平坦模型）
                                            ;EAX=逻辑扇区号
                                            ;EBX=目标缓冲区线性地址
                                            ;返回：EBX=EBX+512
         cli

         push eax
         push ecx
         push edx

         push eax

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

         pop edx
         pop ecx
         pop eax

         sti

         ret

;-------------------------------------------------------------------------------
;汇编语言程序是极难一次成功，而且调试非常困难。这个例程可以提供帮助
put_hex_dword:                              ;在当前光标处以十六进制形式显示
                                            ;一个双字并推进光标
                                            ;输入：EDX=要转换并显示的数字
                                            ;输出：无
         pushad

         mov ebx,bin_hex                    ;指向核心地址空间内的转换表
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

         popad
         ret

;-------------------------------------------------------------------------------
set_up_gdt_descriptor:                      ;在GDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符
                                            ;输出：CX=描述符的选择子
         push eax
         push ebx
         push edx

         sgdt [pgdt]                        ;取得GDTR的界限和线性地址

         movzx ebx,word [pgdt]              ;GDT界限
         inc bx                             ;GDT总字节数，也是下一个描述符偏移
         add ebx,[pgdt+2]                   ;下一个描述符的线性地址

         mov [ebx],eax
         mov [ebx+4],edx

         add word [pgdt],8                  ;增加一个描述符的大小

         lgdt [pgdt]                        ;对GDT的更改生效

         mov ax,[pgdt]                      ;得到GDT界限值
         xor dx,dx
         mov bx,8
         div bx                             ;除以8，去掉余数
         mov cx,ax
         shl cx,3                           ;将索引号移到正确位置

         pop edx
         pop ebx
         pop eax

         ret
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

         ret

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

         ret

;-------------------------------------------------------------------------------
allocate_a_4k_page:                         ;分配一个4KB的页
                                            ;输入：无
                                            ;输出：EAX=页的物理地址
         push ebx
         push ecx
         push edx

         xor eax,eax
  .b1:
         bts [page_bit_map],eax
         jnc .b2
         inc eax
         cmp eax,page_map_len*8
         jl .b1

         mov ebx,message_3
         call put_string
         hlt                                ;没有可以分配的页，停机

  .b2:
         shl eax,12                         ;乘以4096（0x1000）

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
         mov dword [eax],0x00000000
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
         pop esi
         pop ecx
         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
create_copy_cur_pdir:                       ;创建新页目录，并复制当前页目录内容
                                            ;输入：无
                                            ;输出：EAX=新页目录的物理地址
         push esi
         push edi
         push ebx
         push ecx

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

         ret

;-------------------------------------------------------------------------------
task_alloc_memory:                          ;在指定任务的虚拟内存空间中分配内存
                                            ;输入：EBX=任务控制块TCB的线性地址
                                            ;      ECX=希望分配的字节数
                                            ;输出：ECX=已分配的起始线性地址
         push eax

         push ebx                           ;to A

         ;获得本次内存分配的起始线性地址
         mov ebx,[ebx+0x06]                 ;获得本次分配的起始线性地址
         mov eax,ebx
         add ecx,ebx                        ;本次分配，最后一个字节之后的线性地址

         push ecx                           ;To B

         ;为请求的内存分配页
         and ebx,0xfffff000
         and ecx,0xfffff000
  .next:
         call alloc_inst_a_page
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

         mov [ebx+0x06],ecx                 ;将下次分配可用的线性地址回存到TCB中
         mov ecx,eax

         pop eax

         ret

;-------------------------------------------------------------------------------
allocate_memory:                            ;在当前任务的地址空间中分配内存
                                            ;输入：ECX=希望分配的字节数
                                            ;输出：ECX=所分配内存的起始线性地址
         push eax
         push ebx

         ;得到TCB链表首节点的线性地址
         mov eax,[tcb_chain]                ;EAX=首节点的线性地址

         ;搜索状态为忙（当前任务）的节点
  .s0:
         cmp word [eax+0x04],0xffff
         jz .s1                             ;找到忙的节点，EAX=节点的线性地址
         mov eax,[eax]
         jmp .s0

         ;开始分配内存
  .s1:
         mov ebx,eax
         call task_alloc_memory

         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
resume_task_execute:                        ;恢复指定任务的执行
                                            ;输入：EDI=新任务的TCB的线性地址
         mov eax, [edi + 10]
         mov [tss + 4], eax                 ;用新任务的RSP0设置TSS的RSP0域

         mov eax, [edi + 22]
         mov cr3, eax                       ;恢复新任务的CR3

         mov ds, [edi + 34]
         mov es, [edi + 36]
         mov fs, [edi + 38]
         mov gs, [edi + 40]
         mov eax, [edi + 42]
         mov ebx, [edi + 46]
         mov ecx, [edi + 50]
         mov edx, [edi + 54]
         mov esi, [edi + 58]
         mov ebp, [edi + 66]

         test word [edi + 32], 3           ;SS.RPL=3？
         jnz .to_r3                        ;是的。转.to_r3
         mov esp, [edi + 70]
         mov ss, [edi + 32]
         jmp .do_sw

  .to_r3:
         push dword [edi + 32]             ;SS
         push dword [edi + 70]             ;ESP
  .do_sw:
         push dword [edi + 74]             ;EFLAGS
         push dword [edi + 30]             ;CS
         push dword [edi + 26]             ;EIP

         not word [edi + 0x04]             ;将就绪状态的节点改为忙状态的节点
         mov edi, [edi + 62]

         iretd

;-------------------------------------------------------------------------------
initiate_task_switch:                       ;主动发起任务切换
                                            ;输入：无
                                            ;输出：无。
         push eax
         push ebx
         push esi
         push edi

         mov eax, [tcb_chain]
         cmp eax, 0                         ;链表为空？
         jz .return

         ;搜索状态为忙（当前任务）的节点
  .b0:
         cmp word [eax + 4], 0xffff
         cmove esi, eax                     ;若找到忙的节点，ESI=节点的线性地址
         jz .b1
         mov eax, [eax]
         jmp .b0                            ;循环总能终止，起码内核任务是存在的并且为忙

         ;从当前节点继续搜索就绪任务的节点
  .b1:
         mov ebx, [eax]
         or ebx, ebx                        ;到达链表尾部？
         jz .b2                             ;到链表尾部也未发现就绪节点，从头找
         cmp word [ebx + 4], 0              ;是就绪状态？
         cmove edi, ebx                     ;若找到就绪节点，EDI=节点的线性地址
         jz .b3
         mov eax, ebx
         jmp .b1

  .b2:   ;从链表头寻找就绪任务的节点
         mov ebx, [tcb_chain]               ;EBX=链表首节点线性地址
  .b20:
         cmp word [ebx + 4], 0
         cmove edi, ebx                     ;已找到就绪节点，EDI=节点的线性地址
         jz .b3
         mov ebx, [ebx]
         or ebx, ebx
         jz .return                         ;链表中已经不存在就绪任务，返回
         jmp .b20

  .b3:
         ;保存旧任务的状态
         mov eax, cr3
         mov [esi + 22], eax                ;保存CR3
         ;EAX/EBX/ESI/EDI不用保存，在任务恢复执行时将自动从栈中弹出并恢复
         mov [esi + 50], ecx
         mov [esi + 54], edx
         mov [esi + 66], ebp
         mov [esi + 70], esp
         mov dword [esi + 26], .return      ;恢复执行时的EIP
         mov [esi + 30], cs
         mov [esi + 32], ss
         mov [esi + 34], ds
         mov [esi + 36], es
         mov [esi + 38], fs
         mov [esi + 40], gs
         pushfd
         pop dword [esi + 74]
         not word [esi + 4]                 ;将忙状态的节点改为就绪状态的节点

         jmp resume_task_execute            ;转去恢复并执行新任务

  .return:
         pop edi
         pop esi
         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
terminate_current_task:                     ;终止当前任务
                                            ;注意，执行此例程时，当前任务仍在
                                            ;运行中。此例程其实也是当前任务的
                                            ;一部分 
         mov edi, [tcb_chain]
                                            ;EAX=首节点的线性地址
         ;搜索状态为忙（当前任务）的节点
  .s0:
         cmp word [edi + 4], 0xffff
         jz .s1                             ;找到忙的节点，EAX=节点的线性地址
         mov edi, [edi]
         jmp .s0

         ;将状态为忙的节点改成终止状态
  .s1:
         mov word [edi + 4], 0x3333

         ;搜索就绪状态的任务
         mov edi, [tcb_chain]               ;EBX=链表首节点线性地址
  .s2:
         cmp word [edi + 4], 0x0000
         jz .s3                             ;已找到就绪节点，EBX=节点的线性地址
         mov edi, [edi]
         jmp .s2

         ;就绪任务的节点已经找到，准备切换到该任务
  .s3:
         jmp resume_task_execute            ;转去恢复并执行新任务

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
         call put_string

         cli

         hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:                  ;实时时钟中断处理过程
         push eax

         mov al, 0x20                       ;中断结束命令EOI
         out 0xa0, al                       ;向8259A从片发送
         out 0x20, al                       ;向8259A主片发送

         mov al, 0x0c                       ;寄存器C的索引。且开放NMI
         out 0x70, al
         in al, 0x71                        ;读一下RTC的寄存器C，否则只发生一次中断
                                            ;此处不考虑闹钟和周期性中断的情况
         call initiate_task_switch          ;发起任务切换

         pop eax

         iretd

;-------------------------------------------------------------------------------
do_task_clean:                             ;清理已经终止的任务并回收资源

         ;搜索TCB链表，找到状态为终止的节点
         ;将节点从链表中拆除
         ;回收任务占用的各种资源（可以从它的TCB中找到）

         ret

;-------------------------------------------------------------------------------
int_0x88_handler:                          ;系统调用处理过程

         call [eax * 4 + sys_call]
         iretd

;===============================================================================
SECTION core_data vfollows=sys_routine       ;系统核心的数据段
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

         ;系统调用功能入口
         sys_call         dd put_string
                          dd read_hard_disk_0
                          dd put_hex_dword
                          dd terminate_current_task
                          dd initiate_task_switch
                          dd allocate_memory

         message_0        db 'Setup interrupt system and system-call......', 0

         message_1        db  'Done.', 0x0d, 0x0a, 0

         message_2        db  'TSS is created.', 0x0d, 0x0a, 0

         cpu_brnd0        db  0x0d,0x0a,'  ',0
         cpu_brand  times 52  db 0
         cpu_brnd1        db  0x0d,0x0a,0x0d,0x0a,0

         message_3        db  '********No more pages********',0

         excep_msg        db  '********Exception encounted********',0

         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 

         core_buf   times 2048 db 0         ;内核用的缓冲区

         tcb_chain        dd  0             ;任务控制块链

         tss              times 128 db  0   ;任务状态段

         core_msg1        db  'Core task created.',0x0d,0x0a,0
                 
         core_msg2        db  '[CORE TASK]: I am working!',0x0d,0x0a,0

;===============================================================================
SECTION core_code vfollows=core_data
;-------------------------------------------------------------------------------
load_relocate_program:                      ;加载并重定位用户程序
                                            ;输入: PUSH 逻辑扇区号
                                            ;      PUSH 任务控制块基地址
                                            ;输出：无 
         pushad
      
         mov ebp,esp                        ;为访问通过堆栈传递的参数做准备
      
         ;清空当前页目录的前半部分（对应低2GB的局部地址空间）
         mov ebx,0xfffff000
         xor esi,esi
  .clsp:
         mov dword [ebx+esi*4],0x00000000
         inc esi
         cmp esi,512
         jl .clsp

         mov ebx,cr3                        ;刷新TLB
         mov cr3,ebx

         ;以下开始加载用户程序
         mov eax,[ebp+10*4]                 ;从堆栈中取出用户程序起始扇区号
         mov ebx,core_buf                   ;读取程序头部数据
         call read_hard_disk_0

         ;以下判断整个程序有多大
         mov eax,[core_buf]                 ;程序尺寸
         mov ebx,eax
         and ebx,0xfffffe00                 ;使之512字节对齐（能被512整除的数低
         add ebx,512                        ;9位都为0
         test eax,0x000001ff                ;程序的大小正好是512的倍数吗?
         cmovnz eax,ebx                     ;不是。使用凑整的结果

         mov esi,[ebp+9*4]                  ;从堆栈中取得TCB的基地址

         mov ecx,eax                        ;实际需要申请的内存数量
         mov ebx,esi
         call task_alloc_memory

         mov ebx,ecx                        ;ebx -> 申请到的内存首地址 == 0
         xor edx,edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;总扇区数

         mov eax,[ebp+10*4]                 ;起始扇区号
  .b1:
         call read_hard_disk_0
         inc eax
         loop .b1                           ;循环读，直到读完整个用户程序

         ;为用户任务分配栈空间
         mov ebx,esi                        ;TCB的线性地址
         mov ecx,4096                       ;4KB的空间
         call task_alloc_memory
         mov ecx, [esi + 6]                 ;下一次分配的起始线性地址就是栈顶指针
         mov dword [esi + 70], ecx

         ;创建用于中断和调用门的0特权级栈空间
         mov ebx,esi
         mov ecx,4096                       ;4KB的空间
         call task_alloc_memory
         mov ecx, [esi + 6]                 ;下一次分配的起始线性地址就是栈顶指针
         mov dword [esi + 10], ecx          ;TCB的ESP0域

         ;创建用户任务的页目录
         ;注意！页的分配和使用是由页位图决定的，可以不占用线性地址空间
         call create_copy_cur_pdir
         mov [esi + 22], eax                ;填写TCB的CR3(PDBR)域

         mov word [esi + 30], flat_user_code_seg_sel    ;TCB的CS域
         mov word [esi + 32], flat_user_data_seg_sel    ;TCB的SS域
         mov word [esi + 34], flat_user_data_seg_sel    ;TCB的DS域
         mov word [esi + 36], flat_user_data_seg_sel    ;TCB的ES域
         mov word [esi + 38], flat_user_data_seg_sel    ;TCB的FS域
         mov word [esi + 40], flat_user_data_seg_sel    ;TCB的GS域
         mov eax, [0x04]                    ;从任务的4GB地址空间获取入口点
         mov [esi + 26], eax                ;填写TCB的EIP域
         pushfd
         pop dword [esi + 74]               ;填写TCB的EFLAGS域
         mov word [esi + 4], 0              ;任务状态：就绪

         popad
      
         ret 8                              ;丢弃调用本过程前压入的参数 
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ;在TCB链上追加任务控制块
                                            ;输入：ECX=TCB线性基地址
         push eax
         push edx

         pushfd
         cli

         mov dword [ecx+0x00],0             ;当前TCB指针域清零，以指示这是最
                                            ;后一个TCB
                                             
         mov eax,[tcb_chain]                ;TCB表头指针
         or eax,eax                         ;链表为空？
         jz .notcb 
         
  .searc:
         mov edx,eax
         mov eax,[edx+0x00]
         or eax,eax               
         jnz .searc

         mov [es: edx+0x00],ecx
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain],ecx                ;若为空表，直接令表头指针指向TCB
         
  .retpc:
         popfd

         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:
         ;创建中断描述符表IDT
         mov ebx, message_0
         call put_string

         ;前20个向量是处理器异常使用的
         mov eax,general_exception_handler  ;门代码在段内偏移地址
         mov bx,flat_core_code_seg_sel      ;门代码所在段的选择子
         mov cx,0x8e00                      ;32位中断门，0特权级
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
         xor esi,esi
  .idt0:
         mov [ebx+esi*8],eax
         mov [ebx+esi*8+4],edx
         inc esi
         cmp esi,19                         ;安装前20个异常中断处理过程
         jle .idt0

         ;其余为保留或硬件使用的中断向量
         mov eax,general_interrupt_handler  ;门代码在段内偏移地址
         mov bx,flat_core_code_seg_sel      ;门代码所在段的选择子
         mov cx,0x8e00                      ;32位中断门，0特权级
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
  .idt1:
         mov [ebx+esi*8],eax
         mov [ebx+esi*8+4],edx
         inc esi
         cmp esi,255                        ;安装普通的中断处理过程
         jle .idt1

         ;设置实时时钟中断处理过程
         mov eax,rtm_0x70_interrupt_handle  ;门代码在段内偏移地址
         mov bx,flat_core_code_seg_sel      ;门代码所在段的选择子
         mov cx,0x8e00                      ;32位中断门，0特权级
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
         mov [ebx+0x70*8],eax               ;中断向量：0x70
         mov [ebx+0x70*8+4],edx

         ;设置系统调用中断的处理过程
         mov eax,int_0x88_handler           ;门代码在段内偏移地址
         mov bx,flat_core_code_seg_sel      ;门代码所在段的选择子
         mov cx,0xee00                      ;32位中断门，3特权级!!!!!!
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;中断描述符表的线性地址
         mov [ebx+0x88*8],eax               ;中断向量：0x88
         mov [ebx+0x88*8+4],edx

         ;准备开放中断
         mov word [pidt],256*8-1            ;IDT的界限
         mov dword [pidt+2],idt_linear_address
         lidt [pidt]                        ;加载中断描述符表寄存器IDTR

         ;测试系统调用
         mov ebx,message_1
         mov eax, 0                         ;通过系统调用的0号功能显示信息
         int 0x88                           ;尽管TSS尚未准备好，但不会切换栈

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
         call put_string
         mov ebx,cpu_brand
         call put_string
         mov ebx,cpu_brnd1
         call put_string

         ;创建任务状态段TSS的描述符。整个系统实际上只需要一个TSS即可。
         mov ecx, 32
         xor ebx, ebx
  .clear:
         mov dword [tss + ebx], 0          ;TSS的多数字段已经不用，全部清空。
         add ebx, 4
         loop .clear

         ;因特权级之间的转移而发生栈切换时，本系统只会发生3到0的切换。因此，
         ;只需要TSS中设置SS0，且必须是0特权级的栈段选择子。
         mov word [tss + 8], flat_core_data_seg_sel
         mov word [tss + 102], 103          ;没有I/O许可位图部分

         ;创建TSS描述符，并安装到GDT中
         mov eax,tss                        ;TSS的起始线性地址
         mov ebx,103                        ;段长度（界限）
         mov ecx,0x00008900                 ;TSS描述符，特权级0
         call make_seg_descriptor
         call set_up_gdt_descriptor

         ;令任务寄存器TR指向唯一的TSS并不再改变。
         ltr cx

         mov ebx,message_2
         call put_string

         ;开始创建和确立内核任务
         mov ecx, core_lin_tcb_addr         ;移至高端之后的内核任务TCB线性地址
         mov word [ecx + 4], 0xffff         ;任务的状态为“忙”
         mov dword [ecx + 6], core_lin_alloc_at ;登记内核中可用于分配的起始线性地址
         call append_to_tcb_link            ;将内核任务的TCB添加到TCB链中

         ;现在可认为“程序管理器”任务正执行中
         mov ebx,core_msg1
         call put_string

         ;以下开始创建用户任务
         mov ecx, 128                       ;为TCB分配内存
         call allocate_memory
         mov word [ecx+0x04],0              ;任务状态：就绪
         mov dword [ecx+0x06],0             ;任务内可用于分配的初始线性地址

         push dword 50                      ;用户程序位于逻辑50扇区
         push ecx                           ;压入任务控制块起始线性地址 
         call load_relocate_program
         call append_to_tcb_link            ;将此TCB添加到TCB链中

         ;可以创建更多的任务，例如：
         mov ecx,128                        ;为TCB分配内存
         call allocate_memory
         mov word [ecx+0x04],0              ;任务状态：空闲
         mov dword [ecx+0x06],0             ;任务内可用于分配的初始线性地址

         push dword 100                     ;用户程序位于逻辑100扇区
         push ecx                           ;压入任务控制块起始线性地址

         call load_relocate_program
         call append_to_tcb_link            ;将此TCB添加到TCB链中

  .do_switch:
         mov ebx,core_msg2
         call put_string

         ;清理已经终止的任务，并回收它们占用的资源
         call do_task_clean

         hlt

         jmp .do_switch

;-------------------------------------------------------------------------------
SECTION core_tail
;-------------------------------------------------------------------------------
core_end:
