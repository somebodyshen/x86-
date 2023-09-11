         ;代码清单18-3
         ;文件名：c18_app1.asm
         ;文件说明：用户程序 
         ;创建日期：2020-10-30

;===============================================================================
SECTION header vstart=0

         program_length   dd program_end          ;程序总长度#0x00

         head_len         dd header_end           ;程序头部的长度#0x04

         prgentry         dd start                ;程序入口#0x08
         code_seg         dd section.code.start   ;代码段位置#0x0c
         code_len         dd code_end             ;代码段长度#0x10

         data_seg         dd section.data.start   ;数据段位置#0x14
         data_len         dd data_end             ;数据段长度#0x18

         stack_seg        dd section.stack.start  ;栈段位置#0x1c
         stack_len        dd stack_end            ;栈段长度#0x20
;-------------------------------------------------------------------------------
         ;符号地址检索表
         salt_items       dd (header_end-salt)/256 ;#0x24
         
         salt:                                     ;#0x28
         PrintString      db  '@PrintString'
                     times 256-($-PrintString) db 0
                     
         TerminateProgram db  '@TerminateProgram'
                     times 256-($-TerminateProgram) db 0
                     
         ReadDiskData     db  '@ReadDiskData'
                     times 256-($-ReadDiskData) db 0

         InitTaskSwitch   db  '@InitTaskSwitch'
                     times 256-($-InitTaskSwitch) db 0
                 
header_end:
  
;===============================================================================
SECTION data vstart=0                

         message_1        db  '[USER TASK]: CCCCCCCCCCCCCCCCCCCCCCC',0x0d,0x0a,0

data_end:

;===============================================================================
SECTION stack vstart=0

        times 2048        db 0                    ;保留2KB的栈空间

stack_end:

;===============================================================================
      [bits 32]
;===============================================================================
SECTION code vstart=0
start:
         ;任务启动时，DS指向头部段，也不需要设置堆栈 
         mov eax,ds
         mov fs,eax
     
         mov ax,[data_seg]
         mov ds,ax
     
.do_prn:
         mov ebx,message_1
         call far [fs:PrintString]
         jmp .do_prn

         call far [fs:TerminateProgram]      ;退出，并将控制权返回到核心 

code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end:
