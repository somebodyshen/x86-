         ;代码清单：20-3，用户程序一
         ;文件名：c20_app0.asm
         ;李忠，创建日期：2022-6-6

;===============================================================================
SECTION header vstart=0

         program_length   dd program_end          ;程序总长度#0x00
         entry_point      dd start                ;程序入口点#0x04
header_end:
  
;===============================================================================
SECTION data vfollows=header

         message_1        db  '[USER TASK]: ,,,,,,,,,,,,,,,,,,,,,,,',0x0d,0x0a,0

         reserved  times 4096*5 db 0                ;保留一个空白区，以演示分页
data_end:

;===============================================================================
      [bits 32]
;===============================================================================
SECTION code vfollows=data
start:
         ;在当前任务内的虚拟地址空间里分配内存
         mov eax, 5
         mov ecx, 128                               ;请求分配128个字节
         int 0x88                                   ;执行系统调用
         mov ebx, ecx                               ;为后面打印字符串准备参数

         ;复制字符串到分配的内存中
         mov esi, message_1
         mov edi, ecx
         mov ecx, reserved-message_1
         cld
         repe movsb

.show:
         mov eax, 0
         int 0x88
         jmp .show

code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end:
