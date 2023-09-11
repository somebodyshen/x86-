         ;�����嵥c19-1
         ;�ļ�����c19_core.asm
         ;�ļ�˵��������ģʽ΢�ͺ��ĳ��� 
         ;�������ڣ�2022-06-19

         ;���³������岿�֡��ں˵Ĵ󲿷����ݶ�Ӧ���̶� 
         core_code_seg_sel     equ  0x38    ;�ں˴����ѡ����
         core_data_seg_sel     equ  0x30    ;�ں����ݶ�ѡ���� 
         sys_routine_seg_sel   equ  0x28    ;ϵͳ�������̴���ε�ѡ���� 
         video_ram_seg_sel     equ  0x20    ;��Ƶ��ʾ�������Ķ�ѡ����
         core_stack_seg_sel    equ  0x18    ;�ں˶�ջ��ѡ����
         mem_0_4_gb_seg_sel    equ  0x08    ;����0-4GB�ڴ�Ķε�ѡ����
         idt_linear_address    equ  0x1f000 ;�ж�������������Ե�ַ
         core_lin_alloc_at     equ  0x80100000
                                            ;�ں��п����ڷ������ʼ���Ե�ַ
         core_lin_tcb_addr     equ  0x8001f800
                                            ;�ں�����TCB�ĸ߶����Ե�ַ

;-------------------------------------------------------------------------------
         ;������ϵͳ���ĵ�ͷ�������ڼ��غ��ĳ��� 
         core_length      dd core_end       ;���ĳ����ܳ���#00

         sys_routine_seg  dd section.sys_routine.start
                                            ;ϵͳ�������̶�λ��#04

         core_data_seg    dd section.core_data.start
                                            ;�������ݶ�λ��#08

         core_code_seg    dd section.core_code.start
                                            ;���Ĵ����λ��#0c


         core_entry       dd start          ;���Ĵ������ڵ�#10
                          dw core_code_seg_sel

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vstart=0                ;ϵͳ�������̴���� 
;-------------------------------------------------------------------------------
         ;�ַ�����ʾ����
put_string:                                 ;��ʾ0��ֹ���ַ������ƶ���� 
                                            ;���룺DS:EBX=����ַ
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
         retf                               ;�μ䷵��

;-------------------------------------------------------------------------------
put_char:                                   ;�ڵ�ǰ��괦��ʾһ���ַ�,���ƽ�
                                            ;��ꡣ�����ڶ��ڵ���
                                            ;���룺CL=�ַ�ASCII��
         pushad

         ;����ȡ��ǰ���λ��
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         inc dx                             ;0x3d5
         in al,dx                           ;����
         mov ah,al

         dec dx                             ;0x3d4
         mov al,0x0f
         out dx,al
         inc dx                             ;0x3d5
         in al,dx                           ;����
         mov bx,ax                          ;BX=������λ�õ�16λ��

         cmp cl,0x0d                        ;�س�����
         jnz .put_0a
         mov ax,bx
         mov bl,80
         div bl
         mul bl
         mov bx,ax
         jmp .set_cursor

  .put_0a:
         cmp cl,0x0a                        ;���з���
         jnz .put_other
         add bx,80
         jmp .roll_screen

  .put_other:                               ;������ʾ�ַ�
         push es
         mov eax,video_ram_seg_sel          ;0xb8000�ε�ѡ����
         mov es,eax
         shl bx,1
         mov [es:bx],cl
         pop es

         ;���½����λ���ƽ�һ���ַ�
         shr bx,1
         inc bx

  .roll_screen:
         cmp bx,2000                        ;��곬����Ļ������
         jl .set_cursor

         push bx                            ;Ϊ���޸�ԭ�������߼����⣬����
         push ds
         push es
         mov eax,video_ram_seg_sel
         mov ds,eax
         mov es,eax
         cld
         mov esi,0xa0                       ;С�ģ�32λģʽ��movsb/w/d
         mov edi,0x00                       ;ʹ�õ���esi/edi/ecx
         mov ecx,1920
         rep movsw
         mov bx,3840                        ;�����Ļ���һ��
         mov ecx,80                         ;32λ����Ӧ��ʹ��ECX
  .cls:
         mov word[es:bx],0x0720
         add bx,2
         loop .cls

         pop es
         pop ds

         ;mov bx,1920                       ;Ϊ���޸�ԭ�������߼����⣬ɾ��
         pop bx                             ;Ϊ���޸�ԭ�������߼����⣬����
         sub bx,80                          ;Ϊ���޸�ԭ�������߼����⣬����

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
read_hard_disk_0:                           ;��Ӳ�̶�ȡһ���߼�����
                                            ;EAX=�߼�������
                                            ;DS:EBX=Ŀ�껺������ַ
                                            ;���أ�EBX=EBX+512
         push eax 
         push ecx
         push edx
      
         push eax

         cli

         mov dx,0x1f2
         mov al,1
         out dx,al                          ;��ȡ��������

         inc dx                             ;0x1f3
         pop eax
         out dx,al                          ;LBA��ַ7~0

         inc dx                             ;0x1f4
         mov cl,8
         shr eax,cl
         out dx,al                          ;LBA��ַ15~8

         inc dx                             ;0x1f5
         shr eax,cl
         out dx,al                          ;LBA��ַ23~16

         inc dx                             ;0x1f6
         shr eax,cl
         or al,0xe0                         ;��һӲ��  LBA��ַ27~24
         out dx,al

         inc dx                             ;0x1f7
         mov al,0x20                        ;������
         out dx,al

  .waits:
         in al,dx
         and al,0x88
         cmp al,0x08
         jnz .waits                         ;��æ����Ӳ����׼�������ݴ��� 

         mov ecx,256                        ;�ܹ�Ҫ��ȡ������
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
      
         retf                               ;�μ䷵�� 

;-------------------------------------------------------------------------------
;������Գ����Ǽ���һ�γɹ������ҵ��Էǳ����ѡ�������̿����ṩ���� 
put_hex_dword:                              ;�ڵ�ǰ��괦��ʮ��������ʽ��ʾ
                                            ;һ��˫�ֲ��ƽ���� 
                                            ;���룺EDX=Ҫת������ʾ������
                                            ;�������
         pushad
         push ds
      
         mov ax,core_data_seg_sel           ;�л����������ݶ� 
         mov ds,ax
      
         mov ebx,bin_hex                    ;ָ��������ݶ��ڵ�ת����
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
allocate_a_4k_page:                         ;����һ��4KB��ҳ
                                            ;���룺��
                                            ;�����EAX=ҳ�������ַ
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
         hlt                                ;û�п��Է����ҳ��ͣ��

  .b2:
         shl eax,12                         ;����4096��0x1000��

         pop ds
         pop edx
         pop ecx
         pop ebx

         ret

;-------------------------------------------------------------------------------
alloc_inst_a_page:                          ;����һ��ҳ������װ�ڵ�ǰ���
                                            ;�㼶��ҳ�ṹ��
                                            ;���룺EBX=ҳ�����Ե�ַ
         push eax
         push ebx
         push ecx
         push esi
         push ds

         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax

         ;�������Ե�ַ����Ӧ��ҳ���Ƿ����
         mov esi,ebx
         and esi,0xffc00000                 ;���ҳ��������ҳ��ƫ�Ʋ���
         shr esi,20                         ;��ҳĿ¼��������4��Ϊҳ��ƫ��
         or esi,0xfffff000                  ;ҳĿ¼��������Ե�ַ+����ƫ��

         test dword [esi],0x00000001        ;Pλ�Ƿ�Ϊ��1�����������Ե�ַ��
         jnz .b1                            ;���Ѿ��ж�Ӧ��ҳ��

         ;��������װ�����Ե�ַ����Ӧ��ҳ��
         call allocate_a_4k_page            ;����һ��ҳ��Ϊҳ��
         or eax,0x00000007
         mov [esi],eax                      ;��ҳĿ¼�еǼǸ�ҳ��

         ;��յ�ǰҳ��
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
         ;�������Ե�ַ��Ӧ��ҳ���ҳ���Ƿ����
         mov esi,ebx
         and esi,0xfffff000                 ;���ҳ��ƫ�Ʋ���
         shr esi,10                         ;��ҳĿ¼�������ҳ��������ҳ����������4��Ϊҳ��ƫ��
         or esi,0xffc00000                  ;�õ������Ե�ַ��Ӧ��ҳ����

         test dword [esi],0x00000001        ;Pλ�Ƿ�Ϊ��1�����������Ե�ַ��
         jnz .b2                            ;���Ѿ��ж�Ӧ��ҳ

         ;��������װ�����Ե�ַ����Ӧ��ҳ
         call allocate_a_4k_page            ;����һ��ҳ�������Ҫ��װ��ҳ
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
create_copy_cur_pdir:                       ;������ҳĿ¼�������Ƶ�ǰҳĿ¼����
                                            ;���룺��
                                            ;�����EAX=��ҳĿ¼�������ַ
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

         mov esi,0xfffff000                 ;ESI->��ǰҳĿ¼�����Ե�ַ
         mov edi,0xffffe000                 ;EDI->��ҳĿ¼�����Ե�ַ
         mov ecx,1024                       ;ECX=Ҫ���Ƶ�Ŀ¼����
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
task_alloc_memory:                          ;��ָ������������ڴ�ռ��з����ڴ�
                                            ;���룺EBX=������ƿ�TCB�����Ե�ַ
                                            ;      ECX=ϣ��������ֽ���
                                            ;�����ECX=�ѷ������ʼ���Ե�ַ
         push eax

         push ds

         push ebx                           ;to A

         ;��ñ����ڴ�������ʼ���Ե�ַ
         mov ax,mem_0_4_gb_seg_sel
         mov ds,ax

         mov ebx,[ebx+0x46]                 ;��ñ��η������ʼ���Ե�ַ
         mov eax,ebx
         add ecx,ebx                        ;���η��䣬���һ���ֽ�֮������Ե�ַ

         push ecx                           ;to B

         ;Ϊ������ڴ����ҳ
         and ebx,0xfffff000
         and ecx,0xfffff000
  .next:
         call sys_routine_seg_sel:alloc_inst_a_page
                                            ;��װ��ǰ���Ե�ַ���ڵ�ҳ
         add ebx,0x1000                     ;+4096
         cmp ebx,ecx
         jle .next

         ;��������һ�η�������Ե�ַǿ�ư�4�ֽڶ���
         pop ecx                            ;B

         test ecx,0x00000003                ;���Ե�ַ��4�ֽڶ������
         jz .algn                           ;�ǣ�ֱ�ӷ���
         add ecx,4                          ;��ǿ�ư�4�ֽڶ���
         and ecx,0xfffffffc

  .algn:
         pop ebx                            ;A

         mov [ebx+0x46],ecx                 ;���´η�����õ����Ե�ַ�ش浽TCB��
         mov ecx,eax

         pop ds

         pop eax

         retf

;-------------------------------------------------------------------------------
allocate_memory:                            ;�ڵ�ǰ����ĵ�ַ�ռ��з����ڴ�
                                            ;���룺ECX=ϣ��������ֽ���
                                            ;�����ECX=��ʼ���Ե�ַ 
         push eax
         push ebx

         push ds

         ;�õ�TCB�����׽ڵ�����Ե�ַ
         mov eax,core_data_seg_sel
         mov ds,eax

         mov eax,[tcb_chain]                ;EAX=�׽ڵ�����Ե�ַ

         mov ebx,mem_0_4_gb_seg_sel
         mov ds,ebx

         ;����״̬Ϊæ����ǰ���񣩵Ľڵ�
  .s0:
         cmp word [eax+0x04],0xffff
         jz .s1                             ;�ҵ�æ�Ľڵ㣬EAX=�ڵ�����Ե�ַ
         mov eax,[eax]
         jmp .s0

         ;��ʼ�����ڴ�
  .s1:
         mov ebx,eax
         call sys_routine_seg_sel:task_alloc_memory

         pop ds

         pop ebx
         pop eax

         retf

;-------------------------------------------------------------------------------
set_up_gdt_descriptor:                      ;��GDT�ڰ�װһ���µ�������
                                            ;���룺EDX:EAX=������ 
                                            ;�����CX=��������ѡ����
         push eax
         push ebx
         push edx

         push ds
         push es

         mov ebx,core_data_seg_sel          ;�л����������ݶ�
         mov ds,ebx

         sgdt [pgdt]                        ;�Ա㿪ʼ����GDT

         mov ebx,mem_0_4_gb_seg_sel
         mov es,ebx

         movzx ebx,word [pgdt]              ;GDT����
         inc bx                             ;GDT���ֽ�����Ҳ����һ��������ƫ��
         add ebx,[pgdt+2]                   ;��һ�������������Ե�ַ

         mov [es:ebx],eax
         mov [es:ebx+4],edx

         add word [pgdt],8                  ;����һ���������Ĵ�С

         lgdt [pgdt]                        ;��GDT�ĸ�����Ч

         mov ax,[pgdt]                      ;�õ�GDT����ֵ
         xor dx,dx
         mov bx,8
         div bx                             ;����8��ȥ������
         mov cx,ax
         shl cx,3                           ;���������Ƶ���ȷλ��

         pop es
         pop ds

         pop edx
         pop ebx
         pop eax

         retf
;-------------------------------------------------------------------------------
make_seg_descriptor:                        ;����洢����ϵͳ�Ķ�������
                                            ;���룺EAX=���Ի���ַ
                                            ;      EBX=�ν���
                                            ;      ECX=���ԡ�������λ����ԭʼ
                                            ;          λ�ã��޹ص�λ���� 
                                            ;���أ�EDX:EAX=������
         mov edx,eax
         shl eax,16
         or ax,bx                           ;������ǰ32λ(EAX)�������

         and edx,0xffff0000                 ;�������ַ���޹ص�λ
         rol edx,8
         bswap edx                          ;װ���ַ��31~24��23~16  (80486+)

         xor bx,bx
         or edx,ebx                         ;װ��ν��޵ĸ�4λ

         or edx,ecx                         ;װ������

         retf

;-------------------------------------------------------------------------------
make_gate_descriptor:                       ;�����ŵ��������������ŵȣ�
                                            ;���룺EAX=�Ŵ����ڶ���ƫ�Ƶ�ַ
                                            ;       BX=�Ŵ������ڶε�ѡ���� 
                                            ;       CX=�����ͼ����Եȣ�����
                                            ;          ��λ����ԭʼλ�ã�
                                            ;���أ�EDX:EAX=������������
         push ebx
         push ecx
      
         mov edx,eax
         and edx,0xffff0000                 ;�õ�ƫ�Ƶ�ַ��16λ 
         or dx,cx                           ;��װ���Բ��ֵ�EDX
       
         and eax,0x0000ffff                 ;�õ�ƫ�Ƶ�ַ��16λ 
         shl ebx,16                          
         or eax,ebx                         ;��װ��ѡ���Ӳ���
      
         pop ecx
         pop ebx
      
         retf                                   
                             
;-------------------------------------------------------------------------------
initiate_task_switch:                       ;�������������л�
                                            ;���룺��
                                            ;������ޡ�
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

         ;����״̬Ϊæ����ǰ���񣩵Ľڵ�
  .b0:
         cmp word [eax+0x04],0xffff
         cmove esi,eax                     ;�ҵ�æ�Ľڵ㣬ESI=�ڵ�����Ե�ַ
         jz .b1
         mov eax,[eax]
         jmp .b0

         ;�ӵ�ǰ�ڵ����������������Ľڵ�
  .b1:
         mov ebx,[eax]
         or ebx,ebx
         jz .b2                            ;������β��Ҳδ���־����ڵ㣬��ͷ��
         cmp word [ebx+0x04],0x0000
         cmove edi,ebx                     ;���ҵ������ڵ㣬EDI=�ڵ�����Ե�ַ
         jz .b3
         mov eax,ebx
         jmp .b1

  .b2:
         mov ebx,[es:tcb_chain]            ;EBX=�����׽ڵ����Ե�ַ
  .b20:
         cmp word [ebx+0x04],0x0000
         cmove edi,ebx                     ;���ҵ������ڵ㣬EDI=�ڵ�����Ե�ַ
         jz .b3
         mov ebx,[ebx]
         or ebx,ebx
         jz .return                        ;�������Ѿ������ھ������񣬷���
         jmp .b20

         ;��������Ľڵ��Ѿ��ҵ���׼���л���������
  .b3:
         not word [esi+0x04]               ;��æ״̬�Ľڵ��Ϊ����״̬�Ľڵ�
         not word [edi+0x04]               ;������״̬�Ľڵ��Ϊæ״̬�Ľڵ�
         jmp far [edi+0x14]                ;�����л�

  .return:
         pop es
         pop ds
         popad

         retf

;-------------------------------------------------------------------------------
terminate_current_task:                     ;��ֹ��ǰ����
                                            ;ע�⣬ִ�д�����ʱ����ǰ��������
                                            ;�����С���������ʵҲ�ǵ�ǰ�����
                                            ;һ���� 
         mov eax,core_data_seg_sel
         mov es,eax

         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax

         mov eax,[es:tcb_chain]
                                            ;EAX=�׽ڵ�����Ե�ַ
         ;����״̬Ϊæ����ǰ���񣩵Ľڵ�
  .s0:
         cmp word [eax+0x04],0xffff
         jz .s1                             ;�ҵ�æ�Ľڵ㣬EAX=�ڵ�����Ե�ַ
         mov eax,[eax]
         jmp .s0

         ;��״̬Ϊæ�Ľڵ�ĳ���ֹ״̬
  .s1:
         mov word [eax+0x04],0x3333

         ;��������״̬������
         mov ebx,[es:tcb_chain]            ;EBX=�����׽ڵ����Ե�ַ
  .s2:
         cmp word [ebx+0x04],0x0000
         jz .s3                            ;���ҵ������ڵ㣬EBX=�ڵ�����Ե�ַ
         mov ebx,[ebx]
         jmp .s2

         ;��������Ľڵ��Ѿ��ҵ���׼���л���������
  .s3:
         not word [ebx+0x04]               ;������״̬�Ľڵ��Ϊæ״̬�Ľڵ�
         jmp far [ebx+0x14]                ;�����л�

;-------------------------------------------------------------------------------
general_interrupt_handler:                  ;ͨ�õ��жϴ������
         push eax

         mov al,0x20                        ;�жϽ�������EOI
         out 0xa0,al                        ;���Ƭ����
         out 0x20,al                        ;����Ƭ����

         pop eax

         iretd

;-------------------------------------------------------------------------------
general_exception_handler:                  ;ͨ�õ��쳣�������
         mov ebx,excep_msg
         call sys_routine_seg_sel:put_string

         cli

         hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:                  ;ʵʱʱ���жϴ������

         pushad

         mov al,0x20                        ;�жϽ�������EOI
         out 0xa0,al                        ;��8259A��Ƭ����
         out 0x20,al                        ;��8259A��Ƭ����

         mov al,0x0c                        ;�Ĵ���C���������ҿ���NMI
         out 0x70,al
         in al,0x71                         ;��һ��RTC�ļĴ���C������ֻ����һ���ж�
                                            ;�˴����������Ӻ��������жϵ����
         ;�����������
         call sys_routine_seg_sel:initiate_task_switch

         popad

         iretd

;-------------------------------------------------------------------------------
do_task_clean:                             ;�����Ѿ���ֹ�����񲢻�����Դ

         ;����TCB�����ҵ�״̬Ϊ��ֹ�Ľڵ�
         ;���ڵ�������в��
         ;��������ռ�õĸ�����Դ�����Դ�����TCB���ҵ���

         retf

sys_routine_end:

;===============================================================================
SECTION core_data vstart=0                  ;ϵͳ���ĵ����ݶ� 
;------------------------------------------------------------------------------- 
         pgdt             dw  0             ;�������ú��޸�GDT 
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

         ;���ŵ�ַ������
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
                                            ;put_hex_dword�ӹ����õĲ��ұ� 

         core_buf   times 2048 db 0         ;�ں��õĻ�����

         ;������ƿ���
         tcb_chain        dd  0

         core_msg1        db  '  Core task created.',0x0d,0x0a,0
                 
         core_msg2        db  '[CORE TASK]: I am working!',0x0d,0x0a,0

core_data_end:
               
;===============================================================================
SECTION core_code vstart=0
;-------------------------------------------------------------------------------
fill_descriptor_in_ldt:                     ;��LDT�ڰ�װһ���µ�������
                                            ;���룺EDX:EAX=������
                                            ;          EBX=TCB����ַ
                                            ;�����CX=��������ѡ����
         push eax
         push edx
         push edi
         push ds

         mov ecx,mem_0_4_gb_seg_sel
         mov ds,ecx

         mov edi,[ebx+0x0c]                 ;���LDT����ַ
         
         xor ecx,ecx
         mov cx,[ebx+0x0a]                  ;���LDT����
         inc cx                             ;LDT�����ֽ���������������ƫ�Ƶ�ַ
         
         mov [edi+ecx+0x00],eax
         mov [edi+ecx+0x04],edx             ;��װ������

         add cx,8                           
         dec cx                             ;�õ��µ�LDT����ֵ 

         mov [ebx+0x0a],cx                  ;����LDT����ֵ��TCB

         mov ax,cx
         xor dx,dx
         mov cx,8
         div cx
         
         mov cx,ax
         shl cx,3                           ;����3λ������
         or cx,0000_0000_0000_0100B         ;ʹTIλ=1��ָ��LDT�����ʹRPL=00 

         pop ds
         pop edi
         pop edx
         pop eax
     
         ret
         
;------------------------------------------------------------------------------- 
load_relocate_program:                      ;���ز��ض�λ�û�����
                                            ;����: PUSH �߼�������
                                            ;      PUSH ������ƿ����ַ
                                            ;������� 
         pushad
      
         push ds
         push es
      
         mov ebp,esp                        ;Ϊ����ͨ����ջ���ݵĲ�����׼��
      
         mov ecx,mem_0_4_gb_seg_sel
         mov es,ecx

         ;��յ�ǰҳĿ¼��ǰ�벿�֣���Ӧ��2GB�ľֲ���ַ�ռ䣩
         mov ebx,0xfffff000
         xor esi,esi
  .clsp:
         mov dword [es:ebx+esi*4],0x00000000
         inc esi
         cmp esi,512
         jl .clsp

         mov ebx,cr3                        ;ˢ��TLB
         mov cr3,ebx

         mov esi,[ebp+11*4]                 ;�Ӷ�ջ��ȡ��TCB�Ļ���ַ

         ;�������봴��LDT����Ҫ���ڴ�
         mov ebx,esi
         mov ecx,160                        ;����װ20��LDT������
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x0c],ecx              ;�Ǽ�LDT����ַ��TCB��
         mov word [es:esi+0x0a],0xffff      ;�Ǽ�LDT��ʼ�Ľ��޵�TCB�� 

         ;���¿�ʼ�����û����� 
         mov eax,core_data_seg_sel
         mov ds,eax                         ;�л�DS���ں����ݶ�
       
         mov eax,[ebp+12*4]                 ;�Ӷ�ջ��ȡ���û�������ʼ������ 
         mov ebx,core_buf                   ;��ȡ����ͷ������     
         call sys_routine_seg_sel:read_hard_disk_0

         ;�����ж����������ж��
         mov eax,[core_buf]                 ;����ߴ�
         mov ebx,eax
         and ebx,0xfffffe00                 ;ʹ֮512�ֽڶ��루�ܱ�512���������� 
         add ebx,512                        ;9λ��Ϊ0 
         test eax,0x000001ff                ;����Ĵ�С������512�ı�����? 
         cmovnz eax,ebx                     ;���ǡ�ʹ�ô����Ľ��
      
         mov ecx,eax                        ;ʵ����Ҫ������ڴ�����
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x06],ecx              ;�Ǽǳ�����ػ���ַ��TCB��
      
         mov ebx,ecx                        ;ebx -> ���뵽���ڴ��׵�ַ
         xor edx,edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;�������� 
      
         mov eax,mem_0_4_gb_seg_sel         ;�л�DS��0-4GB�Ķ�
         mov ds,eax

         mov eax,[ebp+12*4]                 ;��ʼ������ 
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                           ;ѭ������ֱ�����������û�����

         mov edi,[es:esi+0x06]              ;��ó�����ػ���ַ

         ;��������ͷ����������
         mov eax,edi                        ;����ͷ����ʼ���Ե�ַ
         mov ebx,[edi+0x04]                 ;�γ���
         dec ebx                            ;�ν���
         mov ecx,0x0040f200                 ;�ֽ����ȵ����ݶ�����������Ȩ��3 
         call sys_routine_seg_sel:make_seg_descriptor
      
         ;��װͷ������������LDT�� 
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt

         or cx,0000_0000_0000_0011B         ;����ѡ���ӵ���Ȩ��Ϊ3
         mov [es:esi+0x44],cx               ;�Ǽǳ���ͷ����ѡ���ӵ�TCB 
         mov [edi+0x04],cx                  ;��ͷ���� 
      
         ;������������������
         mov eax,edi
         add eax,[edi+0x0c]                 ;������ʼ���Ե�ַ
         mov ebx,[edi+0x10]                 ;�γ���
         dec ebx                            ;�ν���
         mov ecx,0x0040f800                 ;�ֽ����ȵĴ��������������Ȩ��3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;����ѡ���ӵ���Ȩ��Ϊ3
         mov [edi+0x0c],cx                  ;�ǼǴ����ѡ���ӵ�ͷ��

         ;�����������ݶ�������
         mov eax,edi
         add eax,[edi+0x14]                 ;���ݶ���ʼ���Ե�ַ
         mov ebx,[edi+0x18]                 ;�γ���
         dec ebx                            ;�ν��� 
         mov ecx,0x0040f200                 ;�ֽ����ȵ����ݶ�����������Ȩ��3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;����ѡ���ӵ���Ȩ��Ϊ3
         mov [edi+0x14],cx                  ;�Ǽ����ݶ�ѡ���ӵ�ͷ��

         ;���������ջ��������
         mov eax,edi
         add eax,[edi+0x1c]                 ;���ݶ���ʼ���Ե�ַ
         mov ebx,[edi+0x20]                 ;�γ���
         dec ebx                            ;�ν���
         mov ecx,0x0040f200                 ;�ֽ����ȵĶ�ջ������������Ȩ��3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;����ѡ���ӵ���Ȩ��Ϊ3
         mov [edi+0x1c],cx                  ;�ǼǶ�ջ��ѡ���ӵ�ͷ��

         ;�ض�λSALT 
         mov eax,mem_0_4_gb_seg_sel         ;�����ǰһ�²�ͬ��ͷ����������
         mov es,eax                         ;�Ѱ�װ������û����Ч����ֻ��ͨ
                                            ;��4GB�η����û�����ͷ��          
         mov eax,core_data_seg_sel
         mov ds,eax
      
         cld

         mov ecx,[es:edi+0x24]              ;U-SALT��Ŀ��(ͨ������4GB��ȡ��) 
         add edi,0x28                       ;U-SALT��4GB���ڵ�ƫ�� 
  .b2: 
         push ecx
         push edi
      
         mov ecx,salt_items
         mov esi,salt
  .b3:
         push edi
         push esi
         push ecx

         mov ecx,64                         ;�������У�ÿ��Ŀ�ıȽϴ��� 
         repe cmpsd                         ;ÿ�αȽ�4�ֽ� 
         jnz .b4
         mov eax,[esi]                      ;��ƥ�䣬��esiǡ��ָ�����ĵ�ַ
         mov [es:edi-256],eax               ;���ַ�����д��ƫ�Ƶ�ַ 
         mov ax,[esi+4]
         or ax,0000000000000011B            ;���û������Լ�����Ȩ��ʹ�õ�����
                                            ;��RPL=3 
         mov [es:edi-252],ax                ;���������ѡ���� 
  .b4:
      
         pop ecx
         pop esi
         add esi,salt_item_len
         pop edi                            ;��ͷ�Ƚ� 
         loop .b3
      
         pop edi
         add edi,256
         pop ecx
         loop .b2

         mov esi,[ebp+11*4]                 ;�Ӷ�ջ��ȡ��TCB�Ļ���ַ

         ;����0��Ȩ��ջ
         mov ecx,0                          ;��4KBΪ��λ��ջ�ν���ֵ
         mov [es:esi+0x1a],ecx              ;�Ǽ�0��Ȩ��ջ���޵�TCB
         inc ecx
         shl ecx,12                         ;����4096���õ��δ�С
         push ecx
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x1e],ecx              ;�Ǽ�0��Ȩ��ջ����ַ��TCB
         mov eax,ecx
         mov ebx,[es:esi+0x1a]              ;�γ��ȣ����ޣ�
         mov ecx,0x00c09200                 ;4KB���ȣ���д����Ȩ��0
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt
         ;or cx,0000_0000_0000_0000          ;����ѡ���ӵ���Ȩ��Ϊ0
         mov [es:esi+0x22],cx               ;�Ǽ�0��Ȩ����ջѡ���ӵ�TCB
         pop dword [es:esi+0x24]            ;�Ǽ�0��Ȩ����ջ��ʼESP��TCB

         ;����1��Ȩ����ջ
         mov ecx,0
         mov [es:esi+0x28],ecx              ;�Ǽ�1��Ȩ����ջ�ߴ絽TCB
         inc ecx
         shl ecx,12                         ;����4096���õ��δ�С
         push ecx
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x2c],ecx              ;�Ǽ�1��Ȩ����ջ����ַ��TCB
         mov eax,ecx
         mov ebx,[es:esi+0x28]              ;�γ��ȣ����ޣ�
         mov ecx,0x00c0b200                 ;4KB���ȣ���д����Ȩ��1
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0001          ;����ѡ���ӵ���Ȩ��Ϊ1
         mov [es:esi+0x30],cx               ;�Ǽ�1��Ȩ����ջѡ���ӵ�TCB
         pop dword [es:esi+0x32]            ;�Ǽ�1��Ȩ����ջ��ʼESP��TCB

         ;����2��Ȩ����ջ
         mov ecx,0
         mov [es:esi+0x36],ecx              ;�Ǽ�2��Ȩ����ջ�ߴ絽TCB
         inc ecx
         shl ecx,12                         ;����4096���õ��δ�С
         push ecx
         mov ebx,esi
         call sys_routine_seg_sel:task_alloc_memory
         mov [es:esi+0x3a],ecx              ;�Ǽ�2��Ȩ����ջ����ַ��TCB
         mov eax,ecx
         mov ebx,[es:esi+0x36]              ;�γ��ȣ����ޣ�
         mov ecx,0x00c0d200                 ;4KB���ȣ���д����Ȩ��2
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB�Ļ���ַ
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0010          ;����ѡ���ӵ���Ȩ��Ϊ2
         mov [es:esi+0x3e],cx               ;�Ǽ�2��Ȩ����ջѡ���ӵ�TCB
         pop dword [es:esi+0x40]            ;�Ǽ�2��Ȩ����ջ��ʼESP��TCB

         ;��GDT�еǼ�LDT������
         mov eax,[es:esi+0x0c]              ;LDT����ʼ���Ե�ַ
         movzx ebx,word [es:esi+0x0a]       ;LDT�ν���
         mov ecx,0x00008200                 ;LDT����������Ȩ��0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x10],cx               ;�Ǽ�LDTѡ���ӵ�TCB��
       
         ;�����û������TSS
         mov ecx,104                        ;tss�Ļ����ߴ�
         mov [es:esi+0x12],cx              
         dec word [es:esi+0x12]             ;�Ǽ�TSS����ֵ��TCB 
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x14],ecx              ;�Ǽ�TSS����ַ��TCB
      
         ;�Ǽǻ�����TSS�������
         mov word [es:ecx+0],0              ;������=0

         mov edx,[es:esi+0x24]              ;�Ǽ�0��Ȩ����ջ��ʼESP
         mov [es:ecx+4],edx                 ;��TSS��

         mov dx,[es:esi+0x22]               ;�Ǽ�0��Ȩ����ջ��ѡ����
         mov [es:ecx+8],dx                  ;��TSS��

         mov edx,[es:esi+0x32]              ;�Ǽ�1��Ȩ����ջ��ʼESP
         mov [es:ecx+12],edx                ;��TSS��

         mov dx,[es:esi+0x30]               ;�Ǽ�1��Ȩ����ջ��ѡ����
         mov [es:ecx+16],dx                 ;��TSS��

         mov edx,[es:esi+0x40]              ;�Ǽ�2��Ȩ����ջ��ʼESP
         mov [es:ecx+20],edx                ;��TSS��

         mov dx,[es:esi+0x3e]               ;�Ǽ�2��Ȩ����ջ��ѡ����
         mov [es:ecx+24],dx                 ;��TSS��

         mov dx,[es:esi+0x10]               ;�Ǽ������LDTѡ����
         mov [es:ecx+96],dx                 ;��TSS��

         mov dx,[es:esi+0x12]               ;�Ǽ������I/Oλͼƫ��
         mov [es:ecx+102],dx                ;��TSS��

         mov word [es:ecx+100],0            ;T=0

         ;�����û�����ͷ������ȡ�������TSS 
         mov ebx,[ebp+11*4]                 ;�Ӷ�ջ��ȡ��TCB�Ļ���ַ
         mov edi,[es:ebx+0x06]              ;�û�������صĻ���ַ 

         mov edx,[es:edi+0x08]              ;�Ǽǳ�����ڵ㣨EIP��
         mov [es:ecx+32],edx                ;��TSS

         mov dx,[es:edi+0x0c]               ;�Ǽǳ������Σ�CS��ѡ����
         mov [es:ecx+76],dx                 ;��TSS��

         mov dx,[es:edi+0x1c]               ;�Ǽǳ����ջ�Σ�SS��ѡ����
         mov [es:ecx+80],dx                 ;��TSS��

         mov edx,[es:edi+0x20]              ;��ջ�ĸ߶����Ե�ַ
         mov [es:ecx+56],edx                ;��дTSS��ESP��

         mov dx,[es:edi+0x04]               ;�Ǽǳ������ݶΣ�DS��ѡ����
         mov word [es:ecx+84],dx            ;��TSS�С�ע�⣬��ָ�����ͷ����
      
         mov word [es:ecx+72],0             ;TSS�е�ES=0

         mov word [es:ecx+88],0             ;TSS�е�FS=0

         mov word [es:ecx+92],0             ;TSS�е�GS=0

         pushfd
         pop dword [es:ecx+36]              ;EFLAGS

         ;��GDT�еǼ�TSS������
         mov eax,[es:esi+0x14]              ;TSS����ʼ���Ե�ַ
         movzx ebx,word [es:esi+0x12]       ;�γ��ȣ����ޣ�
         mov ecx,0x00008900                 ;TSS����������Ȩ��0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x18],cx               ;�Ǽ�TSSѡ���ӵ�TCB

         ;�����û������ҳĿ¼
         ;ע�⣡ҳ�ķ����ʹ������ҳλͼ�����ģ����Բ�ռ�����Ե�ַ�ռ�
         call sys_routine_seg_sel:create_copy_cur_pdir
         mov ebx,[es:esi+0x14]              ;��TCB�л�ȡTSS�����Ե�ַ
         mov dword [es:ebx+28],eax          ;��дTSS��CR3(PDBR)��

         pop es                             ;�ָ������ô˹���ǰ��es�� 
         pop ds                             ;�ָ������ô˹���ǰ��ds��
      
         popad
      
         ret 8                              ;�������ñ�����ǰѹ��Ĳ��� 
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ;��TCB����׷��������ƿ�
                                            ;���룺ECX=TCB���Ի���ַ
         push eax
         push edx
         push ds
         push es

         cli

         mov eax,core_data_seg_sel          ;��DSָ���ں����ݶ� 
         mov ds,eax
         mov eax,mem_0_4_gb_seg_sel         ;��ESָ��0..4GB��
         mov es,eax
         
         mov dword [es: ecx+0x00],0         ;��ǰTCBָ�������㣬��ָʾ������
                                            ;��һ��TCB
                                             
         mov eax,[tcb_chain]                ;TCB��ͷָ��
         or eax,eax                         ;����Ϊ�գ�
         jz .notcb 
         
  .searc:
         mov edx,eax
         mov eax,[es: edx+0x00]
         or eax,eax               
         jnz .searc

         mov [es: edx+0x00],ecx
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain],ecx                ;��Ϊ�ձ�ֱ�����ͷָ��ָ��TCB
         
  .retpc:
         sti

         pop es
         pop ds
         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:
         mov ecx,core_data_seg_sel          ;��DSָ��������ݶ� 
         mov ds,ecx

         mov ecx,mem_0_4_gb_seg_sel         ;��ESָ��4GB���ݶ� 
         mov es,ecx

         ;�����ж���������IDT
         ;ע�⣡�ڴ��ڼ䣬���ÿ����жϣ�Ҳ���õ���put_string���̣�

         ;ǰ20�������Ǵ������쳣ʹ�õ�
         mov eax,general_exception_handler  ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,sys_routine_seg_sel         ;�Ŵ������ڶε�ѡ����
         mov cx,0x8e00                      ;32λ�ж��ţ�0��Ȩ��
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
         xor esi,esi
  .idt0:
         mov [es:ebx+esi*8],eax
         mov [es:ebx+esi*8+4],edx
         inc esi
         cmp esi,19                         ;��װǰ20���쳣�жϴ������
         jle .idt0

         ;����Ϊ������Ӳ��ʹ�õ��ж�����
         mov eax,general_interrupt_handler  ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,sys_routine_seg_sel         ;�Ŵ������ڶε�ѡ����
         mov cx,0x8e00                      ;32λ�ж��ţ�0��Ȩ��
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
  .idt1:
         mov [es:ebx+esi*8],eax
         mov [es:ebx+esi*8+4],edx
         inc esi
         cmp esi,255                        ;��װ��ͨ���жϴ������
         jle .idt1

         ;����ʵʱʱ���жϴ������
         mov eax,rtm_0x70_interrupt_handle  ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,sys_routine_seg_sel         ;�Ŵ������ڶε�ѡ����
         mov cx,0x8e00                      ;32λ�ж��ţ�0��Ȩ��
         call sys_routine_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
         mov [es:ebx+0x70*8],eax
         mov [es:ebx+0x70*8+4],edx

         ;׼�������ж�
         mov word [pidt],256*8-1            ;IDT�Ľ���
         mov dword [pidt+2],idt_linear_address
         lidt [pidt]                        ;�����ж���������Ĵ���IDTR

         ;����8259A�жϿ�����
         mov al,0x11
         out 0x20,al                        ;ICW1�����ش���/������ʽ
         mov al,0x20
         out 0x21,al                        ;ICW2:��ʼ�ж�����
         mov al,0x04
         out 0x21,al                        ;ICW3:��Ƭ������IR2
         mov al,0x01
         out 0x21,al                        ;ICW4:�����߻��壬ȫǶ�ף�����EOI

         mov al,0x11
         out 0xa0,al                        ;ICW1�����ش���/������ʽ
         mov al,0x70
         out 0xa1,al                        ;ICW2:��ʼ�ж�����
         mov al,0x04
         out 0xa1,al                        ;ICW3:��Ƭ������IR2
         mov al,0x01
         out 0xa1,al                        ;ICW4:�����߻��壬ȫǶ�ף�����EOI

         ;���ú�ʱ���ж���ص�Ӳ��
         mov al,0x0b                        ;RTC�Ĵ���B
         or al,0x80                         ;���NMI
         out 0x70,al
         mov al,0x12                        ;���üĴ���B����ֹ�������жϣ����Ÿ�
         out 0x71,al                        ;�½������жϣ�BCD�룬24Сʱ��

         in al,0xa1                         ;��8259��Ƭ��IMR�Ĵ���
         and al,0xfe                        ;���bit 0(��λ����RTC)
         out 0xa1,al                        ;д�ش˼Ĵ���

         mov al,0x0c
         out 0x70,al
         in al,0x71                         ;��RTC�Ĵ���C����λδ�����ж�״̬

         sti                                ;����Ӳ���ж�

         mov ebx,message_0
         call sys_routine_seg_sel:put_string

         ;��ʾ������Ʒ����Ϣ
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

         mov ebx,cpu_brnd0                  ;��ʾ������Ʒ����Ϣ
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brand
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brnd1
         call sys_routine_seg_sel:put_string

         ;׼���򿪷�ҳ����

         ;����ϵͳ�ں˵�ҳĿ¼��PDT
         ;ҳĿ¼������
         mov ecx,1024                       ;1024��Ŀ¼��
         mov ebx,0x00020000                 ;ҳĿ¼�������ַ
         xor esi,esi
  .b1:
         mov dword [es:ebx+esi],0x00000000  ;ҳĿ¼��������
         add esi,4
         loop .b1

         ;��ҳĿ¼�ڴ���ָ��ҳĿ¼�Լ���Ŀ¼��
         mov dword [es:ebx+4092],0x00020003

         ;��ҳĿ¼�ڴ��������Ե�ַ0x00000000��Ӧ��Ŀ¼��
         mov dword [es:ebx+0],0x00021003    ;д��Ŀ¼�ҳ��������ַ�����ԣ�

         ;�����������Ǹ�Ŀ¼�����Ӧ��ҳ����ʼ��ҳ����
         mov ebx,0x00021000                 ;ҳ��������ַ
         xor eax,eax                        ;��ʼҳ�������ַ
         xor esi,esi
  .b2:
         mov edx,eax
         or edx,0x00000003
         mov [es:ebx+esi*4],edx             ;�Ǽ�ҳ�������ַ
         add eax,0x1000                     ;��һ������ҳ�������ַ
         inc esi
         cmp esi,256                        ;���Ͷ�1MB�ڴ��Ӧ��ҳ������Ч��
         jl .b2

  .b3:                                      ;�����ҳ������Ϊ��Ч
         mov dword [es:ebx+esi*4],0x00000000
         inc esi
         cmp esi,1024
         jl .b3

         ;��CR3�Ĵ���ָ��ҳĿ¼������ʽ����ҳ����
         mov eax,0x00020000                 ;PCD=PWT=0
         mov cr3,eax

         cli

         mov eax,cr0
         or eax,0x80000000
         mov cr0,eax                        ;������ҳ����

         ;��ҳĿ¼�ڴ��������Ե�ַ0x80000000��Ӧ��Ŀ¼��
         ;mov ebx,0xfffff000                 ;ҳĿ¼�Լ������Ե�ַ
         ;mov esi,0x80000000                 ;ӳ�����ʼ��ַ
         ;shr esi,22                         ;���Ե�ַ�ĸ�10λ��Ŀ¼����
         ;shl esi,2
         ;mov dword [es:ebx+esi],0x00021003  ;д��Ŀ¼�ҳ��������ַ�����ԣ�
                                            ;Ŀ�굥Ԫ�����Ե�ַΪ0xFFFFF200
         mov dword [es:0xfffff800], 0x00021003

         ;��GDT�еĶ�������ӳ�䵽���Ե�ַ0x80000000
         sgdt [pgdt]

         mov ebx,[pgdt+2]

         or dword [es:ebx+0x10+4],0x80000000;������ģʽ�³�ʼ�����������
         or dword [es:ebx+0x18+4],0x80000000;�����ں˵�ջ��������
         or dword [es:ebx+0x20+4],0x80000000;������ʾ������������
         or dword [es:ebx+0x28+4],0x80000000;���������̶�������
         or dword [es:ebx+0x30+4],0x80000000;�����ں����ݶ�������
         or dword [es:ebx+0x38+4],0x80000000;�����ں˴����������

         add dword [pgdt+2],0x80000000      ;GDTRҲ�õ������Ե�ַ

         lgdt [pgdt]

         ;�޸�IDTR�����ж���������ӳ�䵽���Ե�ַ�߶�
         sidt [pidt]
         add dword [pidt+2],0x80000000      ;IDTRҲ�õ������Ե�ַ
         lidt [pidt]

         jmp core_code_seg_sel:flush        ;ˢ�¶μĴ���CS�����ø߶����Ե�ַ

   flush:
         mov eax,core_stack_seg_sel
         mov ss,eax

         mov eax,core_data_seg_sel
         mov ds,eax

         sti

         mov ebx,message_1
         call sys_routine_seg_sel:put_string


         ;���¿�ʼ��װΪ����ϵͳ����ĵ����š���Ȩ��֮��Ŀ���ת�Ʊ���ʹ����
         mov edi,salt                       ;C-SALT�����ʼλ�� 
         mov ecx,salt_items                 ;C-SALT�����Ŀ���� 
  .g0:
         push ecx   
         mov eax,[edi+256]                  ;����Ŀ��ڵ��32λƫ�Ƶ�ַ 
         mov bx,[edi+260]                   ;����Ŀ��ڵ�Ķ�ѡ���� 
         mov cx,1_11_0_1100_000_00000B      ;��Ȩ��3�ĵ�����(3���ϵ���Ȩ����
                                            ;�������)��0������(��Ϊ�üĴ���
                                            ;���ݲ�������û����ջ) 
         call sys_routine_seg_sel:make_gate_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+260],cx                   ;�����ص���������ѡ���ӻ���
         add edi,salt_item_len              ;ָ����һ��C-SALT��Ŀ 
         pop ecx
         loop .g0

         ;���Ž��в��� 
         mov ebx,message_2
         call far [salt_1+256]              ;ͨ������ʾ��Ϣ(ƫ������������) 

         ;��ʼ������ȷ���ں�����
         mov ecx,core_lin_tcb_addr          ;�����߶�֮����ں�����TCB���Ե�ַ
         mov word [es:ecx+0x04],0xffff      ;�����״̬Ϊ��æ��
         mov dword [es:ecx+0x46],core_lin_alloc_at
                                            ;�Ǽ��ں��п����ڷ������ʼ���Ե�ַ
         call append_to_tcb_link            ;���ں������TCB��ӵ�TCB����

         mov esi,ecx

         ;Ϊ�ں������TSS�����ڴ�ռ䡣����TSS���봴�����ں˿ռ�
         mov ecx,104                        ;Ϊ�������TSS�����ڴ�
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x14],ecx              ;���ں�TCB�б���TSS����ַ

         ;�ڳ����������TSS�����ñ�Ҫ����Ŀ
         mov word [es:ecx+0],0              ;������=0
         mov eax,cr3
         mov dword [es:ecx+28],eax          ;�Ǽ�CR3(PDBR)
         mov word [es:ecx+96],0             ;û��LDT������������û��LDT������
         mov word [es:ecx+100],0            ;T=0
         mov word [es:ecx+102],103          ;û��I/Oλͼ��0��Ȩ����ʵ�ϲ���Ҫ��
                                            ;����Ҫ0��1��2��Ȩ����ջ��0�ؼ���
                                            ;�������Ȩ��ת�ƿ��ơ�
         
         ;����TSS������������װ��GDT�� 
         mov eax,ecx                        ;TSS����ʼ���Ե�ַ
         mov ebx,103                        ;�γ��ȣ����ޣ�
         mov ecx,0x00008900                 ;TSS����������Ȩ��0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov word [es:esi+0x18],cx          ;�Ǽ�TSSѡ���ӵ�TCB

         ;����Ĵ���TR�е�������������ڵı�־��������Ҳ�����˵�ǰ������˭��
         ;�����ָ��Ϊ��ǰ����ִ�е�0��Ȩ�����񡰳������������������TSS����
         ltr cx

         ;���ڿ���Ϊ�������������������ִ����
         mov ebx,core_msg1
         call sys_routine_seg_sel:put_string

         ;���¿�ʼ�����û�����
         mov ecx,0x4a
         call sys_routine_seg_sel:allocate_memory
         mov word [es:ecx+0x04],0           ;����״̬������
         mov dword [es:ecx+0x46],0          ;�����ڿ����ڷ���ĳ�ʼ���Ե�ַ

         push dword 50                      ;�û�����λ���߼�50����
         push ecx                           ;ѹ��������ƿ���ʼ���Ե�ַ 
         call load_relocate_program
         call append_to_tcb_link            ;����TCB��ӵ�TCB����

         ;���Դ���������������磺
         mov ecx,0x4a
         call sys_routine_seg_sel:allocate_memory
         mov word [es:ecx+0x04],0           ;����״̬������
         mov dword [es:ecx+0x46],0          ;�����ڿ����ڷ���ĳ�ʼ���Ե�ַ

         push dword 100                     ;�û�����λ���߼�100����
         push ecx                           ;ѹ��������ƿ���ʼ���Ե�ַ

         call load_relocate_program
         call append_to_tcb_link            ;����TCB��ӵ�TCB����

  .do_switch:
         mov ebx,core_msg2
         call sys_routine_seg_sel:put_string

         ;�����Ѿ���ֹ�����񣬲���������ռ�õ���Դ
         call sys_routine_seg_sel:do_task_clean

         hlt

         jmp .do_switch

core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
