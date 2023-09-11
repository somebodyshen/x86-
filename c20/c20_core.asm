         ;�����嵥20-2���ں˳���
         ;�ļ�����c20_core.asm
         ;���ң�2022-6-7 22:30

         ;���³������岿�֡��ں˵Ĵ󲿷����ݶ�Ӧ���̶� 
         flat_core_code_seg_sel  equ  0x0008      ;ƽ̹ģ���µ��ںˣ�0��Ȩ����4GB�����ѡ����
         flat_core_data_seg_sel  equ  0x0010      ;ƽ̹ģ���µ��ںˣ�0��Ȩ����4GB���ݶ�ѡ����
         flat_user_code_seg_sel  equ  0x001b      ;ƽ̹ģ���µ��û���3��Ȩ����4GB�����ѡ����
         flat_user_data_seg_sel  equ  0x0023      ;ƽ̹ģ���µ��û���3��Ȩ����4GB���ݶ�ѡ����

         idt_linear_address    equ  0x8001f000   ;�ж�������������Ե�ַ
         core_lin_alloc_at     equ  0x80100000   ;�ں��п����ڷ������ʼ���Ե�ַ
         core_lin_tcb_addr     equ  0x8001f800   ;�ں�����TCB�ĸ߶����Ե�ַ

;-------------------------------------------------------------------------------
         ;������ϵͳ���ĵ�ͷ�������ڼ��غ��ĳ��� 
SECTION header vstart=0x80040000

         core_length      dd core_end       ;���ĳ����ܳ���#00

         core_entry       dd start          ;���Ĵ������ڵ�#04

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vfollows=header          ;ϵͳ�������̴����
;-------------------------------------------------------------------------------
         ;�ַ�����ʾ���̣�������ƽ̹�ڴ�ģ�ͣ�
put_string:                                 ;��ʾ0��ֹ���ַ������ƶ����
                                            ;���룺EBX=�ַ��������Ե�ַ

         push ebx
         push ecx

         pushfd
         cli                                ;Ӳ�������ڼ䣬���ж�

  .getc:
         mov cl,[ebx]
         or cl,cl                           ;��⴮������־��0��
         jz .exit                           ;��ʾ��ϣ�����
         call put_char
         inc ebx
         jmp .getc

  .exit:

         popfd                              ;Ӳ��������ϣ��ָ�ԭ���ж�״̬

         pop ecx
         pop ebx

         ret

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
         and ebx,0x0000ffff                 ;׼��ʹ��32λѰַ��ʽ�����Դ�

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
         shl bx,1
         mov [0x800b8000+ebx],cl

         ;���½����λ���ƽ�һ���ַ�
         shr bx,1
         inc bx

  .roll_screen:
         cmp bx,2000                        ;��곬����Ļ������
         jl .set_cursor

         push bx                            ;Ϊ���޸�ԭ�������߼����⣬����

         cld
         mov esi,0x800b80a0                 ;С�ģ�32λģʽ��movsb/w/d
         mov edi,0x800b8000                 ;ʹ�õ���esi/edi/ecx
         mov ecx,1920
         rep movsd
         mov bx,3840                        ;�����Ļ���һ��
         mov ecx,80                         ;32λ����Ӧ��ʹ��ECX
  .cls:
         mov word[0x800b8000+ebx],0x0720
         add bx,2
         loop .cls

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
read_hard_disk_0:                           ;��Ӳ�̶�ȡһ���߼�������ƽ̹ģ�ͣ�
                                            ;EAX=�߼�������
                                            ;EBX=Ŀ�껺�������Ե�ַ
                                            ;���أ�EBX=EBX+512
         cli

         push eax
         push ecx
         push edx

         push eax

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

         pop edx
         pop ecx
         pop eax

         sti

         ret

;-------------------------------------------------------------------------------
;������Գ����Ǽ���һ�γɹ������ҵ��Էǳ����ѡ�������̿����ṩ����
put_hex_dword:                              ;�ڵ�ǰ��괦��ʮ��������ʽ��ʾ
                                            ;һ��˫�ֲ��ƽ����
                                            ;���룺EDX=Ҫת������ʾ������
                                            ;�������
         pushad

         mov ebx,bin_hex                    ;ָ����ĵ�ַ�ռ��ڵ�ת����
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
set_up_gdt_descriptor:                      ;��GDT�ڰ�װһ���µ�������
                                            ;���룺EDX:EAX=������
                                            ;�����CX=��������ѡ����
         push eax
         push ebx
         push edx

         sgdt [pgdt]                        ;ȡ��GDTR�Ľ��޺����Ե�ַ

         movzx ebx,word [pgdt]              ;GDT����
         inc bx                             ;GDT���ֽ�����Ҳ����һ��������ƫ��
         add ebx,[pgdt+2]                   ;��һ�������������Ե�ַ

         mov [ebx],eax
         mov [ebx+4],edx

         add word [pgdt],8                  ;����һ���������Ĵ�С

         lgdt [pgdt]                        ;��GDT�ĸ�����Ч

         mov ax,[pgdt]                      ;�õ�GDT����ֵ
         xor dx,dx
         mov bx,8
         div bx                             ;����8��ȥ������
         mov cx,ax
         shl cx,3                           ;���������Ƶ���ȷλ��

         pop edx
         pop ebx
         pop eax

         ret
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

         ret

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

         ret

;-------------------------------------------------------------------------------
allocate_a_4k_page:                         ;����һ��4KB��ҳ
                                            ;���룺��
                                            ;�����EAX=ҳ�������ַ
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
         hlt                                ;û�п��Է����ҳ��ͣ��

  .b2:
         shl eax,12                         ;����4096��0x1000��

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
         mov dword [eax],0x00000000
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
         pop esi
         pop ecx
         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
create_copy_cur_pdir:                       ;������ҳĿ¼�������Ƶ�ǰҳĿ¼����
                                            ;���룺��
                                            ;�����EAX=��ҳĿ¼�������ַ
         push esi
         push edi
         push ebx
         push ecx

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

         ret

;-------------------------------------------------------------------------------
task_alloc_memory:                          ;��ָ������������ڴ�ռ��з����ڴ�
                                            ;���룺EBX=������ƿ�TCB�����Ե�ַ
                                            ;      ECX=ϣ��������ֽ���
                                            ;�����ECX=�ѷ������ʼ���Ե�ַ
         push eax

         push ebx                           ;to A

         ;��ñ����ڴ�������ʼ���Ե�ַ
         mov ebx,[ebx+0x06]                 ;��ñ��η������ʼ���Ե�ַ
         mov eax,ebx
         add ecx,ebx                        ;���η��䣬���һ���ֽ�֮������Ե�ַ

         push ecx                           ;To B

         ;Ϊ������ڴ����ҳ
         and ebx,0xfffff000
         and ecx,0xfffff000
  .next:
         call alloc_inst_a_page
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

         mov [ebx+0x06],ecx                 ;���´η�����õ����Ե�ַ�ش浽TCB��
         mov ecx,eax

         pop eax

         ret

;-------------------------------------------------------------------------------
allocate_memory:                            ;�ڵ�ǰ����ĵ�ַ�ռ��з����ڴ�
                                            ;���룺ECX=ϣ��������ֽ���
                                            ;�����ECX=�������ڴ����ʼ���Ե�ַ
         push eax
         push ebx

         ;�õ�TCB�����׽ڵ�����Ե�ַ
         mov eax,[tcb_chain]                ;EAX=�׽ڵ�����Ե�ַ

         ;����״̬Ϊæ����ǰ���񣩵Ľڵ�
  .s0:
         cmp word [eax+0x04],0xffff
         jz .s1                             ;�ҵ�æ�Ľڵ㣬EAX=�ڵ�����Ե�ַ
         mov eax,[eax]
         jmp .s0

         ;��ʼ�����ڴ�
  .s1:
         mov ebx,eax
         call task_alloc_memory

         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
resume_task_execute:                        ;�ָ�ָ�������ִ��
                                            ;���룺EDI=�������TCB�����Ե�ַ
         mov eax, [edi + 10]
         mov [tss + 4], eax                 ;���������RSP0����TSS��RSP0��

         mov eax, [edi + 22]
         mov cr3, eax                       ;�ָ��������CR3

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

         test word [edi + 32], 3           ;SS.RPL=3��
         jnz .to_r3                        ;�ǵġ�ת.to_r3
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

         not word [edi + 0x04]             ;������״̬�Ľڵ��Ϊæ״̬�Ľڵ�
         mov edi, [edi + 62]

         iretd

;-------------------------------------------------------------------------------
initiate_task_switch:                       ;�������������л�
                                            ;���룺��
                                            ;������ޡ�
         push eax
         push ebx
         push esi
         push edi

         mov eax, [tcb_chain]
         cmp eax, 0                         ;����Ϊ�գ�
         jz .return

         ;����״̬Ϊæ����ǰ���񣩵Ľڵ�
  .b0:
         cmp word [eax + 4], 0xffff
         cmove esi, eax                     ;���ҵ�æ�Ľڵ㣬ESI=�ڵ�����Ե�ַ
         jz .b1
         mov eax, [eax]
         jmp .b0                            ;ѭ��������ֹ�������ں������Ǵ��ڵĲ���Ϊæ

         ;�ӵ�ǰ�ڵ����������������Ľڵ�
  .b1:
         mov ebx, [eax]
         or ebx, ebx                        ;��������β����
         jz .b2                             ;������β��Ҳδ���־����ڵ㣬��ͷ��
         cmp word [ebx + 4], 0              ;�Ǿ���״̬��
         cmove edi, ebx                     ;���ҵ������ڵ㣬EDI=�ڵ�����Ե�ַ
         jz .b3
         mov eax, ebx
         jmp .b1

  .b2:   ;������ͷѰ�Ҿ�������Ľڵ�
         mov ebx, [tcb_chain]               ;EBX=�����׽ڵ����Ե�ַ
  .b20:
         cmp word [ebx + 4], 0
         cmove edi, ebx                     ;���ҵ������ڵ㣬EDI=�ڵ�����Ե�ַ
         jz .b3
         mov ebx, [ebx]
         or ebx, ebx
         jz .return                         ;�������Ѿ������ھ������񣬷���
         jmp .b20

  .b3:
         ;����������״̬
         mov eax, cr3
         mov [esi + 22], eax                ;����CR3
         ;EAX/EBX/ESI/EDI���ñ��棬������ָ�ִ��ʱ���Զ���ջ�е������ָ�
         mov [esi + 50], ecx
         mov [esi + 54], edx
         mov [esi + 66], ebp
         mov [esi + 70], esp
         mov dword [esi + 26], .return      ;�ָ�ִ��ʱ��EIP
         mov [esi + 30], cs
         mov [esi + 32], ss
         mov [esi + 34], ds
         mov [esi + 36], es
         mov [esi + 38], fs
         mov [esi + 40], gs
         pushfd
         pop dword [esi + 74]
         not word [esi + 4]                 ;��æ״̬�Ľڵ��Ϊ����״̬�Ľڵ�

         jmp resume_task_execute            ;תȥ�ָ���ִ��������

  .return:
         pop edi
         pop esi
         pop ebx
         pop eax

         ret

;-------------------------------------------------------------------------------
terminate_current_task:                     ;��ֹ��ǰ����
                                            ;ע�⣬ִ�д�����ʱ����ǰ��������
                                            ;�����С���������ʵҲ�ǵ�ǰ�����
                                            ;һ���� 
         mov edi, [tcb_chain]
                                            ;EAX=�׽ڵ�����Ե�ַ
         ;����״̬Ϊæ����ǰ���񣩵Ľڵ�
  .s0:
         cmp word [edi + 4], 0xffff
         jz .s1                             ;�ҵ�æ�Ľڵ㣬EAX=�ڵ�����Ե�ַ
         mov edi, [edi]
         jmp .s0

         ;��״̬Ϊæ�Ľڵ�ĳ���ֹ״̬
  .s1:
         mov word [edi + 4], 0x3333

         ;��������״̬������
         mov edi, [tcb_chain]               ;EBX=�����׽ڵ����Ե�ַ
  .s2:
         cmp word [edi + 4], 0x0000
         jz .s3                             ;���ҵ������ڵ㣬EBX=�ڵ�����Ե�ַ
         mov edi, [edi]
         jmp .s2

         ;��������Ľڵ��Ѿ��ҵ���׼���л���������
  .s3:
         jmp resume_task_execute            ;תȥ�ָ���ִ��������

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
         call put_string

         cli

         hlt

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:                  ;ʵʱʱ���жϴ������
         push eax

         mov al, 0x20                       ;�жϽ�������EOI
         out 0xa0, al                       ;��8259A��Ƭ����
         out 0x20, al                       ;��8259A��Ƭ����

         mov al, 0x0c                       ;�Ĵ���C���������ҿ���NMI
         out 0x70, al
         in al, 0x71                        ;��һ��RTC�ļĴ���C������ֻ����һ���ж�
                                            ;�˴����������Ӻ��������жϵ����
         call initiate_task_switch          ;���������л�

         pop eax

         iretd

;-------------------------------------------------------------------------------
do_task_clean:                             ;�����Ѿ���ֹ�����񲢻�����Դ

         ;����TCB�����ҵ�״̬Ϊ��ֹ�Ľڵ�
         ;���ڵ�������в��
         ;��������ռ�õĸ�����Դ�����Դ�����TCB���ҵ���

         ret

;-------------------------------------------------------------------------------
int_0x88_handler:                          ;ϵͳ���ô������

         call [eax * 4 + sys_call]
         iretd

;===============================================================================
SECTION core_data vfollows=sys_routine       ;ϵͳ���ĵ����ݶ�
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

         ;ϵͳ���ù������
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
                                            ;put_hex_dword�ӹ����õĲ��ұ� 

         core_buf   times 2048 db 0         ;�ں��õĻ�����

         tcb_chain        dd  0             ;������ƿ���

         tss              times 128 db  0   ;����״̬��

         core_msg1        db  'Core task created.',0x0d,0x0a,0
                 
         core_msg2        db  '[CORE TASK]: I am working!',0x0d,0x0a,0

;===============================================================================
SECTION core_code vfollows=core_data
;-------------------------------------------------------------------------------
load_relocate_program:                      ;���ز��ض�λ�û�����
                                            ;����: PUSH �߼�������
                                            ;      PUSH ������ƿ����ַ
                                            ;������� 
         pushad
      
         mov ebp,esp                        ;Ϊ����ͨ����ջ���ݵĲ�����׼��
      
         ;��յ�ǰҳĿ¼��ǰ�벿�֣���Ӧ��2GB�ľֲ���ַ�ռ䣩
         mov ebx,0xfffff000
         xor esi,esi
  .clsp:
         mov dword [ebx+esi*4],0x00000000
         inc esi
         cmp esi,512
         jl .clsp

         mov ebx,cr3                        ;ˢ��TLB
         mov cr3,ebx

         ;���¿�ʼ�����û�����
         mov eax,[ebp+10*4]                 ;�Ӷ�ջ��ȡ���û�������ʼ������
         mov ebx,core_buf                   ;��ȡ����ͷ������
         call read_hard_disk_0

         ;�����ж����������ж��
         mov eax,[core_buf]                 ;����ߴ�
         mov ebx,eax
         and ebx,0xfffffe00                 ;ʹ֮512�ֽڶ��루�ܱ�512����������
         add ebx,512                        ;9λ��Ϊ0
         test eax,0x000001ff                ;����Ĵ�С������512�ı�����?
         cmovnz eax,ebx                     ;���ǡ�ʹ�ô����Ľ��

         mov esi,[ebp+9*4]                  ;�Ӷ�ջ��ȡ��TCB�Ļ���ַ

         mov ecx,eax                        ;ʵ����Ҫ������ڴ�����
         mov ebx,esi
         call task_alloc_memory

         mov ebx,ecx                        ;ebx -> ���뵽���ڴ��׵�ַ == 0
         xor edx,edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;��������

         mov eax,[ebp+10*4]                 ;��ʼ������
  .b1:
         call read_hard_disk_0
         inc eax
         loop .b1                           ;ѭ������ֱ�����������û�����

         ;Ϊ�û��������ջ�ռ�
         mov ebx,esi                        ;TCB�����Ե�ַ
         mov ecx,4096                       ;4KB�Ŀռ�
         call task_alloc_memory
         mov ecx, [esi + 6]                 ;��һ�η������ʼ���Ե�ַ����ջ��ָ��
         mov dword [esi + 70], ecx

         ;���������жϺ͵����ŵ�0��Ȩ��ջ�ռ�
         mov ebx,esi
         mov ecx,4096                       ;4KB�Ŀռ�
         call task_alloc_memory
         mov ecx, [esi + 6]                 ;��һ�η������ʼ���Ե�ַ����ջ��ָ��
         mov dword [esi + 10], ecx          ;TCB��ESP0��

         ;�����û������ҳĿ¼
         ;ע�⣡ҳ�ķ����ʹ������ҳλͼ�����ģ����Բ�ռ�����Ե�ַ�ռ�
         call create_copy_cur_pdir
         mov [esi + 22], eax                ;��дTCB��CR3(PDBR)��

         mov word [esi + 30], flat_user_code_seg_sel    ;TCB��CS��
         mov word [esi + 32], flat_user_data_seg_sel    ;TCB��SS��
         mov word [esi + 34], flat_user_data_seg_sel    ;TCB��DS��
         mov word [esi + 36], flat_user_data_seg_sel    ;TCB��ES��
         mov word [esi + 38], flat_user_data_seg_sel    ;TCB��FS��
         mov word [esi + 40], flat_user_data_seg_sel    ;TCB��GS��
         mov eax, [0x04]                    ;�������4GB��ַ�ռ��ȡ��ڵ�
         mov [esi + 26], eax                ;��дTCB��EIP��
         pushfd
         pop dword [esi + 74]               ;��дTCB��EFLAGS��
         mov word [esi + 4], 0              ;����״̬������

         popad
      
         ret 8                              ;�������ñ�����ǰѹ��Ĳ��� 
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ;��TCB����׷��������ƿ�
                                            ;���룺ECX=TCB���Ի���ַ
         push eax
         push edx

         pushfd
         cli

         mov dword [ecx+0x00],0             ;��ǰTCBָ�������㣬��ָʾ������
                                            ;��һ��TCB
                                             
         mov eax,[tcb_chain]                ;TCB��ͷָ��
         or eax,eax                         ;����Ϊ�գ�
         jz .notcb 
         
  .searc:
         mov edx,eax
         mov eax,[edx+0x00]
         or eax,eax               
         jnz .searc

         mov [es: edx+0x00],ecx
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain],ecx                ;��Ϊ�ձ�ֱ�����ͷָ��ָ��TCB
         
  .retpc:
         popfd

         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:
         ;�����ж���������IDT
         mov ebx, message_0
         call put_string

         ;ǰ20�������Ǵ������쳣ʹ�õ�
         mov eax,general_exception_handler  ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,flat_core_code_seg_sel      ;�Ŵ������ڶε�ѡ����
         mov cx,0x8e00                      ;32λ�ж��ţ�0��Ȩ��
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
         xor esi,esi
  .idt0:
         mov [ebx+esi*8],eax
         mov [ebx+esi*8+4],edx
         inc esi
         cmp esi,19                         ;��װǰ20���쳣�жϴ������
         jle .idt0

         ;����Ϊ������Ӳ��ʹ�õ��ж�����
         mov eax,general_interrupt_handler  ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,flat_core_code_seg_sel      ;�Ŵ������ڶε�ѡ����
         mov cx,0x8e00                      ;32λ�ж��ţ�0��Ȩ��
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
  .idt1:
         mov [ebx+esi*8],eax
         mov [ebx+esi*8+4],edx
         inc esi
         cmp esi,255                        ;��װ��ͨ���жϴ������
         jle .idt1

         ;����ʵʱʱ���жϴ������
         mov eax,rtm_0x70_interrupt_handle  ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,flat_core_code_seg_sel      ;�Ŵ������ڶε�ѡ����
         mov cx,0x8e00                      ;32λ�ж��ţ�0��Ȩ��
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
         mov [ebx+0x70*8],eax               ;�ж�������0x70
         mov [ebx+0x70*8+4],edx

         ;����ϵͳ�����жϵĴ������
         mov eax,int_0x88_handler           ;�Ŵ����ڶ���ƫ�Ƶ�ַ
         mov bx,flat_core_code_seg_sel      ;�Ŵ������ڶε�ѡ����
         mov cx,0xee00                      ;32λ�ж��ţ�3��Ȩ��!!!!!!
         call make_gate_descriptor

         mov ebx,idt_linear_address         ;�ж�������������Ե�ַ
         mov [ebx+0x88*8],eax               ;�ж�������0x88
         mov [ebx+0x88*8+4],edx

         ;׼�������ж�
         mov word [pidt],256*8-1            ;IDT�Ľ���
         mov dword [pidt+2],idt_linear_address
         lidt [pidt]                        ;�����ж���������Ĵ���IDTR

         ;����ϵͳ����
         mov ebx,message_1
         mov eax, 0                         ;ͨ��ϵͳ���õ�0�Ź�����ʾ��Ϣ
         int 0x88                           ;����TSS��δ׼���ã��������л�ջ

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
         call put_string
         mov ebx,cpu_brand
         call put_string
         mov ebx,cpu_brnd1
         call put_string

         ;��������״̬��TSS��������������ϵͳʵ����ֻ��Ҫһ��TSS���ɡ�
         mov ecx, 32
         xor ebx, ebx
  .clear:
         mov dword [tss + ebx], 0          ;TSS�Ķ����ֶ��Ѿ����ã�ȫ����ա�
         add ebx, 4
         loop .clear

         ;����Ȩ��֮���ת�ƶ�����ջ�л�ʱ����ϵͳֻ�ᷢ��3��0���л�����ˣ�
         ;ֻ��ҪTSS������SS0���ұ�����0��Ȩ����ջ��ѡ���ӡ�
         mov word [tss + 8], flat_core_data_seg_sel
         mov word [tss + 102], 103          ;û��I/O���λͼ����

         ;����TSS������������װ��GDT��
         mov eax,tss                        ;TSS����ʼ���Ե�ַ
         mov ebx,103                        ;�γ��ȣ����ޣ�
         mov ecx,0x00008900                 ;TSS����������Ȩ��0
         call make_seg_descriptor
         call set_up_gdt_descriptor

         ;������Ĵ���TRָ��Ψһ��TSS�����ٸı䡣
         ltr cx

         mov ebx,message_2
         call put_string

         ;��ʼ������ȷ���ں�����
         mov ecx, core_lin_tcb_addr         ;�����߶�֮����ں�����TCB���Ե�ַ
         mov word [ecx + 4], 0xffff         ;�����״̬Ϊ��æ��
         mov dword [ecx + 6], core_lin_alloc_at ;�Ǽ��ں��п����ڷ������ʼ���Ե�ַ
         call append_to_tcb_link            ;���ں������TCB��ӵ�TCB����

         ;���ڿ���Ϊ�������������������ִ����
         mov ebx,core_msg1
         call put_string

         ;���¿�ʼ�����û�����
         mov ecx, 128                       ;ΪTCB�����ڴ�
         call allocate_memory
         mov word [ecx+0x04],0              ;����״̬������
         mov dword [ecx+0x06],0             ;�����ڿ����ڷ���ĳ�ʼ���Ե�ַ

         push dword 50                      ;�û�����λ���߼�50����
         push ecx                           ;ѹ��������ƿ���ʼ���Ե�ַ 
         call load_relocate_program
         call append_to_tcb_link            ;����TCB��ӵ�TCB����

         ;���Դ���������������磺
         mov ecx,128                        ;ΪTCB�����ڴ�
         call allocate_memory
         mov word [ecx+0x04],0              ;����״̬������
         mov dword [ecx+0x06],0             ;�����ڿ����ڷ���ĳ�ʼ���Ե�ַ

         push dword 100                     ;�û�����λ���߼�100����
         push ecx                           ;ѹ��������ƿ���ʼ���Ե�ַ

         call load_relocate_program
         call append_to_tcb_link            ;����TCB��ӵ�TCB����

  .do_switch:
         mov ebx,core_msg2
         call put_string

         ;�����Ѿ���ֹ�����񣬲���������ռ�õ���Դ
         call do_task_clean

         hlt

         jmp .do_switch

;-------------------------------------------------------------------------------
SECTION core_tail
;-------------------------------------------------------------------------------
core_end:
