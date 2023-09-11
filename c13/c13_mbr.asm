         ;�����嵥13-1
         ;�ļ�����c13_mbr.asm
         ;�ļ�˵����Ӳ����������������
         ;�������ڣ�2020-6-22 18:00���޸���2022-02-17 22��12

         ;���ó�ʼ��ջ�κ�ջָ��
         mov ax,cs
         mov ss,ax
         mov sp,0x7c00

         ;����GDT���ڵ��߼��ε�ַ
         mov ax,[cs:gdt_base+0x7c00]        ;��16λ 
         mov dx,[cs:gdt_base+0x7c00+0x02]   ;��16λ 
         mov bx,16        
         div bx            
         mov ds,ax                          ;��DSָ��ö��Խ��в���
         mov bx,dx                          ;������ʼƫ�Ƶ�ַ 
      
         ;����0#�����������ǿ������������Ǵ�������Ҫ��
         mov dword [bx+0x00],0x00
         mov dword [bx+0x04],0x00  

         ;����#1������������ģʽ�µ����ݶ����������ı�ģʽ�µ���ʾ��������
         mov dword [bx+0x08],0x8000ffff
         mov dword [bx+0x0c],0x0040920b

         ;����#2������������ģʽ�µĴ����������
         mov dword [bx+0x10],0x7c0001ff
         mov dword [bx+0x14],0x00409800

         ;��ʼ����������Ĵ���GDTR
         mov word [cs: gdt_size+0x7c00],23  ;��������Ľ��ޣ����ֽ�����һ��
                                             
         lgdt [cs: gdt_size+0x7c00]
      
         in al,0x92                         ;����оƬ�ڵĶ˿� 
         or al,0000_0010B
         out 0x92,al                        ;��A20

         cli                                ;����ģʽ���жϻ�����δ������Ӧ 
                                            ;��ֹ�ж� 
         mov eax,cr0
         or eax,1
         mov cr0,eax                        ;����PEλ

         ;���½��뱣��ģʽ... ...
         jmp 0000000000010_0_00B:flush      ;jmp 0x0010:flush

         bits 32

flush:
         mov cx,0000000000001_0_00B         ;�������ݶ�ѡ����(0x08)
         mov ds,cx

         ;��������Ļ����ʾ"Protect mode OK."
         mov byte [0x00],'P'
         mov byte [0x02],'r'
         mov byte [0x04],'o'
         mov byte [0x06],'t'
         mov byte [0x08],'e'
         mov byte [0x0a],'c'
         mov byte [0x0c],'t'
         mov byte [0x0e],' '
         mov byte [0x10],'m'
         mov byte [0x12],'o'
         mov byte [0x14],'d'
         mov byte [0x16],'e'
         mov byte [0x18],' '
         mov byte [0x1a],'O'
         mov byte [0x1c],'K'
         mov byte [0x1e],'.'

         hlt                                ;�Ѿ���ֹ�жϣ������ᱻ���� 

;-------------------------------------------------------------------------------
     
         gdt_size         dw 0
         gdt_base         dd 0x00007e00     ;GDT�������ַ 
                             
         times 510-($-$$) db 0
                          db 0x55,0xaa
