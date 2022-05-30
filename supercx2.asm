COMMENT $

€€                 S u p e r   C O M - e X t r a c t o r
€€                  C O M   F I L E   E X T R A C T O R
€€
€€               Written  by: Lost Soul [UCF]   13/11/1994
€€               Freeware for beginners to learn and know.
€€
€€
€€      - This program is based on the idea that every com file,
€€        after self decrypting itself, will return to execute
€€        itself as if it wasn't encrypted at all (IP = 100h)
€€
€€      - this program can't extract com files with anti-debugging
€€        instructions.
€€
€€      - When all the registers equal zero, there is high
€€        possibility that the program is decrypted (the 'Clean' option)
€€
€€      - This program isn't optimized at all, you can surely
€€        reduce it's size by 10-100 bytes.
€€
€€
€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
$
.8086                              ;XT - 8086/88 INST. set
WARN PRO

PSP_cmd_size    EQU     80h
CR              EQU     0Dh
LF              EQU     0Ah
CSEG            segment byte public use16
                assume  cs:CSEG , ds:CSEG, ss:CSEG


PROGSTART EQU   $
NEWStack = PROGEND - PROGSTART + 1000h
                org     100h
supercx         proc    near

start:
                JMP     REAL_START
logo_           DB      'Super COM-eXtractor version 2.00, Written by Lost Soul [UCF]   13 Nov 94', 0Dh, 0Ah
                DB      'Evaluation Version. without Fight Anti-Debugging',  CR, LF,LF, '$'
usage_          DB      '    Usage: SuperCX <file>.COM',CR,LF, '$'
error_          DB      'Dos error occured '        ,CR,LF, '$'
memory_         DB      'Not enough memory'         ,CR,LF, '$'
scanning_       DB      'Analyzing Com File...$'
found1_         db      CR,LF,'100% Clean COM FILE,Save Recommended!$'
found2_         DB      CR,LF,'COM-File might be found,Save Now [Yes/No/Clean] ?$'
comsaved_       DB      CR,LF,'COM-File Saved' ,CR,LF, '$'
cant_save_      DB      CR,LF,'Error, Program terminated.$'
terminated_flag db      0
Filename        DW      ?
Handle          DW      ?
SSPTR           DW      ?
label           ComAddress      dword            ; set up data in
COMFileofs      dw              100h             ; code segment
COMFileseg      dw              0
supercx            endp

;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
INT21h          proc    near
                CLC
                int     21h

                jnc     short RETURN           ;if no error then return


                mov     ah,9
                lea     dx,error_
                int     21h                    ; print 'Dos error' message

                jmp     EXIT                   ; Terminate  Process

RETURN:
                retn
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
wrtlogo:
                lea     dx,logo_               ; Author's LOGO
                mov     ah,9
                call    INT21h
                retn

;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
check4com:
                mov     si,PSP_cmd_size         ;
                lodsb
                test    al,al                   ;Check For Arguments
                je      short usage             ;if none exist...
                xor     ah,ah                   ; AH = 0
                add     si,ax
                mov     byte ptr [si],0
                std
                dec     si
                lodsb
                and     al,5Fh
                cmp     al,'M'
                jne     short usage             ; Check For .COM  Filename
                lodsb
                and     al,5Fh
                cmp     al,'O'
                jne     short usage             ; Check For .COM  Filename
                lodsb
                and     al,5Fh
                cmp     al,'C'
                jne     short usage             ; Check For .COM  Filename
                cld
                mov  si,81h
spaces_loop:    lodsb
                cmp al,20h
                jz spaces_loop
                dec si
                mov Filename,si
                retn                            ;Return to main prog.
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
usage:

                mov     ah,9
                lea     dx,usage_               ; Display Usage inst.
                call    INT21h
                jmp     EXIT                    ; Terminate Processe


;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
Allocmem:
                mov     ah,4Ah                  ; Modify Allocated Memory
                                                ; Block
                push    cs
                pop     es                      ;Requseted Block Seg = CS
                mov     bx,NEWStack             ; Set requested block size
                shr     bx,04                   ; '   '         '     '
                inc     bx                      ; '   '         '     '
                call    INT21h
                mov     ah,48h                  ; check largesr block of
                mov     bx,0FFFFh               ; memory (trying to allocate)
                                                ; 1024kb won't work, dos will
                                                ; return max mem avail. in BX
                int     21h
                cmp     bx,1000h                ; FREE MEMORY >= 64k ?
                jae     short enough_mem        ;


                lea     dx,memory_              ; Memory Error Message
                mov     ah,9
                call    INT21h
                jmp     EXIT                    ; Terminate Processe
enough_mem:
                dec     bx
                mov     ah,48h                  ; Allocate Memory Block
                call    INT21h
                mov     word ptr cs:COMFileseg,ax   ; Store The Allocated Segment
                retn


;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
Loadfile:
                mov     ah,26h                  ; Create New PSP
                mov     dx,cs:COMFileseg        ; Seg. of Allocated Mem
                call    INT21h
                mov     ax,3D00h                ; Open File For Read-Only
                mov     dx,Filename
                call    INT21h
                mov     cs:Handle,ax            ; Store Handle
                push    ds                      ; Save data seg.
                mov     ds,word ptr cs:COMfileseg; DS=Comfile Segment
                mov     ah,3Fh                  ; Read from file
                mov     bx,cs:Handle            ;
                mov     cx,0FEFFh               ; Can't be more than that...
                mov     dx,100h                 ; (it's a COM file...)
                int     21h                     ;


                mov     di,ax
                add     di,100h

;    FILL MEMORY WITH NOPs to detect original com file size
fillmem_loop:
                cmp     di,0FFFFh
                jz      short top_of_mem             ; Jump if equal


                mov     word ptr [di],90h     ; Fill EOF upto 0FFFFh
                inc     di
                jmp     short fillmem_loop
top_of_mem:
                mov     bx,0FFEEh
                mov     word ptr [bx],0h
                pop     ds                      ; Restore data seg.
                mov     ah,3Eh                  ; Close file
                mov     bx,cs:Handle
                call    INT21h
                retn


;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
RunCOMFile:
                push    cs
                pop     ds
                lea     dx,scanning_
                mov     ah,9
                call    INT21H
                mov     bx,word ptr cs:COMFILESEG
                mov     ah,50h                  ; Set current Proccess ID
                int     21h                     ; to COMFILE's PSP
                mov     cs:SSPTR,sp             ; Save Stack Pointer
                mov     ax,word ptr cs:COMFILESEG
                mov     es,ax
                lea     ax,CONTINUE
                mov     es:0Ah,ax               ; COMFile-1:0001h = COMFile Seg.
                mov     es:0Ch,cs               ; On CONTINUE:
                mov     word ptr es:PSP_cmd_size,0D00h
                xor     bx,bx                   ; BX = 0
                mov     cx,7302h

                mov     ax,cs:COMFileSeg
                mov     ds,ax                   ; DS = COMFile Seg.
                mov     es,ax                   ; ES = COMFile Seg.
                mov     ss,ax                   ; SS = COMFile Seg.
                xor     ax,ax                   ; AX = 0
                xor     bx,bx                   ; BX = 0
                xor     cx,cx                   ; CX = 0
                xor     dx,dx                   ; DX = 0
                xor     si,si                   ; SI = 0
                xor     di,di                   ; DI = 0
                xor     bp,bp                   ; BP = 0
                mov     sp,0FFFEh

                mov     AX,100h                 ; turn Trap Flag on
                push    AX
                push    ds                      ; put the new cs into stack

                mov     ax,100h                 ; put the new offset into stack
                push    ax

                mov     ax,bx
                iret                            ; begin process
                                                ; (iret restores flags)

;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
CONTINUE:
; By Setting The 0Ah and the 0Ch addresses To This Code, (PSP:0Ah = INT 22)
; When Program Process ends (INT 22) it will return to here

                mov     ax,cs
                mov     ds,ax                   ; DS = COMFile Seg.
                mov     es,ax                   ; ES = COMFile Seg.
                mov     ss,ax                   ; SS = COMFile Seg.
                mov     sp,SSPTR                ; Restore Stack Pointer
                mov     bx,ax
                mov     ah,50h                  ; Set current Proccess ID back
                int     21h                     ; To Our CODE
                retn
INT21h          endp


;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
setint01:
                mov     ax,2501h
                push    cs
                pop     ds
                lea     dx,new_int1
                Call    int21h
                ret

;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
new_int1:
                xchg    bp,sp
                cmp     word ptr ss:[bp],100h   ;  IP = 100h ?
                xchg    bp,sp
                jz      addr_100h
       int1_ret:IRET
      old_ax      DW          0
      old_bx      DW          0
      old_cx      DW          0
      old_dx      DW          0
      old_si      DW          0
      CLEANFLAG   DB          0
      ASKFLAG     DB          0                 ;if 'C' choosen
;-------------------- some of Regs. = 0 recommended to extract
      recommend:
                or      cx,dx
                jnz     cont_save_qstin         ;continue save question
                mov     ax,cs
                mov     ds,ax
                mov     ah,9h
                lea     dx,found1_       ;print 'FOUND' messsage
                int     21h
                mov     CLEANFLAG,1
                CMP     ASKFLAG  ,1
                JZ      cont_save_it
                jmp     cont_save_qstin         ;continue save question
;-------------------- Save Process
      addr_100h:
                mov     cs:old_ax,ax
                mov     cs:old_bx,bx
                mov     cs:old_cx,cx
                mov     cs:old_dx,dx
                mov     cs:old_si,si
                or      ax,bx
                jz      recommend                ;Recommend Opening?

cont_save_qstin:
                cmp     ASKFLAG,1
                jz      process
                mov     ax,cs
                mov     ds,ax
                IN      AX,21h                   ;
                MOV     si,ax                    ; save PORT 21h into SI
                XOR     AX,AX
                OUT     21h,AX                   ; reset port 21h

                mov     ah,9h
                lea     dx,found2_       ;print 'FOUND' messsage
                int     21h

                mov     AX,0h
                int     16h
                and     al,5Fh

                mov     ah,2
                mov     dl,al
                int     21h
                cmp     al,'Y'
                jz      cont_save_it

                cmp     al,'C'
                jnz     process

                cmp     cleanflag,1
                jz      cont_save_it
                mov     askflag,1
         process:
                MOV     AX,SI
                OUT     21h,AX
                mov     cleanflag,0
                mov     ax,cs:old_ax
                mov     bx,cs:old_bx
                mov     cx,cs:old_cx
                mov     dx,cs:old_dx
                mov     si,cs:old_si
                jmp     int1_ret

cont_save_it:

                XOR     AX,AX
                push    ax
                popf
                OUT     21h,AX                   ; reset port 21h
                mov     ax,cs
                mov     ds,ax
                mov     ah,9
                lea     dx,comsaved_
                int     21h

                xchg    bp,sp
                mov     ax,ss:[bp+2]
                mov     cs:COMFileSeg,ax
                mov     cs:terminated_flag,1
                mov     ah,4Ch
                int     21h          ; return process to CONTINUE label
                                     ; via INT 22

;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ

SAVECOMFile:
                mov     ds,cs:COMFileSeg
                mov     si,100h                 ; start search from psp
                cld

get_size:
                xor     cl,cl                   ; zero cl

scan_cs:
                lodsb
                cmp     si,0ffffh
                je      end_scan
                cmp     al,90h
                jne     get_size

                inc     cl
                cmp     cl,10h
                jne     scan_cs
end_scan:
                sub     si,110h
                push    cs
                pop     ds

                mov     bx,7Fh
                add     bl,ds:PSP_cmd_size
                mov     byte ptr [bx],30h
                mov     ax,3C00h                ;Create File
                xor     cx,cx                   ; CX = 0
                mov     dx,Filename
                xor     bx,bx                   ; BX = 0
                call    INT21h
                mov     bx,ax                   ;Save Handle

                push    ds
                mov     ds,COMFileseg
                mov     ah,40h                  ; Write to file
                mov     cx,si                   ; SIZE = SI
                mov     dx,100h                 ; Start of COM File
                int     21h
                pop     ds


                mov     ah,3Eh                  ; Close file
                call    INT21h
                retn

;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
terminated_message proc
                   push cs
                   pop  ds
                   lea  dx,cant_save_
                   mov  ah,9
                   int  21h
                   jmp  exit
terminated_message endp
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ


real_start:
                mov     sp,NEWStack
                Call    Wrtlogo           ; write program text on screen
                Call    Check4com         ; check for .COM extension
                Call    Allocmem          ; allocate 64kb for com file
                Call    Loadfile          ; load com file into memory
                Call    setint01          ; set INT1 to out code
                Call    Runcomfile        ; execute com file from memory
                cmp     terminated_flag,1 ; if program was terminated
                jz      save_it           ; then display terminated message
                call    terminated_message

        save_it:
                Call    SAVECOMfile
EXIT:
                mov     ah,4Ch
                int     21h             ; Terminate process



        PROGEND EQU     $
CSEG            ends


                end     start


